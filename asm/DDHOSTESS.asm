*          DATA SET DDHOSTESS  AT LEVEL 060 AS OF 06/07/19                      
*PHASE HOSTESSA                                                                 
*INCLUDE DMDMGRL                                                                
*INCLUDE LOGIO                                                                  
*INCLUDE BINSRCH2                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE REPLACE                                                                
*INCLUDE PQPROF                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE GETDAY                                                                 
*INCLUDE PDUMPER                                                                
*INCLUDE PRINT                                                                  
*INCLUDE PRINTERL                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE STXITER                                                                
*INCLUDE FATABOFF                                                               
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE GETFACT                                                                
*INCLUDE CARDS                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE TIMBER                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE XSORT                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE EXSERV                                                                 
*INCLUDE EIOSQL                                                                 
*INCLUDE EIOPQR                                                                 
*INCLUDE ESSLOG                                                                 
*INCLUDE DATTIM                                                                 
*INCLUDE ESSRQS                                                                 
*INCLUDE VSAMIO                                                                 
*INCLUDE COMPRES                                                                
*INCLUDE CRYPT                                                                  
*INCLUDE ESSWTO                                                                 
*INCLUDE ESSAPPC                                                                
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* TITLE:       HOSTESS - HOST ESS INTERFACE SUB SYSTEM CONTROLLER     *         
*                                                                     *         
* COMMENTS:    CONTROLLER MANAGING SUB TASKS THAT SEND MESSAGES       *         
*              TO AND RECEIVES MESSAGES FROM ESS                      *         
*                                                                     *         
***********************************************************************         
         TITLE 'DDHOSTESS - HOST ESS INTERFACE CONTROLLER'                      
         EJECT                                                                  
HOSTESS  CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,**HESS**,=A(R13CHAIN),RA,R9,R8,RR=RE                           
*                                                                               
         ENTRY UTL                 FOR DATAMGR                                  
         ENTRY SSB                 FOR DATAMGR                                  
*                                                                               
         LR    R7,RC                                                            
         SHI   R7,4                                                             
         L     R7,0(R7)                                                         
         L     R7,0(R7)            A(R1 PARAMETERS FROM ATTACH)                 
*                                                                               
         LR    R1,RC                                                            
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         USING COMMWORK,RC                                                      
         ST    RE,RELO                                                          
         ST    RB,RBSAVE                                                        
*                                                                               
         CLC   =C'ADV',2(R7)                                                    
         BE    HOST010                                                          
         CLC   =C'REP',2(R7)                                                    
         BE    HOST010                                                          
         CLC   =C'TST',2(R7)                                                    
         BE    HOST010                                                          
         CLC   =C'FQA',2(R7)                                                    
         BE    HOST010                                                          
         CLC   =C'CSC',2(R7)                                                    
         BE    HOST010                                                          
         DC    H'0'                                                             
*                                                                               
HOST010  MVC   MAJORNAM+5(1),2(R7) COMPLETE MAJOR RESCOURCE NAME                
         LA    RF,MAJORNAM                                                      
         STCM  RF,15,VMAJORNM                                                   
*                                                                               
         LA    R0,4                                                             
         LNR   R0,R0                                                            
         SVC   247                 SET SO HOSTESS IS NOT SWAPPABLE              
*                                                                               
         L     RF,VPRNTER          A(SYSPRINT DCB)                              
         MVC   DCBDDNAM-IHADCB(8,RF),=C'ESSTRACE'                               
         MVC   P,SPACES                                                         
*                                                                               
         LA    RE,DUMMYTCB        INITIALISE DUMMY TASK CONTROL BLOCK           
         LA    RF,EATLQ                                                         
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         LA    R3,DUMMYTCB        INITIALISE DUMMY TASK CONTROL BLOCK           
         USING ESSATCD,R3                                                       
         LA    RF,AESSFACS                                                      
         STCM  RF,15,EATFACS                                                    
*                                                                               
         L     R4,=A(ESSB)         INITIALISE SYSTEM STATUS BLOCK               
         ST    R4,AESSB                                                         
         USING ESSBD,R4                                                         
         OI    ESSBSTA1,ESSBSFTP   FTPFLAG=ON                                   
*                                                                               
         ICM   RF,15,AESSB                                                      
         STCM  RF,15,EATESSB                                                    
*                                                                               
         LA    R5,ATIOT                                                         
         EXTRACT (R5),FIELDS=TIOT                                               
         L     R5,ATIOT                                                         
         MVC   EATNAME,0(R5)       MOVE IN JOB NAME                             
         MVC   ESSBJNAM,0(R5)                                                   
*                                                                               
         MVC   WTOMSG,SPACES                                                    
         MVC   WTOMSG(8),ESSBJNAM                                               
         MVI   WTOMSG+8,C'.'                                                    
         MVC   WTOMSG+9(8),EATNAME                                              
         MVI   WTOMSG+17,C'.'                                                   
         MVI   WTOMSG+29,C'-'                                                   
         DROP  R3,R4                                                            
*                                                                               
         L     RF,=A(SSB)                                                       
         NI    3(RF),X'FF'-X'02'   SET OFFLINE RECOVERY ON                      
         OI    3(RF),X'08'         RECOVER COPIES                               
*                                                                               
*&&US*&& B     MAIN                                                             
*                                                                               
         GOTO1 =A(UNIQUE),(RC)     CHECK UNIQUE HOSTESS JOB NAME                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     MAIN                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     EQU   *                                                                
*                                                                               
         GOTO1 =A(READCRDS),(RC)   READ INPUT PARAMETER CARDS                   
*                                                                               
         BAS   RE,INITIAL          GENERAL INITIALISATION                       
*                                                                               
         BAS   RE,BLDSESS          BUILD ESS SESSION CONTROL BLOCKS             
*                                                                               
         BAS   RE,BLDSUB           BUILD ESSIO SUB TASK POOL                    
*                                                                               
         GOTO1 =A(RFAAPPC),(RC)    REGISTER FOR ALLOCATES APPC/MVS              
         BNE   MERR                                                             
*                                                                               
         GOTO1 =A(RALRAPPC),(RC)   ESSLURCV RECEIVE ALLOCATE APPC/MVS           
         BNE   MERR                                                             
*                                                                               
         GOTO1 =A(RALSAPPC),(RC)   ESSLUSND RECEIVE ALLOCATE APPC/MVS           
         BNE   MERR                                                             
*                                                                               
         MVC   P(40),=CL40'HOSTESS INITIALISATION COMPLETE'                     
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+18(8),SPACES                                              
*                                                                               
         BAS   RE,INITTIME         INITIALISE TIMEOUT TIMER                     
*                                                                               
         BAS   RE,UPDTEXS          UPDATE SQL EXTRACT SYSTEM TABLE              
*                                                                               
         BAS   RE,SENDER           PROCESS ESS SENDER SESSIONS                  
*                                                                               
MEVENT   EQU   *                   RETURN HERE AFTER EVENT                      
*                                                                               
         BAS   RE,WAITECB          WAIT ON ESS EVENTS OR TIMEOUT                
*                                                                               
         SR    R0,R0               GET THE CURRENT MVS TIME                     
         SR    R1,R1               (IN HHMMSSTT FORMAT)                         
         TIME                                                                   
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                SET TO 1/100 SEC IF 0 TIME                   
         ST    R0,MVSTIME                                                       
*                                  REFRESH DATE VALUES                          
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAY2)                                    
         GOTO1 VDATCON,DMCB,(5,0),(3,TODAY3)                                    
         GOTO1 VDATCON,DMCB,(5,0),TODAY6                                        
         GOTO1 VDATCON,DMCB,(5,0),(8,TODAY8)                                    
*                                                                               
MOPER    L     RF,AOPERECB         TEST IF OPERATOR COMMS POSTED                
         TM    0(RF),X'40'                                                      
         BZ    MSUB                                                             
         GOTO1 =A(CHKOPER),(RC)    PROCESS OPERATOR COMMUNICATIONS              
         CLI   OPERSTOP,C'Y'       SHUTDOWN IF OPERATOR STOP                    
         BE    MSTOP                                                            
*                                                                               
MSUB     BAS   RE,PROCSUB          PROCESS SUB TASK EVENT ECB LIST              
         BNE   MERR                                                             
*                                                                               
MSESS    BAS   RE,PROCSESS         PROCESS ESS SESSION APPC ECB LIST            
         BNE   MERR                                                             
*                                                                               
MRALLOC  LA    RF,RCVAECB          TEST IF ESSLURCV ALLOCATE POSTED             
         TM    0(RF),X'40'                                                      
         BZ    MSALLOC                                                          
         BAS   RE,PROCRRAL         PROCESS ESSLURCV ALLOCATE                    
         BNE   MERR                                                             
*                                                                               
MSALLOC  LA    RF,SNDAECB          TEST IF ESSLUSND ALLOCATE POSTED             
         TM    0(RF),X'40'                                                      
         BZ    MTIMER                                                           
         BAS   RE,PROCSRAL         PROCESS ESSLUSND ALLOCATE                    
         BNE   MERR                                                             
*                                                                               
MTIMER   LA    RF,TIMERECB         TEST TIMER POP                               
         TM    0(RF),X'40'                                                      
         BZ    MEVENT                                                           
         XC    TIMERECB,TIMERECB   CLEAR TIME OUT ECB                           
*                                                                               
MNEXT    TIME  BIN                 GET THE CURRENT TIME                         
         ST    R0,LOOPTIME         R0 = TIME OF LAST CONTROL LOOP               
*                                                                               
         BAS   RE,INITTIME         REINITIALISE TIMEOUT TIMER                   
*                                                                               
         BAS   RE,UPDTEXS          UPDATE SQL EXTRACT SYSTEM TABLE              
*                                                                               
         BAS   RE,SENDER           PROCESS ESS SENDER SESSIONS                  
*                                                                               
         B     MEVENT              CONTINUE MAIN CONTROL LOOP                   
*                                                                               
MSTOP    EQU   *                   SHUTDOWN                                     
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,X'FF'       HERE IF PROCESS ERROR                        
         MVC   P(40),=CL40'HOSTESS ERROR EXIT'                                  
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     MXIT                                                             
*                                                                               
MXIT     EQU   *                   EXIT FROM SYSTEM                             
         MVC   P(40),=CL40'HOSTESS SHUTDOWN STARTED'                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+18(8),SPACES                                              
*                                                                               
         GOTO1 =A(SHUTDOWN),(RC)   SHUT DOWN SYSTEM SUB TASKS                   
*                                                                               
         GOTO1 =A(SHUTESS),(RC)    SHUT DOWN ESS SESSIONS                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(URAAPPC),(RC)    UNREGISTER FOR ALLOCATES APPC/MVS            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(CLNAPPC),(RC)    CLEAN UP OUTSTANDING ASYNC REQUESTS          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  DUMP REQUEST QUEUE                           
         BAS   RE,DUMPRQS                                                       
*                                  CLOSE DOWN REQUEST QUEUE                     
         GOTO1 VESSRQS,DMCB,=AL4(ERQACLOQ),WORK                                 
         CLI   0(R1),ERQROKQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        GOTO1 VVSAMIO,DMCB,=C'CLS'                                             
*                                                                               
         CLI   CONSOPEN,C'N'       CLOSE CONTROL SYSTEM IF STILL OPEN           
         BE    MXBASE                                                           
         L     RE,=V(UTL)          SWITCH TO CONTROL SYSTEM                     
         MVI   4(RE),CONTROLQ                                                   
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'FILES',,IOL                              
         MVI   CONSOPEN,C'N'                                                    
*                                                                               
MXBASE   XBASE RC=RETCODE,RL=1     RETURN                                       
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
INITIAL  NTR1                                                                   
         MVC   P(40),=CL40'HOSTESS INITIALISATION'                              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         L     RE,=A(EATCB)        A(EATCB)                                     
         STCM  RE,15,ESSBAEAT                                                   
         L     RE,=A(ESECB)        A(ESECB)                                     
         STCM  RE,15,ESSBAESE                                                   
         TM    ESSBSTA1,ESSBSEST   ESTAE=YES ?                                  
         BZ    INIT010                                                          
         DROP  RF                                                               
         ESTAE MAINTAE,CT,ASYNCH=YES,TERM=YES                                   
*                                                                               
INIT010  EQU   *                                                                
         GOTO1 VDATCON,DMCB,(5,0),(2,TODAY2)                                    
         GOTO1 VDATCON,DMCB,(5,0),(3,TODAY3)                                    
         GOTO1 VDATCON,DMCB,(5,0),TODAY6                                        
         GOTO1 VDATCON,DMCB,(5,0),(8,TODAY8)                                    
*                                                                               
         GOTO1 VESSRQS,DMCB,=AL4(ERQAINIQ),WORK                                 
         CLI   0(R1),ERQROKQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        LA    RE,=C'OPU'                                                       
*        GOTO1 VVSAMIO,DMCB,(RE)                                                
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         BAS   RE,LOADRQS                                                       
*                                                                               
         TIME  BIN                 GET THE CURRENT TIME                         
         ST    R0,LOOPTIME         R0 = TIME OF LAST CONTROL LOOP               
*                                  GET THE CURRENT MVS DATE/TIME                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         TIME                                                                   
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                SET TO 1/100 SEC IF 0 TIME                   
         ST    R0,MVSTIME                                                       
*                                                                               
         LA    RF,TIMERECB                                                      
         ST    RF,ATIMRECB                                                      
*                                                                               
         BAS   RE,OPENCTFL         OPEN CTFILE                                  
*                                                                               
         EXTRACT ACOMM,FIELDS=COMM                                              
         L     RF,ACOMM            SET UP OPERATOR COMMUNICATIONS               
         USING COMLIST,RF                                                       
         MVC   AOPERECB+1(3),COMECBPT+1                                         
         L     R2,COMCIBPT         GET A(CIB)                                   
         LA    R3,COMCIBPT         GET A(A(CIB))                                
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTART    WAS IT BROUGHT UP WITH 'START'?              
         BNE   NOSTART                                                          
         DROP  R2                                                               
         QEDIT ORIGIN=(R3),BLOCK=(R2)  YES -- FREE THE CIB                      
NOSTART  QEDIT ORIGIN=(R3),CIBCTR=1    NOW ALLOW MODIFIES                       
*                                                                               
         L     R2,=A(ECBLIST)      SAVE OPERATOR COMMS ECB IN LIST              
         XC    0(4,R2),0(R2)                                                    
         L     RF,AOPERECB                                                      
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
*                                  SAVE ESSLURCV ALLOCATE ECB IN LIST           
         XC    0(4,R2),0(R2)                                                    
         LA    RF,RCVAECB                                                       
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
*                                  SAVE ESSLUSND ALLOCATE ECB IN LIST           
         XC    0(4,R2),0(R2)                                                    
         LA    RF,SNDAECB                                                       
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
*                                                                               
         XC    0(4,R2),0(R2)                                                    
         OI    0(R2),X'80'         MARK END OF ECB LIST                         
         XC    TIMERECB,TIMERECB   AND PLACE TIMER ECB HERE                     
         LA    RF,TIMERECB                                                      
         STCM  RF,7,1(R2)                                                       
         ST    R2,ASESSECB         A(ESS SESSION CONTROL ECB LIST)              
*                                  BUILD APPCECBL                               
         LA    R1,APPCEST                                                       
         STCM  R1,7,AAPPCEST+1                                                  
         LA    R1,RCVAECB                                                       
         STCM  R1,7,ARCVAECB+1                                                  
         LA    R1,SNDAECB                                                       
         STCM  R1,7,ASNDAECB+1                                                  
*                                  INITIALISE APPC/MVS INTERFACE                
         ICM   R6,15,=A(ESSAPPCC)                                               
         USING ESSAPPCD,R6                                                      
         XC    APPCACTN,APPCACTN                                                
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         GOTO1 =A(INITAPPC),(RC)                                                
         LA    R1,APPCECB                                                       
         STCM  R1,15,NOTIFY_TYPE_AECB                                           
*                                  INITIALISE ESSLURCV APPC/MVS                 
         ICM   R6,15,=A(RCVAPPCC)                                               
         GOTO1 =A(INITAPPC),(RC)                                                
         MVI   APPCDUMP,C'N'                                                    
         LA    R1,RCVAECB                                                       
         STCM  R1,15,NOTIFY_TYPE_AECB                                           
*                                  INITIALISE ESSLUSND APPC/MVS                 
         ICM   R6,15,=A(SNDAPPCC)                                               
*                                                                               
         GOTO1 =A(INITAPPC),(RC)                                                
         MVI   APPCDUMP,C'N'                                                    
         LA    R1,SNDAECB                                                       
         STCM  R1,15,NOTIFY_TYPE_AECB                                           
         DROP  R6                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LOAD REQUEST QUEUE FROM HOSTRCV RECOVERY FILE                       *         
***********************************************************************         
LOADRQS  NTR1                                                                   
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         TM    ESSBSTA1,ESSBSRBR   TEST FOR REBUILD=RESTART                     
         BZ    LRQSX                                                            
         DROP  RF                                                               
         MVC   P(40),=CL40'LOADING REQUESTS FROM HOSTRCV'                       
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         LA    R2,HOSTRCV                                                       
         OPEN  ((R2),INPUT)                                                     
*                                                                               
LRQS010  GET   HOSTRCV,IOL                                                      
         GOTO1 VESSRQS,DMCB,=AL4(ERQAADDQ),IO                                   
         CLI   0(R1),ERQROKQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LRQS010                                                          
*                                                                               
EODADREQ EQU   *                                                                
         LA    R2,HOSTRCV                                                       
         CLOSE ((R2))                                                           
         B     LRQSX                                                            
*                                                                               
LRQSX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* OPEN CONTROL FILE IF NOT ALREADY OPEN                               *         
***********************************************************************         
OPENCTFL NTR1                                                                   
         LA    R3,CTFILEU                                                       
         CLI   WRITE,C'Y'                                                       
         BE    *+8                                                              
         LA    R3,CTFILEN                                                       
         GOTO1 VDATAMGR,DMCB,(0,=C'DMOPEN'),CONTROL,(R3),A(IO),0                
         MVI   CONSOPEN,C'Y'                                                    
*                                  GET DTF ADDRESS OF GENDIR                    
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',GENDIR                                  
         ICM   RF,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    RF,0(RF)                                                         
         ST    RF,AGENDIR                                                       
*                                  GET DTF ADDRESS OF GENFIL                    
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',GENFIL                                  
         ICM   RF,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    RF,0(RF)                                                         
         ST    RF,AGENFIL                                                       
*                                  GET DTF ADDRESS OF CTFILE                    
         GOTO1 VDATAMGR,DMCB,=C'DTFADR',CTFILE                                  
         ICM   RF,15,12(R1)                                                     
         BNZ   *+6                                                              
         DC    H'00'                                                            
         LA    RF,0(RF)                                                         
         ST    RF,ACTFILE                                                       
*                                  DUMMY READ FOR GETMAIN PROBLEM               
         XC    IOKEY,IOKEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),CTFILE,IOKEY,IO,0                   
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* DUMMY CALL TO CTRL ENQUEUE TO FORCE DDSQDC TO BE OPENED BY MAIN TASK.         
* OTHERWISE A SUBTASK OPENS IT AND THEN MVS CLOSES IT PREMATURELY WHEN          
* THE SUBTASK THAT OPENED IT IS TERMINATED.                                     
*                                                                               
         GOTO1 VDATAMGR,DMCBENQ,(0,=C'ENQCTL  '),(C'T',=C'CTRL')                
*                                                                               
OPENCTX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD ESS IO SESSION CONTROL BLOCKS FROM ESSID CONTROL RECORDS      *         
***********************************************************************         
BLDSESS  NTR1                                                                   
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         CLI   ESSBCODE,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
         L     R2,ASESSECB         A(ESS SESSION ECB LIST)                      
         L     R3,=A(ESECBF)       A(FIRST ENTRY IN ESECB)                      
         USING ESSESSD,R3                                                       
*                                                                               
BESS010  EQU   *                                                                
         USING GESSD,R5                                                         
         L     RF,=A(BDCBC)                                                     
         STCM  RF,15,ANEXTDCB                                                   
         LA    R5,IOKEY                                                         
         XC    GSKEY,GSKEY         CLEAR KEY                                    
         MVI   GSKREC,GSKRECQ                                                   
         LAY   R5,IO                                                            
*                                  READ HIGH WITH FLUSH BUFFER                  
         GOTO1 VDATAMGR,DMCB,(X'24',DMRDHI),GENDIR,IOKEY,(R5),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+6                                                              
         DC    H'0'                DATAMANAGER RETURN NOT 0 OR EOF              
         CLI   8(R1),0                                                          
         BNE   BESS300                                                          
         CLI   GSKREC,GSKRECQ      TEST FOR ESSID RECORD                        
         BNE   BESS300                                                          
         B     BESS030                                                          
*                                  READ WITH FLUSH                              
BESS020  GOTO1 VDATAMGR,DMCB,(X'24',DMREAD),GENDIR,IOKEY,(R5),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+6                                                              
         DC    H'0'                DATAMANAGER RETURN NOT 0 OR EOF              
         CLI   8(R1),0                                                          
         BNE   BESS300                                                          
         CLI   GSKREC,GSKRECQ                                                   
         BNE   BESS300                                                          
*                                  READ SEQN WITH FLUSH                         
BESS022  GOTO1 VDATAMGR,DMCB,(X'24',DMRSEQ),GENDIR,IOKEY,(R5),DMWORK            
         TM    8(R1),X'FF'-X'80'   ONLY ALLOW EOF ERROR                         
         BZ    *+6                                                              
         DC    H'0'                DATAMANAGER RETURN NOT 0 OR EOF              
         CLI   8(R1),0                                                          
         BNE   BESS300                                                          
         CLI   GSKREC,GSKRECQ                                                   
         BNE   BESS300                                                          
*                                  GETREC WITH FLUSH                            
BESS030  MVC   IOKEY(L'GSKEY),GSKEY                                             
         MVC   DMDA,GSDDA                                                       
         GOTO1 VDATAMGR,DMCB,(X'24',GETREC),GENFIL,DMDA,(R5),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND RECORD                            
*                                                                               
BESS040  LA    R4,GSFIRST(R5)      PROCESS ELEMENT DATA                         
*                                                                               
BESS050  CLI   0(R4),0                                                          
         BE    BESS020                                                          
         CLI   0(R4),GESSELQ       FIND ESS DEFINITION ELEMENT                  
         BE    BESS070                                                          
BESS060  SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     BESS050                                                          
*                                                                               
         USING GESSEL,R4                                                        
BESS070  EQU   *                   PROCESS ESS DEFINITION ELEMENT               
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         CLC   ESSBCODE,GESSHCOD                                                
         BNE   BESS020                                                          
         DROP  RF                                                               
         CLI   GESSMETH,C'N'                                                    
         BNE   BESS020                                                          
         TM    GESSFLG1,GESSFINQ                                                
         BO    BESS020                                                          
         MVC   ESSNUM,GEKNUM                                                    
         EDIT  (B2,GEKNUM),(8,ESSNAME),ZERO=NOBLANK,FILL=0                      
         MVC   ESSNAME(3),=CL3'ESS'                                             
         MVC   TASKNUM,=XL2'00'                                                 
         MVC   ESSLEVEL,GESSLEV                                                 
         MVC   ESSVER,GESSVER                                                   
         MVC   HOSTSLU,GESSHSLU                                                 
         MVC   HOSTRLU,GESSHRLU                                                 
         MVC   ESSENID,GESSENID                                                 
         MVC   ESSELU,GESSELU                                                   
         MVI   ESSLUSN,0                                                        
         MVC   ESSMELU,GESSELU                                                  
         MVC   ESSTPNM,GESSTPNM                                                 
         MVC   ESSAMODE,GESSMODE                                                
         MVC   ESSPWD,GESSPWD                                                   
         MVC   ESSEKEY,GESSEKEY                                                 
         MVC   ESSFAC,GESSFAC                                                   
         MVC   ESSMETH,GESSMETH                                                 
         MVC   ESSFTPS,GESSFTPS                                                 
         MVC   ESSFLG1,GESSFLG1                                                 
         MVC   ESSFLG2,GESSFLG2                                                 
         MVC   ESSATIM,GESSATIM                                                 
         MVC   ESSTRAC,GESSTRAC                                                 
         MVC   ESSSLIM,GESSSLIM                                                 
         DROP  R4                                                               
*                                                                               
BESS080  CLI   0(R4),0                                                          
         BE    BESS130                                                          
         CLI   0(R4),GENFSELQ      FIND NFS MOUNT INFO ELEMENT                  
         BNE   BESS090                                                          
         USING GENFSEL,R4                                                       
         MVC   ESSNFSFR,GENFSFR                                                 
         MVC   ESSNFSTO,GENFSTO                                                 
         MVC   ESSNFSIP,GENFSIP                                                 
         B     BESS130                                                          
         DROP  R4                                                               
BESS090  SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     BESS080                                                          
*                                                                               
*                                  INSERT ENTRY IN SQL EXTRACT TABLE            
BESS130  LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   EXSACTN,=AL4(EXSAINXQ)                                           
         MVC   EXSEID,ESSNAME                                                   
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,0              CHECK RETURN CODE                            
         BE    *+6                 OK, CONTINUE                                 
         DC    H'0'                ERROR                                        
         DROP  R6                                                               
*                                  RECOVER VALUES IN EXTRACT TABLE              
         TM    ESSFLG2,GESSFSIQ    UNLESS SEND IMMEDIATE FLAGGED                
         BO    BESS132                                                          
         LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   EXSACTN,=AL4(EXSARECQ)                                           
         MVC   EXSEID,ESSNAME                                                   
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,0              CHECK RETURN CODE                            
         BE    BESS132                                                          
         MVC   P(40),=CL40'EXSERV RECOVER ERROR'                                
         MVC   P+21(L'EXSEID),EXSEID                                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                  SET FILE RECOMMIT FLAG IN TABLE              
BESS132  EQU   *                                                                
         TM    ESSFLG1,GESSFRCQ    ESSID RECOMMIT=YES ?                         
         BNZ   BESS134                                                          
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         TM    ESSBSTA1,ESSBSRCO   RECOMMIT=YES ?                               
         DROP  RF                                                               
         BZ    BESS150                                                          
BESS134  LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   EXSACTN,=AL4(EXSASRCQ)                                           
         MVC   EXSEID,ESSNAME                                                   
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,0              CHECK RETURN CODE                            
         BE    *+6                 OK, CONTINUE                                 
         DC    H'0'                ERROR                                        
         EJECT                                                                  
*                                  ADD NEW ESS SESSION CONTROL BLOCKS           
BESS150  EQU   *                                                                
         MVI   ESSSNUM,1           INITIALISE ESS SESSION NUMBER                
*                                                                               
BESS160  LR    RE,R3               CLEAR ESSESSD TABLE ENTRY                    
         LA    RF,ESELQ                                                         
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 ADDSESS,=CL8'RECEIVER'                                           
*                                                                               
         LA    RF,ESEAPPCE                                                      
         XC    0(4,R2),0(R2)                                                    
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
*                                                                               
         C     R3,=A(ESECBX)                                                    
         BNE   *+6                                                              
         DC    H'0'                MUST INCREASE ESEC BLOCK                     
         L     RF,=A(ESECBTL)      BUMP BLOCK LENGTH                            
         L     RE,0(RF)                                                         
         AH    RE,=Y(ESELQ)                                                     
         ST    RE,0(RF)                                                         
*                                                                               
         LA    R3,ESELQ(R3)                                                     
         SR    RF,RF                                                            
         ICM   RF,3,SESSCNT                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,SESSCNT                                                     
*                                                                               
         CLC   ESSSNUM,ESSSLIM                                                  
         BNL   BESS200                                                          
         SR    RF,RF                                                            
         IC    RF,ESSSNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,ESSSNUM                                                       
*        B     BESS160             ONLY 1 SESSION PER RECEIVER                  
*                                                                               
BESS200  MVI   ESSSNUM,1           INITIALISE ESS SESSION NUMBER                
*                                                                               
BESS210  LR    RE,R3               CLEAR ESSESSD TABLE ENTRY                    
         LA    RF,ESELQ                                                         
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 ADDSESS,=CL8'SENDER'                                             
*                                                                               
         LA    RF,ESEAPPCE                                                      
         XC    0(4,R2),0(R2)                                                    
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
*                                                                               
         C     R3,=A(ESECBX)                                                    
         BNE   *+6                                                              
         DC    H'0'                MUST INCREASE EATC BLOCK                     
         L     RF,=A(ESECBTL)      BUMP BLOCK LENGTH                            
         L     RE,0(RF)                                                         
         AH    RE,=Y(ESELQ)                                                     
         ST    RE,0(RF)                                                         
*                                                                               
         LA    R3,ESELQ(R3)                                                     
         SR    RF,RF                                                            
         ICM   RF,3,SESSCNT                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,SESSCNT                                                     
*                                                                               
         CLC   ESSSNUM,ESSSLIM                                                  
         BNL   BESS020                                                          
         SR    RF,RF                                                            
         IC    RF,ESSSNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,ESSSNUM                                                       
         B     BESS210                                                          
*                                                                               
BESS300  EQU   *                                                                
         ST    R2,AESUBECB         A(ESS SUB TASK CONTROL ECB LIST)             
*                                                                               
         MVC   P(40),=CL40'HOSTESS SESSION CONTROL BLOCKS BUILT'                
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+18(8),SPACES                                              
         XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* ADD ENTRY TO SESSION CONTROL BLOCK TABLE (ESSESSD)                  *         
***********************************************************************         
         USING ESSESSD,R3                                                       
ADDSESS  NTR1                                                                   
         CLC   0(8,R1),=CL8'RECEIVER'                                           
         BE    ASESRCV                                                          
         CLC   0(8,R1),=CL8'SENDER'                                             
         BE    ASESSND                                                          
         DC    H'0'                                                             
*                                                                               
ASESRCV  MVI   ESEMODE,ESEMRCVQ                                                 
         MVC   ESENAME,ESSNAME                                                  
         MVC   ESENAME(2),=CL2'ER'                                              
         EDIT  ESSSNUM,(1,ESENAME+2),ZERO=NOBLANK                               
         B     ASES010                                                          
*                                                                               
ASESSND  MVI   ESEMODE,ESEMSNDQ                                                 
         MVC   ESENAME,ESSNAME                                                  
         MVC   ESENAME(2),=CL2'ES'                                              
         EDIT  ESSSNUM,(1,ESENAME+2),ZERO=NOBLANK                               
         B     ASES010                                                          
*                                                                               
ASES010  MVC   ESENUM,ESSNUM                                                    
         MVC   ESEENID,ESSENID                                                  
         MVC   ESEELU,ESSELU                                                    
         MVC   ESEMELU,ESSMELU                                                  
         MVC   ESELUSN,ESSLUSN                                                  
         MVC   ESELEV,ESSLEVEL                                                  
         MVC   ESEVER,ESSVER                                                    
         MVC   ESEEPWD,ESSPWD                                                   
         MVC   ESEEKEY,ESSEKEY                                                  
         MVC   ESEFAC,ESSFAC                                                    
         MVC   ESEFTPS,ESSFTPS                                                  
         MVC   ESEHFLG1,ESSFLG1                                                 
         MVC   ESEHFLG2,ESSFLG2                                                 
         MVC   ESESATIM,ESSATIM                                                 
         MVI   ESESTATE,ESESNALQ                                                
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
         MVC   ESESNUM,ESSSNUM                                                  
         MVC   ESESLIM,ESSSLIM                                                  
         MVC   ESETRAC,ESSTRAC                                                  
         MVC   ESENFSFR,ESSNFSFR                                                
         MVC   ESENFSTO,ESSNFSTO                                                
         MVC   ESENFSIP,ESSNFSIP                                                
         LA    RF,ESEAPPCE                                                      
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         ICM   R0,15,=AL4(ESSDATAL)                                             
         AH    R0,=H'4095'                                                      
         SRL   R0,12                                                            
         SLL   R0,12               ROUND TO 4K MULTIPLE                         
         GETMAIN RU,LV=(0),LOC=BELOW,BNDRY=PAGE                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,ESEADATA                                                   
*                                                                               
         ICM   R0,15,=AL4(ESSWORKL)                                             
         AH    R0,=H'4095'                                                      
         SRL   R0,12                                                            
         SLL   R0,12               ROUND TO 4K MULTIPLE                         
         GETMAIN RU,LV=(0),LOC=BELOW,BNDRY=PAGE                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,ESEAWORK                                                   
*                                                                               
         ICM   R0,15,=AL4(ESSPQBL)                                              
         AH    R0,=H'4095'                                                      
         SRL   R0,12                                                            
         SLL   R0,12               ROUND TO 4K MULTIPLE                         
         GETMAIN RU,LV=(0),LOC=BELOW,BNDRY=PAGE                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,ESEAPQB                                                    
*                                                                               
         ICM   R0,15,=AL4(ESSAPPCL)                                             
         AH    R0,=H'4095'                                                      
         SRL   R0,12                                                            
         SLL   R0,12               ROUND TO 4K MULTIPLE                         
         GETMAIN RU,LV=(0),LOC=BELOW,BNDRY=PAGE                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,ESEAAPPC                                                   
         LR    R6,R1                                                            
         USING ESSAPPCD,R6                                                      
         GOTO1 =A(INITAPPC),(RC)                                                
         LA    RF,ESEAPPCE                                                      
         STCM  RF,15,NOTIFY_TYPE_AECB                                           
         STCM  RF,15,POST_ON_RECEIPT_ECB                                        
         DROP  R6                                                               
*                                                                               
         L     RE,ANEXTDCB                                                      
         L     R0,=A(BLKDCB)                                                    
         ICM   RF,15,=AL4(BLKDCBLQ)                                             
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         L     RE,ANEXTDCB                                                      
         STCM  RE,15,ESEADCB                                                    
         MVC   DCBDDNAM-IHADCB(8,RE),ESENAME                                    
         ICM   RF,15,=AL4(BLKDCBLQ)                                             
         AR    RE,RF                                                            
         ST    RE,ANEXTDCB                                                      
*                                                                               
         MVC   P(L'ESENAME),ESENAME                                             
         MVC   P+10(L'ESEELU),ESEELU                                            
         MVC   P+20(40),=CL40'SESSION CONTROL BLOCK ADDED'                      
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
AESSX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD ESSIO SUB TASK POOL. TASKSIZE IS SIZE OF POOL.                          
***********************************************************************         
BLDSUB   NTR1                                                                   
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         CLI   ESSBCODE,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
         L     R2,AESUBECB         A(ESSIO SUB TASK CONTROL ECB LIST)           
         L     R3,=A(EATCBF)       A(FIRST ENTRY IN EATCB)                      
         USING ESSATCD,R3                                                       
         SR    R5,R5                                                            
         ICM   R5,3,TASKSIZE       GET SIZE OF APPC/MVS POOL FROM INPUT         
         BNZ   BSUB010                                                          
         ICM   R5,3,SESSCNT        ELSE USE COUNT OF ESS SESSIONS               
         BNZ   BSUB010                                                          
         LA    R5,1                MINIMUM OF 1 SUB TASK                        
*                                                                               
BSUB010  EQU   *                   ADD APPC/MVS POOL ENTRIES                    
         SR    RF,RF                                                            
         ICM   RF,3,TASKNUM                                                     
         LA    RF,1(RF)                                                         
         STCM  RF,3,TASKNUM                                                     
         MVC   ESSNUM,=XL2'00'                                                  
         MVC   ESSNAME,=CL8'ESSAPPC '                                           
         MVI   ESSLEVEL,X'00'                                                   
         MVI   ESSVER,X'00'                                                     
         MVC   ESSTPNM,SPACES                                                   
         MVC   ESSAMODE,SPACES                                                  
         MVC   ESSEKEY,SPACES                                                   
         MVC   ESSFAC,SPACES                                                    
         MVI   ESSMETH,C'N'                                                     
         MVI   ESSFLG1,X'00'                                                    
         MVI   ESSFLG2,X'00'                                                    
         MVC   ESSNFSFR,SPACES                                                  
         MVC   ESSNFSTO,SPACES                                                  
         MVC   ESSNFSIP,SPACES                                                  
*                                                                               
         LR    RE,R3              CLEAR ATTACHED TASK CONTROL BLOCK             
         LA    RF,EATLQ                                                         
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         MVC   EATENUM,ESSNUM                                                   
         MVC   EATNAME,ESSNAME                                                  
         LA    RF,EATNAME+1                                                     
         EDIT  (B2,TASKNUM),(2,(RF)),ZERO=NOBLANK,FILL=0                        
         MVC   EATTASKN,TASKNUM                                                 
         MVC   EATELUID,APPCLUID                                                
         MVC   EATLEV,ESSLEVEL                                                  
         MVC   EATVER,ESSVER                                                    
         MVC   EATEKEY,ESSEKEY                                                  
         MVC   EATFAC,ESSFAC                                                    
         MVC   EATHFLG1,ESSFLG1                                                 
         MVC   EATHFLG2,ESSFLG2                                                 
         MVC   EATMETH,ESSMETH                                                  
         MVI   EATMODE,EATMSNDQ                                                 
         MVI   EATSTATE,EATSOFFQ                                                
         MVC   EATNFSFR,ESSNFSFR                                                
         MVC   EATNFSTO,ESSNFSTO                                                
         MVC   EATNFSIP,ESSNFSIP                                                
         OC    EATELUID,EATELUID                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  ATTACH ESSIO SUB TASK FOR SENDER             
         XC    EATOSECB,EATOSECB                                                
         XC    EATMTECB,EATMTECB                                                
         XC    EATSTECB,EATSTECB                                                
         XC    EATSPECB,EATSPECB                                                
*                                                                               
         LA    RF,AESSFACS                                                      
         STCM  RF,15,EATFACS                                                    
*                                                                               
         ICM   RF,15,AESSB                                                      
         STCM  RF,15,EATESSB                                                    
*                                  ATTACH ESSIO SUB TASK                        
         LA    R6,EATOSECB                                                      
         ATTACH EPLOC=ESSIONAM,ECB=(R6),PARAM=((R3)),SZERO=NO,         +        
               ESTAI=(SUBTAE,(R3))                                              
*                                                                               
         ST    R1,EATOSTCB                                                      
         OC    EATOSTCB,EATOSTCB                                                
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         WAIT  ECB=EATSTECB                                                     
         XC    EATSTECB,EATSTECB                                                
         MVI   EATSTATE,EATSRDYQ                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'ATTACHED ESSIO SUB TASK'                          
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         POST  EATMTECB                                                         
*                                                                               
         LA    RF,EATOSECB         SAVE OPERATING SYSTEM ECB IN ECBLIST         
         XC    0(4,R2),0(R2)                                                    
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
         LA    RF,EATSTECB         SAVE ESSIO SUB TASK ECB IN ECBLIST           
         XC    0(4,R2),0(R2)                                                    
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
*                                                                               
         C     R3,=A(EATCBX)                                                    
         BNE   *+6                                                              
         DC    H'0'                MUST INCREASE EATC BLOCK                     
         L     RF,=A(EATCBTL)      BUMP BLOCK LENGTH                            
         L     RE,0(RF)                                                         
         AH    RE,=Y(EATLQ)                                                     
         ST    RE,0(RF)                                                         
*                                                                               
* ??     LA    RF,100                                                           
*        ST    RF,FULL                                                          
*        STIMERM SET,ID=STIMERID,BINTVL=FULL,WAIT=YES                           
*        LTR   RF,RF                                                            
*        BZ    *+6                                                              
* ??     DC    H'0'                                                             
*                                                                               
         LA    R3,EATLQ(R3)                                                     
         BCT   R5,BSUB010                                                       
*                                                                               
BSUB200  EQU   *                                                                
         OI    0(R2),X'80'         MARK END OF ECB LIST                         
         XC    TIMERECB,TIMERECB   AND PLACE TIMER ECB HERE                     
         LA    RF,TIMERECB                                                      
         STCM  RF,7,1(R2)                                                       
*                                                                               
         MVC   P(40),=CL40'HOSTESS SUB TASK CONTROL BLOCKS BUILT'               
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+18(8),SPACES                                              
         XIT1                                                                   
         DROP  R3                                                               
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
         BL    ITIM010             YES                                          
*                                                                               
         LA    R1,DMCB                                                          
         L     RF,ATIMRECB         POST TIME OUT ECB                            
         ST    RF,4(R1)                                                         
         BAS   RE,TIMEOUT                                                       
         B     ITIMX                                                            
*                                                                               
ITIM010  SR    R2,R0               R2 = REMAINING TIME TO WAIT                  
         ST    R2,FULL                                                          
       STIMERM SET,ID=STIMERID,BINTVL=FULL,PARM=ATIMRECB,EXIT=TIMEOUT           
         LTR   RF,RF               WAIT THE REQUESTED INTERVAL                  
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
ITIMX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* UPDATE ENTRIES IN SQL EXTRACT TRANSFER CONTROL TABLE                *         
***********************************************************************         
UPDTEXS  NTR1                                                                   
*                                                                               
         L     R6,VMAJORNM         MAJOR RESCOURCE NAME                         
         ENQ   ((6),EQESSIO,E,8)   ENQUEUE ESSIO                                
*                                                                               
         LA    R5,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R5                                                       
         MVC   EXSACTN,=AL4(EXSAUPXQ)                                           
*                                  INSERT VALUES IN EXTRACT TABLE               
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,EXSRER1Q       CHECK RETURN CODE ERROR                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   BYTE,EXSRNOQ                                                     
         BE    UEXS010                                                          
         CLI   BYTE,EXSROKQ        CHECK RETURN CODE FILE READY                 
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UEXS010                                                          
         DROP  R5                                                               
*                                                                               
UEXS010  EQU   *                                                                
         L     R6,VMAJORNM         MAJOR RESCOURCE NAME                         
         DEQ   ((6),EQESSIO,8)     DEQUEUE ESSIO PROCESS                        
*                                                                               
UEXSX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS ESS SENDER SESSIONS                                         *         
***********************************************************************         
SENDER   NTR1                                                                   
         L     R4,=A(ESECBF)                                                    
         USING ESSESSD,R4                                                       
*                                  FIND READY ESS SESSION CONTROL BLOCK         
SEND010  L     RF,=A(ESECBTL)                                                   
         L     RF,0(RF)                                                         
         CR    R4,RF                                                            
         BNL   SENDOK                                                           
*                                                                               
SEND012  CLI   ESEMODE,ESEMSNDQ    TEST ESS SEND SESSION                        
         BNE   SEND020                                                          
         CLI   ESESTATE,ESESRDYQ   TEST ESS SESSION ALLOCATED                   
         BE    SEND030                                                          
         CLI   ESESTATE,ESESNALQ   TEST ESS SESSION NOT ALLOCATED               
         BE    SEND300                                                          
         CLI   ESESTATE,ESESRQPQ   TEST ESS SESSION REQUEST PENDING             
         BE    SEND400                                                          
         CLI   ESESTATE,ESESRSPQ   TEST ESS SESSION RESPONSE PENDING            
         BE    SEND500                                                          
*                                                                               
SEND020  LA    R4,ESELQ(R4)        BUMP POINTER                                 
         B     SEND010                                                          
*                                  HERE WITH READY SESSION FOUND                
SEND030  EQU   *                                                                
         BAS   RE,GETSUB                                                        
         BNE   SEND900                                                          
         L     R3,AESSATC                                                       
         USING ESSATCD,R3                                                       
         STCM  R3,15,ESEAEATC                                                   
         STCM  R4,15,EATAESS                                                    
         GOTO1 =A(SWAPIN),(RC)                                                  
         BNE   SENDOK                                                           
         MVI   EATMODE,EATMSNDQ                                                 
         MVI   EATSTATE,EATSRQPQ                                                
         MVI   ESESTATE,ESESRQPQ                                                
*                                                                               
         L     R5,EATDATA                                                       
         USING ESSDATAD,R5                                                      
         MVI   ESSDSEQN,ESSDSFSQ                                                
         MVI   ESSDMODE,ESSDMSNQ                                                
         GOTO1 VEIOPQR,DMCB,(R3),RR=RELO                                        
         BNE   SENDER1                                                          
         CLI   ESSDRETC,ESSDROKQ   TEST PQ REPORT READY TO PROCESS              
         BE    SEND200                                                          
         CLI   ESSDRETC,ESSDRNFQ                                                
         BE    SEND110                                                          
         DC    H'0'                                                             
*                                                                               
SEND110  GOTO1 VEIOSQL,DMCB,(R3),RR=RELO                                        
         BNE   SENDER2                                                          
         CLI   ESSDRETC,ESSDROKQ   TEST SQL EXTRACT FILE READY TO SEND          
         BE    SEND200                                                          
         CLI   ESSDRETC,ESSDRNFQ                                                
         BE    *+6                                                              
         DC    H'0'                                                             
         B     SEND130                                                          
*                                                                               
SEND130  EQU   *                                                                
         OC    ESESADTM,ESESADTM                                                
         BZ    SEND140                                                          
         OC    ESESATIM,ESESATIM                                                
         BZ    SEND140                                                          
         CLC   ESESADTM(2),TODAY2                                               
         BNE   SEND132                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESADTM+2,WORK                   
         CLC   WORK(4),ESESATIM                                                 
         BL    SEND140                                                          
*                                                                               
SEND132  EQU   *                                                                
         MVI   EATMODE,EATMTSTQ                                                 
         B     SEND200                                                          
*                                                                               
SEND140  EQU    *                                                               
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRDYQ                                                
         B     SEND020                                                          
*                                                                               
SEND200  EQU   *                                                                
         POST  EATMTECB                                                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'SENDER REQUEST POSTED'                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
         B     SEND020                                                          
*                                                                               
SEND300  EQU   *                                                                
         OC    ESESADTM,ESESADTM                                                
         BZ    SEND330                                                          
         OC    ESESATIM,ESESATIM                                                
         BZ    SEND330                                                          
         CLC   ESESADTM(2),TODAY2                                               
         BNE   SEND320                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESADTM+2,WORK                   
         CLC   WORK(4),ESESATIM                                                 
         BL    SEND330                                                          
SEND320  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSAT)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(8),ESENAME                                                  
         MVC   P+22(40),=CL40'ESS SESSION ACTIVITY TIME OUT'                    
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+18(8),ESEELU                                              
         TM    ESETRAC,GESSTSES                                                 
         BZ    SEND322                                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         B     SEND324                                                          
SEND322  EQU   *                                                                
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
SEND324  EQU   *                                                                
         MVC   WTOMSG+18(8),SPACES                                              
         MVC   WTOMSG+26(2),SPACES                                              
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
         B     SEND020                                                          
*                                                                               
SEND330  EQU    *                                                               
         B     SEND020                                                          
*                                                                               
SEND400  EQU   *                                                                
         CLI   ESEMODE,ESEMRCVQ                                                 
         BE    SEND410                                                          
         L     R5,ESEADATA                                                      
         USING ESSDATAD,R5                                                      
         CLI   ESSDBLKD,C'Y'                                                    
         BE    SEND440                                                          
*                                                                               
SEND410  OC    ESESADTM,ESESADTM                                                
         BZ    SEND440                                                          
         OC    ESESATIM,ESESATIM                                                
         BZ    SEND440                                                          
         CLC   ESESADTM(2),TODAY2                                               
         BNE   SEND430                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESADTM+2,WORK                   
         CLC   WORK(4),ESESATIM                                                 
         BL    SEND440                                                          
SEND430  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSAT)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(8),ESENAME                                                  
         MVC   P+22(40),=CL40'ESS SESSION ACTIVITY TIME OUT'                    
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+18(8),ESEELU                                              
         TM    ESETRAC,GESSTSES                                                 
         BZ    SEND432                                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         B     SEND434                                                          
SEND432  EQU   *                                                                
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
SEND434  EQU   *                                                                
         MVC   WTOMSG+18(8),SPACES                                              
         MVC   WTOMSG+26(2),SPACES                                              
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
         B     SEND020                                                          
*                                                                               
SEND440  EQU    *                                                               
         B     SEND020                                                          
*                                                                               
SEND500  EQU   *                                                                
         OC    ESESADTM,ESESADTM                                                
         BZ    SEND530                                                          
         OC    ESESATIM,ESESATIM                                                
         BZ    SEND530                                                          
         CLC   ESESADTM(2),TODAY2                                               
         BNE   SEND520                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESADTM+2,WORK                   
         CLC   WORK(4),ESESATIM                                                 
         BL    SEND530                                                          
SEND520  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSAT)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(8),ESENAME                                                  
         MVC   P+22(40),=CL40'ESS SESSION ACTIVITY TIME OUT'                    
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+18(8),ESEELU                                              
         TM    ESETRAC,GESSTSES                                                 
         BZ    SEND522                                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         B     SEND524                                                          
SEND522  EQU   *                                                                
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
SEND524  EQU   *                                                                
         MVC   WTOMSG+18(8),SPACES                                              
         MVC   WTOMSG+26(2),SPACES                                              
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
         B     SEND020                                                          
*                                                                               
SEND530  EQU    *                                                               
         B     SEND020                                                          
*                                                                               
SEND900  EQU    *                                                               
         MVC   P(40),=CL40'NO SUB TASK CURRENTLY AVAILABLE'                     
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         B     SENDOK                                                           
*                                                                               
SENDER1  MVC   P(8),EATNAME                                                     
         MVC   P+10(35),=CL35'HOSTESS SENDER EIOPQR ERROR'                      
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   ESESECOD,=AL4(0)                                                 
         B     SENDERX                                                          
*                                                                               
SENDER2  MVC   P(8),EATNAME                                                     
         MVC   P+10(35),=CL35'HOSTESS SENDER SQL SUBSYSTEM ERROR'               
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   ESESECOD,=AL4(0)                                                 
         B     SENDERX                                                          
*                                                                               
SENDERX  EQU   *                   SWAP OUT SUB TASK AND CLOSE APPC             
         GOTO1 =A(SWAPOUT),(RC)                                                 
         GOTO1 =A(LOGERROR),(RC)                                                
         B     SEND020                                                          
*                                                                               
SENDOK   B     YES                                                              
SENDNO   B     NO                                                               
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
*  WAIT ON EVENT ECB LIST. EVENTS INCLUDE:                            *         
*  2) DETECT ESSLURCV RECEIVE ALLOCATES POSTED                        *         
*  3) DETECT ESSLUSND RECEIVE ALLOCATES POSTED                        *         
*  1) DETECT OPERATOR CONSOLE COMMUNICATIONS.                         *         
*  4) WAIT ON EACH ESS SESSION APPC ECB.                              *         
*  5) WAIT ON EACH ESSIO ATTACHED SUB TASK ECB.                       *         
*  6) TIMEOUT. WHEN EXPIRED EXIT AND RETURN THROUGH MAIN CONTROL LOOP *         
***********************************************************************         
WAITECB  NTR1                                                                   
*                                                                               
         L     R2,=A(ECBLIST)      WAIT ON ECB LIST                             
         WAIT  1,ECBLIST=(R2)                                                   
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PROCESS RECEIVE ALLOCATE ECB POSTED BY ESSLURCV                     *         
***********************************************************************         
PROCRRAL NTR1                                                                   
*                                                                               
         MVC   P(40),=CL40'PROCESS RECEIVE_ALLOCATE FROM ESSLURCV'              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         BAS   RE,GETSUB                                                        
* ??     BNE   PRRAOK                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AESSATC                                                       
         USING ESSATCD,R3                                                       
         ICM   R6,15,=A(RCVAPPCC)                                               
         USING ESSAPPCD,R6                                                      
*                                                                               
         ICM   RE,15,REASON_CODE                                                
         ICM   RF,15,RTN_CODE                                                   
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TP_NAME,ESSLURCV                                                 
         GOTO1 =A(CONNESM),(RC)                                                 
         BNE   PRRAREJ                                                          
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         MVI   EATMODE,EATMCONQ                                                 
         MVI   EATSTATE,EATSRQPQ                                                
*                                                                               
         ICM   RF,15,ESEADATA                                                   
         STCM  RF,15,EATDATA                                                    
         ICM   RF,15,ESEAWORK                                                   
         STCM  RF,15,EATWORK                                                    
         ICM   RF,15,ESEAPQB                                                    
         STCM  RF,15,EATPQBUF                                                   
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBGTA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
         OC    RTN_CODE,RTN_CODE                                                
         BNZ   PRRAREJ                                                          
         MVC   TP_NAME,ESSLURCV    GTA2 BUG ??                                  
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         POST  EATMTECB                                                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'RECEIVE ALLOCATE REQUEST POSTED'                  
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         B     PRRAEXIT                                                         
*                                                                               
PRRAREJ  EQU   *                   REJECT INCOMING CONVERSATION                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   DEALLOCATE_SENSE_CODE,=XL4'084C0000'                             
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBRJC2 '                                          
         GOTO1 VESSAPPC,DMCB,(R3),(R6)                                          
         OC    RTN_CODE,RTN_CODE                                                
         BZ    PRRAEXIT                                                         
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     PRRANO                                                           
*                                                                               
PRRAEXIT EQU   *                                                                
         XC    RCVAECB,RCVAECB                                                  
         GOTO1 =A(RALRAPPC),(RC)   ESSLURCV RECEIVE ALLOCATE APPC/MVS           
         BNE   PRRANO                                                           
         B     PRRAOK                                                           
*                                                                               
PRRANO   B     NO                                                               
PRRAOK   B     YES                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS RECEIVE ALLOCATE ECB POSTED BY ESSLUSND                     *         
***********************************************************************         
PROCSRAL NTR1                                                                   
*                                                                               
         MVC   P(40),=CL40'PROCESS RECEIVE_ALLOCATE FROM ESSLUSND'              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         BAS   RE,GETSUB                                                        
* ??     BNE   PSRAOK                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AESSATC                                                       
         USING ESSATCD,R3                                                       
         ICM   R6,15,=A(SNDAPPCC)                                               
         USING ESSAPPCD,R6                                                      
*                                                                               
         ICM   RE,15,REASON_CODE                                                
         ICM   RF,15,RTN_CODE                                                   
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TP_NAME,ESSLUSND                                                 
         GOTO1 =A(CONNESS),(RC)                                                 
         BNE   PSRAREJ                                                          
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         MVI   EATMODE,EATMCONQ                                                 
         MVI   EATSTATE,EATSRQPQ                                                
*                                                                               
         ICM   RF,15,ESEADATA                                                   
         STCM  RF,15,EATDATA                                                    
         ICM   RF,15,ESEAWORK                                                   
         STCM  RF,15,EATWORK                                                    
         ICM   RF,15,ESEAPQB                                                    
         STCM  RF,15,EATPQBUF                                                   
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBGTA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
         OC    RTN_CODE,RTN_CODE                                                
         BNZ   PSRAREJ                                                          
         MVC   TP_NAME,ESSLUSND    GTA2 BUG ??                                  
         GOTO1 =A(PRNTATTB),(RC)   PRINT CONVERSATION ATTRIBUTES                
*                                                                               
         POST  EATMTECB                                                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'RECEIVE ALLOCATE REQUEST POSTED'                  
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         B     PSRAEXIT                                                         
*                                                                               
PSRAREJ  EQU   *                   REJECT INCOMING CONVERSATION                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   DEALLOCATE_SENSE_CODE,=XL4'084C0000'                             
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBRJC2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
         OC    RTN_CODE,RTN_CODE                                                
         BZ    PSRAEXIT                                                         
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     PSRANO                                                           
*                                                                               
PSRAEXIT EQU   *                                                                
         XC    SNDAECB,SNDAECB                                                  
         GOTO1 =A(RALSAPPC),(RC)   ESSLUSND RECEIVE ALLOCATE APPC/MVS           
         BNE   PSRANO                                                           
         B     PSRAOK                                                           
*                                                                               
PSRANO   B     NO                                                               
PSRAOK   B     YES                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS ESSIO SUB TASK EVENT ECB LIST                               *         
***********************************************************************         
PROCSUB  NTR1                                                                   
*                                                                               
         L     R2,AESUBECB         A(ESSIO SUB TASK CONTROL ECB LIST)           
         L     R3,=A(EATCBF)       A(FIRST ENTRY IN EATCB)                      
         USING ESSATCD,R3                                                       
*                                                                               
PSUB010  EQU   *                                                                
         TM    0(R2),X'80'         TEST LAST IN ECB LIST                        
         BO    PSUBOK                                                           
         L     R4,0(R2)            GET A(OPERATING SYSTEM ECB)                  
         TM    0(R4),X'40'         TEST ECB POSTED                              
         BZ    PSUB020                                                          
         MVI   0(R4),X'00'         CLEAR ECB POST                               
         OC    0(4,R4),0(R4)       TEST RETURN CODE IN ECB                      
         BZ    PSUB130             REATTACH ESSIO SHUTDOWN                      
         B     PSUB100                                                          
*                                                                               
PSUBNXT  LA    R2,8(R2)            GET NEXT SUBTASK ECBS FROM LIST              
         LA    R3,EATLQ(R3)                                                     
         B     PSUB010                                                          
*                                                                               
PSUB020  L     R4,4(R2)            GET A(ESSIO SUB TASK ECB)                    
         TM    0(R4),X'40'         TEST ECB POSTED                              
         BZ    PSUBNXT                                                          
         MVI   0(R4),X'00'                                                      
         MVC   ECBVALUE,0(R4)      SAVE ECB VALUE                               
         XC    0(4,R4),0(R4)       CLEAR ECB                                    
         BAS   RE,POSTREQ                                                       
         BNE   PSUBNO                                                           
         B     PSUBNXT                                                          
*                                                                               
PSUB100  MVC   P(40),=CL40'ESSIOABEND'                                          
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   P(8),EATNAME                                                     
         MVC   P+10(33),=CL33'SUB TASK FAILURE CONDITION CODE: '                
         GOTO1 VHEXOUT,DMCB,0(R4),P+45,4,=C'TOG'                                
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         OC    0(4,R4),0(R4)                                                    
         BNZ   PSUB112                                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         B     PSUB114                                                          
*                                                                               
PSUB112  MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
PSUB114  CLI   EATSTATE,EATSOFFQ                                                
         BNE   PSUB120             DETACH IF ESSIO ATTACHED                     
         XC    0(4,R4),0(R4)       CLEAR CONDITION CODE FROM ECB                
         B     PSUBNXT             GET NEXT ECB ENTRY                           
*                                                                               
PSUB120  EQU   *                                                                
         CLC   0(4,R4),=XL4'0000029A' TEST FOR ABEND 666 IN LU62SERV            
         BE    PSUB122                                                          
         CLC   0(4,R4),=XL4'000002C6' TEST FOR ABEND 710 IN ESSIO               
         BE    PSUB122                                                          
         CLC   0(4,R4),=XL4'00D23000' TEST FOR LU6.2 ALLOCATION FAIL            
         BE    PSUB122                                                          
         CLC   0(4,R4),=XL4'000C1000' TEST FOR DC H'0'                          
         BE    PSUB122                                                          
         CLC   0(4,R4),=XL4'000C4000' TEST PGM EXCEPTION? ??                    
         BE    PSUB122                                                          
         CLC   0(4,R4),=XL4'00138000' TEST PGM EXCEPTION? ??                    
         BE    PSUB122                                                          
         CLC   0(4,R4),=XL4'00001000' TEST SYSTEM-001 ERROR (IEC020I)           
         BE    PSUB121                                                          
         B     PSUB130             ELSE RESTART SUB TASK ESSIO                  
*                                                                               
PSUB121  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSIA)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(30),=CL30'SYSTEM-001 IEC020I I/O ABEND'                     
         MVC   P+44(36),=CL36'PLEASE RESTART HOSTESSX AFTER ABEND'              
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         MVC   P+59(8),ESSBJNAM                                                 
         DROP  RF                                                               
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         DC    H'0'                ABEND HOSTESS                                
*                                                                               
*                                  DETACH ESSIO SUBTASK                         
PSUB122  MVC   EATABEND,0(R4)      SAVE ABEND CONDITION CODE IN ATCB            
         XC    0(4,R4),0(R4)       CLEAR CONDITION CODE FROM ECB                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(16),=CL16'SUB TASK FAILURE'                                 
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 =A(DETASK),(RC)     DETACH SUB TASK                              
         B     PSUBNXT                                                          
*                                                                               
PSUB130  EQU   *                                                                
         GOTO1 =A(DETASK),(RC)     DETACH SUB TASK                              
*                                  RE-ATTACH ESSIO SUB TASK                     
         XC    EATOSECB,EATOSECB                                                
         XC    EATMTECB,EATMTECB                                                
         XC    EATSTECB,EATSTECB                                                
         XC    EATSPECB,EATSPECB                                                
*                                                                               
         LA    RF,AESSFACS                                                      
         STCM  RF,15,EATFACS                                                    
*                                                                               
         ICM   RF,15,AESSB                                                      
         STCM  RF,15,EATESSB                                                    
*                                                                               
         LA    R6,EATOSECB                                                      
         ATTACH EPLOC=ESSIONAM,ECB=(R6),PARAM=((R3)),SZERO=NO,         +        
               ESTAI=(SUBTAE,(R3))                                              
*                                                                               
         ST    R1,EATOSTCB                                                      
         OC    EATOSTCB,EATOSTCB                                                
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         WAIT  ECB=EATSTECB                                                     
         XC    EATSTECB,EATSTECB                                                
         MVI   EATSTATE,EATSRDYQ                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(10),=CL10'REATTACHED ESS SUB TASK'                          
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         POST  EATMTECB                                                         
         B     PSUBNXT                                                          
*                                                                               
PSUBNO   B     NO                                                               
PSUBOK   B     YES                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*  PROCESS REQUEST POSTED BY ESSIO SUB TASK                           *         
***********************************************************************         
         USING ESSATCD,R3                                                       
POSTREQ  NTR1                                                                   
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'ESSIO SUB TASK POSTED ECB'                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         L     R5,EATDATA                                                       
         USING ESSDATAD,R5                                                      
         CLC   ECBVALUE,=AL4(EATMCONQ)                                          
         BE    POSTCON                                                          
         CLC   ECBVALUE,=AL4(EATMSNDQ)                                          
         BE    POSTSND                                                          
         CLC   ECBVALUE,=AL4(EATMRCVQ)                                          
         BE    POSTRCV                                                          
         CLC   ECBVALUE,=AL4(EATMERRQ)                                          
         BE    POSTERR                                                          
         CLC   ECBVALUE,=AL4(EATMDISQ)                                          
         BE    POSTDIS                                                          
         CLC   ECBVALUE,=AL4(EATMTSTQ)                                          
         BE    POSTTST                                                          
         DC    H'0'                                                             
*                                                                               
POSTCON  EQU   *                                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'HANDSHAKE CONNECT RESPONSE POSTED'                
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         CLI   ESSDMODE,ESSDMRCQ                                                
         BNE   PCON010                                                          
         MVI   EATMODE,EATMRCVQ                                                 
         BAS   RE,PORAPPC                                                       
         BNE   PREQNO                                                           
*                                                                               
PCON010  EQU   *                                                                
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRDYQ                                                
*                                                                               
         BAS   RE,SENDER           PROCESS ESS SENDER SESSIONS                  
*                                                                               
         B     PREQOK                                                           
*                                                                               
POSTSND  EQU   *                                                                
         CLI   EATSTATE,EATSRSPQ                                                
         BE    PSND010                                                          
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'SENDER REQUEST POSTED'                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   EATSTATE,EATSRSPQ                                                
         BAS   RE,PORAPPC                                                       
         BNE   PREQNO                                                           
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRSPQ                                                
         B     PREQOK                                                           
*                                                                               
PSND010  EQU   *                                                                
         CLI   ESSDBLKD,C'Y'       TEST IF BULK DATA DOWNLOAD                   
         BE    PSND020                                                          
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'SENDER RESPONSE POSTED'                           
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRDYQ                                                
*                                                                               
         BAS   RE,SENDER           PROCESS ESS SENDER SESSIONS                  
*                                                                               
         B     PREQOK                                                           
*                                                                               
PSND020  EQU   *                                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'SENDER BULK DATA DOWNLOAD RESPONSE'               
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   ESEBDNFF,C'Y'                                                    
         GOTO1 =A(BULKDOWN),(RC)                                                
         BNE   PREQOK                                                           
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRQPQ                                                
         B     PREQOK                                                           
*                                                                               
POSTRCV  EQU   *                                                                
         CLI   EATSTATE,EATSRSPQ                                                
         BE    PRCV010                                                          
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'RECEIVER REQUEST POSTED'                          
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   EATSTATE,EATSRSPQ                                                
*                                                                               
         POST  EATMTECB                                                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'RECEIVER REQUEST PROCESSED'                       
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         B     PREQOK                                                           
*                                                                               
PRCV010  EQU   *                                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'RECEIVER RESPONSE POSTED'                         
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         BAS   RE,PORAPPC                                                       
         BNE   PREQNO                                                           
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRDYQ                                                
*                                                                               
         BAS   RE,SENDER           PROCESS ESS SENDER SESSIONS                  
*                                                                               
         B     PREQOK                                                           
*                                                                               
POSTERR  EQU   *                                                                
         CLI   ESEMODE,ESEMTSTQ                                                 
         BNE   *+8                                                              
         MVI   ESEMODE,ESEMSNDQ                                                 
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'ESS PROCESSING ERROR POSTED'                      
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 =A(SWAPOUT),(RC)                                                 
         GOTO1 =A(LOGERROR),(RC)                                                
         B     PREQOK                                                           
*                                                                               
POSTDIS  EQU   *                                                                
         CLI   ESEMODE,ESEMTSTQ                                                 
         BNE   *+8                                                              
         MVI   ESEMODE,ESEMSNDQ                                                 
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'APPC DISCONNECT RESPONSE POSTED'                  
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESNALQ                                                
         B     PREQOK                                                           
*                                                                               
POSTTST  EQU   *                                                                
         CLI   EATSTATE,EATSRSPQ                                                
         BE    PTST010                                                          
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'TEST MESSAGE REQUEST POSTED'                      
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   EATSTATE,EATSRSPQ                                                
         BAS   RE,PORAPPC                                                       
         BNE   PREQNO                                                           
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRSPQ                                                
         MVI   ESEMODE,ESEMTSTQ                                                 
         B     PREQOK                                                           
*                                                                               
PTST010  EQU   *                                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(30),=CL30'TEST MESSAGE RESPONSE POSTED'                     
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESRDYQ                                                
         MVI   ESEMODE,ESEMSNDQ                                                 
*                                                                               
         BAS   RE,SENDER           PROCESS ESS SENDER SESSIONS                  
*                                                                               
         B     PREQOK                                                           
*                                                                               
PREQNO   B     NO                                                               
PREQOK   B     YES                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* PROCESS ESS SESSION APPC ECB LIST                                   *         
***********************************************************************         
PROCSESS NTR1                                                                   
*                                                                               
         L     R2,ASESSECB         A(ESS SESSION CONTROL ECB LIST)              
         L     R4,=A(ESECBF)       A(FIRST ENTRY IN ESECB)                      
         USING ESSESSD,R4                                                       
*                                                                               
PSES010  EQU   *                                                                
         C     R2,AESUBECB                                                      
         BNL   PSESOK                                                           
         TM    0(R2),X'80'         TEST LAST IN ECB LIST                        
         BZ    *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R2)            GET A(ESS APPC ECB)                          
         TM    0(RF),X'40'         TEST ECB POSTED                              
         BO    PSES030                                                          
*                                                                               
PSES020  LA    R2,4(R2)            GET NEXT SESSION ECB FROM LIST               
         LA    R4,ESELQ(R4)                                                     
         B     PSES010                                                          
*                                                                               
PSES030  XC    0(4,RF),0(RF)       CLEAR ECB                                    
         USING ESSATCD,R3                                                       
         CLI   ESEMODE,ESEMTSTQ                                                 
         BE    PSES040                                                          
         CLI   ESEMODE,ESEMRCVQ                                                 
         BE    PSES040                                                          
         CLI   ESEMODE,ESEMSNDQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
PSES032  L     R5,ESEADATA                                                      
         USING ESSDATAD,R5                                                      
         CLI   ESESTATE,ESESRSPQ                                                
         BE    PSES040                                                          
         CLI   ESSDBLKD,C'Y'                                                    
         BNE   PSESER1                                                          
         LA    R3,DUMMYTCB                                                      
         STCM  R3,15,AESSATC                                                    
         STCM  R3,15,ESEAEATC                                                   
         STCM  R4,15,EATAESS                                                    
         GOTO1 =A(SWAPIN),(RC)                                                  
         BNE   PSESNO                                                           
         MVI   EATMODE,EATMSNDQ                                                 
         MVI   EATSTATE,EATSRQPQ                                                
         MVI   ESESTATE,ESESRQPQ                                                
         GOTO1 =A(BULKDOWN),(RC)                                                
         BNE   PSES020                                                          
         GOTO1 =A(SWAPOUT),(RC)                                                 
         CLI   ESSDSEQN,ESSDSLSQ                                                
         BNE   PSES020                                                          
*                                                                               
PSES040  EQU   *                                                                
         BAS   RE,GETSUB                                                        
* ??     BNE   PSESOK                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,AESSATC                                                       
         STCM  R3,15,ESEAEATC                                                   
         STCM  R4,15,EATAESS                                                    
         GOTO1 =A(SWAPIN),(RC)                                                  
         BNE   PSESNO                                                           
         CLI   ESEMODE,ESEMRCVQ                                                 
         BNE   PSES050                                                          
         MVI   EATMODE,EATMRCVQ                                                 
         MVI   EATSTATE,EATSRQPQ                                                
         MVI   ESESTATE,ESESRQPQ                                                
         B     PSES100                                                          
*                                                                               
PSES050  CLI   ESEMODE,ESEMSNDQ                                                 
         BNE   PSES060                                                          
         MVI   EATMODE,EATMSNDQ                                                 
         CLI   ESSDSEQN,ESSDSLSQ                                                
         BE    PSES052                                                          
         MVI   EATSTATE,EATSRSPQ                                                
         MVI   ESESTATE,ESESRSPQ                                                
         B     PSES100                                                          
PSES052  MVI   EATSTATE,EATSRQPQ                                                
         MVI   ESESTATE,ESESRQPQ                                                
         B     PSES100                                                          
*                                                                               
PSES060  CLI   ESEMODE,ESEMTSTQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   EATMODE,EATMTSTQ                                                 
         MVI   EATSTATE,EATSRSPQ                                                
         MVI   ESESTATE,ESESRSPQ                                                
         B     PSES100                                                          
*                                                                               
PSES100  EQU   *                                                                
         POST  EATMTECB                                                         
*                                                                               
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'RECEIVE ECB PROCESSED'                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
*                                                                               
         B     PSES020                                                          
*                                                                               
PSESER1  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSSE)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(8),ESENAME                                                  
         MVC   P+22(20),=CL20'SESSION ERROR -'                                  
         MVC   P+39(40),=CL40'CHECK ESS CM/2 TPNAME CONFIGURATION'              
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   ESESECOD,=AL4(0)                                                 
         GOTO1 =A(LOGERROR),(RC)                                                
         B     PSES020                                                          
*                                                                               
PSESNO   B     NO                                                               
PSESOK   B     YES                                                              
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* POST ON RECEIVE APPC CALL                                           *         
***********************************************************************         
         USING ESSATCD,R3                                                       
PORAPPC  NTR1                                                                   
         MVC   P(40),=CL40'POST ON RECEIVE APPC/MVS'                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   PREP_TO_RECEIVE_TYPE,ATB_PREP_TO_RECEIVE_FLUSH                   
         MVC   LOCKS,ATB_LOCKS_SHORT                                            
*                                                                               
         MVC   APPCACTN,=CL8'ATBPTR  '                                          
         GOTO1 VESSAPPC,DMCB,(R3),(R6)                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   PORAER                                                           
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBPOR2 '                                          
         GOTO1 VESSAPPC,DMCB,(R3),(R6)                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BE    PORAOK                                                           
         B     PORAER                                                           
*                                                                               
PORAER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     PORANO                                                           
*                                                                               
PORANO   B     NO                                                               
PORAOK   B     YES                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ASYNCHRONOUS RECEIVE CALL                                           *         
***********************************************************************         
         USING ESSATCD,R3                                                       
ASYNCRCV NTR1                                                                   
         MVC   P(40),=CL40'ASYNCHRONOUS RECEIVE CALL'                           
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   PREP_TO_RECEIVE_TYPE,ATB_PREP_TO_RECEIVE_FLUSH                   
         MVC   LOCKS,ATB_LOCKS_SHORT                                            
*                                                                               
         MVC   APPCACTN,=CL8'ATBPTR  '                                          
         GOTO1 VESSAPPC,DMCB,(R3),(R6)                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   ARCVER                                                           
*                                                                               
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,ATB_FW0                                           
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         LA    RF,ESEAPPCE                                                      
         STCM  RF,15,NOTIFY_TYPE_AECB                                           
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVW '                                          
*                                                                               
         GOTO1 VESSAPPC,DMCB,(R3),(R6)                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BE    ARCVOK                                                           
         B     ARCVER                                                           
*                                                                               
ARCVER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     ARCVNO                                                           
*                                                                               
ARCVNO   B     NO                                                               
ARCVOK   B     YES                                                              
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* DUMP REQUEST QUEUE TO HOSTRCV RECOVERY FILE                         *         
***********************************************************************         
DUMPRQS  NTR1                                                                   
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
*                                  TEST FOR REBUILD=RESTART/START               
         TM    ESSBSTA1,ESSBSRBS+ESSBSRBR                                       
         BZ    DRQSX                                                            
         DROP  RF                                                               
         MVC   P(40),=CL40'DUMPING REQUESTS TO HOSTRCV'                         
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         LA    R2,HOSTRCV                                                       
         OPEN  ((R2),OUTPUT)                                                    
*                                                                               
DRQS010  EQU   *                                                                
*                                                                               
         LAY   R6,IO                                                            
         USING ESSREQD,R6                                                       
         XC    ESSRKEY,ESSRKEY     CLEAR KEY AREA                               
         GOTO1 VESSRQS,DMCB,=AL4(ERQARHIQ),(R6)                                 
         CLI   0(R1),ERQROKQ                                                    
         BE    DRQS030                                                          
         CLI   0(R1),ERQREOFQ                                                   
         BE    DRQS100             EXIT AT END OF TABLE                         
         DC    H'0'                                                             
*                                                                               
DRQS020  EQU   *                   GET NEXT RECORD FROM TSAR                    
         GOTO1 VESSRQS,DMCB,=AL4(ERQARSQQ),(R6)                                 
         CLI   0(R1),ERQROKQ                                                    
         BE    DRQS030                                                          
         CLI   0(R1),ERQREOFQ                                                   
         BE    DRQS100             EXIT AT END OF TABLE                         
         DC    H'0'                                                             
*                                                                               
DRQS030  SR    RE,RE                                                            
         ICM   RE,3,ESSRRLEN                                                    
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         LA    R2,HOSTRCV                                                       
         LA    R4,IOL                                                           
         PUT   (R2),(R4)                                                        
         B     DRQS020                                                          
*                                                                               
DRQS100  EQU   *                                                                
         LA    R2,HOSTRCV                                                       
         CLOSE ((R2))                                                           
         B     DRQSX                                                            
*                                                                               
DRQSX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* GET A(ESSATC) OF NEXT FREE ESSIO SUB TASK                           *         
***********************************************************************         
GETSUB   NTR1                                                                   
         L     R3,=A(EATCBF)       A(FIRST ENTRY IN EATCB)                      
         USING ESSATCD,R3                                                       
*                                  FIND READY SUB TASK CONTROL BLOCK            
GSUB010  L     RF,=A(EATCBTL)                                                   
         L     RF,0(RF)                                                         
         CR    R3,RF               TEST END OF TASK CONTROL BLOCKS              
         BE    GSUBNO                                                           
         C     R3,=A(EATCBX)                                                    
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   EATSTATE,EATSRDYQ   TEST ESSIO READY FOR ANOTHER REQUEST         
         BE    GSUB030                                                          
GSUB020  LA    R3,EATLQ(R3)        BUMP ATCB POINTER                            
         B     GSUB010             GET NEXT DESTINATION                         
*                                                                               
GSUB030  EQU   *                                                                
         ST    R3,AESSATC                                                       
         B     GSUBOK                                                           
*                                                                               
GSUBNO   B     NO                                                               
GSUBOK   B     YES                                                              
         EJECT                                                                  
         GETEL R4,28,ELCODE                                                     
*                                                                               
YES      SR    RC,RC               EXIT SUBROUTINE WITH CC .EQ.                 
NO       LTR   RC,RC               EXIT SUBROUTINE WITH CC .NE.                 
XIT      XIT1  ,                   EXIT SUBROUTINE                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ASYNCHRONOUS TIMER EXIT ROUTINE                                     *         
***********************************************************************         
TIMEOUT  DS    0H                                                               
         STM   RE,RC,12(RD)                                                     
         LR    RB,RF                                                            
         USING TIMEOUT,RB                                                       
         L     R2,4(R1)                                                         
         POST  (R2)                                                             
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,RA,R9,R8                                                      
         EJECT                                                                  
***********************************************************************         
* READ PARAMETER CARDS BETWEEN "++ESS" AND "++ESSEND" CARDS           *         
*                                                                     *         
*   *......          "*" IN COLUMN ONE IS A COMMENT CARD              *         
*                                                                     *         
*   WAITSECS=N       WAIT N SECONDS BETWEEN PQ SEARCHES (DEFAULT = 60)*         
*   FTP=YES          USE NFTP FILE TRANSFER ELSE BULK DOWNLOAD        *         
*   DDSIO=           USE TEST VERSION OF DDSIO                        *         
*   DSPACE=          REQUIRED                                                   
*   WRITE=           DEFAULT IS YES                                             
*   TEST=            GENERAL TEST CARD CHARACTER USED WITH ESSIO      *         
*   RECOMMIT=        ACTIVATE RECOMMIT EXTRACT FILE FUNCTION          *         
*   TASKSIZE=        ESSIO SUB TASK POOL SIZE                         *         
*   APPCLUID=        APPC/MVS HOSTESS LUID                            *         
*   TST=             TST=Y SPECIFIES USING FACPAK TST FILES           *         
*                                                                     *         
***********************************************************************         
READCRDS NMOD1 0,**RCRD**                                                       
         LR    RC,R1                                                            
*                                                                               
RC10     GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'DSPACE=',CARD                                                 
         BNE   RC12                                                             
         L     RE,=A(SSB)                                                       
         USING SSBD,RE                                                          
         MVC   SSODSPAC,CARD+7     SET DSPACE                                   
         DROP  RE                                                               
         B     RC10                                                             
*                                                                               
RC12     CLC   =C'DDSIO=',CARD     DDSIO=                                       
         BNE   RC15                                                             
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         JZ    *+2                                                              
         MVC   0(8,RF),CARD+6                                                   
         B     RC10                                                             
*                                                                               
RC15     CLC   =C'++ESS',CARD      LOOK FOR START OF PARAMETERS                 
         BNE   RC10                                                             
*                                                                               
RC20     GOTO1 VCARDS,DMCB,CARD,=C'RE00'                                        
         CLC   =C'/*',CARD         END-OF-FILE?                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   =C'++ESSEND',CARD   LOOK FOR END OF PARAMETERS                   
         BE    RCX                                                              
*                                                                               
         MVC   P(80),CARD          PRINT ALL PARAMETER CARDS                    
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         CLI   CARD,C'*'           COMMENT CARD?                                
         BE    RC20                YES                                          
*                                                                               
         CLC   =C'WAITSECS=',CARD  WAITSECS=N                                   
         BNE   RC30                                                             
         GOTO1 VNUMVAL,DMCB,CARD+9,(2,0)                                        
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         MH    R1,=H'100'          SCALE THE TIME INTERVAL FOR STIMER           
         ST    R1,WAITSECS                                                      
         B     RC20                                                             
*                                                                               
RC30     CLC   =C'FTP=',CARD       FTP=YES                                      
         BNE   RC40                                                             
         CLC   =C'YES',CARD+4                                                   
         BE    RC20                FTP=YES IS THE DEFAULT                       
         CLC   =C'NO',CARD+4                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         NI    ESSBSTA1,X'FF'-ESSBSFTP   FTPFLAG=ON                             
         DROP  RF                                                               
         B     RC20                                                             
*                                                                               
RC40     EQU   *                                                                
         CLC   =C'PATCH=',CARD     PATCH=                                       
         BNE   RC50                                                             
         XC    FULL,FULL           GET DISPLACEMENT INTO FULL                   
         GOTO1 VHEXIN,DMCB,CARD+6,FULL+1,6                                      
         CLC   DMCB+12(4),=F'3'                                                 
         BE    *+6                                                              
         DC    H'00'                PATCH=111111 11                             
         LA    R2,CARD+71           FIND LENGTH OF PATCH DATA                   
         LH    RE,=H'-1'                                                        
         LA    RF,CARD+12                                                       
         CLI   0(R2),C' '                                                       
         BNE   *+10                                                             
         BXH   R2,RE,*-8                                                        
         DC    H'00'                                                            
         SR    R2,RF               L'PATCH DATA IN R2                           
         GOTO1 VHEXIN,DMCB,CARD+13,WORK,(R2)                                    
         ICM   R1,15,DMCB+12       GET L'HEX PATCH DATA IN R1                   
         BNZ   *+6                 ZERO IS NOT ALLOWED                          
         DC    H'00'                                                            
         BCTR  R1,0                                                             
         L     RF,FULL             PATCH DISPLACEMENT IN RF                     
         A     RF,RBSAVE           RF = A(AREA TO BE PATCHED)                   
         EX    R1,*+8              MOVE IN THE PATCH DATA                       
         B     RC20                                                             
         MVC   0(0,RF),WORK                                                     
*                                                                               
RC50     CLC   =C'WRITE=',CARD                                                  
         BNE   RC60                                                             
         MVC   WRITE,CARD+6                                                     
         B     RC20                                                             
*                                                                               
RC60     EQU   *                                                                
         CLC   =C'TEST=',CARD      TEST=                                        
         BNE   RC70                                                             
         MVC   TESTCHAR,CARD+5                                                  
         CLI   TESTCHAR,C'A'                                                    
         BE    RC62                                                             
         CLI   TESTCHAR,C'B'                                                    
         BE    RC62                                                             
         CLI   TESTCHAR,C'C'                                                    
         BE    RC62                                                             
         CLI   TESTCHAR,C'N'                                                    
         BE    RC62                                                             
         DC    H'0'                                                             
RC62     MVC   ESSIONAM+5(1),TESTCHAR                                           
         B     RC20                                                             
*                                                                               
RC70     EQU   *                                                                
         CLC   =C'RECOMMIT=',CARD                                               
         BNE   RC80                                                             
         CLI   CARD+9,C'Y'                                                      
         BE    RC72                                                             
         CLI   CARD+9,C'N'                                                      
         BE    RC20                                                             
         DC    H'0'                                                             
RC72     L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         OI    ESSBSTA1,ESSBSRCO   RECOMMIT=YES                                 
         DROP  RF                                                               
         B     RC20                                                             
*                                                                               
RC80     EQU   *                                                                
         CLC   =C'TRACE=',CARD                                                  
         BNE   RC90                                                             
         GOTO1 VHEXIN,DMCB,CARD+6,BYTE,2,0                                      
         CLC   12(4,R1),=F'1'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         MVC   ESSBTRAC,BYTE                                                    
         DROP  RF                                                               
         B     RC20                                                             
*                                                                               
RC90     CLC   =C'COMPRESS=',CARD  COMPRESS=YES                                 
         BNE   RC100                                                            
         CLC   =C'NO',CARD+9                                                    
         BE    RC20                COMPRESS=NO IS THE DEFAULT                   
         CLC   =C'YES',CARD+9                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         OI    ESSBSTA1,ESSBSCOM   COMPRESS=ON                                  
         DROP  RF                                                               
         B     RC20                                                             
*                                                                               
RC100    EQU   *                                                                
         CLC   =C'ESTAE=',CARD     ESTAE=YES                                    
         BNE   RC110                                                            
         CLI   CARD+6,C'Y'                                                      
         BE    RC102                                                            
         CLI   CARD+6,C'N'                                                      
         BE    RC20                                                             
         DC    H'0'                                                             
RC102    L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         OI    ESSBSTA1,ESSBSEST   ESTAE=YES                                    
         DROP  RF                                                               
         B     RC20                                                             
*                                                                               
RC110    CLC   =C'REBUILD=',CARD   REBUILD=START/RESTART                        
         BNE   RC120                                                            
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         CLC   =C'NO',CARD+8                                                    
         BE    RC20                REBUILD=NO IS THE DEFAULT                    
         CLC   =C'START',CARD+8                                                 
         BE    RC112                                                            
         CLC   =C'RESTART',CARD+8                                               
         BE    RC114                                                            
         DC    H'0'                                                             
RC112    OI    ESSBSTA1,ESSBSRBS                                                
         B     RC20                                                             
RC114    OI    ESSBSTA1,ESSBSRBR                                                
         B     RC20                                                             
         DROP  RF                                                               
*                                                                               
RC120    CLC   =C'HOSTESS=',CARD   HOSTESS=<INSTANCE CODE>                      
         BNE   RC130                                                            
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         CLI   CARD+8,C'A'                                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLI   CARD+8,C'Z'                                                      
         BNH   RC122                                                            
         CLI   CARD+8,C'0'                                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLI   CARD+8,C'9'                                                      
         BNH   RC122                                                            
         DC    H'0'                                                             
RC122    MVC   ESSBCODE,CARD+8                                                  
         B     RC20                                                             
         DROP  RF                                                               
*                                                                               
RC130    CLC   =C'TASKSIZE=',CARD  TASKSIZE=ESSIO SUB TASK POOL SIZE            
         BNE   RC140                                                            
         GOTO1 VNUMVAL,DMCB,CARD+9,(2,0)                                        
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         CLM   R1,3,=AL2(100)                                                   
         BNH   *+6                                                              
         DC    H'0'                TOO MANY                                     
         STCM  R1,3,TASKSIZE                                                    
         B     RC20                                                             
*                                                                               
RC140    CLC   =C'APPCLUID=',CARD  APPCLUID=APPC/MVC HOSTESS LUID               
         BNE   RC150                                                            
         MVC   APPCLUID,CARD+9                                                  
         B     RC20                                                             
*                                                                               
RC150    CLC   =C'RECOVERY=',CARD  RECOVERY=NO                                  
         BNE   RC160                                                            
         CLI   CARD+9,C'N'                                                      
         BNE   RC20                                                             
         L     RF,=A(SSB)                                                       
         OI    3(RF),X'02'   SET OFFLINE RECOVERY OFF                           
         NI    3(RF),X'FF'-X'08'                                                
         B     RC20                                                             
*                                                                               
RC160    EQU   *                                                                
         CLC   =C'TST=',CARD       TST=YES USING FACPAK TST FILES               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   CARD+4,C'Y'                                                      
         BE    RC162                                                            
         CLI   CARD+4,C'N'                                                      
         BE    RC20                                                             
         DC    H'0'                                                             
RC162    L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         OI    ESSBSTA1,ESSBSTST   TST=YES                                      
         DROP  RF                                                               
         B     RC20                                                             
*                                                                               
RCX      MVC   P,SPACES            SKIP A LINE                                  
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REGISTER FOR ALLOCATES WITH APPC/MVS                                *         
***********************************************************************         
RFAAPPC  NMOD1 0,**RFAA**                                                       
         LR    RC,R1                                                            
         MVC   P(40),=CL40'REGISTER FOR ALLOCATES FROM ESSLURCV'                
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         ICM   R6,15,=A(RCVAPPCC)                                               
         USING ESSAPPCD,R6                                                      
*                                                                               
         MVC   SYM_DEST_NAME,SPACES                                             
         LA    RF,L'ESSLURCV                                                    
         STCM  RF,15,TPNAMELN                                                   
         MVC   TP_NAME,ESSLURCV                                                 
         MVC   LOCAL_LU_NAME,SPACES                                             
         MVC   LOCAL_LU_NAME(L'APPCLUID),APPCLUID                               
         MVC   PARTNER_LU_NAME,SPACES                                           
         MVC   USER_ID,SPACES                                                   
         MVC   PROFILE,SPACES                                                   
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
*                                  ISSUE REGISTER FOR ALLOCATE CALL             
*                                                                               
         MVC   APPCACTN,=CL8'ATBRFA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   RFAAER                                                           
*                                                                               
         MVC   APPCRTKN,ALLOCATE_QUEUE_TOKEN                                    
*                                                                               
         MVC   P(40),=CL40'REGISTER FOR ALLOCATES FROM ESSLUSND'                
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         ICM   R6,15,=A(SNDAPPCC)                                               
*                                                                               
         MVC   SYM_DEST_NAME,SPACES                                             
         LA    RF,L'ESSLUSND                                                    
         STCM  RF,15,TPNAMELN                                                   
         MVC   TP_NAME,ESSLUSND                                                 
         MVC   LOCAL_LU_NAME,SPACES                                             
         MVC   LOCAL_LU_NAME(L'APPCLUID),APPCLUID                               
         MVC   PARTNER_LU_NAME,SPACES                                           
         MVC   USER_ID,SPACES                                                   
         MVC   PROFILE,SPACES                                                   
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
*                                  ISSUE REGISTER FOR ALLOCATE CALL             
*                                                                               
         MVC   APPCACTN,=CL8'ATBRFA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   RFAAER                                                           
*                                                                               
         MVC   APPCSTKN,ALLOCATE_QUEUE_TOKEN                                    
         B     RFAAOK                                                           
*                                                                               
RFAAER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
*                                                                               
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSBD)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(40),=CL40'HOSTESS APPC/MVS REGISTER FAILED'                 
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     RFAANO                                                           
*                                                                               
RFAAOK   SR    RC,RC                                                            
RFAANO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* RECEIVE ALLOCATES WITH APPC/MVS FOR ESSLURCV TP NAME                *         
***********************************************************************         
         DS    0D                                                               
RALRAPPC NMOD1 0,**RALR**                                                       
         LR    RC,R1                                                            
         MVC   P(40),=CL40'RECEIVE ALLOCATE APPC/MVS - ESSLURCV'                
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         ICM   R6,15,=A(RCVAPPCC)                                               
         USING ESSAPPCD,R6                                                      
*                                                                               
         MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_WAIT                                
         MVC   TIME_OUT_VALUE,ATB_FW0                                           
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         MVC   ALLOCATE_QUEUE_TOKEN,APPCRTKN                                    
*                                                                               
*                                  ISSUE RECEIVE ALLOCATE CALL                  
*                                                                               
         MVC   APPCACTN,=CL8'ATBRAL2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   RALRER                                                           
         B     RALROK                                                           
*                                                                               
RALRER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     RALRNO                                                           
*                                                                               
RALROK   SR    RC,RC                                                            
RALRNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* RECEIVE ALLOCATES WITH APPC/MVS FOR ESSLUSND TP NAME                *         
***********************************************************************         
RALSAPPC NMOD1 0,**RALS**                                                       
         LR    RC,R1                                                            
*                                                                               
         MVC   P(40),=CL40'RECEIVE ALLOCATE APPC/MVS - ESSLUSND'                
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         ICM   R6,15,=A(SNDAPPCC)                                               
         USING ESSAPPCD,R6                                                      
*                                                                               
         MVC   RECEIVE_ALLOCATE_TYPE,ATBCTS_WAIT                                
         MVC   TIME_OUT_VALUE,ATB_FW0                                           
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         MVC   ALLOCATE_QUEUE_TOKEN,APPCSTKN                                    
*                                                                               
*                                  ISSUE RECEIVE ALLOCATE CALL                  
*                                                                               
         MVC   APPCACTN,=CL8'ATBRAL2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   RALSER                                                           
         B     RALSOK                                                           
*                                                                               
RALSER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     RALSNO                                                           
*                                                                               
RALSOK   SR    RC,RC                                                            
RALSNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* UNREGISTER FOR ALLOCATES WITH APPC/MVS                              *         
***********************************************************************         
URAAPPC  NMOD1 0,**URAA**                                                       
         LR    RC,R1                                                            
         ICM   R6,15,=A(ESSAPPCC)                                               
         USING ESSAPPCD,R6                                                      
         CLC   APPCRTKN,SPACES                                                  
         BE    URAA010                                                          
         MVC   P(40),=CL40'UNREGISTER FOR ALLOCATES - ESSLURCV'                 
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         MVC   ALLOCATE_QUEUE_TOKEN,APPCRTKN                                    
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
*                                  ISSUE UNREGISTER FOR ALLOCATE CALL           
*                                                                               
         MVC   APPCACTN,=CL8'ATBURA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   URAAER                                                           
*                                                                               
URAA010  CLC   APPCSTKN,SPACES                                                  
         BE    URAAOK                                                           
         MVC   P(40),=CL40'UNREGISTER FOR ALLOCATES - ESSLUSND'                 
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         MVC   ALLOCATE_QUEUE_TOKEN,APPCSTKN                                    
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
*                                  ISSUE UNREGISTER FOR ALLOCATE CALL           
*                                                                               
         MVC   APPCACTN,=CL8'ATBURA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   URAAER                                                           
         B     URAAOK                                                           
*                                                                               
URAAER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     URAANO                                                           
*                                                                               
URAAOK   SR    RC,RC                                                            
URAANO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* CLEAN UP OUTSTANDING ASYNC REQUESTS                                 *         
***********************************************************************         
CLNAPPC  NMOD1 0,**CLNA**                                                       
         LR    RC,R1                                                            
         ICM   R6,15,=A(ESSAPPCC)                                               
         USING ESSAPPCD,R6                                                      
         LHI   R2,100              FOR COMPARE LATER, SET TO HIGH CONV#         
         MVC   P(40),=CL40'CLEAN UP OUTSTANDING ASYNC REQUESTS'                 
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
CLNA010  XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   FUNCTION,ATB_FW1                                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBAMR1 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(40),=CL40'OUTSTANDING ASYN CALLS BEFORE CLEAN-UP:'             
         ICM   RF,15,ASYNCHRONOUS_NUMBER                                        
         EDIT  (RF),(5,P+40),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         OC    ASYNCHRONOUS_NUMBER,ASYNCHRONOUS_NUMBER                          
         BZ    CLNAOK              NO OUTSTANDING APPC CALL, JUST EXIT          
*                                                                               
*WAIT AND RETRY, IF CONV# NOT DECREASES, EMAIL WARNING & WTOR.                  
*                                                                               
         C     R2,ASYNCHRONOUS_NUMBER                                           
         BNH   CLNA050                                                          
CLNA030  L     R2,ASYNCHRONOUS_NUMBER         SAVE THIS CONV#                   
         STIMER WAIT,BINTVL=WAITTIME          WAIT AND THEN RETRY               
         B     CLNA010                                                          
*                                  WTOR FOR RETRY OR CANCEL OR CLEANUP          
CLNA050  DS    0H                                                               
         MVC   WORK(80),SPACES                                                  
         ICM   RF,15,ASYNCHRONOUS_NUMBER                                        
         EDIT  (RF),(5,WORK),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   WORK+3(32),=CL32'APPC CONVERSATIONS OUTSTANDING: '               
         MVC   WORK+36(30),=CL30'(R)RETRY,(A)ABEND,(C)CLEAN_UP'                 
         GOTO1 VLOGIO,DMCB,1,(80,WORK)                                          
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)                                           
         CLI   BYTE,C'A'                                                        
         BNE   *+6                                                              
         DC    H'0'                ABEND                                        
         CLI   BYTE,C'C'                                                        
         BNE   CLNA030             RETRY AS DEFAULT                             
*                                                                               
         MVC   WORK(80),SPACES                                                  
         MVC   WORK(40),=CL40'ARE YOUR SURE FOR DOING CLEAN_UP?  Y/N'           
         GOTO1 VLOGIO,DMCB,1,(80,WORK)                                          
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)                                           
         CLI   BYTE,C'Y'                                                        
         BNE   CLNA030             NO - WAIT AND RETRY                          
*                                  YES - DO THE CLEAN UP                        
*                                                                               
         WTO   TEXT=ANTXT01H                                                    
         B     CLNA060                                                          
*                                                                               
ANTXT01H DC    AL2(ANTXT01Q)                                                    
ANTXT01  DC    C'AUTONOTE*US-MF_FAC_NOTIFY:'                                    
         DC    C'ABOUT TO CALL ATBAMR1 CLEAN_UP!'                               
ANTXT01Q EQU   *-ANTXT01                                                        
*                                                                               
CLNA060  DS    0H                                                               
         MVC   P(40),=CL40'CALL ATBAMR1 TO CLEAN UP'                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   FUNCTION,ATB_FW2                                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBAMR1 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   CLNA080                                                          
         MVC   P(40),=CL40'APPC CLEAN UP COMPLETED OK'                          
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
CLNA080  XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   FUNCTION,ATB_FW1                                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBAMR1 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   CLNAOK                                                           
         MVC   P(40),=CL40'OUTSTANDING ASYN CALLS AFTER CLEAN-UP:'              
         ICM   RF,15,ASYNCHRONOUS_NUMBER                                        
         EDIT  (RF),(5,P+40),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         B     CLNAOK                                                           
*                                                                               
CLNAOK   SR    RC,RC                                                            
CLNANO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SHUT DOWN SYSTEM SUB TASKS                                          *         
***********************************************************************         
SHUTDOWN NMOD1 0,**SDWN**                                                       
         LR    RC,R1                                                            
         MVC   P(40),=CL40'SHUTDOWN SUB TASKS'                                  
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
* READ EXTRACT TRANSFER SYSTEM ATTACHED TASK CONTROL BLOCKS AND                 
* DETACH SUB TASKS                                                              
*                                                                               
         L     R3,=A(EATCBF)        A(FIRST ENTRY IN EATCB)                     
         USING ESSATCD,R3                                                       
*                                                                               
SHUT010  L     RF,=A(EATCBTL)                                                   
         C     R3,0(RF)                                                         
         BE    SHUTX                                                            
         CLI   EATSTATE,EATSOFFQ                                                
         BE    SHUT020                                                          
         BAS   RE,SHUTTASK         SHUT DOWN SUB TASK                           
         GOTO1 =A(DETASK),(RC)     DETACH SUB TASK                              
*                                                                               
SHUT020  LA    R3,EATLQ(R3)        BUMP EATCB POINTER                           
         B     SHUT010             GET NEXT ENTRYD                              
*                                                                               
SHUTX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SHUT DOWN ESSIO SUB TASK                                            *         
* R3=A(ESSIO ATTACHED TASK CONTROL BLOCK)                             *         
***********************************************************************         
SHUTTASK NTR1                                                                   
         USING ESSATCD,R3                                                       
*                                                                               
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'WAITING FOR SHUTDOWN'                             
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                  NOP WHILE FREDS OPEN PROBLEM                 
         POST  EATSPECB            TELL ESSIO TO STOP                           
*                                                                               
         L     R2,=A(SHUTLIST)                                                  
         XC    0(4,R2),0(R2)                                                    
         LA    RF,EATOSECB                                                      
         STCM  RF,7,1(R2)                                                       
         LA    R2,4(R2)                                                         
         XC    0(4,R2),0(R2)                                                    
         XC    TIMERECB,TIMERECB                                                
         LA    RF,TIMERECB                                                      
         STCM  RF,7,1(R2)                                                       
         OI    0(R2),X'80'                                                      
         L     R2,=A(SHUTLIST)                                                  
         WAIT  1,ECBLIST=(R2)                                                   
*                                                                               
         TM    EATOSECB,X'40'      ESSIO TERMINATED?                            
         BO    STAS020             YES                                          
         TM    TIMERECB,X'40'      ESSIO TERMINATED?                            
         BO    *+6                 YES                                          
         DC    H'0'                                                             
         XC    TIMERECB,TIMERECB                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'TIMED OUT SHUTDOWN'                               
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         MVC   ANTXT02A,EATNAME                                                 
         MVI   ANTXT02A+2,C'S'     OVERRIDE THREAD#                             
         WTO   TEXT=ANTXT02H                                                    
         B     STASX                                                            
*                                                                               
ANTXT02H DC    AL2(ANTXT02Q)                                                    
ANTXT02  DC    C'AUTONOTE*US-MF_FAC_NOTIFY,DWHIN:'                              
         DC    C'HOSTESS TIMED OUT, PLEASE RECYCLE '                            
ANTXT02A DC    CL(L'EATNAME)' '                                                 
ANTXT02Q EQU   *-ANTXT02                                                        
*                                                                               
STAS020  XC    EATOSECB,EATOSECB                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(40),=CL40'ACKNOWLEDGED SHUTDOWN'                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
STASX    XIT1                                                                   
         LTORG                                                                  
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SHUTDOWN ESS SESSIONS DISCONNECTING APPC                            *         
***********************************************************************         
SHUTESS  NMOD1 0,**SESS**                                                       
         LR    RC,R1                                                            
         L     R4,=A(ESECBF)                                                    
         USING ESSESSD,R4                                                       
         MVC   P(40),=CL40'SHUT DOWN ESS SESSIONS'                              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         ICM   R6,15,=A(ESSAPPCC)                                               
         USING ESSAPPCD,R6                                                      
*                                                                               
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   FUNCTION,ATB_FW1                                                 
*                                                                               
*NOTE: FIRST CALL TO ATBAMR1 WILL PROHIBIT ANY FURTHER SEND CALL                
*      REGARDLESS OF FUNCTIONS (GET CONV# OR CLEAN UP), 11/11/10,YYUN           
*                                                                               
         MVC   APPCACTN,=CL8'ATBAMR1 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   SESSER                                                           
         MVC   P(40),=CL40'SHUTESS-OUTSTANDING ASYN CALLS:'                     
         ICM   RF,15,ASYNCHRONOUS_NUMBER                                        
         EDIT  (RF),(5,P+32),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                  FIND READY ESS SESSION CONTROL BLOCK         
SESS010  L     RF,=A(ESECBTL)                                                   
         L     RF,0(RF)                                                         
         CR    R4,RF               TEST END OF SESSION CONTROL BLOCKS           
         BE    SESSOK                                                           
         CLI   ESESTATE,ESESNALQ   TEST ESS SESSION ALLOCATED                   
         BNE   SESS030                                                          
*                                                                               
SESS020  LA    R4,ESELQ(R4)        BUMP POINTER                                 
         B     SESS010             GET NEXT                                     
*                                                                               
SESS030  EQU   *                                                                
         GOTO1 =A(CLOSAPPC),(RC)                                                
         B     SESS020                                                          
*                                                                               
SESSER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     SESSNO                                                           
*                                                                               
SESSOK   SR    RC,RC                                                            
SESSNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK ESS SESSIONS BUSY ON SHUTDOWN                                 *         
***********************************************************************         
CHKSESS  NMOD1 0,**CHKS**                                                       
         LR    RC,R1                                                            
         L     R4,=A(ESECBF)                                                    
         USING ESSESSD,R4                                                       
*                                  FIND READY ESS SESSION CONTROL BLOCK         
CHKS010  L     RF,=A(ESECBTL)                                                   
         L     RF,0(RF)                                                         
         CR    R4,RF               TEST END OF SESSION CONTROL BLOCKS           
         BE    CHKSOK                                                           
         CLI   ESEMODE,ESEMSNDQ                                                 
         BNE   CHKS020                                                          
         L     R5,ESEADATA                                                      
         USING ESSDATAD,R5                                                      
         CLI   ESESTATE,ESESRSPQ                                                
         BE    CHKS020                                                          
         CLI   ESSDBLKD,C'Y'                                                    
         BNE   CHKS020                                                          
         MVC   WORK(80),SPACES                                                  
         MVC   WORK(8),ESENAME                                                  
         MVC   WORK+10(8),ESEELU                                                
         MVC   WORK+20(23),=CL23'BUSY SENDING FILE - '                          
         MVC   WORK+43(25),=CL25'CONTINUE SHUTDOWN Y/N?'                        
         GOTO1 VLOGIO,DMCB,1,(80,WORK)                                          
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)                                           
         CLI   BYTE,C'N'                                                        
         BE    CHKSNO                                                           
*                                                                               
CHKS020  LA    R4,ESELQ(R4)        BUMP POINTER                                 
         B     CHKS010             GET NEXT                                     
*                                                                               
CHKSOK   SR    RC,RC                                                            
CHKSNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONNECT ESS RECEIVE ALLOCATE TO ASSOCIATED ESS SESSION CONTROL BLOCK*         
***********************************************************************         
         USING ESSAPPCD,R6                                                      
         USING ESSATCD,R3                                                       
CONNESS  NMOD1 0,**CNCT**                                                       
         LR    RC,R1                                                            
*                                                                               
         MVC   P(40),=CL40'ENTERING CONNESS'                                    
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         L     R4,=A(ESECBF)                                                    
         USING ESSESSD,R4                                                       
CESS010  L     RF,=A(ESECBTL)                                                   
         L     RF,0(RF)                                                         
         CR    R4,RF               TEST END OF SESSION CONTROL BLOCKS           
         BE    CESSER1                                                          
         CLC   TP_NAME,ESSLUSND                                                 
         BE    CESS020                                                          
         CLC   TP_NAME,ESSLURCV                                                 
         BE    CESS030                                                          
         DC    H'0'                                                             
CESS020  CLI   ESEMODE,ESEMRCVQ                                                 
         BE    CESS040                                                          
         B     CESS100                                                          
CESS030  CLI   ESEMODE,ESEMSNDQ                                                 
         BE    CESS040                                                          
         B     CESS100                                                          
*                                                                               
CESS040  MVC   NETWRKID,SPACES                                                  
         LA    RE,NETWRKID                                                      
         LA    RF,ESEENID                                                       
         LA    R0,8                                                             
CESS050  CLI   0(RF),0                                                          
         BE    CESS052                                                          
         CLI   0(RF),C' '                                                       
         BE    CESS052                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CESS050                                                       
CESS052  MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,ESEELU                                                        
         LA    R0,8                                                             
CESS054  CLI   0(RF),0                                                          
         BE    CESS060                                                          
         CLI   0(RF),C' '                                                       
         BE    CESS060                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CESS054                                                       
*                                                                               
CESS060  CLC   NETWRKID,PARTNER_LU_NAME                                         
         BE    CESS200                                                          
         B     CESS100                                                          
*                                                                               
CESS100  LA    R4,ESELQ(R4)                                                     
         B     CESS010                                                          
*                                                                               
CESS200  EQU   *                   CHECK RECONNECT TIME OUT                     
         OC    ESESDDTM,ESESDDTM                                                
         BZ    CESS220                                                          
*        OC    ESESDTIM,ESESDTIM                                                
*        BZ    CESS220                                                          
         CLC   ESESDDTM(2),TODAY2                                               
         BNE   CESS220                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESDDTM+2,WORK                   
         OC    ESESDTIM,ESESDTIM                                                
         BZ    CESS210                                                          
         CLC   WORK(4),ESESDTIM                                                 
         BL    CESSNO                                                           
         B     CESS220                                                          
CESS210  CLC   WORK(4),DCONTOUQ                                                 
         BL    CESSNO                                                           
         B     CESS220                                                          
*                                                                               
CESS220  EQU   *                                                                
         STCM  R4,15,EATAESS                                                    
         STCM  R3,15,ESEAEATC                                                   
*                                                                               
         CLI   ESESTATE,ESESERRQ                                                
         BE    CESS240                                                          
         XC    ESESECNT,ESESECNT                                                
         CLI   ESESTATE,ESESRDYQ                                                
         BE    CESS230                                                          
         CLI   ESESTATE,ESESNALQ                                                
         BE    CESS250                                                          
*                                                                               
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSAB)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(40),=CL40'BUSY SESSION ALLOCATE FROM ESS LU:'               
         MVC   P+47(17),PARTNER_LU_NAME                                         
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     CESSNO                                                           
*                                                                               
CESS230  EQU   *                                                                
         GOTO1 =A(CLOSAPPC),(RC)                                                
         B     CESS250                                                          
*                                                                               
CESS240  EQU   *                   CHECK PROCESS ERROR TIMEOUT                  
         OC    ESESEDTM,ESESEDTM                                                
         BZ    CESS244                                                          
         CLC   ESESEDTM(2),TODAY2                                               
         BNE   CESS244                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESEDTM+2,WORK                   
         CLC   ESESECNT,ERRCMAXQ                                                
         BL    CESS242                                                          
         CLC   WORK(4),SERSTOUQ                                                 
         BL    CESSNO                                                           
         B     CESS244                                                          
CESS242  CLC   WORK(4),SERFTOUQ                                                 
         BL    CESSNO                                                           
         B     CESS244                                                          
CESS244  MVI   ESESTATE,ESESNALQ                                                
         XC    ESESEDTM,ESESEDTM                                                
         B     CESS250                                                          
*                                                                               
         USING ESSAPPCD,R6                                                      
CESS250  ICM   RE,15,ESEAAPPC                                                   
         LA    RE,CONVERSATION_PARAMETERS-ESSAPPCD(RE)                          
         LA    R0,CONVERSATION_PARAMETERS-ESSAPPCD(R6)                          
         ICM   RF,15,=AL4(CPLENGTH)                                             
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         MVC   EATENUM,ESENUM                                                   
         MVC   EATNAME,ESENAME                                                  
         MVC   EATLEV,ESELEV                                                    
         MVC   EATVER,ESEVER                                                    
         MVC   EATEKEY,ESEEKEY                                                  
         MVC   EATEPWD,ESEEPWD                                                  
         MVC   EATFAC,ESEFAC                                                    
         MVC   EATHFLG1,ESEHFLG1                                                
         MVC   EATHFLG2,ESEHFLG2                                                
         MVC   EATTRAC,ESETRAC                                                  
         MVC   EATNFSFR,ESENFSFR                                                
         MVC   EATNFSTO,ESENFSTO                                                
         MVC   EATNFSIP,ESENFSIP                                                
         CLI   ESESTATE,ESESNALQ                                                
         BE    *+14                                                             
         MVC   P(40),=CL40'ESS SESSION RECONNECTED'                             
         B     *+10                                                             
         MVC   P(40),=CL40'ESS SESSION CONNECTED'                               
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+18(8),SPACES                                              
         MVC   ESESDDTM(2),TODAY2                                               
         MVC   ESESDDTM+2(4),MVSTIME                                            
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
         ICM   RF,15,ESESDCNT                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,ESESDCNT                                                   
         MVI   ESESTATE,ESESRSPQ                                                
         B     CESSOK                                                           
*                                                                               
CESSER1  EQU   *                                                                
         L     R2,=A(UNKESST)                                                   
CESE010  CLM   R2,15,=A(UNKESSTX)                                               
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'FF'                                                      
         BE    CESE020                                                          
         CLI   0(R2),0                                                          
         BE    CESE030                                                          
         CLC   0(L'PARTNER_LU_NAME,R2),PARTNER_LU_NAME                          
         BE    CESSNO                                                           
CESE020  LA    R2,L'PARTNER_LU_NAME(R2)                                         
         B     CESE010                                                          
CESE030  L     R2,=A(UNKESST)                                                   
CESE040  CLM   R2,15,=A(UNKESSTX)                                               
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'FF'                                                      
         BE    CESE060                                                          
         CLI   0(R2),0                                                          
         BE    CESE060                                                          
CESE050  LA    R2,L'PARTNER_LU_NAME(R2)                                         
         B     CESE040                                                          
CESE060  EQU   *                                                                
         MVC   0(L'PARTNER_LU_NAME,R2),PARTNER_LU_NAME                          
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSAU)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(40),=CL40'ALLOCATE FROM UNKNOWN ESS LU:'                    
         MVC   P+42(17),PARTNER_LU_NAME                                         
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     CESSNO                                                           
*                                                                               
CESSOK   SR    RC,RC                                                            
CESSNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* CONNECT ESS RECEIVE ALLOCATE TO ASSOCIATED ESS SESSION CONTROL BLOCK*         
* - NEW STYLE FOR MULTIPLE ESS SESSIONS                               *         
***********************************************************************         
         USING ESSAPPCD,R6                                                      
         USING ESSATCD,R3                                                       
CONNESM  NMOD1 0,**CNCT**                                                       
         LR    RC,R1                                                            
*                                                                               
         MVC   P(40),=CL40'ENTERING CONNESM'                                    
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         L     R4,=A(ESECBF)                                                    
         USING ESSESSD,R4                                                       
*                                  FIND ESS SESSION CONTROL BLOCKS              
CESM010  L     RF,=A(ESECBTL)                                                   
         L     RF,0(RF)                                                         
         CR    R4,RF                                                            
         BE    CESMER1                                                          
         CLC   TP_NAME,ESSLUSND                                                 
         BE    CESM020                                                          
         CLC   TP_NAME,ESSLURCV                                                 
         BE    CESM030                                                          
         B     CESMER1                                                          
CESM020  CLI   ESEMODE,ESEMRCVQ                                                 
         BE    CESM040                                                          
         B     CESM070                                                          
CESM030  CLI   ESEMODE,ESEMSNDQ                                                 
         BE    CESM040                                                          
         B     CESM070                                                          
*                                                                               
CESM040  MVC   NETWRKID,SPACES                                                  
         LA    RE,NETWRKID                                                      
         LA    RF,ESEENID                                                       
         LA    R0,8                                                             
CESM050  CLI   0(RF),0                                                          
         BE    CESM052                                                          
         CLI   0(RF),C' '                                                       
         BE    CESM052                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CESM050                                                       
CESM052  MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,ESEELU                                                        
         LA    R0,8                                                             
CESM054  CLI   0(RF),0                                                          
         BE    CESM060                                                          
         CLI   0(RF),C' '                                                       
         BE    CESM060                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,CESM054                                                       
*                                                                               
CESM060  CLC   NETWRKID,PARTNER_LU_NAME                                         
         BE    CESM100                                                          
         B     CESM070                                                          
*                                                                               
CESM070  LA    R4,ESELQ(R4)                                                     
         B     CESM010                                                          
         EJECT                                                                  
CESM100  EQU   *                                                                
         STCM  R4,15,ASESSION      SAVE A(ESSESS SESSIONS)                      
         MVC   SESSLIM,ESESLIM     SAVE ESS SESSION NUMBER LIMIT                
         MVI   SESSNUM,1                                                        
CESM110  L     RF,=A(ESECBTL)      FIND NEXT NOT ALLOCATED SESSION              
         L     RF,0(RF)                                                         
         CR    R4,RF                                                            
         BE    CESM200                                                          
         CLC   NETWRKID,PARTNER_LU_NAME                                         
         BNE   CESM200                                                          
         CLI   ESESTATE,ESESNALQ                                                
         BE    CESM120                                                          
         LA    R4,ESELQ(R4)                                                     
         SR    RF,RF                                                            
         IC    RF,SESSNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,SESSNUM                                                       
         CLC   SESSNUM,SESSLIM                                                  
         BNL   CESM200                                                          
         B     CESM110                                                          
*                                                                               
CESM120  EQU   *                   CHECK RECONNECT TIME OUT                     
         OC    ESESDDTM,ESESDDTM                                                
         BZ    CESM130                                                          
*        OC    ESESDTIM,ESESDTIM                                                
*        BZ    CESM130                                                          
         CLC   ESESDDTM(2),TODAY2                                               
         BNE   CESM130                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESDDTM+2,WORK                   
         OC    ESESDTIM,ESESDTIM                                                
         BZ    CESM122                                                          
         CLC   WORK(4),ESESDTIM                                                 
         BL    CESMNO                                                           
         B     CESM130                                                          
CESM122  CLC   WORK(4),DCONTOUQ                                                 
         BL    CESMNO                                                           
         B     CESM130                                                          
*                                                                               
CESM130  EQU   *                                                                
         XC    ESESECNT,ESESECNT                                                
         STCM  R4,15,EATAESS                                                    
         STCM  R3,15,ESEAEATC                                                   
         B     CESM500                                                          
         EJECT                                                                  
CESM200  EQU   *                                                                
         ICM   R4,15,ASESSION      RESTORE A(ESSESS SESSIONS)                   
         MVI   SESSNUM,1                                                        
CESM210  L     RF,=A(ESECBTL)      FIND NEXT SESSION IN ERROR                   
         L     RF,0(RF)                                                         
         CR    R4,RF                                                            
         BE    CESM300                                                          
         CLC   NETWRKID,PARTNER_LU_NAME                                         
         BNE   CESM300                                                          
         CLI   ESESTATE,ESESERRQ                                                
         BE    CESM220                                                          
         LA    R4,ESELQ(R4)                                                     
         SR    RF,RF                                                            
         IC    RF,SESSNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,SESSNUM                                                       
         CLC   SESSNUM,SESSLIM                                                  
         BNL   CESM300                                                          
         B     CESM210                                                          
*                                                                               
CESM220  EQU   *                                                                
         STCM  R4,15,EATAESS                                                    
         STCM  R3,15,ESEAEATC                                                   
*                                                                               
CESM230  EQU   *                   CHECK PROCESS ERROR TIMEOUT                  
         OC    ESESEDTM,ESESEDTM                                                
         BZ    CESM240                                                          
         CLC   ESESEDTM(2),TODAY2                                               
         BNE   CESM240                                                          
         GOTO1 =A(TIMEDIFF),DMCB,(RC),MVSTIME,ESESEDTM+2,WORK                   
         CLC   ESESECNT,ERRCMAXQ                                                
         BL    CESM232                                                          
         CLC   WORK(4),SERSTOUQ                                                 
         BL    CESMNO                                                           
         B     CESM240                                                          
CESM232  CLC   WORK(4),SERFTOUQ                                                 
         BL    CESMNO                                                           
         B     CESM240                                                          
CESM240  MVI   ESESTATE,ESESNALQ                                                
         XC    ESESEDTM,ESESEDTM                                                
         B     CESM500                                                          
         EJECT                                                                  
CESM300  EQU   *                                                                
         ICM   R4,15,ASESSION      RESTORE A(ESSESS SESSIONS)                   
         MVI   SESSNUM,1                                                        
CESM310  L     RF,=A(ESECBTL)      FIND NEXT READY ALLOCATED SESSION            
         L     RF,0(RF)                                                         
         CR    R4,RF                                                            
         BE    CESM400                                                          
         CLC   NETWRKID,PARTNER_LU_NAME                                         
         BNE   CESM400                                                          
         CLI   ESESTATE,ESESRDYQ                                                
         BE    CESM320                                                          
         LA    R4,ESELQ(R4)                                                     
         SR    RF,RF                                                            
         IC    RF,SESSNUM                                                       
         LA    RF,1(RF)                                                         
         STC   RF,SESSNUM                                                       
         CLC   SESSNUM,SESSLIM                                                  
         BNL   CESM400                                                          
         B     CESM310                                                          
*                                                                               
CESM320  EQU   *                                                                
         XC    ESESECNT,ESESECNT                                                
         STCM  R4,15,EATAESS                                                    
         STCM  R3,15,ESEAEATC                                                   
         GOTO1 =A(CLOSAPPC),(RC)                                                
         B     CESM500                                                          
*                                                                               
CESM400  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSAB)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(40),=CL40'BUSY SESSION ALLOCATE FROM ESS LU:'               
         MVC   P+47(17),PARTNER_LU_NAME                                         
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     CESMNO                                                           
*                                                                               
         USING ESSAPPCD,R6                                                      
CESM500  ICM   RE,15,ESEAAPPC                                                   
         LA    RE,CONVERSATION_PARAMETERS-ESSAPPCD(RE)                          
         LA    R0,CONVERSATION_PARAMETERS-ESSAPPCD(R6)                          
         ICM   RF,15,=AL4(CPLENGTH)                                             
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         MVC   EATENUM,ESENUM                                                   
         MVC   EATNAME,ESENAME                                                  
         MVC   EATLEV,ESELEV                                                    
         MVC   EATVER,ESEVER                                                    
         MVC   EATEKEY,ESEEKEY                                                  
         MVC   EATEPWD,ESEEPWD                                                  
         MVC   EATFAC,ESEFAC                                                    
         MVC   EATHFLG1,ESEHFLG1                                                
         MVC   EATHFLG2,ESEHFLG2                                                
         MVC   EATTRAC,ESETRAC                                                  
         MVC   EATNFSFR,ESENFSFR                                                
         MVC   EATNFSTO,ESENFSTO                                                
         MVC   EATNFSIP,ESENFSIP                                                
         CLI   ESESTATE,ESESNALQ                                                
         BE    *+14                                                             
         MVC   P(40),=CL40'ESS SESSION RECONNECTED'                             
         B     *+10                                                             
         MVC   P(40),=CL40'ESS SESSION CONNECTED'                               
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+18(8),SPACES                                              
         MVC   ESESDDTM(2),TODAY2                                               
         MVC   ESESDDTM+2(4),MVSTIME                                            
         MVC   ESESADTM(2),TODAY2                                               
         MVC   ESESADTM+2(4),MVSTIME                                            
         ICM   RF,15,ESESDCNT                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,ESESDCNT                                                   
         MVI   ESESTATE,ESESRSPQ                                                
         B     CESMOK                                                           
*                                                                               
CESMER1  EQU   *                                                                
         L     R2,=A(UNKESST)                                                   
CSME010  CLM   R2,15,=A(UNKESSTX)                                               
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'FF'                                                      
         BE    CSME020                                                          
         CLI   0(R2),0                                                          
         BE    CSME030                                                          
         CLC   0(L'PARTNER_LU_NAME,R2),PARTNER_LU_NAME                          
         BE    CESMNO                                                           
CSME020  LA    R2,L'PARTNER_LU_NAME(R2)                                         
         B     CSME010                                                          
CSME030  L     R2,=A(UNKESST)                                                   
CSME040  CLM   R2,15,=A(UNKESSTX)                                               
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'FF'                                                      
         BE    CSME060                                                          
         CLI   0(R2),0                                                          
         BE    CSME060                                                          
CSME050  LA    R2,L'PARTNER_LU_NAME(R2)                                         
         B     CSME040                                                          
CSME060  MVC   0(L'PARTNER_LU_NAME,R2),PARTNER_LU_NAME                          
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSAU)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   0(L'PARTNER_LU_NAME,R2),PARTNER_LU_NAME                          
         MVC   P+12(40),=CL40'ALLOCATE FROM UNKNOWN ESS LU:'                    
         MVC   P+42(17),PARTNER_LU_NAME                                         
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     CESMNO                                                           
*                                                                               
CESMOK   SR    RC,RC                                                            
CESMNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* SUBTRACT TIME IN P2 HHMMSSTH FORMAT FROM TIME IN P1 HHMMSSTH FORMAT,*         
* RESULT IN P3, HHMMSSTH FORMAT                                       *         
***********************************************************************         
TIMEDIFF NMOD1 0,**TIMD**                                                       
         L     RC,0(R1)                                                         
         LM    R2,R4,4(R1)                                                      
         L     RF,0(R3)                                                         
         SRL   RF,20                                                            
         XC    DUB,DUB                                                          
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         M     RE,=F'360000'                                                    
         ST    RF,0(R4)                                                         
         L     RF,0(R3)                                                         
         SLL   RF,8                                                             
         SRL   RF,20                                                            
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   RF,DUB                                                           
         MH    RF,=H'6000'                                                      
         A     RF,0(R4)                                                         
         ST    RF,0(R4)                                                         
         L     RF,0(R3)                                                         
         SLL   RF,16                                                            
         SRL   RF,12                                                            
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   RF,DUB                                                           
         A     RF,0(R4)                                                         
*                                                                               
         LR    R5,RF                                                            
         L     RF,0(R2)                                                         
         SRL   RF,20                                                            
         XC    DUB,DUB                                                          
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   RF,DUB                                                           
         SR    RE,RE                                                            
         M     RE,=F'360000'                                                    
         ST    RF,0(R4)                                                         
         L     RF,0(R2)                                                         
         SLL   RF,8                                                             
         SRL   RF,20                                                            
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   RF,DUB                                                           
         MH    RF,=H'6000'                                                      
         A     RF,0(R4)                                                         
         ST    RF,0(R4)                                                         
         L     RF,0(R2)                                                         
         SLL   RF,16                                                            
         SRL   RF,12                                                            
         ST    RF,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   RF,DUB                                                           
         A     RF,0(R4)                                                         
         SR    RF,R5                                                            
*                                                                               
         SR    R0,R0                                                            
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         ST    RF,0(R4)                                                         
         XIT1                                                                   
*                                                                               
         CVD   RE,DUB                                                           
         L     RE,DUB+4                                                         
         SRL   RE,4                                                             
         OR    R0,RE                                                            
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'60'                                                        
         CVD   RE,DUB                                                           
         L     RE,DUB+4                                                         
         SRL   RE,4                                                             
         SLL   RE,8                                                             
         OR    R0,RE                                                            
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'60'                                                        
         CVD   RE,DUB                                                           
         L     RE,DUB+4                                                         
         SRL   RE,4                                                             
         SLL   RE,16                                                            
         OR    R0,RE                                                            
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'60'                                                        
         CVD   RE,DUB                                                           
         L     RE,DUB+4                                                         
         SRL   RE,4                                                             
         SLL   RE,24                                                            
         OR    R0,RE                                                            
*                                                                               
         ST    R0,0(R4)                                                         
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CLOSE DOWN AESS APPC SESSION                                        *         
* R4=A(ESSESSD)                                                       *         
***********************************************************************         
         USING ESSESSD,R4                                                       
CLOSAPPC NMOD1 0,**CAPP**                                                       
         LR    RC,R1                                                            
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         MVC   P(8),ESENAME                                                     
         MVC   P+10(40),=CL40'CLOSE DOWN ESS SESSION'                           
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBGTA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   CAPPDEAL                                                         
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    CAPPDEAL                                                         
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    CAPPDEAL                                                         
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    CAPPDEAL                                                         
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    CAPPDEAL                                                         
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    CAPPDEAL                                                         
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    CAPPDEAL                                                         
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    CAPPRCV                                                          
         B     CAPPDEAL                                                         
*                                                                               
CAPPRCV  EQU   *                                                                
         MVC   P(8),ESENAME                                                     
         MVC   P+10(40),=CL40'CLOSE DOWN SESSION IN RECEIVE STATE'              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVI '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    CAPPDEAB                                                         
         CLC   RTN_CODE,ATB_UNSUCCESSFUL                                        
         BE    CAPPDEAA                                                         
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     CAPPDEAA                                                         
*                                                                               
CAPPDEAL XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_FLUSH                             
*                                                                               
         MVC   APPCACTN,=CL8'ATBDEAL '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    CAPPX                                                            
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     CAPPX                                                            
*                                                                               
CAPPSERR EQU   *                                                                
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   ERROR_DIRECTION,ATB_FW0                                          
*                                                                               
         MVC   APPCACTN,=CL8'ATBSERR '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    CAPPDEAA                                                         
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     CAPPX                                                            
*                                                                               
CAPPDEAA XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
         LA    RF,ESEAPPCE                                                      
         STCM  RF,15,NOTIFY_TYPE_AECB                                           
*                                                                               
         MVC   APPCACTN,=CL8'ATBDEAL '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
CAPPDEAB XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
*                                                                               
         MVC   APPCACTN,=CL8'ATBDEAL '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    CAPPX                                                            
         GOTO1 =A(PRNTLUER),(RC)                                                
         B     CAPPX                                                            
*                                                                               
CAPPX    DS    0H                                                               
         MVI   ESESTATE,ESESNALQ   RESET STATUS TO NOT ALLOCATED                
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* LOG SESSION ERROR                                                   *         
***********************************************************************         
         USING ESSESSD,R4                                                       
LOGERROR NMOD1 0,**LOGE**                                                       
         LR    RC,R1                                                            
         MVI   ESESTATE,ESESERRQ                                                
         MVC   ESESEDTM(2),TODAY2                                               
         MVC   ESESEDTM+2(4),MVSTIME                                            
         CLC   ESESECOD,ESESELCD                                                
         BNE   LOGE010                                                          
         ICM   RF,15,ESESECNT                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,15,ESESECNT                                                   
         B     LOGE020                                                          
*                                                                               
LOGE010  XC    ESESECNT,ESESECNT                                                
         MVC   ESESELCD,ESESECOD                                                
         MVC   ESESELDT(2),TODAY2                                               
         MVC   ESESELDT+2(4),MVSTIME                                            
*                                                                               
LOGE020  EQU   *                                                                
         GOTO1 =A(CLOSAPPC),(RC)                                                
         B     LOGEOK                                                           
*                                                                               
LOGEOK   SR    RC,RC                                                            
LOGENO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SWAP OUT ESS SESSION                                                *         
***********************************************************************         
         USING ESSATCD,R3                                                       
SWAPOUT  NMOD1 0,**SWOU**                                                       
         LR    RC,R1                                                            
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         XC    ESEAEATC,ESEAEATC                                                
         XC    EATAESS,EATAESS                                                  
         MVC   EATENUM,=XL2'00'                                                 
         CLC   EATTASKN,=XL2'00'                                                
         BE    SWOU010                                                          
         MVC   EATNAME,=CL8'ESSAPPC '                                           
         LA    RF,EATNAME+1                                                     
         EDIT  (B2,EATTASKN),(2,(RF)),ZERO=NOBLANK,FILL=0                       
         B     SWOU020                                                          
SWOU010  L     RF,AESSB                                                         
         MVC   EATNAME,ESSBJNAM-ESSBD(RF)                                       
SWOU020  MVI   EATLEV,X'00'                                                     
         MVI   EATVER,X'00'                                                     
         MVC   EATEKEY,SPACES                                                   
         MVC   EATEPWD,SPACES                                                   
         MVC   EATFAC,SPACES                                                    
         MVI   EATHFLG1,X'00'                                                   
         MVI   EATHFLG2,X'00'                                                   
         MVI   EATMETH,C'N'                                                     
         MVI   EATMODE,X'00'                                                    
         MVI   EATTRAC,X'00'                                                    
         MVC   EATNFSFR,SPACES                                                  
         MVC   EATNFSTO,SPACES                                                  
         MVC   EATNFSIP,SPACES                                                  
         XC    EATDATA,EATDATA                                                  
         XC    EATWORK,EATWORK                                                  
         XC    EATPQBUF,EATPQBUF                                                
         MVI   EATSTATE,EATSRDYQ                                                
         B     SWOUOK                                                           
*                                                                               
SWOUOK   SR    RC,RC                                                            
SWOUNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
***********************************************************************         
* SWAP ESS SESSION CONTROL PARAMETERS INTO SUB TASK CONTROL BLOCK     *         
***********************************************************************         
         USING ESSATCD,R3                                                       
SWAPIN   NMOD1 0,**SWIN**                                                       
         LR    RC,R1                                                            
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         MVC   EATENUM,ESENUM                                                   
         MVC   EATNAME,ESENAME                                                  
         MVC   EATLEV,ESELEV                                                    
         MVC   EATVER,ESEVER                                                    
         MVC   EATEKEY,ESEEKEY                                                  
         MVC   EATEPWD,ESEEPWD                                                  
         MVC   EATFAC,ESEFAC                                                    
         MVC   EATHFLG1,ESEHFLG1                                                
         MVC   EATHFLG2,ESEHFLG2                                                
         MVC   EATTRAC,ESETRAC                                                  
         MVC   EATNFSFR,ESENFSFR                                                
         MVC   EATNFSTO,ESENFSTO                                                
         MVC   EATNFSIP,ESENFSIP                                                
         ICM   RF,15,ESEADCB                                                    
         STCM  RF,15,EATBLDCB                                                   
         ICM   RF,15,ESEADATA                                                   
         STCM  RF,15,EATDATA                                                    
         ICM   RF,15,ESEAWORK                                                   
         STCM  RF,15,EATWORK                                                    
         ICM   RF,15,ESEAPQB                                                    
         STCM  RF,15,EATPQBUF                                                   
         B     SWINOK                                                           
*                                                                               
SWINOK   SR    RC,RC                                                            
SWINNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DETACH ESSIO SUBTASK - R3=A(ESSATC)                                 *         
***********************************************************************         
         USING ESSATCD,R3                                                       
DETASK   NMOD1 0,**DTSK**                                                       
         LR    RC,R1                                                            
         DETACH EATOSTCB           DETACH ESSIO                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                UNSUCCESSFUL DETACH                          
         MVI   EATSTATE,EATSOFFQ                                                
         MVC   P,SPACES                                                         
         MVC   P(8),EATNAME                                                     
         MVC   P+10(14),=CL14'SUBTASK DETACH'                                   
         OC    EATABEND,EATABEND                                                
         BZ    DTAS010                                                          
         MVC   P+24(13),=CL13' ABEND CODE: '                                    
         GOTO1 VHEXOUT,DMCB,EATABEND,P+37,4,=C'TOG'                             
*                                                                               
DTAS010  MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         OC    EATABEND,EATABEND                                                
         BNZ   DTAS020                                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         B     DTASX                                                            
*                                                                               
DTAS020  MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
*                                                                               
DTASX    XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEE IF THE OPERATOR HAS INTERRUPTED WITH EITHER A 'STOP' (P) OR     *         
* 'MODIFY' (F) COMMAND.  EXAMINE THE COMMAND AND TAKE CORRECT ACTION. *         
***********************************************************************         
CHKOPER  NMOD1 0,**CHKO**,RA                                                    
         LR    RC,R1                                                            
*                                                                               
         L     RF,ACOMM                                                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         LA    R3,COMCIBPT         A(A(CIB))                                    
         DROP  RF                                                               
*                                                                               
         USING CIBNEXT,R2                                                       
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         BNE   CHKOPMOD                                                         
*                                                                               
CHKOPSTP MVI   OPERSTOP,C'Y'       YES -- SET STOP FLAG                         
         GOTO1 VLOGIO,DMCB,X'FF000001',=C'STOP COMMAND ACCEPTED'                
         B     CHKOPOK                                                          
*                                                                               
CHKOPMOD CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         BE    *+6                 YES                                          
         DC    H'0'                WHAT IS GOING ON?                            
*                                                                               
CHK010   CLC   =C'STATUS',CIBDATA  SHOW PROGRAM STATUS ON CONSOLE               
         BNE   CHK020                                                           
         XC    ESSID,ESSID                                                      
         MVI   ESSMODE,0                                                        
         BAS   RE,STATUS                                                        
         B     CHKOPOK                                                          
*                                                                               
CHK020   CLC   =C'EXTRACT',CIBDATA                                              
         BNE   CHK030                                                           
         LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   CMDLINE,SPACES                                                   
         MVC   CMDERRM,SPACES                                                   
         LA    RF,CMDLINE                                                       
         STCM  RF,15,EXSCLINE                                                   
         LA    RF,CMDERRM                                                       
         STCM  RF,15,EXSCERRM                                                   
         SR    RF,RF                                                            
         ICM   RF,3,CIBDATLN                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   CMDLINE(0),CIBDATA                                               
         MVC   EXSACTN,=AL4(EXSACMDQ)                                           
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'CMDLINE,CMDLINE)                      
         CLI   BYTE,EXSROKQ                                                     
         BNE   CHK022                                                           
   GOTO1 VLOGIO,DMCB,X'FF000001',=C'OPERATOR COMMAND LINE OK'                   
         B     CHKOPOK                                                          
CHK022   EQU   *                                                                
   GOTO1 VLOGIO,DMCB,X'FF000001',=C'OPERATOR COMMAND LINE ERROR'                
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'CMDERRM,CMDERRM)                      
         B     CHKOPOK                                                          
*                                                                               
CHK030   CLC   =C'PIANO',CIBDATA                                                
         BNE   CHK040                                                           
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' PIANO COMMAND PROCESSED'             
         B     CHKOPOK                                                          
*                                                                               
CHK040   CLC   =C'SUBTASK',CIBDATA                                              
         BNE   CHK050                                                           
         XC    ESSID,ESSID                                                      
         MVI   ESSMODE,0                                                        
         BAS   RE,STATASK                                                       
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' SUBTASK COMMAND PROCESSED'           
         B     CHKOPOK                                                          
*                                                                               
CHK050   CLC   =C'RESET',CIBDATA                                                
         BNE   CHK060                                                           
         XC    ESSID,ESSID                                                      
         MVI   ESSMODE,0                                                        
         CLC   =C',ESS',CIBDATA+5                                               
         BNE   CHK058                                                           
         GOTO1 VNUMVAL,DMCB,CIBDATA+9,(0,5)                                     
         CLI   DMCB,0                                                           
         BNE   CHKBAD1             NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         STCM  R1,3,ESSID                                                       
CHK058   BAS   RE,RESET                                                         
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' RESET COMMAND PROCESSED'             
         B     CHKOPOK                                                          
*                                                                               
CHK060   CLC   =C'STATUS',CIBDATA                                               
         BNE   CHK100                                                           
         XC    ESSID,ESSID                                                      
         MVI   ESSMODE,0                                                        
         CLC   =C',ESS',CIBDATA+6                                               
         BNE   CHK068                                                           
         GOTO1 VNUMVAL,DMCB,CIBDATA+10,(0,5)                                    
         CLI   DMCB,0                                                           
         BNE   CHKBAD1             NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         STCM  R1,3,ESSID                                                       
CHK068   BAS   RE,STATUS                                                        
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' STATUS COMMAND PROCESSED'            
         B     CHKOPOK                                                          
*                                                                               
CHK100   CLC   =C'ESS',CIBDATA                                                  
         BNE   CHK110                                                           
         MVI   ESSMODE,EATMSNDQ                                                 
         B     CHK120                                                           
CHK110   CLC   =C'ESR',CIBDATA                                                  
         BNE   CHK200                                                           
         MVI   ESSMODE,EATMRCVQ                                                 
CHK120   GOTO1 VNUMVAL,DMCB,CIBDATA+3,(0,5)                                     
         CLI   DMCB,0                                                           
         BNE   CHKBAD1             NOT NUMERIC                                  
         L     R1,DMCB+4                                                        
         STCM  R1,3,ESSID          STATUS ESSIO SUB TASK                        
         CLC   =C',STATUS',CIBDATA+8                                            
         BNE   CHKTOG                                                           
         BAS   RE,STATUS                                                        
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' STATUS COMMAND PROCESSED'            
         B     CHKOPOK                                                          
*                                                                               
CHK200   CLC   =C'STOP',CIBDATA                                                 
         BNE   CHKBAD                                                           
         GOTO1 =A(CHKSESS),(RC)    CHECK SESSIONS BEFORE SHUTDOWN               
         BE    CHKOPSTP                                                         
         GOTO1 VLOGIO,DMCB,X'FF000001',=C'STOP COMMAND IGNORED'                 
         B     CHKOPOK                                                          
*                                                                               
CHKTOG   CLC   =C',TOGGLE',CIBDATA+8                                            
         BNE   CHKACTV                                                          
         BAS   RE,TOGGLE                                                        
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' TOGGLE COMMAND PROCESSED'            
         B     CHKOPOK                                                          
*                                                                               
CHKACTV  CLC   =C',ACTIVATE',CIBDATA+8                                          
         BNE   CHKRSET                                                          
         BAS   RE,ACTIVATE                                                      
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' ACTIVATE COMMAND PROCESSED'          
         B     CHKOPOK                                                          
*                                                                               
CHKRSET  CLC   =C',RESET',CIBDATA+8                                             
         BNE   CHKBAD2                                                          
         BAS   RE,RESET                                                         
         GOTO1 VLOGIO,DMCB,X'FF000001',=C' RESET COMMAND PROCESSED'             
         B     CHKOPOK                                                          
*                                                                               
CHKBAD   GOTO1 VLOGIO,DMCB,X'FF000001',=C' INVALID HOSTESS COMMAND*'            
         B     CHKOPOK                                                          
*                                                                               
CHKBAD1  GOTO1 VLOGIO,DMCB,X'FF000001',=C' INVALID ESS ID NUMBER*'              
         B     CHKOPOK                                                          
*                                                                               
CHKBAD2  GOTO1 VLOGIO,DMCB,X'FF000001',=C' INVALID ESS SUB PARAMETER*'          
         B     CHKOPOK                                                          
*                                                                               
CHKOPOK  L     RF,ACOMM                                                         
         LA    R3,COMCIBPT-COMLIST(RF) A(A(CIB))                                
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  R2                                                               
*                                                                               
CHKOPX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SHOW THE PROGRAM STATUS ON THE CONSOLE                              *         
***********************************************************************         
STATUS   NTR1                                                                   
         MVI   ESSMODE,EATMSNDQ                                                 
*                                                                               
STATESS  L     R4,=A(ESECBF)        A(FIRST ENTRY IN ESECB)                     
         USING ESSESSD,R4                                                       
*                                                                               
STAT010  L     RF,=A(ESECBTL)                                                   
         C     R4,0(RF)                                                         
         BE    STAT300                                                          
         OC    ESSID,ESSID                                                      
         BZ    STAT030                                                          
         CLC   ESENUM,ESSID                                                     
         BNE   STAT020                                                          
         CLI   ESEMODE,ESEMTSTQ                                                 
         BE    STAT030                                                          
         CLC   ESEMODE(1),ESSMODE                                               
         BE    STAT030                                                          
*                                                                               
STAT020  LA    R4,ESELQ(R4)        BUMP EATCB POINTER                           
         B     STAT010             GET NEXT ENTRYD                              
*                                                                               
STAT030  EQU   *                                                                
         MVC   OPMSG+1(8),ESENAME                                               
*                                                                               
         MVC   P(8),ESENAME                                                     
         MVC   P+10(40),=CL40'ESS SESSION STATUS REPORT'                        
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         MVC   P(8),ESENAME                                                     
         CLI   ESESTATE,ESESNALQ                                                
         BE    STAT040                                                          
         CLI   ESESTATE,ESESRDYQ                                                
         BE    STAT050                                                          
         CLI   ESESTATE,ESESRQPQ                                                
         BE    STAT060                                                          
         CLI   ESESTATE,ESESRSPQ                                                
         BE    STAT070                                                          
         CLI   ESESTATE,ESESERRQ                                                
         BE    STAT080                                                          
         MVC   P(40),=CL40'SESSION IN UNKNOWN STATE'                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+18(8),ESEELU                                              
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBGTA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
         GOTO1 =A(PRNTATTB),(RC)                                                
         B     STAT200                                                          
*                                                                               
STAT040  MVC   P+10(40),=CL40'SESSION NOT ALLOCATED'                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+18(8),ESEELU                                              
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         B     STAT200                                                          
*                                                                               
STAT050  MVC   P+10(40),=CL40'SESSION ALLOCATED AWAITING REQUEST'               
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         B     STAT100                                                          
*                                                                               
STAT060  EQU   *                                                                
         CLI   ESEMODE,ESEMRCVQ                                                 
         BE    STAT062                                                          
         L     R5,ESEADATA                                                      
         USING ESSDATAD,R5                                                      
         CLI   ESSDBLKD,C'Y'                                                    
         BNE   STAT062                                                          
         MVC   P+10(40),=CL40'FILE RECORDS SENT: '                              
         EDIT  ESSDRCNT,(8,P+29),ZERO=NOBLANK,FILL=0                            
         MVI   P+37,C'/'                                                        
         EDIT  ESSDRNUM,(8,P+38),ZERO=NOBLANK,FILL=0                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         B     STAT100                                                          
         DROP  R5                                                               
*                                                                               
STAT062  MVC   P+10(40),=CL40'SESSION REQUEST MESSAGE PENDING'                  
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         B     STAT100                                                          
*                                                                               
STAT070  MVC   P+10(40),=CL40'SESSION RESPONSE MESSAGE PENDING'                 
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         B     STAT100                                                          
*                                                                               
STAT080  MVC   P+10(40),=CL40'SESSION PROCESSING ERROR OCCURRED'                
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         B     STAT100                                                          
*                                                                               
STAT100  EQU   *                                                                
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+18(8),ESEELU                                              
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
*        ICM   R6,15,ESEAAPPC                                                   
*        USING ESSAPPCD,R6                                                      
*        MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*        XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
*        MVC   APPCACTN,=CL8'ATBGTA2 '                                          
*        GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*        GOTO1 =A(PRNTATTB),(RC)                                                
         B     STAT200                                                          
*                                                                               
STAT200  EQU   *                                                                
         OC    ESSID,ESSID                                                      
         BZ    STAT020                                                          
         CLI   ESSMODE,ESEMRCVQ                                                 
         BE    STATX                                                            
         MVI   ESSMODE,ESEMRCVQ                                                 
         B     STATESS                                                          
*                                                                               
STAT300  EQU   *                                                                
         OC    ESSID,ESSID                                                      
         BZ    STATX                                                            
         MVC   P+10(40),=CL40'ESS ID NOT RECOGNISED'                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         B     STATX                                                            
*                                                                               
STATX    XIT1                                                                   
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SHOW THE PROGRAM SUB TASK STATUS IN CONSOLE                         *         
***********************************************************************         
STATASK  NTR1                                                                   
         MVC   OPMSG,SPACES                                                     
*                                                                               
         L     R3,=A(EATCBF)        A(FIRST ENTRY IN EATCB)                     
         USING ESSATCD,R3                                                       
*                                                                               
STSK010  L     RF,=A(EATCBTL)                                                   
         C     R3,0(RF)                                                         
         BE    STSKX                                                            
         OC    ESSID,ESSID                                                      
         BZ    STSK030                                                          
         CLC   EATENUM,ESSID                                                    
         BNE   STSK020                                                          
         CLC   EATMODE(1),ESSMODE                                               
         BE    STSK030                                                          
*                                                                               
STSK020  LA    R3,EATLQ(R3)        BUMP EATCB POINTER                           
         B     STSK010             GET NEXT ENTRYD                              
*                                                                               
STSK030  EQU   *                                                                
         MVC   OPMSG+1(8),EATNAME                                               
         CLI   EATSTATE,EATSRUNQ                                                
         BE    STSK040                                                          
         CLI   EATSTATE,EATSBSYQ                                                
         BE    STSK050                                                          
         CLI   EATSTATE,EATSRDYQ                                                
         BE    STSK060                                                          
         MVC   OPMSG+11(30),=CL30'PROCESS IDLE - ABEND CODE: '                  
         GOTO1 VHEXOUT,DMCB,EATABEND,OPMSG+38,4,=C'TOG'                         
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'OPMSG,OPMSG)                          
         B     STSK200                                                          
*                                                                               
STSK040  MVC   OPMSG+11(30),=CL30'PROCESS WAITING LU6 ALLOCATION'               
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'OPMSG,OPMSG)                          
         B     STSK200                                                          
*                                                                               
STSK050  MVC   OPMSG+11(30),=CL30'PROCESS BUSY WITH REQUEST'                    
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'OPMSG,OPMSG)                          
         B     STSK200                                                          
*                                                                               
STSK060  MVC   OPMSG+11(30),=CL30'PROCESS READY FOR REQUEST'                    
         GOTO1 VLOGIO,DMCB,X'FF000001',(L'OPMSG,OPMSG)                          
         B     STSK200                                                          
*                                                                               
STSK100  GOTO1 VLOGIO,DMCB,X'FF000001',=C' ESS ID DOES NOT EXIST*'              
         B     STSK200                                                          
*                                                                               
STSK200  MVC   OPMSG,SPACES                                                     
         OC    ESSID,ESSID                                                      
         BZ    STSK020                                                          
         B     STSKX                                                            
*                                                                               
STSKX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* TURN AN ESSIO CONTROLLER SUB TASK ON OR OFF, VIA ATTACH/DETACH      *         
***********************************************************************         
TOGGLE   NTR1                                                                   
         MVC   OPMSG,SPACES                                                     
         MVI   TOGFLAG,C'N'                                                     
         L     R3,=A(EATCBF)        A(FIRST ENTRY IN EATCB)                     
         USING ESSATCD,R3                                                       
*                                                                               
TOGL010  L     RF,=A(EATCBTL)                                                   
         C     R3,0(RF)                                                         
         BE    TOGL050                                                          
         CLC   EATENUM,ESSID                                                    
         BNE   TOGL020                                                          
         CLC   EATMODE(1),ESSMODE                                               
         BE    TOGL030                                                          
*                                                                               
TOGL020  LA    R3,EATLQ(R3)        BUMP EATCB POINTER                           
         B     TOGL010             GET NEXT ENTRYD                              
*                                                                               
TOGL030  MVI   TOGFLAG,C'Y'                                                     
         CLI   EATSTATE,EATSOFFQ                                                
         BE    TOGL040                                                          
*                                  NOP WHILE FREDS OPEN PROBLEM                 
*        POST  EATSPECB            TELL ESSIO TO STOP                           
*                                                                               
*        WAIT  ECB=EATOSECB        WAIT FOR ANSWER FROM ESSIO                   
*                                    NEED APPC OPEN ECB TO WORK                 
*        TM    EATOSECB,X'40'      ESSIO TERMINATED?                            
*        BO    *+6                 YES                                          
*        DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DETASK),(RC)     DETACH SUB TASK                              
         B     TOGL020                                                          
*                                                                               
TOGL040  CLI   EATSTATE,EATSBSYQ                                                
         BE    TOGL060                                                          
*                                  RECOVER VALUES IN TABLE ON START             
         LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   EXSACTN,=AL4(EXSARECQ)                                           
         MVC   EXSEID,ESSNAME                                                   
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,0              CHECK RETURN CODE                            
         BE    TOGL041                                                          
         MVC   P(40),=CL40'EXSERV RECOVER ERROR'                                
         MVC   P+21(L'EXSEID),EXSEID                                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                  SET FILE RECOMMIT FLAGS IN TABLE             
TOGL041  L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         TM    ESSBSTA1,ESSBSRCO   RECOMMIT=YES ?                               
         DROP  RF                                                               
         BZ    TOGL042                                                          
         LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   EXSACTN,=AL4(EXSASRCQ)                                           
         MVC   EXSEID,ESSNAME                                                   
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,0              CHECK RETURN CODE                            
         BE    *+6                 OK, CONTINUE                                 
         DC    H'0'                ERROR                                        
         DROP  R6                                                               
*                                  RE-ATTACH ESSIO SUB TASK                     
TOGL042  XC    EATOSECB,EATOSECB                                                
         XC    EATMTECB,EATMTECB                                                
         XC    EATSTECB,EATSTECB                                                
         XC    EATSPECB,EATSPECB                                                
*                                                                               
         LA    RF,AESSFACS                                                      
         STCM  RF,15,EATFACS                                                    
*                                                                               
         ICM   RF,15,AESSB                                                      
         STCM  RF,15,EATESSB                                                    
*                                                                               
         LA    R6,EATOSECB                                                      
         ATTACH EPLOC=ESSIONAM,ECB=(R6),PARAM=((R3)),SZERO=NO,         +        
               ESTAI=(SUBTAE,(R3))                                              
*                                                                               
         ST    R1,EATOSTCB                                                      
         OC    EATOSTCB,EATOSTCB                                                
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         WAIT  ECB=EATSTECB                                                     
         XC    EATSTECB,EATSTECB                                                
         MVI   EATSTATE,EATSRDYQ                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(23),=CL23'REATTACHED ESS SUB TASK'                          
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         POST  EATMTECB                                                         
         B     TOGL020                                                          
*                                                                               
TOGL050  EQU   *                                                                
         CLI   TOGFLAG,C'Y'                                                     
         BE    TOGLOK                                                           
         MVC   P(25),=CL25'ESS SUB TASK ID NOT FOUND'                           
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     TOGLNO                                                           
*                                                                               
TOGL060  EQU   *                                                                
         MVC   P(22),=CL22'ESSIO SUB TASK ID BUSY'                              
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     TOGL020                                                          
*                                                                               
TOGLOK   SR    RC,RC               EXIT SUBROUTINE WITH CC .EQ.                 
TOGLNO   LTR   RC,RC               EXIT SUBROUTINE WITH CC .NE.                 
         XIT1                      EXIT SUBROUTINE                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ACTIVATE AN ESSIO CONTROLLER SUB TASK                               *         
***********************************************************************         
ACTIVATE NTR1                                                                   
         MVC   OPMSG,SPACES                                                     
         MVI   TOGFLAG,C'N'                                                     
         L     R3,=A(EATCBF)        A(FIRST ENTRY IN EATCB)                     
         USING ESSATCD,R3                                                       
*                                                                               
ACTV010  L     RF,=A(EATCBTL)                                                   
         C     R3,0(RF)                                                         
         BE    ACTV050                                                          
         CLC   EATENUM,ESSID                                                    
         BNE   ACTV020                                                          
         CLC   EATMODE(1),ESSMODE                                               
         BE    ACTV030                                                          
*                                                                               
ACTV020  LA    R3,EATLQ(R3)        BUMP EATCB POINTER                           
         B     ACTV010             GET NEXT ENTRYD                              
*                                                                               
ACTV030  MVI   TOGFLAG,C'Y'                                                     
*        CLI   EATSTATE,EATSBSYQ                                                
*        BE    ACTV060                                                          
         CLI   EATSTATE,EATSOFFQ                                                
         BE    ACTV040                                                          
*                                  NOP WHILE FREDS OPEN PROBLEM                 
*        POST  EATSPECB            TELL ESSIO TO STOP                           
*                                                                               
*        WAIT  ECB=EATOSECB        WAIT FOR ANSWER FROM ESSIO                   
*                                    NEED APPC OPEN ECB TO WORK                 
*        TM    EATOSECB,X'40'      ESSIO TERMINATED?                            
*        BO    *+6                 YES                                          
*        DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DETASK),(RC)     DETACH SUB TASK                              
*                                  RECOVER VALUES IN TABLE ON START             
ACTV040  LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   EXSACTN,=AL4(EXSARECQ)                                           
         MVC   EXSEID,ESSNAME                                                   
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,0              CHECK RETURN CODE                            
         BE    ACTV041                                                          
         MVC   P(40),=CL40'EXSERV RECOVER ERROR'                                
         MVC   P+21(L'EXSEID),EXSEID                                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                  SET FILE RECOMMIT FLAGS IN TABLE             
ACTV041  L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         TM    ESSBSTA1,ESSBSRCO   RECOMMIT=YES ?                               
         DROP  RF                                                               
         BZ    ACTV042                                                          
         LA    R6,WORK             BUILD EXSERV PARAMETER BLOCK                 
         XC    WORK,WORK                                                        
         USING EXSERVD,R6                                                       
         MVC   EXSACTN,=AL4(EXSASRCQ)                                           
         MVC   EXSEID,ESSNAME                                                   
         GOTO1 VEXSERV,DMCB,WORK,BYTE,0                                         
         CLI   BYTE,0              CHECK RETURN CODE                            
         BE    *+6                 OK, CONTINUE                                 
         DC    H'0'                ERROR                                        
         DROP  R6                                                               
*                                  RE-ATTACH ESSIO SUB TASK                     
ACTV042  XC    EATOSECB,EATOSECB                                                
         XC    EATMTECB,EATMTECB                                                
         XC    EATSTECB,EATSTECB                                                
         XC    EATSPECB,EATSPECB                                                
*                                                                               
         LA    RF,AESSFACS                                                      
         STCM  RF,15,EATFACS                                                    
*                                                                               
         ICM   RF,15,AESSB                                                      
         STCM  RF,15,EATESSB                                                    
*                                                                               
         LA    R6,EATOSECB                                                      
         ATTACH EPLOC=ESSIONAM,ECB=(R6),PARAM=((R3)),SZERO=NO,         +        
               ESTAI=(SUBTAE,(R3))                                              
*                                                                               
         ST    R1,EATOSTCB                                                      
         OC    EATOSTCB,EATOSTCB                                                
         BNZ   *+6                                                              
         DC    H'0'                UNSUCCESSFUL ATTACH                          
*                                                                               
         WAIT  ECB=EATSTECB                                                     
         XC    EATSTECB,EATSTECB                                                
         MVI   EATSTATE,EATSRDYQ                                                
         MVC   P(8),EATNAME                                                     
         MVC   P+10(23),=CL23'REATTACHED ESS SUB TASK'                          
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         POST  EATMTECB                                                         
         B     ACTV020                                                          
*                                                                               
ACTV050  EQU   *                                                                
         CLI   TOGFLAG,C'Y'                                                     
         BE    ACTVOK                                                           
         MVC   P(25),=CL25'ESS SUB TASK ID NOT FOUND'                           
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     ACTVNO                                                           
*                                                                               
ACTV060  EQU   *                                                                
         MVC   P(22),=CL22'ESSIO SUB TASK ID BUSY'                              
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         B     ACTV020                                                          
*                                                                               
ACTVOK   SR    RC,RC               EXIT SUBROUTINE WITH CC .EQ.                 
ACTVNO   LTR   RC,RC               EXIT SUBROUTINE WITH CC .NE.                 
         XIT1                      EXIT SUBROUTINE                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* RESET AN ESS SESSION                                                *         
***********************************************************************         
RESET    NTR1                                                                   
         OC    ESSID,ESSID                                                      
         BZ    RSETX                                                            
         BAS   RE,GETESS                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ESSMODE,EATMSNDQ                                                 
*                                                                               
RSETESS  L     R4,=A(ESECBF)        A(FIRST ENTRY IN EATCB)                     
         USING ESSESSD,R4                                                       
*                                                                               
RSET010  L     RF,=A(ESECBTL)                                                   
         C     R4,0(RF)                                                         
         BE    RSET300                                                          
         CLC   ESENUM,ESSID                                                     
         BNE   RSET020                                                          
         CLC   ESEMODE(1),ESSMODE                                               
         BE    RSET030                                                          
*                                                                               
RSET020  LA    R4,ESELQ(R4)        BUMP EATCB POINTER                           
         B     RSET010             GET NEXT ENTRYD                              
*                                                                               
RSET030  EQU   *                                                                
         CLI   ESESTATE,ESESRDYQ                                                
         BE    RSET040                                                          
         CLI   ESESTATE,ESESRQPQ                                                
         BE    RSET040                                                          
         CLI   ESESTATE,ESESRSPQ                                                
         BE    RSET040                                                          
         CLI   ESESTATE,ESESNALQ                                                
         BE    RSET050                                                          
         CLI   ESESTATE,ESESERRQ                                                
         BE    RSET040                                                          
         DC    H'0'                                                             
*                                                                               
RSET040  EQU   *                                                                
         MVI   ESESTATE,ESESNALQ                                                
         CLI   ESEMODE,ESEMSNDQ                                                 
         BE    RSET042                                                          
         CLI   ESEMODE,ESEMRCVQ                                                 
         BE    RSET044                                                          
*                                                                               
RSET042  EQU   *                                                                
         MVC   P(L'ESENAME),ESENAME                                             
         MVC   P+10(L'ESEELU),ESEELU                                            
         MVC   P+20(40),=CL40'SENDER SESSION CONTROL BLOCK RESET'               
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_FLUSH                             
*                                                                               
         MVC   APPCACTN,=CL8'ATBDEAL '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BE    RSET050                                                          
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
*                                                                               
         MVC   APPCACTN,=CL8'ATBDEAL '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   RSETER                                                           
         B     RSET050                                                          
*                                                                               
RSET044  EQU   *                                                                
         MVC   P(L'ESENAME),ESENAME                                             
         MVC   P+10(L'ESEELU),ESEELU                                            
         MVC   P+20(40),=CL40'RECEIVER SESSION CONTROL BLOCK RESET'             
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         MVC   FILL,ATB_FILL_LL                                                 
         MVC   RECEIVE_ACCESS_TOKEN,ATB_FW0                                     
         MVC   RECEIVE_LENGTH,=A(L'APPC_BUFFER)                                 
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
*                                                                               
         MVC   APPCACTN,=CL8'ATBRCVI '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_UNSUCCESSFUL                                        
         BE    RSET046                                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   RSETER                                                           
*                                                                               
RSET046  MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         MVC   DEALLOCATE_TYPE,ATB_DEALLOCATE_ABEND                             
*                                                                               
         MVC   APPCACTN,=CL8'ATBDEAL '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
*                                                                               
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   RSETER                                                           
         B     RSET050                                                          
*                                                                               
RSET050  EQU   *                                                                
         MVC   ESEENID,ESSENID                                                  
         MVC   ESEELU,ESSELU                                                    
         MVC   ESEMELU,ESSMELU                                                  
         MVC   ESELUSN,ESSLUSN                                                  
         MVC   ESELEV,ESSLEVEL                                                  
         MVC   ESEVER,ESSVER                                                    
         MVC   ESEEPWD,ESSPWD                                                   
         MVC   ESEEKEY,ESSEKEY                                                  
         MVC   ESEFAC,ESSFAC                                                    
         MVC   ESEFTPS,ESSFTPS                                                  
         MVC   ESEHFLG1,ESSFLG1                                                 
         MVC   ESEHFLG2,ESSFLG2                                                 
         MVC   ESESATIM,ESSATIM                                                 
         MVC   ESESNUM,ESSSNUM                                                  
         MVC   ESESLIM,ESSSLIM                                                  
         MVC   ESETRAC,ESSTRAC                                                  
         MVC   ESENFSFR,ESSNFSFR                                                
         MVC   ESENFSTO,ESSNFSTO                                                
         MVC   ESENFSIP,ESSNFSIP                                                
         LA    RF,ESEAPPCE                                                      
         XC    0(4,RF),0(RF)                                                    
*                                                                               
         ICM   RF,15,=AL4(ESSDATAL)                                             
         ICM   RE,15,ESEADATA                                                   
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         ICM   RF,15,=AL4(ESSWORKL)                                             
         ICM   RE,15,ESEAWORK                                                   
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         ICM   RF,15,=AL4(ESSPQBL)                                              
         ICM   RE,15,ESEAPQB                                                    
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
*                                                                               
         ICM   RF,15,=AL4(ESSAPPCL)                                             
         ICM   RE,15,ESEAAPPC                                                   
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         GOTO1 =A(INITAPPC),(RC)                                                
         LA    RF,ESEAPPCE                                                      
         STCM  RF,15,NOTIFY_TYPE_AECB                                           
         STCM  RF,15,POST_ON_RECEIPT_ECB                                        
         DROP  R6                                                               
         B     RSET200                                                          
*                                                                               
RSET060  EQU   *                                                                
         B     RSET200                                                          
*                                                                               
RSET200  CLI   ESSMODE,EATMRCVQ                                                 
         BE    RSETOK                                                           
         MVI   ESSMODE,EATMRCVQ                                                 
         B     RSETESS                                                          
*                                                                               
RSET300  EQU   *                                                                
         MVC   P+10(40),=CL40'ESS ID NOT RECOGNISED'                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         B     RSETOK                                                           
*                                                                               
RSETER   EQU   *                                                                
         GOTO1 =A(PRNTLUER),(RC)                                                
         DC    H'0'                                                             
         B     RSETNO                                                           
*                                                                               
RSETOK   SR    RC,RC                                                            
RSETNO   LTR   RC,RC                                                            
RSETX    XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET ESS ID RECORD INFORMATION                                       *         
***********************************************************************         
GETESS   NTR1                                                                   
         USING GESSD,R5                                                         
         L     RF,=A(BDCBC)                                                     
         STCM  RF,15,ANEXTDCB                                                   
         LA    R5,IOKEY                                                         
         XC    GSKEY,GSKEY         CLEAR KEY                                    
         MVI   GSKREC,GSKRECQ                                                   
         MVC   GEKNUM,ESSID                                                     
         LAY   R5,IO                                                            
         L     R6,VMAJORNM         MAJOR RESCOURCE NAME                         
         ENQ   ((6),EQESSIO,E,8)   ENQUEUE ESSIO                                
*                                  READ WITH FLUSH                              
         GOTO1 VDATAMGR,DMCB,(X'24',DMREAD),GENDIR,IOKEY,(R5),DMWORK            
         LR    R2,R1                                                            
         LR    R1,R2                                                            
         CLI   8(R1),0                                                          
         BNE   GESSER                                                           
         CLI   GSKREC,GSKRECQ                                                   
         BNE   GESSER                                                           
         CLC   GEKNUM,ESSID                                                     
         BNE   GESSER                                                           
*                                  GETREC WITH FLUSH                            
         MVC   IOKEY(L'GSKEY),GSKEY                                             
         MVC   DMDA,GSDDA                                                       
         GOTO1 VDATAMGR,DMCB,(X'24',GETREC),GENFIL,DMDA,(R5),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND RECORD                            
         L     R6,VMAJORNM         MAJOR RESCOURCE NAME                         
         DEQ   ((6),EQESSIO,8)     DEQUEUE ESSIO PROCESS                        
*                                                                               
         LA    R4,GSFIRST(R5)      PROCESS ELEMENT DATA                         
*                                                                               
GESS050  CLI   0(R4),0                                                          
         BE    GESSNO                                                           
         CLI   0(R4),GESSELQ       FIND ESS DEFINITION ELEMENT                  
         BE    GESS070                                                          
GESS060  SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GESS050                                                          
*                                                                               
         USING GESSEL,R4                                                        
GESS070  EQU   *                   PROCESS ESS DEFINITION ELEMENT               
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         CLC   ESSBCODE,GESSHCOD                                                
         BNE   GESSNO                                                           
         DROP  RF                                                               
         CLI   GESSMETH,C'N'                                                    
         BNE   GESSNO                                                           
         TM    ESSFLG1,GESSFINQ                                                 
         BO    GESSNO                                                           
         MVC   ESSNUM,GEKNUM                                                    
         EDIT  (B2,GEKNUM),(8,ESSNAME),ZERO=NOBLANK,FILL=0                      
         MVC   ESSNAME(3),=CL3'ESS'                                             
         MVC   ESSLEVEL,GESSLEV                                                 
         MVC   ESSVER,GESSVER                                                   
         MVC   HOSTSLU,GESSHSLU                                                 
         MVC   HOSTRLU,GESSHRLU                                                 
         MVC   ESSENID,GESSENID                                                 
         MVC   ESSELU,GESSELU                                                   
         MVI   ESSLUSN,0                                                        
         MVC   ESSMELU,GESSELU                                                  
         MVC   ESSTPNM,GESSTPNM                                                 
         MVC   ESSAMODE,GESSMODE                                                
         MVC   ESSPWD,GESSPWD                                                   
         MVC   ESSEKEY,GESSEKEY                                                 
         MVC   ESSFAC,GESSFAC                                                   
         MVC   ESSMETH,GESSMETH                                                 
         MVC   ESSFTPS,GESSFTPS                                                 
         MVC   ESSFLG1,GESSFLG1                                                 
         MVC   ESSFLG2,GESSFLG2                                                 
         MVC   ESSATIM,GESSATIM                                                 
         MVC   ESSTRAC,GESSTRAC                                                 
         DROP  R4                                                               
*                                                                               
GESS080  CLI   0(R4),0                                                          
         BE    GESS100                                                          
         CLI   0(R4),GENFSELQ      FIND NFS MOUNT INFO ELEMENT                  
         BNE   GESS090                                                          
         USING GENFSEL,R4                                                       
         MVC   ESSNFSFR,GENFSFR                                                 
         MVC   ESSNFSTO,GENFSTO                                                 
         MVC   ESSNFSIP,GENFSIP                                                 
         B     GESS100                                                          
         DROP  R4                                                               
GESS090  SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GESS080                                                          
*                                                                               
GESS100  MVC   NETWRKID,SPACES                                                  
         LA    RE,NETWRKID                                                      
         LA    RF,ESSENID                                                       
         LA    R0,8                                                             
GESS110  CLI   0(RF),0                                                          
         BE    GESS120                                                          
         CLI   0(RF),C' '                                                       
         BE    GESS120                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GESS110                                                       
GESS120  MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         LA    RF,ESSELU                                                        
         LA    R0,8                                                             
GESS130  CLI   0(RF),0                                                          
         BE    GESS140                                                          
         CLI   0(RF),C' '                                                       
         BE    GESS140                                                          
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,GESS130                                                       
*                                                                               
GESS140  L     R2,=A(UNKESST)                                                   
GESS150  CLM   R2,15,=A(UNKESSTX)                                               
         BL    *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'FF'                                                      
         BE    GESS160                                                          
         CLI   0(R2),0                                                          
         BE    GESS180                                                          
         CLC   0(L'PARTNER_LU_NAME,R2),NETWRKID                                 
         BE    GESS170                                                          
GESS160  LA    R2,L'PARTNER_LU_NAME(R2)                                         
         B     GESS150                                                          
GESS170  XC    0(L'PARTNER_LU_NAME,R2),0(R2)                                    
         MVI   0(R2),X'FF'                                                      
*                                                                               
GESS180  EQU   *                                                                
         B     GESSOK                                                           
*                                                                               
GESSER   EQU   *                                                                
         L     R6,VMAJORNM         MAJOR RESCOURCE NAME                         
         DEQ   ((6),EQESSIO,8)     DEQUEUE ESSIO PROCESS                        
         DC    H'0'                                                             
         B     GESSNO                                                           
*                                                                               
GESSNO   DC    H'0'                                                             
GESSOK   SR    RC,RC                                                            
         LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE APPC PARAMETER AREA                                      *         
***********************************************************************         
         DS    0D                                                               
INITAPPC NMOD1 0,**IAPC**                                                       
         LR    RC,R1                                                            
         USING ESSAPPCD,R6                                                      
         LA    RE,ESSAPPCD                                                      
         ICM   RF,15,=AL4(ESSAPPCL)                                             
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         MVC   SYM_DEST_NAME,SPACES                                             
         MVC   PARTNER_LU_NAME,SPACES                                           
         MVC   LOCAL_LU_NAME,SPACES                                             
         MVC   TP_NAME,SPACES                                                   
         MVC   USER_ID,SPACES                                                   
         MVC   PASSWORD,SPACES                                                  
         MVC   PROFILE,SPACES                                                   
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,APPC_BUFFER_LENGTH                                            
         STCM  RE,15,AAPPCBUF                                                   
         XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* BULK DOWNLOAD DATA RECORDS TO ESS                                   *         
***********************************************************************         
         DS    0D                                                               
         USING ESSATCD,R3                                                       
BULKDOWN NMOD1 0,**BLKD**                                                       
         LR    RC,R1                                                            
         ICM   R4,15,EATAESS                                                    
         USING ESSESSD,R4                                                       
         L     R5,ESEADATA                                                      
         USING ESSDATAD,R5                                                      
         ICM   R6,15,ESEAAPPC                                                   
         USING ESSAPPCD,R6                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   ESEBDNFF,C'N'       TEST IF FIRST TIME THROUGH                   
         BE    BLKDNEXT                                                         
*                                  FIRST PASS THRU BULK DOWNLOAD                
BLKDFRST EQU   *                                                                
         ZAP   ESEBDBCN,=PL8'0'                                                 
         XC    ESEBDSTM,ESEBDSTM                                                
         XC    ESEBDETM,ESEBDETM                                                
*                                  GET THE CURRENT TIME IN BINARY               
         TIME  BIN                                                              
         LTR   R0,R0                                                            
         BZ    *+12                                                             
         STCM  R0,15,ESEBDSTM                                                   
         STCM  R0,15,ESEBDETM                                                   
         MVI   ESEBDNFF,C'N'                                                    
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,L'APPC_BUFFER_LENGTH                                          
         STCM  RE,3,APPC_BUFFER_LENGTH                                          
         B     BLKDGETR                                                         
*                                  NEXT PASS THRU BULK DOWNLOAD                 
BLKDNEXT EQU   *                   TEST LAST ASYNC SEND RETURN CODES            
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    BLKDDEAN            EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    BLKDDEAA            EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_PRODUCT_SPECIFIC_ERROR                              
         BE    BLKDPSE                                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   BLKDRCE             EXIT IF RETURN CODE ERROR                    
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   BLKDRSE             EXIT IF REQUEST_TO_SEND NOT NULL             
         LA    RE,APPC_BUFFER                                                   
         ICM   RF,15,=AL4(L'APPC_BUFFER)                                        
         LA    R0,*                                                             
         L     R1,=F'0'                                                         
         MVCL  RE,R0                                                            
         LA    RE,L'APPC_BUFFER_LENGTH                                          
         STCM  RE,3,APPC_BUFFER_LENGTH                                          
         TM    EATHFLG2,GESSFBIQ   TEST BINARY DATA BUFFER                      
         BZ    BLKDGETR                                                         
         TM    ESSDMFF,ESSHLSTQ    TEST MESSAGE WAS LAST IN SEQUENCE            
         BNZ   BLKDEND             END OF BULK DOWNLOAD                         
         BAS   RE,BUFFDAT          BUFFER DOWNLOAD DATA AFTER LAST SEND         
         BE    BLKDSEND            SEND FULL DATA BUFFER                        
*                                                                               
BLKDGETR EQU   *                   GET NEXT DATA RECORD FOR SEND BUFFER         
         LA    RE,ESSDBUFF                                                      
         STCM  RE,15,ESSDBPTR                                                   
         BAS   RE,GETDOWN          GET NEXT SUB SYSTEM RECORD TO SEND           
         BNE   BLKDGER                                                          
         BAS   RE,COMPREC          COMPRESS SUBSEQUENT REPEATED DATA            
         TM    ESSDMFF,ESSHLSTQ    TEST MESSAGE IS LAST IN SEQUENCE             
         BZ    BLKDGR10                                                         
         TM    EATHFLG2,GESSFBIQ   TEST BINARY DATA BUFFER                      
         BZ    BLKDEND             END OF BULK DOWNLOAD IF NOT                  
         CLC   APPC_BUFFER_LENGTH,=YL2(L'APPC_BUFFER_LENGTH)                    
         BE    BLKDEND             END OF BDN. IF EMPTY DATA BUFFER             
         B     BLKDSEND            SEND REMAINING DATA BUFFER                   
BLKDGR10 BAS   RE,BUFFDAT          BUFFER DOWNLOAD DATA BEFORE SEND             
         BE    BLKDSEND            SEND FULL DATA BUFFER                        
         B     BLKDGETR            GET NEXT RECORD FOR SEND BUFFER              
*                                                                               
BLKDSEND EQU   *                   SEND DATA BUFFER                             
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_ECB                                  
         LA    RF,ESEAPPCE                                                      
         STCM  RF,15,NOTIFY_TYPE_AECB                                           
         MVC   SEND_TYPE,ATB_BUFFER_DATA                                        
         MVC   SEND_ACCESS_TOKEN,ATB_FW0                                        
         SR    RF,RF                                                            
         ICM   RF,3,APPC_BUFFER_LENGTH                                          
         STCM  RF,15,SEND_LENGTH                                                
         CVD   RF,DUB                                                           
         AP    ESEBDBCN(8),DUB+4(4)                                             
*                                                                               
         TM    EATTRAC,GESSTBDN    TEST TRACE BULK DOWNLOAD RECORDS             
         BNZ   *+8                                                              
         MVI   APPCTRAC,X'FF'      INHIBIT APPC MESSAGE TRACE                   
*                                                                               
         MVC   APPCACTN,=CL8'ATBSEND '                                          
         GOTO1 VESSAPPC,DMCB,(R3),(R6)                                          
*                                                                               
         MVI   APPCTRAC,0                                                       
*                                                                               
         CLC   RTN_CODE,ATB_DEALLOCATED_NORMAL                                  
         BE    BLKDDEAN            EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_DEALLOCATED_ABEND                                   
         BE    BLKDDEAA            EXIT IF DEALLOCATED                          
         CLC   RTN_CODE,ATB_PRODUCT_SPECIFIC_ERROR                              
         BE    BLKDPSE                                                          
         CLC   RTN_CODE,ATB_OK                                                  
         BNE   BLKDRCE             EXIT IF RETURN CODE ERROR                    
         CLC   REQUEST_TO_SEND_RECEIVED,ATB_FW0                                 
         BNE   BLKDRSE             EXIT IF REQUEST_TO_SEND NOT NULL             
         B     BLKDOK                                                           
*                                                                               
BLKDEND  EQU   *                   LAST MESSAGE IN SEQUENCE                     
         MVC   P(40),=CL40'ESSIO END BULK DOWNLOAD OK'                          
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         MVI   ESSDSEQN,ESSDSLSQ   SET MESSAGE SEQUENCE CODE                    
*                                  GET THE CURRENT TIME IN BINARY               
         MVC   P(22),=CL22'DOWNLOAD STATISTICS - '                              
         MVC   P+22(20),=CL20'TIME:         S.'                                 
         MVC   P+41(20),=CL20'SIZE:          B.'                                
         MVC   P+60(20),=CL20'RATE:          KB/S'                              
         TIME  BIN                                                              
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         STCM  R0,15,ESEBDETM                                                   
         ICM   R1,15,ESEBDSTM                                                   
         SR    R0,R1                                                            
         LR    R2,R0                                                            
         EDIT  (R2),(7,P+28),2,ZERO=NOBLANK                                     
         EDIT  ESEBDBCN,(8,P+47),ZERO=NOBLANK                                   
         LTR   R2,R2                                                            
         BZ    BLKDEN10                                                         
         ZAP   DUB1,ESEBDBCN                                                    
* 11.25.2013 TZIH                                                               
* RATE WILL BE DISPLAYED IN THOUSANDS OF BYTES PER SECOND                       
         SRP   DUB1,61,0            SHIFT RIGHT 3 DIGITS, NO ROUNDING           
         CVB   RF,DUB1                                                          
         SR    RE,RE                                                            
         M     RE,=F'100'                                                       
         DR    RE,R2                                                            
         EDIT  (RF),(8,P+66),ZERO=NOBLANK                                       
         B     BLKDEN20                                                         
BLKDEN10 EQU   *                                                                
         EDIT  (RF),(8,P+66),ZERO=NOBLANK                                       
BLKDEN20 EQU   *                                                                
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         B     BLKDOK                                                           
*                                                                               
BLKDDEAN EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSBD)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+42(40),=CL40'DEALLOCATE NORMAL RETURNED'                       
         MVC   P+12(8),EATNAME                                                  
         MVC   P+22(20),=CL20'BULK DOWNLOAD ERROR'                              
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESNALQ                                                
         B     BLKDNO                                                           
BLKDDEAA EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSBD)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+42(40),=CL40'DEALLOCATE ABEND RETURNED'                        
         MVC   P+12(8),EATNAME                                                  
         MVC   P+22(20),=CL20'BULK DOWNLOAD ERROR'                              
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESNALQ                                                
         B     BLKDNO                                                           
BLKDRCE  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSBD)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+42(40),=CL40'INVALID RETURN CODE:'                             
         MVC   P+12(8),EATNAME                                                  
         ICM   RF,15,RTN_CODE                                                   
         EDIT  (RF),(5,P+63),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   P+22(20),=CL20'BULK DOWNLOAD ERROR'                              
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         GOTO1 =A(SWAPOUT),(RC)                                                 
         MVI   ESESTATE,ESESNALQ                                                
         B     BLKDNO                                                           
BLKDPSE  EQU   *                                                                
         MVC   P+42(40),=CL40'PRODUCT SPECIFIC ERROR'                           
         B     BLKDERR                                                          
BLKDRSE  EQU   *                                                                
         MVC   P+42(40),=CL40'INVALID APPC LU6 CODE'                            
         B     BLKDERR                                                          
BLKDGER  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSRD)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+42(40),=CL40'PROBLEM READING DATA SET'                         
         MVC   P+12(8),EATNAME                                                  
         MVC   P+22(20),=CL20'BULK DOWNLOAD ERROR'                              
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         GOTO1 =A(SWAPOUT),(RC)                                                 
         GOTO1 =A(LOGERROR),(RC)                                                
         B     BLKDNO                                                           
BLKDERR  EQU   *                                                                
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSBD)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(8),EATNAME                                                  
         MVC   P+22(20),=CL20'BULK DOWNLOAD ERROR'                              
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         GOTO1 =A(SWAPOUT),(RC)                                                 
         GOTO1 =A(LOGERROR),(RC)                                                
         B     BLKDNO                                                           
*                                                                               
BLKDOK   SR    RC,RC                                                            
BLKDNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  GET NEXT RECORD TO SEND TO ESS FROM HOSTESS FOR CURRENT            *         
*  BULK DOWNLOAD REQUEST                                              *         
***********************************************************************         
GETDOWN  NTR1                                                                   
         MVC   ESSDSID,EATENUM                                                  
         MVI   ESSDMODE,ESSDMSNQ                                                
         MVI   ESSDSEQN,ESSDSNXQ   SET MESSAGE SEQUENCE CODE                    
         CLC   ESSDMID,=AL3(ESSHSQLQ)                                           
         BE    GDONSQL             PROCESS SQL EXTRACT SUB SYSTEM               
         CLC   ESSDMID,=AL3(ESSHPQRQ)                                           
         BE    GDONPQR             PROCESS PQ REPORT SUB SYSTEM                 
         DC    H'0'                UNKNOWN ACTION CODE                          
*                                                                               
GDONPQR  GOTO1 VEIOPQR,DMCB,(R3),RR=RELO                                        
         BNE   GDONER1                                                          
         B     GDONOK                                                           
*                                                                               
GDONSQL  GOTO1 VEIOSQL,DMCB,(R3),RR=RELO                                        
         BNE   GDONER2                                                          
         B     GDONOK                                                           
*                                                                               
GDONER1  MVC   P(8),ESENAME                                                     
         MVC   P+10(40),=CL40'EIOPQR GET RECORD ERROR'                          
         GOTO1 VHEXOUT,DMCB,ESSDRETC,P+45,1,=C'TOG'                             
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         B     GDONNO                                                           
*                                                                               
GDONER2  MVC   P(8),ESENAME                                                     
         MVC   P+10(40),=CL40'SQL SUBSYSTEM GET RECORD ERROR'                   
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         B     GDONNO                                                           
*                                                                               
GDONOK   SR    RC,RC                                                            
GDONNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  COMPRESS DATA REPEATED IN RECORD FROM LAST SAVED RECORD            *         
*  WORKING FROM START OF RECORD UPTO 255 BYTES                        *         
***********************************************************************         
COMPREC  NTR1                                                                   
         TM    EATHFLG1,GESSFCRQ   TEST RECORD COMPRESSION REQUIRED             
         BNO   CRECOK                                                           
         LA    RE,ESSDBUFF                                                      
         LA    RF,ESSDSAVE                                                      
         LA    R0,L'ESSDSAVE                                                    
         SR    R1,R1                                                            
         ICM   R1,3,ESSDDLN                                                     
         BZ    CRECOK                                                           
         SR    R2,R2                                                            
*                                                                               
CREC010  EQU   *                                                                
         CLI   0(RF),0                                                          
         BE    CREC020                                                          
         CLC   0(1,RE),0(RF)                                                    
         BNE   CREC020                                                          
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,*+8                                                           
         B     CREC020                                                          
         BCT   R0,CREC010                                                       
*                                                                               
CREC020  EQU   *                                                                
         MVC   ESSDSAVE,ESSDBUFF                                                
         C     R2,=F'3'                                                         
         BL    CREC100                                                          
         LA    RF,ESSDBUFF                                                      
         MVI   0(RF),X'3F'                                                      
         STC   R2,1(RF)                                                         
         LA    RF,2(RF)                                                         
         LA    R2,2                                                             
*                                                                               
CREC030  EQU   *                                                                
         LTR   R1,R1                                                            
         BZ    CREC034                                                          
*                                                                               
CREC032  EQU   *                                                                
         MVC   0(1,RF),0(RE)                                                    
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,CREC032                                                       
*                                                                               
CREC034  EQU   *                                                                
         STCM  R2,3,ESSDDLN                                                     
         SR    RE,RF                                                            
*                                                                               
CREC040  EQU   *                                                                
         MVI   0(RF),0                                                          
         LA    RF,1(RF)                                                         
         BCT   RE,CREC040                                                       
         B     CRECOK                                                           
*                                                                               
CREC100  EQU   *                                                                
         B     CRECOK                                                           
*                                                                               
CRECOK   SR    RC,RC                                                            
CRECNO   LTR   RC,RC                                                            
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*  BUFFER DOWNLOAD DATA BEFORE SEND                                   *         
***********************************************************************         
BUFFDAT  NTR1                                                                   
         TM    EATHFLG2,GESSFBIQ   TEST BINARY BLOCKED BUFFER                   
         BO    BUFD010                                                          
*                                                                               
         LA    R0,ESSDBUFF                                                      
         L     RE,AAPPCBUF                                                      
         LA    RE,L'APPC_BUFFER_LENGTH(RE)                                      
         SR    RF,RF                                                            
         ICM   RF,3,ESSDDLN                                                     
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,ESSDDLN                                                     
         LA    RF,2(RF)                                                         
         STCM  RF,3,APPC_BUFFER_LENGTH                                          
         STCM  RF,15,SEND_LENGTH                                                
         B     BUFDOK                                                           
*                                                                               
BUFD010  EQU   *                                                                
         SR    RF,RF                                                            
         ICM   RF,3,ESSDDLN                                                     
         SR    RE,RE                                                            
         ICM   RE,3,APPC_BUFFER_LENGTH                                          
         LA    R1,4000                                                          
         SR    R1,RE                                                            
         CR    RF,R1                                                            
         BH    BUFD100                                                          
         LR    R1,RF                                                            
         AR    R1,RE                                                            
         LR    R2,R1                                                            
         STCM  R2,3,APPC_BUFFER_LENGTH                                          
         ICM   R1,15,AAPPCBUF                                                   
         AR    RE,R1                                                            
         ICM   R0,15,ESSDBPTR                                                   
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         B     BUFDNO                                                           
*                                                                               
BUFD100  EQU   *                                                                
         LR    R3,RF                                                            
         SR    RF,R1                                                            
         STCM  RF,3,ESSDDLN                                                     
         LR    RF,R1                                                            
         LR    R2,RF                                                            
         AR    R1,RE                                                            
         STCM  R1,3,APPC_BUFFER_LENGTH                                          
         ICM   R1,15,AAPPCBUF                                                   
         AR    RE,R1                                                            
         ICM   R0,15,ESSDBPTR                                                   
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         ICM   RE,15,ESSDBPTR                                                   
         AR    RE,R2                                                            
         STCM  RE,15,ESSDBPTR                                                   
         B     BUFDOK                                                           
*                                                                               
BUFDOK   SR    RC,RC                                                            
BUFDNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R3,R4,R5,R6                                                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PRINT LU6 APPC ERROR REPORT                                         *         
***********************************************************************         
         DS    0D                                                               
         USING ESSAPPCD,R6                                                      
PRNTLUER NMOD1 0,**PLUE**                                                       
         LR    RC,R1                                                            
         MVC   P(80),=CL80'<ERROR????>'                                         
         MVC   P+23(40),=CL40'APPC LU6 ERROR          RC='                      
         ICM   RF,15,=AL4(ERNEIOLU)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         ICM   RF,15,RTN_CODE                                                   
         EDIT  (RF),(5,P+50),ALIGN=LEFT,ZERO=NOBLANK                            
         MVC   P+38(L'APPCACTN),APPCACTN                                        
         MVC   WTOMSGD,P                                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
*                                                                               
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(L'APPCACTN),APPCACTN                                        
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'RETURN CODE ='                                    
         ICM   RF,15,RTN_CODE                                                   
         EDIT  (RF),(5,P+34),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'REASON CODE ='                                    
         ICM   RF,15,REASON_CODE                                                
         EDIT  (RF),(5,P+34),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'DATA RECEIVED = '                                 
         ICM   RF,15,DATA_RECEIVED                                              
         EDIT  (RF),(5,P+36),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'REQUEST TO SEND RECEIVED = '                      
         ICM   RF,15,REQUEST_TO_SEND_RECEIVED                                   
         EDIT  (RF),(5,P+47),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVI   P,C'*'                                                           
         MVC   P+1(16),=CL16'APPC/MVS CALL'                                     
         MVC   P+20(40),=CL40'STATUS RECEIVED = '                               
         ICM   RF,15,STATUS_RECEIVED                                            
         EDIT  (RF),(5,P+38),ALIGN=LEFT,ZERO=NOBLANK                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         MVC   NOTIFY_TYPE,ATB_NOTIFY_TYPE_NONE                                 
         XC    APPC_BUFFER_LENGTH,APPC_BUFFER_LENGTH                            
         MVC   APPCACTN,=CL8'ATBGTA2 '                                          
         GOTO1 VESSAPPC,DMCB,DUMMYTCB,(R6)                                      
         GOTO1 =A(PRNTATTB),(RC)                                                
         B     PLUEOK                                                           
*                                                                               
PLUEOK   SR    RC,RC                                                            
PLUENO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,RA                                                            
         USING ESSAPPCD,R6                                                      
         DS    0D                                                               
PRNTATTB DS    0H                                                               
*                                                                               
         NMOD1 0,PRNTATTB                                                       
         LR    RC,R1               A(COMMON STORAGE AREA)                       
*                                                                               
* PRINT DETAILS ABOUT THE CURRENT STATE OF THE LU6.2 CONVERSATION               
*                                                                               
         MVC   P(40),=CL40'***************'                                     
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'APPC ATTRIBUTES'                                     
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'***************'                                     
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         MVC   P(40),=CL40'CONVERSATION_ID'                                     
         GOTO1 VHEXOUT,DMCB,CONVERSATION_ID,P+30,8,=C'TOG'                      
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'PARTNER_LU_NAME'                                     
         MVC   P+30(17),PARTNER_LU_NAME                                         
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'MODE_NAME'                                           
         MVC   P+30(8),MODE_NAME                                                
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'SYNC_LEVEL'                                          
         MVC   P+30(4),=C'NONE'                                                 
         CLC   SYNC_LEVEL,ATB_NONE                                              
         BE    PATT010                                                          
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   SYNC_LEVEL,ATB_CONFIRM                                           
         BE    PATT010                                                          
         MVC   P+30(7),=C'UNKNOWN'                                              
PATT010  MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'TP_NAME_LENGTH'                                      
         EDIT  TPNAMELN,(2,P+30),ALIGN=LEFT,ZERO=NOBLANK                        
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'TP_NAME'                                             
         MVC   P+30(64),TP_NAME                                                 
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'LOCAL_LU_NAME'                                       
         MVC   P+30(8),LOCAL_LU_NAME                                            
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'CONVERSATION_TYPE'                                   
         MVC   P+30(5),=C'BASIC'                                                
         CLC   CONVERSATION_TYPE,ATB_BASIC_CONVERSATION                         
         BE    PATT020                                                          
         MVC   P+30(6),=C'MAPPED'                                               
         CLC   CONVERSATION_TYPE,ATB_MAPPED_CONVERSATION                        
         BE    PATT020                                                          
         MVC   P+30(7),=C'UNKNOWN'                                              
PATT020  MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*        MVC   P(40),=CL40'USER_ID'                                             
*        MVC   P+30(10),USER_ID                                                 
*        MVC   WTOMSGD(L'WTOMSGD),P                                             
*        GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*        GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*        MVC   P(40),=CL40'PROFILE'                                             
*        MVC   P+30(10),PROFILE                                                 
*        MVC   WTOMSGD(L'WTOMSGD),P                                             
*        GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*        GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*        MVC   P(40),=CL40'USER_TOKEN'                                          
*        MVC   P+30(80),USER_TOKEN                                              
*        MVC   WTOMSGD(L'WTOMSGD),P                                             
*        GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*        GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         MVC   P(40),=CL40'CONVERSATION_STATE'                                  
         MVC   P+30(4),=C'SEND'                                                 
         CLC   CONVERSATION_STATE,ATB_SEND_STATE                                
         BE    PATT030                                                          
         MVC   P+30(7),=C'RECEIVE'                                              
         CLC   CONVERSATION_STATE,ATB_RECEIVE_STATE                             
         BE    PATT030                                                          
         MVC   P+30(7),=C'CONFIRM'                                              
         CLC   CONVERSATION_STATE,ATB_CONFIRM_STATE                             
         BE    PATT030                                                          
         MVC   P+30(10),=C'INITIALIZE'                                          
         CLC   CONVERSATION_STATE,ATB_INITIALIZE_STATE                          
         BE    PATT030                                                          
         MVC   P+30(12),=C'SEND PENDING'                                        
         CLC   CONVERSATION_STATE,ATB_SEND_PENDING_STATE                        
         BE    PATT030                                                          
         MVC   P+30(12),=C'CONFIRM SEND'                                        
         CLC   CONVERSATION_STATE,ATB_CONFIRM_SEND_STATE                        
         BE    PATT030                                                          
         MVC   P+30(18),=C'CONFIRM DEALLOCATE'                                  
         CLC   CONVERSATION_STATE,ATB_CONFIRM_DEALLOCATE_STATE                  
         BE    PATT030                                                          
         MVC   P+30(7),=C'UNKNOWN'                                              
*                                                                               
PATT030  EQU   *                                                                
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
         MVC   P(40),=CL40'*************'                                       
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
*                                                                               
PRATTBX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* TEST THAT HOSTESS RUNNING WITH UNIQUE JOB NAME.                               
* DYNAMICALLY ALLOCATE MVS DATA SET WITH DSN DERIVED FORM JOB NAME    *         
***********************************************************************         
         DS    0D                                                               
UNIQUE   NMOD1 0,**UNIQ**                                                       
         LR    RC,R1                                                            
         MVC   TXUDSN+6(44),HJOBDSN                                             
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         MVC   TXUDSN+14(8),ESSBJNAM                                            
         DROP  RF                                                               
         MVC   TXUDD+6(8),=CL8'HJOBFILE'                                        
         MVI   TXUDD+5,8                                                        
         LA    RF,RBLKXUAL                                                      
         USING S99RBX,RF                                                        
         MVC   S99EID,=CL6'S99RBX'                                              
         MVI   S99EVER,S99RBXVR                                                 
         MVI   S99EOPTS,0                                                       
         DROP  RF                                                               
*                                                                               
         LA    R1,ARBLKUAL                                                      
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    UNIQ020             DATASET WAS ALLOCATED OK                     
*                                                                               
         MVC   P(13),=C'<<< ERROR >>>'                                          
         MVC   P+19(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 VHEXOUT,DMCB,RBLKUAL+4,P+52,2,=C'TOG'                            
         GOTO1 VHEXOUT,DMCB,RBLKUAL+6,P+71,2,=C'TOG'                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         MVC   P(80),=CL80'<ERROR????>'                                         
         ICM   RF,15,=AL4(ERNHOSUN)                                             
         EDIT  (RF),(4,P+6),ZERO=NOBLANK,FILL=0                                 
         MVC   P+12(40),=CL40'HOSTESS UNIQUE JOB NAME TEST FAILED'              
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   WTOMSG+26(2),WTOALARM                                            
         GOTO1 VESSWTO,DMCB,X'FF000001',(L'WTOMSG,WTOMSG)                       
         MVC   WTOMSG+26(2),SPACES                                              
         MVC   WORK(40),=CL40'CONTINUE HOSTESS STARTUP Y/N?'                    
         GOTO1 VLOGIO,DMCB,1,(80,WORK)                                          
         GOTO1 VLOGIO,DMCB,0,(1,BYTE)                                           
         CLI   BYTE,C'N'                                                        
         BE    UNIQNO                                                           
*                                  RETRY ASSUMING EXISTING OLD FILE             
         MVC   TXUDISP(7),TXUDISPO                                              
*                                                                               
         LA    R1,ARBLKUAL                                                      
         DYNALLOC                                                               
         LTR   RF,RF                                                            
         BZ    UNIQ020             DATASET WAS ALLOCATED OK                     
*                                                                               
         MVC   P(13),=C'<<<ERROR>>>'                                            
         MVC   P+19(22),=C'DYNALLOC ERROR CODE = '                              
         GOTO1 VHEXOUT,DMCB,RBLKUAL+4,P+41,2,=C'TOG'                            
         MVC   P+46(15),=C',  INFO CODE = '                                     
         GOTO1 VHEXOUT,DMCB,RBLKUAL+6,P+61,2,=C'TOG'                            
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
UNIQ020  EQU   *                                                                
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         MVC   P(40),=CL40'HOSTESS UNIQUE JOB NAME TEST OK'                     
         MVC   WTOMSGD(L'WTOMSGD),P                                             
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         GOTO1 VESSWTO,DMCB,X'FE000001',(L'WTOMSG,WTOMSG)                       
         B     UNIQOK                                                           
*                                                                               
UNIQOK   SR    RC,RC                                                            
UNIQNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
**********************************************************************          
* MVS DATASET -- DYNAMIC ALLOCATION                                             
**********************************************************************          
         DS    0F                                                               
ARBLKUAL DC    X'80',AL3(RBLKUAL) R1 POINTS TO THIS BEFORE DYNALLOC             
*                                                                               
*RBLKUAL  DC    X'1401000000000000',A(ACATUAL),X'0000000000000000'              
RBLKUAL  DC    X'1401000000000000',A(ACATUAL),A(RBLKXUAL),X'00000000'           
*                                                                               
ACATUAL  DC    X'00',AL3(TXUDD)                                                 
         DC    X'00',AL3(TXUDSN)                                                
         DC    X'00',AL3(TXUDISP)                                               
         DC    X'00',AL3(TXUNDISD)                                              
*        DC    X'00',AL3(TXUNDISP)                                              
*        DC    X'00',AL3(TXUTRK)                                                
         DC    X'00',AL3(TXUBLKLN)                                              
         DC    X'00',AL3(TXUPRI)                                                
         DC    X'00',AL3(TXUSEC)                                                
         DC    X'00',AL3(TXURLSE)                                               
         DC    X'00',AL3(TXUUNIT)                                               
         DC    X'80',AL3(TXUUNAL)                                               
*                                                                               
TXUDD    DC    AL2(DALDDNAM),X'00010008',CL8' '        DDNAME=........          
TXUDSN   DC    AL2(DALDSNAM),X'0001002C',CL44' '       DSN=............         
TXUDISPS DC    AL2(DALSTATS),X'0001000108'             DISP=(SHR,.....)         
TXUDISPM DC    AL2(DALSTATS),X'0001000102'             DISP=(MOD,.....)         
TXUDISP  DC    AL2(DALSTATS),X'0001000104'             DISP=(NEW,.....)         
TXUNDISP DC    AL2(DALNDISP),X'0001000102'                 =(...,CATLG)         
TXUDISPO DC    AL2(DALSTATS),X'0001000101'             DISP=(OLD,...)           
TXUNDISD DC    AL2(DALNDISP),X'0001000104'                 =(...,DEL)           
TXUBLKLN DC    AL2(DALBLKLN),X'00010003',AL3(16500)    SPACE=(16500,...         
TXUTRK   DC    AL2(DALTRK),X'0000'                     SPACE=(TRKS,...          
TXUPRI   DC    AL2(DALPRIME),X'00010003',AL3(10)            =(..(10,...         
TXUSEC   DC    AL2(DALSECND),X'00010003',AL3(10)            =(...,10),.         
TXURLSE  DC    AL2(DALRLSE),X'0000'                         =(...,RLSE)         
TXUUNIT  DC    AL2(DALUNIT),X'00010005',C'SYSDA'       UNIT=SYSDA               
TXUUNDD  DC    AL2(DUNDDNAM),X'00010008',CL8' '        DDNAME=........          
TXUREMOV DC    AL2(DUNREMOV),X'0000'                   REMOVE INUSE ATR         
TXUUNAL  DC    AL2(DALCLOSE),X'00000000'               UNALLOCATE CLOSE         
*                                                                               
         DS    0D                                                               
RBLKXUAL DC    XL80'00'                                                         
*&&UK                                                                           
HJOBDSN  DC    CL44'ESS.TST.HOSTESS1'                                           
*&&                                                                             
*&&US                                                                           
HJOBDSN  DC    CL44'DDS.TST.HOSTESS1'                                           
*&&                                                                             
         DS    0D                                                               
HJOBFILE DCB   DDNAME=HJOBFILE,MACRF=(PM),DSORG=PS,RECFM=FB,           *        
               LRECL=132,BLKSIZE=16500                                          
         EJECT                                                                  
         DROP  RB                                                               
***********************************************************************         
* MAIN TASK ASYNCHRONOUS ESTAE EXIT ROUTINE                           *         
***********************************************************************         
MAINTAE  DS    0H                                                               
         USING *,RF                                                             
         C     R0,=F'12'           TEST SDWA ACQUIRED                           
         BNE   *+8                 NO, JUST LET MVS PERC                        
         SR    RF,RF                                                            
         BR    RE                                                               
*                                                                               
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         STM   RE,RC,12(RD)        SAVE CALLING REGS                            
         ST    RD,ESTAERD          AND POINTER                                  
         ST    R1,ESTAER1          AND POINTER TO SDWA                          
*                                                                               
         USING SDWA,R1                                                          
         MVC   MEUSREGS+0(08),SDWAEC1  PSW AT ABEND                             
         MVC   MEUSREGS+3(01),SDWAICD1 SAVE INTERRUPT CODE IN PSW               
         MVC   MEUSREGS+8(64),SDWAGRSV R0-RF AT ABEND                           
         DROP  R1                                                               
*                                                                               
         MVI   MEREASON,C'P'                                                    
         OC    MECHKPSW,MECHKPSW   TEST INST ADDRESS SAVED BY LOOP RTN          
         BZ    MTAEX                                                            
         L     RE,MECHKPSW                                                      
         MVC   0(1,RE),MECHKPSW    RESTORE OP CODE                              
         XC    MECHKPSW,MECHKPSW                                                
         MVI   MEREASON,C'L'                                                    
         MVI   MEUSREGS+3,X'FF'    SET LOOP INDICATOR IN PSW                    
*                                                                               
MTAEX    L     RF,=A(MTAEPROC)                                                  
         BR    RF                                                               
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MAIN TASK ASYNCHRONOUS ESTAE EXIT PROCESSING SUB ROUTINE            *         
***********************************************************************         
         EJECT                                                                  
         DS    0D                                                               
MTAEPROC BASR  RB,R0                                                            
         USING *,RB                                                             
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         L     RD,=A(MEWORK)                                                    
         MVI   MEPOPSW,X'FF'       INHIBIT TIMER INTERRRUPTS                    
         TTIMER CANCEL                                                          
*                                                                               
         CLI   MEBLOW,C'P'                                                      
         BE    MEPR04                                                           
         CLI   MEBLOW,C'Q'         STOP LOOP OF REQLAST/RUNLAST DUMPS           
         BNE   MEPR06                                                           
*                                                                               
MEPR02   MVI   MEBLOW,C'P'                                                      
         MVC   P(40),=CL40'MAIN TASK ESTAE EXIT 1'                              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
MEPR04   L     R1,ESTAER1                                                       
         SETRP DUMP=NO,RC=0,COMPCOD=(665,USER)                                  
         B     MEPRX                                                            
*                                                                               
MEPR06   CLC   MEUSREGS+5(3),MELSTPSW+1                                         
         BE    MEPR046             AVOID MULTIPLE DUMPS ??                      
         MVC   MELSTPSW,MEUSREGS+4                                              
         AP    MENDUMPS,=P'1'      INCREMENT DUMP COUNT                         
         L     R1,ESTAER1                                                       
*                                                                               
         USING SDWA,R1                                                          
         CLC   ABEND666,SDWACMPC   TEST STOP NOW (OPERATOR COMMAND)             
         BE    MEPR014                                                          
         CLC   OPERCANC,SDWACMPC   TEST OPERATOR CANCEL                         
         BE    MEPR046                                                          
         DROP  R1                                                               
*                                                                               
         B     MEPR046             EXIT OTHERWISE ??                            
*                                                                               
* ??     L     R0,=A(HOSTESS)      ESTABLISH START OF DUMP                      
*                                                                               
MEPR014  MVC   P(40),=CL40'MAIN TASK ESTAE EXIT 2'                              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         SETRP DUMP=NO,RC=0                                                     
         B     MEPRX                                                            
         EJECT                                                                  
*                                                                               
MEPR044  L     R3,=A(MERETRY)      SET RETRY ROUTINE ADDRESS                    
         L     R1,ESTAER1          RESTORE SDWA POINTER                         
         USING SDWA,R1                                                          
         SETRP DUMP=NO,RC=4,RETADDR=(3),FRESDWA=YES                             
         B     MEPRX                                                            
*                                                                               
MEPR046  EQU   *                                                                
*                                  DUMP REQUEST QUEUE TO HOSTRCV                
         L     RF,AESSB                                                         
         USING ESSBD,RF                                                         
         TM    ESSBSTA1,ESSBSRBS+ESSBSRBR                                       
         BZ    MEPR056                                                          
         DROP  RF                                                               
         MVC   P(40),=CL40'DUMPING REQUESTS TO HOSTRCV'                         
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
*                                                                               
         LA    R2,HOSTRCV                                                       
         OPEN  ((R2),OUTPUT)                                                    
*                                                                               
MEPR048  EQU   *                                                                
         LAY   R6,IO                                                            
         USING ESSREQD,R6                                                       
         XC    ESSRKEY,ESSRKEY     CLEAR KEY AREA                               
         GOTO1 VESSRQS,DMCB,=AL4(ERQARHIQ),(R6)                                 
         CLI   0(R1),ERQROKQ                                                    
         BE    MEPR052                                                          
         CLI   0(R1),ERQREOFQ                                                   
         BE    MEPR054             EXIT AT END OF TABLE                         
         DC    H'0'                                                             
*                                                                               
MEPR050  EQU   *                   GET NEXT RECORD FROM TSAR                    
         GOTO1 VESSRQS,DMCB,=AL4(ERQARSQQ),(R6)                                 
         CLI   0(R1),ERQROKQ                                                    
         BE    MEPR052                                                          
         CLI   0(R1),ERQREOFQ                                                   
         BE    MEPR054             EXIT AT END OF TABLE                         
         DC    H'0'                                                             
*                                                                               
MEPR052  SR    RE,RE                                                            
         ICM   RE,3,ESSRRLEN                                                    
         LA    RE,4(RE)                                                         
         SLL   RE,16                                                            
         STCM  RE,15,IOL                                                        
         LA    R2,HOSTRCV                                                       
         LA    R4,IOL                                                           
         PUT   (R2),(R4)                                                        
         B     MEPR050                                                          
*                                                                               
MEPR054  EQU   *                                                                
         LA    R2,HOSTRCV                                                       
         CLOSE ((R2))                                                           
*                                  CLOSE DOWN REQUEST QUEUE                     
MEPR056  GOTO1 VESSRQS,DMCB,=AL4(ERQACLOQ),WORK                                 
         CLI   0(R1),ERQROKQ                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   CONSOPEN,C'N'       CLOSE CONTROL SYSTEM IF STILL OPEN           
         BE    MEPR060                                                          
         L     RE,=V(UTL)          SWITCH TO CONTROL SYSTEM                     
         MVI   4(RE),CONTROLQ                                                   
         GOTO1 VDATAMGR,DMCB,DMCLSE,=C'FILES',,IOL                              
         MVI   CONSOPEN,C'N'                                                    
*                                                                               
MEPR060  MVC   P(40),=CL40'MAIN TASK ESTAE EXIT 3'                              
         GOTO1 VESSLOG,DMCB,DUMMYTCB,P                                          
         SETRP DUMP=YES,RC=0                                                    
*                                                                               
MEPRX    L     RD,ESTAERD                                                       
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         USING *,RF                                                             
MERETRY  L     RC,=A(COMMWORK)                                                  
         LM    R0,RF,MEUSREGS+8                                                 
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ESSIO SUB TASK ASYNCHRONOUS ESTAE EXIT ROUTINE                      *         
***********************************************************************         
SUBTAE   DS    0H                                                               
         USING *,RF                                                             
         C     R0,=F'12'           TEST SDWA ACQUIRED                           
         BNE   *+8                 NO, JUST LET MVS PERC                        
         SR    RF,RF                                                            
         BR    RE                                                               
*                                                                               
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         STM   RE,RC,12(RD)        SAVE CALLING REGS                            
         ST    RD,ESTAERD          AND POINTER                                  
         ST    R1,ESTAER1          AND POINTER TO SDWA                          
*                                                                               
         USING SDWA,R1                                                          
         L     R3,SDWAPARM                                                      
         USING ESSATCD,R3                                                       
         DROP  R1                                                               
*                                                                               
STAEX    L     RF,=A(STAEPROC)                                                  
         BR    RF                                                               
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ESSIO SUB TAK ASYNCHRONOUS ESTAE EXIT PROCESSING SUB ROUTINE        *         
***********************************************************************         
STAEPROC BASR  RB,R0                                                            
         USING *,RB                                                             
         L     RC,=A(COMMWORK)     COMMON STORAGE AREA                          
         L     RD,=A(SEWORK)                                                    
*                                                                               
         USING SDWA,R1                                                          
         CLC   ABEND710,SDWACMPC                                                
         BE    SEPR010                                                          
         DROP  R1                                                               
*                                                                               
SEPR010  MVC   P(40),=CL40'ESSIO SUB TASK ESTAE EXIT 3'                         
         GOTO1 VESSLOG,DMCB,(R3),P                                              
         SETRP DUMP=YES,RC=0                                                    
*                                                                               
SEPRX    L     RD,ESTAERD                                                       
         LM    RE,RC,12(RD)                                                     
         BR    RE                                                               
*                                                                               
         USING *,RF                                                             
SERETRY  L     RC,=A(COMMWORK)                                                  
         LM    R0,RF,MEUSREGS+8                                                 
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMON WORK AREA DSECT                                             *          
***********************************************************************         
COMMWORK DS    0D                  COMMON WORKING STORAGE AREA                  
***********************************************************************         
* APPC/MVS PSEUDONYM DEFINITIONS                                     *          
***********************************************************************         
         ATBSERV                                                                
*                                                                               
ATBCTS_WAIT    EQU   ATB_FW2                                                    
                                                                                
CONTROLQ EQU   X'0A'               CONTROL SYSTEM UTL SENUM                     
NOTEND   EQU   X'00'                                                            
END      EQU   X'FF'                                                            
OFF      EQU   X'00'                                                            
ON       EQU   X'FF'                                                            
FFILL    DC    32X'FF'                                                          
SPACES   DC    132C' '                                                          
DCONTOUQ DC    F'60'               DISCONNECT TIMEOUT                           
SERFTOUQ DC    F'120'              ESS ERROR FAST TIMEOUT                       
SERSTOUQ DC    F'600'              ESS ERROR SLOW TIMEOUT                       
ERRCMAXQ DC    F'4'                ESS ERROR COUNT MAXIMUM SLOW                 
WAITSECS DC    F'3000'             DEFAULT WAIT TIME = 30 SECONDS               
ATIOT    DC    F'0'                                                             
*                                                                               
OPMSG    DC    CL60' '                                                          
*                                                                               
WTOALARM DC    CL2'*A'             WTO ALARM MESSAGE                            
WTOMSG   DS    0CL128              WTO MESSAGE AREA                             
WTOMSGK  DS    CL28                KEY PART                                     
WTOMSGD  DS    CL100               DETAIL PART                                  
*                                                                               
ABEND666 DC    AL3(666)                                                         
ABEND667 DC    AL3(667)                                                         
ABEND669 DC    AL3(669)                                                         
ABEND670 DC    AL3(670)                                                         
ABEND710 DC    AL3(710)                                                         
ABEND899 DC    AL3(899)                                                         
OPERCANC DC    X'222000'                                                        
*                                                                               
WTORECB  DC    F'0'                                                             
         EJECT                                                                  
         DS    0D                                                               
AESSFACS EQU   *                                                                
       ++INCLUDE DDESSFACSC                                                     
         EJECT                                                                  
*                                                                               
ESSLURCV DC    CL64'ESSLURCV'                                                   
ESSLUSND DC    CL64'ESSLUSND'                                                   
DMOPEN   DC    C'OPEN   '                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMADD    DC    C'DMADD  '                                                       
DMWRT    DC    C'DMWRT  '                                                       
DMCLSE   DC    C'DMCLSE '                                                       
DMFAST   DC    C'DMFAST '                                                       
SYSFLES  DC    C'SYSFLES'                                                       
GETREC   DC    C'GETREC '                                                       
PUTREC   DC    C'PUTREC '                                                       
ADDREC   DC    C'ADDREC '                                                       
DMRFIL   DC    C'RECOVER'                                                       
CONTROL  DC    C'CONTROL'                                                       
CTFILE   DC    C'CTFILE '                                                       
GENDIR   DC    C'GENDIR '                                                       
GENFIL   DC    C'GENFIL '                                                       
CTFILEU  DC    C'NCTFILE UGENDIR UGENFIL UCTRCVR X'                             
CTFILEN  DC    C'NCTFILE NGENDIR NGENFIL NCTRCVR X'                             
DMDA     DC    F'0'                                                             
*                                                                               
         DS    0D                                                               
HOSTRCV  DCB   DDNAME=HOSTRCV,DSORG=PS,MACRF=(GM,PM),RECFM=VB,         *        
               BLKSIZE=8200,LRECL=2048,BUFNO=2,EODAD=EODADREQ                   
         DS    0D                                                               
         DS    0D                                                               
         DC    CL16'*UTL**UTL**UTL**'                                           
UTL      DC    (TUTLXALN)X'00'                                                  
         ORG   UTL+(TSYS-UTLD)                                                  
         DC    X'0A'               SYSTEM 10 (CONTROL)                          
         ORG                                                                    
UTLL     EQU   *-UTL                                                            
                                                                                
         DS    0D                                                               
         DC    CL16'*SSB**SSB**SSB**'                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG   SSB+(SSOSTAT2-SSBD)                                              
         DC    AL1(SSOSNRCV)       SET RECOVERY OFF                             
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
*                                                                               
DMWORK   DS    12D                                                              
DMCB     DS    6F                                                               
DMCBENQ  DS    6F                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
RELO     DS    A                                                                
RBSAVE   DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
P        DS    CL132                                                            
CMDLINE  DS    CL80                                                             
CMDERRM  DS    CL80                                                             
WORK     DS    CL256                                                            
DUMMYTCB DS    CL(EATLQ)                                                        
*                                                                               
AOPERECB DC    A(0)                A(ECB OF OPERATOR INTERRUPT)                 
ACOMM    DS    A                   A(COMMUNICATIONS PARAMETER LIST)             
AGENDIR  DS    A                   A(GENDIR DTF)                                
AGENFIL  DS    A                   A(GENFIL DTF)                                
ACTFILE  DS    A                   A(CTFILE DTF)                                
AESSB    DS    A                   A(ESSB)                                      
AEATCB   DS    A                   A(EATCB)                                     
ACXREC   DC    A(CXREC)                                                         
*                                                                               
ATIMRECB DS    A                   A(ECB OF HOSTESS WAIT TIMER TIMEOUT)         
LOOPTIME DS    F                   TIME OF LAST CONTROL LOOP                    
*&&US                                                                           
WAITTIME DC    F'1000'             10 SEC                                       
*&&                                                                             
*&&UK                                                                           
WAITTIME DC    F'4000'             40 SEC                                       
*&&                                                                             
TIMERECB DC    F'0'                ECB OF HOSTESS WAIT TIMER TIMEOUT            
MVSTIME  DS    F                   MVS TIME (HHMMSSTT)                          
GENDA    DC    F'0'                GENFIL DISK ADDRESS                          
ELCODE   DS    X                                                                
MAJORNAM DC    C'EDESS   '         MAJOR RESCOURCE NAME                         
EQESSIO  DC    CL8'EQESSIO '       MINOR RESCOURCE NAME FOR ESSIO ENQ           
MINORNAM DC    C'        '         MINOR RESCOURCE NAME                         
ESSIONAM DC    CL8'ESSIO   '       DEFAULT NAME OF ESSIO ATTACH PHASE           
TOGFLAG  DC    C'N'                SUB TASK TOGGLE FLAG                         
ESSID    DS    XL2                 ESS ID NUMBER                                
ESSMODE  DS    XL1                 ESSIO SUB TASK MODE                          
ESSNUM   DS    XL2                 ESS NUMBER                                   
ESSNAME  DS    CL8                 ESS NAME                                     
ESSLEVEL DC    XL1'01'             ESS LEVEL                                    
ESSVER   DC    XL1'00'             ESS VERSION                                  
HOSTSLU  DC    CL8'        '       HOSTESS SENDER LU                            
HOSTRLU  DC    CL8'        '       HOSTESS RECEIVE LU                           
ESSENID  DC    CL8'        '       ESS NETWORK ID                               
ESSELU   DC    CL8'        '       ESS PARTNER LU                               
ESSMELU  DC    CL8'        '       ESS PARTNER MASTER LU                        
ESSLUSN  DC    CL1' '              ESS ASSOCIATED LU SEQUENCE NUMBER            
ESSPWD   DC    CL8'        '       ESS PASSWORD                                 
ESSEKEY  DC    CL10'          '    ESS PASSWORD ENCRYPTION KEY                  
ESSFLG1  DC    XL1'00'             ESS CONTROL FLAGS 1                          
ESSFLG2  DC    XL1'00'             ESS CONTROL FLAGS 2                          
ESSFAC   DC    CL8'        '       ESS FACPAK ALIA TERMINAL ID                  
ESSMETH  DC    CL1' '              ESS APPC METHOD                              
ESSFTPS  DC    XL4'00'             ESS NFTP FILE MIMIMUM SIZE                   
ESSTPNM  DC    CL8'        '       ESS TP NAME                                  
ESSAMODE DC    CL8'        '       ESS APPC VTAM MODE                           
ESSATIM  DC    XL4'00'             ESS ACTIVITY TIMEOUT                         
ESSTRAC  DC    XL1'00'             ESS TRACE LEVEL                              
ESSSNUM  DC    XL1'00'             ESS SESSION NUMBER                           
ESSSLIM  DC    XL1'00'             ESS SESSION NUMBER LIMIT                     
ESSNFSFR DC    CL40' '             ESS NFS MOUNT FROM                           
ESSNFSTO DC    CL40' '             ESS NFS MOUNT TO                             
ESSNFSIP DC    CL40' '             ESS NFS MOUNT IP ADDRESS                     
NETWRKID DS    CL17                                                             
APPCLUID DC    CL8'        '       APPC/MVS HOSTESS LUID                        
TASKSIZE DC    XL2'00'             ESSIO SUB TASK POOL SIZE                     
TASKNUM  DC    XL2'00'             ESSIO SUB TASK POOL NUMBER                   
SESSCNT  DC    XL2'00'             COUNT OF ESS SESSIONS ATTACHED               
SESSNUM  DC    XL1'00'             NUMBER OF ESS SESSION                        
SESSLIM  DC    XL1'00'             LIMIT NUMBER OF ESS SESSIONS                 
ASESSION DS    AL4                 SAVED A(ESSESSD SESSION BLOCKS)              
APPCTOKN DC    CL8'        '       APPC/MVS ALLOCATE QUEUE TOKEN                
APPCRTKN DC    CL8'        '       APPC/MVS ESSLURCV ALOCATE Q TOKEN            
APPCSTKN DC    CL8'        '       APPC/MVS ESSLUSND ALOCATE Q TOKEN            
APPCECB  DC    F'0'                APPC/MVS RECEIVE ALLOCATE ECB                
RCVAECB  DC    F'0'                APPC/MVS ESSLURCV ALLOCATE ECB               
SNDAECB  DC    F'0'                APPC/MVS ESSLUSND ALLOCATE ECB               
APPCEST  DC    F'0'                APPC/MVS OPERATOR STOP ECB                   
ECBVALUE DC    F'0'                ECB VALUE SAVE                               
AESUBECB DS    A                   A(ESSIO SUB TASK CONTROL ECB LIST)           
ASESSECB DS    A                   A(ESS SESSION CONTROL ECB LIST)              
AESSATC  DS    A                   A(ESSATC CONTROL BLOCK)                      
ANEXTDCB DS    A                   A(NEXT BULK DOWNLOAD DCB)                    
*                                                                               
APPCECBL DS    0F                  APPC/MVS ECB LIST                            
AAPPCEST DC    X'00',AL3(0)                                                     
AAPPCECB DC    X'00',AL3(0)                                                     
ARCVAECB DC    X'00',AL3(0)                                                     
ASNDAECB DC    X'80',AL3(0)                                                     
*                                                                               
TODAY2   DS    XL2                 TODAY -- BINARY COMPRESSED                   
TODAY3   DS    XL3                 TODAY -- BINARY YMD                          
TODAY6   DS    CL6                 TODAY -- EBCDIC YYMMDD                       
TODAY8   DS    CL8                 TODAY -- EBCDIC MMMDD/YY                     
OPERSTOP DC    C'N'                'Y' = OPERATOR ENTERED 'STOP'                
CONSOPEN DC    C'N'                'Y' = CONTROL SYSTEM OPEN                    
ENQCFLAG DC    C'N'                'Y' = CONTROL SYSTEM ENQUEUD                 
XMITESS  DC    C'Y'                'N' = DONT TRANSFER ESS PQ REPORTS           
FTPFLAG  DC    C'Y'                'N' = DONT TRANSFER FILES VIA NFTP           
RETCODE  DC    XL1'0'              XBASE RETURN CODE                            
STIMERID DS    XL4                 FOR TIMER POPS                               
TESTCHAR DS    C' '                TEST CHARACTER TO APPEND TO ESSIO            
WRITE    DC    C'Y'                                                             
ATCHFLAG DC    C'Y'                ATTACH ESSIO SUB TASK FLAG                   
CARD     DS    CL80                FOR CONTROL CARDS                            
KEY      DS    XL25                FOR CTFILE READS                             
*                                                                               
         DS    0D                                                               
ESTAERD  DC    F'0'                                                             
ESTAER1  DC    F'0'                                                             
MEUSREGS DC    XL72'00'            PROGRAM CHECK REGISTERS                      
MEBLOW   DC    X'00'               ABEND TYPE                                   
*                                                                               
MERUNLST DC    XL64'00'            PROGRAM CHECK REGISTERS                      
MEREQLST DC    XL64'00'            PROGRAM CHECK REGISTERS                      
MECHKPSW DC    A(0)                PROGRAM CHECK PSW                            
MELSTPSW DC    A(0)                LAST PSW                                     
MEAWORK  DC    A(0)                A(START OF DUMP)                             
MEPOPSW  DC    X'00'               C'P' IF TIMER POP ACTIVE                     
MEREASON DC    C' '                DUMP REASON CODE                             
MENDUMPS DC    PL3'0'              NUMBER OF DUMPS IN THIS RUN                  
MEREQNO  DC    PL3'0'              CURRENT REQUEST NUMBER                       
MEDUMP   DC    C'N'                DUMP=NO/YES                                  
MEDMPEND DC    A(0)                                                             
METSTRUN DC    X'00'                                                            
MERTPCPU DC    F'600'                                                           
*                                                                               
IOKEY    DS    CL(L'EDIKEY)        CTFILE RECORD KEY BUFFER                     
IOL      DS    F                                                                
IO       DC    2048X'00'           CTFILE I/O AREA (FOR ID RECORDS)             
ELEMENT  DS    XL256               RECORD ELEMENT WORK AREA                     
MEWORK   DS    2400D                                                            
SEWORK   DS    2400D                                                            
*                                                                               
WORKX    DS    0D                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'ESSAPPCC'                                                      
ESSAPPCC DS    XL(ESSAPPCL)        COMMON ESSAPPCD CSECT                        
*                                                                               
         DS    0D                                                               
         DC    C'RCVAPPCC'                                                      
RCVAPPCC DS    XL(ESSAPPCL)        ESSLURCV ESSAPPCD CSECT                      
*                                                                               
         DS    0D                                                               
         DC    C'SNDAPPCC'                                                      
SNDAPPCC DS    XL(ESSAPPCL)        ESSLUSND ESSAPPCD CSECT                      
*                                                                               
         DS    0D                                                               
         DC    C'*CXREC**'                                                      
CXREC    DC    14336X'00'          DATAMGR IO BUFFER                            
*                                                                               
         DS    0D                                                               
         DC    C'R13CHAIN'                                                      
R13CHAIN DS    8000D               WORKING STORAGE                              
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    C'*EATC**'                                                       
         CNOP  2,4                                                              
EATCB    DS    0H                  ESS IO ATTACHED TASK CONTROL BLOCKS          
EATCBEL  DC    AL2(EATLQ)          ENTRY LENGTH                                 
EATCBTL  DC    AL4(EATCBF)         TOTAL LENGTH ALL BLOCKS                      
EATCBF   DC    (EATCMAX*EATLQ)X'00'                                             
EATCBX   EQU   *                                                                
EATCMAX  EQU   60                                                               
*                                                                               
         DS    0D                                                               
         DC    C'*ESEC**'                                                       
         CNOP  2,4                                                              
ESECB    DS    0H                  ESS IO APPC SESSION CONTROL BLOCKS           
ESECBEL  DC    AL2(ESELQ)          ENTRY LENGTH                                 
ESECBTL  DC    AL4(ESECBF)         TOTAL LENGTH ALL BLOCKS                      
ESECBF   DC    (ESECMAX*ESELQ)X'00'                                             
ESECBX   EQU   *                                                                
ESECMAX  EQU   100                                                              
*                                                                               
         DS    0D                                                               
         DC    C'*ECBLST*'                                                      
ECBLIST  DS    (ESECMAX+2*EATCMAX+2)A                                           
*                                                                               
         DS    0D                                                               
         DC    C'**ESSB**'         SYSTEM STATUS CONTROL BLOCK                  
ESSB     DC    (ESSBLQ)X'00'                                                    
ESSBX    EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    C'*SHUTLS*'                                                      
SHUTLIST DS    (2)A                                                             
*                                                                               
         DC    C'*BDCBL**'                                                      
         DS    0D                                                               
BLKDCB   DCB   DDNAME=BLFILE,MACRF=(GM),DSORG=PS                                
BLKDCBLQ EQU   *-BLKDCB                                                         
*                                                                               
         DS    0D                                                               
BDCBC    DC    (ESECMAX*BLKDCBLQ)X'00'                                          
BDCBX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
         DS    0D                                                               
         DC    C'*UNKESS*'                                                      
UNKESST  DS    (1000)XL(L'PARTNER_LU_NAME)'00'                                  
UNKESSTX EQU   *                                                                
         PRINT OFF                                                              
*        IECTDECB                                                               
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         DCBD  DSORG=PS,DEVD=DA                                                 
         EJECT                                                                  
       ++INCLUDE CTGENEDICT                                                     
         EJECT                                                                  
       ++INCLUDE GEGENXTR                                                       
         EJECT                                                                  
       ++INCLUDE GEGENESS                                                       
         EJECT                                                                  
       ++INCLUDE DDEXSERVD                                                      
         EJECT                                                                  
       ++INCLUDE DDESSD                                                         
         EJECT                                                                  
       ++INCLUDE FASYSLSTD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE DMGREQUS                                                       
         EJECT                                                                  
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQS                                                        
         EJECT                                                                  
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
       ++INCLUDE DMDTFIS                                                        
         EJECT                                                                  
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         ORG   SSBD                                                             
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* DDESSAPPCD                                                                    
         PRINT ON                                                               
       ++INCLUDE DDESSAPPCD                                                     
         EJECT                                                                  
* IHASDWA                                                                       
         PRINT OFF                                                              
         IHASDWA GR32=YES                                                       
         PRINT ON                                                               
* IHAASCB                                                                       
         PRINT OFF                                                              
         IHAASCB                                                                
         PRINT ON                                                               
         EJECT                                                                  
         PRINT OFF                                                              
         IEFZB4D2                                                               
         EJECT                                                                  
         PRINT ON                                                               
         IEFZB4D0                                                               
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060DDHOSTESS 06/07/19'                                      
         END                                                                    
