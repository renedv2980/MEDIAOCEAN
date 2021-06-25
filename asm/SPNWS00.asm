*          DATA SET SPNWS00    AT LEVEL 250 AS OF 01/05/12                      
*PHASE T20700A,*                                                                
*INCLUDE EQVRD                                                                  
*INCLUDE GETPROF                                                                
*INCLUDE DPTRD                                                                  
T20700   TITLE 'SPNWS00 - NEW BUYERS WORKSHEET - CONTROLLER'                    
T20700   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T20700**,RA,R9,R8,R6,CLEAR=YES,RR=RE                 
         LR    R7,RC                                                            
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
*                                                                               
         MVC   ATWA,4(R1)          SAVE ADDRESS OF TWA                          
         MVC   ASYS,8(R1)          SAVE ADDRESS OF SYSFACS                      
         MVC   ATIA,12(R1)         SAVE ADDRESS OF TWA                          
*                                                                               
         L     R5,ATWA                                                          
         USING TWAD,R5             R5=A(TWA)                                    
*                                                                               
         ST    R1,ACPARMA          SET BASE ADDRESSES                           
         ST    RE,ACRELO                                                        
         ST    RB,ACBASE1                                                       
         ST    RA,ACBASE2                                                       
         ST    R9,ACBASE3                                                       
         ST    R8,ACBASE4                                                       
         ST    R6,ACBASE5                                                       
         ST    RD,ACWORKA                                                       
*                                                                               
         L     RE,ASYS             A(SYSPARMS)                                  
         USING SPSYSFAC,RE                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RE                                                               
*                                                                               
         OC    TWAOVBUF,TWAOVBUF   TEST OFFLINE                                 
         BNZ   *+14                                                             
         OC    ASIOASTR,ASIOASTR   OR SYSTEM EXTRA AREA NOT AVAILABLE           
         BNZ   NWS10                                                            
         LA    R1,2000             YES-                                         
         LA    R0,MINNBUFF         GET EXTRA WORKING STORAGE                    
         MR    R0,R0               FOR MINIO BUFFERS AND TSAR BLOCK             
         LA    R1,7(R1)                                                         
         SRL   R1,3                                                             
         SLL   R1,3                                                             
         LA    R0,WORKD                                                         
         AHI   R0,WORKX-WORKD                                                   
         LR    RE,R0                                                            
         ST    RE,AMINBUFF                                                      
         AR    RE,R1                                                            
         ST    RE,ATSARBLK                                                      
         LA    RF,TSARDL                                                        
         AR    R1,RF                                                            
         LA    R1,7(R1)                                                         
         SRL   R1,3                                                             
         SLL   R1,3                                                             
******** OC    TWAOVBUF,TWAOVBUF   TEST ONLINE                                  
******** BNZ   NWS0                                                             
         LR    RE,R0               YES-ALSO GET STORAGE FOR GLOBAL              
         AR    RE,R1                   AND IOAREA4                              
         ST    RE,AGLOBAL                                                       
         LHI   RF,LOCALEND-GLOBALD                                              
         AR    R1,RF                                                            
         LA    R1,7(R1)                                                         
         SRL   R1,3                                                             
         SLL   R1,3                                                             
         LR    RE,R0                                                            
         AR    RE,R1                                                            
         ST    RE,AIOAREA4                                                      
****     LA    R1,4000(R1)                                                      
         AHI   R1,6000                                                          
*                                                                               
NWS05    L     RE,4(RD)            R1=TOTAL LENGTH OF EXTRA WORKING             
         AR    RD,R1                  STORAGE                                   
         ST    RD,ACWORKA                                                       
         ST    RD,8(RE)                                                         
         ST    RE,4(RD)                                                         
         SR    RE,RE               CLEAR                                        
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         ICM   R1,15,AGLOBAL                                                    
         BZ    NWS10                                                            
         MVC   0(8,R1),=C'*GLOBAL*'                                             
         LHI   RF,LOCALEND-GLOBALD                                              
         ST    RF,GLSIZE-GLOBALD(R1)                                            
*                                                                               
NWS10    DS    0H                                                               
****  LOADING THE SLNTAB HERE                                                   
         L     RF,ASYS            A(SYSFACS)                                    
         L     RF,VCALLOV-SYSFACD(RF)                                           
         MVC   ACPARM+4(4),=X'D9000A57'   GET A(SLNTAB)                         
         GOTO1 (RF),ACPARM,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,ACPARM           POINT TO START OF PHASE                      
         ST    R1,VSLNTAB          SAVE THIS OFF                                
****  LOADING THE SLNTAB HERE         MHC  05/31/06                             
****  LOADING SPBUYVAL HERE           MHC  05/08/07                             
         L     RF,ASYS            A(SYSFACS)                                    
         L     RF,VCALLOV-SYSFACD(RF)                                           
         MVC   ACPARM+4(4),=X'D9000A2A'   GET A(SPBUYVAL)                       
         GOTO1 (RF),ACPARM,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,ACPARM           POINT TO START OF PHASE                      
         ST    R1,VSPBYVAL         SAVE THIS OFF                                
****  LOADING SPBUYVAL HERE           MHC  05/08/07                             
         L     RF,ASYS            A(SYSFACS)                                    
         L     RF,VCALLOV-SYSFACD(RF)                                           
         GOTO1 (RF),ACPARM,(1,0),0,0         LOAD TABLE PHASE (01)              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                CAN'T LOAD CONTROLLER TABLES                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,1(R1)          A(LOAD POINT)                                
         LA    R0,(ACSELTAB-ACRECTAB)/4+1 N'TABLES                              
         SR    RE,RE                                                            
*                                                                               
NWS20    L     R1,0(RE,RF)                                                      
         AR    R1,RF                                                            
         ST    R1,ACRECTAB(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,NWS20                                                         
*                                                                               
         L     R1,0(RE,RF)         TABLE BASE FOR OPTION VALIDATION             
         AR    R1,RF               TABLES                                       
         ST    R1,ACTBASE                                                       
         OI    ACOPTIND,ACOPTITB                                                
*                                                                               
         L     R1,=A(CONADDRS)     SET HOOK ADDRESSES                           
         A     R1,ACRELO                                                        
         SR    RE,RE                                                            
         LA    R0,CONADDRN                                                      
NWS40    L     RF,0(R1,RE)                                                      
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         A     RF,ACRELO                                                        
         ST    RF,ACPHSLST(RE)                                                  
         LA    RE,4(RE)                                                         
         BCT   R0,NWS40                                                         
*                                                                               
         LA    R1,ROUTS            SET EXTRA COMMON CONTROLLER ROUTINES         
         LA    R0,NROUTSX                                                       
         SR    RE,RE                                                            
         LA    RF,AROUTN                                                        
         ST    R1,AROUTSX(RE)                                                   
         STC   RF,AROUTSX(RE)                                                   
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
*                                                                               
         LA    R0,WORKD            SET I/O VARIABLES                            
         AHI   R0,IODA1-WORKD                                                   
         ST    R0,ACIOADD                                                       
         LHI   R0,IODA2-IODA1                                                   
         STH   R0,ACIOLEN                                                       
         MVI   ACIONUM,3                                                        
         MVI   ACIOIND,ACIOIDA+ACIOIWK                                          
*                                                                               
         MVC   ACSYSPGM,=X'0207'                                                
         MVI   ACHLPSCR,X'F0'      SET TWA VARIABLES                            
         MVI   ACTWAREC,1                                                       
         MVI   ACTWAACT,2                                                       
         MVI   ACTWAKEY,3                                                       
         MVI   ACTWAOPT,5                                                       
*                                                                               
         LHI   R0,BWSTABH-TWAD                                                  
         STCM  R0,3,ACENDTWA                                                    
         LHI   R1,IOAREA1-WORKD                                                 
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA1                                                      
         LHI   R1,IOAREA2-WORKD                                                 
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA2                                                      
         LHI   R1,IOAREA3-WORKD                                                 
         LA    R1,WORKD(R1)                                                     
         ST    R1,AIOAREA3                                                      
*                                                                               
         LHI   R1,MINRECTB-WORKD                                                
         LA    R1,WORKD(R1)                                                     
         ST    R1,AMINRTAB                                                      
         LHI   R1,MINBLK-WORKD                                                  
         LA    R1,WORKD(R1)                                                     
         ST    R1,AMINBLK                                                       
*                                                                               
         LHI   R1,APLOCAL-WORKD                                                 
         LA    R1,WORKD(R1)                                                     
         ST    R1,APALOCAL                                                      
*                                                                               
         L     R1,ACRELO                                                        
         L     RE,=V(EQVRD)                                                     
         AR    RE,R1                                                            
         ST    RE,VEQVRD                                                        
         L     RE,=V(GETPROF)                                                   
         AR    RE,R1                                                            
         ST    RE,VGETPROF                                                      
         L     RE,=V(DPTRD)                                                     
         AR    RE,R1                                                            
         ST    RE,VDPTRD                                                        
*                                                                               
         L     R1,ACPARMA                                                       
         L     R1,0(R1)            R1=A(TIOB)                                   
         MVC   ACCURD,TIOBCURD-TIOBD(R1)  SET CURSOR DISPLACEMENT               
         MVC   ACCURS,TIOBCURS-TIOBD(R1)  SET CURSOR ABS SCREEN ADDRESS         
*                                                                               
         MVI   ACFSTIND,ACHKBEF    CONTROLLER HOOK                              
         MVI   ACKEYIND,ACHKAFT    HOOK AFTER KEY FIELD PRE-VALIDATION          
         MVI   ACRECIND,ACHKBEF    HOOK BEFORE RECORD VALIDATION                
         OI    ACRECIND,ACHKAFT    HOOK AFTER RECORD VALIDATION                 
         MVI   ACACTIND,ACHKAFT    HOOK AFTER ACTION VALIDATION                 
         MVI   ACLFMIND,ACHKBEF    HOOK BEFORE MAINTENANCE ACTION               
         MVI   ACLSMIND,ACHKBEF    HOOK BEFORE LSM ACTION                       
         MVI   ACLSTIND,ACHKBEF    HOOK AT TERMINATION                          
*                                                                               
         OI    ACLSMIND,ACLSMISK   EXTENDED SELECT TABLE ENTRIES                
         OI    APINDS2,APIMDIS2    DISPLAY LIST/SELECT RECORD AFTER LFM         
***********************************                                             
* HERE WE GO WITH CLIENT STRING SECURITY                                        
***********************************                                             
NWS42    NI    NWSFLAG,X'FF'-NWSFCLST                                           
         OC    TWASAGN,TWASAGN     ON NEW SECURITY?                             
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS    OR HAVE LIMIT ACCESS?                      
         BZ    NWS45                                                            
*                                                                               
         LR    RF,R5               14K INTO TWA USED FOR SECRET                 
         AHI   RF,14*1024                                                       
         XC    0(256,RF),0(RF)     <==  SO SECRET WILL INIT PROPERLY            
*                                                                               
         XC    APPARM(8),APPARM                                                 
         ST    RF,APPARM                                                        
         MVI   APPARM,SECPINIT                                                  
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           INITIALIZE SECRET                            
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),APPARM,,0                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    RF,R5               14K INTO TWA USED FOR SECRET                 
         AHI   RF,14*1024                                                       
         USING SECD,RF                                                          
         OC    SECCLAL,SECCLAL                                                  
         BZ    *+8                                                              
         OI    NWSFLAG,NWSFCLST    USES CLIENT STRING SECURITY                  
         DROP  RF                                                               
***********************************                                             
* DONE WITH CLIENT STRING SECURITY                                              
***********************************                                             
NWS45    OC    TWAOVBUF,TWAOVBUF   TEST OFFLINE                                 
         BNZ   NWS50                                                            
         NI    TWATAMFL,X'FF'-TWAFRTAM-TWAWSSDN                                 
         GOTO1 =A(CALLNWS),RR=ACRELO                                            
NWS50    STCM  RD,15,TWAB4GNL      A WAY FOR OVERLAYS TO EXIT GENERAL           
         TM    TWATAMFL,TWAFRTAM   FROM TV AVAIL MANAGER?                       
         BZ    NWS60                                                            
NWS55    DS    0H                                                               
***                                                                             
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL                                         
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,(X'80',0),F#MIOST   TURN ON CAN EXCEED I/OS          
***                                                                             
         GOTO1 =A(FROMTAM),RR=ACRELO                                            
         TM    TWATAMFL,TWATAMER   ERROR COMING BACK FROM FROMTAM?              
         BNZ   NWS65               YES, DON'T BOTHER CALLING GENERAL            
         TM    TWATAMFL,TWAWSSDN                                                
         BZ    NWS60                                                            
***                                                                             
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL                                         
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),APPARM,(X'80',0),F#MIOUN   TURN OFF EXCEED I/OS             
***                                                                             
         B     EXIT                                                             
*                                                                               
NWS60    L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL GENERAL CONTROLLER                      
         L     RF,CGENERAL-COMFACSD(RF)                                         
         LA    R1,WORKD                                                         
         BASR  RE,RF                                                            
*                                                                               
NWS65    TM    TWATAMFL,TWAFRTAM   FROM TV AVAIL MANAGER?                       
         BZ    EXIT                NO, JUST EXIT                                
         B     NWS55               LOOP FOR NEXT ENTRY IN WSSVR BUFFER          
         EJECT                                                                  
***********************************************************************         
* GENERAL HOOK                                                        *         
***********************************************************************         
         SPACE 1                                                                
HOOK     NTR1                                                                   
         L     R5,ATWA                                                          
         ZIC   RF,ACMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     FIRST                                                            
         B     RECORD                                                           
         B     ACTION                                                           
         B     PROCKEY                                                          
         B     HOOKX                                                            
         B     PROCLFM                                                          
         B     PROCLSM                                                          
         B     HOOKX                                                            
         B     HOOKX                                                            
         B     HOOKX                                                            
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         DC    AL4(0)              N/D                                          
         B     LAST                                                             
*                                                                               
HOOKX    CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FIRST TIME CONTROLLER HOOK                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RWRKD,RC                                                         
FIRST    CLC   CUAALF,QAGY         TEST AGENCY HAS CHANGED                      
         BE    FIRST100                                                         
         XC    IOKEY,IOKEY         YES-READ AGENCY HEADER                       
         MVI   IOKEY,6                                                          
         MVC   IOKEY+1(2),CUAALF                                                
         GOTO1 AIO,FILRD1                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOAREA1                                                      
         USING AGYHDRD,R1                                                       
*                                                                               
*****    MVC   APROF7,AGYPROF+7                                                 
         MVI   APROFBTS,0                                                       
         CLI   AGYPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   *+8                                                              
         OI    APROFBTS,A00CANAD   YES                                          
**       CLI   AGYPROF+9,C'Y'      USES 2 DECIMAL PRECISION?                    
**  WE'RE GONNA READ THE 00 PROFILE                                             
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(4),=C'S000'                                                
         MVC   IOKEY+4(2),CUAALF                                                
***   USING X'D0' FOR RETURNING DEFAULT PROFILE AND NOT USER ID                 
         GOTO1 VGETPROF,RPARM,(X'D0',IOKEY),RWORK,VDMGR                         
         CLI   RWORK+9,C'Y'        USES 2 DECIMAL PRECISION?                    
         BNE   *+8                                                              
         OI    APROFBTS,A002DOK    YES                                          
*                                                                               
****  WE'RE READING THE SUPERDESK PROFILE                                       
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(4),=C'S0SD'                                                
         MVC   IOKEY+4(2),CUAALF                                                
***   USING X'D0' FOR RETURNING DEFAULT PROFILE AND NOT USER ID                 
         GOTO1 VGETPROF,RPARM,(X'D0',IOKEY),RWORK,VDMGR                         
         CLI   RWORK+13,C'Y'       NOT UPDATING SUPERDESK ON WORK ADD?          
         BNE   *+8                                                              
         OI    APROFBTS,ASDNOWRK   DON'T DO ANY WORK                            
****  WE'RE READING THE SUPERDESK PROFILE                                       
         MVC   QAGY,CUAALF                                                      
         MVC   AFLAG1,AGYFLAG1                                                  
******                                                                          
         LHI   RF,SVRFPID-TWAD                                                  
         AR    RF,R5                                                            
         XC    0(L'SVRFPID,RF),0(RF)                                            
         XR    R0,R0                                                            
         LA    R1,24(R1)                                                        
FIRST10  CLI   0(R1),0                                                          
         BE    FIRST100                                                         
         CLI   0(R1),X'71'                                                      
         BE    FIRST20                                                          
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FIRST10                                                          
*                                                                               
         USING AGYEXTEL,R1                                                      
FIRST20  MVC   0(L'SVRFPID,RF),AGYPRNID    PRINCIPAL AGENCY ID NUM              
         DROP  R1                                                               
FIRST100 MVI   CUDMED,C'T'         DEFAULT DEMO FILE MEDIA = T                  
******   CLI   APROF7,C'C'         TEST CANADIAN AGENCY                         
******   BNE   *+16                                                             
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    *+16                                                             
*                                                                               
         TM    CLTIND2,CLTIUS      AND CLIENT DOESN'T WANT US DEMOS             
         BO    *+8                                                              
         MVI   CUDMED,C'C'         YES - DEMO FILE MEDIA = C                    
*                                                                               
         TM    CUSTAT,CUSPER       TEST PERSONAL PASSWORD                       
         BZ    FIRST160                                                         
         LA    R1,IOKEY            READ PERSONAL AUTHORIZATION RECORD           
         USING CT0KEY,R1                                                        
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,CUAALF                                                   
         CLC   CUAGYSEC,=C'  '     TEST SECURITY AGENCY                         
         BNH   *+10                                                             
         MVC   CT0KAGY,CUAGYSEC    YES-USE THAT                                 
         MVC   CT0KNUM,CUPASS                                                   
         CLC   CUPASS,=H'4095'                                                  
         BNL   *+10                                                             
         MVC   CT0KAGY,=C'#N'                                                   
         GOTO1 AIO,IORD+IOCTFILE+IO1                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
         L     R1,IOADDR                                                        
         USING CT0KEY,R1                                                        
         LA    R1,CT0DATA                                                       
*                                                                               
FIRST140 CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R1),=X'030C'                                                 
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     FIRST140                                                         
         MVC   CUPASSWD,2(R1)      SECRET LOGON CODE                            
*                                                                               
FIRST160 CLI   ASONOFF,ASON        TEST ONLINE                                  
         BNE   FIRST180                                                         
         ICM   RE,15,ASIOASTR      AND SYSTEM EXTRA STORAGE AVAILABLE           
         BZ    FIRST180                                                         
         ST    RE,AMINBUFF         YES-SET VARIOUS BUFFER ADDRESSES             
         LA    R1,2000                 THERE                                    
         LA    R0,MINNBUFF                                                      
         MR    R0,R0               R1=TOTAL L'MINIO BUFFERS                     
         LA    RE,0(R1,RE)                                                      
         ST    RE,AGLOBAL                                                       
         LHI   RF,LOCALEND-GLOBALD                                              
         AR    R1,RF                                                            
         C     R1,ASIOALEN         TEST THERE'S ENOUGH ROOM IN SYSTEM           
         BNH   *+6                 EXTRA AREA                                   
         DC    H'0'                NO-BETTER USE WORKING STORAGE                
         LR    R2,RE                                                            
         LR    R3,RF                                                            
         XCEF  ,                   CLEAR GLOBAL                                 
         MVC   0(8,R2),=C'*GLOBAL*'      AND INITIALIZE                         
         ST    R3,GLSIZE-GLOBALD(R2)                                            
         LA    RE,0(R2,R3)                                                      
         ST    RE,ATSARBLK         A(TSAR BLOCK)                                
         LA    RF,TSARDL                                                        
         AR    R1,RF                                                            
         C     R1,ASIOALEN         TEST THERE'S ENOUGH ROOM IN SYSTEM           
         BNH   *+6                 EXTRA AREA                                   
         DC    H'0'                NO-BETTER USE WORKING STORAGE                
         LR    R2,RE                                                            
         LR    R3,RF                                                            
         XCEF  ,                   CLEAR TSAR BLOCK                             
         LA    RE,0(R2,R3)                                                      
         ST    RE,AIOAREA4                                                      
****     LA    R1,4000(R1)                                                      
         AHI   R1,6000                                                          
         C     R1,ASIOALEN         TEST THERE'S ENOUGH ROOM IN SYSTEM           
         BNH   *+6                 EXTRA AREA                                   
         DC    H'0'                NO-BETTER USE WORKING STORAGE                
*                                                                               
FIRST180 LA    R0,GOMSPACK                                                      
         ST    R0,VMSPACK                                                       
         LA    R0,GOMSUNPK                                                      
         ST    R0,VMSUNPK                                                       
FIRSTX   B     HOOKX                                                            
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* HOOK BEFORE/AFTER RECORD VALIDATION                                 *         
***********************************************************************         
         SPACE 1                                                                
RECORD   CLI   TWALREC,RECSID      TEST PREVIOUS RECORD WAS NSID                
         BNE   *+8                                                              
         MVI   TWALREC,RECWRK      YES-FORCE THIS TO RECORD=WORK                
         CLI   INREC,RECNET        TEST RECORD=NETWORK                          
         BNE   HOOKX                                                            
         TM    CUSTAT,CUSDDS       YES-TEST DDS                                 
         BO    HOOKX                                                            
         CLI   CUDMED,C'C'         OR CANADIAN                                  
         BE    HOOKX                                                            
         MVC   FVMSGNO,=AL2(FVIRECCA) NO-INVALID                                
         B     HOOKX                                                            
         SPACE 2                                                                
***********************************************************************         
* HOOK AFTER ACTION VALIDATION                                        *         
***********************************************************************         
         SPACE 1                                                                
ACTION   DS    0H                                                               
         BRAS  RE,ACTIONOK         CHECK IF THE ACTION IS OKAY                  
         BNE   ACT9                                                             
*                                                                               
ACT00    CLI   INACT,ACTMKT        MSPILL ACTION ONLY VALID FOR CANADA          
         BNE   *+12                                                             
         CLI   CUDMED,C'C'                                                      
         BNE   ACT9                                                             
         CLI   ASONOFF,ASON        IF ONLINE,                                   
         BNE   ACT4                                                             
         MVC   ACWORK(1),TWAMODE2  EXTRACT CURRENT LIST LEVEL                   
         NI    ACWORK,TWAMLVL                                                   
         CLI   SCACTN,ACTDRP       TEST ACTION=RECAP                            
         BE    *+8                                                              
         NI    TWAINDS,255-(TWAIRCP1+TWAIRCP2)  NO-TURN OFF RECAP BITS          
         CLI   SCACTN,ACTSKD       TEST SCHEDULE ACTION                         
         BE    ACT2                                                             
         CLI   SCACTN,ACTSSK                                                    
         BE    ACT2                                                             
         TM    TWAINDS,TWAISKD     NO-TEST SCHEDULE ACTION HAS BEEN             
         BZ    ACT4                                                             
         CLC   TWASKDLV,ACWORK     YES-TEST ITS LIST LEVEL                      
         BL    ACT4                                                             
         NI    TWAINDS,255-TWAISKD        NOT LESS THAN CURRENT LEVEL,          
         MVI   TWASKDLV,0                 SO CLEAR                              
         B     ACT4                                                             
*                                                                               
ACT2     OI    TWAINDS,TWAISKD     SET SCHEDULE INDICATOR                       
         MVC   TWASKDLV,ACWORK                                                  
         MVI   ACPFFST,PFK11       SET PF11 = RESTART LIST                      
*                                                                               
ACT4     CLI   ASONOFF,ASOFF       TEST OFFLINE DRIVER REPORT APPLIC            
         BNE   ACTX                                                             
         TM    INMIX1,MIXIREP                                                   
         BZ    ACTX                                                             
         TM    INMIX2,MIXIDRV                                                   
         BZ    ACTX                                                             
         MVC   VBUFFALO,TWAOVBUF   YES-SET V(BUFFALO)                           
         B     ACTX                                                             
*                                                                               
ACT9     MVC   FVMSGNO,=AL2(FVFNOTV)     INVALID ACTION                         
*                                                                               
ACTX     B     HOOKX                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY HOOK                                                    *         
***********************************************************************         
         SPACE 1                                                                
PROCKEY  DS    0H                  AFTER KEY PRE-VALIDATION                     
         XC    INFPWD,INFPWD       VALIDATE PASSWORD FIELD                      
         TM    CUSTAT,CUSPER       TEST SECRET LOGON CODE                       
         BZ    PROCK2                                                           
         TM    CUSTAT,CUSDDS       AND NOT DDS TERMINAL                         
         BO    PROCK2                                                           
         MVI   INFPWD,C' '         YES-THEN SET DEFAULT PASSWORD                
         MVC   INFPWD+1(L'INFPWD-1),INFPWD                                      
         MVC   INFPWD(L'CUPASSWD),CUPASSWD                                      
*                                                                               
PROCK2   GOTO1 AFVAL,BWSPWDH                                                    
         BH    PROCKX                                                           
         BL    *+10                                                             
         MVC   INFPWD,FVIFLD                                                    
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     PROCKX                                                           
*                                                                               
PROCKX   B     HOOKX                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS FILE MAINTENANCE HOOK                                       *         
***********************************************************************         
         SPACE 1                                                                
PROCLFM  DS    0H                  BEFORE FILE MAINTENANCE                      
         L     R1,AMIXNTRY                                                      
         USING MIXTABD,R1                                                       
         TM    MIXINDS,MIXILFM     DOUBLE CHECK IT'S FILE MAINT                 
         BZ    PROCLX                                                           
         CLI   MIXUSER,0           ACTION OVERRIDE                              
         BE    PROCLX                                                           
         MVC   ACSVACT,INACT                                                    
         MVC   INACT,MIXUSER                                                    
         B     PROCLX                                                           
*                                                                               
PROCLX   B     HOOKX                                                            
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* PROCESS LIST/SELECT MONITOR HOOK                                    *         
***********************************************************************         
         SPACE 1                                                                
PROCLSM  DS    0H                  BEFORE LSM PROCESSING                        
         CLI   APMODE,APMRET       TEST RETURNING TO LSM MODE                   
         BNE   *+8                                                              
         OI    TWAMODE,TWAMLSM     YES-DUPE GENERAL INTO THINKING               
         B     HOOKX                   WE'RE ALREADY IN LSM                     
         SPACE 2                                                                
***********************************************************************         
* TERMINATION HOOK                                                    *         
***********************************************************************         
         SPACE 1                                                                
LAST     DS    0H                                                               
         ICM   RE,15,AINP                                                       
         BZ    LAST1                                                            
         USING TIOBD,RE                                                         
         L     R1,AACTNTRY                                                      
         USING ACTTABD,R1                                                       
         OI    TIOBINDS,TIOBSUBS   SET SUB SCREEN NUMBER                        
         MVC   TIOBAID,ACTUSER+7                                                
         CLI   INREC,RECWRK        TEST WORK/TRANSFER                           
         BNE   LAST0                                                            
         CLI   INACT,ACTXFR                                                     
         BNE   LAST0                                                            
         CLI   TWALSCR,X'FB'       AND LAST SCREEN WAS UPDATE OR SKED           
         BNE   LAST0                                                            
         MVC   TIOBAID,TWALSUBS    YES-SET SUBSCREEN TO LAST ONE                
*                                                                               
LAST0    MVC   TWALSUBS,TIOBAID    SAVE SUB SCREEN NUM                          
         DROP  R1,RE                                                            
*                                                                               
LAST1    MVC   TWALSCR,INSCRN      SAVE SCREEN NUMBER                           
         GOTO1 AMIN,MINCLS                                                      
         TM    INOIND,INOITST                                                   
         BZ    LAST4                                                            
         L     R4,AIOAREA2                                                      
         OC    0(13,R4),0(R4)                                                   
         BNZ   LAST2                                                            
         L     R4,AIOAREA1                                                      
         CLI   0(R4),0                                                          
         BNE   LAST2                                                            
         LA    R4,APRECKEY                                                      
         CLI   0(R4),0                                                          
         BNE   LAST2                                                            
         LA    R4,IOKEY                                                         
*                                                                               
LAST2    GOTO1 VHEXOUT,ACPARM,(R4),BWSOPT,13                                    
         OI    BWSOPTH+6,FVOXMT                                                 
*                                                                               
LAST4    CLI   ASONOFF,ASON        TSAR ONLY ONLINE                             
         BNE   LAST6                                                            
         TM    TWAINDS,TWAITSIN    TEST TSAR INITIALIZED                        
         BZ    LAST6                                                            
         TM    TWAINDS,TWAITSRS    YES-TEST TSAR WAS RESTORED                   
         BZ    LAST6                                                            
         MVI   TSARACT,TSASAV      YES-SAVE TSAR INFORMATION                    
         GOTO1 ATSAR                                                            
         NI    TWAINDS,255-TWAITSRS                                             
*                                                                               
LAST6    B     LASTX                                                            
*                                                                               
LASTX    B     HOOKX                                                            
         EJECT                                                                  
***********************************************************************         
* COMMON ROUTINES AVAILABLE TO CONTROLLER AND OVERLAYS                *         
*                                                                     *         
* NTRY - R1=A(PARAMETER LIST)                                         *         
*        RF=ROUTINE NUMBER (HIGH ORDER BYTE)                          *         
*                                                                     *         
* EXIT - FVMSGNO SET TO ERROR NUMBER WITH CC=NEQ ON ERROR             *         
*        FVMSGNO SET TO FVFOK WITH CC=EQ IF OK                        *         
***********************************************************************         
         SPACE 1                                                                
ROUTS    NTR1  BASE=ACBASE1,LABEL=NO                                            
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         L     R6,ACBASE5                                                       
         L     R5,ATWA                                                          
         L     RE,4(RD)            MOVE REGISTER SAVE AREA                      
         LR    RC,RD               TO MAKE ROOM FOR WORK AREA                   
         AHI   RD,RWRKX-RWRKD                                                   
         LA    RD,7(RD)                                                         
         SRL   RD,3                                                             
         SLL   RD,3                                                             
         ST    RE,4(RD)            BACKWARD POINTER                             
         ST    RD,8(RE)            FORWARD POINTER                              
*                                                                               
         USING RWRKD,RC            RC=A(LOCAL W/S)                              
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   RIOSAVE,IOAREA      SAVE I/O AREA                                
         MVI   RSPACES,C' '                                                     
         MVC   RSPACES+1(L'RSPACES-1),RSPACES                                   
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     ROUTSBR(RF)                                                      
*                                                                               
ROUTSX   CLC   RIOSAVE,IOAREA      TEST ANY I/O EXECUTED                        
         BE    *+14                                                             
         OI    APINDS,APILRERD     YES - SET APPLICATION FLAG                   
         MVC   IOAREA(L'RIOSAVE),RIOSAVE                                        
         CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR CALLER                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE OF BRANCH ADDRESSES TO ROUTS ROUTINES                         *         
***********************************************************************         
         SPACE 1                                                                
ROUTSBR  B     ADDELS                                                           
         B     DELELS                                                           
         B     RECUPA                                                           
         B     VALBYR                                                           
         B     GETBYR                                                           
         B     VALCAM                                                           
         B     GETCAM                                                           
         B     VALMED                                                           
         B     GETMED                                                           
         B     VALCLT                                                           
         B     GETCLT                                                           
         B     VALPRD                                                           
         B     GETPRD                                                           
         B     VALEST                                                           
         B     GETEST                                                           
         B     GETCM                                                            
         B     VALMKT                                                           
         B     GETMKT                                                           
         B     VALSTA                                                           
         B     GETSTA                                                           
         B     VALDAYS                                                          
         B     GETDAY                                                           
         B     VALTIM                                                           
         B     GETTIM                                                           
         B     VALDPL                                                           
         B     GETDPT                                                           
         B     VALPWD                                                           
         B     VALDAT                                                           
         B     VALUPG                                                           
         B     VALADJ                                                           
         B     MINIO                                                            
         B     SPCLERR                                                          
*                                                                               
         B     GETGOAL                                                          
         B     DTLBLD                                                           
         B     XFRADD                                                           
         B     PACKTIME                                                         
         B     TSARBWS                                                          
         B     COSTEQU                                                          
         B     NETCOST                                                          
         B     GETNET                                                           
         B     GETGOAL2                                                         
         B     SPDEMUP                                                          
         B     FMTUPG                                                           
         B     SETUPNBR            CALLED WITH ASTUPNBR                         
         B     CHKBPPRF            CALLED WITH ACHKBPPR                         
         B     GETCMPRC            CALLED WITH AGETCMPR                         
         B     PUTCMPRC            CALLED WITH APUTCMPR                         
         B     GETBKTYP            CALLED WITH AGETBKTY                         
         B     ADJPRC              CALLED WITH AADJPREC                         
         B     ADJDEMO             CALLED WITH AADJDEMO                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - R1=A(RECORD)                                                 *         
*        APELEM CONTAINS ELEMENT TO BE ADDED                          *         
* EXIT - FVMSGNO = FVRECOF IF RECORD OVERFLOW                         *         
***********************************************************************         
         SPACE 1                                                                
ADDELS   MVI   RFLAG,1                                                          
         B     ADDELS2                                                          
*                                                                               
ADDELSC  ST    RE,RFULL                                                         
         MVI   RFLAG,0                                                          
*                                                                               
ADDELS2  LR    R0,R1                                                            
         GOTO1 VHELLO,ACPARM,(C'P',=C'SPTFIL '),(R0),APELEM,0                   
         CLI   12(R1),5            TEST RECORD OVERFLOW                         
         BE    ADDELS9                                                          
         CLI   12(R1),0            TEST SUCCESSFUL                              
         BE    ADDELSX                                                          
         DC    H'0'                                                             
*                                                                               
ADDELS9  MVC   FVMSGNO,=AL2(FVRECOF)                                            
*                                                                               
ADDELSX  CLI   RFLAG,1                                                          
         BE    ROUTSX                                                           
         L     RE,RFULL                                                         
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DELETE AN ELEMENT (OR ELEMENTS) FROM A RECORD            *         
*                                                                     *         
* NTRY - R1=A(RECORD)                                                 *         
*        APELEM CONTAINS ELEMENT CODE OF ELEMENT TO BE DELETED        *         
***********************************************************************         
         SPACE 1                                                                
DELELS   LR    R0,R1                                                            
         GOTO1 VHELLO,ACPARM,(C'D',=C'SPTFIL '),(APELEM,(R0)),0                 
         CLI   12(R1),6            TEST ELEMENT NOT FOUND                       
         BE    DELELSX                                                          
         CLI   12(R1),0                                                         
         BE    DELELSX                                                          
         DC    H'0'                                                             
*                                                                               
DELELSX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD AT A SPECIFIED INSERTION POINT*         
*                                                                     *         
* NTRY - 0(R1) = A(RECORD)                                            *         
*        4(R1) = A(INSERTION POINT)                                   *         
*        APELEM CONTAINS ELEMENT TO BE ADDED                          *         
* EXIT - FVMSGNO = FVRECOF IF RECORD OVERFLOW                         *         
***********************************************************************         
         SPACE 1                                                                
RECUPA   MVC   ACPARM(4),0(R1)     A(RECORD)                                    
         MVI   ACPARM,0            SPOTPAK                                      
         LA    RE,APELEM                                                        
         ST    RE,ACPARM+4         A(ELEMENT)                                   
         MVC   ACPARM+8(4),4(R1)   A(INSERTION POINT)                           
         MVI   ACPARM+8,C'R'       DON'T BLOW UP ON RECORD OVERFLOW             
         GOTO1 VRECUP,ACPARM                                                    
         CLI   8(R1),0                                                          
         BNE   RECUPAX                                                          
         MVC   FVMSGNO,=AL2(FVRECOF)      RECORD OVERFLOW                       
*                                                                               
RECUPAX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE BUYER CODE                                      *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF BUYER FIELD)                            *         
*                                                                     *         
* EXIT - CC=EQUAL IF BUYER IS VALID WITH BUYER VALUES EXTRACTED       *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALBYR   BRAS  RE,VALIBYR                                                       
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET BUYER RECORD                                         *         
*                                                                     *         
* NTRY - R1=A(BUYER)                                                  *         
*        HIGH ORDER BYTE OF R1 EQ 0 - BUYER=3-BYTE BUYER CODE         *         
*        HIGH ORDER BYTE OF R1 NE 0 - BUYER=1-BYTE BUYER CODE         *         
*                                                                     *         
* EXIT - CC=EQUAL IF BUYER IS VALID WITH BUYER VALUES EXTRACTED       *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
GETBYR   LA    R2,IOKEY                                                         
         USING BYRRECD,R2                                                       
         XC    BYRKEY,BYRKEY                                                    
         CLM   R1,8,=X'00'                                                      
         BNE   GETBYR2                                                          
         MVI   RFLAG,0                                                          
         MVC   RDUB(L'QBYR),0(R1)                                               
         CLI   BBYR,0              THIS HAPPENS FROM CAM/LIST                   
         BE    *+14                                                             
         CLC   QBYR,RDUB           TEST CHANGE OF BUYER                         
         BE    GETBYRX                                                          
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAGMD,BAGYMD                                                  
         MVC   BYRKBYR,RDUB                                                     
         B     GETBYR4                                                          
*                                                                               
GETBYR2  MVI   RFLAG,1                                                          
         MVC   RDUB(L'BBYR),0(R1)                                               
         CLC   BBYR,RDUB           TEST CHANGE OF BUYER                         
         BE    GETBYRX                                                          
         MVI   BYRPTYP,BYRPTYPQ                                                 
         MVI   BYRPSUB,BYRPSUBQ                                                 
         MVC   BYRPAGMD,BAGYMD                                                  
         OC    BYRPAGMD,BBYRMASK   ALWAYS APPLY THE AGY/MED MASK                
         MVC   BYRPBYR,RDUB                                                     
*                                                                               
GETBYR4  LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   GETBYR9                                                          
         LA    R2,RIO                                                           
         MVC   BYRNM,BYRNAME                                                    
         MVC   BYRPW,BYRPASS                                                    
         XC    BBYR(BVALSX-BBYR),BBYR                                           
         MVC   BBYR,BYRCODE                                                     
         MVC   BBYRMASK,BYRMASK                                                 
         XC    QBYR(QVALSX-QBYR),QBYR                                           
         MVC   QBYR,BYRKBYR                                                     
         B     GETBYRX                                                          
*                                                                               
GETBYR9  MVC   FVMSGNO,=AL2(FVIBYR)                                             
*                                                                               
GETBYRX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE CAMAPIGN CODE                                   *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF CAMPAIGN FIELD)                         *         
*                                                                     *         
* EXIT - CC=EQUAL IF CAMPAIGN IS VALID WITH CAMPAIGN VALUES EXTRACTED *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALCAM   MVI   FVMINL,1            REQUIRED FIELD                               
         MVI   FVMAXL,L'QCAM                                                    
         GOTO1 AFVAL                                                            
         BNE   VALCAMX                                                          
         TM    FVIIND,FVINUM                                                    
         BZ    VALCAM1                                                          
         OC    SCFULL,SCFULL       TEST CAMPAIGN NUMBER ZERO                    
         BZ    VALCAM8                                                          
         OC    SCFULL(2),SCFULL                                                 
         BNZ   VALCAM8                                                          
         MVC   RDUB(2),SCFULL+2                                                 
         XC    RDUB(2),XFF                                                      
*                                                                               
         CLC   BCAM,RDUB           TEST CHANGE OF CAMPAIGN                      
         BE    VALCAMX                                                          
         MVI   CMPDSTDT,0          CHANGE OF CAMPAIGN, NOT SCROLLED             
         MVI   RCPDSTDT,0                                                       
         XC    SCRLINPT,SCRLINPT                                                
*                                                                               
         TM    TWAINDS,TWAICCS1    YES-TEST SWITCHING TO COMPANION              
         BZ    *+12                    CAMPAIGN NOW                             
         NI    TWAINDS,255-TWAICCS1    YES-TURN OFF FIRST TIME SWITCH           
         B     VALCAM2                                                          
         TM    TWAINDS,TWAICCSW    TEST OLD CAMPAIGN WAS A COMPANION            
         BZ    VALCAM2                                                          
         NI    TWAINDS,255-TWAICCSW     YES-ERASE TRACE OF CAMPAIGN             
         NI    TWALSCTL,255-TWALSHSL        SWITCHING                           
         CLI   SCSELLVL,1          TEST LIST NEST LEVEL GREATER THAN 1          
         BNH   VALCAM2                                                          
         ZIC   RE,SCSELLVL         YES-GO BACK TO PREVIOUS LEVEL                
         BCTR  RE,0                                                             
         STC   RE,SCSELLVL                                                      
         NI    TWAMODE2,255-TWAMLVL                                             
         OC    TWAMODE2,SCSELLVL                                                
         B     VALCAM2                                                          
*                                                                               
VALCAM1  ZIC   R1,FVXLEN           TEST LASTEST CAMPAIGN REQUESTED              
         EX    R1,CAMINP1                                                       
         BE    *+12                                                             
         EX    R1,CAMINP2                                                       
         BNE   VALCAM8                                                          
         XC    SCFULL,SCFULL                                                    
*                                                                               
VALCAM2  LA    R2,IOKEY            BUILD KEY OF CAMPAIGN RECORD                 
         USING CAMRECD,R2                                                       
         XC    CAMKEY,CAMKEY                                                    
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK   ALWAYS APPLY THE AGY/MED MASK                
         MVC   CAMKBYR,BBYR                                                     
         OC    CAMKCAM,SCFULL+2                                                 
         BZ    *+10                                                             
         XC    CAMKCAM,XFF                                                      
         GOTO1 AIO,DIRHI           READ HIGH FOR CAMPAIGN KEY                   
         BNE   VALCAM9                                                          
         CLC   CAMKEY(CAMKCAM-CAMKEY),IOKEYSAV                                  
         BNE   VALCAM9                                                          
         OC    SCFULL,SCFULL       TEST LATEST CAMPAIGN REQUESTED               
         BZ    *+14                                                             
         CLC   CAMKEY(CAMKREST-CAMKEY),IOKEYSAV                                 
         BNE   VALCAM9                                                          
**  NEED TO COMPARE TO SEE IF IT'S SAME CAMPAIGN                                
         CLC   CAMKCAM,BCAM        SAME CAMPAIGN?                               
         BE    VALCAMX              - YUP, GET OUTTA HERE                       
**  IF IT IS, NO NEED TO REEXTRACT DATA AND WIPE BCLT BPRD ETC.                 
         XCEFL RIO,2000            CLEAR IOAREA BEFORE READING INTO IT          
         MVI   CMPDSTDT,0          CHANGE OF CAMPAIGN, NOT SCROLLED             
         MVI   RCPDSTDT,0                                                       
         XC    SCRLINPT,SCRLINPT                                                
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILGET                                                       
         BE    *+14                                                             
         CLI   IOERR,2             RECORD DELETED?                              
         BE    VALCAM9             YES, INVALID CAMPAIGN                        
         DC    H'0'                                                             
         LA    R2,RIO                                                           
         BAS   RE,EXTCAM                                                        
         BE    VALCAM7                                                          
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALCAM7                                                          
         CLC   FVMSGNO,=AL2(FVSECLOK)                                           
         BE    VALCAMX                                                          
         B     VALCAM9                                                          
*                                                                               
VALCAM7  XC    BCAM(G1WPROF-BCAM),BCAM                                          
         MVC   BCAM,CAMKCAM                                                     
         XC    QCAM(QVALSX-QCAM),QCAM                                           
         MVC   RDUB(L'BCAM),BCAM                                                
         XC    RDUB(L'BCAM),XFF                                                 
         SR    R1,R1                                                            
         ICM   R1,3,RDUB                                                        
         CVD   R1,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  QCAM,RDUB                                                        
         B     VALCAMX                                                          
*                                                                               
VALCAM8  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALCAMX                                                          
*                                                                               
VALCAM9  MVC   FVMSGNO,=AL2(FVICAM)                                             
*                                                                               
VALCAMX  B     ROUTSX                                                           
         DROP  R2                                                               
         SPACE 1                                                                
CAMINP1  CLC   FVIFLD(0),=C'LAST  '     ** EXECUTED **                          
CAMINP2  CLC   FVIFLD(0),=C'LATEST'                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET A CAMPAIGN RECORD                                    *         
*                                                                     *         
* NTRY - R1=A(BINARY CAMPAIGN NUMBER)                                 *         
*                                                                     *         
* EXIT - CC=EQUAL IF CAMPAIGN IS VALID WITH CAMPAIGN VALUES EXTRACTED *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
LGETCAM  NTR1  ,                   LOCAL ENTRY POINT                            
         MVI   RFLAG,1                                                          
         B     GETCAM1                                                          
*                                                                               
GETCAM   MVI   RFLAG,0                                                          
*                                                                               
GETCAM1  MVC   RWORK4(L'BCAM),0(R1)                                             
         CLC   BCAM,RWORK4         TEST CHANGE OF CAMPAIGN                      
         BE    GETCAMX                                                          
         MVI   CMPDSTDT,0          CHANGE OF CAMPAIGN, NOT SCROLLED             
*                                                                               
         LA    R2,IOKEY            BUILD KEY OF CAMPAIGN RECORD                 
         USING CAMRECD,R2                                                       
         XC    CAMKEY,CAMKEY                                                    
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK   ALWAYS APPLY THE AGY/MED MASK                
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,RWORK4                                                   
         GOTO1 AIO,DIRHI                                                        
         BNE   GETCAM9                                                          
         CLC   CAMKEY(CAMKREST-CAMKEY),IOKEYSAV                                 
         BNE   GETCAM9                                                          
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILGET                                                       
         BNE   GETCAM9                                                          
         LA    R2,RIO                                                           
         BAS   RE,EXTCAM                                                        
         BNE   GETCAM9                                                          
*                                                                               
GETCAM2  XC    BCAM(G1WPROF-BCAM),BCAM                                          
         MVC   BCAM,RWORK4                                                      
         XC    QCAM(QVALSX-QCAM),QCAM                                           
         SR    R1,R1                                                            
         XC    RWORK4(L'BCAM),XFF                                               
         ICM   R1,3,RWORK4                                                      
         CVD   R1,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  QCAM,RDUB                                                        
         B     GETCAMX                                                          
*                                                                               
GETCAM9  MVC   FVMSGNO,=AL2(FVICAM)                                             
*                                                                               
GETCAMX  CLI   RFLAG,1                                                          
         BE    EXIT                                                             
         B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT CAMPAIGN DETAILS FROM CAMPAIGN RECORD                      
* NTRY - R2=A(CAMPAIGN RECORD)                                                  
* EXIT - CC EQ - CAMPAIGN IS OK                                                 
*           NE - INVALID CAMPAIGN FOR ONE REASON OR ANOTHER                     
*                                                                               
* *** WARNING: ACWORK GETS CLOBBERED                                            
***********************************************************************         
         SPACE 1                                                                
         USING CAMRECD,R2                                                       
EXTCAM   NTR1                                                                   
         MVC   CMPNM,CAMNAME       EXTRACT CAMPAIGN VALUES                      
         MVC   CMPCLTC,CAMCLT                                                   
         MVC   CMPPRDN,CAMPRD                                                   
         MVC   CMPPGRP,CAMPGRP                                                  
         MVC   CMPPGRPN,CAMPGRPN                                                
         MVC   CMPESTN,CAMEST                                                   
         MVC   CMPSTDT,CAMSTRT                                                  
         MVC   CMPND,CAMEND                                                     
         MVC   CMPPRDC,CAMPRDC                                                  
         MVC   CMPUF,CAMUPFIL                                                   
         MVC   CMPUP,CAMUPGRD                                                   
         MVC   CMPUPIN,CAMINPUT                                                 
         MVI   QBOOKTYP,0                                                       
*                                                                               
         CLC   CMPUPIN(L'RSPACES),RSPACES    RSPACES IS SHORTER THAN            
         BNH   EXTCAM1                                                          
         XC    ACWORK,ACWORK                                                    
         MVC   ACWORK(8),FVIHDR                                                 
         NI    ACWORK+1,X'FF'-FVAXTND    NOT AN EXTENDED FIELD                  
         MVC   ACWORK+8(L'CAMINPUT),CAMINPUT                                    
         LA    R0,L'CAMINPUT                                                    
         LA    R1,ACWORK+8+L'CAMINPUT-1                                         
EXTCAM0A CLI   0(R1),C' '                                                       
         BH    EXTCAM0B                                                         
         BCTR  R1,0                                                             
         BCT   R0,EXTCAM0A                                                      
*                                                                               
EXTCAM0B STC   R0,ACWORK+5                                                      
         STC   R0,ACWORK+7                                                      
         LR    RE,R0                                                            
         LA    RE,8(RE)                                                         
         STC   RE,ACWORK                                                        
*                                                                               
         LA    R0,RIO                                                           
         LA    R1,2000                                                          
         LA    RE,RIO2                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   APFLAG,X'F8'                                                     
         GOTO1 AVALUPG,ACWORK                                                   
*                                                                               
         LA    R0,RIO                                                           
         LA    R1,2000                                                          
         LA    RE,RIO2                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
EXTCAM1  MVC   CMPBKTYP,QBOOKTYP   SAVE BOOKTYPE OF THE CAMPAIGN                
         MVC   CMPFB,CAMFRBK                                                    
         TM    CAMFRBK+1,BTY2CHAR                                               
         BNO   EXTCAM1C                                                         
         CLI   CAMFRBKT,0                                                       
         BNE   *+10                                                             
         DC    H'0'                                                             
         CLI   CAMELLN,CAMELLNQ    EXPANDING LENGTH?                            
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   CMPFBTP,CAMFRBKT    CAMFRBK BOOKTYPE                             
EXTCAM1C MVC   CMPFBLST,CAMFRBKL                                                
         MVC   CMPSLN,CAMSLN                                                    
         MVC   CMPOPTS,CAMOPT                                                   
         MVC   CMPOPTS2,CAMOPT2                                                 
         NI    CLTIND2,255-CLTISDLY                                             
         TM    CMPOPTS2,CAMOSDLY   TEST SEPARATE DAILY BUY TRANSFER             
         BZ    *+8                                                              
         OI    CLTIND2,CLTISDLY    YES-TURN ON CLIENT INDICATOR ALSO            
         MVC   CMPDPOPT,CAMDPOPT                                                
         MVC   CMPRSVC,CAMRSVC                                                  
         MVC   CMPADJ,CAMADJ                                                    
         MVC   CMPCCAM,CAMCCAM                                                  
         MVI   CMPIND,0                                                         
         TM    CAMINDS,CAMISKD                                                  
         BZ    *+8                                                              
         OI    CMPIND,CMPISKD                                                   
         TM    CAMINDS,CAMIFR1                                                  
         BZ    *+8                                                              
         OI    CMPIND,CMPIFR1                                                   
         TM    CAMINDS,CAMIGOAL                                                 
         BZ    *+8                                                              
         OI    CMPIND,CMPIGOAL                                                  
*                                                                               
         MVC   CMPBOOKS,CAMBOOKS                                                
         CLI   CAMELLN,CAMELLNQ    EXTENDED LENGTH?                             
         BNH   EXTCAM1E                                                         
         MVC   CMPBKTPS,CAMBKTPS   CAMBOOKS BOOKTYPES (BINARY)                  
EXTCAM1E MVC   CMPPRD1,CAMPPRD1                                                 
         MVC   CMPPRD2,CAMPPRD2                                                 
         MVC   CMPLEN1,CAMPLEN1                                                 
         MVC   CMPLEN2,CAMPLEN2                                                 
         MVC   CMPUPUT,CAMUPUT                                                  
         MVC   CMPUSHR,CAMUSHR                                                  
         MVC   CMPGOALS,CAMGOALS                                                
         MVC   CMPPERS,CAMPERS                                                  
         MVC   CMPSCHEM,CAMSCHEM                                                
         OC    CMPSCHEM,CMPSCHEM                                                
         BZ    EXTCAM2                                                          
         GOTO1 VCLPACK,RPARM,CMPSCHEM,CMPPSCHM                                  
*                                                                               
EXTCAM2  LA    R3,IOKEY            READ ESTIMATE RECORD                         
         USING ESTHDRD,R3                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,CMPCLTC                                                  
         MVC   EKEYPRD,CMPPRDC                                                  
         MVC   EKEYEST,CMPESTN                                                  
         LA    R3,RIO2                                                          
         ST    R3,IOADDR                                                        
         GOTO1 AIO,FILRD                                                        
         BNE   EXTCAMN                                                          
         MVC   ESTOWSDY,EOWSDAY    OUT-OF-WEEK START DAY                        
         CLI   ESTOWSDY,1          TEST MONDAY                                  
         BNE   *+8                                                              
         MVI   ESTOWSDY,0          YES-IGNORE                                   
*                                                                               
         TM    CAMOPT,CAMODLY+CAMOWKS   TEST DAILY SKED AND FLIGHT WKS          
         BNO   *+10                                                             
         MVC   CMPWKDLY,CAMWKS     YES-SAVE FLIGHT WEEKS FOR DAILY SKED         
*                                                                               
         LR    RE,R5               XC    CMPDATSD,CMPDATSD                      
         AHI   RE,CMPDATSD-TWAD    XC    CMPDATSP,CMPDATSP                      
         LHI   RF,L'CMPDATSD+L'CMPDATSP                                         
         LR    R0,RE                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VDATCON,RPARM,(3,CMPSTDT),RWORK                                  
*                                                                               
         TM    CAMOPT,CAMOWKS      TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BO    EXTCAM6             YES                                          
         TM    CAMOPT,CAMODLY      NO-TEST DAILY SCHEDULE                       
         BZ    EXTCAM16            NO                                           
         LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         MVC   0(6,RE),RWORK       YES-BUILD LIST OF DAYS                       
         BAS   RE,GETMON                                                        
         MVC   CMPSTMON,RWORK+6    CAMPAIGN START MONDAY                        
         GOTO1 VDATCON,RPARM,CMPSTMON,(2,CMPSTMNP)                              
         GOTO1 (RF),(R1),(3,CMPND),RWORK+12                                     
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP-TWAD                                                 
*                                                                               
EXTCAM4  CLC   RWORK(6),RWORK+12                                                
         BH    EXTCAM14                                                         
         GOTO1 VDATCON,RPARM,RWORK,(2,(R4))                                     
         MVC   2(2,R4),0(R4)                                                    
         MVC   CMPNDMNP,0(R4)      END DAY                                      
         GOTO1 VADDAY,(R1),RWORK,RWORK+6,1                                      
         MVC   RWORK(6),RWORK+6                                                 
         LA    R4,4(R4)                                                         
         B     EXTCAM4                                                          
*                                                                               
EXTCAM6  TM    CAMOPT,CAMODLY      NON-CONTIGUOUS WEEKS - TEST DAILY            
         BO    EXTCAM7                                                          
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         MVC   0(L'CAMWKS,RE),CAMWKS  NO                                        
         B     EXTCAM14                                                         
*                                                                               
EXTCAM7  LA    R3,CAMWKS           YES-BUILD LIST OF DAYS                       
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP-TWAD                                                 
         LA    R5,2                                                             
*                                                                               
EXTCAM8  OC    0(4,R3),0(R3)                                                    
         BZ    EXTCAM14                                                         
         MVC   0(2,R4),0(R3)                                                    
         MVC   2(2,R4),0(R3)                                                    
         LA    R0,6                                                             
*                                                                               
EXTCAM10 GOTO1 VDATCON,RPARM,(2,(R4)),(0,RWORK)                                 
         GOTO1 VADDAY,(R1),RWORK,RWORK+6,1                                      
         GOTO1 VDATCON,(R1),(0,RWORK+6),(2,RHALF)                               
         CLC   RHALF,2(R3)                                                      
         BH    EXTCAM12                                                         
         LA    R4,4(R4)                                                         
         MVC   0(2,R4),RHALF                                                    
         MVC   2(2,R4),RHALF                                                    
         BCT   R0,EXTCAM10                                                      
*                                                                               
EXTCAM12 LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,EXTCAM8                                                       
*                                                                               
EXTCAM14 L     R5,ATWA             RE-ESTABLISH R5                              
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP-TWAD                                                 
         SR    RE,RE                                                            
         OC    0(4,R4),0(R4)                                                    
         BZ    *+16                                                             
         LA    R4,4(R4)                                                         
         LA    RE,1(RE)                                                         
         B     *-18                                                             
         MVI   0(R4),X'FF'                                                      
         STC   RE,CMPNWKS                                                       
         TM    CAMOPT,CAMOWKS      TEST NON-CONTIGUOUS WEEKS                    
         BZ    EXTCAM18            NO-THEN IT'S REGULAR DAILY                   
         BCTR  R4,0                YES-SET VARIOUS OTHER DATES                  
         BCTR  R4,0                                                             
         GOTO1 VDATCON,RPARM,(2,(R4)),CMPFLND                                   
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         ST    RE,0(R1)                                                         
         MVI   0(R1),2                                                          
         GOTO1 (RF),(R1),,(0,RWORK)                                             
         LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         MVC   0(6,RE),RWORK                                                    
         BAS   RE,GETMON                                                        
         MVC   CMPFLSTM,RWORK+6                                                 
         GOTO1 (RF),(R1),CMPFLSTM,(2,CMPSTMNP)                                  
         MVC   CMPNDMNP,CMPSTMNP                                                
         GOTO1 (RF),(R1),(3,CMPSTDT),(0,RWORK)                                  
         BAS   RE,GETMON                                                        
         MVC   CMPSTMON,RWORK+6                                                 
         B     EXTCAM18                                                         
*                                                                               
EXTCAM16 GOTO1 VDATCON,RPARM,(3,CMPSTDT),(0,RWORK)   BUILD LISTS OF             
         GOTO1 (RF),(R1),(3,CMPND),(0,RWORK+6)       DATES                      
         XC    RWORK2,RWORK2                                                    
         MVC   RWORK2+8(1),ESTOWSDY                                             
         DROP  R3                                                               
         MVC   RPARM+16(4),VGTBROAD                                             
         MVC   RPARM+20(4),VADDAY                                               
         MVC   RPARM+24(4),VGETDAY                                              
         MVC   RPARM+28(4),VDATCON                                              
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         ST    RE,4(R1)                                                         
         MVI   4(R1),5                                                          
*                                                                               
         GOTO1 VMOBILE,(R1),(53,RWORK),,RPARM+16,RWORK2                         
         MVC   CMPNWKS,RPARM       NUMBER OF WEEKS                              
*                                                                               
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         ST    RE,RPARM                                                         
         MVI   RPARM,2                                                          
*                                                                               
         GOTO1 VDATCON,RPARM,,(0,CMPSTMON)  START MONDAY                        
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         MVC   CMPSTMNP,0(RE)      START MONDAY PACKED                          
         MVC   CMPNDMNP,0(RE)      END MONDAY PACKED                            
         LR    RE,R5                                                            
         AHI   RE,CMPDATSD-TWAD                                                 
         MVC   0(6,RE),RWORK       START DATE                                   
*                                                                               
EXTCAM18 LR    R3,R5                                                            
         AHI   R3,CMPDATSD+6-TWAD                                               
         LR    R4,R5                                                            
         AHI   R4,CMPDATSP+4-TWAD                                               
*                                                                               
EXTCAM20 CLI   0(R4),X'FF'                                                      
         BE    EXTCAM22                                                         
         GOTO1 VDATCON,RPARM,(2,(R4)),(0,(R3))                                  
         LA    R3,6(R3)                                                         
         MVC   CMPNDMNP,0(R4)      END MONDAY                                   
         LA    R4,4(R4)                                                         
         B     EXTCAM20                                                         
*                                                                               
EXTCAM22 MVI   0(R3),X'FF'         MARK EOL                                     
*                                                                               
         XC    IOKEY,IOKEY         READ SID PROFILE                             
         MVC   IOKEY(4),=C'S0SI'                                                
         MVC   IOKEY+4(2),CUAALF                                                
         MVC   IOKEY+6(1),QMED                                                  
         CLC   CMPSCHEM,=C'ALL'                                                 
         BE    *+10                                                             
         MVC   IOKEY+7(3),CMPSCHEM                                              
         GOTO1 VGETPROF,RPARM,IOKEY,RWORK,VDMGR                                 
         OC    RWORK(16),RWORK     TEST PROFILE FOUND                           
         BZ    EXTCAM24                                                         
         CLI   RWORK,C'Y'          YES - TEST COMPETITION USER                  
         BNE   EXTCAM24                                                         
         OI    CMPIND,CMPICMPU           YES                                    
*                                                                               
EXTCAM24 CLI   CMPDPOPT,0          TEST CAMPAIGN SUBDPT OPTION NOT SET          
         BE    *+12                                                             
         TM    CMPOPTS,CAMOAALL+CAMOAIMP+CAMOATGT+CAMOANO  OR AUTOADJ           
         BNZ   EXTCAM26                                    NOT SET              
         GOTO1 LGETCLT,CMPCLTC     YES-GET CLIENT DETAILS                       
         BNE   EXTCAMN                                                          
*                                                                               
         CLI   CMPDPOPT,0                                                       
         BNE   *+10                                                             
         MVC   CMPDPOPT,CLTBWPRO+4                                              
         TM    CMPOPTS,CAMOAIMP+CAMOAALL+CAMOATGT+CAMOANO                       
         BNZ   EXTCAM26                                                         
         CLI   CLTBWPRO+3,C'I'                                                  
         BNE   *+8                                                              
         OI    CMPOPTS,CAMOAIMP                                                 
         CLI   CLTBWPRO+3,C'A'                                                  
         BNE   *+8                                                              
         OI    CMPOPTS,CAMOAALL                                                 
         CLI   CLTBWPRO+3,C'T'                                                  
         BNE   EXTCAM26                                                         
         OI    CMPOPTS,CAMOATGT                                                 
*                                                                               
EXTCAM26 XC    CMPSLEQU,CMPSLEQU   SPOT LENGTH EQUIVALENCES                     
         LA    R3,CAMEL                                                         
         SR    R0,R0                                                            
         XC    APWORK,APWORK       FIND RECORD ELEMENTS                         
*                                                                               
EXTCAM28 CLI   0(R3),0                                                          
         BE    EXTCAM30                                                         
         LA    R1,APWORK                                                        
         CLI   0(R3),CEQELCDQ                                                   
         BE    EXTCAM29                                                         
         LA    R1,APWORK+4                                                      
         CLI   0(R3),CUPELCDQ                                                   
         BE    EXTCAM29                                                         
         LA    R1,APWORK+8                                                      
         CLI   0(R3),CESELCDQ                                                   
         BNE   EXTCAM29+4                                                       
*                                                                               
EXTCAM29 ST    R3,0(R1)                                                         
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     EXTCAM28                                                         
*                                                                               
EXTCAM30 ICM   R3,15,APWORK                                                     
         BZ    EXTCAM32                                                         
         USING CEQEL,R3                                                         
         ZIC   RF,CEQELLN                                                       
         AHI   RF,-1*(CEQSLN-CEQEL-1)                                           
         BM    EXTCAM32                                                         
         LA    RE,L'CMPSLEQU-1                                                  
         CR    RF,RE                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         MVC   CMPSLEQU(0),CEQSLN                                               
         EX    RF,*-6                                                           
*                                                                               
EXTCAM32 XC    CMPU2DAT,CMPU2DAT   SECOND UPGRADE                               
         XC    CMPUF2,CMPUF2                                                    
         XC    CMPUP2,CMPUP2                                                    
         XC    CMPUPIN2,CMPUPIN2                                                
         XC    CMPFB2,CMPFB2                                                    
         MVI   CMPFB2TP,0          CMPFB2 BOOKTYPE                              
         XC    CMPFB2L,CMPFB2L                                                  
         XC    CMPUPUT2,CMPUPUT2                                                
         XC    CMPUSHR2,CMPUSHR2                                                
         ICM   R3,15,APWORK+4                                                   
         BZ    EXTCAM34                                                         
         USING CUPEL,R3                                                         
         MVC   CMPU2DAT,CUPDATE                                                 
         MVC   CMPUF2,CUPFILE                                                   
         MVC   CMPUP2,CUPGRADE                                                  
         MVC   CMPUPIN2,CUPINPUT                                                
         MVC   CMPFB2,CUPFRBK                                                   
         TM    CUPFRBK+1,BTY2CHAR                                               
         BNO   EXTCAM33                                                         
         CLI   CUPELLN,CUPELLNQ    EXTENDED LENGTH?                             
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   CMPFB2TP,CUPFRBKT                                                
EXTCAM33 CLI   CUPELLN,CUPFRBKL-CUPEL                                           
         BNH   *+10                                                             
         MVC   CMPFB2L,CUPFRBKL                                                 
         MVC   CMPUPUT2,CUPPUT                                                  
         MVC   CMPUSHR2,CUPSHR                                                  
*                                                                               
EXTCAM34 XC    CMPELIST,CMPELIST                                                
         ICM   R3,15,APWORK+8                                                   
         BZ    EXTCAMY                                                          
         USING CESEL,R3                                                         
         ZIC   RE,CESELLN                                                       
         AHI   RE,-1*(CESTS-CESEL)                                              
         LA    RF,L'CMPELIST                                                    
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   CMPELIST(0),CESTS                                                
         EX    RE,*-6                                                           
*                                                                               
EXTCAMY  CR    RB,RB                                                            
         B     EXTCAMX                                                          
*                                                                               
EXTCAMN  LTR   RB,RB                                                            
*                                                                               
EXTCAMX  DS    0H                                                               
         XC    ACWORK,ACWORK       CLEAR OUT ACWORK                             
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE MEDIA CODE                                      *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF MEDIA FIELD)                            *         
*                                                                     *         
* EXIT - CC=EQUAL IF MEDIA IS VALID WITH MEDIA VALUES EXTRACTED       *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALMED   MVI   FVMINL,1            SET REQUIRED FIELD                           
         MVI   FVMAXL,L'QMED                                                    
         GOTO1 AFVAL                                                            
         BNE   VALMEDX                                                          
         CLI   FVIFLD,C'T'         MUST BE MEDIA T                              
         BNE   VALMED0E             - NOPE                                      
         TM    APROFBTS,A002DOK    ARE WE OK FOR 2 DECIMALS?                    
         BZ    VALMED1                                                          
         OI    APROFBTS,A00TWODC    - YUP WE ARE                                
         B     VALMED1                                                          
*                                                                               
VALMED0E DS    0H                                                               
****  2 DECIMAL ONLY FOR MEDIA T                                                
         NI    APROFBTS,X'FF'-A00TWODC   TURN OFF 2 DECIMALS                    
****  2 DECIMAL ONLY FOR MEDIA T                                                
         CLI   FVIFLD,C'X'         MEDIA X?                                     
         BNE   VALMED0X                                                         
         CLC   CUAALF,=C'SJ'       SO FAR ONLY CARAT CAN USE MEDIA X            
         BE    VALMED1                                                          
         CLC   CUAALF,=C'UB'                                                    
         BE    VALMED1                                                          
         B     VALMED9                                                          
*                                                                               
VALMED0X CLI   FVIFLD,C'R'         RADIO IS ALLOWED FOR FOLLOWING               
         BNE   VALMED9                 AGENCY POWER CODES                       
         CLI   CUDMED,C'C'                                                      
         BE    *+8                                                              
         MVI   CUDMED,C'R'         BECAUSE DBSELMED USES CUDMED                 
*                                                                               
         LA    RE,RADIOAGY                                                      
VALMED0  OC    0(2,RE),0(RE)       ANY RADIO AGENCIES LEFT?                     
         BZ    VALMED9             NONE, NOT A VALID RADIO AGENCY               
         CLC   CUAALF,0(RE)                                                     
         BE    VALMED1                                                          
         LA    RE,2(RE)                                                         
         B     VALMED0                                                          
*                                                                               
VALMED1  CLC   QMED,FVIFLD         TEST CHANGE OF MEDIA                         
         BE    VALMEDX                                                          
         LA    R2,IOKEY            BUILD KEY OF AGENCY RECORD                   
         USING AGYHDRD,R2                                                       
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,6                                                       
         MVC   AGYKAGY,CUAALF                                                   
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,IOSPTFIL+IORD   GET AGENCY RECORD                            
         BNE   VALMED9                                                          
         LA    R2,RIO+(AGYEL-AGYHDR)                                            
         SR    R0,R0                                                            
*                                                                               
VALMED2  CLI   0(R2),0             TEST END-OF-RECORD                           
         BE    VALMED9                                                          
         CLI   0(R2),2                                                          
         BNE   VALMED4                                                          
         CLC   2(1,R2),FVIFLD      MATCH MEDIA CODE TO ELEMENT                  
         BNE   VALMED4                                                          
         XC    BAGYMD(G1WPROF-BAGYMD),BAGYMD                                    
         MVC   BAGYMD,3(R2)        EXTRACT MEDIA VALUES                         
         XC    QMED(QVALSX-QMED),QMED                                           
         MVC   QMED,FVIFLD         SET MEDIA                                    
         MVC   MEDNM,4(R2)                                                      
         B     VALMEDX                                                          
*                                                                               
VALMED4  IC    R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     VALMED2                                                          
*                                                                               
VALMED9  MVC   FVMSGNO,=AL2(FVIMED)    SET ERROR NUMBER                         
*                                                                               
VALMEDX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET MEDIA RECORD                                         *         
*                                                                     *         
* NTRY - R1=A(BINARY AGENCY/MEDIA VALUE)                              *         
*                                                                     *         
* EXIT - CC=EQUAL IF MEDIA IS VALID WITH MEDIA VALUES EXTRACTED       *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
GETMED   MVC   RDUB(L'BAGYMD),0(R1)                                             
*                                                                               
         MVC   RDUB+1(L'BAGYMD),BAGYMD                                          
         OC    RDUB+1(1),BBYRMASK                                               
         CLC   RDUB(1),RDUB+1      TEST CHANGE OF MEDIA                         
         BE    GETMEDX                                                          
*                                                                               
         MVI   BBYRMASK,0                                                       
         TM    RDUB,X'08'          AGY/MED MASK ON FOR OVER 255 BUYERS?         
         BZ    GETMED1                                                          
         OI    BBYRMASK,X'08'                                                   
         NI    RDUB,X'FF'-X'08'    THIS IS THE REAL AGY/MED                     
*                                                                               
GETMED1  LA    R2,IOKEY            BUILD KEY OF AGENCY RECORD                   
         USING AGYHDRD,R2                                                       
         XC    AGYKEY,AGYKEY                                                    
         MVI   AGYKTYPE,6                                                       
         MVC   AGYKAGY,CUAALF                                                   
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD           GET AGENCY RECORD                            
         BNE   GETMED9                                                          
         LA    R2,RIO+(AGYEL-AGYHDR)                                            
         SR    R0,R0                                                            
*                                                                               
GETMED2  CLI   0(R2),0             TEST END-OF-RECORD                           
         BE    GETMED9                                                          
         CLI   0(R2),2                                                          
         BNE   GETMED4                                                          
         CLC   3(1,R2),RDUB        MATCH AGENCY/MEDIA CODE TO ELEMENT           
         BNE   GETMED4                                                          
         XC    BAGYMD(G1WPROF-BAGYMD),BAGYMD                                    
         MVC   BAGYMD,3(R2)        EXTRACT MEDIA VALUES                         
         XC    QMED(QVALSX-QMED),QMED                                           
         MVC   QMED,2(R2)          SET MEDIA                                    
         MVC   MEDNM,4(R2)                                                      
         B     GETMEDX                                                          
*                                                                               
GETMED4  IC    R0,1(R2)            BUMP TO NEXT ELEMENT                         
         AR    R2,R0                                                            
         B     GETMED2                                                          
*                                                                               
GETMED9  MVC   FVMSGNO,=AL2(FVIMED)     SET ERROR NUMBER & SYSTEM               
*                                                                               
GETMEDX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE CLIENT CODE                                     *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF CLIENT FIELD)                           *         
*                                                                     *         
* EXIT - CC=EQUAL IF CLIENT IS VALID WITH CLIENT VALUES EXTRACTED     *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALCLT   BRAS  RE,VALICLT                                                       
         B     ROUTSX                                                           
***********************************************************************         
* ROUTINE TO GET A CLIENT RECORD                                      *         
*                                                                     *         
* NTRY - R1=A(BINARY CLIENT VALUE)                                    *         
*                                                                     *         
* EXIT - CC=EQUAL IF CLIENT IS VALID WITH CLIENT VALUES EXTRACTED     *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
LGETCLT  NTR1                      LOCAL ENTRY POINT                            
         MVI   RFLAG,1                                                          
         LA    R0,RIO2                                                          
         L     R5,ATWA                                                          
         B     GETCLT1                                                          
*                                                                               
GETCLT   MVI   RFLAG,0             ENTRY FROM EXTERNAL MODULE                   
         LA    R0,RIO                                                           
*                                                                               
GETCLT1  MVC   RDUB(L'BCLT),0(R1)                                               
         CLC   BCLT,RDUB           TEST CHANGE OF CLIENT                        
         BE    GETCLTX                                                          
         LA    R2,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,RDUB                                                     
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   GETCLT8                                                          
         L     R2,IOADDR                                                        
         MVC   CLTNM,CNAME                                                      
         MVI   CLTSRC,C'N'                                                      
         MVC   CLTOFF,COFFICE                                                   
         MVI   CLTIND,0                                                         
         NI    CLTIND2,CLTISDLY                                                 
         MVI   CLTIND3,0                                                        
         MVI   CLTMGRID,0                                                       
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
         MVC   SVCOST2,CCOST2                                                   
         MVC   SVCLOCK,CLOCKYM                                                  
         MVC   SVRFPGRP,CRFPGRP                                                 
         DROP  R1                                                               
*                                                                               
         CLI   CPOLONLY,C'Y'       POL ONLY?                                    
         BNE   *+8                                                              
         OI    CLTIND2,CLTPONLY    YES                                          
*                                                                               
         CLI   CPROF+3,C'0'                                                     
         BE    *+8                                                              
         MVI   CLTSRC,C'A'                                                      
         MVC   CLTSRCDF,CLTSRC     SET CLIENT'S DEFAULT SERVICE                 
         CLI   CMPRSVC,0           TEST OVERRIDE RATING SERVICE                 
         BE    *+10                                                             
         MVC   CLTSRC,CMPRSVC                                                   
         CLI   CEXTRA+2,C'*'       TEST CCUSA INTERFACE                         
         BNE   *+8                 NO                                           
         OI    CLTIND,CLTICC       YES                                          
         CLI   CEXTRA+5,C'N'       TEST NO US SPILL                             
         BNE   *+8                 NO                                           
         OI    CLTIND,CLTINOSP     YES                                          
         CLI   CEXTRA+2,C'N'       TEST ID REQUIRED FOR BUY TRANSFER            
         BE    GETCLT2             NO                                           
         OI    CLTIND,CLTIBID      YES                                          
         CLI   CEXTRA+2,C'Y'       TEST ID=MKTGRP                               
         BE    GETCLT2                                                          
         CLI   CEXTRA+2,C'A'                                                    
         BL    GETCLT2                                                          
******** CLI   CEXTRA+2,C'K'                                                    
         CLI   CEXTRA+2,C'Z'                                                    
         BH    GETCLT2                                                          
         OI    CLTIND,CLTIBIMG     YES-                                         
         MVC   CLTMGRID,CEXTRA+2   SAVE MARKET GROUP SCHEME                     
*                                                                               
GETCLT2  CLI   CEXTRA+8,C'Y'       TEST GOALS REQUIRED BEFORE BUY               
         BNE   *+8                                                              
         OI    CLTIND,CLTIGOL                                                   
*                                                                               
         TM    COPT2,COP2BP        USE BUY PROGRAMMING PROFILE?                 
         BZ    *+8                                                              
         OI    CLTIND3,CLTIBPPF    YES                                          
*******                                                                         
         TM    COPT2,COP2DIY       USE DIY TRADE?                               
         BZ    *+8                                                              
         OI    CLTIND3,CLTIDIYT    YES                                          
*******                                                                         
         TM    COPT2,COP2FRZ       FROZEN CLIENT?                               
         BZ    *+8                                                              
         OI    CLTIND2,CLTFROZN    YES                                          
*                                                                               
         MVC   CLTIFC,CCLTIFC      CLIENT INTERFACE CODE                        
         MVC   CLTPROF,CPROF       CLIENT PROFILE                               
         MVC   CLTMTCLT,CMCLTCOD   MASTER TRAFFIC CLIENT CODES                  
         MVC   CLTMTNUM,CMCLTUNQ                                                
         MVC   CLTMTPRD,CMCLTPRD                                                
*                                                                               
*****    CLI   APROF7,C'C'         TEST CANADA                                  
*****    BNE   GETCLT2A                                                         
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    GETCLT2A                                                         
*                                                                               
         MVI   CUDMED,C'C'         YES-MEDIA C FOR DEMO LOOKUPS                 
         CLI   CEXTRA,C'U'         TEST LOOK UP US DEMOS                        
         BNE   GETCLT2A                                                         
         MVI   CUDMED,C'T'         YES-SET MEDIA FOR US DEMO LOOKUPS            
         OI    CLTIND2,CLTIUS                                                   
*                                                                               
GETCLT2A XC    BCLT(G1WPROF-BCLT),BCLT                                          
         MVC   BCLT,RDUB                                                        
         XC    QCLT(QVALSX-QCLT),QCLT                                           
         GOTO1 VCLUNPK,RPARM,(CPROF+6,BCLT),QCLT                                
*                                                                               
         TM    CUSTAT,CUSDDS       IGNORE SECURITY FOR DDS TERMINAL             
         BO    GETCLT5                                                          
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
         MVC   SVCACCS,CACCESS                                                  
         MVI   SVMACCS,X'FF'                                                    
         DROP  R1                                                               
         BRAS  RE,CALLOFCR                                                      
         BE    GETCLT5                                                          
         XC    BCAM(G1WPROF-BCAM),BCAM                                          
         XC    QCAM(QVALSX-QCAM),QCAM                                           
         B     GETCLT9                                                          
*                                                                               
GETCLT5  MVC   RDUB(2),CUAALF      GET THE EQUIVALENCE RECORD                   
         MVC   RDUB+2(1),QMED                                                   
         MVC   RDUB+3(2),BCLT                                                   
         L     R2,IOADDR                                                        
         LA    R3,L'EQUDPT(R2)                                                  
         L     R4,VDMGR                                                         
         GOTO1 VEQVRD,RPARM,RDUB,(R2),(R3),(R4)                                 
         CLI   RPARM+12,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLTEQUIV,0(R3)                                                   
*                                                                               
         XC    RFULL,RFULL                                                      
         MVC   RFULL(2),=C'BW'     BW PROFILE                                   
         GOTO1 PROFILE,RPARM,RFULL,CLTBWPRO                                     
         MVC   RFULL(2),=C'AJ'     AJ PROFILE                                   
         GOTO1 (RF),(R1),RFULL,CLTAJPRO                                         
         MVC   RFULL(2),=C'AD'     AD PROFILE                                   
         GOTO1 (RF),(R1),RFULL,RWORK2                                           
         CLI   RWORK2,C'Y'         TEST ADDS USER                               
         BNE   *+8                                                              
         OI    CLTIND,CLTIADDS     YES                                          
*******   THIS NEEDS TO BE TAKEN OUT WHEN IT GOES LIVE!!                        
***      NI    APROFBTS,X'FF'-A00TWODC                                          
**       MVC   RFULL(2),=C'00'     AD PROFILE                                   
**       GOTO1 (RF),(R1),RFULL,RWORK3                                           
**       CLI   RWORK3+9,C'Y'                                                    
**       BNE   *+8                                                              
**       OI    APROFBTS,A00TWODC                                                
*******   THIS NEEDS TO BE TAKEN OUT WHEN IT GOES LIVE!!                        
*****                                                                           
         XC    GBPPROF,GBPPROF                                                  
         MVC   RFULL(2),=C'BP'     BP PROFILE                                   
         GOTO1 (RF),(R1),RFULL,GBPPROF                                          
*****                                                                           
         MVC   RFULL(2),=C'1W'     1W PROFILE                                   
         GOTO1 (RF),(R1),RFULL,RWORK2                                           
         MVC   G1WPROF,RWORK2                                                   
         CLI   RWORK2+3,C'Y'       TEST CANADIAN ANGLO/FRANCO OPTION ON         
         BNE   *+8                                                              
         OI    CLTIND2,CLTIANFR    YES                                          
         MVC   RFULL(3),=C'BWA'    BWA PROFILE                                  
         GOTO1 (RF),(R1),RFULL,RWORK2                                           
         CLI   RWORK2,C'Y'         TEST TRANSFER DATES REQUIRED                 
         BNE   *+8                                                              
         OI    CLTIND,CLTITRDT     YES                                          
         CLI   RWORK2+1,C'Y'       TEST TRANSFER BUYER NAME                     
         BNE   *+8                                                              
         OI    CLTIND,CLTITRBN     YES                                          
         CLI   RWORK2+2,C'Y'       TEST ROUND EQUIVALENCED COSTS                
         BNE   *+8                                                              
         OI    CLTIND2,CLTIREQC    YES                                          
         CLI   RWORK2+3,C'Y'       TEST SEPARATE LINES FOR DAILY SCHED          
         BNE   *+8                                                              
         OI    CLTIND2,CLTISDLY    YES                                          
         CLI   RWORK2+4,C'Y'       TEST CAMPAIGN DATES MUST MATCH EST           
         BNE   *+8                                                              
         OI    CLTIND2,CLTIEST     YES                                          
         CLI   RWORK2+5,C'Y'       TEST X-OUT SCHEDULED WEEKS FOR COPY          
         BNE   *+8                                                              
         OI    CLTIND2,CLTIXOUT    YES                                          
         CLI   RWORK2+6,C'Y'       MASTER/SUBDAYPART COMBINED?                  
         BNE   *+8                                                              
         OI    CLTIND3,CLTIMSDP    YES                                          
         B     GETCLTX                                                          
*                                                                               
GETCLT8  MVC   FVMSGNO,=AL2(FVICLI)                                             
         B     GETCLTX                                                          
*                                                                               
GETCLT9  MVC   FVMSGNO,=AL2(FVSECLOK)                                           
*                                                                               
GETCLTX  CLI   RFLAG,0                                                          
         BE    ROUTSX                                                           
         CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR INTERNAL CALL         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PRODUCT CODE                                    *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF PRODUCT FIELD)                          *         
*                                                                     *         
* EXIT - CC=EQUAL IF PRODUCT IS VALID WITH PRODUCT VALUES EXTRACTED   *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALPRD   MVI   FVMINL,2            REQUIRED FIELD                               
         MVI   FVMAXL,L'QPRD                                                    
         GOTO1 AFVAL                                                            
         BNE   VALPRDX                                                          
         CLC   =C'AAA',FVIFLD                                                   
         BE    VALPRD9                                                          
         CLC   QPRD,FVIFLD         TEST CHANGE OF PRODUCT                       
         BE    VALPRDX                                                          
         LA    R2,IOKEY            BUILD KEY OF PRODUCT RECORD                  
         USING PRDHDRD,R2                                                       
         XC    PKEY,PKEY                                                        
         MVI   PKEYTYPE,0                                                       
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,FVIFLD                                                   
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   VALPRD9                                                          
         LA    R2,RIO                                                           
         MVC   PRDNM,PNAME                                                      
*                                  GOING TO USE ESTRATE, NEED TO CHECK          
         MVC   ESTRATE,PRATE         WHEN WE GO INTO VALEST                     
         XC    BPRD(G1WPROF-BPRD),BPRD                                          
         MVC   BPRD,PCODE+1                                                     
         XC    QPRD(QVALSX-QPRD),QPRD                                           
         MVC   QPRD,FVIFLD                                                      
         B     VALPRDX                                                          
*                                                                               
VALPRD9  MVC   FVMSGNO,=AL2(FVIPRD)                                             
*                                                                               
VALPRDX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PRODUCT RECORD                                       *         
*                                                                     *         
* NTRY - R1=A(BINARY PRODUCT NUMBER)                                  *         
*                                                                     *         
* EXIT - CC=EQUAL IF PRODUCT IS VALID WITH PRODUCT VALUES EXTRACTED   *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
LGETPRD  NTR1  ,                   LOCAL ENTRY POINT                            
         MVI   RFLAG,1                                                          
         B     GETPRD1                                                          
*                                                                               
GETPRD   MVI   RFLAG,0                                                          
*                                                                               
GETPRD1  MVC   RDUB(L'BPRD),0(R1)                                               
         CLC   BPRD,RDUB           TEST CHANGE OF PRODUCT                       
         BE    GETPRDX                                                          
         LA    R2,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   GETPRD9                                                          
         LA    R2,RIO                                                           
         LA    R1,CLIST                                                         
*                                                                               
GETPRD2  CLI   0(R1),C'A'                                                       
         BL    GETPRD9                                                          
         CLC   RDUB(1),3(R1)                                                    
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     GETPRD2                                                          
         MVC   RDUB+1(3),0(R1)                                                  
         LA    R2,IOKEY            BUILD KEY OF PRODUCT RECORD                  
         USING PRDHDRD,R2                                                       
         XC    PKEY,PKEY                                                        
         MVI   PKEYTYPE,0                                                       
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,RDUB+1                                                   
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   GETPRD9                                                          
         LA    R2,RIO                                                           
         MVC   PRDNM,PNAME                                                      
*                                  GOING TO USE ESTRATE, NEED TO CHECK          
         MVC   ESTRATE,PRATE         WHEN WE GO INTO VALEST                     
         XC    BPRD(G1WPROF-BPRD),BPRD                                          
         MVC   BPRD,RDUB                                                        
         XC    QPRD(QVALSX-QPRD),QPRD                                           
         MVC   QPRD,RDUB+1                                                      
         B     GETPRDX                                                          
*                                                                               
GETPRD9  MVC   FVMSGNO,=AL2(FVIPRD)                                             
*                                                                               
GETPRDX  CLI   RFLAG,1                                                          
         BE    EXIT                                                             
         B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN ESTIMATE CODE                                *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF ESTIMATE FIELD)                         *         
*                                                                     *         
* EXIT - CC=EQUAL IF ESTIMATE IS VALID WITH VALUES EXTRACTED          *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
VALEST   BRAS  RE,VALIEST                                                       
         B     ROUTSX                                                           
***********************************************************************         
* ROUTINE TO GET ESTIMATE RECORD                                      *         
*                                                                     *         
* NTRY - R1=A(BINARY ESTIMATE NUMBER)                                 *         
*                                                                     *         
* EXIT - CC=EQUAL IF ESTIMATE IS VALID WITH VALUES EXTRACTED          *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
LGETEST  NTR1  ,                   LOCAL ENTRY POINT                            
         MVI   RFLAG,1                                                          
         B     GETEST1                                                          
*                                                                               
GETEST   MVI   RFLAG,0                                                          
*                                                                               
GETEST1  MVC   RDUB(L'BEST),0(R1)                                               
         CLC   BEST,RDUB           TEST FOR CHANGE OF ESTIMATE                  
         BE    GETESTX                                                          
         LA    R2,IOKEY            BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,RDUB                                                     
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   GETEST9                                                          
         MVI   ESTIND,0                                                         
         LA    R2,RIO                                                           
         MVC   ESTNM,EDESC                                                      
         MVC   ESTST,ESTART                                                     
         MVC   ESTND,EEND                                                       
         MVC   ESTPW,EPWPCT                                                     
         OC    ECOST2,ECOST2                                                    
         BZ    GETEST1A                                                         
         OC    EPWPCT,EPWPCT                                                    
         BNZ   GETEST9             INVALID EST, HAS PW AND C2                   
         MVC   ESTPW,ECOST2+1                                                   
         OI    ESTIND,ESTICS2                                                   
*                                                                               
GETEST1A BRAS  RE,GETESTIN                                                      
*                                                                               
         TM    CLTIND2,CLTIEST     TEST CAMP AND EST DATES MUST MATCH           
         BZ    GETEST2                                                          
         GOTO1 VDATCON,RPARM,(3,CMPSTDT),RWORK YES-                             
         GOTO1 (RF),(R1),(3,CMPND),RWORK+6                                      
         SR    R1,R1                                                            
         CLC   ESTST,RWORK                                                      
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         CLC   ESTND,RWORK+6                                                    
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1               TEST START OR END MATCH                      
         BZ    GETEST8             NO-ERROR                                     
         OC    CMPCCAM,CMPCCAM     YES-TEST COMPANION CAMPAIGN                  
         BNZ   GETEST2             YES-OK                                       
         CHI   R1,2                NO-BOTH MUST MATCH                           
         BNE   GETEST8                                                          
*                                                                               
GETEST2  CLI   CMPPGRPN,0          TEST PRODUCT GROUP                           
         BE    GETEST2A                                                         
         GOTO1 =A(GETPGDEM),RR=ACRELO  YES-GET ESTIMATE DEMOS FOR ANY           
         BNE   GETESTX                 PRODUCT IN THE GROUP                     
         B     GETEST4                                                          
GETEST2A LA    R0,14                                                            
         LA    RE,ESTDEMS                                                       
         XC    ESTDEMS,ESTDEMS                                                  
         LA    RF,EDEMLST                                                       
*                                                                               
GETEST3  OC    0(3,RF),0(RF)                                                    
         BZ    GETEST4                                                          
         MVC   0(3,RE),0(RF)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,GETEST3                                                       
*                                                                               
GETEST4  MVC   ESTUSRNM,EUSRNMS                                                 
         MVC   ESTDMENU,EDAYMENU                                                
         MVC   ESTBOOK,EBOOK                                                    
         MVC   ESTREQLO,EREQLO                                                  
         MVC   ESTREQHI,EREQHI                                                  
*                                                                               
         CLI   ESTRATE,C'*'        PORDUCT OVERRIDE TO NORMAL RATES?            
         BE    GETEST5                                                          
         CLI   ERATE,C'*'          OVERRIDE TO NORMAL RATES?                    
         BE    *+12                YES                                          
         CLI   ERATE,C'0'          RATE SET FOR ESTIMATE?                       
         BNH   *+10                NO, DON'T OVERWRITE RATE FROM PROD           
         MVC   ESTRATE,ERATE       YES, OVERWRITE RATE FROM PROD                
*                                                                               
GETEST5  MVC   ESTREP,EREP         SPECIAL REP CODE                             
         MVC   ESTOWSDY,EOWSDAY    OUT-OF-WEEK START DAY                        
         CLI   ESTOWSDY,1          TEST MONDAY                                  
         BNE   *+8                                                              
         MVI   ESTOWSDY,0          YES-IGNORE                                   
         MVC   ESTCNTRL,ECNTRL                                                  
         CLI   EDAILY,C'Y'         DAILY ESTIMATE                               
         BNE   *+12                                                             
         OI    ESTIND,ESTIDLY                                                   
         OI    CMPOPTS,CAMODLY                                                  
         BAS   RE,GETDEMNM         PRIMARY DEMO NAME                            
*                                                                               
         XC    BEST(G1WPROF-BEST),BEST                                          
         MVC   BEST,RDUB                                                        
         XC    QEST(QVALSX-QEST),QEST                                           
         ZIC   R1,RDUB                                                          
         CVD   R1,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  QEST,RDUB                                                        
         B     GETESTX                                                          
*                                                                               
GETEST8  MVC   FVMSGNO,=AL2(FVCMPEST)                                           
         B     GETESTX                                                          
*                                                                               
GETEST9  MVC   FVMSGNO,=AL2(FVIEST)                                             
*                                                                               
GETESTX  CLI   RFLAG,1                                                          
         BNE   ROUTSX                                                           
         CLC   FVMSGNO,=AL2(FVFOK) SET CONDITION CODE FOR INTERNAL CALL         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET CAMPAIGN AND MARKET                                  *         
*                                                                     *         
* NTRY - R1=A(2-BYTE CAMPAIGN/MARKET SEQUENCE CODE)                   *         
*                                                                     *         
* EXIT - CC=EQUAL IF CODE IS VALID WITH VALUES EXTRACTED              *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
GETCM    MVC   RHALF(L'BCMSEQ),0(R1)                                            
         CLC   BCMSEQ,RHALF        TEST FOR CHANGE OF SEQUENCE CODE             
         BE    GETCMX                                                           
         LA    R2,IOKEY            BUILD HEADER PASSIVE POINTER                 
         USING BWHPKEY,R2                                                       
         XC    BWHPKEY,BWHPKEY                                                  
         MVI   BWHPTYP,BWHPTYPQ                                                 
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,BAGYMD                                                  
         OC    BWHPAGMD,BBYRMASK                                                
         MVC   BWHPBYR,BBYR                                                     
         MVC   BWHPSEQ,RHALF                                                    
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   GETCM9                                                           
         LA    R2,RIO                                                           
         MVC   RFULL(2),BWHKMKT                                                 
         GOTO1 LGETCAM,BWHKCAM     GET CAMPAIGN                                 
*                                                                               
         MVC   BCMSEQ,RHALF                                                     
         GOTO1 LGETCLT,CMPCLTC     GET CLIENT                                   
         GOTO1 LGETPRD,CMPPRDN     GET PRODUCT                                  
         GOTO1 LGETEST,CMPESTN     GET ESTIMATE                                 
         GOTO1 LGETMKT,RFULL       GET MARKET                                   
*                                                                               
GETCM9   MVC   FVMSGNO,=AL2(FVICAM)                                             
*                                                                               
GETCMX   B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE A MARKET CODE                                   *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF MARKET FIELD)                           *         
*                                                                     *         
* EXIT - CC=EQUAL IF MARKET IS VALID WITH VALUES EXTRACTED            *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALMKT   MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'QMKT                                                    
         GOTO1 AFVAL                                                            
         BNE   VALMKTX                                                          
         TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BZ    VALMKT9                                                          
         OC    SCFULL,SCFULL       MARKET 0 INVALID,                            
         BNZ   *+12                                                             
         CLI   QMED,C'N'           EXCEPT FOR MEDIA = N                         
         BNE   VALMKT9                                                          
         XC    QSTA,QSTA           CLEAR STATION FIELDS                         
         XC    BSTA,BSTA                                                        
         XC    QCABLE,QCABLE       CLEAR CABLE FILTER                           
         MVC   RHALF,SCFULL+2                                                   
         CLC   BMKT,RHALF          TEST FOR CHANGE OF MARKET                    
         BE    VALMKTX                                                          
         UNPK  RWORK(L'QMKT),SCDUB                                              
         OI    RWORK+L'QMKT-1,X'F0'                                             
         NI    RFLAG,X'FE'         NOT ENTRY FROM STATION ROUTINE               
*                                                                               
VALMKT2  XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING MKTRECD,R2                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,RWORK                                                    
         MVC   MKTKAGY,CUAALF                                                   
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   VALMKT9                                                          
         MVC   MKTNM,MKTNAME                                                    
         MVI   RBYTE,C'0'          FIND RATING SERVICE MARKET                   
****  NOTE:  CANADA NEVER USES CLTSRC!!!                                        
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    VALMKT2G                                                         
         CLI   MKTRSVC,C'0'        NSI?                                         
         BNE   *+8                  - NOPE                                      
         MVI   CLTSRC,C'N'                                                      
         CLI   MKTRSVC,C'1'        BBM?                                         
         BNE   *+8                  - NOPE                                      
         MVI   CLTSRC,C'A'                                                      
****  NOTE:  CANADA NEVER USES CLTSRC!!!                                        
VALMKT2G CLI   CLTSRC,C'N'                                                      
         BE    *+8                                                              
         MVI   RBYTE,C'1'                                                       
         CLC   MKTRS1,RBYTE                                                     
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM1                                                    
         CLC   MKTRS2,RBYTE                                                     
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM2                                                    
         MVC   MKTLKUP,RHALF                                                    
         CLI   CMPRSVC,0           TEST RATING SERVICE OVERRIDE                 
         BE    *+10                                                             
         XC    MKTLKUP,MKTLKUP     YES-CLEAR AGY MKT FOR DEMO LOOKUPS           
*                                                                               
         L     RF,ATWA                                                          
         AHI   RF,SVMLPMSD-TWAD    SAVE THE MARKET LPM START DATE               
         MVC   0(L'SVMLPMSD,RF),MKTLPMDT                                        
         L     RF,ATWA             GIVE SPDEMUP MARKET'S ALPHA CODE             
         AHI   RF,SVMALPHA-TWAD                                                 
         MVC   0(L'SVMALPHA,RF),MKTALST                                         
*                                                                               
         MVC   3(L'SVMRTGSV,RF),MKTRSVC   SAVE THE RATING SERVICE               
*                                                                               
         LA    RF,CUACCS                                                        
         CLI   CUACCS,C'+'         TEST MARKET LOCKOUT                          
         BE    *+16                                                             
         LA    RF,CUACCS+2                                                      
         CLI   CUACCS+2,C'+'                                                    
         BNE   VALMKT4                                                          
*                                                                               
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
         CLC   1(1,RF),0(R1)                                                    
         BE    VALMKT4                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         B     VALMKT8                                                          
*                                                                               
VALMKT4  XC    BMKT(G1WPROF-BMKT),BMKT                                          
         MVC   BMKT,RHALF                                                       
         XC    QMKT(QVALSX-QMKT),QMKT                                           
         MVC   QMKT,RWORK                                                       
         TM    RFLAG,X'01'         TEST ENTRY FROM VALSTA                       
         BO    VALSTA5                                                          
         B     VALMKTX                                                          
*                                                                               
VALMKT8  MVC   FVMSGNO,=AL2(FVNOMKTA)                                           
         B     VALMKTX                                                          
*                                                                               
VALMKT9  MVC   FVMSGNO,=AL2(FVIMKT)                                             
*                                                                               
VALMKTX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET MARKET RECORD                                        *         
*                                                                     *         
* NTRY - R1=A(BINARY MARKET NUMBER)                                   *         
*                                                                     *         
* EXIT - CC=EQUAL IF MARKET IS VALID WITH VALUES EXTRACTED            *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
LGETMKT  NTR1  ,                   LOCAL ENTRY POINT                            
         MVI   RFLAG,1                                                          
         B     GETMKT1                                                          
*                                                                               
GETMKT   MVI   RFLAG,0                                                          
*                                                                               
GETMKT1  MVC   RHALF,0(R1)                                                      
         CLC   BMKT,RHALF                                                       
         BE    GETMKTX                                                          
         SR    R1,R1                                                            
         ICM   R1,3,RHALF                                                       
         CVD   R1,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  RWORK(L'QMKT),RDUB                                               
         LA    R2,IOKEY                                                         
         USING MKTRECD,R2                                                       
         MVI   MKTKEY,C'0'                                                      
         MVC   MKTKEY+1(L'MKTKEY-1),MKTKEY                                      
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,RWORK                                                    
         MVC   MKTKAGY,CUAALF                                                   
         MVC   MKTLKUP,RHALF                                                    
         CLC   CLTSRC,CLTSRCDF     TEST RATING SERVICE OVERRIDE                 
         BE    *+10                                                             
         XC    MKTLKUP,MKTLKUP     YES-CLEAR AGY MKT FOR DEMO LOOKUPS           
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   GETMKT9                                                          
         MVC   MKTNM,MKTNAME                                                    
*                                                                               
         L     RF,ATWA                                                          
         AHI   RF,SVMLPMSD-TWAD    SAVE THE MARKET LPM START DATE               
         MVC   0(L'SVMLPMSD,RF),MKTLPMDT                                        
         L     RF,ATWA             GIVE SPDEMUP MARKET'S ALPHA CODE             
         AHI   RF,SVMALPHA-TWAD                                                 
         MVC   0(L'SVMALPHA,RF),MKTALST                                         
*                                                                               
         XC    BMKT(G1WPROF-BMKT),BMKT                                          
         MVC   BMKT,RHALF                                                       
         XC    QMKT(QVALSX-QMKT),QMKT                                           
         MVC   QMKT,RWORK                                                       
         B     GETMKTX                                                          
*                                                                               
GETMKT9  MVC   FVMSGNO,=AL2(FVIMKT)                                             
*                                                                               
GETMKTX  CLI   RFLAG,1                                                          
         BE    EXIT                                                             
         B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE STATION CALL LETTERS                            *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF STATION FIELD)                          *         
*                                                                     *         
* EXIT - CC=EQUAL IF STATION IS VALID WITH VALUES EXTRACTED           *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALSTA   MVI   FVMINL,3                                                         
         MVI   FVMAXL,8                                                         
         GOTO1 AFVAL                                                            
         BNE   VALSTAX                                                          
         XC    RWORK2,RWORK2       CALL STAVAL                                  
         LA    R2,RWORK2                                                        
         USING STABLKD,R2                                                       
         LA    R1,FVIHDR                                                        
         ST    R1,STBADDR                                                       
         MVC   STBMED,QMED                                                      
*                                                                               
         MVI   STBCTRY,C'U'                                                     
*****    CLI   APROF7,C'C'                                                      
*****    BNE   *+8                                                              
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    *+8                                                              
         MVI   STBCTRY,C'C'                                                     
*                                                                               
         MVC   STBACOM,ACOM                                                     
*                                                                               
         GOTO1 VSTAVAL,RPARM,STABLKD                                            
         CLI   STBERR,STBESTA                                                   
         BE    VALSTA9                                                          
         CLI   STBERR,STBNOTON     NETWORK NOT TURNED ON?                       
         BE    VALSTA10            NOT ON                                       
*                                                                               
         MVC   RDUB,STBSTA                                                      
         CLI   RDUB+4,C' '                                                      
         BH    *+10                                                             
         MVC   RDUB+4(1),QMED                                                   
         CLC   QSTA,RDUB           TEST FOR CHANGE IN STATION                   
         BE    VALSTAX                                                          
         CLI   RDUB,C'0'           FOR CABLE STATIONS,                          
         BL    VALSTA2                                                          
         CLC   RDUB+5(3),RSPACES   TEST FOR NETWORK                             
         BH    VALSTA2                                                          
         CLI   INREC,RECBUY        MISSING-ONLY VALID FOR WORK AND BUY          
         BE    *+12                   RECORDS AND LIST ACTIONS                  
         CLI   INREC,RECWRK                                                     
         BNE   VALSTA9                                                          
         CLI   INACT,ACTUDT                                                     
         BE    VALSTA1                                                          
         CLI   INACT,ACTSKD                                                     
         BE    VALSTA1                                                          
         CLI   INACT,ACTSSK                                                     
         BE    VALSTA1                                                          
         CLI   INACT,ACTDEM                                                     
         BE    VALSTA1                                                          
         CLI   INACT,ACTREP                                                     
         BE    VALSTA1                                                          
         CLI   INACT,ACTUPR                                                     
         BE    VALSTA1                                                          
         CLI   INACT,ACTXFR        AND TRANSFER                                 
         BE    VALSTA1                                                          
         CLI   INACT,ACTBYA        AND BUYADD                                   
         BNE   VALSTA9                                                          
*                                                                               
VALSTA1  DS    0H                                                               
*&&DO                                                                           
VALSTA1  CLC   QCABLE,RDUB         IF NO CHANGE OF CABLE SYS FILTER,            
         BE    VALSTAX             THEN EXIT                                    
         DROP  R2                                                               
*&&                                                                             
VALSTA2  XC    QCABLE,QCABLE                                                    
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY            READ THE STATION MASTER RECORD               
         USING STARECD,R2                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(STAKEYLN-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,RDUB                                                    
         MVC   STAKAGY,CUAALF                                                   
         LA    R2,RIO              READ NON CLIENT SPECIFIC STATION REC         
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   VALSTA9                                                          
         PACK  RDUB2,SMKT                                                       
         CVB   RE,RDUB2                                                         
         STCM  RE,3,MKTNOCLT       NON CLIENT SPECIFIC MARKET                   
         LA    R2,IOKEY                                                         
         MVC   STAKCLT,QCLT        NOW READ CLIENT SPECIFIC                     
         GOTO1 AIO,IOSTAFIL+IORD                                                
         LA    R2,RIO                                                           
         OC    SEFFDATE,SEFFDATE   TEST STATION HAS EFFECTIVE DATE              
         BZ    *+14                                                             
         CLC   ASBDAT,SEFFDATE     YES-ERROR IF TODAY IS PRIOR                  
         BL    VALSTA7                                                          
         CLC   SMKT,=C'0000'       TEST MARKET = 0                              
         BNE   *+12                                                             
         CLI   QMED,C'N'           YES-INVALID FOR MEDIA NEQ N                  
         BNE   VALSTA6                                                          
         MVC   RWORK(L'QMKT),SMKT  EXTRACT MARKET                               
         MVC   STANTAX,SNEWTAX             TAX RATE                             
*********                                                                       
*****    CLI   APROF7,C'C'         CANADIAN AGENCY?                             
*****    BNE   VALSTA3                                                          
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    VALSTA3                                                          
*                                                                               
         CLI   SCOUNTRY,C'U'       YES, US STATION?                             
         BE    VALSTA3                  YES, KEEP SALES TAX                     
         XC    STANTAX,STANTAX          NO, NO SALES TAX THEN                   
*********                                                                       
VALSTA3  MVC   STAGST,SGSTCODE             CANADIAN GST CODE                    
*****                                                                           
         TM    ESTIND,ESTIBTYP     ESTIMATE HAS OVERRIDE BOOKTYPE?              
         BNZ   VALSTA3A                                                         
*****                                                                           
         MVC   STABKTYP,SBKTYPE            BOOK TYPE                            
*&&DO                                                                           
         CLI   STABKTYP,C'A'              (ONLY ALLOW A-Z)                      
         BL    *+12                                                             
         CLI   STABKTYP,C'9'                                                    
         BNH   *+8                                                              
         MVI   STABKTYP,0                                                       
*&&                                                                             
*                                                                               
VALSTA3A XC    ACWORK,ACWORK                                                    
         CLI   RDUB,C'0'           IF CABLE,                                    
         BL    *+10                                                             
         MVC   ACWORK(24),SSYSNAME SAVE CABLE SYSTEM NAME IN WORK               
         OC    ACWORK,RSPACES                                                   
         MVC   ACWORK+24(10),SPST  PASS PST CODES IN WORK+24                    
         TM    SFLAG1,SQARBF94     F94=ARB OPTION FOR STATION                   
         BZ    *+8                                                              
         OI    STAIND,STAIF94A                                                  
*                                                                               
         GOTO1 VMSPACK,RPARM,RWORK,RDUB,RWORK+4                                 
         BNE   VALSTA9                                                          
*                                                                               
         CLI   RDUB,C'0'           FOR CABLE STATIONS,                          
         BL    VALSTA4                                                          
         CLC   RDUB+5(3),RSPACES   TEST FOR NETWORK                             
         BH    *+14                                                             
         MVC   QCABLE,RDUB         NO-SAVE CABLE SYSTEM FILTER                  
         B     VALSTA4                                                          
*&&DO                                                                           
         MVC   RBYTE,RWORK+8                                                    
*CBL     NI    RBYTE,X'3F'         ISOLATE CABLE NETWORK BITS                   
         NI    RBYTE,X'7F'         ** FOR 7-BIT NETWORK                         
         ZIC   RE,RBYTE                                                         
         LA    RF,RWORK3                                                        
         XC    RWORK3(16),RWORK3                                                
         CLI   RBYTE,64                                                         
         BNH   *+12                                                             
         LA    RF,8(RF)                                                         
         AHI   RE,-1*64                                                         
         BCTR  RE,0                                                             
         SR    R1,R1                                                            
         LA    R0,1                                                             
         SLL   R0,31                                                            
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         SRDL  R0,1                                                             
         BCT   RE,*-4                                                           
         STM   R0,R1,0(RF)                                                      
*CBL     NC    RWORK3(8),SSYSNETS  TEST NETWORK BIT IS ON IN RECORD             
         NC    RWORK3(16),SSYSNETS ** FOR 7-BIT NETWORK                         
         BZ    VALSTA8             NO-INVALID NETWORK                           
*&&                                                                             
VALSTA4  CLC   QMKT,RWORK          TEST FOR CHANGE IN MARKET                    
         BE    VALSTA5                                                          
         MVC   RHALF,RWORK+4       BINARY MARKET                                
         OI    RFLAG,X'01'         ENTRY FROM STATION ROUTINE                   
         B     VALMKT2             VALIDATE THE MARKET                          
*                                                                               
VALSTA5  OC    QCABLE,QCABLE       TEST CABLE SYSTEM FILTER                     
         BZ    *+16                                                             
         XC    RDUB,RDUB           YES-THEN NO STATION FILTERS                  
         XC    RWORK+6(3),RWORK+6                                               
*                                                                               
         XC    QSTA(QVALSX-QSTA),QSTA                                           
         MVC   QSTA,RDUB                                                        
         XC    BSTA(G1WPROF-BSTA),BSTA                                          
         MVC   BSTA,RWORK+6                                                     
         B     VALSTAX                                                          
*                                                                               
VALSTA6  MVC   FVMSGNO,=AL2(FVIMKT)   INVALID MARKET                            
         B     VALSTAX                                                          
*                                                                               
VALSTA7  MVC   FVMSGNO,=AL2(FVSTAEFF) STATION NOT EFFECTIVE UNTIL ....          
         XC    FVXTRA,FVXTRA                                                    
         GOTO1 VDATCON,RPARM,(3,SEFFDATE),(5,FVXTRA)                            
         B     VALSTAX                                                          
*                                                                               
VALSTA8  MVC   FVMSGNO,=AL2(FVICNET)  CABLE NETWORK NOT ON MASTER REC           
         B     VALSTAX                                                          
*                                                                               
VALSTA9  MVC   FVMSGNO,=AL2(FVISTA)   INVALID STATION                           
         B     VALSTAX                                                          
*                                                                               
VALSTA10 MVC   FVMSGNO,=AL2(FVIANTWK) NETWORK NOT TURNED ON                     
*                                                                               
VALSTAX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET STATION RECORD                                       *         
*                                                                     *         
* NTRY - R1=A(STATION CALL LETTERS, EG WABCT)                                   
*                                                                     *         
* EXIT - CC=EQUAL IF STATION IS VALID WITH VALUES EXTRACTED           *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
GETSTA   MVC   RDUB(5),0(R1)                                                    
         LA    R2,IOKEY            READ THE STATION MASTER RECORD               
         USING STARECD,R2                                                       
         MVI   STAKEY,C'0'                                                      
         MVC   STAKEY+1(L'STAKEY-1),STAKEY                                      
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,RDUB                                                    
         MVC   STAKAGY,CUAALF                                                   
         MVC   STAKCLT,QCLT                                                     
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,IOSTAFIL+IORD                                                
         BNE   GETSTA9                                                          
         MVC   STANTAX,SNEWTAX     TAX RATE                                     
         MVC   STAGST,SGSTCODE     CANADIAN GST CODE                            
*****                                                                           
         TM    ESTIND,ESTIBTYP     ESTIMATE HAS OVERRIDE BOOKTYPE?              
         BNZ   GETSTA5                                                          
*****                                                                           
         MVC   STABKTYP,SBKTYPE    BOOK TYPE                                    
*&&DO                                                                           
         CLI   STABKTYP,C'A'       (ONLY ALLOW A-Z)                             
         BL    *+12                                                             
         CLI   STABKTYP,C'9'                                                    
         BNH   GETSTA5                                                          
         MVI   STABKTYP,0                                                       
*&&                                                                             
*                                                                               
GETSTA5  XC    ACWORK,ACWORK                                                    
         CLI   RDUB,C'0'           IF CABLE,                                    
         BL    *+10                                                             
         MVC   ACWORK(24),SSYSNAME SAVE CABLE SYSTEM NAME IN WORK               
         OC    ACWORK,RSPACES                                                   
         MVC   ACWORK+24(10),SPST  PASS PST CODES IN WORK+24                    
         TM    SFLAG1,SQARBF94     F94=ARB OPTION FOR STATION                   
         BZ    *+8                                                              
         OI    STAIND,STAIF94A                                                  
         B     GETSTAX                                                          
*                                                                               
GETSTA9  MVC   FVMSGNO,=AL2(FVISTA)                                             
*                                                                               
GETSTAX  B     ROUTSX                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DAY EXPRESSION                                  *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF DAY EXPRESSION FIELD)                   *         
*                                                                     *         
* EXIT - CC=EQUAL IF DAY EXPRESSION IS VALID WITH VALUES EXTRACTED    *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALDAYS  MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALDAYX                                                          
         ZIC   R2,FVILEN                                                        
         GOTO1 VDAYPAK,RPARM,((R2),FVIFLD),RBYTE,RWORK                          
         CLI   RBYTE,0                                                          
         BE    VALDAY9                                                          
         ZIC   RE,RWORK                                                         
         STC   RE,RHALF+1                                                       
         NI    RHALF+1,X'0F'                                                    
         SRL   RE,4                                                             
         STC   RE,RHALF                                                         
         CLI   ESTOWSDY,0          TEST OUT-OF-WEEK ROTATIONS                   
         BNE   VALDAY2                                                          
         CLC   RHALF(1),RHALF+1    NO-CHECK START DAY LE END DAY                
         BH    VALDAY9                                                          
         B     VALDAY8                                                          
*                                                                               
VALDAY2  CLI   FVILEN,7            TEST SPECIAL 7-BYTE DAY EXPRESSION           
         BNE   VALDAY4                                                          
         CLC   FVIFLD(7),=C'MTWTFSS'                                            
         BE    VALDAY8                                                          
         LA    R0,7                                                             
         LA    R1,FVIFLD                                                        
         CLI   0(R1),C'.'                                                       
         BE    VALDAY8                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VALDAY4  CLC   RHALF(1),RHALF+1    NO-CHECK DAYS DON'T CROSS START DAY          
         BE    VALDAY8                OF WEEK                                   
         BH    VALDAY6                                                          
         CLC   RHALF(1),ESTOWSDY                                                
         BNL   VALDAY8                                                          
         CLC   RHALF+1(1),ESTOWSDY                                              
         BNL   VALDAY9                                                          
         B     VALDAY8                                                          
*                                                                               
VALDAY6  CLC   RHALF(1),ESTOWSDY                                                
         BL    VALDAY9                                                          
         CLC   RHALF+1(1),ESTOWSDY                                              
         BNL   VALDAY9                                                          
*                                                                               
VALDAY8  XC    BDAYS(G1WPROF-BDAYS),BDAYS                                       
         MVC   BDAYS,RBYTE                                                      
         XC    QDAYS(QVALSX-QDAYS),QDAYS                                        
         MVC   QDAYS,FVIFLD                                                     
         B     VALDAYX                                                          
*                                                                               
VALDAY9  MVC   FVMSGNO,=AL2(FVIDAYS)                                            
*                                                                               
VALDAYX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DAY EXPRESSION                                       *         
*                                                                     *         
* NTRY - R1=A(1-BYTE DAY FIELD)                                       *         
*                                                                     *         
* EXIT - CC=EQUAL IF DAY IS VALID WITH DAY EXPRESSION SET             *         
***********************************************************************         
         SPACE 1                                                                
GETDAY   MVC   RDUB(L'BDAYS),0(R1)                                              
         CLC   BDAYS,RDUB                                                       
         BE    GETDAYX                                                          
         GOTO1 VDAYUNPK,RPARM,RDUB,(7,RWORK)                                    
         XC    BDAYS(G1WPROF-BDAYS),BDAYS                                       
         MVC   BDAYS,RDUB                                                       
         XC    QDAYS(QVALSX-QDAYS),QDAYS                                        
         MVC   QDAYS,RWORK                                                      
*                                                                               
GETDAYX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE TIME EXPRESSION                                 *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF TIME EXPRESSION FIELD                   *         
*                                                                     *         
* EXIT - CC=EQUAL IF TIME  EXPRESSION IS VALID WITH VALUES EXTRACTED  *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALTIM   MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALTIMX                                                          
         ZIC   R2,FVILEN                                                        
         GOTO1 VTIMVAL,RPARM,((R2),FVIFLD),RFULL                                
         CLI   RPARM,X'FF'                                                      
         BE    VALTIM9                                                          
         CLC   RFULL+2(2),=C'CC'   TEST 'TILL CONCLUSION'                       
         BE    VALTIM9                                                          
         CLC   RFULL,=C'VARY'      TEST 'VARIOUS'                               
         BE    VALTIM9                                                          
         GOTO1 LPACKTIM,RFULL      PACK THE TIMES                               
*                                                                               
         XC    BTIMES(G1WPROF-BTIMES),BTIMES                                    
         MVC   BTIMES,RFULL                                                     
         XC    QTIMES(QVALSX-QTIMES),QTIMES                                     
         MVC   QTIMES,FVIFLD                                                    
         B     VALTIMX                                                          
*                                                                               
VALTIM9  MVC   FVMSGNO,=AL2(FVITIMES)                                           
*                                                                               
VALTIMX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TIME EXPRESSION                                      *         
*                                                                     *         
* NTRY - R1=A(4-BYTE TIME FIELD)                                      *         
*                                                                     *         
* EXIT - CC=EQUAL IF DAY IS VALID WITH TIME EXPRESSION SET            *         
***********************************************************************         
         SPACE 1                                                                
GETTIM   MVC   RDUB(L'BTIMES),0(R1)                                             
         CLC   BTIMES,RDUB                                                      
         BE    GETTIMX                                                          
         XC    RWORK(L'QTIMES),RWORK                                            
         GOTO1 VUNTIME,RPARM,RDUB,RWORK                                         
         XC    BTIMES(G1WPROF-BTIMES),BTIMES                                    
         MVC   BTIMES,RDUB                                                      
         XC    QTIMES(QVALSX-QTIMES),QTIMES                                     
         MVC   QTIMES,RWORK                                                     
         GOTO1 LPACKTIM,BTIMES     SET PTIMES                                   
*                                                                               
GETTIMX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DAYPART/LENGTH                                  *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF DAYPART/LENGTH FIELD)                   *         
*                                                                     *         
* EXIT - CC=EQUAL IF DPT/LEN IS VALID WITH VALUES EXTRACTED           *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALDPL   MVI   FVMINL,1                                                         
         MVI   FVMAXL,5                                                         
         GOTO1 AFVAL                                                            
         BNE   VALDPLX                                                          
         MVI   RBYTE,0                                                          
         MVI   RFLAG,0                                                          
         TM    FVIIND,FVINUM       TEST SPOT LENGTH ONLY                        
         BZ    VALDPL1                                                          
***      OC    SCFULL(3),SCFULL    CHECK LENGTH NOT TOO BIG                     
***      BNZ   VALDPL92                                                         
***      CLI   FVILEN,1            TEST SINGLE NUMERIC                          
***      BE    VALDPL1             YES-IT MUST BE A NUMERIC DAYPART             
***   NOTE:  SPOT LENGTH COULD BE SINGLE NUMERIC NOW!!                          
         CLI   FVIFLD+1,C'/'                                                    
         BE    VALDPL1                                                          
         CLI   FVIFLD+1,C','                                                    
         BE    VALDPL1                                                          
         MVC   RFLAG,SCFULL+3      VALIDATE SPOT LENGTH                         
         B     VALDPL5                                                          
*                                                                               
VALDPL1  MVC   RBYTE,FVIFLD        DAYPART                                      
         CLC   FVIFLD+1(2),=C'/ '  FOR DAYPART + /                              
         BE    VALDPL7              - YUP                                       
         CLI   FVILEN,1            TEST FOR SPOT LENGTH INCLUDED                
         BNE   VALDPL2             YES                                          
         B     VALDPL7                                                          
***      MVC   RFLAG,CMPSLN        NO-USE CAMPAIGN SPOT LENGTH                  
***      CLI   RFLAG,0             TEST SLN=0                                   
***      BNE   VALDPL7             NO                                           
***      TM    INMIX1,MIXILST+MIXISEL+MIXIREP YES-OK LIST/SELECT MODE           
***      BNZ   VALDPL7                               OR REPORT MODE             
***      B     VALDPL94            ELSE INSIST ON SPOT LENGTH                   
*                                                                               
VALDPL2  DS    0H                                                               
***DPL2  CLI   FVILEN,2            VALIDATE THE SPOT LENGTH                     
***      BE    VALDPL92                                                         
***  NOTE:  P6 IS FAIR GAME FOR DPL NOW!!                                       
         ZIC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         LA    RF,FVIFLD+1                                                      
         CLI   0(RF),C','                                                       
         BE    *+12                                                             
         CLI   0(RF),C'/'                                                       
         BNE   VALDPL3                                                          
         BCTR  RE,0                                                             
         LA    RF,1(RF)                                                         
         CLI   FVILEN,3                                                         
         BE    VALDPL92                                                         
         B     VALDPL4                                                          
*                                                                               
VALDPL3  CLI   FVILEN,5                                                         
         BE    VALDPL92                                                         
*                                                                               
VALDPL4  CLI   0(RF),C'0'                                                       
         BL    VALDPL92                                                         
         CLI   1(RF),C' '          BLANK IS OK                                  
         BNH   VALDPL4C                                                         
         CLI   1(RF),C'0'          IT'S OK IF LENGTH IS SINGLE DIGIT!!          
         BL    VALDPL92            LENGTH OF 6 IS OK!!                          
VALDPL4C CHI   RE,2                                                             
         BNE   *+12                                                             
         CLI   2(RF),C'0'                                                       
         BL    VALDPL92                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  RDUB,0(0,RF)                                                     
         CVB   RE,RDUB                                                          
         STC   RE,RFLAG                                                         
*                                                                               
***DPL5  LA    RE,SLNTAB                                                        
VALDPL5  DS    0H                                                               
         L     R1,VSLNTAB          POINT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RF POINTS TO EOT                             
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMED,C'T'                                                        
         BE    VALDPL5C                                                         
         CLI   QMED,C'N'                                                        
         BE    VALDPL5C                                                         
         CLI   QMED,C'C'                                                        
         BE    VALDPL5C                                                         
         CLI   QMED,C'R'                                                        
         BE    VALDPL5C                                                         
         CLI   QMED,C'X'                                                        
         BE    VALDPL5C                                                         
         DC    H'0'                                                             
*                                                                               
VALDPL5C CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    VALDPL5G                                                         
         CLC   CUAALF,0(R1)        MATCH AGY ALPHA                              
         BNE   *+12                                                             
VALDPL5G CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    VALDPL5K                                                         
*                                                                               
         BXLE  R1,RE,VALDPL5C      NEXT ENTRY                                   
         DC    H'0'                                                             
*                                                                               
VALDPL5K AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,RFLAG            GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         BE    VALDPL92                                                         
*                                                                               
*&&DO                                                                           
VALDPL6  CLI   0(RE),0                                                          
         BE    VALDPL92                                                         
         CLC   RFLAG,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     VALDPL6                                                          
*&&                                                                             
         CLI   CMPSLN,0            VALID-TEST CAMPAIGN SLN IS NON-ZERO          
         BE    VALDPL7                                                          
         CLC   CMPSLN,RFLAG        YES-SLN'S MUST MATCH                         
         BNE   VALDPL92                                                         
*                                                                               
VALDPL7  CLI   RBYTE,0                                                          
         BE    VALDPL16                                                         
         CLC   BDPT,RBYTE          TEST FOR CHANGE IN DAYPART                   
         BE    VALDPL16                                                         
         CLI   RBYTE,C'Z'          YES-                                         
         BE    VALDPL90            DISALLOW DAYPART=Z                           
         XC    RPARM,RPARM         READ DAYPART RECORD                          
         MVC   RPARM(2),CUAALF                                                  
         MVC   RPARM+2(1),QMED                                                  
         MVC   RPARM+3(1),ESTDMENU                                              
         GOTO1 VDPTRD,RPARM,,RIO,VDMGR                                          
         CLI   RPARM+8,X'FF'                                                    
         BE    VALDPL90                                                         
         LA    RE,RIO                                                           
*                                                                               
VALDPL8  CLI   0(RE),0             FIND THE DAYPART CODE                        
         BE    VALDPL90                                                         
         CLC   0(1,RE),RBYTE                                                    
         BE    *+12                FOUND - VALID DAYPART                        
         LA    RE,5(RE)                                                         
         B     VALDPL8                                                          
*                                                                               
         MVC   RWORK(L'QDPT),2(RE)                                              
         XC    DPTSUBS,DPTSUBS                                                  
         MVI   DPTTYPE,C'R'        REGULAR DAYPART                              
         ZIC   R1,1(RE)                                                         
         SRL   R1,4                                                             
         LTR   R1,R1               TEST ANY SUB-DAYPARTS                        
         BZ    VALDPL16            NO                                           
         MVI   DPTTYPE,C'M'        MASTER DAYPART                               
         LA    R0,L'DPTSUBS                                                     
         LA    R5,DPTSUBS          YES - BUILD LIST OF SUB-DAYPARTS             
         LA    RE,RIO                                                           
         SR    RF,RF                                                            
*                                                                               
VALDPL10 CLI   0(RE),0                                                          
         BE    VALDPL16                                                         
         IC    RF,1(RE)                                                         
         SRL   RF,4                                                             
         CR    RF,R1               TEST THIS IS A SUB-DAYPART                   
         BNE   VALDPL14                                                         
         CLC   RBYTE,0(RE)         YES - TEST THIS IS OUR DAYPART               
         BNE   VALDPL12                                                         
         CLI   DPTSUBS,0           YES - TEST ANY SUBDAYPARTS BEFORE            
         BE    VALDPL14                                                         
         MVI   DPTTYPE,C'S'        YES - DPT IS NOT THE MASTER DPT,             
         XC    DPTSUBS,DPTSUBS           SO TYPE=SUB AND IT HAS                 
         B     VALDPL16                  NO SUB-DAYPARTS                        
*                                                                               
VALDPL12 MVC   0(1,R5),0(RE)                                                    
         LA    R5,1(R5)                                                         
         BCT   R0,VALDPL14                                                      
         B     VALDPL16                                                         
*                                                                               
VALDPL14 LA    RE,5(RE)                                                         
         B     VALDPL10                                                         
*                                                                               
VALDPL16 CLC   BSLN,RFLAG                                                       
         BE    VALDPL20                                                         
         ZIC   RE,RFLAG                                                         
         CVD   RE,RDUB                                                          
         OI    RDUB+7,X'0F'                                                     
         UNPK  RWORK+3(3),RDUB                                                  
*                                                                               
VALDPL20 XC    BDPT(G1WPROF-BDPT),BDPT                                          
         MVC   BDPT,RBYTE                                                       
         MVC   BSLN,RFLAG                                                       
         XC    QDPT(QVALSX-QDPT),QDPT                                           
         MVC   QDPT,RWORK                                                       
         MVC   QSLN,RWORK+3                                                     
*                                                                               
         CLI   BDPT,0              TEST DAYPART                                 
         BNE   VALDPLX                                                          
         TM    INMIX1,MIXILST+MIXISEL+MIXIREP  NO-OK LIST/SELECT MODE           
         BZ    VALDPL96                              OR REPORT MODE             
         B     VALDPLX                                                          
*                                                                               
VALDPL90 MVC   FVMSGNO,=AL2(FVIDPT)     INVALID DAYPART                         
         B     VALDPLX                                                          
*                                                                               
VALDPL92 MVC   FVMSGNO,=AL2(FVISLN)     INVALID LENGTH                          
         B     VALDPLX                                                          
*                                                                               
VALDPL94 MVC   FVMSGNO,=AL2(FVNOSLN)    LENGTH MISSING                          
         B     VALDPLX                                                          
*                                                                               
VALDPL96 MVC   FVMSGNO,=AL2(FVNODPT)    DAYPART MISSING                         
*                                                                               
VALDPLX  B     ROUTSX                                                           
         SPACE 2                                                                
**SLNTAB   DS    0XL1                                                           
**       ++INCLUDE SPSLNTAB                                                     
**       DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DAYPART CODE                                         *         
*                                                                     *         
* NTRY - R1=A(1-BYTE DAYPART FIELD)                                   *         
*                                                                     *         
* EXIT - CC=EQUAL IF DAYPART IS VALID WITH DAYPART CODE SET           *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
LGETDPT  NTR1  ,                   LOCAL ENTRY POINT                            
         MVI   RFLAG,1                                                          
         B     GETDPT1                                                          
*                                                                               
GETDPT   MVI   RFLAG,0                                                          
*                                                                               
GETDPT1  MVC   RBYTE,0(R1)                                                      
         CLC   BDPT,RBYTE          TEST FOR CHANGE IN DAYPART                   
         BE    GETDPTX                                                          
         XC    RPARM,RPARM         YES - READ DAYPART RECORD                    
         MVC   RPARM(2),CUAALF                                                  
         MVC   RPARM+2(1),QMED                                                  
         MVC   RPARM+3(1),ESTDMENU                                              
         GOTO1 VDPTRD,RPARM,,RWORK4,VDMGR                                       
         CLI   RPARM+8,X'FF'                                                    
         BE    GETDPT9                                                          
         LA    RE,RWORK4                                                        
*                                                                               
GETDPT2  CLI   0(RE),0             FIND THE DAYPART CODE                        
         BE    GETDPT9                                                          
         CLC   0(1,RE),RBYTE                                                    
         BE    *+12                FOUND - VALID DAYPART                        
         LA    RE,5(RE)                                                         
         B     GETDPT2                                                          
*                                                                               
         MVC   RWORK(L'QDPT),2(RE)                                              
         XC    DPTSUBS,DPTSUBS                                                  
         MVI   DPTTYPE,C'R'        REGULAR DAYPART                              
         ZIC   R1,1(RE)                                                         
         SRL   R1,4                                                             
         LTR   R1,R1               TEST ANY SUB-DAYPARTS                        
         BZ    GETDPT8                                                          
         MVI   DPTTYPE,C'M'        MASTER DAYPART                               
         LA    R0,L'DPTSUBS                                                     
         LA    R5,DPTSUBS          YES - BUILD LIST OF SUB-DAYPARTS             
         LA    RE,RWORK4                                                        
         SR    RF,RF                                                            
*                                                                               
GETDPT4  CLI   0(RE),0                                                          
         BE    GETDPT8                                                          
         IC    RF,1(RE)                                                         
         SRL   RF,4                                                             
         CR    RF,R1               TEST THIS IS A SUB-DAYPART                   
         BNE   GETDPT6                                                          
         CLC   RBYTE,0(RE)         YES - TEST THIS IS OUR DAYPART               
         BNE   GETDPT5                                                          
         CLI   DPTSUBS,0           YES - TEST ANY SUBDAYPARTS BEFORE            
         BE    GETDPT6                                                          
         MVI   DPTTYPE,C'S'        YES - DPT IS NOT MASTER DPT, SO              
         MVC   DPTMAS,DPTSUBS            SET MASTER DAYPART AND                 
         XC    DPTSUBS,DPTSUBS           DPT TYPE IS SUB AND NO                 
         B     GETDPT8                   SUB-DAYPARTS                           
*                                                                               
GETDPT5  MVC   0(1,R5),0(RE)                                                    
         LA    R5,1(R5)                                                         
         BCT   R0,GETDPT6                                                       
         B     GETDPT8                                                          
*                                                                               
GETDPT6  LA    RE,5(RE)                                                         
         B     GETDPT4                                                          
*                                                                               
GETDPT8  XC    BDPT(G1WPROF-BDPT),BDPT                                          
         MVC   BDPT,RBYTE                                                       
         XC    QDPT(QVALSX-QDPT),QDPT                                           
         MVC   QDPT,RWORK                                                       
         B     GETDPTX                                                          
*                                                                               
GETDPT9  MVC   FVMSGNO,=AL2(FVIDPT)                                             
*                                                                               
GETDPTX  CLI   RFLAG,1                                                          
         BE    EXIT                                                             
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PASSWORD                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALPWD   OC    BYRPW,BYRPW                                                      
         BZ    VALPWDX                                                          
         CLC   =C'TM',CUAALF                                                    
         BE    *+12                                                             
         TM    CUSTAT,CUSDDS       IGNORE IF DDS TERMINAL                       
         BO    VALPWDX                                                          
         OC    INFPWD,INFPWD                                                    
         BZ    VALPWD9                                                          
         CLC   INFPWD,BYRPW                                                     
         BNE   VALPWD8                                                          
         B     VALPWDX                                                          
*                                                                               
VALPWD8  MVC   FVMSGNO,=AL2(FVIPWD)    INVALID PASSWORD                         
         B     *+10                                                             
VALPWD9  MVC   FVMSGNO,=AL2(FVNOPWD)   ENTER PASSWORD                           
         LA    R1,BWSPWDH                                                       
         ST    R1,FVADDR                                                        
*                                                                               
VALPWDX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE ONE OR TWO DATES                                *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF DATE FIELD)                             *         
*                                                                     *         
* EXIT - APWORK+0(6)=FIRST DATE  (IF VALID)                           *         
*        APWORK+6(6)=SECOND DATE (IF VALID)                           *         
*        CC=EQUAL IF DATE(S) VALID                                    *         
*        CC=NOT EQUAL IF DATE(S) INVALID WITH FVMSGNO SET             *         
***********************************************************************         
         SPACE 1                                                                
VALDAT   XC    APWORK(12),APWORK                                                
         GOTO1 AFVAL                                                            
         BNE   VALDATX                                                          
         GOTO1 VSCANNER,RPARM,FVIHDR,(3,RIO),C',=,-'                            
         CLI   4(R1),0                                                          
         BE    VALDAT9                                                          
         CLI   4(R1),2                                                          
         BH    VALDAT9                                                          
         LA    R2,RIO                                                           
         CLI   0(R2),0             TEST FIRST DATE GIVEN                        
         BE    VALDAT9                                                          
         GOTO1 VDATVAL,RPARM,(0,12(R2)),APWORK                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    VALDAT9                                                          
         CLC   3(1,R1),0(R2)                                                    
         BNE   VALDAT9                                                          
         CLI   1(R2),0             TEST SECOND DATE GIVEN                       
         BE    VALDATX                                                          
         MVI   FVINDX,2                                                         
         GOTO1 (RF),(R1),(0,22(R2)),APWORK+6                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    VALDAT9                                                          
         CLC   3(1,R1),1(R2)                                                    
         BNE   VALDAT9                                                          
         B     VALDATX                                                          
*                                                                               
VALDAT9  MVC   FVMSGNO,=AL2(FVIDAT)                                             
*                                                                               
VALDATX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE UPGRADE EXPRESSION                              *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF UPGRADE FIELD)                          *         
*        APFLAG X'80' BIT ON IF UPGRADE EXPRESSION ALLOWED            *         
*        APFLAG X'40' BIT ON IF SHARE BOOK EXPRESSION ALLOWED         *         
*        APFLAG X'20' BIT ON IF DAY/TIME OVERRIDE EXPRESSION ALLOWED  *         
*        APFLAG X'10' BIT ON IF PUT AVERAGING EXPRESSION ALLOWED      *         
*        APFLAG X'08' BIT ON IF SHR AVERAGING EXPRESSION ALLOWED      *         
*        APFLAG X'04' THRU X'01' NOT DEFINED (AS YET)                 *         
*                                                                     *         
* EXIT - APWORK+00(1)=UPGRADE FILE                                    *         
*        APWORK+01(8)=UPGRADE EXPRESSION                              *         
*        APWORK+09(2)=SHARE BOOK (BINARY ZEROES=LATEST)               *         
*        APWORK+11(1)=OVERRIDE DAY(S)                                 *         
*        APWORK+12(4)=OVERRIDE TIME(S)                                *         
*        APWORK+16(1)=PUT AVERAGING VALUE (CHARACTER 1 OR 2)          *         
*        APWORK+17(1)=SHR AVERAGING VALUE (CHARACTER 1 OR 2)          *         
*        APWORK+18(6)=SHARE BOOK LIST (MAX 3)                         *         
*        APFLAG INDICATES WHICH UPGRADE FIELDS WERE INPUT AS ABOVE    *         
*        CC=EQUAL IF UPGRADE FIELD IS VALID                           *         
*        CC=NOT EQUAL IF INVALID WITH FVMSGNO/FVINDX SET              *         
***********************************************************************         
         SPACE 1                                                                
VALUPG   XC    APWORK(24),APWORK                                                
         GOTO1 AFVAL                                                            
         BNE   VALUPGX                                                          
*****                                                                           
         CLI   QMED,C'R'              RADIO?                                    
         BNE   VALUPG0                                                          
         MVC   FVMSGNO,=AL2(FVNOUPGR) UPGRADES NOT SUPPORTED FOR RADIO          
         B     VALUPGX                                                          
*****                                                                           
VALUPG0  CLC   FVIFLD(7),=C'UPT=BK/'  TEST FORMAT IS                            
         BNE   VALUPG1                UPT=BK/MYY(/MYY/MYY/MYY)                  
         ZIC   RE,FVILEN              YES-ALTER FORMAT TO                       
         AHI   RE,-1*5                UPT=IX/100,BK=MYY(/MYY/....)              
         BM    VALUPG99                                                         
         MVC   RWORK4(7),=C'IX/100,'                                            
         MVC   RWORK4+7(0),FVIFLD+4                                             
         EX    RE,*-6                                                           
         MVI   RWORK4+9,C'='                                                    
         LA    RE,7(RE)                                                         
         MVC   FVIFLD+4(0),RWORK4                                               
         EX    RE,*-6                                                           
         LA    RE,5(RE)                                                         
         STC   RE,FVILEN                                                        
         LA    RE,L'FVIHDR(RE)                                                  
         STC   RE,FVTLEN                                                        
*                                                                               
VALUPG1  GOTO1 VSCANNER,RPARM,(30,FVIHDR),RIO,C',=,='                           
         MVC   RBYTE,4(R1)         SAVE NUMBER OF SCANNER LINES                 
         MVI   RFLAG,0             FIELD INPUT INDICATORS                       
         CLI   RBYTE,0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALUPGX                                                          
         LA    R2,RIO              R2=A(SCANNER BLOCK)                          
         MVI   FVINDX,1                                                         
*                                                                               
VALUPG2  CLI   0(R2),0             TEST L'FIRST HALF OF ENTRY                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     VALUPGX                                                          
         CLI   0(R2),L'UPGNAME                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     VALUPGX                                                          
         CLI   1(R2),0             TEST L'SECOND HALF OF ENTRY                  
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALUPGX                                                          
         ZIC   R1,0(R2)                                                         
         BCTR  R1,0                R1=L'FIRST HALF OF INPUT-1                   
         LA    R3,UPGTAB                                                        
         USING UPGTABD,R3          R3=A(UPGRADE TABLE)                          
*                                                                               
VALUPG4  CLI   UPGNAME,0           TEST E-O-T                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKINV)                                            
         B     VALUPGX                                                          
         CLI   UPGSHRT,C' '        TEST ENTRY HAS SHORT KEYWORD                 
         BE    VALUPG6                                                          
         LA    RE,2                                                             
         CLI   UPGSHRT+2,C' '                                                   
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         CLI   UPGSHRT+1,C' '                                                   
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         CR    R1,RE               TEST INPUT LEN = SHORT NAME LEN              
         BNE   VALUPG6                                                          
         EX    R1,*+8              YES - MATCH ON SHORT NAME                    
         B     *+10                                                             
         CLC   UPGSHRT(0),12(R2)                                                
         BE    VALUPG10                                                         
*                                                                               
VALUPG6  EX    R1,*+8              MATCH ON LONG NAME                           
         B     *+10                                                             
         CLC   UPGNAME(0),12(R2)                                                
         BE    VALUPG10                                                         
*                                                                               
VALUPG8  LA    R3,UPGTABL(R3)      BUMP TO NEXT UPGRADE TABLE ENTRY             
         B     VALUPG4                                                          
*                                                                               
VALUPG10 MVC   RDUB(1),RFLAG       TEST KEYWORD INPUT PREVIOUSLY                
         NC    RDUB(1),UPGIND1                                                  
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFKDUP)                                            
         B     VALUPGX                                                          
         MVC   RDUB(1),APFLAG      TEST KEYWORD IS VALID                        
         NC    RDUB(1),UPGIND1                                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALUPGX                                                          
         XC    FVIHDR,FVIHDR                                                    
         ZIC   R1,1(R2)                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),22(R2)                                                 
         STC   R1,FVILEN                                                        
         LA    R1,L'FVIHDR(R1)                                                  
         STC   R1,FVTLEN                                                        
         OC    RFLAG,UPGIND1       SET THIS KEYWORD INPUT                       
         SR    RF,RF                                                            
         ICM   RF,3,UPGROUT                                                     
         LA    RF,T20700(RF)       RF=A(VALIDATION ROUTINE)                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         BASR  RE,RF                                                            
         BNE   VALUPGX                                                          
         ZIC   R1,FVINDX           BUMP TO NEXT SCANNER TABLE ENTRY             
         LA    R1,1(R1)                                                         
         CLM   R1,1,RBYTE                                                       
         BH    VALUPG12                                                         
         STC   R1,FVINDX                                                        
         LA    R2,22+30(R2)                                                     
         B     VALUPG2                                                          
*                                                                               
VALUPG12 MVC   APFLAG(1),RFLAG     RETURN FIELDS INPUT                          
         B     VALUPGX                                                          
*                                                                               
VALUPG99 MVC   FVMSGNO,=AL2(FVFNOTV)   INVALID                                  
*                                                                               
VALUPGX  B     ROUTSX                                                           
         SPACE 2                                                                
UPGTAB   DS    0H               ** UPGRADE VALIDATION TABLE **                  
         DC    C'PUT     ',C'   ',X'1000',AL2(VALUPA-T20700)                    
         DC    C'SHR     ',C'   ',X'0800',AL2(VALUPA-T20700)                    
         DC    C'RTG     ',C'   ',X'0800',AL2(VALUPA-T20700)                    
         DC    C'RP      ',C'   ',X'1800',AL2(VALUPA-T20700)                    
         DC    C'TUPGRADE',C'UPT',X'8000',AL2(VALUPE-T20700)                    
         DC    C'PUPGRADE',C'UPP',X'8000',AL2(VALUPE-T20700)                    
         DC    C'BOOK    ',C'BK ',X'4000',AL2(VALUPB-T20700)                    
         DC    C'DAYTIME ',C'DT ',X'2000',AL2(VALUPD-T20700)                    
UPGTABX  DC    AL1(EOT)                                                         
*                                                                               
UPGTABD  DSECT                                                                  
UPGNAME  DS    CL8                 KEYWORD NAME                                 
UPGSHRT  DS    CL3                 SHORT KEYWORD NAME                           
UPGIND1  DS    XL1                 INDICATORS                                   
UPGIND2  DS    XL1                 INDICATORS                                   
UPGROUT  DS    AL2                 DISPLACEMENT OF VALIDATION ROUTINE           
UPGTABL  EQU   *-UPGTABD                                                        
*                                                                               
T20700   CSECT                                                                  
         DS    0H                                                               
         EJECT                                                                  
VALUPE   LR    R0,RE               ** VALIDATE UPGRADE EXPRESSION **            
         MVC   APWORK(1),UPGSHRT+2 SET FILE INDICATOR                           
         GOTO1 VUPVAL,RPARM,FVIHDR,APWORK+32,(C'/',ACOM)                        
         MVC   APWORK+1(8),APWORK+36  UPGRADE EXPRSSN FROM UPGRADE ELEM         
         CLI   0(R1),0                                                          
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(FVIUPG)                                             
         B     VALUPX                                                           
         SPACE 1                                                                
VALUPB   LR    R0,RE               ** VALIDATE OVERRIDE BOOK **                 
         ZIC   RF,FVILEN                                                        
         LA    R1,FVIFLD                                                        
VALUPB2  CLI   0(R1),C'/'          ALTER SLASHES TO COMMAS IF THEY              
         BNE   VALUPB4             SEPARATE BOOKS                               
         CLI   1(R1),C'0'                                                       
         BNL   VALUPB4                                                          
         MVI   0(R1),C','                                                       
VALUPB4  LA    R1,1(R1)                                                         
         BCT   RF,VALUPB2                                                       
*                                                                               
         MVI   QBOOKTYP,0                                                       
         L     R4,ACPARMA                                                       
         L     R4,16(R4)                                                        
         XC    RWORK+16(16),RWORK+16                                            
         GOTO1 VBOOKVAL,RPARM,(C'N',FVIHDR),(4,RWORK),(C'B',VSCANNER), X        
               RWORK+16,(C'C',(R4))                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,4(R1)          RF=N'BOOKS                                   
         BZ    VALUPB9                                                          
         TM    RWORK,X'BF'         TEST FOR FUNNY BOOK FORMATS                  
         BNZ   VALUPB9                                                          
         MVC   APWORK+9(2),RWORK+1                                              
         MVC   QBOOKTYP,RWORK+16                                                
*                                                                               
***   GOING TO TRY TO FIND THE 2 CHARACTER BOOKTYPE FROM QBOOKTYP               
         CLI   QBOOKTYP,0                                                       
         BE    VALUPB5                                                          
         LR    R2,RF               SAVE OFF RF                                  
         LR    R3,R1               SAVE OFF R1                                  
*                                                                               
**       OC    APWORK+64(12),APWORK+64                                          
**       BZ    *+6                                                              
**       DC    H'0'                SAFETY CODE                                  
         MVC   APWORK+64(12),APPARM   SAVE OFF APPARM                           
         GOTO1 AGETBKTY,APPARM,(C'A',0),RFULL,QBOOKTYP                          
****  DIES IN GEGEN00 AT 1DBC IF I DON'T SAVE OFF APPARM                        
         MVC   APPARM(12),APWORK+64   RESTORE APPARM                            
         XC    APWORK+64(12),APWORK+64                                          
*&&DO                                                                           
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL GLOBBER                                 
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),ACPARM,SPBOOKTB   GET A(BOOK TABLE)                         
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R4,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
VALUPB4A DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   QBOOKTYP,SPBKTYPN   R4 HAS THE ALPHANUMERIC                      
         BE    *+10                                                             
         AR    RF,R4                                                            
         B     VALUPB4A                                                         
         MVC   RFULL(2),SPBKTYPA   BOOK TYPE                                    
         DROP  RF                                                               
*&&                                                                             
*                                                                               
         LR    RF,R2                                                            
         LR    R1,R3                                                            
***   GOING TO TRY TO FIND THE 2 CHARACTER BOOKTYPE FROM QBOOKTYP               
*                                                                               
         LA    RE,BKTYPTB1         (CAN'T ADDRESS BKTYPTAB)                     
VALUPB4C CLI   0(RE),0                                                          
         BE    VALUPB4E            NOT IN TABLE!  TRY DEMTABS                   
         CLC   RWORK+16(1),1(RE)                                                
         BE    *+12                                                             
         LA    RE,L'BKTYPTB1(RE)                                                
         B     VALUPB4C                                                         
         OC    APWORK+9+1(1),0(RE) SET ON IN MONTH                              
         B     VALUPB5                                                          
*                                                                               
VALUPB4E DS    0H                                                               
         LR    R2,RF               SAVE OFF RF                                  
         LR    R3,R1               SAVE OFF R1                                  
*                                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL GLOBBER                                 
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),ACPARM,SPBOOKTB   GET A(BOOK TABLE)                         
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R4,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
VALUPB4G DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RFULL(2),SPBKTYPA                                                
         BE    *+10                                                             
         AR    RF,R4                                                            
         B     VALUPB4G                                                         
         OC    APWORK+9(2),APWORK+9   ANYTHING HERE?                            
         BZ    VALUPB5              - NOPE                                      
         OI    APWORK+9+1,BTY2CHAR   WE ARE DOING 2 CHARACTERS + L              
         MVC   QBOOKTYP,SPBKTYPN                                                
         DROP  RF                                                               
*                                                                               
         LR    RF,R2               RESTORE RF                                   
         LR    R1,R3               RESTORE R1                                   
*                                                                               
VALUPB5  CLI   4(R1),1             TEST 2 OR MORE BOOKS                         
         BNH   VALUPX                                                           
         CLI   4(R1),4                                                          
         BH    VALUPB9                                                          
         BCTR  RF,0                                                             
         LA    R1,APWORK+18                                                     
         LA    RE,RWORK+3                                                       
         LA    R2,RWORK+17                                                      
VALUPB6  TM    0(RE),X'BE'                                                      
         BNZ   VALUPB9                                                          
         MVC   0(2,R1),1(RE)                                                    
*                                  NO FREE REGISTER TO USE TABLE!               
         CLI   0(R2),C'O'          OLYMPIC BOOK?                                
         BNE   *+8                                                              
         OI    1(R1),BTYOLYMQ      YES - SET ON                                 
         CLI   0(R2),C'A'          PARENT ONLY DATA?                            
         BNE   *+8                                                              
         OI    1(R1),BTYPRNTQ      YES - SET ON                                 
         CLI   0(R2),C'B'          BLACK?                                       
         BNE   *+8                                                              
         OI    1(R1),BTYBLAKQ                                                   
         CLI   0(R2),C'H'          HISPANIC?                                    
         BNE   *+8                                                              
         OI    1(R1),BTYHISPQ                                                   
         CLI   0(R2),C'T'          TRADE?                                       
         BNE   *+8                                                              
         OI    1(R1),BTYTRADQ                                                   
         CLI   0(R2),C'P'          PEOPLE METER?                                
         BNE   *+8                                                              
         OI    1(R1),BTYPEOPQ                                                   
         CLI   0(R2),C'M'          METRO?                                       
         BNE   *+8                                                              
         OI    1(R1),BTYMTROQ                                                   
         CLI   0(R2),C'D'          DMA?                                         
         BNE   *+8                                                              
         OI    1(R1),BTYDMAQ                                                    
         CLI   0(R2),C'E'          OTHER?                                       
         BNE   *+8                                                              
         OI    1(R1),BTYOTHRQ                                                   
*                                                                               
         LA    R1,2(R1)                                                         
         LA    RE,3(RE)                                                         
         LA    R2,1(R2)                                                         
         BCT   RF,VALUPB6                                                       
         B     VALUPX                                                           
VALUPB9  MVC   FVMSGNO,=AL2(FVIBOOK)                                            
         B     VALUPX                                                           
         SPACE 1                                                                
VALUPD   LR    R0,RE               ** VALIDATE DAY/TIME OVERRIDE **             
         GOTO1 VSCANNER,RPARM,FVIHDR,(2,RWORK),C',=,/'                          
         CLI   4(R1),1                                                          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALUPX                                                           
         GOTO1 VDAYPAK,RPARM,(RWORK,RWORK+12),RDUB,RDUB+1                       
         CLI   RDUB,0                                                           
         BE    VALUPD2                                                          
         MVC   APWORK+11(1),RDUB                                                
         GOTO1 VTIMVAL,RPARM,(RWORK+1,RWORK+22),RDUB                            
         CLI   0(R1),X'FF'                                                      
         BE    VALUPD4                                                          
         MVC   APWORK+12(4),RDUB                                                
         B     VALUPX                                                           
VALUPD2  MVC   FVMSGNO,=AL2(FVIDAY)                                             
         B     VALUPX                                                           
VALUPD4  MVC   FVMSGNO,=AL2(FVITIME)                                            
         B     VALUPX                                                           
         SPACE 1                                                                
VALUPA   LR    R0,RE               ** VALIDATE PUT/SHR AVERAGING **             
         CLI   FVILEN,1                                                         
         BNE   VALUPA2                                                          
         CLI   FVIFLD,C'1'                                                      
         BE    *+12                                                             
         CLI   FVIFLD,C'2'                                                      
         BNE   VALUPA2                                                          
         TM    UPGIND1,X'10'                                                    
         BZ    *+10                                                             
         MVC   APWORK+16(1),FVIFLD                                              
         TM    UPGIND1,X'08'                                                    
         BZ    *+10                                                             
         MVC   APWORK+17(1),FVIFLD                                              
         B     VALUPX                                                           
VALUPA2  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALUPX                                                           
         SPACE 1                                                                
VALUPX   LR    RE,R0                                                            
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         BR    RE                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE PROGRAM ADJACENCY CODE                          *         
*                                                                     *         
* NTRY - R1 = A(FIELD TO BE VALIDATED)                                *         
*                                                                     *         
* EXIT - CC=EQ IF ADJACENCY CODE VALID AND QADJCD IS SET              *         
*        CC=NE IF ADJACENCY CODE INVALID                              *         
***********************************************************************         
         SPACE 1                                                                
VALADJ   CLI   CLTPROF+9,C'0'      TEST CLIENT ALLOWED ADJACENCY CODE           
         BE    VALADJ9                                                          
         CLI   CLTPROF+9,C'1'      TEST ALPHA REQUIRED                          
         BE    VALADJ2             YES                                          
         CLI   0(R1),C'0'          NO - TEST VALID NUMERIC                      
         BL    VALADJ9                                                          
         CLI   0(R1),C'9'                                                       
         BH    VALADJ9                                                          
         PACK  QADJCD,0(1,R1)                                                   
         NI    QADJCD,X'F0'                                                     
         CLI   1(R1),C' '                                                       
         BNH   VALADJX                                                          
         CLI   1(R1),C'0'                                                       
         BL    VALADJ9                                                          
         CLI   1(R1),C'9'                                                       
         BH    VALADJ9                                                          
         CLI   2(R1),C' '                                                       
         BH    VALADJ9                                                          
         MVC   RBYTE,1(R1)                                                      
         NI    RBYTE,X'0F'                                                      
         OC    QADJCD,RBYTE                                                     
         B     VALADJX                                                          
*                                                                               
VALADJ2  MVC   QADJCD,0(R1)        TEST VALID ALPHA                             
         CLI   1(R1),C' '                                                       
         BH    VALADJ9                                                          
         LA    R0,15               SCAN CLIENT AJ PROFILE FOR VALIDITY          
         LA    R1,CLTAJPRO+1                                                    
         CLI   CLTAJPRO,C'I'       TEST INCLUDE/EXCLUDE LIST                    
         BNE   VALADJ6                                                          
*                                                                               
VALADJ4  CLI   0(R1),C'*'                                                       
         BE    *+14                                                             
         CLC   QADJCD,0(R1)        MATCH I FILTER IS GOOD                       
         BE    VALADJX                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VALADJ4                                                       
         B     VALADJ9                                                          
*                                                                               
VALADJ6  CLI   0(R1),C'*'                                                       
         BE    *+14                                                             
         CLC   QADJCD,0(R1)        MATCH X FILTER IS BAD                        
         BE    VALADJ9                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VALADJ6                                                       
         B     VALADJX                                                          
*                                                                               
VALADJ9  MVC   FVMSGNO,=AL2(FVIADJ)   INVALID ADJACENCY CODE                    
*                                                                               
VALADJX  B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* MINIO INTERFACE ROUTINE                                             *         
*                                                                     *         
* NTRY - R1=I/O CONTROL BYTE (LOW ORDER BYTE)                         *         
*           BITS 0-1 - 1=RETURN KEYS AND ELEMENT CLUSTER IN IOAREA1   *         
*                      2=RETURN KEYS AND ELEMENT CLUSTER IN IOAREA2   *         
*                      3=RETURN KEYS AND ELEMENT CLUSTER IN IOAREA3   *         
*           BITS 2-7 - MINIO COMMAND                                  *         
*        IOKEY CONTAINS MASTER KEY FOLLOWED BY ELEMENT KEY            *         
*                                                                     *         
* EXIT - CC=EQ IF NO ERRORS                                           *         
*        CC=NE IF COMMAND IS HI,SEQ OR READ AND THERE IS AN ERROR     *         
*              FVMSGNO SET TO FVFIOER                                 *         
***********************************************************************         
         SPACE 1                                                                
MINIO    STC   R1,RBYTE                                                         
         XC    RFULL,RFULL                                                      
         ZIC   RE,RBYTE                                                         
         SRL   RE,6                                                             
         LTR   RE,RE                                                            
         BZ    MIN1                                                             
         BCTR  RE,0                                                             
         SLL   RE,2                                                             
         LA    RE,AIOAREA1(RE)                                                  
         L     RE,0(RE)                                                         
         ST    RE,RFULL            RFULL=A(IO AREA)                             
*                                                                               
MIN1     ZIC   RE,RBYTE                                                         
         SLL   RE,26                                                            
         SRL   RE,26                                                            
         STC   RE,RBYTE            RBYTE=MINIO COMMAND EQUATE                   
*                                                                               
         L     R2,AMINBLK                                                       
         USING MINBLKD,R2                                                       
         CLI   MINFKLEN,0          TEST BLOCK INITIALIZED                       
         BNE   MIN2                YES                                          
         CLI   RBYTE,MINCLS        NO-TEST COMMAND=CLOSE                        
         BE    MINX                   YES-EXIT                                  
         IC    R0,MINBF2                                                        
         LR    RE,R2               INITIALIZE THE BLOCK                         
         LA    RF,MINBLKL                                                       
         XCEF                                                                   
         STC   R0,MINBF2                                                        
         MVC   MINFIL,=CL8'SPTFIL'                                              
         MVC   MINDIR,=CL8'SPTDIR'                                              
         MVI   MINFKLEN,13                                                      
         MVI   MINNCTL,1                                                        
         MVC   MINFRCLM,=H'1976'                                                
         MVI   MINEKLEN,L'BWDKEL                                                
         MVI   MINEKDSP,L'BWDKEY-L'BWDKEL                                       
         MVC   MINBUFF,AMINBUFF                                                 
         MVC   MINRTAB,AMINRTAB                                                 
         MVC   MINCOMF,ACOM                                                     
         MVC   MINRECUP,VRECUP                                                  
         LA    RE,L'MINRECTB                                                    
         MHI   RE,MINRTMAX                                                      
         STH   RE,MINRTABL                                                      
         MVC   MINMAXEL,=H'1976'                                                
         MVI   MINNBUF,MINNBUFF                                                 
         MVI   MINNKLO,2                                                        
         MVI   MINNKHI,99                                                       
         MVC   MINAGYC,CUAALF                                                   
         MVI   MINAGYD,20                                                       
*                                                                               
MIN2     LA    R1,IOKEY                                                         
         CLI   RBYTE,MINADD                                                     
         BL    *+12                                                             
         ICM   R1,15,RFULL                                                      
         BZ    MIN3                                                             
         LA    RE,BWDKEL-BWDKEY                                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MINMKEY(0),0(R1)    SET MASTER KEY                               
         LA    RE,L'BWDKEL                                                      
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MINEKEY(0),L'BWDKEY-L'BWDKEL(R1)    SET ELEMENT KEY              
         L     RE,RFULL                                                         
         CLI   RBYTE,MINADD        TEST COMMAND=HI/SEQ/WRT                      
         BNL   *+10                                                             
         MVC   0(L'BWDKEY,RE),IOKEY   YES-SET THE RECORD KEY                    
         LA    RE,BWDEL-BWDKEY(RE)                                              
         ST    RE,MINELEM          SET A(CLUSTER)                               
*                                                                               
MIN3     ST    R2,RPARM                                                         
         MVC   RPARM(1),RBYTE                                                   
         CLI   RBYTE,MINHI                                                      
         BE    *+12                                                             
         CLI   RBYTE,MINSEQ                                                     
         BNE   *+10                                                             
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VMINIO,RPARM        ** CALL MINIO **                             
         CLI   MINERR,0                                                         
         BE    MIN8                                                             
         CLI   RBYTE,MINADD        ERROR-TEST HI/SEQ/RD                         
         BL    MIN3G                                                            
         CLI   MINERR,MINETOVF     RECORD TABLE OVERFLOW???                     
         BE    *+6                                                              
         DC    H'0'                NO-CATASTROPHE                               
         MVC   BWSMSG(45),MIN3MSG                                               
         OI    BWSMSGH+6,X'80'                                                  
         DC    H'0',C'$ABEND'      ALERT THE USER!                              
MIN3MSG  DC    C'Campaign/Market has grown out of proportion!!'                 
*****                                                                           
MIN3G    MVC   FVMSGNO,=AL2(FVFIOER)                                            
         CLI   MINERR,MINESNF      TEST RECORD SET NOT FOUND                    
         BE    MIN4                                                             
         CLI   MINERR,MINEEOF      OR END-OF-FILE                               
         BE    MIN4                                                             
         CLI   RBYTE,MINHI         NO-TEST HI/SEQ                               
         BE    *+12                                                             
         CLI   RBYTE,MINSEQ                                                     
         BNE   MIN4                                                             
         DC    H'0'                   YES-CATASTROPHE                           
*                                                                               
MIN4     LA    R1,MINERTAB                                                      
*                                                                               
MIN5     CLI   0(R1),0                                                          
         BE    MIN6                                                             
         CLC   MINERR,0(R1)                                                     
         BE    MIN6                                                             
         LA    R1,1(R1)                                                         
         B     MIN5                                                             
*                                                                               
MIN6     MVC   IODMCB+8(1),1(R1)                                                
         B     MINX                                                             
*                                                                               
MIN8     CLI   RBYTE,MINHI                                                      
         BE    *+12                                                             
         CLI   RBYTE,MINSEQ                                                     
         BNE   MINX                                                             
         ZIC   RE,MINEKLEN                                                      
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         ZIC   RF,MINEKDSP                                                      
         LA    RF,IOKEY(RF)                                                     
         L     R3,MINELEM                                                       
         MVC   0(1,RF),0(R3)                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),2(R3)                                                    
         L     R1,RFULL                                                         
         MVC   0(13,R1),IOKEY                                                   
*                                                                               
         CLC   =X'0D68',0(R1)      NWS DETAIL RECORD?                           
         BNE   MINX                                                             
         SR    R0,R0               MODIFY DEMO PRECISION HERE                   
         LA    R1,24(R1)                                                        
MIN9     CLI   0(R1),0             ANY DEMO ELEMENT?                            
         BE    MINX                NONE                                         
         CLI   0(R1),DMOELCDQ                                                   
         BE    MIN9A                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MIN9                                                             
*                                                                               
MIN9A    ICM   R1,8,=C'N'          WE'RE DOING NWS DEMO                         
         BRAS  RE,ADJDEMEL         LET'S SEE IF WE NEED TO ADJUST               
***      ICM   R1,8,=X'00'         CLEAR OUT THE =C'N'                          
         LA    R1,0(R1)            CLEAR OUT THE =C'N'                          
*                                                                               
MINX     MVC   RIOSAVE,IOAREA      SO THAT CURRENT IO AREA PRESERVED            
         B     ROUTSX                                                           
         SPACE 2                                                                
MINERTAB DC    AL1(MINEEOF),X'80'                                               
         DC    AL1(MINEDUP),X'20'                                               
         DC    AL1(MINERNF),X'10'                                               
         DC    AL1(MINESNF),X'10'                                               
         DC    AL1(0),X'40'                                                     
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL SPCLERR                                                       
***********************************************************************         
SPCLERR  L     RF,=A(SPECLERR)                                                  
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM,RWRKD,,0                                             
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET GOAL DOLLARS AND POINTS                              *         
***********************************************************************         
         SPACE 1                                                                
GETGOAL  L     RF,=A(GETG)         REGULAR GETGOAL ROUTINE                      
         A     RF,ACRELO                                                        
         GOTO1 (RF),ACPARM,RWRKD                                                
         B     ROUTSX                                                           
*                                                                               
GETGOAL2 L     RF,=A(GETG)         SPECIAL CALL TO IGNORE BWS GOAL RECS         
         A     RF,ACRELO                                                        
         GOTO1 (RF),ACPARM,(X'80',RWRKD)                                        
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD A BWS DETAIL RECORD FROM AN NSID RECORD            *         
* IF TWAFLAG IS SET TO TWANOHDR (NO HEADER RECORD) AND THE CAMPAIGN/  *         
* MARKET SEQUENCE NUMBER (BCMSEQ) IS NOT SET, THEN THIS ROUTINE WILL  *         
* SET IT TO THE NEXT AVAILABLE SEQUNCE NUMBER.                        *         
* INPUT  : APRECKEY                                                   *         
*          IOAREA1 CONTAINS HEADER RECORD (IF IT EXISTS)              *         
* OUTPUT : IOAREA2 CONTAINS BWS RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
DTLBLD   LA    R4,APRECKEY                                                      
         USING NSIDKEYD,R4                                                      
         LA    R3,RIO2                                                          
         USING SRBLKD,R3                                                        
         LA    RF,SRBLKLN          CLEAR RANSID BLOCK                           
         XCEF  SRBLK,(RF)                                                       
         LA    R1,RIO                                                           
         ST    R1,SRASIR                                                        
         MVC   SRACOM,ACOM                                                      
         MVC   SRACLPAC,VCLPACK                                                 
         MVC   SRAMSUNP,VMSUNPK                                                 
         MVC   SRADYUNP,VDAYUNPK                                                
         MVC   SRAUNTIM,VUNTIME                                                 
         MVC   SRSELSCH,CMPSCHEM                                                
         MVC   SRSELAM,BAGYMD                                                   
         MVC   SRSELAGY,CUAALF                                                  
         MVC   SRSELMED,QMED                                                    
         MVC   SRSELSLN,NSSLN                                                   
         MVC   SRSELPER,NSPER                                                   
         MVC   SRSELYR,NSYEAR                                                   
         MVC   SRSELMKT,NSMKT                                                   
         MVC   SRSELSTA,NSSTA                                                   
         MVC   SRSELDPT(1),NSDPT                                                
         MVC   SRSELPRG(1),NSPRG                                                
         MVC   SRSELDAY,NSDAY                                                   
         MVC   SRSELTIM,NSTIME                                                  
         OC    SRSELTIM+2(2),SRSELTIM+2                                         
         BNZ   *+10                                                             
         MVC   SRSELTIM+2(2),SRSELTIM                                           
*                                                                               
*****    MVC   SRSELCTY,APROF7                                                  
         MVI   SRSELCTY,C'U'                                                    
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BZ    *+8                                                              
         MVI   SRSELCTY,C'C'                                                    
*                                                                               
DBLD2    GOTO1 VRANSID,RPARM,SRBLK      READ THE DETAIL RECORD                  
         CLI   SRERROR,SRNOERR                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   SRMODE,SRONEREC                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SRACTDAY,NSDAY                                                   
         BNE   DBLD2                                                            
         CLC   SRACTTIM,NSTIME                                                  
         BNE   DBLD2                                                            
         DROP  R4                                                               
*                                                                               
         TM    TWAFLAG,TWANOHDR    TEST NO HEADER RECORD                        
         BZ    DBLD4                                                            
         OC    BCMSEQ,BCMSEQ       YES-TEST CAMPAIGN/MARKET SEQ NO SET          
         BNZ   DBLD4                                                            
         MVI   BSTACD,1            NO-SET STATION CODE TO 1                     
         XC    IOKEY,IOKEY            AND READ PASSIVE POINTERS TO GET          
         LA    R2,IOKEY               NEXT CAMPAIGN/MARKET SEQ NO               
         USING BWHRECD,R2                                                       
         MVI   BWHPTYP,BWHPTYPQ                                                 
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,BAGYMD                                                  
         OC    BWHPAGMD,BBYRMASK                                                
         MVC   BWHPBYR,BBYR                                                     
         GOTO1 AIO,DIRHID                                                       
         BNE   DBLDX                                                            
         MVC   BCMSEQ,XFF                                                       
         CLC   IOKEY(BWHPSEQ-BWHPKEY),IOKEYSAV  TEST FIRST CAMPAIGN             
         BNE   DBLD10                           FOR BUYER                       
         SR    RE,RE               NO-NEXT SEQ NO                               
         ICM   RE,3,BWHPSEQ                                                     
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BZ    DBLD99                                                           
         STCM  RE,3,BCMSEQ                                                      
         B     DBLD10                                                           
*                                                                               
DBLD4    OC    BSTA,BSTA           TEST STATION IN THE KEY                      
         BNZ   *+12                                                             
         NI    TWAFLAG,255-TWANOSTA   NO-RESET 'NO STATION' FLAG                
         B     DBLD5                                                            
         TM    TWAFLAG,TWANOSTA    YES-TEST STATION NOT IN HEADER               
         BZ    DBLD10                                                           
         CLI   BSTACD,0            YES-TEST STATION CODE SET YET                
         BNE   DBLD10                  YES                                      
*                                                                               
DBLD5    L     R2,AIOAREA1         GET STATION CODE                             
         USING BWHRECD,R2                                                       
         LA    R1,BWHFSTEL                                                      
         SR    R0,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVC   APDUB,SRERSTAN                                                   
         CLI   APDUB+4,C' '                                                     
         BNE   *+8                                                              
         MVI   APDUB+4,C'T'        SET MEDIA TO T                               
         CLI   APDUB,C'0'                                                       
         BNL   DBLD6                                                            
         MVC   APDUB+5(3),RSPACES                                               
*                                                                               
DBLD6    CLI   0(R1),0                                                          
         BE    DBLD8                                                            
         CLI   0(R1),BWHELCDQ                                                   
         BNE   DBLD7                                                            
         USING BWHEL,R1                                                         
         IC    RE,BWHSEQ                                                        
         CR    RE,RF                                                            
         BNH   *+6                                                              
         LR    RF,RE                                                            
         CLC   BWHSTA,APDUB                                                     
         BNE   DBLD7                                                            
         OC    BSTA,BSTA           STATION MATCH-TEST STATION IN KEY            
         BZ    *+6                                                              
         DC    H'0'                YES-DEATH                                    
         STC   RE,BSTACD           NO-SAVE STATION CODE                         
         B     DBLD10                                                           
*                                                                               
DBLD7    IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DBLD6                                                            
*                                                                               
DBLD8    LA    RF,1(RF)            SET STATION CODE                             
         STC   RF,BSTACD                                                        
         OI    TWAFLAG,TWANOSTA    MAKE SURE 'NO STATION' FLAG ON               
         XC    APELEM,APELEM                                                    
         LA    R1,APELEM           ADD STATION ELEMENT TO HEADER                
         MVI   BWHELCD,BWHELCDQ                                                 
         MVI   BWHELLN,BWHELLNQ                                                 
         STC   RF,BWHSEQ                                                        
         MVC   BWHSTA,APDUB                                                     
         GOTO1 AADDELS,BWHRECD                                                  
*                                                                               
         CLC   FVMSGNO,=AL2(FVFOK) RECORD OVERFLOW?                             
         BNE   DBLDX               YES, NO GOOD                                 
         DROP  R1,R3                                                            
*                                                                               
DBLD10   L     R3,AIOAREA2         BUILD BWS DETAIL RECORD                      
         USING BWDRECD,R3                                                       
         LA    R5,RIO2                                                          
         USING SRBLKD,R5                                                        
         XC    BWDKEY(256),BWDKEY                                               
         MVI   BWDKTYP,BWDKTYPQ                                                 
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BAGYMD                                                  
         OC    BWDKAGMD,BBYRMASK                                                
         MVC   BWDKBYR,BBYR                                                     
         MVC   BWDKSEQ,BCMSEQ                                                   
         MVI   BWDKELCD,BWDELCDQ                                                
         MVC   BWDKELST,BSTACD                                                  
         MVI   BWDKELPO,0                                                       
         MVC   BWDKELDY,SRACTDAY                                                
         MVC   RFULL,SRACTTIM                                                   
         GOTO1 LPACKTIM,RFULL      GET PACKED TIMES                             
         MVC   BWDKELTM,PTIMES                                                  
         MVI   BWDKELSQ,1          ASSUME SEQ NO = 1                            
         LA    RE,BWDEL+BWDELLNQ+1-BWDRECD                                      
         STCM  RE,3,BWDLEN                                                      
*                                                                               
         XC    DTLKEY,DTLKEY       MAKE SURE DETAIL KEY IS SET                  
         MVC   DTLKEY(BWDKELST-BWDKEY),BWDKEY                                   
*                                                                               
         MVI   BWDELCD,BWDELCDQ    BUILD DESCRIPTION ELEMENT                    
         MVI   BWDELLN,BWDELLNQ                                                 
         MVC   BWDSTACD,BWDKELST                                                
         MVI   BWDPKOR,0                                                        
         MVC   BWDDAYS,BWDKELDY                                                 
         MVC   BWDTIMCD,BWDKELTM                                                
         MVI   BWDSEQ,1                                                         
         MVC   BWDTIMES,SRACTTIM                                                
         MVC   BWDDPT,BDPT                                                      
         CLI   BDPT,0              TEST SINGLE DAYPART REQUEST                  
         BE    DBLD12                                                           
         CLC   SRACTDPT,BDPT       YES-TEST DAYPARTS MATCH                      
         BE    DBLD14              YES                                          
         MVC   BWDSUBDP,SRACTDPT   NO-SET SUB-DAYPART                           
         B     DBLD14                                                           
*                                                                               
DBLD12   MVC   BWDDPT,SRACTDPT     MULTI-DAYPART REQUEST --                     
         CLI   CMPDPOPT,C'M'       TEST SUB DAYPARTS UNDER MASTER               
         BNE   DBLD14              NO                                           
         MVC   RHALF,BDPT          YES-GET DAYPART                              
         GOTO1 LGETDPT,SRACTDPT                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDPT(2),RHALF                                                    
         CLI   DPTTYPE,C'S'        TEST THIS IS A SUB DAYPART                   
         BNE   DBLD14              NO                                           
         MVC   BWDDPT,DPTMAS       YES-SET DAYPART TO THE MASTER                
         MVC   BWDSUBDP,SRACTDPT       AND SET SUB-DAYPART                      
*                                                                               
DBLD14   MVC   BWDSLN,SRSELSLN     SPOT LENGTH                                  
         CLI   BWDSLN,0            TEST LENGTH = 0                              
         BNE   DBLD16                                                           
         MVC   BWDSLN,BSLN         YES-SET TO REQUESTED LENGTH                  
         CLI   BWDSLN,0            TEST REQUESTED LENGTH = 0                    
         BNE   DBLD16                                                           
         MVI   BWDSLN,30           YES-SET SLN=30                               
*                                                                               
DBLD16   MVC   BWDSTA,SRERSTAN     STATION                                      
         CLI   BWDSTA+4,C' '                                                    
         BNE   *+8                                                              
         MVI   BWDSTA+4,C'T'                                                    
         CLI   BWDSTA,C'0'                                                      
         BNL   *+10                                                             
         MVC   BWDSTA+5(3),RSPACES                                              
         MVC   BWDEFDT2,SRACTEF2   EFFECTIVE DATES                              
         MVC   BWDEFDT3,SRACTEF3                                                
*                                                                               
         TM    INOXFI,INOXFINC     TEST FOR NO COST TRANSFER                    
         BO    DBLD36                                                           
         MVC   BWDCOST1,SRACTCS1                                                
         MVC   BWDCOST2,SRACTCS2                                                
         MVC   BWDCOST3,SRACTCS3                                                
*                                                                               
         CLC   SRSELSLN,SRACTSLN   TEST GOT REQUIRED SPOT LENGTH                
         BE    DBLD24                                                           
         CLI   SRACTSLN,0          NO-TEST SPOT LENGTH = 0                      
         BE    DBLD24                                                           
         CLI   BSLN,0              NO-TEST SPOT LENGTH FILTER                   
         BE    DBLD18              NO                                           
         CLI   CLTBWPRO+13,C'N'    YES-TEST INHIBIT ADJUST FOR LEN < 30         
         BNE   DBLD18              NO                                           
         CLI   SRSELSLN,30         YES-TEST SPOT LENGTH GE 30                   
         BL    DBLD24                                                           
*                                                                               
DBLD18   LA    R2,RWORK            THEN EQUIVALENCE THE COSTS                   
         MVC   0(4,R2),BWDCOST1                                                 
         MVC   4(4,R2),BWDCOST2                                                 
         MVC   8(4,R2),BWDCOST3                                                 
         LA    R4,3                                                             
*                                                                               
DBLD20   OC    0(4,R2),0(R2)                                                    
         BZ    DBLD22                                                           
         GOTO1 ACOSTEQU,APPARM,(SRACTSLN,(R2)),(SRSELSLN,(R2))                  
*                                                                               
DBLD22   LA    R2,4(R2)                                                         
         BCT   R4,DBLD20                                                        
*                                                                               
         MVC   BWDCOST1,RWORK                                                   
         MVC   BWDCOST2,RWORK+4                                                 
         MVC   BWDCOST3,RWORK+8                                                 
*                                                                               
DBLD24   L     R2,SRASIR           LOOK FOR BWS COST OVERRIDE ELEMENT           
         USING SIRRECD,R2                                                       
         MVI   RBYTE,0                                                          
         SR    R0,R0                                                            
         LA    R4,EOKELEM                                                       
*                                                                               
DBLD26   CLI   0(R4),0                                                          
         BE    DBLD35                                                           
         CLI   0(R4),EBCCODEQ                                                   
         BNE   DBLD34                                                           
         USING EBCELEM,R4                                                       
         CLC   EBCCLT,BCLT         FOUND - MATCH CLT/SLN                        
         BNE   DBLD34                                                           
         CLC   EBCSLN,BWDSLN                                                    
         BNE   DBLD34                                                           
         OC    EBCEFF,EBCEFF                                                    
         BZ    DBLD28                                                           
         GOTO1 VDATCON,RPARM,(2,EBCEFF),(3,RFULL)                               
         CLC   RFULL(3),CMPSTDT                                                 
         BNH   DBLD28                                                           
         CLC   RFULL(3),CMPND                                                   
         BH    DBLD34                                                           
         OC    BWDEFDT2,BWDEFDT2                                                
         BZ    DBLD28                                                           
         CLC   RFULL(3),BWDEFDT2                                                
         BL    DBLD28                                                           
         OC    BWDEFDT3,BWDEFDT3                                                
         BZ    DBLD30                                                           
         CLC   RFULL(3),BWDEFDT3                                                
         BL    DBLD30                                                           
         B     DBLD32                                                           
*                                                                               
DBLD28   CLI   RBYTE,1                                                          
         BE    DBLD30                                                           
         CLI   RBYTE,2                                                          
         BE    DBLD32                                                           
         MVI   RBYTE,1                                                          
         MVC   BWDCOST1,EBCCOST                                                 
         XC    BWDCOST2,BWDCOST2                                                
         XC    BWDCOST3,BWDCOST3                                                
         XC    BWDEFDT2,BWDEFDT2                                                
         XC    BWDEFDT3,BWDEFDT3                                                
         B     DBLD34                                                           
*                                                                               
DBLD30   CLI   RBYTE,2                                                          
         BE    DBLD32                                                           
         MVI   RBYTE,2                                                          
         MVC   BWDCOST2,EBCCOST                                                 
         MVC   BWDEFDT2,RFULL                                                   
         XC    BWDCOST3,BWDCOST3                                                
         XC    BWDEFDT3,BWDEFDT3                                                
         B     DBLD34                                                           
*                                                                               
DBLD32   CLI   RBYTE,3                                                          
         BE    DBLD35                                                           
         MVI   RBYTE,3                                                          
         MVC   BWDCOST3,EBCCOST                                                 
         MVC   BWDEFDT3,RFULL                                                   
*                                                                               
DBLD34   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DBLD26                                                           
         DROP  R2,R4                                                            
*                                                                               
DBLD35   GOTO1 ANETCOST,BWDCOST1   NET DOWN THE COSTS IF NECESSARY              
         GOTO1 (RF),BWDCOST2                                                    
         GOTO1 (RF),BWDCOST3                                                    
*                                                                               
DBLD36   MVC   BWDPROG,SRACTPRO    PROGRAMMING                                  
         TM    SRACTFLG,SRPROOVR   TEST PROGRAM NAME IS AN OVERRIDE             
         BZ    *+12                                                             
         OI    BWDINDS,BWDIPRG     YES                                          
         B     DBLD37                                                           
         CLC   BWDPROG,RSPACES     NO-TEST PROGRAM NAME EXISTS                  
         BNH   DBLD37                                                           
         OI    BWDINDS,BWDIPSID    YES-INDICATE PROGRAM FROM SID                
*                                                                               
DBLD37   XC    RWORK,RWORK         INITIALIZE UPGRADE EXPRESSION                
         MVC   LUPSTA(5),BWDSTA                                                 
         OC    CMPUP,CMPUP         TEST FOR DEFAULT CAMPAIGN UPGRADE            
         BZ    DBLD38                                                           
         MVC   LUPFIL(1),CMPUF     YES - SAVE UPGRADE VALUES                    
         MVC   LUPGRD(8),CMPUP                                                  
         MVC   LUPFRBK(2),CMPFB                                                 
         TM    CMPFB+1,BTY2CHAR                                                 
         BNO   DBLD37G                                                          
         CLI   CMPFBTP,0                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT(1),CMPFBTP                                              
DBLD37G  MVC   LUPFRBKL(6),CMPFBLST                                             
         MVC   LUPPUT(1),CMPUPUT                                                
         MVC   LUPSHR(1),CMPUSHR                                                
*                                                                               
DBLD38   TM    CMPOPTS,CAMONSU     TEST NO SID UPGRADE                          
         BO    DBLD46                                                           
         MVC   LAOVREL(4),SROVELEM SAVE A(FIRST DEMO OVERRIDE ELEM)             
         CLI   SRUPWHER,C'D'       CHECK UPGRADE FROM DETAIL                    
         BE    *+14                                                             
         OC    CMPUP,CMPUP         NO - TEST FOR CAMPAIGN UPGRADE               
         BNZ   DBLD46                   YES - USE IT                            
         MVC   LUPFIL(1),SRUPFILE  SAVE UPGRADE VALUES                          
         MVC   LUPGRD(8),SRUPEXP                                                
         MVC   LUPFRBK(2),SRUPBOOK                                              
         MVC   LUPFRBKL(6),SRUPEXBK                                             
         MVC   LUPDAY(1),SRUPDAY                                                
         MVC   LUPTIM(4),SRUPTIM                                                
         OC    SRUPSTA,SRUPSTA                                                  
         BZ    *+10                                                             
         MVC   LUPSTA(5),SRUPSTA    STATION OVERRIDE                            
*                                                                               
         OC    LUPGRD(8),LUPGRD    BUILD UPGRADE ELEMENT                        
         BZ    DBLD46                                                           
         XC    APELEM,APELEM                                                    
         LA    R1,APELEM                                                        
         USING UPGEL,R1                                                         
         MVI   UPGELCD,UPGELCDQ                                                 
         MVI   UPGELLN,UPGELLNQ                                                 
         MVC   UPGFILE,LUPFIL                                                   
         MVC   UPGRADE,LUPGRD                                                   
         MVC   UPGFRBK,LUPFRBK                                                  
         TM    LUPFRBK+1,BTY2CHAR                                               
         BNO   DBLD38G                                                          
         CLI   LUPFRBKT,0          ANYTHING HERE?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   UPGFRBKT,LUPFRBKT   LUPFRBK BOOKTYPE                             
         MVI   UPGELLN,UPGELLQ2    EXTENDED LENGTH                              
DBLD38G  MVC   UPGFRBKL,LUPFRBKL                                                
         MVC   UPGINPUT(4),=C'UPT='                                             
         OC    SRUPDATA,SRUPDATA   TEST SPECIAL CANADIAN BOOK LIST              
         BNZ   DBLD39                                                           
         MVC   UPGINPUT+4(3),=C'BK/'    YES                                     
         LA    R4,UPGINPUT+7                                                    
         OC    UPGFRBK,UPGFRBK                                                  
         BNZ   DBLD42                                                           
         DC    H'0'                                                             
*                                                                               
DBLD39   MVC   UPGINPUT+4(L'SRUPDATA),SRUPDATA                                  
         OC    UPGFRBK,UPGFRBK     TEST FROM SHARE BOOK                         
         BZ    DBLD45                                                           
         LA    R4,UPGINPUT+4       YES - CHECK FOR BK= EXPRESSION IN            
         LA    RF,L'SRUPDATA(R4)         INPUT                                  
*                                                                               
DBLD40   CLI   0(R4),C' '                                                       
         BNH   DBLD41                                                           
         CLC   0(3,R4),=C'BK='                                                  
         BE    DBLD45                                                           
         LA    R4,1(R4)                                                         
         CR    R4,RF                                                            
         BL    DBLD40                                                           
*                                                                               
DBLD41   MVI   0(R4),C','          MISSING - ADD BK= EXPRESSION                 
         MVC   1(3,R4),=C'BK='                                                  
         LA    R4,4(R4)                                                         
*                                                                               
DBLD42   MVC   RFULL(2),UPGFRBK                                                 
         MVI   RFULL+2,1                                                        
         GOTO1 VDATCON,RPARM,(3,RFULL),(6,(R4))                                 
         MVC   3(2,R4),4(R4)                                                    
         MVI   5(R4),C'/'                                                       
         LA    R4,6(R4)                                                         
         LA    R2,LUPFRBKL         FORMAT MORE BOOKS IF ANY                     
         LA    R0,L'UPGFRBKL/2                                                  
*                                                                               
DBLD43   OC    0(2,R2),0(R2)                                                    
         BZ    DBLD44                                                           
         MVC   RFULL(2),0(R2)                                                   
         MVI   RFULL+2,1                                                        
         GOTO1 VDATCON,RPARM,(3,RFULL),(6,(R4))                                 
         MVC   3(2,R4),4(R4)                                                    
         MVI   5(R4),C'/'                                                       
         LA    R2,2(R2)                                                         
         LA    R4,6(R4)                                                         
         BCT   R0,DBLD43                                                        
*                                                                               
DBLD44   BCTR  R4,0                                                             
         MVI   0(R4),C' '                                                       
*                                                                               
DBLD45   GOTO1 ADDELSC,BWDRECD                                                  
         CLC   SRUPSTA(L'SRUPSTA+L'SRUPDAY+L'SRUPTIM),SRUPSTA                   
         BZ    DBLD46                                                           
         XC    APELEM,APELEM       BUILD OVERRIDE DAY/TIME/STATION ELEM         
         LA    R1,APELEM                                                        
         USING ODTEL,R1                                                         
         MVI   ODTELCD,ODTELCDQ                                                 
         MVI   ODTELLN,ODTELLNQ                                                 
         MVC   ODTDAY,SRUPDAY                                                   
         MVC   ODTTIME,SRUPTIM                                                  
         MVC   ODTSTA,SRUPSTA                                                   
         GOTO1 ADDELSC,BWDRECD                                                  
*                                                                               
DBLD46   OC    SRACTCOM,SRACTCOM   TEST FOR COMMENT                             
         BZ    DBLD48                                                           
         XC    APELEM,APELEM       YES - BUILD COMMENT ELEMENT                  
         LA    R1,APELEM                                                        
         USING COMEL,R1                                                         
         MVI   COMELCD,COMELCDQ                                                 
         LA    RE,L'SRACTCOM                                                    
         LA    RE,COMCOM-COMEL(RE)                                              
         STC   RE,COMELLN                                                       
         MVC   COMCOM(L'SRACTCOM),SRACTCOM                                      
         GOTO1 ADDELSC,BWDRECD                                                  
*                                                                               
DBLD48   OC    LUPGRD(8),LUPGRD    TEST FOR UPGRADE EXPRESSION                  
         BZ    DBLDX               NO - SKIP UPGRADE                            
         BRAS  RE,DEMUP            YES - DO THE DEMO UPGRADES                   
         B     DBLDX                     AND ADD DEMO ELEMENT                   
*                                                                               
DBLD99   MVC   FVMSGNO,=AL2(FVCMSEQ)                                            
*                                                                               
DBLDX    B     ROUTSX                                                           
         DROP  R5                                                               
         SPACE 2                                                                
LUPGRD   EQU   RWORK                                                            
LUPFIL   EQU   RWORK+8                                                          
LUPFRBK  EQU   RWORK+9                                                          
LUPSTA   EQU   RWORK+11                                                         
LUPDAY   EQU   RWORK+16                                                         
LUPTIM   EQU   RWORK+17                                                         
LUPPUT   EQU   RWORK+21                                                         
LUPSHR   EQU   RWORK+22                                                         
LAOVREL  EQU   RWORK+23                                                         
LUPFRBKL EQU   RWORK+27                                                         
LUPFRBKT EQU   RWORK+33                                                         
         EJECT                                                                  
***********************************************************************         
* SID TRANSFER : ADD BWS DETAIL RECORD                                *         
* INPUT  : AIOAREA1 CONTAINS HEADER RECORD                            *         
*          AIOAREA2 CONTAINS DETAIL RECORD                            *         
* OUTPUT : APRECKEY = DETAIL RECORD KEY                               *         
***********************************************************************         
         SPACE 1                                                                
XFRADD   L     R2,AIOAREA1                                                      
         USING BWHRECD,R2                                                       
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         USING TWAD,R5             R5=A(TWA)                                    
         TM    TWAFLAG,TWANOHDR    TEST HEADER EXISTS                           
         BZ    XFR2                                                             
         XC    BWHKEY(256),BWHKEY  NO - BUILD THE HEADER                        
         MVC   BWHKEY,HDRKEY       KEY                                          
         MVC   BWHKSEQ,BCMSEQ      CAMPAIGN/MARKET SEQ NO                       
         MVI   BWHELCD,BWHELCDQ    STATION ELEMENT                              
         MVI   BWHELLN,BWHELLNQ                                                 
         MVI   BWHSEQ,1                                                         
         MVC   BWHSTA,QSTA         STATION                                      
         OC    QSTA,QSTA           TEST STATION FILTER                          
         BNZ   XFR1                                                             
         LA    R1,APRECKEY         NO-GET STATION FROM THE KEY                  
         XC    APDUB,APDUB                                                      
         MVC   APDUB+2(3),NSSTA-NSIDKEYD(R1)                                    
         GOTO1 VMSUNPK,APPARM,(X'80',APDUB),APFULL,BWHSTA                       
         CLI   BWHSTA+4,C' '                                                    
         BNE   XFR1                                                             
         MVI   BWHSTA+4,C'T'                                                    
*                                                                               
XFR1     LA    R1,BWHFSTEL-BWHKEY+BWHELLNQ                                      
         STCM  R1,3,BWHLEN                                                      
         LA    R1,FILADD1          ADD RECORD                                   
         B     XFR4                                                             
*                                                                               
XFR2     TM    TWAFLAG,TWANOSTA    HEADER EXISTS - TEST NEW STATION             
         BZ    XFR6                                                             
         MVC   IODA,HDRDA                                                       
         LA    R1,RIO              YES-PUT RECORD                               
         ST    R1,IOADDR                                                        
         GOTO1 AIO,FILGETU                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOAREA1                                                      
         ST    R1,IOADDR                                                        
         LA    R1,FILPUT                                                        
*                                                                               
XFR4     GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TWAFLAG,TWANOHDR    TEST ADDING NEW HEADER                       
         BZ    XFR5                                                             
         MVC   HDRDA,IODA                                                       
         XC    IOKEY,IOKEY         YES-ADD PASSIVE POINTER                      
         LA    R2,IOKEY                                                         
         MVI   BWHPTYP,BWHPTYPQ                                                 
         MVI   BWHPSUB,BWHPSUBQ                                                 
         MVC   BWHPAGMD,BAGYMD                                                  
         OC    BWHPAGMD,BBYRMASK                                                
         MVC   BWHPBYR,BBYR                                                     
         MVC   BWHPSEQ,BCMSEQ                                                   
         MVC   BWHKCNTL+1(4),HDRDA                                              
         GOTO1 AIO,DIRADD                                                       
         BNL   XFR5                                                             
         DC    H'0'                                                             
*                                                                               
XFR5     TM    TWAMODE,TWAMLSM     TEST IN LIST/SELECT MODE                     
         BZ    XFR6                                                             
         L     R1,ALSM             YES-ALTER GENERAL'S KEY SO THAT              
         USING LSMD,R1                 GENERAL DOESN'T THINK THAT THE           
         XC    LSMUKEY(13),LSMUKEY     KEY HAS CHANGED                          
         LA    RE,BWDKELST-BWDKEY-1                                             
         OC    BSTA,BSTA                                                        
         BZ    *+8                                                              
         LA    RE,BWDKELPO-BWDKEY-1                                             
         EX    RE,*+8                                                           
         B     XFR6                                                             
         MVC   LSMUKEY(0),BWDKEY                                                
         DROP  R1                                                               
*                                                                               
XFR6     L     R2,AIOAREA1         ** DOUBLE CHECK THAT THE                     
         LA    R1,BWHFSTEL            STATION CODE IS CORRECT **                
         SR    R0,R0                                                            
*                                                                               
XFR7     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),BWHELCDQ                                                   
         BNE   *+14                                                             
         USING BWHEL,R1                                                         
         CLC   BWDSTA,BWHSTA                                                    
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     XFR7                                                             
         CLC   BWDKELST,BWHSEQ                                                  
         BNE   XFR98                                                            
         DROP  R1                                                               
*                                                                               
         NI    TWAFLAG,255-TWANODET                                             
         OI    BWDINDS,BWDIXFR     INDICATE TRANSFERRED FROM NSID               
         OC    CMPU2DAT,CMPU2DAT   TEST SECOND CAMPAIGN UPGRADE                 
         BZ    XFR12                                                            
         OI    BWDINDS,BWDIUP1     YES-INDICATE FROM FIRST UPGRADE              
         LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         MVC   BWDDATES(2),0(RE)                                                
         TM    CMPOPTS,CAMOWKS     DATES=FLIGHT START TO WEEK BEFORE            
         BO    XFR8                      2ND UPGRADE EFFECTIVE DATE             
         GOTO1 VDATCON,RPARM,(3,CMPSTDT),(2,BWDDATES)                           
*                                                                               
XFR8     LR    RE,R5                                                            
         AHI   RE,CMPDATSP-TWAD                                                 
         LA    R1,1                                                             
*                                                                               
XFR10    CLI   4(RE),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   CMPU2DAT,6(RE)                                                   
         BNH   *+16                                                             
         LA    R1,1(R1)                                                         
         LA    RE,4(RE)                                                         
         B     XFR10                                                            
         MVC   BWDDATES+2(2),2(RE)                                              
         MVC   RHALF,4(RE)         RHALF=START OF SECOND RECORD                 
         LA    R0,16                                                            
         SR    RF,RF               SET INACTIVE WEEKS MASK                      
         SLDL  RE,1                                                             
         BCTR  R0,0                                                             
         BCT   R1,*-6                                                           
         L     RF,=F'-1'                                                        
         SLDL  RE,1                                                             
         BCT   R0,*-4                                                           
         STCM  RE,3,BWDWKS                                                      
*                                                                               
XFR12    MVC   IOKEY(13),BWDKEY    READ DETAIL RECORDS TO FIND NEXT             
         LA    R3,IOKEY            AVAILABLE SEQUENCE NUMBER                    
         MVI   BWDKELSQ,0                                                       
         SR    R4,R4                                                            
         LA    R1,MINHI3                                                        
         B     XFR14+4                                                          
*                                                                               
XFR14    LA    R1,MINSEQ3                                                       
         GOTO1 AMIN                                                             
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     XFR16                                                            
         CLC   IOKEY(BWDKELSQ-BWDKEY),IOKEYSAV                                  
         BNE   XFR16                                                            
         IC    R4,BWDKELSQ         SAVE LATEST SEQUENCE NUMBER                  
         B     XFR14                                                            
*                                                                               
XFR16    LA    R4,1(R4)            NEXT AVAILABLE SEQ NO                        
         CHI   R4,255                                                           
         BH    XFR99                                                            
         L     R3,AIOAREA2                                                      
         STC   R4,BWDKELSQ                                                      
         STC   R4,BWDSEQ                                                        
         GOTO1 AMIN,MINADD2        ** ADD THE DETAIL REORD **                   
*                                                                               
         TM    BWDINDS,BWDIUP1     TEST THIS RECORD IS FIRST OF TWO             
         BZ    XFR40               UPGRADES PAIR                                
*                                                                               
         NI    BWDINDS,255-BWDIUP1 YES-NOW ADD SECOND RECORD                    
         OI    BWDINDS,BWDIUP2                                                  
         XC    BWDWKS,=X'FFFF'     INACTIVE WEEKS = REVERSE                     
         MVC   BWDDATES,RHALF      SET START DATE                               
         TM    CMPOPTS,CAMOWKS     AND END DATE                                 
         BO    XFR18                                                            
         GOTO1 VDATCON,RPARM,(3,CMPND),(2,BWDDATES+2)                           
         B     XFR20                                                            
*                                                                               
XFR18    GOTO1 VDATCON,RPARM,CMPFLND,(2,BWDDATES+2)                             
*                                                                               
XFR20    MVC   BWDUPUT,CMPUPUT2    MOVE 2ND CAMPAIGN UPGRADE INTO               
         MVC   BWDUSHR,CMPUSHR2    THE RECORD                                   
         MVI   APELEM,UPGELCDQ                                                  
         GOTO1 ADELELS,BWDRECD                                                  
         XC    APELEM,APELEM                                                    
         LA    R4,APELEM                                                        
         USING UPGEL,R4                                                         
         MVI   UPGELCD,UPGELCDQ                                                 
         MVI   UPGELLN,UPGELLNQ                                                 
         MVC   UPGFILE,CMPUF2                                                   
         MVC   UPGRADE,CMPUP2                                                   
         MVC   UPGFRBK,CMPFB2                                                   
         TM    CMPFB2+1,BTY2CHAR                                                
         BNO   XFR20G                                                           
         CLI   CMPFB2TP,0          ANYTHING HERE?                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   UPGFRBKT,CMPFB2TP                                                
XFR20G   MVC   UPGINPUT,CMPUPIN2                                                
         MVC   UPGFRBKL,CMPFB2L                                                 
         GOTO1 ADDELSC,BWDRECD                                                  
         XC    RWORK,RWORK         PREPARE FOR UPGRADE                          
         MVC   LUPFIL(1),CMPUF2                                                 
         MVC   LUPGRD(8),CMPUP2                                                 
         MVC   LUPFRBK(2),CMPFB2                                                
         TM    CMPFB2+1,BTY2CHAR                                                
         BNO   XFR20T                                                           
         CLI   CMPFB2TP,0                                                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   LUPFRBKT(1),CMPFB2TP   CMPFB2 BOOKTYPE                           
XFR20T   MVC   LUPPUT(1),CMPUPUT2                                               
         MVC   LUPSHR(1),CMPUSHR2                                               
         MVC   LUPSTA(5),BWDSTA                                                 
         LA    R1,BWDEL            LOOK FOR OVERRIDE DAY/TIME/STATION           
         SR    R0,R0               ELEMENT AND DEMO ELEMENT                     
         XC    RDUB,RDUB                                                        
*                                                                               
XFR22    CLI   0(R1),0                                                          
         BE    XFR24                                                            
         LA    RE,RDUB                                                          
         CLI   0(R1),ODTELCDQ                                                   
         BE    *+16                                                             
         LA    RE,RDUB+4                                                        
         CLI   0(R1),DMOELCDQ                                                   
         BNE   *+8                                                              
         ST    R1,0(RE)                                                         
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     XFR22                                                            
*                                                                               
XFR24    ICM   R1,15,RDUB          TEST OVERRIDE STATION/DAY/TIME ELEM          
         BZ    XFR26                                                            
         USING ODTEL,R1                                                         
         OC    ODTSTA,ODTSTA       YES-TEST OVERRIDE STATION                    
         BZ    *+10                                                             
         MVC   LUPSTA(5),ODTSTA                                                 
         MVC   LUPDAY(1),ODTDAY    OVERRIDE DAYS                                
         MVC   LUPTIM(4),ODTTIME   OVERRIDE TIMES                               
         DROP  R1                                                               
*                                                                               
XFR26    XC    RIO(256),RIO                                                     
         ICM   R1,15,RDUB+4        BUILD LIST OF DEMO OVERRIDE                  
         BZ    XFR32               ELEMENTS (IF ANY)                            
         B     XFR32               *** 2ND RECORD SHOULD NOT GET ANY            
*                                  *** DEMO OVERRIDES ***                       
         USING DMOEL,R1                                                         
         ZIC   RF,1(R1)                                                         
         AR    RF,R1                                                            
         LA    RE,RIO                                                           
         LA    R1,DMODEMO                                                       
*                                                                               
XFR28    TM    4(R1),DMODEMOV                                                   
         BZ    XFR30                                                            
         MVI   0(RE),OVERELEM                                                   
         MVI   1(RE),6                                                          
         MVC   2(2,RE),1(R1)                                                    
         MVC   4(2,RE),6(R1)                                                    
         LA    RE,6(RE)                                                         
*                                                                               
XFR30    LA    R1,L'DMODEMO(R1)                                                 
         CR    R1,RF                                                            
         BL    XFR28                                                            
         DROP  R1                                                               
*                                                                               
XFR32    LA    RE,RIO              A(LIST OF OVERRIDE DEMO ELS)                 
         STCM  RE,15,LAOVREL                                                    
         MVI   APELEM,DMOELCDQ     DELETE DEMO ELEMENT                          
         GOTO1 ADELELS,BWDRECD                                                  
         BRAS  RE,DEMUP            DO UPGRADES AND ADD DEMO ELEMENT             
         ZIC   R4,BWDSEQ                                                        
         B     XFR16               ADD SECOND WORK RECORD                       
*                                                                               
XFR40    NI    APRECID,255-RINSID  NOT NSID RECORD ANY MORE                     
         MVI   APRECNUM,0                                                       
         MVC   APRECKEY(13),BWDKEY                                              
         OC    CMPU2DAT,CMPU2DAT   TEST ADDED SECOND RECORD                     
         BZ    XFRX                                                             
         OI    TWAACTIV,TWAACSCR   YES-THEN RE-SCROLL TO CURRENT SCREEN         
         B     XFRX                                                             
*                                                                               
XFR98    MVC   FVMSGNO,=AL2(FVISTA)    INVALID STATION                          
         B     XFRX                                                             
*                                                                               
XFR99    MVC   FVMSGNO,=AL2(FVTMD) TOO MANY DUPLICATES                          
*                                                                               
XFRX     B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PACKED TIMES                                         *         
* INPUT  : R1=A(4-BYTE TIMES)                                         *         
* OUTPUT : PTIMES                                                     *         
***********************************************************************         
         SPACE 1                                                                
LPACKTIM NTR1                                                                   
         MVI   RFLAG,1             LOCAL ENTRY                                  
         B     PTIM1                                                            
*                                                                               
PACKTIME MVI   RFLAG,0                                                          
*                                                                               
PTIM1    MVC   RDUB(4),0(R1)                                                    
         CLC   RDUB(2),RDUB+2                                                   
         BNH   *+16                                                             
         LH    R1,RDUB+2                                                        
         AHI   R1,2400                                                          
         STH   R1,RDUB+2                                                        
         SR    R0,R0                                                            
         LH    R1,RDUB             CALCULATE NUMBER OF QTR HRS                  
         SR    RE,RE               0 = 00-15 MINS                               
         SR    RF,RF               1 = 16-30 MINS                               
         ICM   RF,3,RDUB+2         2 = 31-45 MINS ETC                           
         BZ    PTIM2                                                            
         D     R0,=F'100'                                                       
         D     RE,=F'100'                                                       
         SR    RF,R1                                                            
         MHI   RF,60                                                            
         AR    RF,RE                                                            
         SR    RF,R0                                                            
         SR    RE,RE                                                            
         D     RE,=F'15'                                                        
         LTR   RF,RF                                                            
         BZ    *+12                                                             
         LTR   RE,RE                                                            
         BNZ   *+6                                                              
         BCTR  RF,0                                                             
         C     RF,=F'15'                                                        
         BNH   PTIM2                                                            
         L     RF,=F'15'                                                        
*                                                                               
PTIM2    SR    RE,RE               PACKED TIMES                                 
         ICM   RE,3,RDUB                                                        
         SLL   RE,4                                                             
         AR    RE,RF                                                            
         STCM  RE,3,PTIMES                                                      
*                                                                               
PTIMX    CLI   RFLAG,1                                                          
         BNE   ROUTSX                                                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* TSAR INTERFACE ROUTINE                                              *         
* INPUT  : R1=A(TSAR RECORD)                                          *         
*          TSARACT=TSAR ACTION                                        *         
*          TSARNUM=TSAR RECORD NUMBER                                 *         
*          NOTE: IF ACTION=DELETE AND R1=0, THEN DELETE BY NUMBER     *         
* OUTPUT : CC=EQUAL IF ACTION OK                                      *         
*          CC=NE AND FVMSGNO SET AS FOLLOWS IF NOT OK                 *         
*             FVMSGNO=FVFERNF FOR ACTIONS TSAWRT,TSADEL,TSARDH        *         
*             FVMSGNO=FVFERAE FOR ACTION  TSAADD                      *         
*             FVMSGNO=FVTMR   FOR ACTIONS TSAINI,TSARDH,TSANXT        *         
***********************************************************************         
         SPACE 1                                                                
LTSAR    NTR1  ,                                                                
         MVI   RTSARLOC,C'Y'                                                    
         B     TSAR1                                                            
*                                                                               
TSARBWS  MVI   RTSARLOC,C'N'                                                    
*                                                                               
TSAR1    LR    R3,R1                                                            
         LR    R3,R1                                                            
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         LTR   R3,R3               TEST A(RECORD) PASSED                        
         BZ    *+8                                                              
         ST    R3,TSAREC           YES-UPDATE IT                                
         LA    RE,TSACTAB          FIND THE ACTION'S ROUTINE                    
*                                                                               
TSAR2    CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TSARACT,0(RE)                                                    
         BE    *+12                                                             
         LA    RE,4(RE)                                                         
         B     TSAR2                                                            
         SR    RF,RF                                                            
         ICM   RF,7,1(RE)                                                       
         A     RF,ACRELO                                                        
         L     R2,VTSAR            R2=V(TSAR)                                   
         CLI   TSARACT,TSAINI      TEST INIT                                    
         BE    TSAR4                                                            
         TM    TWAINDS,TWAITSIN    NO-TEST ALREADY INITIALIZED                  
         BO    TSAR3                                                            
         MVI   TSACTN,TSAINI                                                    
         ST    RF,ACFULL                                                        
         BAS   R4,TSINI            NO-INITIALIZE FIRST                          
         BNE   TSARX                                                            
         L     RF,ACFULL                                                        
         B     TSAR4                                                            
*                                                                               
TSAR3    CLI   TSARACT,TSARES      TEST RESTORE                                 
         BE    TSAR4                                                            
         TM    TWAINDS,TWAITSRS    NO-TEST TSAR NEEDS TO BE RESTORED            
         BO    TSAR4                                                            
         MVI   TSACTN,TSARES                                                    
         ST    RF,ACFULL                                                        
         BAS   R4,TSRES            YES-RESTORE                                  
         L     RF,ACFULL                                                        
*                                                                               
TSAR4    MVC   TSACTN,TSARACT      EXECUTE TSAR ACTION                          
         BASR  R4,RF                                                            
         B     TSARX                                                            
*                                                                               
TSACTAB  DC    AL1(TSAINI),AL3(TSINI)                                           
         DC    AL1(TSAADD),AL3(TSADD)                                           
         DC    AL1(TSAWRT),AL3(TSWRT)                                           
         DC    AL1(TSAPUT),AL3(TSPUT)                                           
         DC    AL1(TSADEL),AL3(TSDEL)                                           
         DC    AL1(TSARDH),AL3(TSRDH)                                           
         DC    AL1(TSAGET),AL3(TSGET)                                           
         DC    AL1(TSANXT),AL3(TSNXT)                                           
         DC    AL1(TSASAV),AL3(TSSAV)                                           
         DC    AL1(TSARES),AL3(TSRES)                                           
         DC    X'00'                                                            
*                                                                               
TSINI    BAS   RE,INITSBLK         INITIALIZE                                   
         OI    TSPAGN,TSPAGNCI+1                                                
         TM    TWAINDS,TWAITSIN                                                 
         BZ    *+8                                                              
         OI    TSINDS,TSIREUSE                                                  
         BASR  RE,R2                                                            
         BE    TSINIRES                                                         
         TM    TSERRS,TSEALF       ERROR-TEST ALLOCATION FAILURE                
         BO    ETSALF              YES                                          
         CLI   TSARACT,TSAINI      TEST USER ACTION=INIT                        
         BE    ETSEOF              YES-RETURN ERROR CODE                        
         DC    H'0'                NO-DIE NOW                                   
*                                                                               
TSRES    TM    TWAINDS,TWAITSRS    RESTORE                                      
         BO    TSX                                                              
         BAS   RE,INITSBLK                                                      
         MVC   TSINDS,TWATSARI                                                  
         OI    TSINDS,TSIREUSE                                                  
         MVC   TSPAGL,TWALOWPG                                                  
         MVC   TSPAGN,TWANUMPG                                                  
         BASR  RE,R2                                                            
*                                                                               
TSINIRES OI    TWAINDS,TWAITSIN+TWAITSRS    POST INIT/RESTORE                   
         MVC   TWALOWPG,TSPAGL                                                  
         MVC   TWANUMPG,TSPAGN                                                  
         MVC   TWATSARI,TSINDS                                                  
         NI    TWATSARI,TSIALLOC+TSIRTNAF                                       
         B     TSX                                                              
*                                                                               
INITSBLK MVI   TSNBUF,1            INITIALIZE TSAR BLOCK                        
         ICM   RF,15,ATSARBUF                                                   
         BNZ   *+8                                                              
         L     RF,ATIA                                                          
         STCM  RF,7,TSABUF+1                                                    
         MVC   TSACOM,ACOM                                                      
         OI    TSRECI,TSRVAR                                                    
         MVI   TSKEYL,L'TKEY                                                    
         MVC   TSRECL,=Y(TSPDATAL/8)                                            
         MVI   TSINDS,TSIALLOC+TSIRTNAF                                         
         BR    RE                                                               
*                                                                               
TSADD    BASR  RE,R2               ADD                                          
         BE    TSX                                                              
         TM    TSERRS,TSEDUP       TEST KEY ALREADY EXISTS                      
         BO    ETSAE               YES-TELL THE CALLER                          
*****                                                                           
         TM    TSERRS,TSEEOF       THIS WAS NEVER HERE BEFORE                   
         BO    ETSEOF              EOF, REPORT THIS TO BUY OVERLAYS             
*****                                                                           
TSADDDIE DC    H'0'                                                             
*                                                                               
TSWRT    BASR  RE,R2               WRITE                                        
         BE    TSX                                                              
         TM    TSERRS,TSERNF       TEST KEY NOT FOUND                           
         BO    ETSNF               YES-TELL THE CALLER                          
         DC    H'0'                                                             
*                                                                               
TSDEL    XC    TSRNUM,TSRNUM       DELETE                                       
         LTR   R3,R3               TEST A(RECORD) PASSED                        
         BNZ   *+10                                                             
         MVC   TSRNUM,TSARNUM      NO-THEN USE RECORD NUMBER                    
         BASR  RE,R2                                                            
         BE    TSX                                                              
         TM    TSERRS,TSERNF       TEST RECORD NOT FOUND                        
         BO    ETSNF               YES-TELL THE CALLER                          
         DC    H'0'                                                             
*                                                                               
TSRDH    BASR  RE,R2               READ HIGH                                    
         BE    TSX                                                              
         TM    TSERRS,TSEEOF       TEST END-OF-FILE                             
         BO    ETSEOF              YES-TELL THE USER                            
         TM    TSERRS,TSERNF       TEST EXACT KEY NOT FOUND                     
         BO    ETSNF               YES-TELL THE USER                            
         DC    H'0'                                                             
*                                                                               
TSNXT    BASR  RE,R2                                                            
         BE    TSX                                                              
         TM    TSERRS,TSEEOF                                                    
         BO    ETSEOF                                                           
         DC    H'0'                                                             
*                                                                               
TSPUT    MVC   TSRNUM,TSARNUM      PUT                                          
         BASR  RE,R2                                                            
         BE    TSX                                                              
         DC    H'0'                                                             
*                                                                               
TSGET    MVC   TSRNUM,TSARNUM      GET                                          
         BASR  RE,R2                                                            
         BE    TSX                                                              
         TM    TSERRS,TSERNF+TSEEOF                                             
         BNZ   ETSNF                                                            
         DC    H'0'                                                             
*                                                                               
TSSAV    BASR  RE,R2               SAVE                                         
         BE    TSX                                                              
         DC    H'0'                                                             
*                                                                               
*                                                                               
ETSNF    MVC   FVMSGNO,=AL2(FVFERNF)     NOT FOUND                              
         B     TSX                                                              
*                                                                               
ETSAE    MVC   FVMSGNO,=AL2(FVFERAE)     ALREADY EXISTS                         
         B     TSX                                                              
*                                                                               
ETSEOF   MVC   FVMSGNO,=AL2(FVTMR)       END-OF-FILE                            
         B     TSX                                                              
*                                                                               
ETSALF   MVC   FVMSGNO,=AL2(FVFTSAF)     ALLOCATION FAILURE                     
         B     TSX                                                              
*                                                                               
TSX      CLC   FVMSGNO,=AL2(FVFOK)                                              
         BR    R4                                                               
*                                                                               
TSARX    CLI   RTSARLOC,C'Y'                                                    
         BNE   ROUTSX                                                           
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EQUIVALENCE A COST                                       *         
* INPUT  : P1 - BYTE 0 = SPOT LENGTH OF ORIGINAL COST                 *         
*               BYTES 1-3 = A(ORIGINAL COST)                          *         
*          P2 - BYTE 0 = NEW SPOT LENGTH                              *         
* OUTPUT : P2 - BYTES 1-3 = A(EQUIVALENCED COST)                      *         
***********************************************************************         
         SPACE 1                                                                
COSTEQU  LM    R2,R3,0(R1)         R2=A(COST),R3=A(ADJUSTED COST)               
         MVC   0(4,R3),0(R2)       SET ADJUSTED COST = ORIGINAL COST            
         CLC   0(1,R1),4(R1)       CHECK SPOT LENGTHS ARE DIFFERENT             
         BE    CEQUX                                                            
         CLI   CLTBWPRO+12,C'N'    CHECK FOR INHIBIT COST ADJUSTMENTS           
         BE    CEQUX                                                            
         CLI   4(R1),30            TEST SPOT LENGTH LESS THAN 30 SEC            
         BNL   *+12                                                             
         CLI   CLTBWPRO+13,C'N'    YES-CHECK INHIBIT FOR SLN < 30               
         BE    CEQUX                                                            
         XC    RDUB,RDUB                                                        
         OC    CMPSLEQU,CMPSLEQU   TEST CAMPAIGN SPOT LENGTH EQUIVS             
         BZ    CEQU6                                                            
         LA    R0,5                YES-                                         
         LA    R4,CMPSLEQU                                                      
*                                                                               
CEQU2    CLI   0(R4),0                                                          
         BE    CEQU4                                                            
         LA    RE,RDUB                                                          
         CLC   0(1,R4),0(R1)                                                    
         BE    *+18                                                             
         LA    RE,4(RE)                                                         
         CLC   0(1,R4),4(R1)                                                    
         BNE   *+10                                                             
         MVC   2(2,RE),1(R4)                                                    
         LA    R4,3(R4)                                                         
         BCT   R0,CEQU2                                                         
*                                                                               
CEQU4    OC    RDUB(4),RDUB        TEST EQUIVALENCES FOUND FOR EACH             
         BZ    *+14                SPOT LENGTH                                  
         OC    RDUB+4(4),RDUB+4                                                 
         BNZ   CEQU12              YES                                          
         XC    RDUB,RDUB           NO-USE CLIENT EQUIVALENCE TABLE              
*                                                                               
CEQU6    LA    R0,2                CLIENT EQUIVALENCE TABLE                     
         LA    RE,RDUB                                                          
         LR    RF,R1                                                            
*                                                                               
CEQU8    LA    R4,LENTAB2                                                       
*                                                                               
CEQU10   CLI   0(R4),0                                                          
         BE    CEQUX                                                            
         CLC   0(1,R4),0(RF)                                                    
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     CEQU10                                                           
         LA    RF,LENTAB2                                                       
         SR    R4,RF                                                            
         SLL   R4,2                                                             
         LA    R4,CLTEQUIV(R4)                                                  
         MVC   2(2,RE),0(R4)                                                    
         BCT   R0,*+8                                                           
         B     CEQU12                                                           
         LA    RE,4(RE)                                                         
         LA    RF,4(R1)                                                         
         B     CEQU8                                                            
*                                                                               
CEQU12   L     RF,0(R2)            DO THE COST ADJUSTMENT                       
         M     RE,RDUB+4                                                        
         SLDA  RE,1                                                             
         ICM   R4,15,RDUB                                                       
         BZ    CEQUX                                                            
         TM    CLTIND2,CLTIREQC    TEST TO ROUND EQUIVALENCED COSTS             
         BZ    *+8                                                              
         MHI   R4,100              YES-DIVIDE BY 100 TO GET DOLLARS             
         DR    RE,R4                                                            
         LTR   RF,RF                                                            
         BM    *+8                                                              
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         TM    CLTIND2,CLTIREQC    TEST ROUNDING                                
         BZ    *+8                                                              
         M     RE,=F'100'          YES-MULIPLY BY 100 TO GET EXACT              
         ST    RF,0(R3)                DOLLAR FIGURE                            
*                                                                               
CEQUX    B     ROUTSX                                                           
         SPACE 2                                                                
LENTAB2  DC    AL1(10,15,20,30,40,45,50,60,90,120,27,3,75,0)                    
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO NET DOWN A COST                                          *         
* OPTION TO NET DOWN THE COST IS IN THE BW PROFILE                    *         
* INPUT  : R1=A(COST)                                                 *         
***********************************************************************         
         SPACE 1                                                                
NETCOST  CLI   CLTBWPRO+14,C'Y'                                                 
         BNE   ROUTSX                                                           
         TM    CMPOPTS,CAMONOND    CAN BE OVERRIDDEN BY CAMPAIGN                
         BO    ROUTSX                                                           
         L     RF,0(R1)                                                         
         M     RE,=F'170'                                                       
         D     RE,=F'100'                                                       
         LTR   RF,RF                                                            
         BM    *+8                                                              
         LA    RF,1(RF)                                                         
         SRA   RF,1                                                             
         ST    RF,0(R1)                                                         
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NETWORK BUY DOLLARS AND DEMOS                                  
* INPUT  : PARM1=A(DAYPART)                                                     
*          PARM2=A(SPOT LENGTH)                                                 
* OUTPUT : CC=EQUAL IF SUCCESSFUL AND                                           
*                 IOAREA4+00(04)   = TOTAL DOLLARS                              
*                 IOAREA4+04(04)   = TOTAL POINTS                               
*                 IOAREA4+08(212)  = DOLLARS BY WEEK (53X4)                     
*                 IOAREA4+220(212) = POINTS  BY WEEK (53X4)                     
*          CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER               
*                                                                               
* NOTE: SINCE IOAREA4 IS GETTING CLOBBERED, ALSO USING +1024 FOR                
*        READING IN THE TSAR RECORDS AS IT HAS GROWN BIGGER THAN RWORK4         
***********************************************************************         
         SPACE 1                                                                
LGETNET  NTR1  ,                   LOCAL ENTRY POINT                            
         MVI   RFLAG,1                                                          
         L     R5,ATWA                                                          
         B     GETNET2                                                          
*                                                                               
GETNET   MVI   RFLAG,0                                                          
*                                                                               
GETNET2  LM    RE,RF,0(R1)                                                      
         XC    RHALF,RHALF                                                      
         MVI   RBYTE,0                                                          
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         MVC   RHALF(1),0(RE)      DAYPART                                      
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   RHALF+1(1),0(RF)    LENGTH                                       
*                                                                               
         L     R3,AIOAREA4                                                      
         LR    RE,R3                                                            
         LA    RF,2048            (1ST 432 FOR THE DEFINED AREA)                
         XCEFL                                                                  
*                                                                               
         LR    R4,R3              (+1024 FOR READING IN TSAR REC)               
         AHI   R4,1024                                                          
         USING TREC,R4                                                          
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BE    GETNET9             YES-GET OUT                                  
         TM    TWAINDS,TWAITSIN    TEST TSAR INITIALIZED                        
         BZ    GETNET9                                                          
         GOTO1 VDMGR,APPARM,DMWRITE,TEMPSTR,(3,0),ATIA  SAVE THE TIA            
         MVI   TRECTYP,TRECSPL                                                  
         MVC   TAGYMD,BAGYMD                                                    
         MVC   TBYR,BBYR                                                        
         MVC   TCAM,BCAM                                                        
         MVC   TMKT,BMKT                                                        
         MVC   RWORK(L'TKEY),TKEY                                               
         MVI   TSARACT,TSARDH                                                   
         GOTO1 LTSAR,TREC          READ A RECORD                                
         BE    GETNET3                                                          
         CLC   FVMSGNO,=AL2(FVTMR) TEST EOF                                     
         BE    GETNET8                                                          
         CLC   FVMSGNO,=AL2(FVFERNF)  TEST EXACT KEY NOT FOUND                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   TKEY(TTYPE-TKEY),RWORK                                           
         BNE   GETNET8                                                          
         MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
GETNET3  OI    NETIND,NETIDATA                                                  
         MVC   TKEY,RWORK                                                       
         MVI   TTYPE,C'N'                                                       
         MVC   TDPT,RHALF                                                       
         MVC   TSLN,RHALF+1                                                     
         MVC   RWORK(L'TKEY),TKEY                                               
         GOTO1 LTSAR,TREC          READ NETWORK RECORD                          
         BNE   GETNET4                                                          
         MVI   RBYTE,1                                                          
         MVC   0(4,R3),TCOST                                                    
         MVC   4(4,R3),TDEMTOT                                                  
         MVC   8(212,R3),TDOLS                                                  
         MVC   220(212,R3),TDEMS                                                
*                                                                               
GETNET4  MVC   TKEY,RWORK                                                       
         MVI   TTYPE,C'S'                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 LTSAR,TREC          READ NETWORK SPILL RECORD                    
         BNE   GETNET6                                                          
         MVI   RBYTE,1                                                          
         BAS   RE,GETNETTO                                                      
*                                                                               
GETNET6  MVC   TKEY,RWORK                                                       
         MVI   TTYPE,C'T'                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 LTSAR,TREC          READ TV SPILL RECORD                         
         BNE   GETNET7                                                          
         MVI   RBYTE,1                                                          
         BAS   RE,GETNETTO                                                      
*                                                                               
GETNET7  MVC   TKEY,RWORK                                                       
         MVI   TTYPE,C'L'                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         GOTO1 LTSAR,TREC          READ SELECT TV RECORD                        
         BNE   GETNET8                                                          
         MVI   RBYTE,1                                                          
         BAS   RE,GETNETTO                                                      
*                                                                               
GETNET8  MVI   TSARACT,TSASAV      SAVE TSAR INFO                               
         GOTO1 LTSAR                                                            
         NI    TWAINDS,255-TWAITSRS                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   RBYTE,1                                                          
         BE    GETNETX                                                          
*                                                                               
GETNET9  MVC   FVMSGNO,=AL2(FVFERNF)   NOT ON FILE                              
*                                                                               
GETNETX  CLI   TRECTYP,0           TEST TSAR CALLED                             
         BE    GETNETX2                                                         
         GOTO1 VDMGR,APPARM,DMREAD,TEMPSTR,(3,0),ATIA  YES-RESTORE TIA          
GETNETX2 CLI   RFLAG,1                                                          
         BNE   ROUTSX                                                           
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         SPACE 2                                                                
GETNETTO LR    R0,RE                                                            
         L     RE,0(R3)                                                         
         ICM   RF,15,TCOST                                                      
         AR    RE,RF                                                            
         ST    RE,0(R3)                                                         
         L     RE,4(R3)                                                         
         ICM   RF,15,TDEMTOT                                                    
         AR    RE,RF                                                            
         ST    RE,4(R3)                                                         
         LA    RF,53               REAL MAX                                     
         LA    R1,TDOLS                                                         
         LA    R2,8(R3)                                                         
         L     RE,0(R2)                                                         
         A     RE,0(R1)                                                         
         ST    RE,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   RF,*-20                                                          
         LA    RF,53               REAL MAX                                     
         LA    R1,TDEMS                                                         
         LA    R2,220(R3)                                                       
         L     RE,0(R2)                                                         
         A     RE,0(R1)                                                         
         ST    RE,0(R2)                                                         
         LA    R1,4(R1)                                                         
         LA    R2,4(R2)                                                         
         BCT   RF,*-20                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL SPDEMUP                                             *         
* INPUT  : R1=A(SPDEMUP'S PARAMETER LIST)                             *         
***********************************************************************         
SPDEMUP  L     RF,=A(SPTDEMUP)                                                  
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM,RWRKD,,0                                             
         B     ROUTSX                                                           
***********************************************************************         
* ROUTINE TO CALL FMTUPGRD                                                      
* INPUT  : R1=A(FMTUPG'S PARAMETER LIST)                                        
*              1ST PARAM           A(UPELEM)                                    
*              2ND PARAM           A(APELEM)                                    
*              3RD PARAM           A(DBLOCK)                                    
***********************************************************************         
FMTUPG   L     RF,=A(FMTUPGRD)                                                  
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM,RWRKD,,0                                             
         B     ROUTSX                                                           
***********************************************************************         
* ROUTINE TO CALL STBRVREC                                                      
* INPUT  : R1=A(SETUPNBR PARAMETER LIST)                                        
*              1ST PARAM           A(BUY RECORD)                                
*              2ND PARAM           A(BUY REVISION RECORD)                       
*              3RD PARAM           A(DBLOCK)                                    
***********************************************************************         
SETUPNBR L     RF,=A(STBRVREC)                                                  
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM,RWRKD,,0                                             
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL GTCOMPRC                                                      
* INPUT  : R1=A(GTCOMPRC PARAMETER LIST)                                        
*              1ST PARAM           A(DAYS BINARY)                               
*              2ND PARAM           A(TIMES BINARY)                              
*              3RD PARAM           A(STATION TEXT)                              
*              4TH PARAM           A(LIST OF DEMO VALUES)                       
***********************************************************************         
GETCMPRC L     RF,=A(GTCOMPRC)                                                  
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM,RWRKD,,0                                             
         B     ROUTSX                                                           
***********************************************************************         
* ROUTINE TO CALL PTCOMPRC                                                      
* INPUT  : R1=A(PTCOMPRC PARAMETER LIST)                                        
*              1ST PARAM           A(DAYS BINARY)                               
*              2ND PARAM           A(TIMES BINARY)                              
*              3RD PARAM           A(STATION TEXT)                              
*              4TH PARAM           A(LIST OF DEMO VALUES)                       
***********************************************************************         
PUTCMPRC L     RF,=A(PTCOMPRC)                                                  
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM,RWRKD,,0                                             
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL GTBKTYPE                                                      
* INPUT  : R1=A(GTBKTYPE PARAMETER LIST)                                        
***********************************************************************         
GETBKTYP L     RF,=A(GTBKTYPE)                                                  
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM,RWRKD,,0                                             
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL ADJPREC                                                       
* INPUT  : R1=A(ADJPRC PARAMETER LIST)                                          
***********************************************************************         
*DJPRC   BRAS  RE,ADJPREC                                                       
ADJPRC   L     RF,=A(ADJPREC)                                                   
         A     RF,ACRELO                                                        
         ST    R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         L     R3,0(R1)            ADJPREC EXPECTS R3 ON INPUT                  
         GOTO1 (RF),ACPARM                                                      
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL ADJDEMO                                                       
* INPUT  : R1=A(ADJDEMO PARAMETER LIST)                                         
***********************************************************************         
ADJDEMO  L     RF,=A(ADJDEMEL)                                                  
         A     RF,ACRELO                                                        
         S     R1,ACPARM+4         PRESERVE THE A(PARAMETER LIST)               
         GOTO1 (RF),ACPARM                                                      
         B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FOR BUY PROGRAMMING PROFILE                                        
* INPUT  : R1=A(PROGRAM FIELD HEADER)                                           
***********************************************************************         
CHKBPPRF DS    0H                                                               
*****    CLI   APROF7,C'C'         CANADIAN AGENCY?                             
*****    BE    CKBPX                                                            
         TM    APROFBTS,A00CANAD   CANADIAN?                                    
         BNZ   CKBPX                                                            
*                                                                               
         TM    CLTIND3,CLTIBPPF    BUY PROG PROFILE ON?                         
         BZ    CKBPX                                                            
         LA    RE,GBPPROF                                                       
         LA    RF,15                                                            
         CLI   0(RE),C'I'                                                       
         LA    RE,1(RE)                                                         
         BNE   CKBP20                                                           
CKBP10   CLC   8(1,R1),0(RE)       MATCHES ONE OF THE BP VALUES?                
         BE    CKBP30                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,CKBP10                                                        
CKBPERR  LR    RE,R1                                                            
         GOTO1 AFVAL,(RE)                                                       
         MVC   FVMSGNO,=AL2(FVFEPGM)                                            
         B     CKBPX               NO, ERROR                                    
*                                                                               
CKBP20   CLC   8(1,R1),0(RE)       ONE OF THE EXCLUDED BP VALUES?               
         BE    CKBPERR             YES, ERROR                                   
         LA    RE,1(RE)                                                         
         BCT   RF,CKBP20                                                        
*                                                                               
CKBP30   CLI   9(R1),C'/'          SHOULD BE SEPARATED WITH A '/'               
         BNE   CKBPERR                                                          
*                                                                               
CKBPX    B     ROUTSX                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET PRIMARY DEMO NAME FOR AN ESTIMATE                    *         
***********************************************************************         
         SPACE 1                                                                
GETDEMNM NTR1                                                                   
         LA    R4,RWORK4                                                        
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOM                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         LA    R1,ESTUSRNM                                                      
         ST    R1,APPARM+12                                                     
         XC    ESTDEMNM,ESTDEMNM                                                
         GOTO1 VDEMOCON,RPARM,(1,ESTDEMS),(2,ESTDEMNM),(C'S',DBLOCK)            
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO READ A CLIENT PROFILE                                    *         
*                                                                     *         
* NTRY - P1=A(2 OR 3-BYTE PROFILE ID)                                 *         
*        P2=A(16-BYTE PROFILE AREA)                                   *         
***********************************************************************         
         SPACE 1                                                                
PROFILE  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         XC    RWORK,RWORK                                                      
         MVC   RWORK(2),=C'S0'                                                  
         MVC   RWORK+2(2),0(R2)                                                 
         CLI   2(R2),C' '          TEST PROFILE ID 3 CHAR LONG                  
         BNH   *+14                                                             
         NI    RWORK,255-X'40'     YES-MAKE LOWER CASE                          
         MVC   RWORK+1(3),0(R2)                                                 
         MVC   RWORK+4(2),CUAALF                                                
         MVC   RWORK+6(1),QMED                                                  
         MVC   RWORK+7(3),QCLT                                                  
         MVI   RWORK+10,C'*'                                                    
         MVC   RWORK+11(1),CLTOFF                                               
         GOTO1 VGETPROF,RPARM,RWORK,(R3),VDMGR                                  
         CLI   8(R1),0                                                          
         BE    EXIT                                                             
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET MONDAY DATE                                          *         
* NTRY - RWORK(6)=DATE                                                *         
* EXIT - RWORK+6(6)=MONDAY                                            *         
***********************************************************************         
         SPACE 1                                                                
GETMON   NTR1                                                                   
         MVC   RWORK+6(6),RWORK                                                 
         GOTO1 VGETDAY,RPARM,RWORK,RFULL                                        
         CLC   RFULL(3),=C'   '                                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,RPARM                                                         
         SR    RF,RF                                                            
         ICM   RF,1,ESTOWSDY                                                    
         BZ    GETMON2                                                          
         SR    RF,RE                                                            
         BZ    GETMONX                                                          
         BM    *+8                                                              
         AHI   RF,-1*7                                                          
         LR    RE,RF                                                            
         B     GETMON4                                                          
*                                                                               
GETMON2  BCTR  RE,0                                                             
         LNR   RE,RE                                                            
         BZ    GETMONX                                                          
*                                                                               
GETMON4  ST    RE,RPARM+8                                                       
         GOTO1 VADDAY,RPARM,RWORK,RWORK+6                                       
*                                                                               
GETMONX  B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
***********************************************************************         
GOMSPACK NTR1  BASE=ACBASE1,WORK=(R4,8),LABEL=NO                                
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         L     R6,ACBASE5                                                       
         USING RWRKD,RC            RC=A(LOCAL W/S)                              
         L     RF,=A(GOMSPK)       REGULAR GETGOAL ROUTINE                      
*                                                                               
         B     MSPKUNPK                                                         
*                                                                               
GOMSUNPK NTR1  BASE=ACBASE1,WORK=(R4,8),LABEL=NO                                
         L     RA,ACBASE2                                                       
         L     R9,ACBASE3                                                       
         L     R8,ACBASE4                                                       
         L     R6,ACBASE5                                                       
         USING RWRKD,RC            RC=A(LOCAL W/S)                              
         L     RF,=A(GOMSUP)       REGULAR GETGOAL ROUTINE                      
*                                                                               
MSPKUNPK LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         A     RF,ACRELO                                                        
         GOTO1 (RF),ACPARM,RWRKD,(R4),(R5)                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 2                                                                
XFF      DC    X'FFFF'                                                          
DMREAD   DC    CL7'DMREAD'                                                      
DMWRITE  DC    CL7'DMWRT '                                                      
TEMPSTR  DC    CL7'TEMPSTR'                                                     
OVERELEM EQU   X'DE'                                                            
*                                                                               
* SPECIAL TYPES TABLE - NOTE: MASKS ARE BIT COMBINATIONS                        
* NOTE: COPY OF BKTYPTAB WHICH WE CANNOT ACCESS                                 
BKTYPTB1 DS    0XL2                 SEE BOOKTYPD FOR SETTINGS                   
         DC    AL1(BTY2CHAR),X'FE'  - 2 CHARACTER BOOK TYPES + L                
         DC    AL1(BTYZR4Q),C'4'    - ZERO 4                                    
         DC    AL1(BTYZR1Q),C'1'    - ZERO 1                                    
         DC    AL1(BTYHPEOQ),C'I'   - HISPANIC PEOPLE METER                     
         DC    AL1(BTYOLYMQ),C'O'   - OLYMPICS                                  
         DC    AL1(BTYPRNTQ),C'A'   - PARENT ONLY DATA                          
         DC    AL1(BTYHISPQ),C'H'   - HISPANIC                                  
         DC    AL1(BTYBLAKQ),C'B'   - BLACK                                     
         DC    AL1(BTYPEOPQ),C'P'   - PEOPLE METER                              
         DC    AL1(BTYTRADQ),C'T'   - TRADE                                     
         DC    AL1(BTYMTROQ),C'M'   - METRO                                     
         DC    AL1(BTYDMAQ),C'D'    - DMA                                       
         DC    AL1(BTYOTHRQ),C'E'   - OTHER                                     
         DC    AL1(0)                                                           
*                                                                               
RADIOAGY DS    0CL2                                                             
         DC    CL2'BS'                                                          
         DC    CL2'BD'                                                          
         DC    CL2'BN'                                                          
         DC    CL2'BW'                                                          
         DC    CL2'BB'                                                          
         DC    CL2'MI'                                                          
         DC    CL2'DF'                                                          
         DC    CL2'SF'                                                          
         DC    CL2'DT'                                                          
         DC    CL2'CE'                                                          
         DC    CL2'LI'                                                          
         DC    CL2'NE'                                                          
         DC    CL2'DM'       FOR BBDO BUYING ON DMAT                            
         DC    CL2'TH'       FOR ZENITH                                         
         DC    CL2'YN'       FOR YOUNG & RUBICAM, U.S.                          
         DC    CL2'YR'       FOR YOUNG & RUBICAM, CANADA                        
         DC    CL2'EZ'       FOR DOREMUS                                        
         DC    CL2'OH'       FOR DWP BATES                                      
         DC    CL2'H9'       FOR STARCOM                                        
         DC    CL2'H0'       FOR EXCELERATOR                                    
         DC    CL2'HD'       <---- TESTER                                       
         DC    CL2'SJ'       <---- TESTER                                       
         DC    CL2'H1'       FOR DDSUCAN                                        
         DC    XL2'0000'                                                        
         EJECT                                                                  
***********************************************************************         
* ALL AGENCIES ARE NOT ALLOWED TO DO THESE ACTIONS                              
***********************************************************************         
ACTIONOK NTR1  BASE=*,LABEL=*                                                   
         CLI   INACT,ACTADD        ADD?                                         
         BE    ACTNO                - CAN'T DO IT                               
         CLI   INACT,ACTXFR        TRANSFER?                                    
         BE    ACTNO                - CAN'T DO IT                               
         CLI   INACT,ACTTFR        TRANSFER?                                    
         BE    ACTNO                - CAN'T DO IT                               
         CLI   INACT,ACTERA        ERASE?                                       
         BE    ACTNO                - CAN'T DO IT                               
         CLI   INACT,ACTFIX        FIX?                                         
         BE    ACTNO                - CAN'T DO IT                               
         CLI   INACT,ACTBYA        BUYADD?                                      
         BE    ACTNO                - CAN'T DO IT                               
*                                                                               
ACTYES   SR    RF,RF                                                            
*                                                                               
ACTNO    LTR   RF,RF                                                            
         B     EXIT                                                             
***********************************************************************         
* GET ESTIMATE INFO TO PUT INTO THE SAVAREA FIELDS SVCOST2 & SVELOCK            
*                                                                               
* ON ENTRY:    (R2)                ESTIMATE HEADER                              
***********************************************************************         
         USING ESTHDRD,R2                                                       
GETESTIN NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
*****                                                                           
         TM    EFLAG1,EF1SDE       SUPERDESK AUTHORIZATION ON?                  
         BZ    GETEIN00                                                         
         LR    RE,R5                                                            
         AHI   RE,SVINDS-TWAD                                                   
         OI    0(RE),SVIEAUTH     YES, INDICATE AS SUCH                         
*****                                                                           
GETEIN00 OC    ECOST2,ECOST2       USE ESTIMATE'S COST2 ?                       
         BZ    *+10                NO                                           
         MVC   SVCOST2,ECOST2                                                   
*                                                                               
         MVI   SVESLN,0                                                         
         OC    ESLN,ESLN           USE ESTIMATE'S BUY ONLY THIS SLN?            
         BZ    *+10                NO                                           
         MVC   SVESLN,ESLN                                                      
*                                                                               
         MVI   STABKTYP,0          CLEAR STATION OVERRIDE BOOKTYPE              
         CLI   EBKTYPE,0           ANY OVERRIDE BOOK TYPE IN ESTIMATE?          
         BE    GETEIN05            NONE, LEAVE STABKTYP AS NULL                 
         OI    ESTIND,ESTIBTYP     YES, KEEP IT IN STABKTYP                     
         MVC   STABKTYP,EBKTYPE                                                 
*                                                                               
GETEIN05 OC    SVCLOCK,SVCLOCK     ANY CLIENT LOCK MONTH?                       
         BZ    GETEIN10                                                         
         MVC   SVELOCK,SVCLOCK     YES, TAKES PRIORITY OVER ESTIMATE'S          
         B     GETEIN20                                                         
*                                                                               
GETEIN10 XC    SVELOCK(SVELKNDT+L'SVELKNDT-SVELOCK),SVELOCK                     
         OC    ELOCKYM,ELOCKYM     USE ESTIMATE'S LOCK MONTH                    
         BZ    GETEINX             NO                                           
         MVC   SVELOCK,ELOCKYM                                                  
*                                                                               
GETEIN20 MVC   ACDUB(2),SVELOCK                                                 
         DROP  R1                                                               
         NI    ACDUB+1,X'3F'       DROP PRIOR/SUBSEQ FLAGS                      
         MVI   ACDUB+2,1                                                        
         GOTO1 VDATCON,ACPARM,(3,ACDUB),ACWORK                                  
         GOTO1 VGTBROAD,ACPARM,(1,ACWORK),ACWORK+6,VGETDAY,VADDAY               
         GOTO1 VDATCON,ACPARM,ACWORK+6,(2,ACFULL)                               
         GOTO1 (RF),(R1),ACWORK+12,(2,ACFULL+2)                                 
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
         MVC   SVELKSDT(L'ACFULL),ACFULL                                        
         TM    SVELOCK+1,X'80'     TEST MONTH AND PRIOR                         
         BZ    *+10                                                             
         XC    SVELKSDT,SVELKSDT                                                
         TM    SVELOCK+1,X'40'     TEST MONTH AND SUBSEQUENT                    
         BZ    *+12                                                             
         LHI   R0,-1                                                            
         STCM  R0,3,SVELKNDT                                                    
GETEINX  J     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* DEMO UPGRADES FOR WORK RECORD AND ADD DEMO ELEMENT                  *         
***********************************************************************         
DEMUP    NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA2                                                      
         USING BWDRECD,R3                                                       
         LA    R0,14               BUILD DEMO LIST FOR SPDEMUP                  
         LA    R1,RWORK2                                                        
         LA    RE,ESTDEMS                                                       
*                                                                               
DEMU2    OC    0(3,RE),0(RE)                                                    
         BZ    DEMU4                                                            
         MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,DEMU2                                                         
*                                                                               
DEMU4    MVI   0(R1),X'FF'                                                      
         XC    RWORK3,RWORK3                                                    
         LA    R4,RWORK4           BUILD SPDEMUP BLOCK                          
         USING SPDEMUPD,R4                                                      
         XC    SPDEMUPD(SPDEMUP2),SPDEMUPD                                      
         LA    R1,RIO2                                                          
         ST    R1,SPUPAREC                                                      
         MVC   SPUPAFAC,ACOM                                                    
         MVC   SPUPAGY,CUAALF                                                   
         MVC   SPUPMED,CUDMED                                                   
         MVC   SPUPCLI,QCLT                                                     
         MVC   SPUPMKT,MKTLKUP                                                  
*****  CABLE/FUSION DATE LOOKUP                                                 
         CLI   QSTA,C'0'           IS IT A NUMBER?                              
         BL    DEMU4E               - NOPE, WE DON'T HAVE CABLE NETWORK         
         XC    SPUPSTA,SPUPSTA                                                  
         MVC   SPUPSTA(3),QSTA+5   MOVE THE NETWORK IN                          
         MVC   SPUPSYSE,QSTA                                                    
         B     DEMU4G                                                           
*****  CABLE/FUSION DATE LOOKUP                                                 
DEMU4E   MVC   SPUPSTA,LUPSTA                                                   
DEMU4G   MVC   SPUPDAY,BWDDAYS                                                  
         MVC   SPUPTIM,BWDTIMES                                                 
         MVC   SPUPFIL,LUPFIL                                                   
         MVC   SPUPSRC,CLTSRC                                                   
         MVC   SPUPFBK,LUPFRBK                                                  
         MVC   SPUPFBKL,LUPFRBKL                                                
         OC    SPUPFBK,SPUPFBK                                                  
         BNZ   *+16                                                             
         MVC   SPUPFBK,ESTBOOK                                                  
         XC    SPUPFBKL,SPUPFBKL                                                
         MVC   SPUPAOVR,LAOVREL                                                 
         MVC   SPUPUDAY,LUPDAY                                                  
         MVC   SPUPUTIM,LUPTIM                                                  
         MVC   SPUPTYPE(8),LUPGRD                                               
         MVC   SPUPBTYP,STABKTYP                                                
*                                                                               
         CLI   CMPBKTYP,0          OVERRIDING BOOKTYPE?                         
         BE    *+10                                                             
         MVC   SPUPBTYP,CMPBKTYP                                                
*                                                                               
         CLI   QBOOKTYP,0          OVERRIDING BOOKTYPE?                         
         BE    *+10                                                             
         MVC   SPUPBTYP,QBOOKTYP                                                
*                                                                               
         TM    SPUPFBK+1,BTYBITSQ  DO WE HAVE SPECIAL BOOK TYPE?                
         BZ    DEMU5                                                            
         TM    SPUPFBK+1,BTY2CHAR  IS IT 2 CHARACTER BOOKTYPE?                  
         BO    DEMU5                - YES, SPUPBTYP IS READY                    
         MVC   RBYTE,SPUPFBK+1                                                  
         NI    RBYTE,X'FF'-BMNBITSQ   REMOVE MONTH FOR TEST                     
         NI    SPUPFBK+1,X'FF'-BTYBITSQ                                         
         LA    RE,BKTYPTB1         (CAN'T ADDRESS BKTYPTAB)                     
DEMU4T   CLI   0(RE),0                                                          
         BE    DEMU5               NOT IN TABLE!                                
         CLC   RBYTE,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,L'BKTYPTB1(RE)                                                
         B     DEMU4T                                                           
         MVC   SPUPBTYP,1(RE)                                                   
*                                                                               
DEMU5    CLI   CUDMED,C'C'         TEST CANADA                                  
         BNE   *+16                                                             
         TM    CLTIND2,CLTIANFR    AND 1W ANGLO/FRANCO OPTION ON                
         BZ    *+8                                                              
         OI    SPUPOPTS,SPOANGFR   YES                                          
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   LUPPUT,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'N'                                                    
         CLI   LUPPUT,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRP,C'Y'                                                    
         CLI   LUPSHR,C'1'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'N'                                                    
         CLI   LUPSHR,C'2'                                                      
         BNE   *+8                                                              
         MVI   SPUP2YRR,C'Y'                                                    
*                                                                               
***  2 DECIMAL  ***                                                             
         TM    APROFBTS,A00TWODC   ARE WE DOING 2 DECIMALS?                     
         BNO   DEMU5T                                                           
         OI    SPUPOPTS,SPOP2DEC    - YUP, SPECIAL 2 DECIMAL LOOKUP             
***  2 DECIMAL  ***                                                             
DEMU5T   GOTO1 ASPDEMUP,RPARM,RWORK4,RWORK2,RWORK3     CALL SPDEMUP             
*                                                                               
         MVC   BWDBOOK,SPUPFBK     ACTUAL UPGRADE BOOK                          
         TM    BWDINDS,BWDIPRG+BWDIPSID   TEST PROGRAM OVERRIDE OR              
         BNZ   DEMU6                      PROGRAM NAME FROM SID                 
         MVC   BWDPROG,RSPACES            NO-USE UPGRADE PROGRAM                
         MVC   BWDPROG(L'SPUPPRG),SPUPPRG                                       
         LA    R0,L'BWDPROG-2                                                   
         LA    R1,BWDPROG+L'BWDPROG-2                                           
         CLC   0(2,R1),=C'-S'                                                   
         BE    *+14                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-12                                                          
         B     DEMU6                                                            
         MVC   0(2,R1),RSPACES                                                  
*                                                                               
DEMU6    XC    APELEM,APELEM       BUILD DEMO ELEMENT                           
         LA    R4,APELEM                                                        
         USING DMOEL,R4                                                         
         MVI   DMOELCD,DMOELCDQ                                                 
         SR    R0,R0                                                            
         LA    R1,DMODEMO                                                       
         LA    RE,RWORK2                                                        
         LA    RF,RWORK3                                                        
*                                                                               
DEMU8    CLI   0(RE),X'FF'         TEST NO MORE DEMOS                           
         BE    DEMU10                                                           
         MVC   1(2,R1),1(RE)       DEMO                                         
         MVC   4(4,R1),0(RF)       DEMO VALUE                                   
         CLI   0(RE),EDOCODEQ                                                   
         BNE   *+8                                                              
         OI    4(R1),DMODEMOV      OVERRIDE                                     
         LA    R1,L'DMODEMO(R1)    NEXT DEMO                                    
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         B     DEMU8                                                            
*                                                                               
DEMU10   SR    R1,R4               LENGTH OF DEMO ELEMENT                       
         CHI   R1,2                                                             
         BNH   DEMUX                                                            
         STC   R1,DMOELLN                                                       
         GOTO1 AADDELS,BWDRECD     ADD DEMO ELEMENT                             
*                                                                               
DEMUX    B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADJUST NWS/BUY DEMO ELEMENT (SAME FOR NWSDTL AND NWSBRV)                      
*                                                                               
* ON ENTRY:    (N OR B,R1)          NWS OR BUY,A(NWS DEMO ELEMENT)              
***********************************************************************         
ADJDEMEL NTR1  BASE=*,LABEL=*                                                   
         ST    R1,APWORK                                                        
         XC    APBYTE,APBYTE                                                    
         ICM   R1,8,APBYTE         CLEARING OUT THE C'N' OR C'B'                
****  IF I DON'T CLEAR THIS, CR LATER THINKS REGISTER IS NEGATIVE NUM           
         LR    RF,R1                                                            
         CLI   APWORK,C'B'         IS IT BUY RECORD DEMO ELEMENT?               
         BE    ADJDMBUY             - YUP IT IS                                 
*                                                                               
ADJDMNWS DS    0H                                                               
         ZIC   R1,DMOELLN-DMOEL(RF)                                             
         AR    R1,RF                 R1 = END BOUNDARY                          
         LA    R3,DMODEMO-DMOEL(RF)  R3 = A(1ST DEMO VALUE)                     
         B     ADJDEM10                                                         
*                                                                               
ADJDMBUY DS    0H                                                               
         ZIC   R1,NDLEN-NDELEM(RF)                                              
         AR    R1,RF                 R1 = END BOUNDARY                          
         LA    R3,NDEMNO-NDELEM(RF)  R3 = A(1ST DEMO VALUE)                     
*                                                                               
ADJDEM10 BRAS  RE,ADJPREC                                                       
         AHI   R3,8                                                             
         CR    R3,R1               GO THROUGH ALL THE DEMO VALUES               
         BL    ADJDEM10                                                         
*                                                                               
ADJDEMX  J     EXIT                                                             
***********************************************************************         
* ADJUST DEMO DECIMAL PRECISION                                                 
*                                                                               
* ON ENTRY:    (R3)                 A(DEMO 8 BYTE)                              
***********************************************************************         
ADJPREC  NTR1  BASE=*,LABEL=*                                                   
         CLI   1(R3),C'R'          SEE IF DEMO IS A RATING                      
         BE    ADJPRC10                                                         
         CLI   1(R3),C'E'                                                       
         BNE   ADJPRECX                                                         
*                                                                               
ADJPRC10 MVC   ACFULL(1),4(R3)        GET VALUE AND FLAGS                       
*                                                                               
         TM    ACFULL,X'40'        DEMO HAS 2 DEC                               
         BO    ADJPRC20            YES                                          
***********************************                                             
* THIS COMMENTED BLOCK IS HERE UNTIL WE CAN DISPLAY/REPORT 2 DEC                
***********************************                                             
*****    CLI   APROF9,C'Y'         NO, SUPPORT 2 DEC                            
         TM    APROFBTS,A00TWODC                                                
         BZ    ADJPRECX                NO - JUST EXIT                           
*                                                                               
         NI    4(R3),X'3F'         YES, DROP FLAGS FROM VALUE                   
         ICM   R0,15,4(R3)         ADJUST 1 DECIMAL PRECISION TO 2              
         MHI   R0,10                                                            
         STCM  R0,15,4(R3)                                                      
         OI    4(R3),X'40'         SET 2 DEC PRECISION FLAG                     
         B     ADJPRC22                                                         
*=========================================================                      
* DEMO HAS 2 DEC                                                                
*=========================================================                      
ADJPRC20 DS    0H                                                               
***********************************                                             
* THIS COMMENTED BLOCK IS HERE UNTIL WE CAN DISPLAY/REPORT 2 DEC                
***********************************                                             
*****    CLI   APROF9,C'Y'         USER WANT 2 DEC                              
         TM    APROFBTS,A00TWODC                                                
         BO    ADJPRECX            YES - DONE                                   
*                                                                               
         XR    R0,R0               CLEAR OUT IN CASE IT HAS SOMETHIN'           
         NI    4(R3),X'3F'         DROP FLAGS FROM VALUE                        
         ICM   R1,15,4(R3)         ADJUST 2 DECIMAL PRECISION TO 1              
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         STCM  R1,15,4(R3)                                                      
*                                                                               
ADJPRC22 TM    ACFULL,X'80'        TEST OVERRIDE FLAG WAS SET                   
         BZ    *+8                                                              
         OI    4(R3),X'80'         SET IT BACK ON                               
*                                                                               
ADJPRECX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE BUYER CODE                                      *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF BUYER FIELD)                            *         
*                                                                     *         
* EXIT - CC=EQUAL IF BUYER IS VALID WITH BUYER VALUES EXTRACTED       *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
         SPACE 1                                                                
VALIBYR  NTR1  BASE=*,LABEL=*                                                   
         MVI   FVMINL,1            REQUIRED FIELD                               
         MVI   FVMAXL,L'QBYR                                                    
         GOTO1 AFVAL                                                            
         BNE   VALBYRX                                                          
         CLI   BBYR,0              THIS HAPPENS FROM CAM/LIST                   
         BE    *+14                                                             
         CLC   QBYR,FVIFLD         TEST CHANGE OF BUYER                         
         BE    VALBYRX                                                          
         LA    R2,IOKEY            BUILD KEY OF BUYER RECORD                    
         USING BYRRECD,R2                                                       
         XC    BYRKEY,BYRKEY                                                    
         MVI   BYRKTYP,BYRKTYPQ                                                 
         MVI   BYRKSUB,BYRKSUBQ                                                 
         MVC   BYRKAGMD,BAGYMD                                                  
         MVC   BYRKBYR,FVIFLD                                                   
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   VALBYR9                                                          
         LA    R2,RIO                                                           
         MVC   BYRNM,BYRNAME                                                    
         MVC   BYRPW,BYRPASS                                                    
         XC    BBYR(BVALSX-BBYR),BBYR                                           
         MVC   BBYR,BYRCODE                                                     
         MVC   BBYRMASK,BYRMASK                                                 
         XC    QBYR(QVALSX-QBYR),QBYR                                           
         MVC   QBYR,FVIFLD                                                      
         B     VALBYRX                                                          
*                                                                               
VALBYR9  MVC   FVMSGNO,=AL2(FVIBYR)                                             
*                                                                               
VALBYRX  J     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET ESTIMATE DEMOS FOR ANY PRODUCT IN THE CAMPAIGN'S     *         
* PRODUCT GROUP. FIRST DEMO IS THE TARGET DEMO FOR ANY PRODUCT IN THE *         
* PRODUCT GROUP THAT HAS AN OPEN ESTIMATE. SECOND DEMO IS THE IMPS OF *         
* THAT DEMO IF IT'S SECOND FOR THE ESTIMATE. THE REST OF THE DEMOS    *         
* ARE THE PRODUCT POL ESTIMATE DEMOS.                                 *         
* NTRY - RIO CONTAINS POL ESTIMATE RECORD                            *          
* EXIT - CC SET TO NE IF THERE IS AN ERROR                            *         
***********************************************************************         
         SPACE 1                                                                
GETPGDEM NTR1  BASE=*,LABEL=*                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R3,IOKEY            READ PRODUCT GROUP PASSIVES TO               
         USING PRGRECD,R3          FIND A PRODUCT IN THE GROUP                  
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,BAGYMD                                                  
         MVC   PRGPCLT,BCLT                                                     
         MVC   PRGPID,CMPPGRP                                                   
         MVC   PRGPGRP,CMPPGRP+1                                                
         SR    R4,R4               INDICATE NO PRODUCTS FOUND YET               
         MVC   RBYTE,CMPPGRPN                                                   
         NI    RBYTE,X'7F'                                                      
         LA    R5,PRGPGRP-PRGKEY-1 SET R5 FOR EXECUTED COMPARE                  
         CLI   CMPPGRPN,X'81'                                                   
         BE    GETPG2                                                           
         LA    R5,1(R5)                                                         
         CLI   RBYTE,3                                                          
         BNE   GETPG2                                                           
         LA    R5,1(R5)                                                         
*                                                                               
GETPG2   LA    R1,DIRHI                                                         
         B     GETPG6                                                           
*                                                                               
GETPG4   LA    R1,DIRSQ                                                         
*                                                                               
GETPG6   DS    0H                                                               
         GOTO1 AIO                                                              
         BNE   GETPG99                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   PRGKEY(0),IOKEYSAV                                               
         BNE   GETPG99                                                          
         CLI   CMPPGRPN,X'81'                                                   
         BNE   GETPG8                                                           
         MVC   RBYTE,PRGPGRP                                                    
         NI    RBYTE,X'F0'                                                      
         CLC   RBYTE,CMPPGRP+1                                                  
         BNE   GETPG99                                                          
*                                                                               
GETPG8   LTR   R4,R4               INDICATE A PRODUCT'S BEEN FOUND              
         BNZ   *+8                                                              
         LA    R4,1                                                             
         MVC   RWORK2(L'IOKEY),IOKEY                                            
         XC    IOKEY,IOKEY                                                      
         LA    R1,RIO              READ ESTIMATE RECORD FOR THIS PRD            
         MVC   IOKEY(L'EKEY),0(R1)                                              
         LA    R2,IOKEY                                                         
         USING ESTHDRD,R2                                                       
         MVC   EKEYPRD,RWORK2+(PRGPPRD-PRGKEY)                                  
         LA    R2,RIO2                                                          
         ST    R2,IOADDR                                                        
         GOTO1 AIO,FILRD                                                        
         BE    GETPG12             FOUND                                        
*                                                                               
GETPG10  MVC   IOKEY,RWORK2        READ NEXT PRDGRP PASSIVE                     
         GOTO1 AIO,DIRHI                                                        
         BNE   GETPG99                                                          
         B     GETPG4                                                           
*                                                                               
GETPG12  CHI   R4,2                TEST FIRST ESTIMATE HAS BEEN FOUND           
         BNE   *+18                                                             
         CLC   ESTDEMS(3),EDEMLST  YES-TEST THIS EST HAS SAME TARGET            
         BNE   GETPG98                 NO-ERROR                                 
         B     GETPG10                                                          
         LA    R4,2                INDICATE AN ESTIMATE'S BEEN FOUND            
         XC    ESTDEMS,ESTDEMS     SET DEMO LIST                                
         MVC   ESTDEMS(3),EDEMLST  PRIMARY DEMO                                 
         LA    R1,ESTDEMS+3                                                     
         LA    R0,19                                                            
         MVI   ACFULL,0                                                         
         CLI   EDEMLST+1,C'R'      TEST 2ND DEMO IS IMPS OF FIRST               
         BE    *+12                                                             
         CLI   EDEMLST+1,C'E'                                                   
         BNE   GETPG14                                                          
         CLI   EDEMLST+4,C'I'                                                   
         BNE   GETPG14                                                          
         CLC   EDEMLST+2(1),EDEMLST+5                                           
         BNE   GETPG14                                                          
         MVC   ESTDEMS+3(3),EDEMLST+3   YES-MAKE THAT THE 2ND DEMO              
         LA    R1,3(R1)                                                         
         BCTR  R0,0                                                             
         MVI   ACFULL,1                                                         
*                                                                               
GETPG14  LA    RE,RIO              REST OF DEMOS COME FROM POL ESTIMATE         
         LA    RE,EDEMLST-ESTHDRD(RE)                                           
*                                                                               
GETPG16  OC    0(3,RE),0(RE)                                                    
         BZ    GETPG20                                                          
         CLC   0(3,RE),ESTDEMS     DON'T REPEAT PRIMARY                         
         BE    GETPG18                                                          
         CLI   ACFULL,0                                                         
         BE    *+14                                                             
         CLC   0(3,RE),ESTDEMS+3   OR SECONDARY                                 
         BE    GETPG18                                                          
         MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
*                                                                               
GETPG18  LA    RE,3(RE)                                                         
         BCT   R0,GETPG16                                                       
*                                                                               
GETPG20  B     GETPG10             GOTO NEXT PRODUCT                            
*                                                                               
GETPG98  MVC   FVMSGNO,=AL2(FVIPRIM)  PRIMARY DEMOS MUST MATCH                  
         B     GETPGX                                                           
*                                                                               
GETPG99  LTR   R4,R4               TEST PRODUCT FOUND                           
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVNOPRPG)  NO PRODUCTS FOUND FOR PRDGRP             
         B     GETPGX                                                           
         CHI   R4,2                TEST ESTIMATE FOUND                          
         BE    GETPGX                                                           
         MVC   FVMSGNO,=AL2(FVIESTPG)  INVALID ESTIMATE FOR PRDGRP              
*                                                                               
GETPGX   CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK IF WE'RE GETTING CALLED FROM ANOTHER PROGRAM VIA GLOBBER                
***********************************************************************         
CALLNWS  NTR1  BASE=*,LABEL=*                                                   
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL GLOBBER                                 
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    CNWSX                                                            
*                                                                               
         GOTO1 (RF),ACPARM,=C'GETD',APELEM,24,GLVXCTL                           
         CLI   8(R1),0                                                          
         BNE   CNWSX                                                            
         GOTO1 (RF),(R1),=C'DELE'  GET RID OF IT QUICKLY !                      
*                                                                               
         LA    R1,APELEM           SAVE CALLING SYS/PRG FOR OVERLAYS            
         USING GLVXFRSY,R1                                                      
         CLC   GLVXFRSY(6),=C'SPOTAM'                                           
         BNE   CNWSX                                                            
         TM    GLVXFLG1,GLV1RETN                                                
         BNZ   CNWSX                                                            
         OI    TWATAMFL,TWAFRTAM+TWAWSSVR  CAME FROM TV AVAIL MANAGER           
         XC    TWARDBFR,TWARDBFR                                                
         DROP  R1                                                               
*                                                                               
         L     RF,ASYS            A(SYSFACS)                                    
         L     RF,VCALLOV-SYSFACD(RF)                                           
         GOTO1 (RF),ACPARM,(X'FC',BWSTABH),0  LOAD UP WORK/MAINT SCRN           
         CLI   4(R1),X'FF'         ERROR FROM CALLOV?                           
         BNE   *+6                 NO                                           
         DC    H'0'                YES, DIE                                     
         MVI   TWASCRN,X'FC'       TO FOOL GENERAL SO IT WON'T LOAD SCR         
*                                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL GLOBBER                                 
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
*                                                                               
         XC    APELEM,APELEM                                                    
         GOTO1 (RF),ACPARM,=C'GETD',APELEM,60,GLVSMSG                           
         CLI   8(R1),0                                                          
         BNE   CNWSX                                                            
*                                                                               
         MVC   BWSREC(4),=C'WORK'                                               
         MVI   BWSRECH+5,4                                                      
         MVC   BWSACT(3),=C'ADD'                                                
         MVI   BWSACTH+5,4                                                      
*                                                                               
         LA    R1,APELEM                                                        
         USING TAMDATAD,R1                                                      
         MVC   DETMED,TAMMED                                                    
         MVI   DETMEDH+5,1                                                      
*                                                                               
         MVI   DETBYRH+5,3                                                      
         MVC   DETBYR(3),TAMBYR                                                 
         CLI   DETBYR+2,C' '                                                    
         BH    *+8                                                              
         MVI   DETBYRH+5,2                                                      
*                                                                               
         MVC   DETNUM,TAMCAMP                                                   
         MVI   DETNUMH+5,5                                                      
*                                                                               
         MVC   BWSPWD,TAMPASS                                                   
*                                                                               
********   ADDING A WKMASK FROM TAM                                             
         CLC   TAMVERS,=X'02030000'                                             
         BL    CNWS05                                                           
         OI    TWATAMFL,TWAWKMSK   WE HAVE AN INACTIVE WEEK MASK                
********   ADDING A WKMASK FROM TAM      MHC  09/05/06                          
*                                                                               
CNWS05   CLC   TAMVERS,=X'02020002'   IS IT THE OLDER VERSION?                  
         BNH   CNWS08                  - YUP, NO REP OR COST2 POSSIBLE          
*                                                                               
         OI    TWATAMFL,TWACS2        WE ALWAYS HAVE A COST2                    
         NI    DETC2NH+1,X'FF'-X'20'     AND NO LONGER PROTECTED                
         OI    DETC2NH+6,X'80'                                                  
         OI    TWATAMFL,TWAREP                AND A REP                         
         DROP  R1                                                               
*                                                                               
CNWS08   LA    R2,BWSPWD           COUNT HOW MANY BYTES IN PASSWD               
         LA    R1,BWSPWD+L'BWSPWD-1                                             
CNWS10   CR    R1,R2                                                            
         BNL   *+10                                                             
         SR    R1,R1               NOTHING IN PASSWORD                          
         B     CNWS25                                                           
*                                                                               
         CLI   0(R1),C' '                                                       
         BNH   CNWS15                                                           
         AHI   R1,1                                                             
         B     CNWS20                                                           
*                                                                               
CNWS15   BCT   R1,CNWS10                                                        
*                                                                               
CNWS20   SR    R1,R2                                                            
CNWS25   STC   R1,BWSPWDH+5                                                     
*                                                                               
CNWSX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READS WORK/ADD FROM THE WSSVR BUFFER TO FEED INTO THE WORK/ADD OVERLY         
***********************************************************************         
FROMTAM  NTR1  BASE=*,LABEL=*                                                   
         TWAXC DETSTAH,DETCM5H     CLEAR SCREEN TO NOT GET OVERRIDES            
*                                                                               
********   ADDING A WKMASK FROM TAM                                             
         TM    TWATAMFL,TWAWKMSK   WE HAVE AN INACTIVE WEEK MASK                
         BZ    FTAM00                                                           
         NI    DETMSKH+1,X'FF'-X'20'   TAKE OFF PROTECTION                      
         XC    DETMSK,DETMSK                                                    
********   ADDING A WKMASK FROM TAM      MHC  09/05/06                          
*                                                                               
FTAM00   NI    TWATAMFL,X'FF'-TWATAMER   NO ERROR YET FROM THIS ROUTINE         
*                                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           GET A(FAWSSVR)                               
         L     RF,CWSSVR-COMFACSD(RF)                                           
*                                                                               
         XC    ACWORK,ACWORK                                                    
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         LHI   R0,16384            SAVE OFF 16K OF THE TIA                      
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ACWORK,ACWORK       NOW PULL IN THE DATA FROM TAM                
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSURST                                                
         LHI   R0,16384                                                         
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
         L     R6,ATIA             R6 = A(WORK RECORD)                          
         XR    RE,RE                                                            
         ICM   RE,3,TWARDBFR                                                    
         AR    R6,RE                                                            
*                                                                               
         TM    TWATAMFL,TWAWSSVR   1ST TIME READING?                            
         BZ    *+12                                                             
         NI    TWATAMFL,X'FF'-TWAWSSVR   NOT 1ST TIME ANYMORE                   
         B     FTAM10                                                           
*                                                                               
         ICM   RE,3,0(R6)                                                       
         AR    R6,RE                                                            
         LR    RE,R6                                                            
         L     RF,ATIA                                                          
         SR    RE,RF                                                            
         STCM  RE,3,TWARDBFR       UPDATE DISPLACEMENT TO ENTRY                 
*                                                                               
         CLI   0(R6),X'FF'         END OF BUFFER?                               
         BNE   FTAM10                                                           
         OI    TWATAMFL,TWAWSSDN   YES, DONE WITH WSSVR BUFFER                  
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL GLOBBER                                 
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
         BZ    FTAMX                                                            
*                                                                               
         XC    APELEM,APELEM                                                    
         LA    R1,APELEM           SAVE CALLING SYS/PRG FOR OVERLAYS            
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'NWS'    NWS PROGRAM                                  
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'TAM'    TAM PROGRAM                                  
         OI    GLVXFLG1,GLV1RETN+GLV1SEPS   RETURN BACK                         
         GOTO1 (RF),ACPARM,=C'PUTD',APELEM,24,GLVXCTL                           
         DROP  R1                                                               
         B     FTAMX                                                            
*                                                                               
FTAM10   LA    R2,DETSTAH                                                       
         XR    R1,R1                                                            
         ICM   R1,3,0(R6)          R1 = A(NEXT ENTRY)                           
         LA    R1,0(R1,R6)                                                      
         LA    R6,8(R6)            POINT TO FIRST PIECE OF DATA                 
*                                                                               
FTAM20   LA    RF,8(R2)            RF = A(PUT PIECE OF DATA ON FIELD)           
         ZIC   RE,0(R2)            RF SHOULD NEVER REACH RE OTHERWISE           
         SHI   RE,8                   FIELD WILL GET TRASHED                    
         TM    1(R2),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SHI   RE,8                                                             
         AR    RE,RF                                                            
*                                                                               
FTAM30   CR    R6,R1               ARE WE FINISHED WITH THIS ENTRY?             
         BNL   FTAM40                                                           
         CLC   DETMSKH,0(RF)                                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R6),X'4F'         PIPE (C'|') DELIMITED                        
         BE    FTAM40                                                           
         CR    RF,RE               ARE WE STILL WITHIN THE FIELD?               
         BNL   FTAM33                                                           
         MVC   0(1,RF),0(R6)                                                    
         LA    RF,1(RF)                                                         
         LA    R6,1(R6)                                                         
         B     FTAM30                                                           
*                                                                               
FTAM33   LR    R1,R2               NO, THIS FIELD IS IN ERROR                   
         LA    R2,FVINPTLG                                                      
         SLL   R2,16               ERROR CODE IN HIGH ORDER NIBBLE              
*                                                                               
         SR    R1,R5                   AS THE INPUT IS TOO LONG                 
         LA    RF,INPTFLD2                                                      
FTAM35   OC    0(2,RF),0(RF)                                                    
         BZ    FTAM37                                                           
         CLM   R1,3,0(RF)                                                       
         BE    FTAM39                                                           
         AHI   RF,L'INPTFLD2                                                    
         B     FTAM35                                                           
*                                                                               
FTAM37   SHI   RF,L'INPTFLD2                                                    
FTAM39   ICM   R2,2,2(RF)                                                       
*                                                                               
         L     R6,ATIA                                                          
         XR    RE,RE                                                            
         ICM   RE,3,TWARDBFR                                                    
         AR    R6,RE                                                            
         STCM  R2,15,4(R6)         SAVE ERROR AMD FIELD NUMBER                  
         OI    TWATAMFL,TWATAMER   ERROR FROM THIS ROUTINE                      
*                                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           GET A(FAWSSVR)                               
         L     RF,CWSSVR-COMFACSD(RF)                                           
*                                                                               
         XC    ACWORK,ACWORK       SAVE THE ERROR BACK                          
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         LHI   R0,16384                                                         
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
         B     FTAMX                                                            
*                                                                               
FTAM40   LA    R0,8(R2)            CALCULATE THE LENGTH                         
         SR    RF,R0                                                            
         STC   RF,5(R2)            AND STORE IT                                 
         LTR   RF,RF               NOTHING HERE?                                
         BZ    FTAM45                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    8(0,R2),=CL80' '   DO WHAT INPUT TRANSLATOR DOES                 
FTAM45   LA    R6,1(R6)            SKIP THE DELIMITER                           
*                                                                               
FTAM50   ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
*                                                                               
         LA    RF,DETCM5H          AFTER LAST COMMENT FIELD?                    
         CR    R2,RF                                                            
         BH    FTAMX                                                            
*                                                                               
**  COST2 FIELD THE ONLY PROTECTED FIELD THAT WE WON'T SKIP RIGHT AWAY          
         LA    RF,DETC2NH                                                       
         CR    R2,RF                                                            
         BNE   FTAM55                                                           
         TM    TWATAMFL,TWACS2     WE HAVE A COST2?                             
         BZ    FTAM50               - NOPE, SKIP THIS FIELD                     
         B     FTAM90              NO NEED TO CHECK IF OTHER FIELDS             
*                                                                               
FTAM55   TM    1(R2),X'20'         PROTECTED FIELD?                             
         BNZ   FTAM50              FILLING IN ONLY UNPROTECTED FIELDS           
*                                                                               
         LA    RF,DETREPNH                                                      
         CR    R2,RF                                                            
         BNE   FTAM60                                                           
         TM    TWATAMFL,TWAREP     WE HAVE A REP?                               
         BZ    FTAM50               - NOPE, SKIP THIS FIELD                     
         B     FTAM90              NO NEED TO CHECK IF OTHER FIELDS             
*                                                                               
FTAM60   LA    RF,DETIDH           ID OR CODE FIELDS?                           
         CR    R2,RF                                                            
         BE    FTAM50                                                           
         LA    RF,DETCODH                                                       
         CR    R2,RF                                                            
         BE    FTAM50              YES, SKIP THEM FOR NOW                       
FTAM90   B     FTAM20                                                           
*                                                                               
FTAMX    DS    0H                                                               
********   ADDING A WKMASK FROM TAM                                             
         OI    DETMSKH+1,X'20'     PROTECT THE FIELD AGAIN                      
********   ADDING A WKMASK FROM TAM      MHC  09/05/06                          
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL FAWSSVR TO GET INPUT BUFFER             
         L     RF,CWSSVR-COMFACSD(RF)                                           
*                                                                               
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSARST                                                
         LHI   R0,16384            RESTORE 16K BACK TO THE TIA                  
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
FTAMXX   TM    TWATAMFL,TWAWSSDN   DONE WITH WSSVR BUFFER?                      
         BZ    *+8                                                              
         L     RD,ACWORKA                                                       
         XIT1                                                                   
INPTFLD2 DS    0CL3             ***COPY OF SAME TABLE IN SPECLERR***            
         DC    AL2(DETSTAH-TWAD),AL1(01)                                        
         DC    AL2(DETDAYH-TWAD),AL1(02)                                        
         DC    AL2(DETTIMH-TWAD),AL1(03)                                        
         DC    AL2(DETDPLH-TWAD),AL1(04)                                        
         DC    AL2(DETSDPH-TWAD),AL1(05)                                        
         DC    AL2(DETCS1H-TWAD),AL1(06)                                        
         DC    AL2(DETEF2H-TWAD),AL1(07)                                        
         DC    AL2(DETCS2H-TWAD),AL1(08)                                        
         DC    AL2(DETEF3H-TWAD),AL1(09)                                        
         DC    AL2(DETCS3H-TWAD),AL1(10)                                        
         DC    AL2(DETC2NH-TWAD),AL1(29)                                        
         DC    AL2(DETREPNH-TWAD),AL1(30)                                       
         DC    AL2(DETPRGH-TWAD),AL1(11)                                        
         DC    AL2(DETDATH-TWAD),AL1(12)                                        
         DC    AL2(DETUPGH-TWAD),AL1(13)                                        
         DC    AL2(DETDM1H-TWAD),AL1(14)                                        
         DC    AL2(0)                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE AN ESTIMATE CODE                                *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF ESTIMATE FIELD)                         *         
*                                                                     *         
* EXIT - CC=EQUAL IF ESTIMATE IS VALID WITH VALUES EXTRACTED          *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
VALIEST  NTR1  BASE=*,LABEL=*                                                   
         MVI   FVMINL,1            REQUIRED FIELD                               
         MVI   FVMAXL,L'QEST                                                    
         GOTO1 AFVAL                                                            
         BNE   VALESTX                                                          
         TM    FVIIND,FVINUM       TEST ESTIMATE IS NUMERIC                     
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     VALESTX                                                          
         OC    SCFULL(4),SCFULL    TEST ESTIMATE NUMBER ZERO                    
         BZ    VALEST9                                                          
         OC    SCFULL(3),SCFULL                                                 
         BNZ   VALEST9                                                          
         UNPK  RWORK(3),SCDUB                                                   
         OI    RWORK+2,X'F0'                                                    
         MVC   RWORK+3(1),SCFULL+3                                              
         CLC   QEST,RWORK          TEST FOR CHANGE OF ESTIMATE                  
         BE    VALESTX                                                          
         LA    R2,IOKEY            BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,RWORK+3                                                  
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   VALEST9                                                          
         MVI   ESTIND,0                                                         
         LA    R2,RIO                                                           
         MVC   ESTNM,EDESC                                                      
         MVC   ESTST,ESTART                                                     
         MVC   ESTND,EEND                                                       
         MVC   ESTPW,EPWPCT                                                     
         OC    ECOST2,ECOST2                                                    
         BZ    VALEST1                                                          
         OC    EPWPCT,EPWPCT                                                    
         BNZ   VALEST9             INVALID EST, HAS PW AND C2                   
         MVC   ESTPW,ECOST2+1                                                   
         OI    ESTIND,ESTICS2                                                   
*                                                                               
VALEST1  BRAS  RE,GETESTIN                                                      
*                                                                               
         CLI   CMPPGRPN,0          TEST PRODUCT GROUP                           
         BE    VALEST1A                                                         
         GOTO1 =A(GETPGDEM),RR=ACRELO  YES-GET ESTIMATE DEMOS FOR ANY           
         BNE   VALESTX                 PRODUCT IN THE GROUP                     
         B     VALEST4                                                          
VALEST1A LA    R0,14                                                            
         LA    RE,ESTDEMS                                                       
         XC    ESTDEMS,ESTDEMS                                                  
         LA    RF,EDEMLST                                                       
*                                                                               
VALEST2  OC    0(3,RF),0(RF)                                                    
         BZ    VALEST4                                                          
         MVC   0(3,RE),0(RF)                                                    
         LA    RE,3(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,VALEST2                                                       
*                                                                               
VALEST4  MVC   ESTUSRNM,EUSRNMS                                                 
         MVC   ESTDMENU,EDAYMENU                                                
         MVC   ESTBOOK,EBOOK                                                    
         MVC   ESTREQLO,EREQLO                                                  
         MVC   ESTREQHI,EREQHI                                                  
*                                                                               
         CLI   ESTRATE,C'*'        PORDUCT OVERRIDE TO NORMAL RATES?            
         BE    VALEST5                                                          
         CLI   ERATE,C'*'          OVERRIDE TO NORMAL RATES?                    
         BE    *+12                YES                                          
         CLI   ERATE,C'0'          RATE SET FOR ESTIMATE?                       
         BNH   *+10                NO, DON'T OVERWRITE RATE FROM PROD           
         MVC   ESTRATE,ERATE       YES, OVERWRITE RATE FROM PROD                
*                                                                               
VALEST5  MVC   ESTREP,EREP         SPECIAL REP CODE                             
         MVC   ESTOWSDY,EOWSDAY    OUT-OF-WEEK START DAY                        
         CLI   ESTOWSDY,1          TEST MONDAY                                  
         BNE   *+8                                                              
         MVI   ESTOWSDY,0          YES-IGNORE                                   
         MVC   ESTCNTRL,ECNTRL                                                  
         CLI   EDAILY,C'Y'         DAILY ESTIMATE                               
         BNE   *+12                                                             
         OI    ESTIND,ESTIDLY                                                   
         OI    CMPOPTS,CAMODLY                                                  
*****                                                                           
         TM    EFLAG1,EF1SDE       SUPERDESK AUTHORIZATION ON?                  
         BZ    VALEST6                                                          
         LR    RE,R5                                                            
         AHI   RE,SVINDS-TWAD                                                   
         OI    0(RE),SVIEAUTH     YES, INDICATE AS SUCH                         
*****                                                                           
VALEST6  BAS   RE,GETDEMNM         PRIMARY DEMO NAME                            
*                                                                               
         XC    BEST(G1WPROF-BEST),BEST                                          
         MVC   BEST,RWORK+3                                                     
         XC    QEST(QVALSX-QEST),QEST                                           
         MVC   QEST,RWORK                                                       
         B     VALESTX                                                          
*                                                                               
VALEST9  MVC   FVMSGNO,=AL2(FVIEST)                                             
*                                                                               
VALESTX  XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
***********************************************************************         
GOMSPK   DS    0H                                                               
         NMOD1 0,**GMSP**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R4,4(R1)                                                         
         L     R5,8(R1)            CALLER'S R1                                  
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,QAGY                                                     
*                                                                               
*****    MVC   STAPCTRY,APROF7                                                  
         MVI   STAPCTRY,C'U'                                                    
         TM    APROFBTS,A00CANAD   CANADIAN?                                    
         BZ    *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOM                                                    
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BNE   GOMSPKNO                                                         
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
*                                                                               
GOMSPKYS SR    RC,RC                                                            
GOMSPKNO LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
GOMSUP   DS    0H                                                               
         NMOD1 0,**GMSU**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R4,4(R1)                                                         
         L     R5,8(R1)            CALLER'S R1                                  
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,QAGY                                                     
*                                                                               
*****    MVC   STAPCTRY,APROF7                                                  
         MVI   STAPCTRY,C'U'                                                    
         TM    APROFBTS,A00CANAD   CANADIAN?                                    
         BZ    *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
*                                                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOM                                                    
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL OFFICE FOR SECURITY CHECKS (INCLUDE CLIENT STRING SECURITY NOW)          
*                                                                               
* FUCKING APBYTE IS USED IN NWS11 AS A FIRST TIME FLAG!!!!!                     
***********************************************************************         
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
         XR    R0,R0                                                            
         OC    TWASAGN,TWASAGN     ON NEW SECURITY?                             
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS    OR HAVE LIMIT ACCESS?                      
         BZ    COFCRX                                                           
*                                                                               
         LA    R1,RWORK                                                         
         XC    RWORK,RWORK                                                      
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,CUACCS                                                   
         MVC   OFCAGY,CUAALF                                                    
         MVC   OFCOFC,CLTOFF                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),CUACCS                                                 
         LA    RE,SAVAREA                                                       
         USING SAVAREA,RE                                                       
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLT RECORD               
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKT RECORD               
         DROP  RE                                                               
         OC    TWASAGN,TWASAGN     ON NEW SECURITY?                             
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS    OR HAVE LIMIT ACCESS?                      
         BZ    COFCR10                                                          
         LR    RF,R5               14K INTO TWA                                 
         AHI   RF,14*1024                                                       
         ST    RF,OFCSECD          SECRET AREA SET FROM ABOVE NWS45             
         DROP  R1                                                               
COFCR10  GOTO1 VOFFICER,RPARM,(C'N',RWORK),ACOM                                 
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         LA    R0,1                                                             
*                                                                               
COFCRX   LTR   R0,R0               INSTEAD OF USING APBYTE, SEE ABOVE           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE CLIENT CODE                                     *         
*                                                                     *         
* NTRY - R1=A(FIELD HEADER OF CLIENT FIELD)                           *         
*                                                                     *         
* EXIT - CC=EQUAL IF CLIENT IS VALID WITH CLIENT VALUES EXTRACTED     *         
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER       *         
***********************************************************************         
VALICLT  NTR1  BASE=*,LABEL=*                                                   
         MVI   FVMINL,2            REQUIRED FIELD                               
         MVI   FVMAXL,L'QCLT                                                    
         GOTO1 AFVAL                                                            
         BNE   VALCLTX                                                          
         CLC   QCLT,FVIFLD         TEST CHANGE OF CLIENT                        
         BE    VALCLTX                                                          
         GOTO1 VCLPACK,RPARM,FVIFLD,RWORK                                       
         CLI   0(R1),0                                                          
         BNE   VALCLT8                                                          
         LA    R2,IOKEY            BUILD KEY OF CLIENT RECORD                   
         USING CLTHDRD,R2                                                       
         XC    CKEY,CKEY                                                        
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,RWORK                                                    
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   VALCLT8                                                          
         LA    R2,RIO                                                           
         MVC   CLTNM,CNAME                                                      
         MVI   CLTSRC,C'N'                                                      
         MVC   CLTOFF,COFFICE                                                   
         MVI   CLTIND,0                                                         
         NI    CLTIND2,CLTISDLY                                                 
         MVI   CLTIND3,0                                                        
         MVI   CLTMGRID,0                                                       
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
         MVC   SVCOST2,CCOST2                                                   
         MVC   SVCLOCK,CLOCKYM                                                  
         MVC   SVRFPGRP,CRFPGRP                                                 
         DROP  R1                                                               
*                                                                               
         CLI   CPOLONLY,C'Y'       POL ONLY?                                    
         BNE   *+8                                                              
         OI    CLTIND2,CLTPONLY    YES                                          
*                                                                               
         CLI   CPROF+3,C'0'                                                     
         BE    *+8                                                              
         MVI   CLTSRC,C'A'                                                      
         MVC   CLTSRCDF,CLTSRC     CLIENT'S DEFAULT SERVICE                     
         CLI   CMPRSVC,0           TEST OVERRIDE RATING SERVICE                 
         BE    *+10                                                             
         MVC   CLTSRC,CMPRSVC                                                   
         CLI   CEXTRA+2,C'*'       TEST CCUSA INTERFACE                         
         BNE   *+8                 NO                                           
         OI    CLTIND,CLTICC       YES                                          
         CLI   CEXTRA+5,C'N'       TEST NO US SPILL                             
         BNE   *+8                 NO                                           
         OI    CLTIND,CLTINOSP     YES                                          
         CLI   CEXTRA+2,C'N'       TEST ID REQUIRED FOR BUY TRANSFER            
         BE    VALCLT1             NO                                           
         OI    CLTIND,CLTIBID      YES                                          
         CLI   CEXTRA+2,C'Y'       TEST ID=MKTGRP                               
         BE    VALCLT1                                                          
         CLI   CEXTRA+2,C'A'                                                    
         BL    VALCLT1                                                          
******** CLI   CEXTRA+2,C'K'                                                    
         CLI   CEXTRA+2,C'Z'                                                    
         BH    VALCLT1                                                          
         OI    CLTIND,CLTIBIMG     YES-                                         
         MVC   CLTMGRID,CEXTRA+2   SAVE MARKET GROUP SCHEME                     
*                                                                               
VALCLT1  CLI   CEXTRA+8,C'Y'       TEST GOALS REQUIRED BEFORE BUY               
         BNE   *+8                                                              
         OI    CLTIND,CLTIGOL                                                   
*                                                                               
         TM    COPT2,COP2BP        USE BUY PROGRAMMING PROFILE?                 
         BZ    *+8                                                              
         OI    CLTIND3,CLTIBPPF    YES                                          
*******                                                                         
         TM    COPT2,COP2DIY       USE DIY TRADE?                               
         BZ    *+8                                                              
         OI    CLTIND3,CLTIDIYT    YES                                          
*******                                                                         
         TM    COPT2,COP2FRZ       FROZEN CLIENT?                               
         BZ    *+8                                                              
         OI    CLTIND2,CLTFROZN    YES                                          
*                                                                               
         MVC   CLTIFC,CCLTIFC      CLIENT INTERFACE CODE                        
         MVC   CLTPROF,CPROF                                                    
         MVC   CLTMTCLT,CMCLTCOD   MASTER TRAFFIC CLIENT CODES                  
         MVC   CLTMTNUM,CMCLTUNQ                                                
         MVC   CLTMTPRD,CMCLTPRD                                                
*                                                                               
*****    CLI   APROF7,C'C'         TEST CANADA                                  
*****    BNE   VALCLT1A                                                         
         TM    APROFBTS,A00CANAD   CANADIAN?                                    
         BZ    VALCLT1A                                                         
*                                                                               
         BNE   VALCLT1A                                                         
         MVI   CUDMED,C'C'         YES-MEDIA C FOR DEMO LOOKUPS                 
         CLI   CEXTRA,C'U'         TEST LOOK UP US DEMOS                        
         BNE   VALCLT1A                                                         
         MVI   CUDMED,C'T'         YES-SET MEDIA FOR US DEMO LOOKUPS            
         OI    CLTIND2,CLTIUS                                                   
*                                                                               
VALCLT1A XC    BCLT(G1WPROF-BCLT),BCLT                                          
         MVC   BCLT,RWORK                                                       
         XC    QCLT(QVALSX-QCLT),QCLT                                           
         GOTO1 VCLUNPK,RPARM,(CPROF+6,BCLT),QCLT                                
*                                                                               
         TM    CUSTAT,CUSDDS       IGNORE SECURITY FOR DDS TERMINAL             
         BO    VALCLT4                                                          
         LA    R1,SAVAREA                                                       
         USING SAVAREA,R1                                                       
         MVC   SVCACCS,CACCESS                                                  
         MVI   SVMACCS,X'FF'                                                    
         DROP  R1                                                               
         BRAS  RE,CALLOFCR                                                      
         BE    VALCLT4                                                          
         XC    BCAM(G1WPROF-BCAM),BCAM                                          
         XC    QCAM(QVALSX-QCAM),QCAM                                           
         B     VALCLT9                                                          
*                                                                               
VALCLT4  MVC   RDUB(2),CUAALF      GET THE EQUIVALENCE RECORD                   
         MVC   RDUB+2(1),QMED                                                   
         MVC   RDUB+3(2),BCLT                                                   
         LA    R2,RIO                                                           
         LA    R3,L'EQUDPT(R2)                                                  
         L     R4,VDMGR                                                         
         GOTO1 VEQVRD,RPARM,RDUB,(R2),(R3),(R4)                                 
         CLI   RPARM+12,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLTEQUIV,0(R3)                                                   
*                                                                               
         XC    RFULL,RFULL                                                      
         MVC   RFULL(2),=C'BW'     BW PROFILE                                   
         GOTO1 PROFILE,RPARM,RFULL,CLTBWPRO                                     
         MVC   RFULL(2),=C'AJ'     AJ PROFILE                                   
         GOTO1 (RF),(R1),RFULL,CLTAJPRO                                         
         MVC   RFULL(2),=C'AD'     AD PROFILE                                   
         GOTO1 (RF),(R1),RFULL,RWORK2                                           
         CLI   RWORK2,C'Y'         TEST ADDS USER                               
         BNE   *+8                                                              
         OI    CLTIND,CLTIADDS     YES                                          
         MVC   RFULL(2),=C'1W'     1W PROFILE                                   
         GOTO1 (RF),(R1),RFULL,RWORK2                                           
         MVC   G1WPROF,RWORK2                                                   
         CLI   RWORK2+3,C'Y'       TEST CANADIAN ANGLO/FRANCO OPTION ON         
         BNE   *+8                                                              
         OI    CLTIND2,CLTIANFR    YES                                          
         MVC   RFULL(3),=C'BWA'    BWA PROFILE                                  
         GOTO1 (RF),(R1),RFULL,RWORK2                                           
         CLI   RWORK2,C'Y'         TEST TRANSFER DATES REQUIRED                 
         BNE   *+8                                                              
         OI    CLTIND,CLTITRDT     YES                                          
         CLI   RWORK2+1,C'Y'       TEST TRANSFER BUYER NAME                     
         BNE   *+8                                                              
         OI    CLTIND,CLTITRBN     YES                                          
         CLI   RWORK2+2,C'Y'       TEST ROUND EQUIVALENCED COSTS                
         BNE   *+8                                                              
         OI    CLTIND2,CLTIREQC    YES                                          
         CLI   RWORK2+3,C'Y'       TEST SEPARATE LINES FOR DAILY SCHED          
         BNE   *+8                                                              
         OI    CLTIND2,CLTISDLY    YES                                          
         CLI   RWORK2+4,C'Y'       TEST CAMPAIGN DATES MUST MATCH EST           
         BNE   *+8                                                              
         OI    CLTIND2,CLTIEST     YES                                          
         CLI   RWORK2+5,C'Y'       TEST X-OUT SCHEDULED WEEKS FOR COPY          
         BNE   *+8                                                              
         OI    CLTIND2,CLTIXOUT    YES                                          
         B     VALCLTX                                                          
*                                                                               
VALCLT8  MVC   FVMSGNO,=AL2(FVICLI)                                             
         B     VALCLTX                                                          
*                                                                               
VALCLT9  MVC   FVMSGNO,=AL2(FVSECLOK)                                           
*                                                                               
VALCLTX  XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPECIAL ERROR EXIT ROUTINE SO WE CAN EXIT OUT OF GENERAL AND WRITE            
* TO THE WSSVR BUFFER                                                           
***********************************************************************         
SPECLERR NMOD1 0,**SPCL**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         SR    R2,R2                                                            
         ICM   R2,12,6(R1)         ERROR CODE                                   
         L     RE,FVADDR           GET D(FIELD IN ERROR)                        
         SR    RE,R5                                                            
*                                                                               
         LA    RF,INPTFLDS         FIND WHICH INPUT FIELD IN ERROR              
SPCLERR1 OC    0(2,RF),0(RF)                                                    
         BZ    SPCLERR8            A DEMO FIELD IN ERROR                        
         CLM   RE,3,0(RF)                                                       
         BE    SPCLERR9                                                         
         LA    RF,L'INPTFLDS(RF)   CHECK AGAINST ALL INPUT FIELDS               
         B     SPCLERR1                                                         
*                                                                               
SPCLERR8 SHI   RF,L'INPTFLDS                                                    
*                                                                               
SPCLERR9 ICM   R2,2,2(RF)          R2 HAS 2 BYTE ERROR AND FIELD #              
*                                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL FAWSSVR TO SAVE BUFFER                  
         L     RF,CWSSVR-COMFACSD(RF)                                           
****     L     RF,CGETFACT-COMFACSD(RF)                                         
****     GOTO1 (RF),APPARM,(X'80',0),F#VWSSVR                                   
****     ICM   RF,15,APPARM                                                     
*                                                                               
         XC    ACWORK,ACWORK                                                    
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         LHI   R0,16384            SAVE OFF 16K OF THE TIA                      
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ACWORK,ACWORK                                                    
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSURST                                                
         LHI   R0,16384                                                         
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
         L     R6,ATIA             R6 = A(CURRENT WORK RECORD)                  
         XR    RE,RE                                                            
         ICM   RE,3,TWARDBFR                                                    
         AR    R6,RE                                                            
*                                                                               
         STCM  R2,15,4(R6)         SAVE ERROR AND FIELD NUMBER                  
*                                                                               
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVI   FAWSACTN,FAWSUSVE                                                
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ACWORK,ACWORK                                                    
         LA    R1,ACWORK                                                        
         USING FAWSSVRD,R1                                                      
         MVC   FAWSTOKN,=CL4'$TAM'                                              
         MVI   FAWSACTN,FAWSARST                                                
         LHI   R0,16384            RESTORE 16K OF THE TIA                       
         STH   R0,FAWSLEN                                                       
         MVC   FAWSADR,ATIA                                                     
         GOTO1 (RF),(R1)                                                        
         CLI   FAWSRTN,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
SPCLERRX ICM   RD,15,TWAB4GNL     EXIT ALL THE WAY BACK OUT OF GENERAL          
         L     RD,8(RD)                                                         
         XIT1                                                                   
*                                                                               
INPTFLDS DS    0CL3                ID AND CODE FIELD NOT PASSED                 
         DC    AL2(DETSTAH-TWAD),AL1(01)                                        
         DC    AL2(DETDAYH-TWAD),AL1(02)                                        
         DC    AL2(DETTIMH-TWAD),AL1(03)                                        
         DC    AL2(DETDPLH-TWAD),AL1(04)                                        
         DC    AL2(DETSDPH-TWAD),AL1(05)                                        
         DC    AL2(DETCS1H-TWAD),AL1(06)                                        
         DC    AL2(DETEF2H-TWAD),AL1(07)                                        
         DC    AL2(DETCS2H-TWAD),AL1(08)                                        
         DC    AL2(DETEF3H-TWAD),AL1(09)                                        
         DC    AL2(DETCS3H-TWAD),AL1(10)                                        
         DC    AL2(DETC2NH-TWAD),AL1(29)                                        
         DC    AL2(DETREPNH-TWAD),AL1(30)                                       
         DC    AL2(DETPRGH-TWAD),AL1(11)                                        
         DC    AL2(DETDATH-TWAD),AL1(12)                                        
         DC    AL2(DETUPGH-TWAD),AL1(13)                                        
         DC    AL2(DETDM1H-TWAD),AL1(14)                                        
         DC    AL2(0)                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SETS UP NWS BUY REVISION RECORD IN SPECIFIED I/O AREA                         
*                                                                               
* ON ENTRY:    4(R1)               SETUPNBR'S PARAM LIST - A(APPARM)            
***********************************                                             
* NOTE: USES  R5  SO IT WON'T ADDRESS  TWAD                                     
*       USES  R6  SO IT WON'T ADDRESS  T20700+X'4000'                           
*       USES  R8  SO IT WON'T ADDRESS  T20700+X'3000'                           
***********************************************************************         
         DROP  R5,R6,R8                                                         
STBRVREC NMOD1 0,**SNBR**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R1,4(R1)            RESTORE A(SETUPNBR'S PARAMETER LIST)         
*                                                                               
         L     R2,0(R1)            A(BUY RECORD)                                
         USING BUYKEY,R2                                                        
         L     R3,4(R1)            A(BUY REVISION RECORD)                       
         USING NBRKEY,R3                                                        
         L     R8,8(R1)            A(DBLOCK)                                    
         USING DBLOCKD,R8                                                       
*                                                                               
         LR    RE,R3                                                            
****     LA    RF,4000                                                          
         LHI   RF,6000                                                          
         XCEFL                                                                  
*                                                                               
         TM    APROFBTS,A00CANAD   TEST CANADIAN AGENCY                         
         BNZ   SNBRERR              - YUP, NO NBR FOR CANADA!                   
*                                                                               
         MVI   NBRKTYP,NBRKTYPQ                                                 
         MVI   NBRKSTY,NBRKSTYQ                                                 
         MVC   NBRKAGMD,BAGYMD                                                  
         OC    NBRKAGMD,BBYRMASK                                                
         MVC   NBRKBYR,BBYR                                                     
         MVC   NBRKSEQ,BCMSEQ                                                   
         MVC   NBRKSTA,BUYMSTA+2                                                
         MVC   NBRKKBUY,BUYKBUY-BUYKEY+APRECKEY                                 
         MVI   NBRLEN+1,NBRFSTEL-NBRKEY                                         
         MVC   20(2,R2),QAGY       AGENCY POWER CODE                            
*                                                                               
         LA    R4,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING NBRSELD,R4                                                       
         MVI   NBRSEL,NBRSELQ                                                   
         MVI   NBRSLN,NBRSLNQ                                                   
         MVC   NBRSBYLN,BUYKBUY                                                 
*   NO PACKAGE/ORBIT NUMBER                                                     
         MVC   NBRSDAYS,BDDAY                                                   
         GOTO1 VMSUNPK,RPARM,(X'80',BUYMSTA),ACWORK,NBRSSTA                     
*                                                                               
         CLI   NBRSSTA+4,C' '                                                   
         BNE   *+16                                                             
         CLI   QMED,C'T'                                                        
         BNE   *+8                                                              
         MVI   NBRSSTA+4,C'T'                                                   
*                                                                               
         MVC   NBRSTIMS(L'TBUYTIMS*2),BDTIMST    TIMES                          
         MVC   NBRSDYPT,BDDAYPT    DAYPART                                      
         TM    CLTIND3,CLTIMSDP    MASTER/SUBDAYPART COMBINED?                  
         BZ    SNBR09              NO, BUY'S DPT IS REVISION'S DPT              
         MVC   RHALF,BDPT          YES-GET DAYPART                              
         MVI   BDPT,0              FORCE GETDPT TO SET DPTMAS                   
         GOTO1 LGETDPT,BDDAYPT                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BDPT(2),RHALF                                                    
*                                                                               
         CLI   DPTTYPE,C'S'        TEST THIS IS A SUB DAYPART                   
         BNE   SNBR09              NO                                           
         MVC   NBRSDYPT,DPTMAS     YES-SET DAYPART TO THE MASTER                
         MVC   NBRSSBDP,BDDAYPT        AND SET SUB-DAYPART                      
*                                                                               
SNBR09   MVC   NBRSSLN,BDSEC       SPOT LENGTH                                  
         ZICM  RF,BDCOST,3                                                      
         TM    BDCIND2,X'20'       CANADIAN AGENCY BUY?                         
         BNZ   SNBR10              YES                                          
         TM    BDCIND2,X'10'       COST IN US DOLLARS                           
         BZ    SNBR10                                                           
         MHI   RF,100                                                           
SNBR10   STCM  RF,15,NBRSCST1                                                   
         TM    BDCIND,X'01'        MINUS SPOT (NEG AMT)?                        
         BZ    SNBR12                                                           
         LNR   RF,RF               YES                                          
         STCM  RF,15,NBRSCST1                                                   
SNBR12   MVC   NBRSPROG,BDPROGRM   PROGRAM                                      
         MVC   NBRSADJC,BDPROGT    ADJACENCY CODE                               
         MVC   NBRSREP,BDREP       REP CODE                                     
         GOTO1 AADDELS,NBRRECD                                                  
***********************************                                             
* NOW FOR THE DEMOS                                                             
***********************************                                             
         XC    ACFULL,ACFULL                                                    
         LA    R1,BDELEM                                                        
         SR    R0,R0                                                            
SNBR17   CLI   0(R1),0                                                          
         BE    SNBR20                                                           
         CLI   0(R1),X'02'         ORIGINAL DEMO ELEMENT                        
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     SNBR17                                                           
*                                                                               
         ICM   R1,8,=C'B'          WE'RE DOING BUY DEMO                         
         BRAS  RE,ADJDEMEL         LET'S SEE IF WE NEED TO ADJUST               
***      ICM   R1,8,=X'00'         TAKE OUT THE =C'B'                           
         LA    R1,0(R1)            TAKE OUT THE =C'B'                           
*                                                                               
         XC    ACWORK,ACWORK                                                    
         LA    RE,ESTDEMS+3                                                     
         LA    R0,ESTDEMS+L'ESTDEMS                                             
         LA    RF,ACWORK+3                                                      
         MVC   ACWORK(3),ESTDEMS                                                
*                                                                               
SNBR17A  MVC   0(3,RF),0(RE)       NO                                           
         LA    RF,3(RF)            BUMP TO NEXT ENTRY IN OUR LIST               
         LA    RE,3(RE)            BUMP TO NEXT ENTRY IN EST LIST               
         OC    0(3,RE),0(RE)                                                    
         BZ    SNBR17X                                                          
         CR    RE,R0                                                            
         BL    SNBR17A                                                          
*                                                                               
SNBR17X  DS    0H                                                               
         LA    R4,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING NBRDMELD,R4                                                      
         MVI   NBRDMEL,NBRDMELQ                                                 
         LA    R5,NBRDMDMO                                                      
*                                                                               
         LA    R6,ACWORK           OUR DEMO LIST                                
         LR    R0,R1               SAVE A(DEMO ELEMENT)                         
SNBR18   LR    R1,R0               R1 = A(DEMO ELEMENT)                         
         LA    RE,8                RE = L'DEMO                                  
         ZIC   RF,1(R1)            RF = A(LAST BYTE IN DEMO ELEMENT)            
         AR    RF,R1                                                            
         BCTR  RF,0                                                             
         LA    R1,NDEMNO-NDELEM(R1)  R1 = A(1ST DEMO ENTRY)                     
         MVC   0(3,R5),0(R6)       COPY DEMO CATEGORY                           
                                                                                
*                                                                               
SNBR19   CLC   0(3,R1),0(R6)                                                    
         BNE   *+10                                                             
         MVC   3(5,R5),3(R1)       COPY THE RATING                              
         BXLE  R1,RE,SNBR19                                                     
*                                                                               
         LA    R5,L'NBRDMDMO(R5)                                                
         LA    R6,3(R6)                                                         
         LA    RE,NBRDMDMO+8*L'NBRDMDMO                                         
         CR    R5,RE               PAST OUR 8 DEMOS WE CAN STORE?               
         BNL   *+14                YES                                          
         OC    0(3,R6),0(R6)                                                    
         BNZ   SNBR18                                                           
*                                                                               
         LR    R1,R5                                                            
         SR    R1,R4                                                            
         STC   R1,NBRDMLEN                                                      
***      BCTR  R1,0                SAVE DEMO ELEMENT                            
***      EX    R1,*+8                                                           
***      B     *+10                                                             
***      MVC   LDMOEL(0),APELEM                                                 
         GOTO1 AADDELS,NBRRECD                                                  
***********************************                                             
* NOW FOR THE SPOTS                                                             
***********************************                                             
SNBR20   DS    0H                                                               
         LA    R6,BDELEM                                                        
         USING REGELEM,R6                                                       
         LA    R4,APELEM                                                        
         XC    APELEM,APELEM                                                    
         USING NBRSPELD,R4                                                      
         MVI   NBRSPEL,NBRSPELQ                                                 
******   MVC   NBRSPDAT,CMPDATSP   <=== NO NEED FOR THIS YET                    
*                                                                               
SNBR30   CLI   0(R6),0             END OF RECORD?                               
         BE    SNBR60              THAT'S IT!                                   
*                                                                               
SNBR32   CLI   0(R6),X'06'         NON-POL SPOT ELEMENT?                        
         BL    SNBR50                                                           
         CLI   0(R6),X'07'                                                      
         BNH   SNBR40                                                           
*                                                                               
         CLI   0(R6),X'0B'         POL SPOT ELEMENT?                            
         BL    SNBR50                                                           
         CLI   0(R6),X'0C'                                                      
         BH    SNBR50                                                           
*                                                                               
         CLC   RPPRD,BPRD          MATCHES ON THE CAMPAIGN PRODUCT?             
         BE    SNBR34              YES                                          
         CLI   BPRD,X'FF'          POL IN THE CAMPAIGN DEFINITION?              
         BE    SNBR34              YES, WHEN BUILDING, TAKE ALL OF THEM         
*                                                                               
SNBR33   CLI   CMPPRD2,0           NO, MAKE SURE NOT PR2-PR1                    
         BE    SNBR50                  CAN'T BE                                 
         CLC   RPPRD,CMPPRD2                                                    
         BNE   SNBR50                  CAN'T BE                                 
         CLI   RLEN,RPSTAT2+L'RPSTAT2-REGELEM   PIGGYBACK IN ELEMENT?           
         BNH   SNBR50                           NO                              
         CLC   RPPRD+L'RPALLOC,BPRD                                             
         BNE   SNBR50                                                           
         B     SNBR40                                                           
*                                                                               
SNBR34   CLI   CMPPRD2,0           ANY CAMPAIGN PIGGYBACK PRODUCT?              
         BNE   SNBR34A             YES, MAKE SURE THERE IS A PB IN ELEM         
         CLI   RLEN,RPSTAT2+L'RPSTAT2-REGELEM   PIGGYBACK IN ELEMENT?           
         BH    SNBR50                           YES, SHOULDN'T BE               
         B     SNBR40                                                           
*                                                                               
SNBR34A  CLI   RLEN,RPSTAT2+L'RPSTAT2-REGELEM   PIGGYBACK IN ELEMENT?           
         BNH   SNBR50                           NO                              
         CLC   RPPRD+L'RPALLOC,CMPPRD2          MATCH CAMP PIGGYBACK?           
         BNE   SNBR50                           NOPE                            
*                                                                               
SNBR40   L     R1,ATWA             LIST OF FLIGHT DATES                         
         AHI   R1,CMPDATSP-TWAD                                                 
SNBR42   OC    0(2,R1),0(R1)                                                    
         BZ    SNBR50              NO MATCH IN FLIGHT DATES, NEXT SPOT          
*                                                                               
         CLC   RDATE,0(R1)         SPOT DATE IS IN THIS FLIGHT WEEK?            
         BL    *+14                                                             
         CLC   RDATE,2(R1)                                                      
         BNH   SNBR44                                                           
         LA    R1,4(R1)            CHECK NEXT FLIGHT WEEK                       
         B     SNBR42                                                           
*                                                                               
SNBR44   LR    RE,R1               RE = NTH DISPLACED WEEK IN SCHEDULE          
         L     R0,ATWA                                                          
         AHI   R0,CMPDATSP-TWAD                                                 
         SR    RE,R0                                                            
         SRL   RE,2                DIVIDE BY 4                                  
         LA    RF,NBRSPSPW(RE)                                                  
*                                                                               
         LA    R0,1                                                             
         CLI   RCODE,X'0B'         POOL SPOT?                                   
         BNL   *+8                                                              
         IC    R0,RNUM                                                          
         TM    RSTATUS,X'80'       MINUS SPOT?                                  
         BZ    *+6                                                              
         LNR   R0,R0               YES, NEGATE TO SUBTRACT                      
*                                                                               
SNBR48   ZIC   RE,0(RF)            BUMP UP THE SPOT COUNT                       
         AR    RE,R0                                                            
         CHI   RE,256                                                           
         BL    *+6                                                              
         DC    H'0'                DIE, MORE THAN 255 SPOTS IN WEEK             
         CHI   RE,0                                                             
         BNL   *+6                                                              
         XR    RE,RE               CAN'T HANDLE NEGATIVE SPOTS YET              
         STC   RE,0(RF)                                                         
*                                                                               
SNBR49   CLI   RCODE,X'0B'                                                      
         BL    SNBR50                                                           
         TM    RSTATUS,X'20'       COST OVERRIDE?                               
         BZ    SNBR50                                                           
         BAS   RE,ADDOVRDE         ADD COST OVERRIDE TO NBR RECORD              
*                                                                               
SNBR50   XR    R0,R0               NEXT SPOT ELEMENT                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SNBR30                                                           
*                                                                               
SNBR60   XR    RE,RE                                                            
         IC    RE,CMPNWKS                                                       
         LA    RE,NBRSPSPW-NBRSPELD(RE)                                         
         STC   RE,NBRSPLEN                                                      
***      BCTR  RE,0                SAVE SPOT/WEEK ELEMENT                       
***      EX    RE,*+8                                                           
***      B     *+10                                                             
***      MVC   LSPWEL(0),APELEM                                                 
         GOTO1 AADDELS,NBRRECD                                                  
         DROP  R6                                                               
***********************************                                             
* NOW FOR THE COMMENTS                                                          
***********************************                                             
         LA    R6,BDELEM                                                        
SNBR70   CLI   0(R6),0             END OF RECORD?                               
         BE    SNBR80              THAT'S IT!                                   
*                                                                               
SNBR72   CLI   0(R6),X'66'         COMMENT ELEMENT?                             
         BNE   SNBR79                                                           
*                                                                               
         XC    APELEM,APELEM       COPY THE BUY COMMENTS                        
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   APELEM(0),0(R6)                                                  
         MVI   APELEM,NBRCMELQ                                                  
*                                                                               
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
SNBR79   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SNBR70                                                           
***********************************                                             
* NOW FOR THE UPGRADE                                                           
***********************************                                             
SNBR80   LA    R6,BDELEM                                                        
         XC    APWORK(4),APWORK    FOR SPECIAL BKTP(1) FLAG(1) BOOK(2)          
*                                                                               
SNBR80A  CLI   0(R6),0             END OF RECORD?                               
         BE    SNBR90              THAT'S IT!                                   
*                                                                               
         CLI   0(R6),X'02'         ORIGINAL DEMOGRAPHIC ELEMENT                 
         BNE   SNBR81                                                           
         USING NDELEM,R6                                                        
         MVC   APWORK+2(2),NDBOOK   BOOK IS ALWAYS HERE                         
         LA    R1,NBRFSTEL                                                      
         USING NBRSELD,R1                                                       
         MVC   NBRSBOOK,NDBOOK     NOW LOOK FOR POSSIBLE SPECIAL BOOK           
         DROP  R1                      (X'24')                                  
         B     SNBR89                                                           
*****                                                                           
*                                                                               
SNBR81   CLI   0(R6),X'24'         DEMO LOOK-UP OVERRIDE (BOOKTYPE)             
         BNE   SNBR82                                                           
         USING DLUELEM,R6                                                       
         MVC   APWORK(1),DLUBKTYP                                               
         CLI   APWORK,0            ANY SPECIAL BOOKTYPE?                        
         BE    SNBR89               - NOPE, GET TO NEXT ELEMENT                 
         MVC   APWORK+4(2),APWORK+2                                             
**                                                                              
         BRAS  RE,BKTPBIT          RESTORE THE SPECIAL BOOKTYPE BIT             
**                                                                              
         LA    R1,NBRFSTEL                                                      
         USING NBRSELD,R1                                                       
         MVC   NBRSBOOK,APWORK+4   NOW LOOK FOR POSSIBLE SPECIAL BOOK           
         DROP  R1                      (X'24')                                  
         MVC   QBOOKTYP,APWORK     SAVE IN QBOOKTYP                             
         B     SNBR89                                                           
*                                                                               
SNBR82   CLI   0(R6),X'62'         UPGRADE ELEMENT?                             
         BNE   SNBR89                                                           
*                                                                               
         OI    APWORK+1,X'80'      WE HAVE AN UPGRADE ELEMENT                   
         USING UPELEM,R6                                                        
                                                                                
*****             CHECKING FOR SPECIAL TYPE BOOKS                               
*****  UPFBK NEEDS THE SPECIAL BOOKTYPE BITS BACK FOR FMTUPG LATER!!            
**                                                                              
         CLI   APWORK,0            WE HAVE SPECIAL BOOKTYPE?                    
         BE    SNBR83               - NOPE WE DON'T                             
         MVC   APWORK+4(2),UPFBK                                                
**                                                                              
         BRAS  RE,BKTPBIT          RESTORE THE SPECIAL BOOKTYPE BIT             
**                                                                              
         MVC   UPFBK,APWORK+4                                                   
*****                                                                           
*****  UPFBK IS NOW READY TO BE SAVED IN NBRUPOBK LATER ON                      
*                                                                               
SNBR83   LA    R1,NBRFSTEL                                                      
         USING NBRSELD,R1                                                       
*                                                                               
         MVI   NBRSUPUT,0                                                       
         CLI   UP2YRP,C'N'                                                      
         BNE   *+8                                                              
         MVI   NBRSUPUT,1                                                       
         CLI   UP2YRP,C'Y'                                                      
         BNE   *+8                                                              
         MVI   NBRSUPUT,2                                                       
*                                                                               
         MVI   NBRSUSHR,0                                                       
         CLI   UP2YRS,C'N'                                                      
         BNE   *+8                                                              
         MVI   NBRSUSHR,1                                                       
         CLI   UP2YRS,C'Y'                                                      
         BNE   *+8                                                              
         MVI   NBRSUSHR,2                                                       
         DROP  R1                                                               
*                                                                               
         OC    UPSTA(L'UPSTA+L'UPDAYTIM),UPSTA   ANY OVERRIDE D/T, STA?         
         BZ    SNBR84                            NONE                           
         LA    R4,APELEM                                                        
         USING NBRODELD,R4                                                      
         XC    APELEM,APELEM       MAKE AN OVERRIDE ELEMENT                     
         MVI   NBRODEL,NBRODELQ                                                 
         MVI   NBRODLEN,NBRODLNQ                                                
         MVC   NBRODODY(L'UPDAYTIM),UPDAYTIM                                    
****     MVC   NBRODSTA,UPSTA                                                   
         GOTO1 AADDELS,NBRRECD                                                  
*                                                                               
**BR84   TM    APWORK+1,X'80'      DID WE HAVE AN UPGRADE FROM BUY?             
**       BZ    SNBR90               - NOPE, DON'T MAKE A NBRUPEL                
SNBR84   LA    R4,APELEM                                                        
         USING NBRUPELD,R4                                                      
         XC    APELEM,APELEM       MAKE AN UPGRADE ELEMENT                      
         MVI   NBRUPEL,NBRUPELQ                                                 
         MVI   NBRUPLEN,NBRUPLNQ                                                
         MVC   NBRUPFIL,UPFILE                                                  
         MVC   NBRUPEXP,UPTYPE                                                  
         MVC   NBRUPOBK,UPFBK                                                   
**********************************                                              
***  SHOULD BE SAFE TO USE APPARM HERE NOW                                      
**********************************                                              
         GOTO1 AFMTUPG,APPARM,UPELEM,APELEM,DBLOCK                              
***  X'40' NEEDS THE NEW LENGTH IF IT IS SPECIAL BOOKTYPE                       
         TM    NBRUPFIL+10,X'E0'   IS IT SPECIAL BOOKTYPE?                      
         BNO   SNBR85               - NOPE                                      
         MVI   NBRUPLEN,NBRUPLQ2   WE NEED THE LONGER LENGTH                    
         MVC   NBRUPOBT,QBOOKTYP   MOVE IN THE BINARY BOOKTYPE                  
***  X'40' NEEDS THE NEW LENGTH IF IT IS SPECIAL BOOKTYPE                       
SNBR85   MVC   NBRUPBKL,UPFBKLST           BOOKLIST                             
*                                                                               
         GOTO1 AADDELS,NBRRECD                                                  
         B     SNBR90                                                           
*                                                                               
SNBR89   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SNBR80A                                                          
         DROP  R6                                                               
***********************************                                             
* NOW FOR THE COST 2                                                            
***********************************                                             
SNBR90   LA    R6,BDELEM                                                        
*                                                                               
SNBR91   CLI   0(R6),0             END OF RECORD?                               
         BE    SNBRX                - YUP, LET'S GET OUTTA HERE                 
*                                                                               
         CLI   0(R6),X'71'         COST 2 ELEMENT?                              
         BNE   SNBR99                                                           
         USING COS2ELEM,R6                                                      
         LA    R4,APELEM                                                        
         USING NBRC2ELD,R4                                                      
         XC    APELEM,APELEM       MAKE AN OVERRIDE ELEMENT                     
         MVI   NBRC2EL,NBRC2ELQ                                                 
         MVI   NBRC2LEN,NBRC2LNQ                                                
         MVC   NBRC2CST,2(R6)                                                   
         GOTO1 AADDELS,NBRRECD                                                  
         B     SNBRX                                                            
*                                                                               
SNBR99   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     SNBR91                                                           
         DROP  R6                                                               
*                                                                               
SNBRERR  MVC   FVMSGNO,=AL2(1269)   NO NBR FOR CANADIANS                        
         CLC   FVMSGNO,=AL2(FVFOK)   SETS CONDITION JUST IN CASE                
*                                                                               
SNBRX    XIT1                                                                   
         DROP  R2,R3,R4,R8                                                      
         EJECT                                                                  
***********************************************************************         
* ADD COST OVERRIDE ELEMENT TO BUY REVISION RECORD                              
*                                                                               
* ON ENTRY: (R6)                   A(POL SPOT ELEMENT)                          
*           (R3)                   A(BUY REVISION RECORD)                       
*           (R2)                   A(BUY RECORD)                                
*                                                                               
* WARNING!  ACWORK GETS CLOBBERED                                               
***********************************************************************         
ADDOVRDE NTR1                                                                   
*  SAVE SPOTS PER WEEK ELEMENT IN ACWORK SO WE CAN RESTORE LATER                
         MVC   ACWORK(NMAXWKS+NBRSPSPW-NBRSPELD),APELEM                         
         USING REGELEM,R6                                                       
         LA    R1,APELEM                                                        
         USING NBRCOELD,R1                                                      
         XC    APELEM,APELEM                                                    
         MVI   NBRCOEL,NBRCOELQ                                                 
*        MVI   NBRCOLEN,NBRCOLNQ   THIS IS THE NEW LENGTH (9, NOT 8)            
         MVI   NBRCOLEN,NBRCOL2Q   WHICH HAS NEW FLAG FOR MINUS STUFF           
         MVC   NBRCODAT,RDATE                                                   
         MVC   NBRCOFLG,RSTATUS                                                 
         NI    NBRCOFLG,X'C0'      ONLY LEAVE MINUS STUFF                       
*                                  X'80' + X'40' BOTH MINUS STUFF               
         XR    R0,R0                                                            
         ICM   R0,7,RPCOST                                                      
         LR    RF,R2                                                            
         AHI   RF,BDCIND2-BUYKEY                                                
         TM    0(RF),X'20'         CANADIAN AGENCY?                             
         BNZ   AOVR10                                                           
         TM    0(RF),X'10'         COST IN US DOLLARS?                          
         BZ    AOVR10                                                           
         MHI   R0,100                                                           
AOVR10   STCM  R0,15,NBRCOCST                                                   
         GOTO1 AADDELS,(R3)                                                     
*  RESTORE SPOTS PER WEEK ELEMENT FROM ACWORK SO WE CAN CONTINUE                
         MVC   APELEM(NMAXWKS+NBRSPSPW-NBRSPELD),ACWORK                         
AOVRX    B     SNBRX                                                            
         DROP  R1,R6                                                            
***********************************************************************         
*  THIS SUBROUTINE ADDS THE SPECIAL BOOKTYPE BITS BACK                          
*  ON EXIT:    QBOOKTYP IS SET                                                  
*              INPUT BOOK GETS SPECIAL BOOKTYPE BIT IF THERE WAS X'24'          
*                                      ELEMENT FROM BUY                         
***********************************************************************         
BKTPBIT  NTR1  BASE=*,LABEL=*                                                   
         CLI   APWORK,C'4'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYZR4Q    X'C0' ZERO 4                                 
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'1'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYZR1Q    X'B0' ZERO 1                                 
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'I'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYHPEOQ   X'A0' HISPANIC PEOPLE METER                  
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'O'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYOLYMQ   X'80' OLYMPIC                                
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'A'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYPRNTQ   X'90' PARENT ONLY DATA                       
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'H'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYHISPQ   X'70' HISPANIC                               
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'B'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYBLAKQ   X'60' BLACK                                  
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'P'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYPEOPQ   X'50' PEOPLE METER                           
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'T'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYTRADQ   X'40' TRADE                                  
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'M'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYMTROQ   X'30' METRO                                  
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'D'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYDMAQ    X'20' DMA                                    
         B     BKTPBITX                                                         
*                                                                               
         CLI   APWORK,C'E'                                                      
         BNE   *+12                                                             
         OI    APWORK+5,BTYOTHRQ   X'10' OTHER                                  
         B     BKTPBITX                                                         
*                                                                               
         OI    APWORK+5,BTY2CHAR   X'E0' USING 2 CHARACTER BOOKTYPES+L          
*                                                                               
BKTPBITX J     EXIT                                                             
         LTORG                                                                  
***********************************                                             
* RESTORE THE REGISTERS DSECTS THAT WE DROPPED EARLIER                          
***********************************                                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING T20700+X'4000',R6                                                
         USING T20700+X'3000',R8                                                
         EJECT                                                                  
***********************************************************************         
* GET THE NWS COMPETITION RECORD IN AIOAREA3                                    
*                                                                               
* ON ENTRY:    4(R1)               GETCMPRC'S PARAM LIST - A(APPARM)            
***********************************                                             
*              1ST PARAM           A(DAYS BINARY)                               
*              2ND PARAM           A(TIMES BINARY)                              
*              3RD PARAM           A(STATION TEXT)                              
*              4TH PARAM           A(LIST OF DEMO VALUES)                       
***********************************                                             
* NOTE: HAVE TO ASSUME APPARM WILL NOT GET MESSED UP                            
*                                                                               
*       USES  R5  SO IT WON'T ADDRESS  TWAD                                     
*       USES  R6  SO IT WON'T ADDRESS  T20700+X'4000'                           
*       USES  R8  SO IT WON'T ADDRESS  T20700+X'3000'                           
***********************************************************************         
         DROP  R5,R6,R8                                                         
GTCOMPRC NMOD1 0,**GCMR**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R5,4(R1)            RESTORE A(GETCMPRC'S PARAMETER LIST)         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CMPKEY,R2                                                        
         MVI   CMPKTYP,CMPKTYPQ    (X'0D6C')                                    
         MVI   CMPKFIL,CMPKFILQ                                                 
         MVC   CMPKAM,BAGYMD                                                    
         OC    CMPKAM,BBYRMASK                                                  
         MVC   CMPKBYR,BBYR                                                     
         MVC   CMPKCAM,BCAM                                                     
         MVC   CMPKMKT,BMKT                                                     
         L     RE,0(R5)            1ST PARAM - A(DAYS BIN)                      
         MVC   CMPKDAY,0(RE)                                                    
         L     RE,4(R5)            2ND PARAM - A(TIMES BIN)                     
         MVC   CMPKTIME,0(RE)                                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         BNE   GCMRNO                                                           
         CLC   IOKEY(13),IOKEYSAV  WE FOUND A COMP RECORD?                      
         BNE   GCMRNO                                                           
         GOTO1 AIO,FILGET3         YES, PUT IT INTO AIOAREA3                    
         BE    *+14                                                             
         CLI   IOERR,X'02'         DELETED                                      
         BE    GCMRNO                                                           
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA3                                                      
         USING CMPREC,R2                                                        
         LA    R6,CMSSTA           R6 = A(STATION LIST)                         
         LA    R1,1                STATION SEQ NUMBER                           
         L     RF,8(R5)            3RD PARAM - A(STA EBCDIC)                    
         LA    RE,CMSSEND                                                       
*                                                                               
GCMR10   CR    R6,RE               OUR STATION NOT IN THE LIST?                 
         BNL   GCMRNO              NO                                           
*                                                                               
         CLC   0(L'QSTA,RF),0(R6)  WE HAVE A STATION MATCH?                     
         BE    GCMR20                                                           
         LA    R1,1(R1)                                                         
         AHI   R6,L'CMSSTA                                                      
         B     GCMR10              LOOP UNTIL WE DO, OR END OF ELEM             
***************                                                                 
* R1 = STATION SEQ NUMBER                                                       
***************                                                                 
GCMR20   LA    RE,ESTDEMS          ESTIMATE DEMO CATEGORIES                     
         L     RF,12(R5)           4TH PARAM - A(DEMO VALUE LIST)               
*                                                                               
GCMR30   OC    0(3,RE),0(RE)       ANY MORE DEMOS?                              
         BZ    GCMRYES             NO MORE                                      
*                                                                               
         LA    R6,CMDEL            R6 = A(FIRST DEMO VALUE ELEM)                
         USING CMDEL,R6                                                         
GCMR32   CLI   CMDEL,CMDCODEQ      WE HAVE A DEMO ELEM (X'02')?                 
         BNE   GCMR30LP                                                         
         CLC   0(3,RE),CMDTYP      CORRECT DEMO ELEM?                           
         BE    GCMR34                                                           
         ZIC   R0,CMDLEN           NO, CHECK ALL DEMO ELEMS                     
         AR    R6,R0                                                            
         B     GCMR32                                                           
*                                                                               
GCMR34   XR    R0,R0               R0 = A(BYTE AFTER END OF THIS DEMO)          
         IC    R0,CMDLEN                                                        
         AR    R0,R6                                                            
         LA    R6,CMDDEMO          R6 = A(DEMO/SHR VALUES FOR EACH STA)         
         DROP  R6                                                               
*                                                                               
GCMR35   CLM   R1,1,0(R6)          ANY DEMO/SHR FOR OUR STATION?                
         BE    GCMR36              YES                                          
         AHI   R6,L'CMDDEMO                                                     
         CR    R6,R0                                                            
         BL    GCMR35                                                           
         B     GCMR30LP            NOT FOR THIS DEMO, NEXT DEMO CATEG.          
*                                                                               
GCMR36   MVC   0(4,RF),1(R6)       USE THIS VALUE INSTEAD                       
*                                                                               
GCMR30LP LA    RE,3(RE)            NEXT DEMO CATEGORY                           
         LA    RF,4(RF)                                                         
         B     GCMR30                                                           
*                                                                               
GCMRYES  SR    RC,RC                                                            
GCMRNO   LTR   RC,RC                                                            
GCMRX    XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
***********************************                                             
* RESTORE THESE REGISTERS THAT WE DROPPED EARLIER                               
***********************************                                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING T20700+X'4000',R6                                                
         USING T20700+X'3000',R8                                                
         EJECT                                                                  
***********************************************************************         
* PUT THE NWS COMPETITION RECORD IN AIOAREA3                                    
*                                                                               
* ON ENTRY:    4(R1)               PUTCMPRC'S PARAM LIST - A(APPARM)            
***********************************                                             
*              1ST PARAM BYTE  0   WHAT CHANGED, X'80'-PRG/X'40'-DEMO           
*                        BYTES 1-3 A(DAYS BINARY)                               
*              2ND PARAM           A(TIMES BINARY)                              
*              3RD PARAM           A(STATION TEXT)                              
*              4TH PARAM           A(DEMO ELEMENT)                              
***********************************                                             
* NOTE: HAVE TO ASSUME APPARM WILL NOT GET MESSED UP                            
*                                                                               
*       USES  R5  SO IT WON'T ADDRESS  TWAD                                     
*       USES  R6  SO IT WON'T ADDRESS  T20700+X'4000'                           
*       USES  R8  SO IT WON'T ADDRESS  T20700+X'3000'                           
***********************************************************************         
         DROP  R5,R6,R8                                                         
PTCOMPRC NMOD1 0,**PCMR**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R5,4(R1)            RESTORE A(GETCMPRC'S PARAMETER LIST)         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING CMPKEY,R2                                                        
         MVI   CMPKTYP,CMPKTYPQ    (X'0D6C')                                    
         MVI   CMPKFIL,CMPKFILQ                                                 
         MVC   CMPKAM,BAGYMD                                                    
         OC    CMPKAM,BBYRMASK                                                  
         MVC   CMPKBYR,BBYR                                                     
         MVC   CMPKCAM,BCAM                                                     
         MVC   CMPKMKT,BMKT                                                     
         L     RE,0(R5)            1ST PARAM - A(DAYS BIN)                      
         MVC   CMPKDAY,0(RE)                                                    
         L     RE,4(R5)            2ND PARAM - A(TIMES BIN)                     
         MVC   CMPKTIME,0(RE)                                                   
         DROP  R2                                                               
*                                                                               
         GOTO1 AIO,DIRHI+IO3                                                    
         BNE   PCMRNO                                                           
         CLC   IOKEY(13),IOKEYSAV  WE FOUND A COMP RECORD?                      
         BNE   PCMRNO                                                           
         GOTO1 AIO,FILGETU3        YES, PUT IT INTO AIOAREA3                    
         BE    *+14                                                             
         CLI   IOERR,X'02'         DELETED                                      
         BE    PCMRNO                                                           
         DC    H'0'                                                             
*                                                                               
         L     R2,AIOAREA3                                                      
         USING CMPREC,R2                                                        
         LA    R6,CMSSTA           R6 = A(STATION LIST)                         
         LA    R1,1                STATION SEQ NUMBER                           
         L     RF,8(R5)            3RD PARAM - A(STA EBCDIC)                    
         LA    RE,CMSSEND                                                       
*                                                                               
PCMR10   CR    R6,RE               OUR STATION NOT IN THE LIST?                 
         BNL   PCMRNO              NO                                           
*                                                                               
         CLC   0(L'QSTA,RF),0(R6)  WE HAVE A STATION MATCH?                     
         BE    PCMR20                                                           
         LA    R1,1(R1)                                                         
         AHI   R6,L'CMSSTA                                                      
         B     PCMR10              LOOP UNTIL WE DO, OR END OF ELEM             
***************                                                                 
* R1 = STATION SEQ NUMBER                                                       
***************                                                                 
PCMR20   TM    0(R5),X'80'         PROGRAM CHANGE?                              
         BZ    PCMR30              NO, THEN DEMO CHANGE?                        
         L     RE,16(R5)           A(PROGRAM NAME)                              
         LA    R6,CMDEL                                                         
PCMR22   CLI   0(R6),0                                                          
         BE    PCMR30                                                           
         CLI   0(R6),CMPCODEQ      WE HAVE A PROGRAM ELEM (X'03')?              
         BE    PCMR26                                                           
PCMR24   ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PCMR22                                                           
*                                                                               
         USING CMPEL,R6                                                         
PCMR26   CLM   R1,1,CMPSEQ                                                      
         BNE   PCMR24                                                           
         MVC   CMPPROG,0(RE)                                                    
         DROP  R6                                                               
***************                                                                 
* R1 = STATION SEQ NUMBER                                                       
***************                                                                 
PCMR30   TM    0(R5),X'40'         CHANGE DEMO?                                 
         BZ    PCMR50              NO                                           
*                                                                               
         L     RE,12(R5)           RE = A(DEMO ELEMENT)                         
         USING DMOEL,RE                                                         
         LA    RF,DMODEMO          RF = 1ST DEMO CATEGORY/VALUE                 
*                                                                               
PCMR40   LA    R6,CMDEL            R6 = A(FIRST DEMO VALUE ELEM)                
         USING CMDEL,R6                                                         
PCMR42   CLI   CMDEL,CMDCODEQ      WE HAVE A DEMO ELEM (X'02')?                 
         BNE   PCMR40LP                                                         
         CLC   0(3,RF),CMDTYP      CORRECT DEMO ELEM?                           
         BE    PCMR44                                                           
         ZIC   R0,CMDLEN           NO, CHECK ALL DEMO ELEMS                     
         AR    R6,R0                                                            
         B     PCMR42                                                           
*                                                                               
PCMR44   XR    R0,R0               R0 = A(BYTE AFTER END OF THIS DEMO)          
         IC    R0,CMDLEN                                                        
         AR    R0,R6                                                            
         MVC   ACFULL,CMDPUT       SAVE PUT TO CALC SHR                         
         NI    ACFULL,X'FF'-X'80'                                               
         LA    R6,CMDDEMO          R6 = A(DEMO/SHR VALUES FOR EACH STA)         
         DROP  R6                                                               
*                                                                               
PCMR45   CLM   R1,1,0(R6)          ANY DEMO/SHR FOR OUR STATION?                
         BE    PCMR46              YES                                          
         AHI   R6,L'CMDDEMO                                                     
         CR    R6,R0                                                            
         BL    PCMR45                                                           
         B     PCMR40LP            NOT FOR THIS DEMO, NEXT DEMO CATEG.          
*                                                                               
PCMR46   MVC   1(4,R6),4(RF)       USE THIS VALUE INSTEAD                       
         LR    R5,R1               TEMP STORAGE FOR STATION SEQ                 
         L     R1,4(RF)                                                         
***      N     R1,=X'7FFFFFFF'     GET RID OF THE OVERRIDE                      
         N     R1,=X'3FFFFFFF'     GET RID OF THE OVERRIDE + 2 DECIMAL          
         XR    R0,R0                                                            
***  2 DECIMAL  ***                                                             
         TM    4(RF),DMODEM2D      2 DECIMAL PRECISION?                         
         BZ    PCMR46E                                                          
         M     R0,=F'1000'                                                      
         B     PCMR46G                                                          
***  2 DECIMAL  ***                                                             
PCMR46E  M     R0,=F'10000'                                                     
*                                                                               
PCMR46G  OC    ACFULL,ACFULL                                                    
         BNZ   PCMR46M                                                          
         XR    R1,R1                                                            
         B     PCMR46P                                                          
*                                                                               
PCMR46M  D     R0,ACFULL                                                        
         AHI   R1,5                TO GET PRECISION ROUNDING                    
         XR    R0,R0                                                            
         D     R0,=F'10'                                                        
PCMR46P  ST    R1,5(R6)                                                         
         OI    5(R6),X'80'                                                      
         LR    R1,R5               RESTORE STATION SEQUENCE                     
*                                                                               
PCMR40LP LA    RF,8(RF)            NEXT DEMO CATEGORY                           
         XR    R0,R0               ANY MORE DEMOS?                              
         IC    R0,1(RE)                                                         
         AR    R0,RE                                                            
         CR    RF,R0                                                            
         BL    PCMR40              YES, REPLACE ALL OF THEM                     
*                                                                               
PCMR50   GOTO1 AIO,FILPUT3         AND THEN WRITE IT OUT                        
*                                                                               
PCMRYES  SR    RC,RC                                                            
PCMRNO   LTR   RC,RC                                                            
PCMRX    XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
***********************************                                             
* RESTORE THESE REGISTERS THAT WE DROPPED EARLIER                               
***********************************                                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING T20700+X'4000',R6                                                
         USING T20700+X'3000',R8                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE/GET SPECIAL BOOK TYPE CODE                                           
*                                                                               
* ON ENTRY:    4(R1)               GETBKTYP'S PARAM LIST - A(APPARM)            
***********************************                                             
*              1ST PARAM BYTE  0   FORMAT, C'B' BINARY, C'C' CHARACTER          
*                                  C'A' BINARY -> 2 CHARACTER                   
*                        BYTES 1-3 A(BOOK TYPE CODE)                            
*              2ND PARAM           A(RETURN BOOK TYPE CODE)                     
* RETURN CODE IS OPPOSITE FORMAT TO INPUT - IF BINARY CODE INPUT THEN           
* THE CORRESPONDING CHAR CODE RETURNED. IF P2=A(0) NO CODE IS RETURNED          
***********************************                                             
* ON EXIT :    1ST APPARM BYTE 0   SET TO X'FF' IF INVALID CODE                 
*                                                                               
*       USES  R5  SO IT WON'T ADDRESS  TWAD                                     
*       USES  R6  SO IT WON'T ADDRESS  T20700+X'4000'                           
*       USES  R8  SO IT WON'T ADDRESS  T20700+X'3000'                           
***********************************************************************         
         DROP  R5,R6,R8                                                         
GTBKTYPE NMOD1 0,**GTBK**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R5,4(R1)            RESTORE A(GETBKTYP'S PARAMETER LIST)         
*                                                                               
         L     RE,0(R5)            A(INPUT CODE)                                
         LR    R1,RE               FOR CHECKS IN GTBK20                         
         MVC   RBYTE,0(RE)                                                      
         L     RF,4(R5)            A(OUTPUT CODE)                               
         LA    RE,BKTYPTAB                                                      
         CLI   0(R5),C'C'          FORMAT                                       
         BE    GTBK20                                                           
         CLI   0(R5),C'B'                                                       
         BE    GTBK00                                                           
         CLI   0(R5),C'A'          WE NEED 2 CHAR FROM ALPHANUMERIC             
         BE    GTBK40                                                           
         BNE   GTBKERR             INVALID FORMAT - RETURN ERROR                
* BINARY CODE                                                                   
GTBK00   NI    RBYTE,X'FF'-BMNBITSQ    REMOVE MONTH BITS FOR TEST               
         CLI   RBYTE,BTY2CHAR      X'E0' - 2 CHARACTER BOOKTYPES + L            
         BE    GTBK40               - YES, GO THERE                             
*                                                                               
GTBK10   CLI   0(RE),0                                                          
***      BE    GTBKERR             NOT IN TABLE!                                
         BE    GTBK40              NOT IN TABLE!  TRY DEMTABS                   
         CLC   RBYTE,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,L'BKTYPTAB(RE)                                                
         B     GTBK10                                                           
         LTR   RF,RF               RETURN CHARACTER CODE IF REQUIRED            
         BZ    *+10                                                             
         MVC   0(1,RF),1(RE)                                                    
***      MVI   1(RF),C' '                                                       
         B     GTBKXIT                                                          
* EBCIDIC CODE                                                                  
GTBK20   DS    0H                                                               
         CLI   1(R1),0             SOMETHING IN THE 2ND BYTE?                   
         BNE   GTBK40               - YUP                                       
***      L     R2,8(R5)            ANYTHING IN 3RD PARAMETER (2 CHAR)           
***      OC    0(4,R2),0(R2)                                                    
***      BZ    GTBK20E              - NOPE                                      
***      LTR   RF,RF               ANYTHING IN 2ND PARAMETER?                   
***      BNZ   GTBK40               - CERTAINLY                                 
*                                                                               
GTBK20E  CLI   0(RE),0                                                          
***      BE    GTBKERR             NOT IN TABLE!                                
         BE    GTBK40              NOT IN TABLE!  TRY DEMTABS                   
         CLC   RBYTE,1(RE)                                                      
         BE    *+12                                                             
         LA    RE,L'BKTYPTAB(RE)                                                
         B     GTBK20                                                           
         LTR   RF,RF               RETURN BINARY CODE IF REQUIRED               
         BZ    *+10                                                             
         OC    0(1,RF),0(RE)       NOTE: 'OR' TO PRESERVE MONTH BITS!           
         B     GTBKXIT                                                          
*                                                                               
***************************     2 CHARACTER BOOKTYPES     MHC 11/28/06          
GTBK40   DS    0H                                                               
         L     RF,ACPARMA                                                       
         L     RF,16(RF)           CALL GLOBBER                                 
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),ACPARM,SPBOOKTB   GET A(BOOK TABLE)                         
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
         L     R2,0(R5)            A(INPUT CODE)                                
         L     R3,4(R5)            A(OUTPUT CODE)                               
         L     R4,8(R5)            A(OUTPUT 2 CHAR)                             
         CLI   0(R5),C'C'          FORMAT                                       
         BE    GTBK60                                                           
         CLI   0(R5),C'B'                                                       
         BE    GTBK50                                                           
         CLI   0(R5),C'A'                                                       
         BE    GTBK80                                                           
         BNE   GTBKERR             INVALID FORMAT - RETURN ERROR                
*                                                                               
GTBK50   DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R4),SPBKTYPN    R4 HAS THE ALPHANUMERIC                      
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     GTBK50                                                           
         MVC   0(2,R3),SPBKTYPA    BOOK TYPE                                    
         B     GTBKXIT                                                          
*                                                                               
GTBK60   DS    0H                                                               
         CLI   1(R2),0             IS IT NULL?                                  
         BNE   *+8                                                              
         MVI   1(R2),C' '          MAKE IT A SPACE INSTEAD FOR TABLE            
GTBK62   CLI   0(RF),X'FF'                                                      
         BE    GTBKERR                                                          
         CLC   0(2,R2),SPBKTYPA                                                 
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     GTBK62                                                           
*                                                                               
         CLI   SPBKTYPN,0          STANDARD (NO BOOK TYPE)?                     
         BE    GTBK65              NO BOOK TYPE                                 
*                                                                               
         LTR   R3,R3               ANYTHING HERE?                               
         BZ    GTBK65               - NOPE                                      
         OI    0(R3),BTY2CHAR      WE ARE DOING 2 CHARACTERS + L                
GTBK65   MVC   0(1,R4),SPBKTYPN                                                 
         B     GTBKXIT                                                          
*                                                                               
GTBK80   DS    0H                                                               
         CLC   0(1,R4),SPBKTYPN    R4 HAS THE ALPHANUMERIC                      
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     GTBK80                                                           
         MVC   0(2,R3),SPBKTYPA    BOOK TYPE                                    
         DROP  RF                                                               
         B     GTBKXIT                                                          
***************************     2 CHARACTER BOOKTYPES     MHC 11/28/06          
*                                                                               
GTBKERR  MVI   0(R5),X'FF'                                                      
*                                                                               
GTBKXIT  XIT1  ,                                                                
*                                                                               
* SPECIAL TYPES TABLE - NOTE: MASKS ARE BIT COMBINATIONS                        
* NOTE: SOME ROOT ROUTINES DO NOT ACCESS THIS TABLE AND HAVE OWN CODE           
BKTYPTAB DS    0XL2                 SEE BOOKTYPD FOR SETTINGS                   
         DC    AL1(BTY2CHAR),X'FE'  - 2 CHARACTER BOOK TYPES + L                
         DC    AL1(BTYZR4Q),C'4'    - ZERO 4                                    
         DC    AL1(BTYZR1Q),C'1'    - ZERO 1                                    
         DC    AL1(BTYHPEOQ),C'I'   - HISPANIC PEOPLE METER                     
         DC    AL1(BTYOLYMQ),C'O'   - OLYMPICS                                  
         DC    AL1(BTYPRNTQ),C'A'   - PARENT ONLY DATA                          
         DC    AL1(BTYHISPQ),C'H'   - HISPANIC                                  
         DC    AL1(BTYBLAKQ),C'B'   - BLACK                                     
         DC    AL1(BTYPEOPQ),C'P'   - PEOPLE METER                              
         DC    AL1(BTYTRADQ),C'T'   - TRADE                                     
         DC    AL1(BTYMTROQ),C'M'   - METRO                                     
         DC    AL1(BTYDMAQ),C'D'    - DMA                                       
         DC    AL1(BTYOTHRQ),C'E'   - OTHER                                     
         DC    AL1(0)                                                           
*                                                                               
         LTORG                                                                  
***********************************                                             
* RESTORE THESE REGISTERS THAT WE DROPPED EARLIER                               
***********************************                                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING T20700+X'4000',R6                                                
         USING T20700+X'3000',R8                                                
         EJECT                                                                  
***********************************************************************         
* FORMAT UPGRADE TO GET CORRECT INPUT STREAM                                    
*                                                                               
* ON ENTRY:    4(R1)               FMTUPG'S PARAM LIST - A(APPARM)              
***********************************                                             
* NOTE: USES  R5  SO IT WON'T ADDRESS  TWAD                                     
*       USES  R6  SO IT WON'T ADDRESS  T20700+X'4000'                           
*       USES  R8  SO IT WON'T ADDRESS  T20700+X'3000'                           
***********************************                                             
* NOTE: THIS IS COPIED FROM SPBUY22/LD530 AND MODIFIED TO FIT                   
***********************************************************************         
         DROP  R5,R6,R8                                                         
FMTUPGRD NMOD1 0,**FMUP**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R1,4(R1)            RESTORE A(FMTUPG'S PARAMETER LIST)           
*                                                                               
         L     R2,0(R1)                                                         
         USING UPELEM,R2                                                        
         L     R4,4(R1)                                                         
         USING NBRUPELD,R4                                                      
         L     R3,8(R1)                                                         
         USING DBLOCKD,R3                                                       
*                                                                               
         LA    R6,NBRUPINP                                                      
         DROP  R4                                                               
         MVC   0(4,R6),=C'UPX='                                                 
         MVC   2(1,R6),UPFILE                                                   
*                                                                               
         LA    R1,UPTAB                                                         
LD532    CLI   0(R1),0             TEST E-O-T                                   
         BE    LD534               YES - MUST BE DEMO UPGRADE                   
         CLC   0(2,R1),UPTYPE      MATCH SPUPTYPE/SPUPSTYP                      
         BE    *+12                                                             
         LA    R1,L'UPTAB(R1)                                                   
         B     LD532                                                            
         MVC   4(6,R6),2(R1)       MOVE UPGRADE TYPE NAME                       
         B     LD536                                                            
*                                                                               
LD534    XC    APDUB,APDUB             DEMO UPGRADE                             
         MVC   APDUB+1(2),UPTYPE                                                
         MVI   APDUB+3,X'FF'                                                    
         XC    RPARM(8),RPARM                                                   
         GOTO1 VDEMOCON,RPARM,(1,APDUB),(6,4(R6)),(C'S',DBLOCK)                 
*                                                                               
LD536    LA    R6,10(R6)           FIND END OF UPGRADE NAME                     
         CLI   0(R6),C' '                                                       
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'/'          ATTACH DELIMITER                             
         LA    R6,2(R6)                                                         
*                                                                               
         LA    R5,UPTYPE+2         POINT TO FIRST UPGRADE VALUE                 
         LA    R8,3                SET NUMBER OF VALUES                         
*                                                                               
LD540    OC    0(2,R5),0(R5)       TEST VALUE PRESENT                           
         BZ    LD544                                                            
         CLC   0(2,R5),=H'500'     TEST BOOK OR VALUE                           
         BNH   LD542                                                            
         NI    1(R5),X'FF'-BTYBITSQ  CAN'T HAVE SPECIAL BOOK TYPE HERE          
         BAS   RE,EDITBK                                                        
         B     LD544                                                            
*                                                                               
LD542    SR    R0,R0                                                            
         ICM   R0,3,0(R5)                                                       
         CVD   R0,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(4,R6),APDUB                                                    
         CLI   0(R6),C'0'                                                       
         BNE   LD544                                                            
         MVC   0(4,R6),1(R6)                                                    
         LA    R6,6(R6)                                                         
*                                                                               
LD544    LA    R5,2(R5)            BUMP TO NEXT VALUE                           
         BCT   R8,LD540            DO FOR MAX VALUES                            
         BCTR  R6,0                OVERWRITE LAST DELIMITER                     
         MVI   0(R6),C' '                                                       
*                                                                               
         OC    UPFBK,UPFBK                                                      
         BZ    LD546                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(3,R6),=C'BK='                                                  
         LA    R6,3(R6)                                                         
         LA    R5,UPFBK                                                         
         BAS   RE,EDITBK                                                        
*                                                                               
         CLI   UPELEM+1,49                                                      
         BL    *+14                                                             
         OC    UPFBKLST,UPFBKLST   TEST BOOK LIST                               
         BNZ   LD544A                                                           
         BCTR  R6,0                OVERWRITE LAST DELIMETER                     
         MVI   0(R6),C' '                                                       
         LA    R6,12(R6)                                                        
         B     LD546                                                            
*                                                                               
LD544A   LA    R0,L'UPFBKLST/2                                                  
         LA    R5,UPFBKLST                                                      
*                                                                               
LD545    OC    0(2,R5),0(R5)                                                    
         BZ    LD545A                                                           
         BAS   RE,EDITBK                                                        
         LA    R5,2(R5)                                                         
         BCT   R0,LD545                                                         
*                                                                               
LD545A   BCTR  R6,0                                                             
         MVI   0(R6),C' '                                                       
         LA    R6,2(R6)                                                         
*                                                                               
LD546    DS    0H                                                               
*&&DO                                                                           
LD546    OC    UPDAYTIM,UPDAYTIM                                                
         BZ    LD547                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(3,R6),=C'DT='                                                  
         GOTO1 VCALLOV,RPARM,0,X'D9000A0F' GET DAYUNPK ADDRESS                  
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),UPDAYTIM,3(R6)                                         
         LA    R6,11(R6)                                                        
         BAS   RE,LDCOMMA2                                                      
         BCTR  R6,0                                                             
         MVI   0(R6),C'/'                                                       
*                                                                               
         GOTO1 VCALLOV,RPARM,0,X'D9000A11' GET UNTIME ADDRESS                   
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),UPDAYTIM+1,1(R6)                                       
*                                                                               
         OC    UPSTA,UPSTA         TEST STATION OVERRIDE                        
         BZ    LD547                                                            
         LA    R6,14(R6)                                                        
         BAS   RE,LDCOMMA2                                                      
         BCTR  R6,0                                                             
         MVI   0(R6),C'/'                                                       
         MVC   1(4,R6),UPSTA                                                    
         LA    R6,5(R6)                                                         
*&&                                                                             
LD547    CLI   1(R2),41            TEST ELEMENT HAS OLD LENGTH                  
         BNH   LD549               YES                                          
         CLI   UP2YRP,C'Y'         NO-PUT/SHR 2 YEAR AVERAGING                  
         BNE   LD548                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(5,R6),=C'PUT=2'                                                
         LA    R6,5(R6)                                                         
*                                                                               
LD548    CLI   UP2YRS,C'Y'                                                      
         BNE   LD549                                                            
         BAS   RE,LDCOMMA2                                                      
         MVC   0(5,R6),=C'SHR=2'                                                
*                                                                               
LD549    DS    0H                                                               
*                                                                               
FUPGX    XIT1                                                                   
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* R5 POINTS TO THE BOOK. PLACE OUTPUT AT 0(R6)                                  
* R6 POINTS TO NEXT OUTPUT ADDRESS ON EXIT                                      
***********************************************************************         
EDITBK   NTR1                                                                   
         MVC   APDUB(2),0(R5)                                                   
         MVI   APDUB+2,1                                                        
         NI    APDUB+1,X'FF'-BTYBITSQ   TURN OFF SPECIAL TYPE                   
         GOTO1 VDATCON,RPARM,(3,APDUB),(6,ACWORK)                               
         MVC   ACWORK+3(2),ACWORK+4     MOVE YEAR LEFT                          
         MVC   ACWORK+5(5),=5C' '                                               
         TM    1(R5),BTYBITSQ                                                   
         BZ    EDITBK10                                                         
         MVC   ACWORK+5(3),=C'( )'                                              
         MVC   RBYTE,1(R5)                                                      
         NI    RBYTE,X'FF'-BMNBITSQ     REMOVE MONTH FOR TEST                   
*                                                                               
         LA    RE,BKTYPTB2         (CAN'T ADDRESS BKTYPTAB)                     
EDITBK5  CLI   0(RE),0                                                          
         BE    EDITBK7             NOT IN TABLE!                                
         CLC   RBYTE,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,L'BKTYPTB2(RE)                                                
         B     EDITBK5                                                          
         MVC   ACWORK+6(1),1(RE)                                                
         B     EDITBK10                                                         
*                                                                               
EDITBK7  DS    0H                  NOT IN TABLE, SPECIAL BOOKTYPE               
         L     RF,ACPARMA          LET'S TRY THE DEMTAB                         
         L     RF,16(RF)                                                        
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),RPARM,SPBOOKTB   GET A(BOOK TABLE)                          
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R4,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
EDITBK8  DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   SPBKTYPN,APWORK     DO WE HAVE THE RIGHT BINARY?                 
         BE    *+10                                                             
         AR    RF,R4                                                            
         B     EDITBK8                                                          
         OC    APWORK,APWORK       ANYTHING HERE?                               
         BZ    EDITBK10             - NOPE                                      
         MVC   ACWORK+6(2),SPBKTYPA                                             
         MVI   ACWORK+8,C')'                                                    
         CLI   ACWORK+7,C' '       ANY CHARACTER HERE?                          
         BNE   *+10                                                             
         MVC   ACWORK+7(2),=C') '                                               
         DROP  RF                                                               
****     MVC   ACWORK+6(1),APWORK   THIS SHOULD HAVE THE BOOKTYPE               
*                                                                               
EDITBK10 MVC   0(10,R6),ACWORK                                                  
         LA    R6,10(R6)                                                        
*                                                                               
         CLI   0(R6),C' '          FIND END OF EDITED VALUE                     
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         MVI   1(R6),C'/'                                                       
         LA    R6,2(R6)                                                         
         XIT1  REGS=(R6)                                                        
***********************************                                             
* PUT A COMMA ON THE OUTPUT LINE                                                
***********************************                                             
LDCOMMA2 OI    0(R6),C' '                                                       
         CLI   0(R6),C' '                                                       
         BNE   *+8                                                              
         BCT   R6,LDCOMMA2                                                      
         MVI   1(R6),C','                                                       
         LA    R6,2(R6)                                                         
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
UPTAB    DS    0XL8                SPUPTYPE/SPUTSTYP/UPGRADE NAME               
         DC    AL1(SPUPTRTG),X'0',C'RATING'                                     
         DC    AL1(SPUPTHUT),X'0',C'HUT   '                                     
         DC    AL1(SPUPTPUT),C'P',C'PUT   '                                     
         DC    AL1(SPUPTNDX),X'0',C'INDEX '                                     
         DC    AL1(SPUPTHPT),X'0',C'HPT   '                                     
         DC    AL1(SPUPTPIN),C'N',C'PIN   '                                     
         DC    AL1(SPUPTPIM),C'M',C'PIM   '                                     
         DC    AL1(SPUPTPIY),C'Y',C'PIY   '                                     
         DC    AL1(SPUPTPIQ),C'Q',C'PIQ   '                                     
         DC    AL1(SPUPTPIB),C'B',C'PIB   '                                     
         DC    AL1(SPUPTSIN),C'N',C'SIN   '                                     
         DC    AL1(SPUPTSIM),C'M',C'SIM   '                                     
         DC    AL1(SPUPTSIY),C'Y',C'SIY   '                                     
         DC    AL1(SPUPTSIQ),C'Q',C'SIQ   '                                     
         DC    AL1(SPUPTSIB),C'B',C'SIB   '                                     
         DC    AL1(SPUPTPAV),C'N',C'PAVG  '                                     
         DC    AL1(SPUPTPAY),C'Y',C'PAY   '                                     
         DC    AL1(SPUPTPAQ),C'Q',C'PAQ   '                                     
         DC    AL1(SPUPTPAB),C'B',C'PAB   '                                     
         DC    AL1(SPUPTSAV),C'N',C'SAVG  '                                     
         DC    AL1(SPUPTSAY),C'Y',C'SAY   '                                     
         DC    AL1(SPUPTSAQ),C'Q',C'SAQ   '                                     
         DC    AL1(SPUPTSAB),C'B',C'SAB   '                                     
         DC    X'00'                                                            
*                                                                               
* SPECIAL TYPES TABLE - NOTE: MASKS ARE BIT COMBINATIONS                        
* NOTE: COPY OF BKTYPTAB WHICH WE CANNOT ACCESS                                 
BKTYPTB2 DS    0XL2                 SEE BOOKTYPD FOR SETTINGS                   
         DC    AL1(BTYZR4Q),C'4'    - ZERO 4                                    
         DC    AL1(BTYZR1Q),C'1'    - ZERO 1                                    
         DC    AL1(BTYHPEOQ),C'I'   - HISPANIC PEOPLE METER                     
         DC    AL1(BTYOLYMQ),C'O'   - OLYMPICS                                  
         DC    AL1(BTYPRNTQ),C'A'   - PARENT ONLY DATA                          
         DC    AL1(BTYHISPQ),C'H'   - HISPANIC                                  
         DC    AL1(BTYBLAKQ),C'B'   - BLACK                                     
         DC    AL1(BTYPEOPQ),C'P'   - PEOPLE METER                              
         DC    AL1(BTYTRADQ),C'T'   - TRADE                                     
         DC    AL1(BTYMTROQ),C'M'   - METRO                                     
         DC    AL1(BTYDMAQ),C'D'    - DMA                                       
         DC    AL1(BTYOTHRQ),C'E'   - OTHER                                     
         DC    AL1(0)                                                           
***********************************                                             
* RESTORE THESE REGISTERS THAT WE DROPPED EARLIER                               
***********************************                                             
         L     R5,ATWA                                                          
         USING TWAD,R5             R5=A(TWA)                                    
         USING T20700+X'4000',R6                                                
         USING T20700+X'3000',R8                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL SPDEMUP                                                       
*                                                                               
* ON ENTRY:    PARAM 1             RC = A(WORK AREA)                            
*              PARAM 2             R1 = A(SPDEMUP'S PARAMETER LIST)             
***********************************************************************         
SPTDEMUP NMOD1 0,**SDUP**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
         L     R1,4(R1)            RESTORE A(SPDEMUP'S PARAMETER LIST)          
*                                                                               
         L     R4,0(R1)                                                         
         USING SPDEMUPD,R4                                                      
*                                                                               
         L     RE,ATWA             GIVE SPDEMUP MARKET'S LPM START DATE         
         AHI   RE,SVMLPMSD-TWAD                                                 
         MVC   SPUPLPM,0(RE)                                                    
         L     RE,ATWA             GIVE SPDEMUP MARKET'S ALPHA CODE             
         AHI   RE,SVMALPHA-TWAD                                                 
         MVC   SPUPMALF,0(RE)                                                   
*                                                                               
         CLI   G1WPROF+5,C'I'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPDMAI                                                
*                                                                               
         CLI   G1WPROF+7,C'Y'      AS PER ZEN                                   
         BNE   *+8                                                              
         OI    SPUPOPTS,SPOPNORM                                                
*                                                                               
         CLI   SPUPSRC,C'A'        TEST SRC=ARB                                 
         BE    *+12                                                             
         CLI   SPUPSRC,C'N'        OR NSI                                       
         BNE   SPDEMUPG                                                         
         CLI   SPUPMED,C'T'        AND MEDIA=T                                  
         BNE   SPDEMUPG                                                         
         CLI   CMPRSVC,0           AND NO RATING SERVICE OVERRIDE               
         BNE   SPDEMUPG                                                         
         CLC   CMPND,=X'5D0C1A'    AND CAMPAIGN ENDS AFTER 1993                 
         BNH   SPDEMUPG                                                         
         TM    CMPOPTS2,CAMOF94A   TEST F94=ARB OPTION ON FOR CAMPAIGN          
         BZ    SPDEMUP1                                                         
         TM    STAIND,STAIF94A     AND F94=ARB OPTION ON FOR STATION            
         BZ    SPDEMUP1                                                         
         CLC   CMPSTDT,=X'5E031C'  YES-DON'T FORCE NSI FOR CAMPAIGNS            
         BNL   SPDEMUP1            THAT START BEFORE 3/28/94                    
         MVI   SPUPSRC,C'A'        AND MAKE SURE SRC=ARB                        
         B     SPDEMUPG                                                         
*                                                                               
SPDEMUP1 CLI   QSTA,C'0'           WE HAVE A SYSCODE HERE?                      
         BNL   SPDEMUPG             - YUP, DON'T DO THE CRAP BELOW THIS         
         MVI   SPUPSRC,C'N'        FORCE SERVICE TO NSI                         
         XC    SPUPMKT,SPUPMKT     AND CLEAR MKT OVERRIDE POSSIBILITY           
*                                                                               
SPDEMUPG GOTO1 VSPDEMUP            CALL SPDEMUP                                 
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET GOAL DOLLARS AND POINTS                                        
*                                                                               
* EXIT - CC=EQUAL IF SUCCESSFUL AND                                             
*                 IOAREA3+000(004) = TOTAL GOAL DOLLARS                         
*                 IOAREA3+004(004) = TOTAL GOAL POINTS                          
*                 IOAREA3+008(212) = GOAL DOLLARS BY WEEK (53X4)                
*                 IOAREA3+220(212) = GOAL POINTS  BY WEEK (53X4)                
*        CC=NOT EQUAL ON ERROR WITH FVMSGNO SET TO ERROR NUMBER                 
*                                                                               
*        FOR CANADIAN AGENCIES, NETIND=NETIDATA IF THERE IS                     
*        NETWORK BUY DATA IN SAVED STORAGE (TSAR)                               
***********************************************************************         
*        SPACE 1                                                                
GETG     NMOD1 0,**GETG**                                                       
         USING WORKD,R7                                                         
         L     RC,0(R1)                                                         
         USING RWRKD,RC                                                         
*                                                                               
         L     R3,AIOAREA3         R3 = A(IOAREA3)                              
         LR    RE,R3                                                            
         LA    RF,432                                                           
         XCEFL                                                                  
*                                                                               
         TM    0(R1),X'80'         TEST SKIP READING BWS GOALS                  
         BO    GETG0                                                            
         CLI   BPRD,X'FF'          NO-TEST PRD=POL AND GOALS WERE ADDED         
         BNE   GETG0                  FOR THIS CAMPAIGN                         
         TM    CMPIND,CMPIGOAL                                                  
         BZ    GETG0                                                            
         BAS   RE,BWSGOAL          YES-GET BWS GOALS RECORD                     
         B     GETGX                                                            
*                                                                               
GETG0    MVI   RFLAG,0                                                          
         XC    RWORK2,RWORK2       BUILD LIST OF DAYPARTS                       
         MVC   RWORK2(1),BDPT      THIS DAYPART                                 
         CLI   CMPDPOPT,C'S'       TEST SUBDPTS SCHEDULED SEPERATELY            
         BE    *+10                YES-THEN NO SUBDAYPARTS                      
         MVC   RWORK2+1(L'DPTSUBS),DPTSUBS                                      
*                                                                               
         GOTO1 VDATCON,RPARM,(3,CMPND),(2,RHALF) RHALF = CAMP END DATE          
*                                                                               
         MVC   TWADPT,BDPT         SAVE DAYPART/LENGTH FOR THIS GETGOAL         
         MVI   TWASLN,30                                                        
         CLI   BSLN,0                                                           
         BE    *+10                                                             
         MVC   TWASLN,BSLN                                                      
*                                                                               
         TM    INOIND,INOIXGL      EXIT NOW IF GOALS=N                          
         BO    GETGX                                                            
*                                                                               
         CLI   CMPPGRPN,0          TEST PRODUCT GROUPS                          
         BE    *+8                                                              
         BAS   RE,GETPLIST         YES-GET PRODUCT LIST FROM CLIENT REC         
*                                                                               
         XC    RFULL,RFULL                                                      
         OC    CMPELIST,CMPELIST   TEST CAMPAIGN ESTIMATE LIST                  
         BZ    GETG1                                                            
         LA    R1,CMPELIST         YES-RFULL=POINTER INTO THE LIST              
         ST    R1,RFULL                                                         
         LA    R1,L'CMPELIST(R1)                                                
         ST    R1,RFULL2                                                        
         MVC   RFULL(1),BEST                                                    
*                                                                               
GETG1    GOTO1 CKIDRBYR,ACPARM,ACWORK   SAVE IDRBYR IN ACWORK                   
*                                                                               
         CLC   CMPELIST(2),=AL2(CESTALLQ)                                       
         BE    GETG28AA            DETERMINE FIRST ESTIMATE (1-255)             
*                                                                               
GETG1A1  LA    R2,IOKEY            GET THE GOAL RECORD                          
         USING GOALRECD,R2                                                      
         XC    GKEY,GKEY                                                        
         LA    R5,RWORK2                                                        
         CLI   CMPPGRPN,0          TEST PRODUCT GROUPS                          
         BE    GETG1A                                                           
         BAS   RE,FSTPRD           GET FIRST PRODUCT IN GROUP                   
         BNE   GETG30              GET OUT IF NO PRODUCTS                       
         MVC   GKEYPRD,RBYTE                                                    
         B     GETG2                                                            
*                                                                               
GETG1A   CLI   BPRD,X'FF'          FOR PRODUCT POOL,                            
         BNE   GETG2                                                            
         MVI   GKEYPRD,1           READ ALL PRODUCTS                            
*                                                                               
GETG2    LA    R2,IOKEY                                                         
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM,BAGYMD                                                    
         CLI   CUDMED,C'C'         TEST CANADIAN AGENCY                         
         BNE   GETG3                                                            
         CLI   CMPGOALS,C'C'       AND CAMPAIGN ASKS FOR MEDIA C GOALS          
         BNE   GETG3                                                            
         NI    GKEYAM,X'F0'        YES-CHANGE MEDIA TO COMBINED                 
         OI    GKEYAM,X'08'                                                     
*                                                                               
GETG3    MVC   GKEYCLT,BCLT                                                     
*                                                                               
         CLI   CMPPGRPN,0          PRODUCT ALREADY SET FOR PRDGRPS              
         BNE   *+18                AND POL                                      
         CLI   BPRD,X'FF'                                                       
         BE    *+10                                                             
         MVC   GKEYPRD,BPRD                                                     
*                                                                               
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,BEST                                                     
*                                                                               
****************         NEW "ALLGOALS" OPTION ADDED                            
         TM    INOIND,INOIALG      IS IT ALL GOALS FROM ALL DAYPARTS?           
         BZ    GETG3C               - NEGATIVE SIRE, CONTINUE                   
         B     *+10                DON'T WIPE OUT THE KEY FIRST TIME            
*                                                                               
GETG3A   XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,X'02'                                                   
         MVC   GKEYAM,BAGYMD                                                    
         CLI   CUDMED,C'C'         TEST CANADIAN AGENCY                         
         BNE   GETG3B                                                           
         CLI   CMPGOALS,C'C'       AND CAMPAIGN ASKS FOR MEDIA C GOALS          
         BNE   GETG3B                                                           
         NI    GKEYAM,X'F0'        YES-CHANGE MEDIA TO COMBINED                 
         OI    GKEYAM,X'08'                                                     
*                                                                               
***  THE PRODUCT IS ALREADY TAKEN CARE OF                                       
*                                                                               
GETG3B   MVC   GKEYCLT,BCLT                                                     
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,BEST                                                     
*                                                                               
         LA    R1,DIRHI            IT'S ALL GOALS, GO RIGHT TO READHI           
         B     GETG4                                                            
****************                        07/08/03  MHC                           
*                                                                               
GETG3C   CLI   0(R5),0             TEST ANY MORE DAYPARTS                       
         BE    GETG27                                                           
         MVC   GKEYDPT,0(R5)                                                    
         MVI   GKEYSLN,0                                                        
*                                                                               
         CLI   CMPPGRPN,0          DON'T SET LENGTH FOR POL AND NOT             
         BNE   *+12                PRODUCT GROUPS                               
         CLI   BPRD,X'FF'                                                       
         BE    *+14                                                             
         L     R1,ATWA                                                          
         MVC   GKEYSLN,TWASLN-TWAD(R1)                                          
*                                                                               
         MVI   GKEYSEC,0                                                        
         MVI   GKEYAGY,0                                                        
         MVI   GKEYPRD2,0                                                       
         LA    R1,DIRHI                                                         
*                                                                               
         CLI   CMPPRD1,0           TEST FOR PIGGYBACK PRODUCTS                  
         BE    GETG4                                                            
         MVC   GKEYPRD,CMPPRD1     YES - SET PRODUCT 1                          
         MVC   GKEYSLN,CMPLEN1               SPOT LENGTH 1                      
*                                                                               
         MVI   GKEYSEC,X'1E'                 TOTAL SPOT LENGTH                  
         CLI   CMPSLN,0                                                         
         BE    *+10                                                             
         MVC   GKEYSEC,CMPSLN                                                   
*                                                                               
         MVC   GKEYPRD2,CMPPRD2              PRODUCT 2                          
*                                                                               
GETG4    GOTO1 AIO                                                              
         BNE   GETGX                                                            
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         CLI   CMPPGRPN,0          TEST NOT PRODUCTS GROUPS, AND                
         BNE   GETG7                                                            
         CLI   BPRD,X'FF'          PRODUCT POOL                                 
         BNE   GETG7                                                            
         CLC   GKEY(GKEYPRD-GKEY),IOKEYSAV    YES - CHECK SAME A/M-CLT          
         BNE   GETG28                                                           
         CLI   GKEYPRD,X'FF'                        CHECK GOAL NE POL           
         BE    GETG28                                                           
****                                                                            
***  'AFG'  ***                                                                 
         TM    INOIND,INOIALG      WE DOING ALL GOALS?                          
         BZ    GETG4E               - NOPE, NOT AT ALL, CONTINUE NORMAL         
         CLC   GKEYMKT(GKEYDPT-GKEYMKT),IOKEYSAV+GKEYMKT-GKEY                   
         BE    GETG5C              SAME MARKET AND ESTIMATE                     
         BL    GETG4C                                                           
*                                                                               
         XR    R0,R0                                                            
         IC    R0,GKEYPRD          RAISE THE PRODUCT BY 1                       
         AHI   R0,1                                                             
         STC   R0,GKEYPRD                                                       
*                                                                               
GETG4C   XC    APWORK,APWORK                                                    
         MVC   APWORK(5),GKEY      MOVE IN TYPE/AM/CLT/PRD                      
         MVC   GKEY(L'IOKEY),APWORK                                             
         MVC   GKEYMKT,BMKT        FILL IN MARKET AND ESTIMATE AGAIN            
         MVC   GKEYEST,BEST                                                     
         LA    R1,DIRHI            IT'S ALL GOALS, GO RIGHT TO READHI           
         B     GETG4                                                            
***  'AFG'  ***                                                                 
****                                                                            
GETG4E   CLC   GKEYPRD,IOKEYSAV+GKEYPRD-GKEY        TEST NEW PRODUCT            
         BE    GETG5                                                            
         CLI   CMPPRD1,0                            YES-TEST PIGGY              
         BNE   GETG28                                                           
         LA    R5,RWORK2                            NO-THEN START AT            
         B     GETG2                                   FIRST DAYPART            
*                                                                               
*****  NEW CODE ADDED FOR 'AFG' OPTION                ************              
GETG5    TM    INOIND,INOIALG      WE DOING ALL GOALS?                          
         BZ    GETG5E               - NOPE, NOT AT ALL, CONTINUE NORMAL         
         CLC   GKEY(GKEYDPT-GKEY),IOKEYSAV   UP TO ESTIMATE IS FINE             
         BNE   GETG26                                                           
GETG5C   CLC   GKEYEST,BEST        CHECK ESTIMATE ISN'T TOO HIGH                
         BH    GETG28              NOT GETG27 BECAUSE WE'RE DONE! AFG           
         CLC   GKEYMKT,BMKT        CHECK MARKET ISN'T TOO HIGH                  
         BH    GETG28              NOT GETG27 BECAUSE WE'RE DONE! AFG           
         B     GETG5X                                                           
*****                                  MHC  08/26/03  ************              
GETG5E   CLC   GKEY(GKEYSLN-GKEY),IOKEYSAV                                      
         BNE   GETG26                                                           
         CLC   GKEYEST,BEST        CHECK ESTIMATE ISN'T TOO HIGH                
         BH    GETG27                                                           
         L     R1,ATWA                                                          
         CLC   GKEYSEC,TWASLN-TWAD(R1)   COMPARE SECOND'S LENGTH                
         BE    *+18                                                             
         CLC   GKEYSLN,TWASLN-TWAD(R1)                                          
         BNL   GETG26                                                           
         B     GETG6                                                            
GETG5X   CLI   CMPPRD1,0                 EQUAL-TEST PIGGYBACK                   
         BNE   GETG8                                                            
         TM    GKEYAGY,X'80'             NO-SKIP IF PASSIVE POINTER             
         BZ    GETG8                                                            
*                                                                               
GETG6    LA    R1,DIRSQ            READ SEQUENTIAL                              
         B     GETG4                                                            
*                                                                               
GETG7    TM    INOIND,INOIALG      WE DOING ALL GOALS?                          
         BZ    GETG7A               - NOPE, NOT AT ALL, CONTINUE NORMAL         
         CLC   GKEY(8),IOKEYSAV    ONLY NEED TO COMPARE UP TO ESTIMATE          
         BNE   GETG28               - DIFFERENT ESTIMATE, KIDDO                 
         B     GETG7B                                                           
*                                                                               
GETG7A   CLC   GKEY(10),IOKEYSAV   COMPARE UP TO DAYPART AND LENGTH             
         BNE   GETG25                                                           
*                                                                               
GETG7B   OC    ACWORK(L'CAMIDRNM),ACWORK  ANY IDR BUYER NAME?                   
         BZ    GETG7C                                                           
         TM    GKEYAGY,X'40'       YES, THEN THIS BIT SHOULD BE ON              
         BZ    GETG6               IT ISN'T ON, READ NEXT GOAL RECORD           
         B     GETG7X                                                           
*                                                                               
GETG7C   TM    GKEYAGY,X'40'       NO IDR BUYER, THIS BIT SHOULDN'T BE          
         BNZ   GETG25              TRY FOR THE NEXT DAYPART                     
*                                                                               
GETG7X   CLI   CMPPRD1,0           TEST FOR PIGGYBACK PRODUCTS                  
         BE    GETG8                                                            
         CLC   GKEY(13),IOKEYSAV                                                
         BNE   GETG25                                                           
*                                                                               
GETG8    LA    R0,RIO                                                           
         ST    R0,IOADDR                                                        
         GOTO1 AIO,FILGET                                                       
         BNE   GETG25                                                           
         LA    R2,RIO                                                           
         LA    R4,GDELEM                                                        
         USING GDELEM,R4                                                        
         CLI   GOCODE,X'20'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    ACWORK(L'CAMIDRNM),ACWORK    ANY IDR BUYER?                      
         BZ    GETG8A                                                           
         CLC   GDIDR,ACWORK        SAME IDR BUYER AS GOAL RECORD?               
         BE    GETG8X                                                           
         LA    R2,IOKEY            NO, READ NEXT GOAL RECORD                    
         B     GETG6                                                            
*                                                                               
GETG8A   OC    GDIDR,GDIDR         ANY IDR BUYER?                               
         BNZ   GETG25              CAN'T BE                                     
*                                                                               
GETG8X   LA    R2,RIO                                                           
         LA    R4,GDELEM           FIND GOAL WEEK ELEMENTS                      
         USING GLEMENT,R4                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
GETG9    CLI   0(R4),0                                                          
         BE    GETG25                                                           
         CLI   0(R4),X'21'                                                      
         BE    *+16                                                             
*                                                                               
GETG10   ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GETG9                                                            
         CLC   GLWEEK,CMPSTMNP     TEST WEEK WITHIN PERIOD                      
         BL    GETG10                                                           
         TM    CMPOPTS,CAMOWKS     TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BO    GETG10A                                                          
         CLC   GLWEEK,RHALF        NO-TEST WEEK AGAINST CAMPAIGN END            
         BH    GETG10                                                           
         B     GETG12                                                           
GETG10A  L     RE,ATWA             YES-TEST WEEK FALLS WITHIN ONE OF            
         AHI   RE,CMPDATSP+2-TWAD                                               
         CLC   GLWEEK,0(RE)                                                     
         BNH   GETG12                  THE FLIGHT WEEKS                         
         L     R1,ATWA                                                          
         AHI   R1,CMPDATSP+4-TWAD                                               
*                                                                               
GETG11   CLI   0(R1),X'FF'                                                      
         BE    GETG10                                                           
         CLC   GLWEEK,0(R1)                                                     
         BL    GETG10                                                           
         CLC   GLWEEK,2(R1)                                                     
         BNH   GETG12                                                           
         LA    R1,4(R1)                                                         
         B     GETG11                                                           
*                                                                               
GETG12   MVC   RDUB(4),GLBUDGET    RDUB   = GOAL DOLLARS                        
         MVC   RDUB+4(4),GLGRP     RDUB+4 = GOAL POINTS                         
         OC    GLBUDGET,GLBUDGET                                                
         BZ    GETG13                                                           
         OC    GLGRP,GLGRP                                                      
         BNZ   GETG20                                                           
*                                                                               
GETG13   CLI   CLTPROF+8,C'0'      TEST CPP GUIDES AREA USED FOR CLT            
         BH    GETG20              NO                                           
         CLI   GDCPPES,0           TEST NEW CPP GUIDE USED                      
         BE    GETG14              NO                                           
         CLI   RFLAG,0             YES - TEST CPP GUIDE RECALLED YET            
         BE    GETG16                    NO - GO READ IT                        
         B     GETG18                                                           
*                                                                               
GETG14   CLC   RFLAG,GKEYEST       TEST HAVE CPP GUIDE FOR THIS EST YET         
         BE    GETG18              YES                                          
*                                                                               
GETG16   MVC   RWORK3(L'IOKEY),IOKEY                                            
         BAS   RE,GETGUIDE         GET CPP GUIDES                               
         MVC   IOKEY,RWORK3        RESTORE KEY                                  
         LA    R0,RIO                                                           
         ST    R0,IOADDR                                                        
         GOTO1 AIO,FILRD           GET GOAL RECORD                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETG18   BAS   RE,CPPCONV          COMPUTE DOLLARS/POINTS                       
         BNE   GETGX                                                            
*                                                                               
GETG20   L     RE,RDUB             ACCUMULATE GOAL DOLLARS                      
         A     RE,0(R3)                                                         
         ST    RE,0(R3)                                                         
*                                                                               
         L     RE,4(R3)                                                         
         A     RE,RDUB+4                                                        
         ST    RE,4(R3)                                                         
*                                                                               
         LA    R1,8(R3)            ACCUMULATE GOAL DOLLARS BY WEEK              
         LA    RE,220(R3)           ACCUMULATE GOAL POINTS BY WEEK              
         L     RF,ATWA                                                          
         AHI   RF,CMPDATSP-TWAD                                                 
         TM    CMPOPTS,CAMOWKS     TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BZ    GETG22                                                           
         CLC   GLWEEK,0(RF)        YES-TEST WEEK BEFORE FLIGHT START            
         BNL   GETG22                                                           
         CLC   GLWEEK,CMPSTMNP     YES-OK IF NOT BEFORE FLIGHT START            
         BNL   GETG24                  MONDAY                                   
         DC    H'0'                                                             
*                                                                               
GETG22   CLC   GLWEEK,0(RF)        TEST GOAL WEEK LOWER THAT PERIOD ST          
         BNL   *+16                                                             
         TM    CMPOPTS,CAMODLY     YES-TEST DAILY SKED                          
         BO    GETG24              YES-OK                                       
         B     *+14                                                             
         CLC   GLWEEK,2(RF)                                                     
         BNH   GETG24                                                           
         LA    R1,4(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   GETG22                                                           
         DC    H'0'                                                             
*                                                                               
GETG24   L     RF,0(R1)                                                         
         A     RF,RDUB                                                          
         ST    RF,0(R1)                                                         
         L     RF,0(RE)                                                         
         A     RF,RDUB+4                                                        
         ST    RF,0(RE)                                                         
         B     GETG10              NEXT WEEK                                    
*                                                                               
GETG25   CLI   CMPPGRPN,0          FINISHED WITH RECORD-TEST PRD=POL            
         BNE   GETG26                                   AND NOT PRDGRP          
         CLI   BPRD,X'FF'                                                       
         BNE   GETG26                                                           
         CLI   CMPPRD1,0           AND NOT PIGGYBACK                            
         BNE   GETG26                                                           
         LA    R1,DIRSQ            YES-READ SEQUENTIAL FOR CURRENT              
         LA    R2,IOKEY                PRODUCT/DAYPART                          
         B     GETG4                                                            
*                                                                               
****  NEW "ALLGOALS" OPTION                                                     
GETG26   TM    INOIND,INOIALG      WE DOING ALL GOALS?                          
         BNO   GETG26A              - NOPE, CONTINUE, SON                       
         LA    R2,IOKEY            RESET R2                                     
         B     GETG6               WE'RE DOING A SEQUENTIAL NOW                 
****                             07/08/03  MHC                                  
GETG26A  LA    R5,1(R5)            NEXT DAYPART                                 
         B     GETG2                                                            
*                                                                               
GETG27   CLI   CMPPGRPN,0          TEST PRODUCT GROUPS                          
         BE    GETG27A                                                          
         BAS   RE,NXTPRD           YES-GET NEXT PRODUCT                         
         BNE   GETG28              DONE                                         
         LA    R2,IOKEY                                                         
         MVC   GKEYPRD,RBYTE       PUT PRODUCT INTO GOAL KEY                    
****  NEW "ALLGOALS" OPTION                                                     
         TM    INOIND,INOIALG      WE DOING ALL GOALS?                          
         BO    GETG3A               - WE SURE ARE                               
****                             07/08/03  MHC                                  
         LA    R5,RWORK2           AND START WITH FIRST DAYPART                 
         B     GETG2               AND READ                                     
*                                                                               
GETG27A  CLI   BPRD,X'FF'          TEST PRODUCT POOL                            
         BNE   GETG28              NO - DONE                                    
         CLI   CMPPRD1,0           TEST PIGGYBACK PRODUCTS                      
         BNE   GETG28              YES - DONE                                   
         LA    R2,IOKEY            OTHERWISE, NEXT PRODUCT                      
         ZIC   RE,GKEYPRD                                                       
         LA    RE,1(RE)                                                         
         STC   RE,GKEYPRD                                                       
****  NEW "ALLGOALS" OPTION                                                     
         TM    INOIND,INOIALG      WE DOING ALL GOALS?                          
         BO    GETG3A               - WE SURE ARE                               
****                             07/08/03  MHC                                  
         LA    R5,RWORK2                                                        
         B     GETG2                     READ NEXT GOAL RECORD                  
*                                                                               
GETG28   SR    R1,R1                                                            
         ICM   R1,7,RFULL+1        TEST CAMPAIGN ESTIMATE LIST                  
         BZ    GETG30                                                           
         C     R1,RFULL2           YES-                                         
         BNL   GETG29                                                           
         CLI   0(R1),0                                                          
         BE    GETG29                                                           
         CLC   0(2,R1),=AL2(CESTALLQ)                                           
         BE    GETG28A             NEXT ESTIMATE FOR 'ALL'                      
         MVC   BEST,0(R1)          SET NEXT ESTIMATE IN LIST                    
         LA    R1,1(R1)                                                         
         STCM  R1,7,RFULL+1                                                     
*>>>     B     GETG1               GO BACK AND DO AGAIN FOR THIS EST            
         B     GETG1A1             DO NOT CHKIDRBYR AGAIN ! CLEARS IO3          
*                                                                               
GETG28AA MVI   BEST,0              *** HANDLE 'ALL' ESTIMATES ***               
GETG28A  CLI   BEST,255                                                         
         BNL   GETG29              DONE 1-255                                   
         SR    R1,R1                                                            
         IC    R1,BEST                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BEST             TRY NEXT POSSIBLE ESTIMATE NUMBER            
*                                  VALIDATE FOR CAMPAIGN ON THE FLY             
         LA    R2,IOKEY            BUILD KEY OF ESTIMATE RECORD                 
         USING ESTHDRD,R2                                                       
         XC    EKEY,EKEY                                                        
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         LA    R0,RIO                                                           
         ST    R0,IOADDR           POINT TO MY I/O AREA                         
         GOTO1 AIO,FILRD                                                        
         BNE   GETG28A                                                          
*                                                                               
         LA    R2,RIO                                                           
         TM    CLTIND2,CLTIEST     TEST CAMP AND EST DATES MUST MATCH           
         BZ    GETG1A1                                                          
         GOTO1 VDATCON,RPARM,(3,CMPSTDT),RWORK YES-                             
         GOTO1 (RF),(R1),(3,CMPND),RWORK+6                                      
         SR    R1,R1                                                            
         CLC   ESTART,RWORK                                                     
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         CLC   EEND,RWORK+6                                                     
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1               TEST START OR END MATCH                      
         BZ    GETG28A             NO-ERROR                                     
         OC    CMPCCAM,CMPCCAM     YES-TEST COMPANION CAMPAIGN                  
         BNZ   GETEST2             YES-OK                                       
         CHI   R1,2                NO-BOTH MUST MATCH                           
         BNE   GETG28A             NOT VALID/FOR CAM PERIOD ETC                 
         B     GETG1A1             DO GOALS FOR THIS ESTIMATE                   
*                                                                               
GETG29   MVC   BEST,RFULL          RESTORE CAMPAIGN ESTIMATE                    
*                                                                               
GETG30   NI    NETIND,255-NETIDATA                                              
*                                                                               
         OC    ESTPW,ESTPW         ANY PW PERCENTAGE?                           
         BZ    GETG30Z             NONE                                         
         TM    ESTIND,ESTICS2                                                   
         BNZ   GETG30Z                                                          
         BAS   RE,PWGOALS                                                       
*                                                                               
GETG30Z  CLI   CUDMED,C'C'         TEST CANADIAN AGENCY                         
         BNE   GETGX                                                            
         L     R5,ATWA                                                          
         LA    R4,RWORK2           DPT + ANY SUBDPTS                            
GETG31   GOTO1 AGETNET,RPARM,(R4),TWASLN  YES-GET NETWORK BUYS                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFOK) NOT FOUND, RESET OK FOR EQUAL EXIT           
         B     GETG33                                                           
         L     RE,AIOAREA3         FOUND-SUBTRACT FROM GOALS                    
         L     RF,AIOAREA4                                                      
         LA    R0,108              108 SEPARATE NUMBERS (1+1+53+53)             
*                                                                               
GETG32   L     R1,0(RE)                                                         
         S     R1,0(RF)                                                         
         ST    R1,0(RE)                                                         
         LA    RE,4(RE)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,GETG32                                                        
*                                                                               
GETG33   LA    R4,1(R4)                                                         
         CLI   0(R4),0             TEST ANY MORE DAYPARTS                       
         BNE   GETG31                                                           
*                                                                               
GETGX    B     XIT1                                                             
*                                                                               
XIT1     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* READ BWS GOAL RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
BWSGOAL  LR    R0,RE                                                            
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING BWGRECD,R2                                                       
         MVI   BWGKTYP,BWGKTYPQ                                                 
         MVI   BWGKSUB,BWGKSUBQ                                                 
         MVC   BWGKAGMD,BAGYMD                                                  
         OC    BWGKAGMD,BBYRMASK                                                
         MVC   BWGKBYR,BBYR                                                     
         MVC   BWGKCAM,BCAM                                                     
         MVC   BWGKMKT,BMKT                                                     
         MVC   BWGKDPT,BDPT                                                     
         MVC   BWGKLEN,BSLN                                                     
         GOTO1 AIO,DIRHI                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   BWGKEY,IOKEYSAV     TEST RECORD FOUND FOR DPT/LEN                
         BNE   BWSGOALX            NO-RETURN NO GOALS                           
         LA    R2,RIO                                                           
         ST    R2,IOADDR                                                        
         GOTO1 AIO,FILGET                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIOAREA3                                                      
         MVC   0(4,R1),BWGBUDT     THESE DISPLACEMENTS MATCHES THE DESC         
         MVC   4(4,R1),BWGGRPT        IN GETG ROUTINE                           
         MVC   8(56,R1),BWGBUD                                                  
         MVC   220(56,R1),BWGGRP                                                
*                                                                               
         OC    ESTPW,ESTPW         ANY PW PERCENTAGE?                           
         BZ    BWSGOALX            NONE                                         
         TM    ESTIND,ESTICS2                                                   
         BNZ   BWSGOALX                                                         
         BAS   RE,PWGOALS                                                       
BWSGOALX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ADJUSTS THE GOAL DOLLARS BY THE PW PERCENTAGE USING              
* PWCALC                                                                        
***********************************************************************         
PWGOALS  NTR1                                                                   
         XC    RWORK3,RWORK3       YES, ADJUST GOALS BY PW %-AGE                
         LA    R6,RWORK3                                                        
         USING PWBLKD,R6                                                        
         MVI   PWACT,PWGETGOL                                                   
         ZICM  R0,ESTPW,3                                                       
         ST    R0,PWPCT                                                         
*                                                                               
         L     R3,AIOAREA3         ADJUST TOTAL GOAL DOLLARS                    
         MVC   PWACTGOL,0(R3)                                                   
         GOTO1 VPWCALC,RPARM,(R6)                                               
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R3),PWVAL                                                    
*                                                                               
         LA    R0,53               MAX OF 53 WEEKS                              
         L     R3,AIOAREA3                                                      
         LA    R3,8(R3)                                                         
PWGOAL10 MVC   PWACTGOL,0(R3)                                                   
         GOTO1 VPWCALC,RPARM,(R6)                                               
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R3),PWVAL                                                    
*                                                                               
         LA    R3,4(R3)                                                         
         BCT   R0,PWGOAL10                                                      
*                                                                               
PWGOALX  B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* READ CLIENT HEADER TO GET LIST OF PRODUCTS                          *         
* CLIENT HEADER IS READ INTO RIO2                                     *         
***********************************************************************         
         SPACE 1                                                                
GETPLIST LR    R0,RE                                                            
         MVC   RWORK3(L'IOKEY),IOKEY                                            
         XC    IOKEY,IOKEY                                                      
         LA    R6,IOKEY                                                         
         USING CLTHDRD,R6                                                       
         MVI   CKEYTYPE,0                                                       
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         DROP  R6                                                               
         LA    R1,RIO2                                                          
         ST    R1,IOADDR                                                        
         GOTO1 AIO,FILRD                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,RWORK3                                                     
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET FIRST/NEXT PRODUCT FOR PRODUCT GROUP PROCESSING                 *         
* RETURNS: CC EQ - PRODUCT FOUND AND RETURNED IN RBYTE                *         
*          CC NE - ALL PRODUCTS HAVE BEEN FOUND                       *         
***********************************************************************         
         SPACE 1                                                                
FSTPRD   LR    R0,RE                                                            
         MVC   RWORK3(L'IOKEY),IOKEY                                            
         XC    IOKEY,IOKEY                                                      
         LA    R6,IOKEY            READ PRODUCT GROUP PASSIVES                  
         USING PRGRECD,R6                                                       
         MVC   PRGPTYP,=X'0D81'                                                 
         MVC   PRGPAGMD,BAGYMD                                                  
         MVC   PRGPCLT,BCLT                                                     
         MVC   PRGPID,CMPPGRP                                                   
         MVC   PRGPGRP,CMPPGRP+1                                                
         LA    R1,DIRHI                                                         
         B     NXTPRD2                                                          
*                                                                               
NXTPRD   LR    R0,RE               GET NEXT PRODUCT                             
         MVC   RWORK3(L'IOKEY),IOKEY                                            
         MVC   IOKEY,RWORK4                                                     
         GOTO1 AIO,DIRHI           RE-ESTABLISH READ SEQUENCE                   
         LA    R1,DIRSQ                                                         
*                                                                               
NXTPRD2  MVC   RBYTE,CMPPGRPN                                                   
         NI    RBYTE,X'7F'                                                      
         LA    R8,PRGPGRP-PRGKEY-1 SET R8 FOR EXECUTED COMPARE                  
         CLI   CMPPGRPN,X'81'                                                   
         BE    NXTPRD4                                                          
         LA    R8,1(R8)                                                         
         CLI   RBYTE,3                                                          
         BNE   NXTPRD4                                                          
         LA    R8,1(R8)                                                         
*                                                                               
NXTPRD4  DS    0H                                                               
         GOTO1 AIO                                                              
         BNE   NXTPRD7                                                          
         EX    R8,*+8                                                           
         B     *+10                                                             
         CLC   PRGKEY(0),IOKEYSAV                                               
         BNE   NXTPRD7                                                          
         CLI   CMPPGRPN,X'81'                                                   
         BNE   NXTPRD6                                                          
         MVC   RBYTE,PRGPGRP                                                    
         NI    RBYTE,X'F0'                                                      
         CLC   RBYTE,CMPPGRP+1                                                  
         BE    NXTPRD6                                                          
         BH    NXTPRD7                                                          
         LA    R1,DIRSQ                                                         
         B     NXTPRD4                                                          
*                                                                               
NXTPRD6  LA    R1,RIO2             PRODUCT'S BEEN FOUND                         
         LA    R1,CLIST-CLTHDR(R1)                                              
         CLC   0(3,R1),PRGPPRD     GET PRODUCT CODE FROM CLIENT HEADER          
         BE    NXTPRD8                                                          
         LA    R1,4(R1)                                                         
         CLI   0(R1),C'A'                                                       
         BNL   *-18                                                             
         DC    H'0'                                                             
*                                                                               
NXTPRD7  MVI   RBYTE,0             ALL PRODUCTS FOUND                           
         B     NXTPRDX                                                          
*                                                                               
NXTPRD8  MVC   RBYTE,3(R1)         RETURN PRODUCT CODE IN RBYTE                 
*                                                                               
NXTPRDX  MVC   RWORK4(L'IOKEY),IOKEY   SAVE CURRENT PRODUCT GROUP               
         MVC   IOKEY,RWORK3            PASSIVE KEY IN RWORK4                    
         LR    RE,R0                                                            
         CLI   RBYTE,0                                                          
         BE    *+8                                                              
         CR    RE,RE                                                            
         BR    RE                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET POL-30 CPP GUIDES                                               *         
* OUTPUT : RFLAG = EST NUMBER READ                                    *         
*          RWORK = WEEKLY CPP - 14X4 BYTES                            *         
***********************************************************************         
         SPACE 1                                                                
GETGUIDE NTR1                                                                   
         XC    RWORK,RWORK                                                      
         LA    R2,RIO                                                           
         USING GOALRECD,R2                                                      
         LA    R3,IOKEY                                                         
         MVC   RFLAG,GKEYEST       SAVE GOAL EST NUMBER                         
         MVI   GKEYPRD-GKEY(R3),X'FF'   GOAL PRODUCT POL                        
         XC    GKEYSLN-GKEY(4,R3),GKEYSLN-GKEY(R3)                              
         MVI   RBYTE,0                                                          
         CLI   GDCPPES,0                                                        
         BE    GUID2                                                            
         MVC   GKEYCLT-GKEY(L'GKEYCLT,R3),GDCPPCL                               
         MVC   GKEYEST-GKEY(L'GKEYEST,R3),GDCPPES                               
         MVC   RBYTE,GDCPPES2      SAVE 2ND CPP EST                             
         DROP  R2                                                               
         USING GOALRECD,R3                                                      
*                                                                               
GUID2    GOTO1 AIO,DIRHI                                                        
         CLC   GKEY(GKEYDPT-GKEY+L'GKEYDPT),IOKEYSAV                            
         BE    GUID6                                                            
*                                                                               
GUID4    CLI   RBYTE,0             TEST NEED 2ND CPP GUIDE                      
         BE    GUIDX               NO - DONE                                    
         LA    R3,IOKEY                                                         
         MVC   GKEY,IOKEYSAV       RESTORE KEY                                  
         MVC   GKEYEST,RBYTE       2ND EST                                      
         MVI   RBYTE,0                                                          
         B     GUID2                                                            
*                                                                               
GUID6    LA    R0,RIO              GET CPP GUIDE RECORD                         
         ST    R0,IOADDR                                                        
         GOTO1 AIO,FILGET                                                       
         LA    R3,RIO                                                           
         LA    R2,RWORK                                                         
         L     R5,ATWA             WEEK LIST                                    
         AHI   R5,CMPDATSP-TWAD                                                 
         ST    R5,ACFULL                                                        
         SR    R0,R0                                                            
         LA    R4,GDELEM                                                        
*                                                                               
GUID8    CLI   0(R4),0                                                          
         BE    GUID4                                                            
*                                                                               
GUID10   CLI   0(R4),X'21'                                                      
         BNE   GUID12                                                           
         TM    CMPOPTS,CAMOWKS     TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BZ    GUID11                                                           
         C     R5,ACFULL           YES-TEST ON FIRST FLIGHT WEEK                
         BNE   GUID11                                                           
         CLC   GLWEEK,CMPSTMNP     YES-TEST WEEK PRIOR TO START MONDAY          
         BL    GUID12                  YES-NEXT WEEK ELEMENT                    
         B     GUID11A                 NO-GO ON TO TEST WITHIN 1ST WEEK         
*                                                                               
GUID11   CLC   GLWEEK,0(R5)        TEST CPP WEEK IS THIS WEEK                   
         BL    GUID12                                                           
*                                                                               
GUID11A  CLC   GLWEEK,2(R5)                                                     
         BH    GUID14                                                           
         MVC   0(4,R2),GLBUDGET    MOVE CPP DOLLARS                             
         LA    R2,4(R2)            NEXT SAVE BUCKET                             
         LA    R5,4(R5)            NEXT MONTH                                   
         CLI   0(R5),X'FF'                                                      
         BE    GUID4                                                            
*                                                                               
GUID12   IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     GUID8                                                            
*                                                                               
GUID14   LA    R2,4(R2)                                                         
         LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   GUID10                                                           
         B     GUID4                                                            
*                                                                               
GUIDX    B     XIT1                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* CONVERT DOLLARS VIA CPP GUIDE                                       *         
***********************************************************************         
         SPACE 1                                                                
CPPCONV  NTR1                                                                   
         USING GOALRECD,R2                                                      
         USING GLEMENT,R4                                                       
         XC    RPARM(8),RPARM                                                   
         LA    R3,CLTEQUIV                                                      
         LA    R5,LENTABLE                                                      
         MVC   RPARM+2(2),12(R3)   30 SEC EQUIV FACTOR                          
         MVC   RPARM+6(2),12(R3)   IN CASE LENGTH NOT IN TABLE                  
*                                                                               
CPP2     CLI   0(R5),0                                                          
         BE    CPP4                                                             
         CLC   GKEYSEC,0(R5)       MATCH LENGTH                                 
         BNE   *+14                                                             
         MVC   RPARM+6(2),0(R3)    EQUIV FACTOR                                 
         B     CPP4                                                             
         LA    R3,4(R3)            NEXT EQUIV FACTOR                            
         LA    R5,1(R5)            NEXT LENGTH                                  
         B     CPP2                                                             
*                                                                               
CPP4     LA    R3,RWORK                                                         
         L     R5,ATWA                                                          
         AHI   R5,CMPDATSP-TWAD                                                 
         TM    CMPOPTS,CAMOWKS     TEST NON-CONTIGUOUS FLIGHT WEEKS             
         BZ    CPP6                                                             
         CLC   GLWEEK,0(R5)        YES-TEST WEEK BEFORE FLIGHT START            
         BNL   CPP6                                                             
         CLC   GLWEEK,CMPSTMNP     YES-OK IF NOT BEFORE FLIGHT START            
         BNL   CPP7                    MONDAY                                   
         B     CPP99                                                            
*                                                                               
CPP6     CLC   GLWEEK,0(R5)        MATCH WEEK                                   
         BL    *+14                                                             
         CLC   GLWEEK,2(R5)                                                     
         BNH   CPP7                                                             
         LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   CPP6                                                             
         B     CPP99               NO CPP GUIDE                                 
*                                                                               
CPP7     OC    0(4,R3),0(R3)       TEST ZERO CPP                                
         BZ    CPP10                                                            
         SR    R0,R0                                                            
         OC    RDUB(4),RDUB                                                     
         BNZ   CPP8                                                             
         L     R1,RDUB+4           POINTS                                       
         M     R0,RPARM+4          EF                                           
         M     R0,0(R3)            POINTS X CPP                                 
         LH    RE,RPARM+2          EF30                                         
         MHI   RE,1000             SCALE                                        
         DR    R0,RE                                                            
         SRA   RE,1                                                             
         CR    R0,RE                                                            
         BL    *+8                                                              
         LA    R1,1(R1)            ROUND                                        
         ST    R1,RDUB                                                          
         B     CPP10                                                            
*                                                                               
CPP8     L     R1,RDUB             DOLLARS                                      
         M     R0,=F'2000'         SCALE X1000 X2                               
         D     R0,0(R3)            CPP $                                        
         AHI   R1,1                ROUND                                        
         SRL   R1,1                / 2                                          
         AR    R1,R1               X 2                                          
         M     R0,RPARM            EF30                                         
         LH    RE,RPARM+6          EF                                           
         DR    R0,RE                                                            
         AHI   R1,1                ROUND                                        
         SRL   R1,1                                                             
         ST    R1,RDUB+4           GOAL POINTS                                  
*                                                                               
CPP10    B     CPPX                                                             
*                                                                               
CPP99    MVC   FVMSGNO,=AL2(FVNOCPP)   CPP GUIDE NOT FOUND                      
*                                                                               
CPPX     CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT1                                                             
         SPACE 2                                                                
LENTABLE DC    AL1(10,15,20,30,40,45,50,60,90,120,27,3,75,0)                    
         EJECT                                                                  
***********************************************************************         
* CHECKS TO SEE IF THE CAMPAIGN HAS A IDR BUYER                                 
*                                                                               
* ON ENTRY:    PARAM 1             A(IDR BUYER STORAGE)                         
***********************************************************************         
CKIDRBYR NTR1                                                                   
         L     R4,0(R1)                                                         
         XC    0(L'CAMIDRNM,R4),0(R4)                                           
*                                                                               
         XC    IOKEY,IOKEY         YES, EXTRACT IT FROM CAMPAIGN RECORD         
         LA    R2,IOKEY                                                         
         USING CAMRECD,R2                                                       
         MVI   CAMKTYP,CAMKTYPQ                                                 
         MVI   CAMKSUB,CAMKSUBQ                                                 
         MVC   CAMKAGMD,BAGYMD                                                  
         OC    CAMKAGMD,BBYRMASK                                                
         MVC   CAMKBYR,BBYR                                                     
         MVC   CAMKCAM,BCAM                                                     
         GOTO1 AIO,DIRHI                                                        
         CLC   CAMKEY(CAMKREST-CAMKEY),IOKEYSAV                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,CAMKEY+14    SHOULDN'T HAVE TO DO THIS, BUT..           
         GOTO1 AIO,FILGET3                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA3                                                      
         LA    R2,CAMFSTEL                                                      
         CLI   0(R2),CAMELCDQ      DESCRIPTION ELEMENT?                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CAMEL,R2                                                         
         CLI   CAMELLN,CAMELLNQ             ANY IDR BUYER?                      
         BNH   CIDR20                       NO                                  
         MVC   0(L'CAMIDRNM,R4),CAMIDRNM    YES, SAVE IDR BUYER                 
         DROP  R2                                                               
*                                                                               
CIDR20   L     R2,AIOAREA3         R3 = GOALS AREA                              
         LR    RE,R2                  CLEAR IT                                  
         LA    RF,432                                                           
         XCEFL                                                                  
*                                                                               
CIDRX    B     XIT1                                                             
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
GETGWRK  DSECT                                                                  
CLTREC   DS    CL(CLTHDRL)                                                      
*                                                                               
T20700   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ENTRY POINT FOR OPTION ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
OBASE    NMOD1 0,**OBASE*                                                       
         USING WORKD,R7                                                         
         SRL   RF,24               DETERMINE OPTION NUMBER (1 BASED)            
         SLL   RF,2                                                             
         CH    RF,=AL2(OPTMAX)                                                  
         BNH   *+2(RF)                                                          
         DC    H'0'                OPTION NUMBER OUSIDE RANGE                   
         SPACE 2                                                                
OBRANCH  B     VALSID                                                           
         B     VALPRG                                                           
         B     VALRTG                                                           
         B     VALDEM                                                           
         B     VALPRO                                                           
         B     VALPER                                                           
         B     VALID                                                            
         B     VALSLN                                                           
         B     VALDT                                                            
         B     VALLIN                                                           
         B     VALSTAT                                                          
         B     VALSDT                                                           
OPTMAX   EQU   *-OBRANCH                                                        
         SPACE 2                                                                
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE SID OPTION                                                 *         
* FORMAT:  SID=YES(/YY) FOR NSID, WHERE YY=OPTIONAL OVERRIDE YEAR     *         
***********************************************************************         
         SPACE 1                                                                
VALSID   GOTO1 VSCANNER,ACPARM,FVIHDR,(1,AIOAREA2),C',=,/'                      
         CLI   4(R1),1                                                          
         BNE   VALSID9                                                          
         L     R1,AIOAREA2                                                      
         CLI   1(R1),0                                                          
         BE    VALSID2                                                          
         CLI   1(R1),2                                                          
         BH    VALSID9                                                          
         TM    3(R1),X'80'                                                      
         BZ    VALSID9                                                          
         MVC   SCWORK+1(1),11(R1)  NSID YEAR                                    
*                                                                               
VALSID2  CLI   0(R1),3                                                          
         BH    VALSID9                                                          
         ZIC   RF,0(R1)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   12(0,R1),=C'YES'                                                 
         BNE   VALSID9                                                          
         MVI   SCWORK,C'Y'                                                      
         B     VALSIDX                                                          
*                                                                               
VALSID9  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALSIDX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SID PROGRAM TYPE OPTION                                    *         
* FORMAT:  PRG=X WHERE X=PROGRAM TYPE                                 *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVC   SCWORK(L'INOPRG),FVIFLD                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE RATING OPTION                                              *         
* FORMAT:  RTG=DDDDDDD  WHERE DDDDDDD=DEMO CATEGORY                   *         
***********************************************************************         
         SPACE 1                                                                
VALRTG   L     RF,AIOAREA2                                                      
         USING DBLOCKD,RF                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,ACOM                                                    
         GOTO1 VDEMOVAL,ACPARM,(1,FVIHDR),(1,SCWORK),(C'S',DBLOCKD),0           
         CLI   0(R1),0                                                          
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEMOS OPTION                                               *         
* FORMAT:  DEM=1ST/2ND/3RD/4TH OR LIST OF DEMOS                       *         
***********************************************************************         
         SPACE 1                                                                
VALDEM   CLI   FVILEN,3            TEST FOR NEXT                                
         BH    VALDEM2                                                          
         ZIC   RF,FVXLEN                                                        
         LA    RE,VALDEMT          TEST FOR SPECIAL INPUTS FIRST                
*                                                                               
VALDEM1  CLI   0(RE),EOT                                                        
         BE    VALDEM2                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(RE)     MATCH FIELD TO TABLE                         
         BE    *+12                                                             
         LA    RE,L'VALDEMT(RE)                                                 
         B     VALDEM1                                                          
         CLI   INACT,ACTDRP                                                     
         BE    VALDEM2             WORK/RECAP HAS NO SPECIAL DEMO REQT          
         MVC   SCWORK(1),3(RE)     SET SPECIAL OUTPUT VALUE                     
         B     VALDEMX                                                          
*                                                                               
VALDEM2  L     RF,AIOAREA2                                                      
         USING DBLOCKD,RF                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         MVC   DBSELMED,CUDMED                                                  
         MVI   DBSELSRC,C'N'                                                    
         MVC   DBCOMFCS,ACOM                                                    
         GOTO1 VDEMOVAL,ACPARM,(1,FVIHDR),(4,SCWORK+1),(C'S',DBLOCKD), *        
               (C'/',0)                                                         
         MVC   SCWORK(1),4(R1)                                                  
         CLI   0(R1),0                                                          
         BE    VALDEMX                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALDEMX  B     XIT                                                              
         DROP  RF                                                               
         SPACE 1                                                                
VALDEMT  DS    0XL4                ** SPECIAL DEMO TABLE **                     
         DC    C'1ST',X'81'                                                     
         DC    C'2ND',X'82'                                                     
         DC    C'3RD',X'83'                                                     
         DC    C'4TH',X'84'                                                     
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT OPTION                                             *         
* FORMAT:  PRD=PRD1(-PRD2/SLN1-SLN2)                                  *         
***********************************************************************         
         SPACE 1                                                                
VALPRO   XC    INOPRD,INOPRD                                                    
         XC    POLPRD1(L'POLPRD1*2),POLPRD1                                     
         CLI   INREC,RECWRK        WORK/TRANSFER?                               
         BE    *+12                                                             
         CLI   INREC,RECBUY        BUY/TRANSFER?                                
         BNE   VALPRO0                                                          
         CLI   INACT,ACTXFR                                                     
         BNE   VALPRO0                                                          
         TM    BWSKEYH+4,X'20'     YES, WAS KEY PREVIOUSLY VALIDATED?           
         BZ    VALPROX             NO, KEY WAS CHANGED, SKIP FOR NOW            
*                                                                               
VALPRO0  CLI   BPRD,X'FF'                                                       
         BNE   VALPRO9                                                          
         GOTO1 VSCANNER,ACPARM,FVIHDR,(2,AIOAREA3),C',=/-'                      
         MVC   ACFULL(1),4(R1)                                                  
         CLI   ACFULL,0                                                         
         BE    VALPRO9                                                          
         L     R3,AIOAREA3                                                      
         CLI   0(R3),3                                                          
         BH    VALPRO9                                                          
         CLC   12(3,R3),=C'POL'    POL IS INVALID                               
         BE    VALPRO8                                                          
         LA    R4,IOKEY                                                         
         USING PRDHDRD,R4                                                       
         XC    PKEY,PKEY                                                        
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,12(R3)                                                   
         GOTO1 AIO,IOSPTFIL+IORD+IO2                                            
         BNE   VALPRO8                                                          
         L     R4,AIOAREA2                                                      
         MVC   SCWORK(1),PCODE+1                                                
         MVC   POLPRD1,12(R3)      SAVE PRODUCT 1                               
         CLI   1(R3),0                                                          
         BNE   *+16                                                             
         CLI   ACFULL,1                                                         
         BNE   VALPRO9                                                          
         B     VALPROX                                                          
         CLI   1(R3),2                                                          
         BL    VALPRO9                                                          
         CLI   1(R3),3                                                          
         BH    VALPRO9                                                          
         LA    R4,IOKEY                                                         
         MVC   PKEYPRD,22(R3)                                                   
         GOTO1 AIO,IOSPTFIL+IORD+IO2                                            
         BNE   VALPRO8                                                          
         L     R4,AIOAREA2                                                      
         MVC   SCWORK+1(1),PCODE+1                                              
         MVC   POLPRD2,22(R3)      SAVE PRODUCT 2                               
         DROP  R4                                                               
         CLI   ACFULL,2                                                         
         BE    VALPRO1                                                          
         CLI   INREC,RECBUY        BUY REVISION?                                
         BNE   VALPRO9             NO, ERROR                                    
         CLI   INACT,ACTXFR        YES, BUYS/TRANSFER?                          
         BE    VALPRO9                  YES, NEED SECONDS BREAKDOWN             
         B     VALPROX                                                          
*                                                                               
VALPRO1  TM    34(R3),X'80'                                                     
         BZ    VALPRO9                                                          
         L     R4,36(R3)                                                        
         CHI   R4,255                                                           
         BH    VALPRO9                                                          
         STC   R4,ACDUB                                                         
         BAS   RE,VALPROSL                                                      
         MVC   SCWORK+2(1),ACDUB                                                
         TM    35(R3),X'80'                                                     
         BZ    VALPRO9                                                          
         L     RF,40(R3)                                                        
         CHI   RF,255                                                           
         BH    VALPRO9                                                          
         STC   RF,ACDUB                                                         
         BAS   RE,VALPROSL                                                      
         MVC   SCWORK+3(1),ACDUB                                                
         AR    R4,RF                                                            
         CLI   CMPSLN,0            TEST SLN DEFINED FOR CAMPAIGN                
         BNE   *+16                YES                                          
         STC   R4,ACDUB            NO-TEST TOTAL LENGTH IS VALID                
         BAS   RE,VALPROSL                                                      
         B     VALPROX                                                          
         XC    ACFULL,ACFULL       TEST TOTAL SLN = CAMPAIGN SLN                
         MVC   ACFULL+3(1),CMPSLN                                               
         C     R4,ACFULL                                                        
         BNE   VALPRO9                                                          
         B     VALPROX                                                          
*                                                                               
VALPRO8  MVC   FVMSGNO,=AL2(FVIPRD)   INVALID PRODUCT                           
         B     VALPROX                                                          
*                                                                               
VALPRO9  MVC   FVMSGNO,=AL2(FVFNOTV)  INVALID FIELD                             
*                                                                               
VALPROX  B     XIT                                                              
         SPACE 1                                                                
VALPROSL LA    R1,VALPROT                                                       
VALPROS2 CLI   0(R1),0                                                          
         BE    VALPRO9                                                          
         CLC   ACDUB(1),0(R1)                                                   
         BER   RE                                                               
         LA    R1,1(R1)                                                         
         B     VALPROS2                                                         
VALPROT  DC    AL1(10,15,20,30,40,45,50,60,75,90,120),AL1(0)                    
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD OPTION                                              *         
* FORMAT:  PER=MMMDD/YY(-MMMDD/YY)                                    *         
***********************************************************************         
         SPACE 1                                                                
VALPER   L     R3,AIOAREA3                                                      
         XC    APWORK(12),APWORK                                                
         CLI   FVIFLD,C'-'                                                      
         BNE   VALPER2                                                          
         MVC   1(1,R3),FVXLEN                                                   
         MVC   22(8,R3),FVIFLD+1                                                
         B     VALPER4                                                          
*                                                                               
VALPER2  GOTO1 VSCANNER,ACPARM,FVIHDR,(3,AIOAREA3),C',=,-'                      
         CLI   4(R1),0                                                          
         BE    VALPER20                                                         
         CLI   4(R1),2                                                          
         BH    VALPER20                                                         
         CLI   0(R3),0             TEST FIRST DATE GIVEN                        
         BE    VALPER4                                                          
         GOTO1 VDATVAL,ACPARM,(0,12(R3)),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    VALPER20                                                         
         CLC   3(1,R1),0(R3)                                                    
         BNE   VALPER20                                                         
*                                                                               
VALPER4  CLI   1(R3),0             TEST SECOND DATE GIVEN                       
         BE    VALPER6                                                          
         GOTO1 VDATVAL,ACPARM,(0,22(R3)),APWORK+6                               
         OC    0(4,R1),0(R1)                                                    
         BZ    VALPER20                                                         
         CLC   3(1,R1),1(R3)                                                    
         BNE   VALPER20                                                         
*                                                                               
         CLI   APWORK,X'00'                                                     
         BE    VALPER8                                                          
*                                                                               
VALPER6  GOTO1 VDATCON,ACPARM,(0,APWORK),(2,SCWORK)                             
*                                                                               
         CLI   APWORK+6,X'00'                                                   
         BE    VALPER10                                                         
*                                                                               
VALPER8  GOTO1 VDATCON,ACPARM,(0,APWORK+6),(2,SCWORK+2)                         
         CLC   SCWORK(2),SCWORK+2  START CANNOT BE GREATER THEN END             
         BH    VALPER20                                                         
*                                                                               
         CLI   INOFRM,C'Y'        DAY OR DD REQUEST FORMAT?                     
         BNE   VALPER10           NO                                            
*                                 YES - PERIOD MUST NOT BE > 2 WKS              
         GOTO1 VADDAY,ACPARM,APWORK,APDUB,F'13'                                 
         GOTO1 VDATCON,ACPARM,(0,APDUB),(2,APHALF)                              
         CLC   APHALF,SCWORK+2                                                  
         BNL   VALPER10                                                         
         MVC   FVMSGNO,=AL2(FVPTL2)                                             
         B     VALPERX                                                          
*                                                                               
VALPER10 B     VALPERX                                                          
*                                                                               
VALPER20 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALPERX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE ID OPTION                                                  *         
* FORMAT:  ID=5 DIGIT ACN NUMBER OR 1-5 CHARACTER BUY ID              *         
***********************************************************************         
         SPACE 1                                                                
VALID    MVC   SCWORK(L'INOBUYID),FVIFLD                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE SLN OPTION                                                 *         
* FORMAT:  SLN=A/B/C/D, WHERE A,B,C,D=SPOT LENGTHS                    *         
***********************************************************************         
         SPACE 1                                                                
VALSLN   GOTO1 VSCANNER,ACPARM,FVIHDR,(4,AIOAREA2),C',=/='                      
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    VALSLN9                                                          
         CLI   4(R1),L'INOSLN                                                   
         BH    VALSLN9                                                          
         L     R2,AIOAREA2                                                      
         LA    R3,SCWORK                                                        
*                                                                               
VALSLN2  CLI   1(R2),0                                                          
         BNE   VALSLN9                                                          
         TM    2(R2),X'80'                                                      
         BZ    VALSLN9                                                          
         OC    4(3,R2),4(R2)                                                    
         BNZ   VALSLN8                                                          
         LA    RE,LENTAB                                                        
*                                                                               
VALSLN4  CLI   0(RE),0                                                          
         BE    VALSLN8                                                          
         CLC   0(1,RE),7(R2)                                                    
         BE    *+12                                                             
         LA    RE,1(RE)                                                         
         B     VALSLN4                                                          
         MVC   0(1,R3),0(RE)                                                    
         LA    R2,32(R2)                                                        
         LA    R3,1(R3)                                                         
         BCT   R0,VALSLN2                                                       
         B     VALSLNX                                                          
*                                                                               
VALSLN8  MVC   FVMSGNO,=AL2(FVISLN)                                             
         B     VALSLNX                                                          
*                                                                               
VALSLN9  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALSLNX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE DATES OPTION                                               *         
* FORMAT:  DATES=MM/DD/YY-MM/DD/YY                                    *         
***********************************************************************         
         SPACE 1                                                                
VALDT    GOTO1 VDATVAL,ACPARM,FVIFLD,ACDUB                                      
         SR    R2,R2                                                            
         ICM   R2,1,3(R1)                                                       
         BNZ   VALDT2                                                           
         GOTO1 (RF),(R1),(1,FVIFLD),ACDUB                                       
         ICM   R2,1,3(R1)                                                       
         BZ    VALDT8                                                           
*                                                                               
VALDT2   GOTO1 VDATCON,ACPARM,ACDUB,(3,SCWORK)                                  
         MVC   SCWORK+3(3),SCWORK                                               
         LA    R2,FVIFLD(R2)                                                    
         CLI   0(R2),C' '                                                       
         BNH   VALDTX                                                           
         CLI   0(R2),C'-'                                                       
         BNE   VALDT9                                                           
         LA    R2,1(R2)                                                         
         GOTO1 VDATVAL,ACPARM,(R2),ACDUB                                        
         CLI   3(R1),0                                                          
         BNE   VALDT4                                                           
         GOTO1 (RF),(R1),(1,(R2)),ACDUB                                         
         CLI   3(R1),0                                                          
         BE    VALDT8                                                           
*                                                                               
VALDT4   GOTO1 VDATCON,ACPARM,ACDUB,(3,SCWORK+3)                                
         B     VALDTX                                                           
*                                                                               
VALDT8   MVC   FVMSGNO,=AL2(FVIDAT)                                             
         B     VALDTX                                                           
*                                                                               
VALDT9   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALDTX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE START LINE NUMBER FOR DMB&B FTP TRANSMISSION               *         
* FORMAT:  LINE=LINE NUMBER, IN RANGE 10-9990                         *         
***********************************************************************         
         SPACE 1                                                                
VALLIN   TM    FVIIND,FVINUM       TEST NUMERIC                                 
         BZ    VALLIN9                                                          
         L     RF,SCFULL           YES-TEST DIVISIBLE BY 10                     
         SR    RE,RE                                                            
         D     RE,=F'10'                                                        
         LTR   RE,RE                                                            
         BNZ   VALLIN8                                                          
         MVC   SCWORK(2),SCFULL+2                                               
         B     VALLINX                                                          
*                                                                               
VALLIN8  MVC   FVMSGNO,=AL2(FVILINE)                                            
         B     VALLINX                                                          
*                                                                               
VALLIN9  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALLINX  B     XIT                                                              
         EJECT                                                                  
LENTAB   DS    0XL1                                                             
         DC    AL1(10,15,20,30,40,45,50,60,75,90,120),AL1(0)                    
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION FOR A CALL LETTER CHANGE                           *         
* FORMAT:  STA=NEW CALL LETTERS                                       *         
***********************************************************************         
         SPACE 1                                                                
VALSTAT  MVC   SCWORK(L'INFCHSTA),FVIFLD                                        
         CLI   SCWORK+4,C'T'                                                    
         BE    VALSTATX                                                         
         CLI   SCWORK+4,C'/'                                                    
         BE    VALSTATX                                                         
         CLI   SCWORK+4,C' '                                                    
         BNE   VALSTAT9                                                         
         MVI   SCWORK+4,C'T'                                                    
         B     VALSTATX                                                         
*                                                                               
VALSTAT9 MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALSTATX B     XIT                                                              
         SPACE 1                                                                
***********************************************************************         
* VALIDATE START DATE ROUTINE                                         *         
* FORMAT:  STDATE=MMMDD/YY  OR  SDT=MMMDD/YY                          *         
***********************************************************************         
VALSDT   DS    0H                                                               
         GOTO1 VDATVAL,APPARM,(0,FVIFLD),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   VALSDT5             GOOD M/D/Y                                   
*                                                                               
         GOTO1 VDATVAL,APPARM,(1,FVIFLD),APWORK                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    VALSDT9             INVALID M/D                                  
         MVI   SCWORK+2,X'80'        YEAR WAS OMITTED                           
*                                                                               
VALSDT5  GOTO1 VDATCON,APPARM,(0,APWORK),(2,SCWORK)                             
         B     VALSDTX                                                          
*                                                                               
VALSDT9  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALSDTX  B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OTHER TABLES                                                        *         
***********************************************************************         
         SPACE 1                                                                
CONADDRS DS    0F                  ** ADDRESS OF ROUTINES ETC. **               
         DC    A(PHASES)                                                        
         DC    A(HOOK)                                                          
         DC    A(OBASE)                                                         
         DC    A(ROUTS)                                                         
CONADDRN EQU   (*-CONADDRS)/L'CONADDRS                                          
         SPACE 1                                                                
PHASES   DS    0X                  ** LOADED CORERES PHASES **                  
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QSPDEMUP)                                                    
         DC    AL1(QGETDEM2)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QBOOKVAL)                                                    
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QUNDAY)                                                      
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QXSORT)                                                      
         DC    AL1(QEDITOR)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QSPACNVL)                                                    
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QRANSID)                                                     
         DC    AL1(0)              A(CORERES MSPACK) GOES HERE                  
         DC    AL1(0)              A(CORERES MSUNPK) GOES HERE                  
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QTSAR)                                                       
         DC    AL1(QSPOTBUY)                                                    
         DC    AL1(QSPGETBU)                                                    
         DC    AL1(QSPADINT)                                                    
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QPWCALC)                                                     
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QRCPACK)                                                     
         DC    AL1(QSPAUTH)                                                     
         DC    X'FF'                                                            
         EJECT                                                                  
* SPNWSWRK                                                                      
       ++INCLUDE SPNWSWRK                                                       
TWAD     DSECT                                                                  
         ORG   BWSTABH                                                          
       ++INCLUDE SPNWSFCD                                                       
         EJECT                                                                  
***********************************************************************         
* ROUTS S/R LOCAL W/S                                                 *         
***********************************************************************         
RWRKD    DSECT                                                                  
RIOSAVE  DS    XL(IOAREAX-IOAREA)                                               
RDUB     DS    D                                                                
RDUB2    DS    D                                                                
RFULL    DS    F                                                                
RFULL2   DS    F                                                                
RWORK    DS    XL64                                                             
RWORK2   DS    XL64                                                             
RWORK3   DS    XL64                                                             
RWORK4   DS    XL256                                                            
RPARM    DS    8F                                                               
RHALF    DS    H                                                                
RBYTE    DS    XL1                                                              
RFLAG    DS    XL1                                                              
RTSARLOC DS    CL1                                                              
         DS    XL3                 SPARE                                        
RSPACES  DS    CL24                                                             
RIO      DS    2000C                                                            
RIO2     DS    2000C                                                            
RWRKX    EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* KEEP THIS IN SYNC WITH SAME DSECT IN SPNWS00                                  
***********************************************************************         
TAMDATAD DSECT                                                                  
TAMMED   DS    CL1                                                              
TAMBYR   DS    CL3                                                              
TAMCAMP  DS    CL5                                                              
TAMPASS  DS    CL10                                                             
TAMVERS  DS    XL4                                                              
TAMDATAL EQU   *-TAMDATAD                                                       
         EJECT                                                                  
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
* DDTWABLDD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
* SPGENAGY                                                                      
         PRINT OFF                                                              
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
* SPGENCLT                                                                      
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
* SPGENPRD                                                                      
         PRINT OFF                                                              
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         PRINT ON                                                               
* SPGENPRG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENPRG                                                       
         PRINT ON                                                               
* SPGENEST                                                                      
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
* SPGENMKT                                                                      
         PRINT OFF                                                              
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
* SPGENSTA                                                                      
         PRINT OFF                                                              
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT ON                                                               
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
* SPGENDAYPT                                                                    
         PRINT OFF                                                              
DPTHDRD  DSECT                                                                  
       ++INCLUDE SPGENDAYPT                                                     
         PRINT ON                                                               
* SPNWSBYR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSBYR                                                       
         PRINT ON                                                               
* SPNWSCAM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSCAM                                                       
         PRINT ON                                                               
* SPNWSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSHDR                                                       
         PRINT ON                                                               
* SPNWSGOAL                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPNWSGOAL                                                      
         PRINT ON                                                               
* SPNWSBRV                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPNWSBRV                                                       
         PRINT ON                                                               
* SPNWSCOMP                                                                     
         PRINT OFF                                                              
CMPRECD  DSECT                                                                  
       ++INCLUDE SPNWSCOMP                                                      
         PRINT ON                                                               
* SPGENEQU                                                                      
         PRINT OFF                                                              
EQUHDRD  DSECT                                                                  
       ++INCLUDE SPGENEQU                                                       
         PRINT ON                                                               
* SPGENGOAL                                                                     
         PRINT OFF                                                              
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
* SPDEMUPD                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPDEMUPD                                                       
         PRINT ON                                                               
* DEDBLOCK                                                                      
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
* SPPWBLOCK                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPPWBLOCK                                                      
         PRINT ON                                                               
* SPRANSIDD                                                                     
         PRINT OFF                                                              
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
         PRINT ON                                                               
* SPGENSIR                                                                      
         PRINT OFF                                                              
SIRRECD  DSECT                                                                  
       ++INCLUDE SPGENSIR                                                       
         PRINT ON                                                               
* SPSTABLK                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPSTABLK                                                       
         PRINT ON                                                               
* DDOFFICED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DRGLOBAL                                                                      
* DROOLLOCAL                                                                    
* DDGLOBEQUS                                                                    
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DROOLLOCAL                                                     
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE SPSYSFAC                                                       
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DEDEMTABD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'250SPNWS00   01/05/12'                                      
         END                                                                    
