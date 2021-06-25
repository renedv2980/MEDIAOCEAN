*          DATA SET SEACS16    AT LEVEL 038 AS OF 04/27/10                      
*PHASE TA0D16A                                                                  
         TITLE '- SECURITY ACCESS - TS APPROVER GROUP RECORDS'                  
ACS16    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SA16**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         USING SAAPREC,R2                                                       
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         OC    OPTAGY,OPTAGY                                                    
         BZ    *+14                                                             
         MVC   AGENCYID,OPTAGY                                                  
         B     *+10                                                             
         MVC   AGENCYID,CUAALF                                                  
*                                                                               
         XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY              01 - APMVALK                                 
         B     VALREC              02 - APMVALR                                 
         B     DISKEY              03 - APMDISK                                 
         B     DISREC              04 - APMDISR                                 
         B     DELREC              05 - APMDELR                                 
         B     RESREC              06 - APMRESR                                 
         B     VALSEL              07 - APMVALP                                 
         B     GETSEL              08 - APMGETS                                 
         B     DISSEL              09 - APMDISS                                 
         B     XIT                 10 - APMVALS                                 
         B     FSTLST              11 - APMFLST                                 
         B     VAL2DEL             12 - APMPROC                                 
         B     XIT                 13 - APMFSCR                                 
         B     LSTSCR              14 - APMLSCR                                 
         B     VALREQ              15 - APMVALQ                                 
         B     PRTREP              16 - APMREPP                                 
         B     XIT                 17 - APMSETT                                 
         B     PUTKEY              18 - APMPUTK                                 
         B     XIT                 19 - APMNEWK                                 
         B     XIT                 20 - APMFRP                                  
         B     DISCHGS             21 - APMDISS2                                
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF AN APPROVER GROUP RECORD                 *         
***********************************************************************         
VALKEY   LA    R2,IOKEY                                                         
         XC    SAAPKEY,SAAPKEY     SET UP KEY:                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID    AGENCY FROM LOGON                            
         LA    R1,TSAGCDEH         GROUP CODE FROM SCREEN                       
         MVI   FVMAXL,L'TSAGCDE    (IF IT'S OK)                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALKEYX                                                          
         MVC   SAAPAGR,FVIFLD      APPROVER GROUP CODE - SPACE FILLED           
*                                                                               
VKEY02   MVC   APRECKEY(L'SAAPKEY),SAAPKEY  SAVE KEY FOR LATER                  
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1 SETUP FOR I/O                              
         CLI   APACTN,ACTDIS       DON'T LOCK FOR ACTION 'DISPLAY'              
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 DO THE I/O                                   
         BE    VKEY04                                                           
         BL    VALKEYX             I/O ERROR EXIT                               
         MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
*                                                                               
VKEY04   L     R2,AIOAREA1         R2=A(RECORD)                                 
         USING SAPCTD,R4           R4=A(PERSON COUNT ELEMENT)                   
         LA    R4,SAAPDATA         FIND THE PERSON COUNT ELEMENT                
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ    PERSON COUNT ELEMENT                         
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R4,RF,*-12                                                       
*                                                                               
         CLC   SAPCTVAL,=H'0'      CAN ONLY DEL IF PERSON COUNT IS 0            
         BE    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEYX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN APPROVER GROUP RECORD                   *         
***********************************************************************         
         USING SAAPGD,R3           R3=A(APPROVER GROUP ELEMENT)                 
         USING SAPCTD,R4           R4=A(PERSON COUNT ELEMENT)                   
VALREC   CLI   APACTN,ACTADD       ADDING THE RECORD?                           
         BE    VALRECA             YES                                          
*                                                                               
VALRECC  EQU   *                   FORMAT ELEMENTS FOR CHANGE                   
         L     R2,AIOAREA1         R2=A(RECORD)                                 
         LA    R4,SAAPDATA         FIND THE PERSON COUNT ELEMENT                
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R4,RF,*-12                                                       
         XC    APWORK(SAPCTLNQ+1),APWORK                                        
         MVC   APWORK(SAPCTLNQ),SAPCTEL  SAVE IN APWORK                         
*                                                                               
         LA    R3,SAAPDATA         FIND THE GROUP COUNT ELEMENT                 
         XR    RF,RF                                                            
         CLI   SAAPGEL,SAAPGELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAPGLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         MVC   APELEM(SAAPGLNQ),SAAPGEL   SAVE FIXED PART                       
         LA    R3,APELEM           HANG DSECT TO FORMAT IT                      
         XC    SAAPGNAM(L'SAAPGNAM+1),SAAPGNAM                                  
         MVI   SAAPGLN,SAAPGLNQ    CHANGE LENGTH ACCORDINGLY                    
         B     VALRECF                                                          
*                                                                               
VALRECA  EQU   *                   FORMAT RECORD FOR ADD.                       
         LA    R3,APELEM           SET UP THE TWO ELEMENTS READY                
         XC    SAAPGEL(SAAPGLNQ+L'SAAPGNAM+1),SAAPGEL                           
         MVI   SAAPGEL,SAAPGELQ                                                 
         MVI   SAAPGLN,SAAPGLNQ                                                 
         LA    R4,APWORK                                                        
         XC    SAPCTEL(SAPCTLNQ+1),SAPCTEL                                      
         MVI   SAPCTEL,SAPCTELQ                                                 
         MVI   SAPCTLN,SAPCTLNQ                                                 
*                                                                               
         LA    R2,IOKEY                                                         
         XC    SAAPKEY,SAAPKEY     READ THE GROUP COUNT REC                     
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID    LOGON AGENCY                                 
         LA    R1,IOREAD+IOLOCK+IOCONFIL+IO1                                    
         GOTO1 AIO                                                              
         BL    VALRECX             I/O ERROR                                    
         BZ    VALRECE                                                          
*                                                                               
         L     R2,AIOAREA1         GROUP CNT REC DOESNT EXIST SO ADD IT         
         XC    SAAPKEY(SAAPDATA-SAAPKEY+1),SAAPKEY                              
         MVC   SAAPKEY,IOKEY       INITIALISE TO CORRECT VALUE                  
         XR    R0,R0               INITIALISE LENGTH                            
         LA    R0,SAAPDATA-SAAPKEY+1                                            
         STCM  R0,3,SAAPLEN                                                     
         LA    RF,1                INIT LAST USED A.G. NO TO 1                  
         STCM  RF,3,SAAPGNUM       STORE IN GROUP NAME ELEMENT                  
         MVC   WORK(SAAPGLNQ),APELEM SAVE EL COZ SETACT WILL DESTROY IT         
         GOTO1 AADDELS,SAAPREC                                                  
         GOTO1 ASETACT,SAAPREC                                                  
         MVC   APELEM(SAAPGLNQ),WORK RESTORE APELEM TO HOW IT WAS               
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                 ADD NEW RECORD FOR THIS AGENCY               
         BZ    VALRECF             OK, CARRY ON                                 
         DC    H'0'                SHOULD NOT FAIL                              
*                                                                               
VALRECE  EQU   *                   ADD WHEN GROUP COUNT REC EXISTS              
         L     R2,AIOAREA1                                                      
         LA    R3,SAAPDATA         FIND THE GROUP COUNT ELEMENT                 
         XR    RF,RF                                                            
         CLI   SAAPGEL,SAAPGELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAPGLN                                                       
         BXH   R3,RF,*-12                                                       
         MVC   APELEM(SAAPGLNQ),0(R3)  SAVE IN APELEM FOR LATER                 
         XR    RF,RF               WHAT'S THE NEXT NUMBER                       
         ICM   RF,3,SAAPGNUM                                                    
         LA    RF,1(RF)            ADD 1 FOR NEW NUMBER                         
         STCM  RF,3,SAAPGNUM       MOVE IT BACK                                 
         LA    R3,APELEM           HANG THE DSECT ON STORED VERSION             
         STCM  RF,3,SAAPGNUM       PUT NUMBER IN THERE FOR LATER                
         MVC   WORK(SAAPGLNQ),APELEM SAVE EL COZ SETACT WILL DESTROY IT         
         GOTO1 ASETACT,SAAPREC                                                  
         MVC   APELEM(SAAPGLNQ),WORK RESTORE AS BEFORE                          
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                 REWRITE RECORD FOR THIS AGENCY               
         BZ    *+6                                                              
         DC    H'0'                SHOULD NOT FAIL                              
*                                                                               
VALRECF  EQU   *                   CLEAR KEY AND FILL IN ELEMENTS               
         L     R2,AIOAREA1                                                      
         XC    SAAPKEY(SAAPDATA-SAAPKEY+1),SAAPKEY                              
         MVC   SAAPKEY,APRECKEY     REMOVE ALL ELEMENTS                         
         LA    R0,SAAPDATA+1-SAAPKEY                                            
         STCM  R0,3,SAAPLEN                                                     
*                                                                               
         LA    R3,APELEM           AFTER PREVIOUS STUFF THE ELS                 
         LA    R4,APWORK           ARE IN APELEM AND APWORK.                    
*                                                                               
         LA    R1,TSAGNMEH         GROUP NAME FROM SCREEN                       
         MVI   FVMAXL,L'TSAGNME        (IF IT'S OK)                             
         MVI   FVMINL,1            1 => IT'S REQUIRED INPUT                     
         GOTO1 AFVAL                                                            
         BNE   VALRECX                                                          
*                                                                               
         MVC   TSAGNME,FVIFLD      RETRANSMIT NAME                              
         OI    TSAGNMEH+FHOID,FHOITR                                            
         XR    RF,RF               MOVE IT INTO ELEMENT                         
         IC    RF,FVXLEN           RF = LENGTH OF INPUT - 1                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SAAPGNAM(0),FVIFLD                                               
*                                                                               
         LA    RE,SAAPGLNQ         RECALCULATE ELEMENT LENGTH                   
         LA    RE,1(RE,RF)                                                      
         STC   RE,SAAPGLN                                                       
*                                                                               
VALRECL  EQU *                     NAME NOT I/P SO ELEMENT SHORT                
         MVC   TSAGCDE,SAAPAGR     RETRANSMIT CODE                              
         OI    TSAGCDEH+FHOID,FHOITR                                            
*                                                                               
         GOTO1 AADDELS,SAAPREC     ADD GROUP NAME ELEMENT                       
*                                                                               
         MVC   APELEM(SAPCTLNQ+1),APWORK                                        
         GOTO1 AADDELS,SAAPREC     ADD PERSON COUNT ELEMENT                     
*                                  VALIDATE COMMENT LINES                       
         LA    R0,6                NUMBER OF COMMENT LINES                      
         LA    R1,TSACOMH          ADDRESS OF FIRST LINE                        
         BAS   RE,VALIDCOM         DO VALIDATION                                
*                                                                               
VR100    LA    R4,TSAPIDH          PROCESS MANAGER ID ELEMENTS                  
         LA    R0,MANNUM                                                        
         SR    R8,R8                                                            
         USING MANIDD,R4                                                        
*                                                                               
VR110    LA    R3,APELEM           INITIALISE MANAGER ID ELEMENT                
         USING SAMAND,R3                                                        
         XC    SAMANEL(SAMANLNQ),SAMANEL                                        
         MVI   SAMANEL,SAMANELQ                                                 
         MVI   SAMANLN,SAMANLNQ                                                 
         MVI   FVMINL,1            READ OFFICE NAME FIELD                       
         GOTO1 AFVAL,MANPIDH       READ MANAGER PERSONAL ID                     
         BNE   VR120                                                            
         GOTO1 AGETPNUM,FVIFLD                                                  
         BNE   VALRECX                                                          
         MVC   SAMANID,APHALF      SAVE MANAGER ID NUMBER                       
         LA    R8,1(R8)                                                         
         STC   R8,SAMANORD         SAVE MANAGER ORDER NUMBER                    
         GOTO1 AADDELS,SAAPREC     ADD MANGER ID ELEMENT                        
VR120    LA    R4,MANIDL(R4)                                                    
         BCT   R0,VR110                                                         
         EJECT                                                                  
VR200    GOTO1 ASETACT,SAAPREC     UPDATE ACTIVITY ELEMENT                      
         SPACE 1                                                                
         MVC   IOKEY(L'SAAPKEY),APRECKEY  RESTORE GROUP RECORD KEY              
         CLI   APACTN,ACTADD                                                    
         BE    *+12                                                             
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         B     *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                 PERFORM THE I/O                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
         SPACE 1                                                                
         DROP  R3,R4                                                            
VALRECX  B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF AN APPROVER GROUP RECORD                  *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         TWAXC TSAGCDEH,TSAGCDEH                                                
         OI    TSAGCDEH+FHOID,FHOITR                                            
         MVC   TSAGCDE,SPACES                                                   
         MVC   TSAGCDE,SAAPAGR                                                  
*                                                                               
DISKEYX  B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY APPROVER GROUP RECORD                            *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         TWAXC TSAGCDEH,TSAGCDEH              DISPLAY THE KEY                   
         BAS   RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
         LA    R4,TSAPIDH                                                       
         LA    R0,MANNUM                                                        
         USING MANIDD,R4                                                        
*                                                                               
DR010    XC    MANPIDN,MANPIDN                                                  
         OI    MANPIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R4,MANIDL(R4)                                                    
         BCT   R0,DR010                                                         
DR012    LA    R4,TSAPIDH                                                       
         SPACE 1                                                                
         OI    TSAGCDEH+FHOID,FHOITR                                            
         MVC   TSAGCDE,SPACES                                                   
         MVC   TSAGCDE,SAAPAGR                                                  
         SPACE 1                                                                
         TWAXC TSAGNMEH                                                         
         OI    TSAGNMEH+FHOID,FHOITR                                            
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SAAPGELQ     FIND THE APPROVER GROUP ELEMENT              
         GOTO1 AGETELS,SAAPREC                                                  
         L     R3,APPARM           APPARM =A(ELEMENT) IF FOUND                  
         LTR   R3,R3               ZEROS IF NOT                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SAAPGD,R3                                                        
         SPACE 1                                                                
         MVC   TSAGNME,SPACES      CLEAR SCREEN FIELD                           
         LA    RE,SAAPGLNQ         RE = LENGTH OF FIXED PART                    
         XR    RF,RF                                                            
         IC    RF,SAAPGLN          RF = LENGTH OF WHOLE ELEMENT                 
         SR    RF,RE               RF = LENGTH OF VARIABLE PART                 
         BCTR  RF,0                -1 FOR EXECUTE                               
         LTR   RF,RF               CHECK FOR NULL LENGTH                        
         BM    DR020                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSAGNME(0),SAAPGNAM                                              
*                                  DISPLAY COMMENT LINES                        
DR020    LA    R0,6                  NUMBER OF COMMENT LINES                    
         LA    R1,TSACOMH            ADDRESS OF FIRST LINE                      
         BAS   RE,DISPCOM            DO DISPLAY                                 
         EJECT                                                                  
*                                  PROCESS MANAGER ID ELEMENTS                  
         LA    R3,SAAPDATA                                                      
DR100    CLI   0(R3),0                                                          
         BE    DISRECX             END OF RECORD                                
         CLI   0(R3),SAMANELQ                                                   
         BE    DR120                                                            
*                                                                               
DR110    SR    R0,R0               GET NEXT ELEMENT                             
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR100                                                            
*                                                                               
         USING SAMAND,R3                                                        
DR120    EQU   *                   MANAGER ID ELEMENT                           
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   DR122                                                            
         MVC   MANPID,APWORK                                                    
         GOTO1 AGETPNAM,APWORK     GET PERSON NAME                              
         BNE   DR122                                                            
         L     RF,4(R4)                                                         
         MVC   MANPIDN,APWORK      DISPLAY MANAGER NAME                         
         LA    R4,MANIDL(R4)                                                    
         B     DR110                                                            
DR122    MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APHALF,=Y(CS#PIDNF) NAME NOT FOUND MESSAGE                       
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   MANPIDN,APWORK      DISPLAY MANAGER NAME                         
         LA    R4,MANIDL(R4)                                                    
         B     DR110                                                            
*                                                                               
DISRECX  B     XIT                                                              
         DROP  R3                                                               
         SPACE 1                                                                
REENTER  CLI   APACTN,ACTDEL       HANDLE DELETE REENTER PROMPT                 
         BE    *+12                                                             
         CLI   APACTN,ACTRES       HANDLE RESTORE REENTER PROMPT                
         BNE   *+8                                                              
*                                  AVOID NO DATA ENTERED SYSTEM MESSAGE         
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN APPROVER GROUP RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
DELREC   L     R2,AIOAREA1         CHECK GROUP NAME XREFERENCE                  
         GOTO1 ASETACT,SAAPREC                                                  
         OI    SAAPSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DELRECX                                                          
         SPACE 1                                                                
DRECNO   MVC   FVMSGNO,=AL2(FVFXDEL)  CAN'T DELETE MESSAGE                      
         B     DELRECX                                                          
         SPACE 1                                                                
DELRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RESTORE A DELETED APPROVER GROUP RECORD                  *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAAPREC                                                  
         NI    SAAPSTAT,FF-X'80'   UNSET DELETE                                 
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
RESRECX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)    *         
***********************************************************************         
         SPACE 1                                                                
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS & INIT VALUES FOR LIST PROCESS*         
***********************************************************************         
VALSEL   LA    R2,APRECKEY         SET UP RECORD KEY                            
         OI    ACSSRVH+FHOID,FHOIMO                                             
         XC    SAAPKEY,SAAPKEY                                                  
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID    AGENCY FROM LOGON                            
         MVC   SAAPAGR,SPACES      GROUP=SPACES SO DONT READ COUNT REC          
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VSTSA    GOTO1 AFVAL,LSTKTSAH      GET GROUP FILTER                             
         BNE   VSTSAX                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VSTSA1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VSTSA2              FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VSTSA1                                                        
VSTSA2   STC   RE,SELKEYCL                                                      
         MVC   SELTSA,FVIFLD                                                    
         MVC   SELTSAL,FVILEN                                                   
         MVC   SELTSASP,0(RF)                                                   
VSTSAX   EQU   *                                                                
         OC    SELTSA,SELTSA                                                    
         BZ    *+10                                                             
         MVC   SAAPAGR,SELTSA                                                   
         SPACE 1                                                                
         LA    R0,LSTACTH          SET INIT VALUES                              
         ST    R0,APPARM           A(FIRST LIST LINE)                           
         MVC   APPARM+6(L'LSTLN),LSTLN   LENGTH OF LIST LINE                    
         MVI   APPARM+4,(LSTFOOTH-LSTACTH)/(LSTACT2H-LSTACTH)                   
         MVC   FVMSGNO,=AL2(FVFOK) MESSAGE NO                                   
VALSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET NEXT LIST/SEL RECORD                                            *         
***********************************************************************         
GETSEL   L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'SAAPKEY),APRECKEY                                        
         SPACE 1                                                                
         TM    APINDS,APILRERD                                                  
         BZ    *+12                                                             
         NI    APINDS,X'FF'-APILRERD                                            
         B     GETSELR                                                          
         TM    APINDS,APILNSEQ     IS IT THE START OF A NEW SCREEN?             
         BO    GETSELS             NO -> READ SEQ                               
         GOTO1 AIO,IOCONFIL+IOHI+IO1  READ HIGH TO START OFF                    
         BNE   GETSELN                                                          
         B     GETSELF                                                          
         SPACE 1                                                                
GETSELR  GOTO1 AIO,IOCONFIL+IORD+IO1  READ EQUAL TO RESTART                     
         BNE   GETSELN             & DROP THRU INTO READ SEQ                    
         SPACE 1                                                                
GETSELS  EQU   *                   READ SEQUENTIAL                              
         GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   GETSELN                                                          
         SPACE 1                                                                
GETSELF  EQU   *                   CHECK THE RECORD IS OK                       
         CLI   SAAPTYP,SAAPTYPQ    CORRECT TYPE?                                
         BNE   GETSELN                                                          
         CLI   SAAPSUB,SAAPSUBQ    CORRECT SUB TYPE?                            
         BNE   GETSELN                                                          
         CLC   SAAPAGY,AGENCYID    LOGON AGENCY                                 
         BNE   GETSELN                                                          
*                                 FILTER ON GROUP CODE                          
GSTSA    CLI   SELTSASP,C' '       GROUP CODE - FILTER ONLY IF IT               
         BNH   GSTSAX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSTSA1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAAPAGR(0),SELTSA                                                
         BH    GETSELN             (NO MORE RELEVENT RECORDS)                   
GSTSA1   GOTO1 ATXTFLT,APPARM,(SELTSAL,SELTSA),(L'SAAPAGR,SAAPAGR)              
         BNE   GETSELS             READ NEXT RECORD                             
GSTSAX   EQU   *                                                                
*                                                                               
         MVC   APRECKEY(L'SAAPKEY),SAAPKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
GETSELN  MVI   APMODE,APMEOFS      NO MORE RECORDS                              
         SPACE 1                                                                
GETSELX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIST/SELECT LINE                                 *         
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
         L     R4,APPARM           APPARM=A(LINE TO BE FILLED)                  
         USING LSTACTH,R4                                                       
         SPACE 1                                                                
         MVC   LSTGCDE,SAAPAGR     FILL IN APPROVER GROUP CODE                  
         SPACE 1                                                                
         LA    R3,SAAPDATA         FIND THE GROUP NAME ELEMENT                  
         USING SAAPGD,R3           R3=A(GRP NAME ELEMENT)                       
         XR    RF,RF                                                            
         CLI   SAAPGEL,SAAPGELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAPGLN                                                       
         BXH   R3,RF,*-12                                                       
         SPACE 1                                                                
         MVC   LSTGNME,SPACES      CLEAR NAME AND FILL IT IN                    
         CLI   SAAPGLN,SAAPGLNQ    IF IT EXISTS                                 
         BE    DISSELP                                                          
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,SAAPGLN          RF=TOTAL LENGTH                              
         LA    RE,SAAPGLNQ+1       RE=L'FIXED PART (+1 SO THAT...               
         SR    RF,RE               RF=L'NAME-1 FOR THE EXECUTE)                 
         LTR   RF,RF               CHECK FOR NULL LENGTH                        
         BM    DISSELP                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSTGNME(0),SAAPGNAM                                              
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
DISSELP  LA    R3,SAAPDATA         FIND THE PERSON COUNT                        
         USING SAPCTD,R3                                                        
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R3,RF,*-12                                                       
*                                          EDIT INTO LINE                       
         EDIT  (B2,SAPCTVAL),(5,LSTGPCT),ZERO=NOBLANK                           
         SPACE 1                                                                
         DROP  R3                                                               
*                                                                               
         LA    R3,SAAPDATA         FIND MANAGER ID ELEMENT                      
         USING SAMAND,R3                                                        
         XR    RF,RF                                                            
DSEL010  CLI   SAMANEL,0                                                        
         BE    DSEL100                                                          
         CLI   SAMANEL,SAMANELQ                                                 
         BNE   DSEL020                                                          
         CLI   SAMANORD,1          ORDER # 1 MANAGER ONLY                       
         BE    DSEL030                                                          
DSEL020  IC    RF,SAMANLN                                                       
         BXH   R3,RF,DSEL010                                                    
         B     DSEL100                                                          
*                                                                               
DSEL030  EQU   *                                                                
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   DSEL100                                                          
         MVC   LSTPID,APWORK                                                    
         B     DSEL100                                                          
         SPACE 1                                                                
         DROP  R3                                                               
*                                                                               
DSEL100  EQU   *                                                                
         SPACE 1                                                                
DISSELX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* TEST TEST TEST ROUTINE TO TRANSFER CHANGES TO LIST SELECT SCREEN    *         
* IF IT'S JUST CHANGED THE NEW RECORD IMAGE IS IN IOAREA1 SO WE CAN   *         
* LEAVE IT ALONE.                                                     *         
* IF IT'S BEEN DELETED WE SET UP IOAREA1 WITH SPOOF VALUES AND LEAVE  *         
* DISSEL TO DO ITS JOB                                                *         
***********************************************************************         
DISCHGS  L     R2,AIOAREA1         DO NOTHING IF IT'S NOT BEEN DELETED          
         TM    SAAPSTAT,X'80'                                                   
         BZ    DISCHGSX                                                         
         TM    APINDS,APIOKDEL     OR IF IT'S NOT ALLOWED TO BE                 
         BZ    DISCHGSX                                                         
DISCHGSD EQU   *                   REC IS DELETED SO SET UP SPOOF               
         XC    SAAPKEY(SAAPFRST+1),SAAPKEY  INIT KEY                            
         MVC   SAAPKEY,IOKEY                                                    
         LA    R0,SAAPFRST+1                                                    
         STCM  R0,3,SAAPLEN                                                     
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM           INIT NAME ELEMENT TO                         
         USING SAAPGD,R3           SPOOF VALUE OF '** DELETED **'               
         MVI   SAAPGEL,SAAPGELQ                                                 
         LA    RF,SAAPGLNQ                                                      
         LA    RF,L'SAAPGNAM(RF)                                                
         STC   RF,SAAPGLN                                                       
         MVC   SAAPGNAM(2),=C'**'                                               
         MVCDD SAAPGNAM+3(8),CT#DELD                                            
         MVC   SAAPGNAM+12(2),=C'**'                                            
         GOTO1 AADDELS,SAAPREC                                                  
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM           INIT PERSON COUNT ELEMENT TO                 
         USING SAPCTD,R3           SPOOF VALUE OF ZERO                          
         MVI   SAPCTEL,SAPCTELQ                                                 
         MVI   SAPCTLN,SAPCTLNQ                                                 
         GOTO1 AADDELS,SAAPREC                                                  
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
DISCHGSX B     DISSEL              N.B. NOT TO EXIT                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DELETE ACTION FROM LIST/SELECT                  *         
* ONLY ALL RIGHT IF PERSON COUNT IS 0                                 *         
***********************************************************************         
VAL2DEL  MVC   IOKEY(L'SAAPKEY),APRECKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1    READ DEL IN CASE SOMEONE ELSE           
         GOTO1 AIO                      DELETED IT                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
*                                                                               
         TM    IOERR,IOEDEL        CAN'T DELETE IF ALREADY DELETED              
         BO    VAL2DELN                                                         
*                                                                               
         LA    R3,SAAPDATA         FIND THE PERSON COUNT                        
         USING SAPCTD,R3                                                        
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R3,RF,*-12                                                       
*                                                                               
         CLC   SAPCTVAL,=AL2(0)    ONLY OK IF NO PEOPLE ATTACHED                
         BNE   VAL2DELN                                                         
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK) GOT THIS FAR SO IT MUST BE OK                
         B     VAL2DELX                                                         
*                                                                               
VAL2DELN EQU   *                   IT'S NOT OK                                  
         MVC   FVMSGNO,=AL2(FVFXDEL)  CAN'T DELETE MESSAGE                      
VAL2DELX B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PFKEYS)                   *         
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE VALIDATE REPORT REQUEST SCREEN                              *         
***********************************************************************         
VALREQ   L     R9,AREP                                                          
         USING REPD,R9                                                          
         XC    APRECKEY,APRECKEY                                                
         SPACE 1                                                                
         MVI   FVMINL,1                                                         
         MVI   FVMAXL,L'REPREQ                                                  
         GOTO1 AFVAL,REPREQH       VALIDATE REQUESTOR                           
         BNE   VALREQX                                                          
         MVC   INUSER,FVIFLD       PUT VALUE IN THIS TIME INPUTS                
         SPACE 1                                                                
         GOTO1 AVALWHEN,REPWHENH   VALIDATE WHEN (NOW SOON O/NIGHT)             
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALDEST,REPDESTH   VALIDATE DEST                                
         BNE   VALREQX                                                          
         SPACE 1                                                                
         GOTO1 AVALOTYP,REPOTYPH   VALIDATE OUTPUT TYPE                         
         BNE   VALREQX                                                          
         SPACE 1                                                                
         LA    R2,APRECKEY         SET INITIAL KEY VALUE                        
         MVI   SAAPTYP,SAAPTYPQ                                                 
         MVI   SAAPSUB,SAAPSUBQ                                                 
         MVC   SAAPAGY,AGENCYID                                                 
         MVC   SAAPAGR,SPACES      SPACES IN GROUP CODE SO WE...                
*                                               ...DONT READ COUNT REC          
*                                                                               
*        GOTO1 AFVAL,REPKTSAH      GET START APPROVER GROUP CODE KEY            
         BNE   VQ010                                                            
         MVC   SELTSA,FVIFLD                                                    
         MVC   SELTSAL,FVILEN                                                   
         MVC   SAAPAGR,SELTSA                                                   
*                                                                               
VQ010    MVCDD REPDESC,CT#APGL     INITIAL REPORT STUFF                         
         GOTO1 VDICTAT,APPARM,C'SL  ',REPDESC                                   
         MVI   REPHEADI,REPHSPAC+REPHCLRA                                       
         MVI   REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVI   REPFOOTN,0                                                       
         LA    R0,REPSPEC                                                       
         ST    R0,REPAPHS                                                       
         OI    REPIND2,REPILOW                                                  
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALREQX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PRINT APPROVER GROUP RECORDS                             *         
***********************************************************************         
PRTREP   L     R9,AREP                                                          
         LA    R2,IOKEY            SET UP INITIAL KEY                           
         MVC   SAAPKEY(L'SAAPKEY),APRECKEY                                      
         LA    R1,IOHI+IOCONFIL+IO1                                             
         GOTO1 AIO                 READ FIRST                                   
         L     R2,AIOAREA1         HANG DSECT ON RECORD                         
         SPACE 1                                                                
PRTREPL  BNE   PRTREPX                                                          
         CLI   SAAPTYP,SAAPTYPQ    CORRECT TYPE?                                
         BNE   PRTREPX                                                          
         CLI   SAAPSUB,SAAPSUBQ    CORRECT TYPE?                                
         BNE   PRTREPX                                                          
         CLC   SAAPAGY,AGENCYID    AGENCY FROM LOGON                            
         BNE   PRTREPX                                                          
         MVC   APRECKEY(L'SAAPKEY),SAAPKEY   SAVE LAST KEY                      
         SPACE 1                                                                
         MVC   LINEGCDE,SAAPAGR    FILL IN APPROVER GROUP CODE                  
         SPACE 1                                                                
         LA    R3,SAAPDATA         FIND THE GROUP NAME ELEMENT                  
         USING SAAPGD,R3           R3=A(GRP NAME ELEMENT)                       
         XR    RF,RF                                                            
         CLI   SAAPGEL,SAAPGELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAPGLN                                                       
         BXH   R3,RF,*-12                                                       
         SPACE 1                                                                
         MVC   LINEGNME,SPACES     CLEAR NAME AND FILL IT IN                    
         CLI   SAAPGLN,SAAPGLNQ    IF IT EXISTS                                 
         BE    PRTREPP                                                          
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,SAAPGLN          RF=TOTAL LENGTH                              
         LA    RE,SAAPGLNQ+1       RE=L'FIXED PART (+1 SO THAT...               
         SR    RF,RE               RF=L'NAME-1 FOR THE EXECUTE)                 
         LTR   RF,RF               CHECK NULL LENGTH                            
         BM    PRTREPP                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LINEGNME(0),SAAPGNAM                                             
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
PRTREPP  LA    R3,SAAPDATA         FIND THE PERSON COUNT                        
         USING SAPCTD,R3                                                        
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R3,RF,*-12                                                       
*                                          EDIT INTO LINE                       
         EDIT  (B2,SAPCTVAL),(4,LINEPCT),ZERO=NOBLANK                           
         SPACE 1                                                                
         DROP  R3                                                               
*                                                                               
         LA    R4,LINEPID                                                       
         LA    R3,SAAPDATA         FIND MANAGER ID ELEMENT                      
         USING SAMAND,R3                                                        
PRT010   CLI   SAMANEL,0                                                        
         BE    PRT100                                                           
         CLI   SAMANEL,SAMANELQ                                                 
         BE    PRT030                                                           
PRT020   ZIC   RF,SAMANLN                                                       
         BXH   R3,RF,PRT010                                                     
         B     PRT100                                                           
*                                                                               
PRT030   EQU   *                                                                
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   PRT020                                                           
         MVC   0(L'MANPID,R4),APWORK                                            
*                                  BACKUP TO LAST NON-BLANK CHAR                
         AHI   R4,L'MANPID-1                                                    
         CLI   0(R4),C' '                                                       
         BNE   *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
*                                                                               
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         B     PRT020                                                           
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
                                                                                
PRT100   LA    RE,LINEPID                                                       
         CR    RE,R4                                                            
         BE    *+10                NO MANAGER LIST                              
         BCTR  R4,0                                                             
         MVI   0(R4),C' '          REMOVE LAST ','                              
         GOTO1 VREPORT,REPD        PRINT A LINE                                 
*                                                                               
         LA    R3,SAAPDATA         FIND MANAGER ID ELEMENT                      
         USING SACOMD,R3                                                        
PRT110   CLI   SACOMEL,0                                                        
         BE    PRT200                                                           
         CLI   SACOMEL,SACOMELQ                                                 
         BNE   PRT120                                                           
         ZIC   RE,SACOMLN                                                       
         SHI   RE,SACOMDAT-SACOMD+1                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LINEGNME+2(0),SACOMDAT                                           
         GOTO1 VREPORT,REPD        PRINT A LINE                                 
PRT120   ZIC   RF,SACOMLN                                                       
         BXH   R3,RF,PRT110                                                     
*                                                                               
*                                  GET NEXT RECORD (SEQUENCE BROKEN)            
PRT200   LA    R2,IOKEY                                                         
         MVC   SAAPKEY(L'SAAPKEY),APRECKEY                                      
         GOTO1 AIO,IORD+IOCONFIL+IO1                                            
         BNE   PRTREPX                                                          
         L     R2,AIOAREA1         HANG DSECT ON RECORD                         
*                                  GET NEXT RECORD (IN SEQUENCE)                
         LA    R1,IOSQ+IOCONFIL+IO1                                             
         GOTO1 AIO                 READ NEXT REC                                
         B     PRTREPL             BRANCH TO TOP OF LOOP                        
         SPACE 1                                                                
PRTREPX  B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PUT KEY OF RECORD TO SAVED KEY TABLE                     *         
***********************************************************************         
PUTKEY   LA    R2,APRECKEY                                                      
         LA    R3,APELEM                                                        
         SPACE 1                                                                
         MVI   0(R3),KEYTSA        FORMATTED LIKE AN ELEMENT                    
         MVI   1(R3),L'SAAPAGR+2   LENGTH                                       
         MVC   2(8,R3),SAAPAGR                                                  
         SPACE 1                                                                
         XR    R0,R0               X'00' TO MARK THE END                        
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         MVI   0(R3),0                                                          
         SPACE 1                                                                
         GOTO1 APUTKEY             CONTROLLER ROUTINE                           
         SPACE 1                                                                
PUTKEYX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE COMMENT LINES AND BUILD COMMENT ELEMENTS        *         
* R0 - # LINES  R1 - FIELD HEADER  R2 - A(RECORD)                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SACOMD,R3                                                        
VALIDCOM NTR1                                                                   
         LA    R4,1                                                             
         LA    R3,APELEM                                                        
VCOM010  XC    SACOMEL(SACOMLNQ),SACOMEL                                        
         MVI   SACOMEL,SACOMELQ                                                 
         MVI   SACOMLN,SACOMLNQ                                                 
         STC   R4,SACOMLIN                                                      
         LR    R8,R1                                                            
         GOTO1 AFVAL                                                            
         BNE   VCOMX                                                            
         ZIC   R1,SACOMLN                                                       
         LA    RE,SACOMD(R1)                                                    
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),FVIFLD                                                   
         LA    RF,1(RF,R1)                                                      
         STC   RF,SACOMLN                                                       
         GOTO1 AADDELS,(R2)                                                     
         ZIC   RF,0(R8)                                                         
         LR    R1,R8                                                            
         AR    R1,RF                                                            
         LA    R4,1(R4)                                                         
         BCT   R0,VCOM010                                                       
VCOMX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY COMMENT ELEMENTS                                 *         
* R0 - # LINES  R1 - FIELD HEADER  R2 - A(RECORD)                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SACOMD,R3                                                        
DISPCOM  NTR1                                                                   
         LA    R4,1                                                             
         LA    R3,APELEM                                                        
DCOM010  XC    APELEM,APELEM                                                    
         MVI   APELEM,SACOMELQ    FIND NEXT COMMENT ELEMENT                     
         MVI   APELEM+1,2                                                       
         STH   R4,APELEM+2                                                      
         LR    R8,R1                                                            
         GOTO1 AGETELS,(R2)                                                     
         L     R3,APPARM           APPARM =A(ELEMENT) IF FOUND                  
         LTR   R3,R3               ZEROS IF NOT                                 
         BZ    DCOM100                                                          
         SR    RE,RE                                                            
         IC    RE,SACOMLN                                                       
         LA    RF,SACOMLNQ+1                                                    
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R8),SACOMDAT                                                 
*                                                                               
DCOM100  ZIC   RF,0(R8)                                                         
         LR    R1,R8                                                            
         AR    R1,RF                                                            
         LA    R4,1(R4)                                                         
         BCT   R0,DCOM010                                                       
DCOMX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK WHETHER APPROVER GROUP NUMBER IS USED IN OTHER     *         
* SECURITY ACCESS RECORD KEYS.                                        *         
* RECORD SUB TYPE IN APBYTE, APPROVER GROUP NUMBER IN APHALF.         *         
***********************************************************************         
         SPACE 1                                                                
CHKXREF  NTR1                                                                   
         LA    R2,IOKEY                                                         
         USING SAASREC,R2                                                       
         XC    SAASKEY,SAASKEY       SET UP KEY:                                
         MVI   SAASTYP,SAASTYPQ                                                 
         MVC   SAASSUB,APBYTE                                                   
         MVC   SAASAGY,AGENCYID                                                 
         L     R2,AIOAREA2                                                      
         LA    R1,IOCONFIL+IOHI+IO2                                             
         B     *+8                                                              
CREF010  LA    R1,IOCONFIL+IOSQ+IO2                                             
         GOTO1 AIO                                                              
         BNE   CREFOK                                                           
         CLC   IOKEY(SAASOVS-SAASKEY),SAASKEY                                   
         BNE   CREFOK                                                           
         CLC   SAASAGN,APHALF                                                   
         BE    CREFNO                                                           
         B     CREF010                                                          
CREFOK   SR    RC,RC                                                            
CREFNO   LTR   RC,RC                                                            
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
         SPACE 1                                                                
SAAPFRST EQU   SAAPDATA-SAAPREC                                                 
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
SPACES   DC    132C' '                                                          
         LTORG                                                                  
         SPACE 1                                                                
LSTLN    DC    AL2(LSTACT2H-LSTACTH)  LIST LINE LENGTH                          
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,30,CT#APGL,30,L                                               
         SPEC  H2,30,CT#APGL,30,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,4,CT#GROUP,8,L                                                
         SPEC  M2,4,CT#GROUP,8,LU                                               
         SPEC  M1,15,CT#NAME,30,L                                               
         SPEC  M2,15,CT#NAME,30,LU                                              
         SPEC  M1,47,CT#STAFF,5,L                                               
         SPEC  M2,47,CT#STAFF,5,LU                                              
         SPEC  M1,58,CT#MANID,12,L                                              
         SPEC  M2,58,CT#MANID,12,LU                                             
         SPEC  END                                                              
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSCCD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSCBD                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSCAD                                                       
         ORG   LSTLIN              * LIST LINE LAYOUT *                         
LSTGCDE  DS    CL8                 APPROVER GROUP CODE                          
         DS    CL2                                                              
LSTGNME  DS    CL30                APPROVER GROUP NAME                          
         DS    CL3                                                              
LSTPID   DS    CL8                 MANAGER PERSONAL ID                          
         DS    CL1                                                              
LSTGPCT  DS    CL5                 PERSON COUNT                                 
         SPACE 2                                                                
         ORG                                                                    
         EJECT                                                                  
MANIDD   DSECT                     ** MANAGER ID FIELD LAYOUT **                
MANPIDH  DS    XL8                                                              
MANPID   DS    CL8                 PERSONAL ID                                  
MANPIDX  DS    XL8                                                              
MANPIDNH DS    XL8                                                              
MANPIDN  DS    CL30                PERSON NAME                                  
MANIDL   EQU   (TSAPI2H-TSAPIDH)                                                
MANNUM   EQU   6                                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
         DS    CL3                                                              
LINEGCDE DS    CL8                 APPROVER GROUP CODE                          
         DS    CL3                                                              
LINEGNME DS    CL30                APPROVER GROUP NAME                          
         DS    CL3                                                              
LINEPCT  DS    CL5                 PERSON COUNT                                 
         DS    CL5                                                              
LINEPID  DS    CL55                MANAGER PERSONAL ID                          
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
SAVEKEY  DS    XL(L'SAAPKEY)                                                    
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
SELOPT   DS    0X                  LIST SELECT FILTERS                          
SELTSA   DS    XL(L'SAAPAGR)       APPROVER GROUP CODE FILTER                   
SELTSAL  DS    XL1                 TSA FILTER LENGTH                            
SELTSASP DS    XL1                 FIRST NON SPACE CHAR                         
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SEACS16   04/27/10'                                      
         END                                                                    
