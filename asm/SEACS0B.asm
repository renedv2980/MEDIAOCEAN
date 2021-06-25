*          DATA SET SEACS0B    AT LEVEL 028 AS OF 11/29/17                      
*PHASE TA0D0BA                                                                  
         TITLE '- SECURITY ACCESS - ACCESS GROUP RECORDS'                       
ACS0B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACSB**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         LA    R2,IOKEY            R2=A(RECORD KEY)                             
         USING SAAGREC,R2                                                       
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
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE KEY OF AN ACCESS GROUP RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   LA    R2,IOKEY                                                         
         XC    SAAGKEY,SAAGKEY       SET UP KEY:                                
         MVC   SAAGTYP(RTYPL),RTYP     RECORD TYPE (CONSTANT)                   
         MVC   SAAGAGY,AGENCYID        AGENCY FROM LOGON                        
         LA    R1,AGRGCDEH             GROUP CODE FROM SCREEN                   
         MVI   FVMAXL,L'AGRGCDE        (IF IT'S OK)                             
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL                                                            
         BNE   VALKEYX                                                          
         MVC   SAAGAGR,FVIFLD                                                   
         CLI   APACTN,ACTDIS       TEST SECURITY MANAGER ACCESS                 
         BE    VKEY1                 WITH UODATEIVE ACTIONS                     
         CLI   APACTN,ACTLST                                                    
         BE    VKEY1                                                            
         CLI   APACTN,ACTREP                                                    
         BE    VKEY1                                                            
         CLI   APACTN,ACTSEL                                                    
         BE    VKEY1                                                            
         GOTO1 ATSTGMAN,FVIFLD                                                  
         BNE   VALKEYX                                                          
         B     VKEY1                                                            
         SPACE 1                                                                
VKEY1    MVC   APRECKEY(L'SAAGKEY),SAAGKEY  SAVE KEY FOR LATER                  
         SPACE 1                                                                
         LA    R1,IORDD+IOCONFIL+IO1 SETUP FOR I/O                              
         CLI   APACTN,ACTDIS       DON'T LOCK FOR ACTION 'DISPLAY'              
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                 DO THE I/O                                   
         BE    VKEY2                                                            
         BL    VALKEYX             I/O ERROR EXIT                               
         MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
         SPACE 1                                                                
VKEY2    EQU   *                   CHECK WHETHER ITS OK TO DELETE               
         L     R2,AIOAREA1         R2=A(RECORD)                                 
         USING SAPCTD,R4           R4=A(PERSON COUNT ELEMENT)                   
         LA    R4,SAAGDATA         FIND THE PERSON COUNT ELEMENT                
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R4,RF,*-12                                                       
         SPACE 1                                                                
         CLC   SAPCTVAL,=H'0'      CAN ONLY DEL IF PERSON COUNT IS 0            
         BE    *+12                                                             
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
         SPACE 1                                                                
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         SPACE 1                                                                
VALKEYX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD OR CHANGE AN ACCESS GROUP RECORD                     *         
***********************************************************************         
         SPACE 1                                                                
VALREC   EQU   *                                                                
         USING SAAGND,R3           R3=A(ACCESS GRP ELEMENT)                     
         USING SAPCTD,R4           R4=A(PERSON COUNT ELEMENT)                   
         CLI   APACTN,ACTADD                                                    
         BE    VALRECA                                                          
         SPACE 1                                                                
VALRECC  EQU   *                   FORMAT ELEMENTS FOR CHANGE                   
         L     R2,AIOAREA1         R2=A(RECORD)                                 
         LA    R4,SAAGDATA         FIND THE PERSON COUNT ELEMENT                
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R4,RF,*-12                                                       
         XC    APWORK(SAPCTLNQ+1),APWORK                                        
         MVC   APWORK(SAPCTLNQ),SAPCTEL  SAVE IN APWORK                         
         SPACE 1                                                                
         LA    R3,SAAGDATA         FIND THE GROUP COUNT ELEMENT                 
         XR    RF,RF                                                            
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAGNLN                                                       
         BXH   R3,RF,*-12                                                       
         SPACE 1                                                                
         MVC   APELEM(SAAGNLNQ),SAAGNEL   SAVE FIXED PART                       
         LA    R3,APELEM           HANG DSECT TO FORMAT IT                      
         XC    SAAGNNAM(L'SAAGNNAM+1),SAAGNNAM                                  
         MVI   SAAGNLN,SAAGNLNQ    CHANGE LENGTH ACCORDINGLY                    
         B     VALRECF                                                          
         SPACE 1                                                                
VALRECA  EQU   *                   FORMAT RECORD FOR ADD.                       
         LA    R3,APELEM           SET UP THE TWO ELEMENTS READY                
         XC    SAAGNEL(SAAGNLNQ+L'SAAGNNAM+1),SAAGNEL                           
         MVI   SAAGNEL,SAAGNELQ                                                 
         MVI   SAAGNLN,SAAGNLNQ                                                 
         LA    R4,APWORK                                                        
         XC    SAPCTEL(SAPCTLNQ+1),SAPCTEL                                      
         MVI   SAPCTEL,SAPCTELQ                                                 
         MVI   SAPCTLN,SAPCTLNQ                                                 
         SPACE 1                                                                
         LA    R2,IOKEY                                                         
         XC    SAAGKEY,SAAGKEY     READ THE GROUP COUNT REC                     
         MVC   SAAGTYP(RTYPL),RTYP                                              
         MVC   SAAGAGY,AGENCYID        LOGON AGENCY                             
         LA    R1,IOREAD+IOLOCK+IOCONFIL+IO1                                    
         GOTO1 AIO                                                              
         BL    VALRECX             I/O ERROR                                    
         BZ    VALRECE                                                          
         SPACE 1                                                                
         L     R2,AIOAREA1         GROUP CNT REC DOESNT EXIST SO ADD IT         
         XC    SAAGKEY(SAAGFRST+1),SAAGKEY     CLEAR KEY OF RECORD              
         MVC   SAAGKEY,IOKEY       INITIALISE TO CORRECT VALUE                  
         XR    R0,R0               INITIALISE LENGTH                            
         LA    R0,SAAGFRST+1                                                    
         STCM  R0,3,SAAGLEN                                                     
         LA    RF,1                INIT LAST USED A.G. NO TO 1                  
         STCM  RF,3,SAAGNNUM       STORE IN GROUP NAME ELEMENT                  
         MVC   WORK(SAAGNELQ),APELEM SAVE EL COZ SETACT WILL DESTROY IT         
         GOTO1 AADDELS,SAAGREC                                                  
         GOTO1 ASETACT,SAAGREC                                                  
         MVC   APELEM(SAAGNELQ),WORK RESTORE APELEM TO HOW IT WAS               
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                 ADD NEW RECORD FOR THIS AGENCY               
         BZ    VALRECF             OK, CARRY ON                                 
         DC    H'0'                SHOULD NOT FAIL                              
         SPACE 1                                                                
VALRECE  EQU   *                   ADD WHEN GROUP COUNT REC EXISTS              
         L     R2,AIOAREA1                                                      
         LA    R3,SAAGDATA         FIND THE GROUP COUNT ELEMENT                 
         XR    RF,RF                                                            
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAGNLN                                                       
         BXH   R3,RF,*-12                                                       
         MVC   APELEM(SAAGNLNQ),0(R3)  SAVE IN APELEM FOR LATER                 
         XR    RF,RF               WHAT'S THE NEXT NUMBER                       
         ICM   RF,3,SAAGNNUM                                                    
         LA    RF,1(RF)            ADD 1 FOR NEW NUMBER                         
         STCM  RF,3,SAAGNNUM       MOVE IT BACK                                 
         LA    R3,APELEM           HANG THE DSECT ON STORED VERSION             
         STCM  RF,3,SAAGNNUM       PUT NUMBER IN THERE FOR LATER                
         MVC   WORK(SAAGNELQ),APELEM SAVE EL COZ SETACT WILL DESTROY IT         
         GOTO1 ASETACT,SAAGREC                                                  
         MVC   APELEM(SAAGNELQ),WORK RESTORE AS BEFORE                          
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         GOTO1 AIO                 REWRITE RECORD FOR THIS AGENCY               
         BZ    *+6                                                              
         DC    H'0'                SHOULD NOT FAIL                              
         SPACE 1                                                                
VALRECF  EQU   *                   CLEAR KEY AND FILL IN ELEMENTS               
         L     R2,AIOAREA1                                                      
         XC    SAAGKEY(SAAGFRST+1),SAAGKEY                                      
         MVC   SAAGKEY,APRECKEY     REMOVE ALL ELEMENTS                         
         LA    R0,SAAGFRST+1                                                    
         STCM  R0,3,SAAGLEN                                                     
         SPACE 1                                                                
         LA    R3,APELEM           AFTER PREVIOUS STUFF THE ELS                 
         LA    R4,APWORK           ARE IN APELEM AND APWORK.                    
         SPACE 1                                                                
         LA    R1,AGRGNMEH         GROUP NAME FROM SCREEN                       
         MVI   FVMAXL,L'AGRGNME        (IF IT'S OK)                             
         MVI   FVMINL,1            1 => IT'S REQUIRED INPUT                     
         GOTO1 AFVAL                                                            
         BNE   VALRECX                                                          
         SPACE 1                                                                
         MVC   AGRGNME,FVIFLD      RETRANSMIT NAME                              
         OI    AGRGNMEH+FHOID,FHOITR                                            
         XR    RF,RF               MOVE IT INTO ELEMENT                         
         IC    RF,FVXLEN           RF = LENGTH OF INPUT - 1                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SAAGNNAM(0),FVIFLD                                               
         SPACE 1                                                                
         LA    RF,1(RF)            BUMP RF TO ACTUAL LENGTH                     
         LA    RE,SAAGNLNQ         RECALCULATE ELEMENT LENGTH                   
         AR    RE,RF                                                            
         STC   RE,SAAGNLN                                                       
         SPACE 1                                                                
VALRECL  EQU *                     NAME NOT I/P SO ELEMENT SHORT                
         MVC   AGRGCDE,SAAGAGR     RETRANSMIT CODE                              
         OI    AGRGCDEH+FHOID,FHOITR                                            
         SPACE 1                                                                
         GOTO1 AADDELS,SAAGREC     ADD GROUP NAME ELEMENT                       
         SPACE 1                                                                
         MVC   APELEM(SAPCTLNQ+1),APWORK                                        
         GOTO1 AADDELS,SAAGREC     ADD PERSON COUNT ELEMENT                     
*                                  VALIDATE COMMENT LINES                       
         LA    R0,6                  NUMBER OF COMMENT LINES                    
         LA    R1,AGRCOMH            ADDRESS OF FIRST LINE                      
         BAS   RE,VALIDCOM           DO VALIDATION                              
*                                                                               
VR100    LA    R4,AGRPIDH          PROCESS MANAGER ID ELEMENTS                  
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
         GOTO1 AADDELS,SAAGREC     ADD MANGER ID ELEMENT                        
VR120    LA    R4,MANIDL(R4)                                                    
         BCT   R0,VR110                                                         
*                                                                               
         EJECT                                                                  
VR200    GOTO1 ASETACT,SAAGREC     UPDATE ACTIVITY ELEMENT                      
         SPACE 1                                                                
         MVC   IOKEY(L'SAAGKEY),APRECKEY  RESTORE GROUP RECORD KEY              
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
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY KEY OF AN ACCESS GROUP RECORD                    *         
***********************************************************************         
DISKEY   LA    R2,APRECKEY                                                      
         TWAXC AGRGCDEH,AGRGCDEH                                                
         OI    AGRGCDEH+FHOID,FHOITR                                            
         MVI   AGRGCDE,C' '                                                     
         MVC   AGRGCDE+1(L'AGRGCDE-1),AGRGCDE                                   
         MVC   AGRGCDE,SAAGAGR                                                  
         SPACE 1                                                                
DISKEYX  B     XIT                                                              
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS GROUP RECORD                              *         
***********************************************************************         
DISREC   L     R2,AIOAREA1                                                      
         TWAXC AGRGCDEH,AGRGCDEH              DISPLAY THE KEY                   
         BAS   RE,REENTER          HANDLE DELETE/RESTORE PROMPT                 
         LA    R4,AGRPIDH                                                       
         LA    R0,MANNUM                                                        
         USING MANIDD,R4                                                        
*                                                                               
DR010    XC    MANPIDN,MANPIDN                                                  
         OI    MANPIDNH+(FVOIND-FVIHDR),FVOXMT                                  
         LA    R4,MANIDL(R4)                                                    
         BCT   R0,DR010                                                         
DR012    LA    R4,AGRPIDH                                                       
*                                                                               
         OI    AGRGCDEH+FHOID,FHOITR                                            
         MVI   AGRGCDE,C' '                                                     
         MVC   AGRGCDE+1(L'AGRGCDE-1),AGRGCDE                                   
         MVC   AGRGCDE,SAAGAGR                                                  
*                                                                               
         TWAXC AGRGNMEH                                                         
         OI    AGRGNMEH+FHOID,FHOITR                                            
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SAAGNELQ     FIND THE ACCESS GROUP ELEMENT                
         GOTO1 AGETELS,SAAGREC                                                  
         L     R3,APPARM           APPARM =A(ELEMENT) IF FOUND                  
         LTR   R3,R3               ZEROS IF NOT                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SAAGND,R3                                                        
*                                                                               
         MVI   AGRGNME,C' '        CLEAR SCREEN FIELD                           
         MVC   AGRGNME+1(L'AGRGNME-1),AGRGNME                                   
         LA    RE,SAAGNLNQ         RE = LENGTH OF FIXED PART                    
         XR    RF,RF                                                            
         IC    RF,SAAGNLN          RF = LENGTH OF WHOLE ELEMENT                 
         SR    RF,RE               RF = LENGTH OF VARIABLE PART                 
         BCTR  RF,0                -1 FOR EXECUTE                               
         LTR   RF,RF               CHECK FOR NULL LENGTH                        
         BM    DR020                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   AGRGNME(0),SAAGNNAM                                              
*                                  DISPLAY COMMENT LINES                        
DR020    LA    R0,6                NUMBER OF COMMENT LINES                      
         LA    R1,AGRCOMH          ADDRESS OF FIRST LINE                        
         BAS   RE,DISPCOM          DO DISPLAY                                   
*                                                                               
         LA    R3,SAAGDATA         PROCESS MANAGER ID ELEMENTS                  
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
DISRECX  MVC   FVMSGNO,=AL2(FVFOK)                                              
         TM    SAAGSTAT,X'80'                                                   
         BNO   XIT                                                              
         OI    APINDS,APIOKRES                                                  
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
REENTER  CLI   APACTN,ACTDEL       HANDLE DELETE REENTER PROMPT                 
         BE    *+12                                                             
         CLI   APACTN,ACTRES       HANDLE RESTORE REENTER PROMPT                
         BNE   *+8                                                              
*                                  AVOID NO DATA ENTERED SYSTEM MESSAGE         
         OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE AN ACCESS GROUP RECORD                            *         
***********************************************************************         
         SPACE 1                                                                
DELREC   EQU   *                                                                
*                                  CHECK GROUP NAME XREFERENCE                  
         L     R2,AIOAREA1                                                      
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SAAGNELQ     FIND THE ACCESS GROUP ELEMENT                
         GOTO1 AGETELS,SAAGREC                                                  
         L     R3,APPARM           APPARM =A(ELEMENT) IF FOUND                  
         LTR   R3,R3               ZEROS IF NOT                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SAAGND,R3                                                        
         MVC   APHALF,SAAGNNUM                                                  
         DROP  R3                                                               
         MVI   APBYTE,SAASSUBQ       IN ACCESS RECORD KEYS                      
         BAS   RE,CHKXREF                                                       
         BNE   DRECNO                                                           
         MVI   APBYTE,SAFCSUBQ       IN FCONTROL RECORD KEYS                    
         BAS   RE,CHKXREF                                                       
         BNE   DRECNO                                                           
         MVI   APBYTE,SAOCSUBQ       IN OCONTROL RECORD KEYS                    
         BAS   RE,CHKXREF                                                       
         BNE   DRECNO                                                           
         GOTO1 ASETACT,SAAGREC                                                  
         OI    SAAGSTAT,X'80'      SET DELETE FLAG IN RECORD                    
         GOTO1 AIO,IOPUT+IOCONFIL+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DELRECX                                                          
*                                                                               
DRECNO   MVC   FVMSGNO,=AL2(FVFXDEL)  CAN'T DELETE MESSAGE                      
         B     DELRECX                                                          
*                                                                               
DELRECX  B     XIT                                                              
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO RESTORE A DELETED ACCESS GROUP RECORD                    *         
***********************************************************************         
         SPACE 1                                                                
RESREC   L     R2,AIOAREA1                                                      
         GOTO1 ASETACT,SAAGREC                                                  
         NI    SAAGSTAT,FF-X'80'   UNSET DELETE                                 
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
         XC    SAAGKEY,SAAGKEY                                                  
         MVC   SAAGTYP(RTYPL),RTYP TYPE                                         
         MVC   SAAGAGY,AGENCYID    AGENCY FROM LOGON                            
         MVI   SAAGAGR,C' '        GROUP=SPACES SO DONT READ COUNT REC          
         MVC   SAAGAGR+1(L'SAAGAGR-1),SAAGAGR                                   
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
VSAGR    GOTO1 AFVAL,LSTKAGRH      GET GROUP FILTER                             
         BNE   VSAGRX                                                           
         ZIC   R1,FVILEN                                                        
         L     RE,=F'-1'                                                        
         LA    RF,FVIFLD                                                        
VSAGR1   CLI   0(RF),C'A'          FIND LENGTH TO 1ST SP CHAR                   
         BL    VSAGR2              FOR KEY COMPARE IN GETSEL                    
         LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R1,VSAGR1                                                        
VSAGR2   STC   RE,SELKEYCL                                                      
         MVC   SELAGR,FVIFLD                                                    
         MVC   SELAGRL,FVILEN                                                   
         MVC   SELAGRSP,0(RF)                                                   
VSAGRX   EQU   *                                                                
         OC    SELAGR,SELAGR                                                    
         BZ    *+10                                                             
         MVC   SAAGAGR,SELAGR                                                   
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
         MVC   IOKEY(L'SAAGKEY),APRECKEY                                        
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
         CLI   SAAGTYP,SAAGTYPQ    CORRECT TYPE?                                
         BNE   GETSELN                                                          
         CLI   SAAGSUB,SAAGSUBQ    CORRECT SUB TYPE?                            
         BNE   GETSELN                                                          
         CLC   SAAGAGY,AGENCYID    LOGON AGENCY                                 
         BNE   GETSELN                                                          
*                                 FILTER ON GROUP CODE                          
GSAGR    CLI   SELAGRSP,C' '       GROUP CODE - FILTER ONLY IF IT               
         BNH   GSAGRX                CONTAINS SPECIAL (WILD) CHARS.             
         XR    R1,R1                                                            
         ICM   R1,1,SELKEYCL                                                    
         BM    GSAGR1                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SAAGAGR(0),SELAGR                                                
         BH    GETSELN             (NO MORE RELEVENT RECORDS)                   
GSAGR1   GOTO1 ATXTFLT,APPARM,(SELAGRL,SELAGR),(L'SAAGAGR,SAAGAGR)              
         BNE   GETSELS             READ NEXT RECORD                             
GSAGRX   EQU   *                                                                
*                                                                               
         MVC   APRECKEY(L'SAAGKEY),SAAGKEY                                      
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
         MVC   LSTGCDE,SAAGAGR     FILL IN ACCESS GROUP CODE                    
         SPACE 1                                                                
         LA    R3,SAAGDATA         FIND THE GROUP NAME ELEMENT                  
         USING SAAGND,R3           R3=A(GRP NAME ELEMENT)                       
         XR    RF,RF                                                            
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAGNLN                                                       
         BXH   R3,RF,*-12                                                       
         SPACE 1                                                                
         MVI   LSTGNME,C' '        CLEAR NAME AND FILL IT IN                    
         MVC   LSTGNME+1(L'LSTGNME-1),LSTGNME                                   
         CLI   SAAGNLN,SAAGNLNQ    IF IT EXISTS                                 
         BE    DISSELP                                                          
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,SAAGNLN          RF=TOTAL LENGTH                              
         LA    RE,SAAGNLNQ+1       RE=L'FIXED PART (+1 SO THAT...               
         SR    RF,RE               RF=L'NAME-1 FOR THE EXECUTE)                 
         LTR   RF,RF               CHECK FOR NULL LENGTH                        
         BM    DISSELP                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSTGNME(0),SAAGNNAM                                              
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
DISSELP  LA    R3,SAAGDATA         FIND THE PERSON COUNT                        
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
         SPACE 1                                                                
*                                                                               
         LA    R3,SAAGDATA         FIND MANAGER ID ELEMENT                      
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
         TM    SAAGSTAT,X'80'                                                   
         BZ    DISCHGSX                                                         
         TM    APINDS,APIOKDEL     OR IF IT'S NOT ALLOWED TO BE                 
         BZ    DISCHGSX                                                         
DISCHGSD EQU   *                   REC IS DELETED SO SET UP SPOOF               
         XC    SAAGKEY(SAAGFRST+1),SAAGKEY  INIT KEY                            
         MVC   SAAGKEY,IOKEY                                                    
         LA    R0,SAAGFRST+1                                                    
         STCM  R0,3,SAAGLEN                                                     
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM           INIT NAME ELEMENT TO                         
         USING SAAGND,R3           SPOOF VALUE OF '** DELETED **'               
         MVI   SAAGNEL,SAAGNELQ                                                 
         LA    RF,SAAGNLNQ                                                      
         LA    RF,L'SAAGNNAM(RF)                                                
         STC   RF,SAAGNLN                                                       
         MVC   SAAGNNAM(2),=C'**'                                               
         MVCDD SAAGNNAM+3(8),CT#DELD                                            
         MVC   SAAGNNAM+12(2),=C'**'                                            
         GOTO1 AADDELS,SAAGREC                                                  
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
         XC    APELEM,APELEM                                                    
         LA    R3,APELEM           INIT PERSON COUNT ELEMENT TO                 
         USING SAPCTD,R3           SPOOF VALUE OF ZERO                          
         MVI   SAPCTEL,SAPCTELQ                                                 
         MVI   SAPCTLN,SAPCTLNQ                                                 
         GOTO1 AADDELS,SAAGREC                                                  
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
DISCHGSX B     DISSEL              N.B. NOT TO EXIT                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE DELETE ACTION FROM LIST/SELECT                  *         
* ONLY ALL RIGHT IF PERSON COUNT IS 0                                 *         
***********************************************************************         
VAL2DEL  MVC   IOKEY(L'SAAGKEY),APRECKEY                                        
         LA    R1,IORDD+IOCONFIL+IO1    READ DEL IN CASE SOMEONE ELSE           
         GOTO1 AIO                      DELETED IT                              
         BNL   *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA1                                                      
*                                                                               
         TM    IOERR,IOEDEL        CAN'T DELETE IF ALREADY DELETED              
         BO    VAL2DELN                                                         
*                                                                               
         LA    R3,SAAGDATA         FIND THE PERSON COUNT                        
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
         MVC   SAAGTYP(RTYPL),RTYP                                              
         MVC   SAAGAGY,AGENCYID                                                 
         MVI   SAAGAGR,C' '        SPACES IN GROUP CODE SO WE...                
         MVC   SAAGAGR+1(L'SAAGAGR-1),SAAGAGR   ...DONT READ COUNT REC          
*                                                                               
         GOTO1 AFVAL,REPKAGRH      GET START ACCESS GROUP CODE KEY              
         BNE   VQ010                                                            
         MVC   SELAGR,FVIFLD                                                    
         MVC   SELAGRL,FVILEN                                                   
         MVC   SAAGAGR,SELAGR                                                   
*                                                                               
VQ010    MVCDD REPDESC,CT#AGRL     INITIAL REPORT STUFF                         
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
* ROUTINE TO PRINT ACCESS GROUP RECORDS                               *         
***********************************************************************         
PRTREP   L     R9,AREP                                                          
         LA    R2,IOKEY            SET UP INITIAL KEY                           
         MVC   SAAGKEY(L'SAAGKEY),APRECKEY                                      
         LA    R1,IOHI+IOCONFIL+IO1                                             
         GOTO1 AIO                 READ FIRST                                   
         L     R2,AIOAREA1         HANG DSECT ON RECORD                         
         SPACE 1                                                                
PRTREPL  BNE   PRTREPX                                                          
         CLC   SAAGTYP(RTYPL),RTYP CORRECT TYPE?                                
         BNE   PRTREPX                                                          
         CLC   SAAGAGY,AGENCYID    AGENCY FROM LOGON                            
         BNE   PRTREPX                                                          
         MVC   APRECKEY(L'SAAGKEY),SAAGKEY   SAVE LAST KEY                      
         SPACE 1                                                                
         MVC   LINEGCDE,SAAGAGR    FILL IN ACCESS GROUP CODE                    
         SPACE 1                                                                
         LA    R3,SAAGDATA         FIND THE GROUP NAME ELEMENT                  
         USING SAAGND,R3           R3=A(GRP NAME ELEMENT)                       
         XR    RF,RF                                                            
         CLI   SAAGNEL,SAAGNELQ                                                 
         BE    *+12                                                             
         IC    RF,SAAGNLN                                                       
         BXH   R3,RF,*-12                                                       
         SPACE 1                                                                
         MVI   LINEGNME,C' '       CLEAR NAME AND FILL IT IN                    
         MVC   LINEGNME+1(L'LINEGNME-1),LINEGNME                                
         CLI   SAAGNLN,SAAGNLNQ    IF IT EXISTS                                 
         BE    PRTREPP                                                          
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,SAAGNLN          RF=TOTAL LENGTH                              
         LA    RE,SAAGNLNQ+1       RE=L'FIXED PART (+1 SO THAT...               
         SR    RF,RE               RF=L'NAME-1 FOR THE EXECUTE)                 
         LTR   RF,RF               CHECK NULL LENGTH                            
         BM    PRTREPP                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LINEGNME(0),SAAGNNAM                                             
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
PRTREPP  LA    R3,SAAGDATA         FIND THE PERSON COUNT                        
         USING SAPCTD,R3                                                        
         XR    RF,RF                                                            
         CLI   SAPCTEL,SAPCTELQ                                                 
         BE    *+12                                                             
         IC    RF,SAPCTLN                                                       
         BXH   R3,RF,*-12                                                       
*                                          EDIT INTO LINE                       
         EDIT  (B2,SAPCTVAL),(5,LINEPCT),ZERO=NOBLANK                           
         SPACE 1                                                                
         DROP  R3                                                               
*                                                                               
         LA    R3,SAAGDATA         FIND MANAGER ID ELEMENT                      
         USING SAMAND,R3                                                        
         XR    RF,RF                                                            
PRT010   CLI   SAMANEL,0                                                        
         BE    PRT100                                                           
         CLI   SAMANEL,SAMANELQ                                                 
         BNE   PRT020                                                           
         CLI   SAMANORD,1          ORDER # 1 MANAGER ONLY                       
         BE    PRT030                                                           
PRT020   IC    RF,SAMANLN                                                       
         BXH   R3,RF,PRT010                                                     
         B     PRT100                                                           
*                                                                               
PRT030   EQU   *                                                                
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   PRT100                                                           
         MVC   LINEPID,APWORK                                                   
         B     PRT100                                                           
         SPACE 1                                                                
         DROP  R3                                                               
         SPACE 1                                                                
PRT100   GOTO1 VREPORT,REPD        PRINT A LINE                                 
         SPACE 1                                                                
*                                  GET NEXT RECORD (SEQUENCE BROKEN)            
         LA    R2,IOKEY                                                         
         MVC   SAAGKEY(L'SAAGKEY),APRECKEY                                      
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
         MVI   0(R3),KEYAGR        FORMATTED LIKE AN ELEMENT                    
         MVI   1(R3),L'SAAGAGR+2   LENGTH                                       
         MVC   2(8,R3),SAAGAGR                                                  
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
* ROUTINE TO CHECK WHETHER ACCESS GROUP NUMBER IS USED IN OTHER       *         
* SECURITY ACCESS RECORD KEYS.                                        *         
* RECORD SUB TYPE IN APBYTE, ACCESS GROUP NUMBER IN APHALF.           *         
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
SAAGFRST EQU   SAAGDATA-SAAGREC                                                 
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
LSTLN    DC    AL2(LSTACT2H-LSTACTH)  LIST LINE LENGTH                          
         SPACE 1                                                                
RTYP     DS    0XL4                RECORD TYPE/SUB TYPE                         
         DC    AL1(SAAGTYPQ,SAAGSUBQ)                                           
RTYPL    EQU   *-RTYP                                                           
         SPACE 1                                                                
REPSPEC  DS    0X                                                               
         SPEC  H1,1,RUN                                                         
         SPEC  H1,30,CT#AGRL,30,L                                               
         SPEC  H2,30,CT#AGRL,30,LU                                              
         SPEC  H1,100,AGYNAME                                                   
         SPEC  H2,100,AGYADD                                                    
         SPEC  H3,100,REQUESTOR                                                 
         SPEC  H3,120,PAGE                                                      
         SPEC  H4,100,REPORT                                                    
         SPEC  M1,4,CT#GROUP,8,L                                                
         SPEC  M2,4,CT#GROUP,8,LU                                               
         SPEC  M1,15,CT#NAME,30,L                                               
         SPEC  M2,15,CT#NAME,30,LU                                              
         SPEC  M1,48,CT#MANID,12,L                                              
         SPEC  M2,48,CT#MANID,12,LU                                             
         SPEC  M1,64,CT#STAFF,15,L                                              
         SPEC  M2,64,CT#STAFF,15,LU                                             
         SPEC  END                                                              
         EJECT                                                                  
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSF4D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSD4D                                                       
         ORG   LSTLIN              * LIST LINE LAYOUT *                         
LSTGCDE  DS    CL8                 ACCESS GROUP CODE                            
         DS    CL2                                                              
LSTGNME  DS    CL30                ACCESS GROUP NAME                            
         DS    CL3                                                              
LSTPID   DS    CL8                 MANAGER PERSONAL ID                          
         DS    CL1                                                              
LSTGPCT  DS    CL5                 PERSON COUNT                                 
         SPACE 2                                                                
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSB4D                                                       
         ORG                                                                    
         EJECT                                                                  
MANIDD   DSECT                     ** MANAGER ID FIELD LAYOUT **                
MANPIDH  DS    XL8                                                              
MANPID   DS    CL8                 PERSONAL ID                                  
MANPIDX  DS    XL8                                                              
MANPIDNH DS    XL8                                                              
MANPIDN  DS    CL30                PERSON NAME                                  
MANIDL   EQU   (AGRPI2H-AGRPIDH)                                                
MANNUM   EQU   6                                                                
         SPACE 2                                                                
REPD     DSECT                     ** PRINT LINE LAYOUT **                      
         ORG   REPP1                                                            
         DS    CL3                                                              
LINEGCDE DS    CL8                 ACCESS GROUP CODE                            
         DS    CL3                                                              
LINEGNME DS    CL30                ACCESS GROUP NAME                            
         DS    CL3                                                              
LINEPID  DS    CL8                 MANAGER PERSONAL ID                          
         DS    CL8                                                              
LINEPCT  DS    CL5                 PERSON COUNT                                 
         EJECT                                                                  
WORKD    DSECT                                                                  
         ORG   APLOCAL             ** DSECT TO COVER LOCAL W/S **               
DUB      DS    D                                                                
WORK     DS    XL64                                                             
SAVEKEY  DS    XL(L'SAAGKEY)                                                    
*                                                                               
AGENCYID DS    XL2                 AGENCY ALPHA ID                              
*                                                                               
SELOPT   DS    0X                  LIST SELECT FILTERS                          
SELAGR   DS    XL(L'SAAGAGR)       ACCESS GROUP CODE FILTER                     
SELAGRL  DS    XL1                 AGR FILTER LENGTH                            
SELAGRSP DS    XL1                 FIRST NON SPACE CHAR                         
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'028SEACS0B   11/29/17'                                      
         END                                                                    
