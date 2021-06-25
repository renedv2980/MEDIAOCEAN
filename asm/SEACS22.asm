*          DATA SET SEACS22    AT LEVEL 003 AS OF 09/19/13                      
*PHASE TA0D22B                                                                  
*INCLUDE LISTIO                                                                 
ACS22    TITLE '- SECURITY ACCESS - DATA RECORDS'                               
ACS22    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**CS22**,RA,RR=RE                                              
         USING WORKD,R7            R7=A(GLOBAL W/S)                             
         USING TWAD,R5             R5=A(TWA)                                    
         USING SAVAREA,R6          R6=A(GLOBAL SAVE AREA)                       
         USING SALAREC,R2                                                       
         L     RC,ASYSWORK                                                      
         USING SYSWORK,RC          RC=A(SYSTEM + LOCAL W/S)                     
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
*                                                                               
         CLI   APACTN,ACTADD       ACTION ADD?                                  
         BNE   MAIN10                                                           
         CLI   APPFKEY,0           PFKEY PRESSED?                               
         BE    MAIN10              . NO                                         
         BAS   RE,CHKSCR           CHECK SCREEN FOR CHANGES                     
         BE    MAIN10                                                           
         MVI   APINDS,APIOKADD+APIOKCHA+APIOKDEL+APIOKRES+APIOKDIS              
         B     MODEX10             SWAP BEFORE SCREEN IS PROCESSED              
*                                                                               
MAIN10   XR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *+0(RF)                                                          
*                                                                               
         B     VALKEY         01 - APMVALK                                      
         B     VALREC         02 - APMVALR                                      
         B     DISKEY         03 - APMDISK                                      
         B     DISREC         04 - APMDISR                                      
         B     DELREC         05 - APMDELR                                      
         B     RESREC         06 - APMRESR                                      
         B     VALSEL         07 - APMVALP                                      
         B     GETSEL         08 - APMGETS                                      
         B     DISSEL         09 - APMDISS                                      
         B     XIT            10 - APMVALS                                      
         B     FSTLST         11 - APMFLST                                      
         B     XIT            12 - APMPROC                                      
         B     XIT            13 - APMFSCR                                      
         B     LSTSCR         14 - APMLSCR                                      
         B     XIT            15 - APMVALQ                                      
         B     XIT            16 - APMREPP                                      
         B     XIT            17 - APMSETT                                      
         B     XIT            18 - APMPUTK                                      
         B     XIT            19 - APMNEWK                                      
         B     XIT            20 - APMFRP                                       
         B     XIT            21 - APMDISS2                                     
*                                                                               
XIT      XIT1  ,                                                                
*                                                                               
MODEXIT  CLI   APPFKEY,0                 ANY PFKEY PRESSED?                     
         BE    EXIT                                                             
         CLI   APMODE,APMVALK            END OF VALKEY?                         
         BE    EXIT                                                             
         TM    TWASWPST,TWASWAP          SWAP TO NEW RECORD/ACTION?             
         BZ    EXIT                      . NO                                   
MODEX10  XC    APCURSOR,APCURSOR         DON'T SET CURSOR                       
         MVI   APMODE,APMSWP             SWAP                                   
         MVC   APPARM(1),TWASWPRE        SWAP RECORD                            
         MVC   APPARM+1(1),TWASWPAC      SWAP ACTION                            
         MVC   SVKEYFLD(L'AGRGCDE),AGRGCDE                                      
*                                                                               
EXIT     OI    ACSSRVH+FHOID,FHOIMO      AVOID SCREEN NOT CHANGED               
         CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE KEY OF A DATA GROUP RECORD                                           
***********************************************************************         
VALKEY   CLI   APACTN,ACTGRD                                                    
         BNE   *+12                                                             
         MVI   APINDS,APIOKDIS                                                  
         B     EXIT                                                             
*                                                                               
         LA    R2,IOKEY                                                         
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ          RECORD TYPE                            
         MVI   SALASUB,SALASUBQ          RECORD SUB TYPE                        
         MVC   SALAAGY,CUAALF            AGENCY                                 
*                                                                               
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,AGRGCDEH                                                   
         BNE   VALKEYK                                                          
*                                                                               
         BAS   RE,VALGRP                 VALIDATE AND FORMAT FIELD              
         BNE   SAEIIF                                                           
         MVC   SALAAGR,APWORK                                                   
         MVC   AGRGCDE,APWORK                                                   
         OI    AGRGCDEH+6,FVOXMT                                                
*                                                                               
         MVC   APRECKEY(L'SALAKEY),SALAKEY  SAVE KEY FOR LATER                  
*                                                                               
         LA    R1,IORDD+IOCONFIL+IO1     SETUP FOR I/O                          
         CLI   APACTN,ACTDIS             DON'T LOCK ACTION 'DISPLAY'            
         BE    *+8                                                              
         LA    R1,IOLOCK(R1)                                                    
         GOTO1 AIO                       DO THE I/O                             
         BE    VKEY020                                                          
         BL    VALKEYX                   I/O ERROR EXIT                         
         MVI   APINDS,APIOKADD                                                  
         TM    IOERR,IOEDEL              TEST RECORD IS DELETED                 
         BZ    *+8                                                              
         MVI   APINDS,APIOKDIS+APIOKRES                                         
         B     VALKEYY                                                          
VKEY020  MVI   APINDS,APIOKDIS+APIOKCHA+APIOKDEL                                
*                                                                               
VALKEYY  MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     MODEXIT                                                          
VALKEYK  MVC   FVMSGNO,=AL2(FVFEKEY)     ENTER KEY                              
VALKEYX  B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* ADD OR CHANGE A DATA GROUP RECORD                                             
***********************************************************************         
VALREC   L     R2,AIOAREA1                                                      
*                                                                               
         CLI   APACTN,ACTADD                                                    
         BNE   VR080                                                            
*----------------------------------------                                       
* ADD NEW RECORD - BUILD KEY AND NUMBER ELEMENT                                 
*----------------------------------------                                       
         XC    0(50,R2),0(R2)                                                   
         MVC   SALAKEY,APRECKEY                                                 
         MVI   SALALEN+1,(SALADATA-SALAREC)+1                                   
*                                                                               
         MVC   CURGRP,=XL2'FFFE'         START HERE AND GO BACK                 
*                                                                               
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   SALATYP,SALATYPQ                                                 
         MVI   SALASUB,SALASUBQ                                                 
         MVC   SALAAGY,CUAALF                                                   
*                                                                               
         GOTO1 AIO,IOHIUPD+IOCONFIL+IO2                                         
         BNL   *+6                                                              
         DC    H'0'                                                             
         L     R2,AIOAREA2                                                      
         CLC   SALAKEY(SALAAGN-SALAKEY),IOKEYSAV                                
         BNE   VR070                                                            
         SR    R1,R1                     DECREMENT TO GET NEW NUMBER            
         ICM   R1,3,SALAAGN                                                     
         AHI   R1,-1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,3,CURGRP                                                      
*                                                                               
VR070    L     R2,AIOAREA1                                                      
         MVC   APRECKEY,0(R2)                                                   
         B     VR090                                                            
*----------------------------------------                                       
* CHANGE - GET # AND CLEAR ELEMENTS                                             
*----------------------------------------                                       
VR080    MVI   APELEM,SALANELQ           FIND THE ACCESS GROUP ELEMENT          
         GOTO1 AGETELS,SALAREC                                                  
         ICM   R3,15,APPARM              APPARM =A(ELEMENT) IF FOUND            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SALAND,R3                                                        
         MVC   CURGRP,SALANNUM           SAVE GROUP NUMBER                      
         DROP  R3                                                               
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SALANELQ                                                  
         GOTO1 ADELELS,SALAREC           DELETE GROUP # AND NAME ELEM           
*                                                                               
         MVI   APELEM,SACOMELQ           DELETE COMMENTS ELEMENTS               
         GOTO1 ADELELS,SALAREC                                                  
*                                                                               
         MVI   APELEM,SAMANELQ           DELETE MANAGER ELEMENTS                
         GOTO1 ADELELS,SALAREC                                                  
*----------------------------------------                                       
* ADD/CHANGE - BUILD ELEMENTS                                                   
*----------------------------------------                                       
VR090    XC    APELEM,APELEM                                                    
         LA    R3,APELEM                 SET UP THE TWO ELEMENTS READY          
         USING SALAND,R3                                                        
         MVI   SALANEL,SALANELQ          X'CB'                                  
         MVC   SALANNUM,CURGRP           DATA ACCESS NUMBER                     
         MVC   SALANCOD,SALAAGR          DATA ACCESS CODE                       
*                                                                               
         MVI   FVMINL,1                                                         
         OI    ACVALIND,ACVALIDB                                                
         GOTO1 AFVAL,AGRGNMEH            GROUP NAME                             
         BNE   VALRECX                                                          
*                                                                               
         XR    RF,RF                     MOVE IT INTO ELEMENT                   
         IC    RF,FVXLEN                 RF = LENGTH OF INPUT - 1               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SALANNAM(0),FVIFLD                                               
*                                                                               
         LA    RF,1(RF)                  BUMP RF TO ACTUAL LENGTH               
         LHI   R1,SALANLNQ               RECALCULATE ELEMENT LENGTH             
         AR    R1,RF                                                            
         STC   R1,SALANLN                                                       
*                                                                               
         GOTO1 AADDELS,SALAREC           ADD GROUP NAME ELEMENT                 
*                                                                               
         LA    R0,6                      NUMBER OF COMMENT LINES                
         LA    R1,AGRCOMH                ADDRESS OF FIRST LINE                  
         BAS   RE,BLDCOM                                                        
*                                                                               
         SR    R8,R8                     PROCESS MANAGER ID ELEMENTS            
         LA    R0,MANNUM                                                        
         LA    R4,AGRPIDH                                                       
         USING MANIDD,R4                                                        
VR110    LA    R3,APELEM                 INITIALISE MANAGER ID ELEMENT          
         USING SAMAND,R3                                                        
         XC    SAMANEL(SAMANLNQ),SAMANEL                                        
         MVI   SAMANEL,SAMANELQ                                                 
         MVI   SAMANLN,SAMANLNQ                                                 
         MVI   FVMINL,1                  READ OFFICE NAME FIELD                 
         GOTO1 AFVAL,MANPIDH             READ MANAGER PERSONAL ID               
         BNE   VR120                                                            
         GOTO1 AGETPNUM,FVIFLD                                                  
         BNE   VALRECX                                                          
         MVC   SAMANID,APHALF            SAVE MANAGER ID NUMBER                 
         LA    R8,1(R8)                                                         
         STC   R8,SAMANORD               SAVE MANAGER ORDER NUMBER              
         GOTO1 AADDELS,SALAREC           ADD MANGER ID ELEMENT                  
VR120    LA    R4,MANIDL(R4)                                                    
         BCT   R0,VR110                                                         
*                                                                               
         MVI   APBYTE,CTFILEQ                                                   
         GOTO1 ASETACT,SALAREC           UPDATE ACTIVITY ELEMENT                
*                                                                               
         MVC   IOKEY(L'SALAKEY),APRECKEY  RESTORE GROUP RECORD KEY              
         CLI   APACTN,ACTADD                                                    
         BE    *+12                                                             
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         B     *+8                                                              
         LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                       PERFORM THE I/O                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   APWORK(L'SALAAGR),SALAAGR SAVE CODE FOR AFTER PASSIVE            
*                                                                               
         XC    SALAAGR,SALAAGR           SET KEY FOR NUMBER RECORD              
         MVC   SALAAGN,CURGRP                                                   
         MVC   IOKEY(L'SALAKEY),0(R2)                                           
*                                                                               
         CLI   APACTN,ACTADD                                                    
         BE    VR130                                                            
         GOTO1 AIO,IOREAD+IOCONFIL+IO2   FOR CHANGE, NEED TO READ # REC         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,IOWRITE+IOCONFIL+IO1                                          
         B     *+8                                                              
VR130    LA    R1,IOADD+IOCONFIL+IO1                                            
         GOTO1 AIO                       PERFORM THE I/O                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SALAAGR,APWORK            RESTORE CODE AFTER PASSIVE             
*                                                                               
VROKX    MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     DISREC                                                           
*                                                                               
VALRECX  B     MODEXIT                                                          
         DROP  R3,R4                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY KEY OF AN ACCESS GROUP RECORD                                         
***********************************************************************         
DISKEY   TM    TWASWPST,TWASWAP          PFKEY TO GET HERE?                     
         BZ    DK010                     . NOPE                                 
         MVC   AGRGCDE,SVKEYFLD                                                 
         OI    AGRGCDEH+6,FVOXMT                                                
         NI    TWASWPST,X'FF'-TWASWAP                                           
         LA    R1,ACSACTH                                                       
         ST    R1,APCURSOR                                                      
         B     VALKEY                                                           
*                                                                               
DK010    LA    R2,APRECKEY                                                      
         MVC   AGRGCDE,SPACES                                                   
         MVC   AGRGCDE,SALAAGR                                                  
         OI    AGRGCDEH+6,FVOXMT                                                
DISKEYX  B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY ACCESS GROUP RECORD                              *         
***********************************************************************         
DISREC   OI    ACSSRVH+FHOID,FHOITR+FHOIMO                                      
*                                                                               
         CLI   APACTN,ACTGRD                                                    
         BNE   DR010                                                            
         BRAS  RE,DISGRID                                                       
         B     MODEXIT                                                          
                                                                                
*----------------------------------------                                       
* ACCESS GROUP CODE AND NAME                                                    
*----------------------------------------                                       
DR010    L     R2,AIOAREA1                                                      
         MVC   AGRGCDE,SALAAGR                                                  
         OI    AGRGCDEH+FHOID,FHOITR                                            
*                                                                               
         MVC   AGRGNME,SPACES                                                   
         OI    AGRGNMEH+FHOID,FHOITR                                            
*                                                                               
         LA    R3,SALADATA                                                      
DR014    CLI   0(R3),0                                                          
         BE    DR030                                                            
         CLI   0(R3),SALANELQ                                                   
         BE    DR020                                                            
         LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR014                                                            
*                                                                               
         USING SALAND,R3                                                        
DR020    LA    RE,SALANLNQ               RE = LENGTH OF FIXED PART              
         XR    RF,RF                                                            
         IC    RF,SALANLN                RF = LENGTH OF WHOLE ELEMENT           
         SR    RF,RE                     RF = LENGTH OF VARIABLE PART           
         BCTR  RF,0                      -1 FOR EXECUTE                         
         LTR   RF,RF                     CHECK FOR NULL LENGTH                  
         BM    DR030                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   AGRGNME(0),SALANNAM                                              
         MVC   CURGRP,SALANNUM           SAVE GROUP NUMBER                      
         DROP  R3                                                               
*----------------------------------------                                       
* COMMENT LINES                                                                 
*----------------------------------------                                       
DR030    XC    AGRCOM,AGRCOM                                                    
         XC    AGRCOM2,AGRCOM2                                                  
         XC    AGRCOM3,AGRCOM3                                                  
         XC    AGRCOM4,AGRCOM4                                                  
         XC    AGRCOM5,AGRCOM5                                                  
         XC    AGRCOML,AGRCOML                                                  
*                                                                               
         LA    R1,AGRCOMH                                                       
         LA    R3,SALADATA                                                      
DR040    CLI   0(R3),0                                                          
         BE    DR080                                                            
         CLI   0(R3),SACOMELQ            X'CE'  COMMENT ELEMENT                 
         BE    DR060                                                            
DR050    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR040                                                            
*                                                                               
         USING SACOMD,R3                                                        
DR060    SR    RE,RE                                                            
         IC    RE,SACOMLN                                                       
         LA    RF,SACOMLNQ+1                                                    
         SR    RE,RF                                                            
         BM    DR070                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R1),SACOMDAT                                                 
DR070    OI    FHOID(R1),FHOITR                                                 
         LLC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         B     DR050                                                            
         DROP  R3                                                               
*----------------------------------------                                       
* MANAGER ID ELEMENTS                                                           
*----------------------------------------                                       
DR080    LA    R4,AGRPIDH                CLEAR                                  
         LA    R0,MANNUM                                                        
         USING MANIDD,R4                                                        
DR090    XC    MANPIDN,MANPIDN                                                  
         OI    MANPIDNH+FHOID,FHOITR                                            
         LA    R4,MANIDL(R4)                                                    
         BCT   R0,DR090                                                         
*                                                                               
         LA    R4,AGRPIDH                                                       
         LA    R3,SALADATA                                                      
DR100    CLI   0(R3),0                                                          
         BE    DR150                     END OF RECORD                          
         CLI   0(R3),SAMANELQ                                                   
         BE    DR120                                                            
*                                                                               
DR110    SR    R0,R0                     GET NEXT ELEMENT                       
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DR100                                                            
*                                                                               
         USING SAMAND,R3                                                        
DR120    GOTO1 AGETPID,SAMANID           GET PERSONAL ID                        
         BNE   DR122                                                            
         MVC   MANPID,APWORK                                                    
         GOTO1 AGETPNAM,APWORK           GET PERSON NAME                        
         BNE   DR122                                                            
         MVC   MANPIDN,APWORK            DISPLAY MANAGER NAME                   
         LA    R4,MANIDL(R4)                                                    
         B     DR125                                                            
*                                                                               
DR122    MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   APHALF,=Y(CS#PIDNF)       NAME NOT FOUND MESSAGE                 
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   MANPIDN,APWORK            DISPLAY MANAGER NAME                   
         LA    R4,MANIDL(R4)                                                    
         B     DR110                                                            
                                                                                
DR125    MVC   IOKEY,APRECKEY                                                   
         GOTO1 AIO,IOREAD+IOCONFIL+IO1                                          
         B     DR110                                                            
         DROP  R3,R4                                                            
*                                                                               
DR150    BAS   RE,DISSSH                                                        
         MVI   APBYTE,CTFILEQ                                                   
         GOTO1 ADISACT,SALAREC           DISPLAY ACTIVITY DATE                  
*                                                                               
         LA    R1,AGRGCDEH               SET CURSOR ON SUCCESS DISPLAY          
         ST    R1,FVADDR                                                        
*                                                                               
         B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* DELETE A DATA GROUP RECORD                                                    
***********************************************************************         
DELREC   L     R2,AIOAREA1               CHECK GROUP NAME XREFERENCE            
*                                                                               
         XC    APELEM,APELEM                                                    
         MVI   APELEM,SALANELQ           FIND THE ACCESS GROUP ELEMENT          
         GOTO1 AGETELS,SALAREC                                                  
         L     R3,APPARM                 APPARM =A(ELEMENT) IF FOUND            
         LTR   R3,R3                     ZEROS IF NOT                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING SALAND,R3                                                        
         MVC   CURGRP,SALANNUM                                                  
         DROP  R3                                                               
*                                                                               
         GOTO1 ASETACT,SALAREC                                                  
         OI    SALASTAT,X'80'            SET DELETE FLAG IN RECORD              
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     DELRECX                                                          
*                                                                               
DRECNO   MVC   FVMSGNO,=AL2(FVFXDEL)     CAN'T DELETE MESSAGE                   
         B     DELRECX                                                          
*                                                                               
DELRECX  B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* RESTORE A DELETED ACCESS GROUP RECORD                                         
***********************************************************************         
RESREC   L     R2,AIOAREA1                                                      
*                                                                               
         GOTO1 ASETACT,SALAREC                                                  
         NI    SALASTAT,X'FF'-X'80'      UNSET DELETE                           
         GOTO1 AIO,IOWRITE+IOCONFIL+IO1                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RESRECX  B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE FIRST FOR SCREEN LIST (SET SCREEN TO MODIFIED)    *         
***********************************************************************         
FSTLST   OI    ACSSRVH+FHOID,FHOIMO                                             
         B     MODEXIT                                                          
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE SELECT PARAMETERS & INIT VALUES FOR LIST PROCESS*         
***********************************************************************         
VALSEL   LA    R2,APRECKEY               SET UP RECORD KEY                      
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ          RECORD TYPE                            
         MVI   SALASUB,SALASUBQ          RECORD SUB TYPE                        
         MVC   SALAAGY,CUAALF            AGENCY                                 
         MVC   SALAAGR,SPACES            SKIP THE NUMBER RECORDS                
*                                                                               
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
         GOTO1 AFVAL,LSTKAGRH            GET GROUP FILTER                       
         BNE   VS010                                                            
         MVC   SELDGC,FVIFLD                                                    
         MVC   SELDGCL,FVILEN                                                   
*                                                                               
VS010    LA    R0,LSTACTH                SET INIT VALUES                        
         ST    R0,APPARM                 A(FIRST LIST LINE)                     
         MVC   APPARM+6(L'LSTLN),LSTLN   LENGTH OF LIST LINE                    
         MVI   APPARM+4,(LSTFOOTH-LSTACTH)/(LSTACT2H-LSTACTH)                   
         MVC   FVMSGNO,=AL2(FVFOK)       MESSAGE NO                             
VALSELX  B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* GET NEXT LIST/SEL RECORD                                            *         
***********************************************************************         
GETSEL   L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'SALAKEY),APRECKEY                                        
*                                                                               
         TM    APINDS,APILRERD                                                  
         BZ    *+12                                                             
         NI    APINDS,X'FF'-APILRERD                                            
         B     GS010                                                            
         TM    APINDS,APILNSEQ           IS IT START OF NEW SCREEN?             
         BO    GS020                     NO -> READ SEQ                         
         GOTO1 AIO,IOCONFIL+IOHI+IO1     READ HIGH TO START OFF                 
         BNE   GETSELN                                                          
         B     GS030                                                            
*                                                                               
GS010    GOTO1 AIO,IOCONFIL+IORD+IO1     READ EQUAL TO RESTART                  
         BNE   GETSELN                   & DROP THRU INTO READ SEQ              
*                                                                               
GS020    GOTO1 AIO,IOCONFIL+IOSQ+IO1                                            
         BNE   GETSELN                                                          
*                                                                               
GS030    CLI   SALATYP,SALATYPQ          CORRECT TYPE?                          
         BNE   GETSELN                                                          
         CLI   SALASUB,SALASUBQ          CORRECT SUB TYPE?                      
         BNE   GETSELN                                                          
         CLC   SALAAGY,CUAALF            LOGON AGENCY                           
         BNE   GETSELN                                                          
*                                        FILTER ON GROUP CODE                   
         CLI   SELDGCL,0                                                        
         BE    GSOKX                                                            
         XR    R1,R1                                                            
         IC    R1,SELDGCL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SALAAGR(0),SELDGC                                                
         BNE   GETSELN                   (NO MORE RELEVENT RECORDS)             
*                                                                               
GSOKX    MVC   APRECKEY(L'SALAKEY),SALAKEY                                      
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     GETSELX                                                          
GETSELN  MVI   APMODE,APMEOFS            NO MORE RECORDS                        
*                                                                               
GETSELX  B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY LIST/SELECT LINE                                                      
***********************************************************************         
DISSEL   L     R2,AIOAREA1                                                      
*                                                                               
         L     R4,APPARM           APPARM=A(LINE TO BE FILLED)                  
         USING LSTACTH,R4                                                       
         MVC   LSTGCDE,SALAAGR     FILL IN ACCESS GROUP CODE                    
*                                                                               
         LA    R3,SALADATA         FIND THE GROUP NAME ELEMENT                  
DS030    CLI   0(R3),0                                                          
         BE    DISSELX                                                          
         CLI   0(R3),SALANELQ                                                   
         BE    DS050                                                            
         CLI   0(R3),SAMANELQ                                                   
         BE    DS060                                                            
DS040    XR    RF,RF                                                            
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     DS030                                                            
*                                                                               
         USING SALAND,R3           R3=A(GRP NAME ELEMENT)                       
DS050    MVI   LSTGNME,C' '        CLEAR NAME AND FILL IT IN                    
         MVC   LSTGNME+1(L'LSTGNME-1),LSTGNME                                   
*                                                                               
         CLI   SALANLN,SALANLNQ    IF IT EXISTS                                 
         BE    DS040                                                            
         XR    RF,RF                                                            
         IC    RF,SALANLN          RF=TOTAL LENGTH                              
         LA    RE,SALANLNQ+1       RE=L'FIXED PART (+1 SO THAT...               
         SR    RF,RE               RF=L'NAME-1 FOR THE EXECUTE)                 
         LTR   RF,RF               CHECK FOR NULL LENGTH                        
         BM    DS040                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LSTGNME(0),SALANNAM                                              
         B     DS040                                                            
         DROP  R3                                                               
*                                                                               
         USING SAMAND,R3                                                        
DS060    CLI   SAMANORD,1          ORDER # 1 MANAGER ONLY                       
         BNE   DS040                                                            
         GOTO1 AGETPID,SAMANID     GET PERSONAL ID                              
         BNE   DISSELX                                                          
         MVC   LSTPID,APWORK                                                    
         B     DISSELX                                                          
         DROP  R3                                                               
*                                                                               
DISSELX  B     MODEXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO HANDLE LAST FOR SCREEN (ENABLE PFKEYS)                             
***********************************************************************         
LSTSCR   MVI   APMODE,APMPFKS                                                   
LSTSCRX  B     MODEXIT                                                          
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUILD COMMENT ELEMENTS FROM COMMENT LINES                                     
* R0 - # LINES  R1 - FIELD HEADER  R2 - A(RECORD)                               
***********************************************************************         
BLDCOM   NTR1                                                                   
*                                                                               
         LA    R4,1                                                             
         LA    R3,APELEM                                                        
         USING SACOMD,R3                                                        
BCOM010  XC    SACOMEL(SACOMLNQ),SACOMEL                                        
         MVI   SACOMEL,SACOMELQ                                                 
         MVI   SACOMLN,SACOMLNQ                                                 
         STC   R4,SACOMLIN                                                      
         LR    R8,R1                                                            
         GOTO1 AFVAL                                                            
         BNE   BCOMX                                                            
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
         BCT   R0,BCOM010                                                       
*                                                                               
BCOMX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DISPLAY COMMENT LINES                                                         
***********************************************************************         
DISCOM   NTR1                                                                   
*                                                                               
         LTR   R4,R1                                                            
         BNZ   DCO39                                                            
         LA    R4,FCOMMENT                                                      
         XC    FCOMMENT(256),FCOMMENT                                           
         XC    FCOMMENT+256(L'FCOMMENT-256),FCOMMENT                            
*                                                                               
DCO39    LA    R3,SALADATA                                                      
DCO40    CLI   0(R3),0                                                          
         BE    DC100                                                            
         CLI   0(R3),SACOMELQ            X'CE'  COMMENT ELEMENT                 
         BE    DCO60                                                            
DCO50    LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     DCO40                                                            
*                                                                               
         USING SACOMD,R3                                                        
DCO60    SR    RE,RE                                                            
         IC    RE,SACOMLN                                                       
         LA    RF,SACOMLNQ+1                                                    
*                                                                               
         LTR   R1,R1               IS THIS A SCREEN FIELD?                      
         BZ    *+12                . NO                                         
         OI    FHOID(R4),FHOITR    . YES                                        
         AHI   R4,FHDAD            BUMP TO FIELD VALUE                          
*                                                                               
         SR    RE,RF               ANY COMMENTS?                                
         BM    DCO70               . NO                                         
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SACOMDAT                                                 
*                                                                               
DCO70    LTR   R1,R1               IS THIS A SCREEN FIELD?                      
         BZ    DCO80               . NO                                         
         AHI   R4,-FHDAD           . YES, BACK UP TO FIELD HEADER               
         LLC   RF,1(R4)                                                         
         B     *+8                                                              
DCO80    LA    RF,1(RE)                                                         
         AR    R4,RF                                                            
         B     DCO50                                                            
*                                                                               
DC100    LTR   R1,R1               SCREEN FIELD?                                
         BNZ   XIT                 . YES, EXIT                                  
*                                                                               
         LA    R1,FCOMMENT         FUDGE COMMENT TO MAX ONE BYTE LENGTH         
         SR    R4,R1               FOR GRIDS                                    
         CHI   R4,255                                                           
         BNH   XIT                                                              
         MVC   FCOMMENT+252(3),=C'...'                                          
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ROUTINE TO DISPLAY SYSTEMS SUMMARY HEADER                                     
***********************************************************************         
DISSSH   NTR1                                                                   
*                                                                               
         LA    RE,SYSNAMS          INIT SYSNAMS                                 
         ST    RE,ASYSNAMS                                                      
         MVC   SYSNAMS,SPACES                                                   
         MVI   SYSNAMSL,0                                                       
         MVI   SYSCNT,0                                                         
         MVI   APHALF,C' '                                                      
*                                                                               
         CLI   APACTN,ACTGRD                                                    
         BE    DSSH010                                                          
                                                                                
         MVC   APHALF,=Y(CS#SYSAS)                                              
         GOTO1 ADISTXT,APPARM,APHALF                                            
         MVC   SYSNAMS(16),APWORK                                               
*                                                                               
         LA    RE,SYSNAMS+18       SET A(NEXT ENTRY)                            
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
*                                                                               
DSSH010  XC    APELEM,APELEM                                                    
         MVI   APELEM,SALASELQ     FIND SYSTEM ELEMENTS                         
         GOTO1 AGETELS,SALAREC                                                  
         ICM   R3,15,APPARM        APPARM =A(ELEMENT) IF FOUND                  
         BZ    DSSH090                                                          
*                                                                               
         USING SALASD,R3                                                        
DSSH020  CLI   0(R3),0                                                          
         BE    DSSH080                                                          
         CLI   0(R3),SALASELQ                                                   
         BNE   DSSH080                                                          
*                                                                               
         L     R4,ASYS             SEARCH SE LIST FOR SE NUM SESYSNUM           
         L     R4,VSELIST-SYSFACD(R4)                                           
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)            R3=A(SELIST ENTRY)                           
         USING SELISTD,R4                                                       
         CLC   SALASNUM,SEOVSYS                                                 
         BE    DSSH050                                                          
         BXLE  R4,RE,*-10                                                       
         LA    R4,=CL7'XXX    '    SET UNKNOWN SYSTEM NAME                      
*                                                                               
DSSH050  MVI   APBYTE,C' '                                                      
*                                                                               
         XC    LISTIOB(LISTIOBL),LISTIOB                                        
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURGRP                                                  
         MVC   LISTISYS,SALASNUM                                                
DSSH055  MVI   LISTACTN,LISTANLG   NEXT LIST FOR THIS GROUP                     
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSSH060                                                          
         CLC   LISTISYS,SALASNUM                                                
         BNE   DSSH060                                                          
*                                                                               
         XC    LISTDVAL,LISTDVAL   FIRST ITEM IN LIST                           
         MVI   LISTACTN,LISTANXT   ANY DATA IN LIST                             
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   DSSH055                                                          
         MVI   APBYTE,C'D'                                                      
         B     DSSH055                                                          
*                                                                               
DSSH060  CLI   APBYTE,C'D'                                                      
         BNE   DSSH070                                                          
DSSH062  MVI   APHALF,C'S'                                                      
*                                                                               
         CLC   SELSYS,SALASNUM     DOES THIS MATCH THE SELECT SYSTEM            
         BNE   *+8                                                              
         OI    LOIN,LOINSSFN       SELECT SYSTEM FOUND                          
*                                                                               
         L     RE,ASYSNAMS         MOVE NAME TO LIST                            
         SR    R1,R1                                                            
         ICM   R1,1,SYSCNT         TEST COUNT OF ITEMS IN LIST                  
         BZ    *+12                                                             
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,SYSCNT           BUMP ITEM COUNT                              
         LA    RF,SYSNAMS+L'SYSNAMS-3                                           
         CR    RE,RF                                                            
         BH    DSSH070                                                          
         MVC   0(3,RE),SENAME      EXTRACT SE NAME                              
         NC    1(2,RE),=8X'BF'     SET TO LOWER CASE                            
         LA    RE,3(RE)                                                         
         ST    RE,ASYSNAMS                                                      
         LA    R1,SYSNAMS                                                       
         SR    RE,R1                                                            
         STC   RE,SYSNAMSL         SET LIST LENGTH                              
         DROP  R4                                                               
*                                                                               
DSSH070  LLC   R1,1(R3)            NEXT ELEMENT                                 
         AR    R3,R1                                                            
         B     DSSH020                                                          
         DROP  R3                                                               
*                                                                               
DSSH080  CLI   APHALF,C'S'                                                      
         BE    DSSH100                                                          
*                                                                               
DSSH090  MVC   SYSNAMS(19),=C'<None Assigned>'                                  
         MVI   SYSNAMSL,19                                                      
*                                                                               
DSSH100  CLI   APACTN,ACTGRD       FOR GRIDS DON'T OUTPUT TO SCREEN             
         BE    DSSHX                                                            
         MVI   AGRSSS,C'-'         DISPLAY LIST OF SYSTEMS                      
         MVC   AGRSSS+1(L'AGRSSS-1),AGRSSS                                      
         SR    RF,RF                                                            
         ICM   RF,1,SYSNAMSL       RF=L'SYSTEM NAMES LIST                       
         BZ    DSSH110                                                          
         LA    R1,L'AGRSSS                                                      
         SR    R1,RF                                                            
         BNP   DSSH110                                                          
         SRL   R1,1                                                             
         LA    RE,AGRSSS(R1)                                                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),SYSNAMS                                                  
DSSH110  OI    AGRSSSH+6,X'80'                                                  
*                                                                               
DSSHX    B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* VALIDATE AND FORMAT ALPHANUMERIC GROUP FIELD                                  
*      ENTRY: FVIFLD AND FVILEN                                                 
*      EXIT:  CC=NE IF INVALID, APBYTE AND APWORK=LENGTH AND DATA               
***********************************************************************         
VALGRP   NTR1                                                                   
*                                                                               
         MVI   APBYTE,0                                                         
         MVC   APWORK,SPACES                                                    
*                                                                               
         LA    R1,FVIFLD                                                        
         XR    R2,R2                                                            
         XR    R3,R3                                                            
         XR    R4,R4                                                            
         ICM   R3,1,FVILEN                                                      
         BZ    VGRNEX                                                           
*                                                                               
VGR010   CLI   0(R1),C' '          SKIP LEADING SPACES                          
         BH    VGR015                                                           
         LA    R1,1(R1)                                                         
         BCT   R3,VGR010                                                        
         B     VGRNEX                                                           
*                                                                               
VGR015   LR    R2,R1               FIRST SIGNIFICANT CHARACTER                  
*                                                                               
VGR020   CLI   0(R1),C' '                                                       
         BNH   VGR040                                                           
         CLI   0(R1),C'A'                                                       
         BL    VGRNEX                                                           
         CLI   0(R1),C'Z'                                                       
         BNH   VGR030                                                           
         CLI   0(R1),C'0'                                                       
         BL    VGRNEX                                                           
         CLI   0(R1),C'9'                                                       
         BH    VGRNEX                                                           
*                                                                               
VGR030   LA    R1,1(R1)            NEXT CHARACTER                               
         LA    R4,1(R4)            KEEP TRACK OF LENGTH                         
         BCT   R3,VGR020                                                        
         B     VGR050                                                           
*                                                                               
VGR035   CLI   0(R1),C' '                                                       
         BH    VGRNEX                                                           
VGR040   LA    R1,1(R1)            NEXT CHARACTER                               
         BCT   R3,VGR035                                                        
*                                                                               
VGR050   STC   R4,APBYTE           LENGTH OF DATA                               
         AHI   R4,-1                                                            
         BM    VGRNEX                                                           
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   APWORK,0(R2)        SAVE VALUE                                   
*                                                                               
VGREQX   CR    RB,RB                                                            
         B     XIT                                                              
VGRNEX   LTR   RB,RB                                                            
         B     XIT                                                              
                                                                                
***********************************************************************         
* GRIDS PROCESSING                                                              
***********************************************************************         
DISGRID  NTR1                                                                   
*                                                                               
         XC    SELOPT(SELOPTL),SELOPT                                           
*                                                                               
         L     RF,=A(GCTBL)        GRIDS COLUMN TABLE                           
         A     RF,APRELO                                                        
         ST    RF,AGCTBL                                                        
*                                                                               
         LHI   R3,PCDRIVEN-TWAD    GRIDS INDICATOR AREA IN TWA                  
         A     R3,ATWA                                                          
         USING PCDRIVEN,R3                                                      
*                                                                               
         TM    PCGRIDS,PCGPAPQ     GRIDS IN PROCESS FOR APP?                    
         BO    DG010               . YES                                        
         OI    PCGRIDS,PCGPAPQ     IT IS NOW                                    
         GOTO1 AGRIDS,APPARM,(C'C',AGCTBL),SELFOR  CLEAR GRIDS SCREEN           
         XC    SVSELOPT,SVSELOPT                                                
         XC    SAVLAKEY,SAVLAKEY                                                
         XC    SAVCTF(SAVCTFL),SAVCTF                                           
         MVC   FVMSGNO,=AL2(FVFEKEY)     ENTER KEY                              
         B     DG090               CHANCE TO ENTER FILTERS                      
*                                                                               
DG010    BRAS  RE,VALGSEL          VALIDATE SELECT FIELDS                       
         BNE   DGX                                                              
*                                                                               
DG020    BRAS  RE,GETGSEL          GET THE RECORD                               
         BL    DG040               LIST FINISHED                                
         BH    DG060               TOO MANY I/OS                                
*                                                                               
         CLI   SELFOR,C'C'         COLLAPSE MODE?                               
         BNE   DG030                                                            
         BRAS  RE,PREGRID          PRE-PROCESS GRIDS DATA                       
         BNE   DG020                                                            
         GOTO1 AGRIDS,APPARM,AGCTBL,SELFOR                                      
         BNE   DG070                                                            
         B     DG020                                                            
*                                                                               
DG030    BRAS  RE,GETGSYS          GET SYSTEM FOR GROUP                         
         BNE   DG020               NEXT GROUP                                   
DG032    BRAS  RE,PREGRID          PRE-PROCESS GRIDS DATA                       
         BNE   DG030               GET NEXT SYSTEM                              
DG035    GOTO1 AGRIDS,APPARM,AGCTBL,SELFOR                                      
         BNE   DG070                                                            
         B     DG032                                                            
*                                                                               
DG040    TM    PCGRIDS,PCGCOFQ     OUTPUT COLUMN HEADINGS?                      
         BO    DG050                                                            
         MVC   FVMSGNO,=AL2(CE#NATLS)    NOTHING AVAILABLE TO LIST              
         B     DG090                                                            
DG050    GOTO1 AGRIDS,APPARM,(C'E',AGCTBL),SELFOR    END GRIDS DISPLAY          
         B     DG070                                                            
*                                                                               
DG060    GOTO1 AGRIDS,APPARM,(C'F',AGCTBL),SELFOR    FLUSH THE SCREEN           
*                                                                               
DG070    MVC   SVSYS,CURLSYS       SAVE CURRENT READ VALUES                     
         MVC   SVTYP,CURLTYP                                                    
         MVC   SVDVAL,CURDVAL                                                   
         MVC   SVLNUM,CURLNUM                                                   
         MVC   SVSALAEL,ASALAEL                                                 
*                                                                               
         NI    GETSEQF,X'FF'-APILNSEQ    NO SEQ WHEN WE COME BACK               
         MVC   ACSMSG,FVOMSG                                                    
         MVC   FVMSGNO,=AL2(FVFSET)                                             
*                                                                               
DG090    LA    R1,GRDAGRH          SET CURSOR                                   
         ST    R1,FVADDR                                                        
DGX      B     EXIT                                                             
         DROP  R3                                                               
                                                                                
***********************************************************************         
* ROUTINE TO VALIDATE GRIDS SELECT PARAMETERS                                   
***********************************************************************         
VALGSEL  NTR1                                                                   
*                                                                               
         OI    GRDFORH+6,FVOXMT                                                 
         GOTO1 AFVAL,GRDFORH       EXPAND/COLLAPSE                              
         BNE   VGS002                                                           
         LLC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL12'COLLAPSE'                                        
         BE    VGS002                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),=CL12'EXPAND'                                          
         BNE   SAEIIF                                                           
         MVC   GRDFOR(12),=CL12'EXPAND'                                         
         MVI   SELFOR,C'E'                                                      
         B     VGS004                                                           
VGS002   MVC   GRDFOR,=CL12'COLLAPSE'                                           
         MVI   SELFOR,C'C'                                                      
*                                                                               
VGS004   LA    R2,APRECKEY         SET UP RECORD KEY                            
         XC    SALAKEY,SALAKEY                                                  
         MVI   SALATYP,SALATYPQ    RECORD TYPE                                  
         MVI   SALASUB,SALASUBQ    RECORD SUB TYPE                              
         MVC   SALAAGY,CUAALF      AGENCY                                       
         MVC   SALAAGR,SPACES      SKIP THE NUMBER RECORDS                      
*                                                                               
         GOTO1 AFVAL,GRDAGRH       DATA GROUP                                   
         BNE   VGS010                                                           
         MVC   SELDGC,FVIFLD                                                    
         MVC   SELDGCL,FVILEN                                                   
*                                                                               
VGS010   GOTO1 AFVAL,GRDSYSH                                                    
         BNE   VGS030                                                           
         GOTO1 AVALSYS,GRDSYSH     VALIDATE SYSTEM                              
         BNE   VGSERX                                                           
         MVC   SELSYS,APWORK       HOLD FOR LATER USE                           
*                                                                               
VGS030   GOTO1 AFVAL,GRDTYPH       DATA TYPE FILTER                             
         BNE   VGS040                                                           
         CLI   SELSYS,0            CANNOT USE FILTER WITHOUT SYSTEM             
         BE    SAEDNV                                                           
         CLI   SELFOR,C'C'         CANNOT USE FILTER IN COLLAPSE MODE           
         BE    SAEFNV                                                           
*                                                                               
         XC    LISTIOB(LISTIOBL),LISTIOB                                        
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTISYS,SELSYS                                                  
         MVC   LISTITNM(L'GRDTYP),FVIFLD                                        
         MVI   LISTACTN,LISTATYP   VALIDATE TYPE                                
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   SAEIIF                                                           
         MVC   SELTYP,LISTITYP                                                  
*                                                                               
         MVC   GRDTYP,LISTITNM                                                  
         OI    GRDTYPH+6,FVOXMT                                                 
*                                                                               
VGS040   GOTO1 AFVAL,GRDCODH       DATA ACCESS VALUE FILTER                     
         BNE   VGS050                                                           
         CLI   SELFOR,C'C'         CANNOT USE FILTER IN COLLAPSE MODE           
         BE    SAEFNV                                                           
         LLC   R1,FVXLEN                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SELDVAL(0),FVIFLD                                                
         OC    SELDVAL,SPACES                                                   
         MVC   SELDVALN,FVILEN                                                  
*                                                                               
VGS050   LHI   R3,PCDRIVEN-TWAD                                                 
         A     R3,ATWA                                                          
         USING PCDRIVEN,R3                                                      
*                                                                               
         TM    PCGRIDS,PCGBEGQ                                                  
         BNO   VGS082                                                           
         CLC   SVSELOPT,SELOPT                                                  
         BE    VGS090                                                           
*                                                                               
VGS082   NI    PCGRIDS,X'FF'-PCGBEGQ-PCGCOFQ                                    
         MVC   SVSELOPT,SELOPT                                                  
         XC    SAVCTF(SAVCTFL),SAVCTF                                           
         B     VGS092                                                           
         DROP  R3                                                               
*                                                                               
VGS090   OC    SAVLAKEY,SAVLAKEY                                                
         BNZ   VGSOKX                                                           
VGS092   MVC   SAVLAKEY,APRECKEY                                                
         NI    GETSEQF,X'FF'-APILNSEQ                                           
*                                                                               
VGSOKX   CR    RB,RB                                                            
         J     XIT                                                              
VGSERX   LTR   RB,RB                                                            
         J     XIT                                                              
                                                                                
***********************************************************************         
* GET NEXT RECORD FOR GRIDS                                                     
***********************************************************************         
GETGSEL  NTR1                                                                   
         XC    CURGRPC,CURGRPC           INIT CURRENT GROUP CODE                
         XC    ASALAEL,ASALAEL           CLEAR DISPLACEMENT FOR SYSEL           
*                                                                               
         XC    LISTIOB(LISTIOBL),LISTIOB INIT LIST IO BLOCK                     
         MVC   LISTACOM,ACOM             SET A(COMFACS)                         
*                                                                               
         L     R2,AIOAREA1                                                      
         MVC   IOKEY(L'SALAKEY),SAVLAKEY RESTORE SAVED KEY                      
*                                                                               
         TM    GETSEQF,APILRERD          WAS READ SEQUENCE BROKEN?              
         BZ    GGS010                    . NO                                   
         NI    GETSEQF,X'FF'-APILRERD                                           
         GOTO1 AIO,IOCONFIL+IORD+IO1     . YES, RE-READ RECORD                  
         BNE   GGSLOX                                                           
*                                                                               
GGS010   TM    GETSEQF,APILNSEQ                                                 
         BO    GGS020                                                           
         OI    GETSEQF,APILNSEQ                                                 
         LA    R1,IOCONFIL+IOHI+IO1                                             
         B     *+8                                                              
GGS020   LA    R1,IOCONFIL+IOSQ+IO1                                             
         GOTO1 AIO                                                              
         MVC   SAVLAKEY(L'SALAKEY),SALAKEY                                      
         BNE   GGSLOX                                                           
*                                                                               
         CLI   SALATYP,SALATYPQ    CORRECT TYPE?                                
         BNE   GGSLOX                                                           
         CLI   SALASUB,SALASUBQ    CORRECT SUB TYPE?                            
         BNE   GGSLOX                                                           
         CLC   SALAAGY,CUAALF      AGENCY                                       
         BNE   GGSLOX                                                           
         MVC   CURGRPC,SALAAGR                                                  
*                                                                               
         GOTO1 VGETFACT,APPARM,0   GET A(SYSTEM INFO BLOCK)                     
         L     R1,APPARM                                                        
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,FATMAXIO       MAXIMUM ALLOWABLE IOS                        
         MH    RF,=H'9'                                                         
         D     RE,=F'10'           90 PERCENT OF MAX IOS IN RF                  
         CLM   RF,3,FATIOCNT       TEST RUNNING OUT OF IOS                      
         BNH   GGSHIX                                                           
         DROP  R1                                                               
*                                                                               
         CLI   SELDGCL,0           FILTER ON GROUP CODE                         
         BE    GGS038                                                           
         XR    R1,R1                                                            
         IC    R1,SELDGCL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CURGRPC(0),SELDGC                                                
         BNE   GGS020                                                           
*                                                                               
GGS038   LA    R3,SALADATA                                                      
GGS040   CLI   0(R3),0                                                          
         BE    GGSOKX                                                           
         CLI   0(R3),SALANELQ      DATA GROUP NAME ELEMENT                      
         BE    GGS050                                                           
GGS044   LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GGS040                                                           
*                                                                               
         USING SALAND,R3                                                        
GGS050   MVC   CURGRP,SALANNUM     SAVE GROUP NUMBER                            
         B     GGS044                                                           
         DROP  R3                                                               
*                                                                               
GGSHIX   MVI   APBYTE,2            MAX IO LIMIT                                 
         B     *+8                                                              
GGSLOX   MVI   APBYTE,0            NO MORE RELEVENT RECORDS                     
         B     *+8                                                              
GGSOKX   MVI   APBYTE,1            DISPLAY THIS RECORD                          
         CLI   APBYTE,1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
* GET NEXT SYSTEM FOR GROUP                                                     
***********************************************************************         
GETGSYS  NTR1                                                                   
         L     R2,AIOAREA1                                                      
         MVI   CURSIND,0                                                        
         MVC   CURLSYS,SPACES                                                   
*                                                                               
         OC    SVSYS,SVSYS         CONTINUING FROM PREVIOUS SCREEN              
         BZ    GGSY020             . NO                                         
         MVC   CURLSYS,SVSYS       RESTORE SAVED VALUES FOR CONTINUE            
         MVC   ASALAEL,SVSALAEL                                                 
         XC    SVSYS,SVSYS                                                      
         XC    SVSALAEL,SVSALAEL                                                
         OI    LOIN,LOINCONG       CONTINUING                                   
*                                                                               
GGSY020  LA    R3,SALADATA         FIRST ELEMENT                                
         AH    R3,ASALAEL          DISPLACEMENT TO CURRENT ELEM                 
         TM    LOIN,LOINCONG       ARE WE CONTINUING ON NEW SCREEN?             
         BZ    GGSY040             . NO, GET NEXT SYSTEM ELEMENT                
*                                                                               
GGSY030  CLI   0(R3),0             END OF RECORD - DONE WITH GROUP              
         BE    GGSYNOX                                                          
         CLI   0(R3),SALASELQ      SYSTEM ELEMENT                               
         BE    GGSY050             . PROCESS THIS SYSTEM                        
GGSY040  LLC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     GGSY030                                                          
*                                                                               
         USING SALASD,R3                                                        
GGSY050  MVC   CURLSYS,SALASNUM    SYSTEM NUMBER                                
         MVC   CURSIND,SALASIND    SYSTEM INDICATOR                             
         LA    R1,SALADATA                                                      
         LR    R0,R3                                                            
         SR    R0,R1                                                            
         STH   R0,ASALAEL          SAVE THE DISPLACEMENT TO THIS ELEM           
*                                                                               
         CLI   SELSYS,0            ANY SYSTEM FILTER?                           
         BE    GGSY052                                                          
         CLC   SELSYS,CURLSYS      MAKE SURE IT'S WHAT WE WANT                  
         BNE   GGSY040                                                          
*                                                                               
GGSY052  GOTO1 ADISSYS,CURLSYS     GET SYSTEM NAME                              
         MVC   CURLSYSN,APWORK                                                  
         MVC   CURDVAL,SPACES      INIT LIST DATA VALUEATA VALUE                
         MVC   CURLTYPN,SPACES     INIT TYPE NAME                               
         B     GGSYOKX                                                          
         DROP  R3                                                               
*                                                                               
GGSYNOX  MVI   APBYTE,0            DONE WITH THIS GROUP                         
         B     *+8                                                              
GGSYOKX  MVI   APBYTE,1            DISPLAY THIS RECORD                          
         NI    LOIN,X'FF'-LOINCONG RESET NO GRIDS NEW SCREEN                    
         CLI   APBYTE,1                                                         
         J     XIT                                                              
                                                                                
***********************************************************************         
* PRE-PROCESS GRIDS DATA                                                        
***********************************************************************         
PREGRID  NTR1                                                                   
         L     R2,AIOAREA1         NEED GROUP REC TO GET SYSTEMS                
*                                                                               
         CLI   SELFOR,C'C'         COLLAPSE MODE?                               
         BNE   PG010                                                            
         NI    LOIN,X'FF'-LOINSSFN                                              
         BAS   RE,DISSSH           . NO, DISPLAY SYSTEMS ASSIGNED               
         CLI   SELSYS,0                                                         
         BE    *+12                                                             
         TM    LOIN,LOINSSFN       SELECTED SYSTEM ASSIGNED?                    
         BZ    PGHIX               . NO, EXIT - DONE WITH GROUP                 
         GOTOR DISCOM,0            DISPLAY COMMENTS                             
         B     PGYEX               EXIT - DISPLAY AND COME BACK                 
*                                                                               
PG010    TM    LOIN,LOINMORE                                                    
         BO    PG060                                                            
*                                                                               
         MVC   CURDVAL,SPACES      INIT LIST DATA VALUE                         
         MVC   CURLTYPN,SPACES     INIT TYPE NAME                               
         MVC   CURDERV,SPACES      INIT DERIVED LIST                            
         MVC   CURLTYPN(12),=C'<Company ID>'                                    
*                                                                               
         OC    SVTYP,SVTYP         CONTINUING ON NEW SCREEN?                    
         BNZ   PG015               . NO                                         
         OC    SVLNUM,SVLNUM                                                    
         BNZ   PG015                                                            
         OC    SVDVAL,SVDVAL                                                    
         BZ    PG020                                                            
*                                                                               
PG015    MVC   CURLTYP,SVTYP                                                    
         MVC   CURLNUM,SVLNUM                                                   
         MVC   CURDVAL,SVDVAL                                                   
         XC    SVTYP,SVTYP                                                      
         XC    SVLNUM,SVLNUM                                                    
         XC    SVDVAL,SVDVAL                                                    
         OI    LOIN,LOINCONG                                                    
*                                                                               
PG020    CLI   SELSYS,0            ANY SYSTEM FILTER?                           
         BE    PG052                                                            
         CLC   SELSYS,CURLSYS      MAKE SURE IT'S WHAT WE WANT                  
         BNE   PGHIX               EXIT - DONE WITH SYSTEM                      
*                                                                               
PG052    GOTO1 ADISSYS,CURLSYS     GET SYSTEM NAME                              
         MVC   CURLSYSN,APWORK                                                  
*                                                                               
         MVC   LISTACOM,ACOM                                                    
         MVC   LISTIAGY,CUAALF                                                  
         MVC   LISTIGRP,CURGRP                                                  
         MVC   LISTISYS,CURLSYS                                                 
         XC    LISTITYP,LISTITYP                                                
         XC    LISTDVAL,LISTDVAL                                                
         MVI   LISTACTN,LISTANLG   GET NEXT LIST FOR THIS GROUP                 
*                                                                               
         TM    LOIN,LOINCONG       ARE WE CONTINUING ON NEW SCREEN?             
         BZ    PG055               . NO, GET NEXT SYSTEM ELEMENT                
         MVC   LISTITYP,CURLTYP                                                 
         MVI   LISTACTN,LISTAINF   GET LIST INFO                                
*                                                                               
PG055    GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   PGHIX               EXIT - DONE WITH SYSTEM                      
         CLC   CURLSYS,LISTISYS    MAY BE FOR THE GROUP BUT FOR SYSTEM?         
         BNE   PGHIX               EXIT - DONE WITH SYSTEM                      
*                                                                               
         OC    SELTYP,SELTYP       ANY TYPE FILTER?                             
         BZ    PG058                                                            
         CLC   SELTYP,LISTITYP     MAKE SURE IT'S WHAT WE WANT                  
         BE    PG058                                                            
         MVI   LISTACTN,LISTANLG   GET NEXT LIST FOR THIS GROUP                 
         B     PG055                                                            
*                                                                               
PG058    MVC   CURLNUM,LISTINUM                                                 
         MVC   CURLTYP,LISTITYP                                                 
         MVC   CURDERV,LISTIDER                                                 
         OC    CURDERV,CURDERV                                                  
         BNZ   *+10                                                             
         MVC   CURDERV,SPACES                                                   
         MVI   LISTACTN,LISTATYP   GET TYPE NAME                                
         GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   PGHIX               EXIT - DONE WITH SYSTEM                      
         MVC   CURLTYPN,SPACES                                                  
         MVC   CURLTYPN,LISTITNM                                                
*                                                                               
PG060    NI    LOIN,X'FF'-LOINMORE                                              
         MVC   LISTIGRP,CURGRP                                                  
         MVC   LISTISYS,CURLSYS                                                 
         MVC   LISTITYP,CURLTYP                                                 
         MVC   LISTINUM,CURLNUM                                                 
         MVI   LISTACTN,LISTANXT   NEXT PIECE OF DATA FOR THIS LIST             
*                                                                               
         TM    LOIN,LOINCONG       ARE WE CONTINUING ON NEW SCREEN?             
         BZ    PG065               . NO                                         
         NI    LOIN,X'FF'-LOINCONG RESET NO GRIDS NEW SCREEN                    
         MVC   LISTDVAL,CURDVAL                                                 
         MVI   LISTACTN,LISTAGET   GET LIST DATA                                
*                                                                               
PG065    GOTO1 =V(LISTIO),LISTIOB,RR=APRELO                                     
         BNE   PGHIX                                                            
         CLI   SELDVALN,0                                                       
         BE    PG070                                                            
         LLC   R1,SELDVALN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SELDVAL(0),LISTDVAL                                              
         BNE   PG060                                                            
PG070    MVC   CURDVAL,LISTDVAL    JUST DISPLAY THE DATA                        
         TM    LISTDATT,LDDANOQ    IS THIS AN EXCLUDED VALUE?                   
         BZ    PGYEX               NO                                           
         MVC   TEMPVAL,CURDVAL     YES: DISPLAY WITH A MINUS SIGN               
         MVI   CURDVAL,C'-'                                                     
         MVC   CURDVAL+1(L'CURDVAL-1),TEMPVAL                                   
         B     PGYEX                                                            
*                                                                               
PGHIX    MVI   APBYTE,2                                                         
         B     PGX                                                              
PGLOX    MVI   APBYTE,0                                                         
         B     PGX                                                              
PGYEX    MVI   APBYTE,1            EXIT - DISPLAY AND COME BACK                 
         OI    LOIN,LOINMORE       COME BACK FOR MORE DATA                      
PGX      NI    LOIN,X'FF'-LOINCONG RESET NO GRIDS NEW SCREEN                    
         CLI   APBYTE,1            SET CONDITION CODE                           
         B     XIT                                                              
*                                                                               
***********************************************************************         
* CHECK SCREEN FOR CHANGES                                                      
***********************************************************************         
CHKSCR   NTR1                                                                   
*                                                                               
         LA    R1,AGRGCDEH         GROUP CODE FIELD                             
         LA    RF,AGRCOMLH         LAST COMMENT LINE FIELD                      
*                                                                               
CKS010   CR    R1,RF               DID WE PASS THE LAST FIELD?                  
         BH    CKSNOX              . YES, THEN NOTHING INPUT                    
         LLC   RE,0(R1)                                                         
         SHI   RE,FHDAD+1          SUBTRACT HEADER LENGTH+1 FOR EXECUTE         
         BNM   *+6                                                              
         DC    H'0'                                                             
         TM    FHATD(R1),FHATXH    ANY EXTENDED FIELD HEADER?                   
         BZ    *+14                . NO                                         
         SHI   RE,FHDAD            . YES, SUBTRACT FROM LENGTH                  
         BNM   *+6                                                              
         DC    H'0'                                                             
         TM    FHIID(R1),FHIITH    INPUT THIS TIME?                             
         BZ    *+8                                                              
         B     CKSYEX              YES, SOMETHING WAS INPUT THIS TIME           
         LLC   R0,0(R1)                                                         
         AR    R1,R0                                                            
         B     CKS010                                                           
*                                                                               
CKSYEX   CR    RB,RB               EXIT W/ CC EQUAL                             
         B     XIT                                                              
CKSNOX   LTR   RB,RB               EXIT W/ CC NOT EQUAL                         
         B     XIT                                                              
         EJECT                                                                  
                                                                                
***********************************************************************         
* ERROR EXITS AND CONSTANTS                                                     
***********************************************************************         
SAERNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         J     EXIT                      RECORD NOT FOUND                       
SAEIIF   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         J     EXIT                      INPUT FIELD ERROR                      
SAEDNV   MVC   FVMSGNO,=AL2(CE#DTNVS)                                           
         J     EXIT                      DATA TYPE FILTER NOT VALID             
SAEFNV   MVC   FVMSGNO,=AL2(CE#FNVCF)                                           
         J     EXIT                      FILTER NOT VALID FOR COLLAPSE          
*                                                                               
SPACES   DC    CL80' '                                                          
LSTLN    DC    AL2(LSTACT2H-LSTACTH)     LIST LINE LENGTH                       
*                                                                               
         LTORG                                                                  
                                                                                
***********************************************************************         
* GRIDS COLUMN TABLE - COVERED BY GRIDFD AND GRIDCD                             
***********************************************************************         
         DS    0D                                                               
         DC    CL16'**GCT**GCT**GCT**GCT'                                       
*                                                                               
GCTBL    DC    C'220'                         GRIDS FORMAT ID                   
         DC    AL1(5)                         DOR ADJUSTMENT NUMBER             
         DC    AL2(GRDLIN1-TWAD)              DISP TO FIRST LINE                
         DC    AL2(GRDLINL-TWAD)              DISP TO LAST LINE                 
         DC    AL1(L'GRDLIN1)                 LENGTH OF LINE                    
         DC    AL1(GRDLIN2-GRDLIN1)           DISP BETWEEN LINES                
         DC    AL1(0)                         # OF GRIDS DESC LINES             
         DC    AL1(0)                         N/D                               
                                                                                
*----------------------------------------------------------------------         
* GRIDS DESCRIPTION DATA                                                        
*----------------------------------------------------------------------         
*        DC    AL1(GRLDQ),AL1(0)              DATA GROUP                        
*        DC    AL1(L'GRDAGRN,L'GRDAGR)                                          
*        DC    AL2(GRDAGRN-TWAD,GRDAGR-TWAD)                                    
*        DC    AL2(0)                                                           
*                                                                               
*        DC    AL1(GRLDQ),AL1(0)              SYSTEM                            
*        DC    AL1(L'GRDSYSN,L'GRDSYS)                                          
*        DC    AL2(GRDSYSN-TWAD,GRDSYS-TWAD)                                    
*        DC    AL2(0)                                                           
*                                                                               
*----------------------------------------------------------------------         
* GRIDS COLUMNS                                                                 
*----------------------------------------------------------------------         
         DC    CL3'DG'    * DATA GROUP *      COLUMN ID           +00           
         DC    AL1(GRCTTXT)                   DATA TYPE           +02           
         DC    AL1(0)                         FORMAT OPTIONS      +03           
         DCDDL CT#DATAG,16                    COLUMN NAME         +04           
         DC    AL2(SALAAGR-SALAKEY)           DISP TO DATA        +08           
         DC    AL1(L'SALAAGR)                 LENGTH OF DATA      +10           
         DC    AL1(0)                         DATA INDICATOR      +11           
         DC    AL1(0)                         ELEMENT CODE        +12           
         DC    AL1(0)                         ELEMENT SUB-CODE    +13           
         DC    AL1(0)                         GENERAL INDICATOR   +14           
         DC    AL1(5)                         SPLIT POSITION      +15           
         DC    AL1(0)                         COLUMN SELECTOR     +16           
         DC    AL1(0)                         N/D                 +17           
*                                                                               
         DC    CL3'GN',AL1(GRCTTXT,0)         DATA GROUP NAME                   
         DCDDL CT#DATAN,16                                                      
         DC    AL2(SALANNAM-SALAND),AL1(0,GRCDEVL)                              
         DC    AL1(SALANELQ,0,0,0)                                              
         DC    AL1(0),AL1(0)                                                    
*                                                                               
         DC    CL3'LS',AL1(GRCTTXT,0)         LIST SYSTEM                       
         DCDDL CT#SYS,16                                                        
         DC    AL2(CURLSYSN-WORKD),AL1(L'CURLSYSN,GRCDWS)                       
         DC    AL1(0,0,0,0)                                                     
         DC    C'E',AL1(0)                                                      
*                                                                               
         DC    CL3'DR',AL1(GRCTTXT,0)         DERIVED FROM LIST                 
         DCDDL CT#DERI,16                                                       
         DC    AL2(CURDERV-WORKD),AL1(L'CURDERV,GRCDWS)                         
         DC    AL1(0,0,0,0)                                                     
         DC    C'E',AL1(0)                                                      
*                                                                               
         DC    CL3'LT',AL1(GRCTTXT,0)         DATA TYPE                         
         DCDDL CT#DATAT,16                                                      
         DC    AL2(CURLTYPN-WORKD),AL1(L'CURLTYPN,GRCDWS)                       
         DC    AL1(0,0,0,0)                                                     
         DC    C'E',AL1(0)                                                      
*                                                                               
         DC    CL3'LD',AL1(GRCTTXT,0)         CODES                             
         DCDDL CT#DATAS,16                                                      
         DC    AL2(CURDVAL-WORKD),AL1(L'CURDVAL,GRCDWS)                         
         DC    AL1(0,0,0,0)                                                     
         DC    C'E',AL1(0)                                                      
*                                                                               
         DC    CL3'SN',AL1(GRCTTXT,0)         SYSTEMS ASSIGNED                  
         DCDDL CT#SYSTS,16                                                      
         DC    AL2(SYSNAMS-WORKD),AL1(L'SYSNAMS,GRCDWS)                         
         DC    AL1(0,0,0,0)                                                     
         DC    C'C',AL1(0)                                                      
*                                                                               
         DC    CL3'CO',AL1(GRCTTXT,GRCFSIZ)   COMMENTS                          
         DCDDL CT#CMTS,16                                                       
         DC    AL2(FCOMMENT-WORKD),AL1(255,GRCDWS)                              
         DC    AL1(0,0,0,0)                                                     
         DC    C'C',AL1(0)                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
***********************************************************************         
* SECURITY ACCESS INCLUDES                                                      
***********************************************************************         
* SEACSWRK                                                                      
       ++INCLUDE SEACSWRK                                                       
         EJECT                                                                  
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
                                                                                
***********************************************************************         
* TWA                                                                           
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA7D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA4D                                                       
         ORG   ACSTABH                                                          
       ++INCLUDE SEACSA8D                                                       
         ORG   LSTLIN              * LIST LINE LAYOUT *                         
LSTGCDE  DS    CL8                 ACCESS GROUP CODE                            
         DS    CL2                                                              
LSTGNME  DS    CL30                ACCESS GROUP NAME                            
         DS    CL3                                                              
LSTPID   DS    CL8                 MANAGER PERSONAL ID                          
         DS    CL1                                                              
LSTGPCT  DS    CL5                 PERSON COUNT                                 
                                                                                
         ORG   SAVOVER                                                          
SAVLAKEY DS    CL(L'SALAKEY)       SAVE DATA GROUP KEY                          
*                                                                               
SAVCTF   DS    0X                  SAVE GRIDS CONTINUE FIELDS                   
SVSYS    DS    XL(L'CURLSYS)                                                    
SVTYP    DS    XL(L'CURLTYP)                                                    
SVDVAL   DS    XL(L'CURDVAL)                                                    
SVLNUM   DS    XL(L'CURLNUM)                                                    
SVSALAEL DS    XL(L'ASALAEL)                                                    
SAVCTFL  EQU    *-SAVCTF                                                        
*                                                                               
SVSELOPT DS    CL(SELOPTL)         SAVED OPTIONS                                
         EJECT                                                                  
                                                                                
***********************************************************************         
* MANAGER ID FIELD LAYOUT                                                       
***********************************************************************         
MANIDD   DSECT                                                                  
MANPIDH  DS    XL8                                                              
MANPID   DS    CL8                 PERSONAL ID                                  
MANPIDX  DS    XL8                                                              
MANPIDNH DS    XL8                                                              
MANPIDN  DS    CL30                PERSON NAME                                  
MANIDL   EQU   (AGRPI2H-AGRPIDH)                                                
MANNUM   EQU   6                                                                
*                                                                               
***********************************************************************         
* PRINT LINE LAYOUT                                                             
***********************************************************************         
REPD     DSECT                                                                  
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
                                                                                
***********************************************************************         
* DSECT TO COVER LOCAL W/S **                                                   
***********************************************************************         
WORKD    DSECT                                                                  
         ORG   APLOCAL                                                          
DUB      DS    D                                                                
ASYSNAMS DS    A                   A(SYSTEM NAME LIST)                          
AGCTBL   DS    A                   A(GRIDS CONTROL TABLE)                       
*                                                                               
LOWORK   DS    XL64                                                             
GETSEQF  DS    X                   GRID READ SEQUENCE INDICATOR                 
*                                                                               
CURGRP   DS    XL2                 DATA GROUP NUMBER                            
CURGRPC  DS    CL8                 DATA GROUP CODE                              
CURSIND  DS    X                   SYSTEM ELEMENT INDICATOR                     
CURLNUM  DS    XL4                 LIST NUMBER                                  
CURLSYS  DS    X                   LIST SYSTEM NUMBER                           
CURLSYSN DS    CL7                 LIST SYSTEM NAME                             
CURLTYP  DS    XL2                 LIST TYPE                                    
CURLTYPN DS    CL8                 LIST TYPE NAME                               
CURDERV  DS    CL4                 LIST DERIVED FROM VALUE                      
CURDVAL  DS    CL24                DATA VALUE                                   
*                                                                               
SELOPT   DS    0X                  LIST SELECT FILTERS                          
SELDGC   DS    XL(L'SALAAGR)       ACCESS GROUP CODE FILTER                     
SELDGCL  DS    XL1                 AGR FILTER LENGTH                            
SELSYS   DS    XL1                 SYSTEM                                       
SELTYP   DS    XL2                 DATA TYPE NUMBER                             
SELLIN   DS    X                   LIST INDICATOR                               
SELDVAL  DS    CL24                DATA VALUE                                   
SELDVALN DS    X                   LENGTH OF DATA VALUE                         
SELFOR   DS    XL1                 FORMAT EXPAND/COLLAPSE (E/C)                 
SELDGCSP DS    XL1                 FIRST NON SPACE CHAR                         
SELKEYCL DS    XL1                 KEY COMPARE LENGTH                           
SELOPTL  EQU   *-SELOPT                                                         
*                                                                               
TEMPVAL  DS    CL24                TEMPORARY DATA VALUE                         
*                                                                               
LOIN     DS    X                   LOCAL INDICATOR                              
LOINMORE EQU   X'80'               . MORE DATA IN LIST                          
LOINSSFN EQU   X'20'               . SELECTED SYSTEM FOUND FOR GROUP            
LOINCONG EQU   X'08'               . CONTINUING GRIDS ON A NEW SCREEN           
*                                                                               
ASALAEL  DS    H                   DISPLACEMENT TO SALASYS ELEM IN REC          
*                                                                               
SYSNAMS  DS    CL78                                                             
SYSNAMSL DS    XL1                                                              
SYSCNT   DS    X                                                                
FCOMMENT DS    XL450               FULL COMMENTS                                
*                                                                               
       ++INCLUDE DDLISTD                                                        
*                                                                               
DATACODS DS    XL200                                                            
*                                                                               
LOCALX   EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SEACS22   09/19/13'                                      
         END                                                                    
