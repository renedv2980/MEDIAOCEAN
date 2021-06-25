*          DATA SET SPSFM2E    AT LEVEL 060 AS OF 08/17/10                      
*PHASE T2172EA                                                                  
**                                                                              
***********************************************************************         
*                                                                     *         
*  TITLE: T2172E - NETWORK SHOW MAINTENANCE                           *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  ***  'NETWORK UPLOAD' SCSPFILSHO SCRIPTS THE MAINT SCREEN!  ***    *         
*                                                                     *         
*  INPUTS: SCREENS SPSFMAF  (T217F0) -- MAINTENANCE                   *         
*          SCREENS SPSFMD0  (T217F1) -- LIST                          *         
*                                                                     *         
*  OUTPUTS: UPDATED NETWORK RECORDS                                   *         
*                                       RES                           *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - DEFINITION RECORD                                     *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T2172E NETWORK SHOW MAINTENANCE'                                
T2172E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2172E*,R7,RR=R3                                              
         ST    R3,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LIST                                                             
         CLI   MODE,RECDEL         BEFORE A DELETE                              
         BE    DELREC                                                           
         CLI   MODE,XRECDEL        AFTER A DELETE                               
         BE    DELDEMO             DELETE DEMO OVERRIDE RECORDS                 
         CLI   MODE,XRECREST       AFTER A RESTORE                              
         BE    RESDEMO             RESTORE DEMO OVERRIDE RECORDS                
EXIT     XIT1                                                                   
         EJECT                                                                  
*******************************************************************             
* SETUP      SETUP (CHECK FOR PF KEYS)                            *             
*******************************************************************             
SETUP    NTR1  ,                                                                
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    SETUPADD                                                         
         XC    SVADDPFK,SVADDPFK                                                
         XC    SVADDKEY,SVADDKEY                                                
         CLI   ACTNUM,ACTLIST                                                   
         BE    SETUPX              NO PFKEYS ON LIST                            
         B     SETUP04                                                          
*                                                                               
* ADD - ONLY ACTION PFKEYS AFTER REC ADDED                                      
* ALLOWS <PF4> RATHER THAN <ENTER><PF4> TO ADD SHOW THEN SWAP TO DEMD           
SETUPADD CLI   MODE,XRECADD                                                     
         BE    SETUPA2                                                          
         CLI   MODE,VALKEY         ADD ALWAYS GETS VALKEY                       
         BNE   SETUPA1                                                          
         CLC   SVADDKEY,KEY        ACTION PFKEY IF JUST ADDED THIS REC          
         BE    SETUP04                                                          
         MVC   SVADDPFK,PFKEY      SAVE PFKEY ACTIVE ON VALKEY                  
         XC    SVADDKEY,SVADDKEY                                                
SETUPA1  XC    PFKEY,PFKEY         IGNORE PFKEYS UNTIL READY TO ACTION          
         B     SETUPX                                                           
SETUPA2  MVC   PFKEY,SVADDPFK      ACTION ANY PENDING PFKEY NOW                 
         XC    SVADDPFK,SVADDPFK   CLEAR PFKEY PENDING                          
         MVC   SVADDKEY,KEY        REMEMBER THIS KEY JUST ADDED                 
*                                                                               
SETUP04  CLI   PFKEY,4                                                          
         BNE   SETUP20                                                          
*                                                                               
         MVC   MPF04ACT,SPACES     SWAP TO SAME ACTION                          
         CLI   ACTNUM,ACTSEL       ACTION SELECT?                               
         BNE   *+10                                                             
         MVC   MPF04ACT,=CL8'DISPLAY'     SWAP TO DISP ACTION                   
*                                                                               
SETUP20  OC    PFKEY,PFKEY                                                      
         BZ    SETUPX                                                           
         GOTO1 INITPFKY,DMCB,MPFTABLE                                           
*                                                                               
SETUPX   B     EXIT                                                             
*******************************************************************             
* DELDEMO    DELETE DEMO OVERRIDE RECORDS WITH DELETED SHOW       *             
*******************************************************************             
DELDEMO  DS    0H                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DOVRECD,R3                                                       
         MVC   DOVKTYP,=X'0D17'                                                 
         MVC   DOVKAGMD,BAGYMD                                                  
         MVC   DOVKNET,NETBCODE                                                 
*                                                                               
         GOTO1 HIGH                                                             
         B     DDOR10                                                           
DDOR05   GOTO1 SEQ                                                              
DDOR10   CLC   KEY(DOVKCLT-DOVKEY),KEYSAVE  ANY MORE TO CHECK?                  
         BNE   DDORX                        NO                                  
*                                                                               
         CLC   DOVKPGM,PGM         CONTAIN PGM JUST DELETED?                    
         BNE   DDOR20              NO                                           
*                                                                               
         OI    KEY+13,X'80'        YES,DELETE DIR                               
         GOTO1 WRITE                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         OI    DOVCNTL,X'80'      DELETE FIL                                    
         GOTO1 PUTREC                                                           
         LA    R3,KEY                                                           
DDOR20   B     DDOR05                                                           
*                                                                               
DDORX    MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R3                                                               
*******************************************************************             
* RESDEMO RESTORE DEMO OVERRIDE RECORDS WITH RESTORED SHOW        *             
*******************************************************************             
RESDEMO  DS    0H                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DOVRECD,R3                                                       
         MVC   DOVKTYP,=X'0D17'                                                 
         MVC   DOVKAGMD,BAGYMD                                                  
         MVC   DOVKNET,NETBCODE                                                 
*                                                                               
         OI    DMINBTS,X'88'       PASS DELETED RECORD(RDUPDATE=C'Y')           
         NI    DMOUTBTS,X'FF'-X'02'   PASS DELETED RECORD                       
*                                                                               
         GOTO1 HIGH                                                             
         B     RDOR10                                                           
RDOR05   GOTO1 SEQ                                                              
RDOR10   CLC   KEY(DOVKCLT-DOVKEY),KEYSAVE  ANY MORE TO CHECK?                  
         BNE   RDORX                        NO                                  
*                                                                               
         CLC   DOVKPGM,PGM          CONTAIN PGM JUST DELETED?                   
         BNE   RDOR20               NO                                          
*                                                                               
         NI    KEY+13,X'FF'-X'80'   YES,RESTORE DIR                             
         GOTO1 WRITE                                                            
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         NI    DOVCNTL,X'FF'-X'80'  RESTORE FIL                                 
         GOTO1 PUTREC                                                           
         LA    R3,KEY                                                           
*                                                                               
RDOR20   OI    DMINBTS,X'88'       PASS DELETED RECORD(RDUPDATE=C'Y')           
         NI    DMOUTBTS,X'FF'-X'02'   PASS DELETED RECORD                       
         B     RDOR05                                                           
*                                                                               
RDORX    MVC   AIO,AIO1                                                         
         OI    DMINBTS,X'FF'-X'08'    TURN OFF PASS DELETED RECORD              
         NI    DMOUTBTS,X'FF'-X'02'   RESET                                     
         B     EXIT                                                             
*******************************************************************             
*                  VALIDATE KEY ROUTINE                           *             
*******************************************************************             
VKEY     DS    0H                                                               
         OI    GENSTAT4,CONFDEL    CONFIRM DELETES                              
         XC    FILTERS,FILTERS     CLEAR ANY LIST FILTERS                       
         LA    R4,KEY                                                           
         USING NPGMRECD,R4                                                      
         XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
         MVC   NPGMKTYP,=X'0D12'    NETWORK RECORD ID                           
         MVC   NPGMKAGY,AGENCY      AGENCY ID                                   
         CLI   ACTNUM,ACTLIST       IS ACTION A LIST?                           
         BE    VKEYLIST                                                         
* MAINT KEY                                                                     
* ENSURE DELETE FIELDS NOT LEFT FROM CANCELLED LIST/DELETE                      
VKEY010  CLI   ACTNUM,ACTDEL                                                    
         BE    VKEY020             LEAVE AS IS FOR DELETE                       
         OI    PRGCONTH+1,X'0C'    HIDE CONFIRM DEL TEXT                        
         OI    PRGCONTH+6,X'80'    AND XMT                                      
         OI    PRGCONFH+1,X'20'    PROT CONFIRM DEL FIELD                       
         MVI   PRGCONFH+8,C' '     CLEAR IT                                     
         MVI   PRGCONFH+5,0                                                     
         OI    PRGCONFH+6,X'80'    AND XMT                                      
*                                                                               
VKEY020  BAS   RE,STAXFLDS         HIDE STAX FIELDS                             
*                                                                               
         LA    R2,PRGNETWH         NETWORK FIELD - REQUIRED                     
         CLI   5(R2),0             ANY DATA?                                    
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
         CLI   5(R2),3             IS LENGTH 3                                  
         BE    *+12                                                             
         CLI   5(R2),4             IS LENGTH 3                                  
         BNE   VKINV               INPUT INVALID                                
         MVC   NPGMKNET,SPACES     BLANK OUT                                    
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPGMKNET(0),PRGNETW    NETWORK FIELD                             
*              SEE IF NETWORK IS IN NETWORK DEF RECORD                          
         MVC   SVKEY,KEY           STORE THE KEY                                
         MVC   AIO,AIO2                                                         
         BAS   RE,VALINET                                                       
         MVC   NETBCODE,BSTA                                                    
*                                                                               
*  VALIDATE PROGRAM(SHOW) DATA - REQUIRED FIELD                                 
         MVC   KEY,SVKEY                                                        
         LA    R2,PRGPRGMH         PROGRAM FIELD - REQUIRED                     
         CLI   5(R2),0             ANY DATA?                                    
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
         CLI   5(R2),4             CHECK LENGTH OF PROGRAM FIELD                
         BH    VKINV                                                            
         MVC   NPGMKID,SPACES     BLANK OUT                                     
         ZIC   R1,5(R2)            LENGTH OF INPUT                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPGMKID(0),PRGPRGM    PROGRAM ID - REQUIRED                      
         MVC   PGM,NPGMKID         SAVE PGM CODE                                
         MVC   SVKEY,KEY           SAVE THE KEY                                 
         MVC   AIO,AIO1                                                         
         GOTO1 MEDGET,DMCB,(=C'N',AGENCY),DATAMGR,WORK                          
         MVC   BAGYMD,WORK                                                      
*                                                                               
* DISPLAYING FOR DELETE - SHOW THE CONFIRMATION FIELDS                          
         CLI   ACTNUM,ACTDEL                                                    
         BNE   VKEY030                                                          
         NI    PRGCONTH+1,X'FF'-X'0C'  SHOW CONFIRM DEL TEXT                    
         OI    PRGCONTH+1,X'08'        HILITE                                   
         OI    PRGCONTH+6,X'80'    AND XMT                                      
         NI    PRGCONFH+1,X'FF'-X'20'  UNPROT CONFIRM DEL FIELD                 
         OI    PRGCONFH+6,X'80'    AND XMT                                      
*                                                                               
VKEY030  EQU   *                                                                
         B     VKEYX                                                            
*                                                                               
* LIST KEY                                                                      
VKEYLIST CLI   LPGNTWKH+5,0        NETWORK IS OPTIONAL                          
         BE    VKEYL05                                                          
         MVC   NPGMKNET,SPACES     SPACE PAD, NOT 0'S                           
         SR    R1,R1               SET NETWORK 'FILTER' TEXT                    
         IC    R1,LPGNTWKH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VKEYL05                                                          
         MVC   NPGMKNET(0),LPGNTWK                                              
VKEYL05  MVC   SVKEY(L'KEY),KEY                                                 
*                                                                               
         CLI   LPGFLTH+FHILD,0                                                  
         BE    VKEYX                                                            
         LA    R2,LPGFLTH                                                       
         BRAS  RE,FILTVAL          VALIDATE FILTERS                             
         BNE   ALLERR                                                           
*                                                                               
VKEYX    B     EXIT                                                             
*                                                                               
VKINV    MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
         EJECT                                                                  
*******************************************************************             
*                         VALIDATE RECORD                         *             
*******************************************************************             
VREC     DS    0H                                                               
*        L     R3,AIO             ADDRESS OF PROGRAM RECORD                     
         MVC   SVKEY,KEY                                                        
         MVI   ELCODE,X'01'       STATION ELEMENT                               
         GOTO1 REMELEM            REMOVES ELEMENT                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING NPGMEL01,R3                                                      
         LA    R2,PRGDESCH        DESCRIPTION FIELD                             
         CLI   5(R2),0            ANY DESCRIPTION INPUT                         
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
*                                                                               
* GET DATA FROM SCREEN FOR 01 ELEMENT                                           
*                                                                               
         MVC   NPGMEL01(2),=X'011D'                                             
         MVC   NPGMPGM,PRGDESC      DESCRIPTION                                 
         LA    R2,PRGDAYH           DAY FIELD                                   
         GOTO1 ANY                                                              
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 DAYVAL,DMCB,((R5),8(R2)),NPGMDAY,WORK2                           
         CLI   NPGMDAY,0                                                        
         BE    VRERR                                                            
         MVC   WORK+1(1),WORK2                                                  
         NI    WORK+1,X'0F'                                                     
         CLI   WORK+1,0                                                         
         BE    VR10                NO END DAY                                   
         IC    R5,WORK2                                                         
         SRL   R5,4                                                             
         STC   R5,WORK                                                          
         CLC   WORK(1),WORK+1                                                   
         BNH   *+10                                                             
         MVC   NPGMOOWR,WORK2      OUT OF WEEK ST/END DAY                       
         BNE   VR10                                                             
         NI    WORK2,X'F0'         IF EQUAL SET OFF END DAY                     
*                                                                               
VR10     DS    0H                                                               
         LA    R2,PRGSTARH                                                      
         GOTO1 ANY                                                              
* CATER FOR MISSING A/P ON START TIME                                           
         SR    RF,RF               LOOK FOR A/P SUFFIX ON START TIME            
         IC    RF,FHILD(R2)                                                     
         CHI   RF,5                                                             
         BNL   VR12                ONLY DEAL WITH 1-4 CHR TIME INPUT            
         LR    R1,RF                                                            
         BCTR  RF,0                                                             
         LA    RF,PRGSTAR(RF)      CHECK LAST CHR                               
         TM    0(RF),X'F0'                                                      
         BNO   VR12                - NOT A DIGIT                                
         SR    RE,RE               SEE IF END TIME HAS A/P                      
         ICM   RE,1,PRGENDH+FHILD                                               
         BZ    VR12                END TIME NOT INPUT YET                       
         BCT   RE,*+8                                                           
         B     VR12                ONLY SINGLE CHR INPUT                        
         LA    RE,PRGEND(RE)                                                    
         CLI   0(RE),C'A'                                                       
         BE    *+12                                                             
         CLI   0(RE),C'P'                                                       
         BNE   VR12                N(OON)/M(IDNIGHT) NOT CATERED FOR            
         MVC   1(1,RF),0(RE)       APPEND A/P SUFFIX TO START TIME              
         AHI   R1,1                INCREMENT L'INPUT                            
         STC   R1,PRGSTARH+FHILD                                                
VR12     BAS   RE,TIMEVAL                                                       
         CLC   DUB(4),=C'NONE'                                                  
         BNE   VR10X                                                            
VRERR    MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
VR10X    MVC   NPGMSTR,DUB                                                      
*                                                                               
VR15     LA    R2,PRGENDH                                                       
         GOTO1 ANY                                                              
         BAS   RE,TIMEVAL                                                       
         CLC   DUB(4),=C'NONE'                                                  
         BE    VRERR                                                            
VR15X    MVC   NPGMEND,DUB                                                      
*                                                                               
*******  NOTE: TIME RESTICTIONS HAVE BEEN LIFTED                                
******** NO SHOW CAN CROSS OVER THE CUT OFF HOUR (6:00AM) ***********           
*        CLC   NPGMSTR,=C'NONE'    IS THERE A TIME SLOT                         
*        BE    VR20                   NO                                        
*        CLC   NPGMSTR(2),NPGMEND  IS START TIME > END TIME?                    
*        BNH   VR17                   NO                                        
*        CLC   NPGMEND(2),CUTOFFHR IS END TIME > CUTOFF HOUR?                   
*        BH    ERRTSLOT               YES                                       
*        B     VR20                   NO                                        
*R17     CLC   NPGMSTR(2),CUTOFFHR IS START TIME >= CUTOFF?                     
*        BNL   VR20                   YES                                       
*        CLC   NPGMEND(2),CUTOFFHR IS END TIME <= CUTOFF?                       
*        BH    ERRTSLOT            INVLD TIME SLOT CROSSES CUTOFF               
*********************************************************************           
*                                                                               
VR20     LA    R2,PRGDPTH                                                       
         GOTO1 ANY                                                              
         BAS   RE,DPTVAL                                                        
         MVC   NPGMDPT,DUB                                                      
*                                                                               
VR25     LA    R2,PRGKILLH                                                      
         XC    NPGMKDAT,NPGMKDAT                                                
         CLI   5(R2),0                                                          
         BE    VREXCP                                                           
         GOTO1 DATVAL,DMCB,(0,PRGKILL),WORK                                     
         OC    DMCB(4),DMCB                                                     
         BZ    VRERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(2,NPGMKDAT)                                
*                                                                               
VREXCP   DS    0H                  EDIT EXCEPTIONS                              
         GOTO1 ADDELEM             ADD THE 01 ELEMENT                           
*                                                                               
         BAS   RE,STAXFLDS                                                      
         TM    PRGSTXH+FHIID,FHIITH    USER INPUT THIS TIME?                    
         BZ    VREX3                                                            
         CLI   PRGSTXH+FHDAD,C'Y'                                               
         BNE   VREX3                                                            
         LA    R2,PRGFSTAH         POSITION CURSOR                              
         MVC   ERRNUM,=AL2(1009)   ENTER REQUIRED DATA                          
         B     MYERR                                                            
*                                                                               
VREX3    MVI   ELCODE,X'05'                                                     
         GOTO1 REMELEM             REMOVE ELEMENTS                              
         XC    ELEM,ELEM                                                        
VREX4    LA    R2,PRGFSTAH                                                      
         LHI   R6,MAXSTEXQ         SET R6 FOR BCT - NUMBER OF STA               
         LA    R3,ELEM                                                          
         USING NPGMEL05,R3                                                      
*              FIRST READ NETWORK DEF RECORD INTO REC2                          
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D11'                                                  
         MVC   KEY+2(6),SVKEY+2    AGY AND NETWORK                              
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
VREX6    DS    0H                                                               
         XC    ELEM(20),ELEM                                                    
         MVC   NPGMEL05(2),=X'050C'                                             
         CLI   5(R2),0                                                          
         BNE   VREX8                                                            
         LA    R2,PRGSTA2H-PRGFSTAH(R2)  TRY NEXT STA FIELD FOR INPUT           
         B     NEXTSTA                                                          
*                                                                               
VREX8    DS    0H                                                               
         CLI   NETSEQ,NDEFCABQ                                                  
         BE    VRERR               CABLE CANNOT HAVE EXCEPTIONS                 
         MVC   KEY(17),ZEROS       ATTEMPT TO READ STATION                      
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'T'          MUST BE TV                                   
         MVC   KEY+2(4),8(R2)      STATION                                      
         OC    KEY+2(4),SPACES                                                  
         MVI   KEY+6,C'T'                                                       
         MVC   KEY+7(2),AGENCY                                                  
         MVC   AIO,AIO3                                                         
         MVC   ELEM2,ELEM                                                       
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
*              NOW BE SURE STATION IS IN NETWORK                                
         MVC   ELEM,ELEM2          PROGRAM ELEMENT                              
         MVC   NPGMSTA,KEY+2                                                    
         BAS   RE,CHKNSTA                                                       
         BE    *+14                                                             
         MVC   ERRNUM,=AL2(246)    STATION NOT IN NETWORK                       
         B     MYERDING                                                         
         MVC   AIO,AIO1            PROGRAM RECORD                               
         LA    R2,PRGFDAYH-PRGFSTAH(R2)  DAY FIELD HEADER                       
         CLI   5(R2),0                                                          
         BE    VREX12              DAY NOT REQUIRED                             
         TM    WORK2,X'0F'         NO DAY ALLOWED IF ROTATOR                    
         BZ    VREX9               WORK2 SAVED FROM DEFAULT DAY                 
         B     VRERR                                                            
*                                                                               
VREX9    DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,8(R2),(R5)                                          
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BE    VREX11                                                           
         B     ERRX                                                             
*                                                                               
VREX11   L     R5,DMCB+4                                                        
         C     R5,=F'700'          7 DAYS MAX                                   
         BH    ERRX                                                             
         C     R5,=F'-700'                                                      
         BL    ERRX                                                             
         CVD   R5,DUB                                                           
         DP    DUB,=P'100'                                                      
         CP    DUB+6(2),=P'0'      MUST GET REMAINDER 0                         
         BNE   ERRX                                                             
         MVC   WORK(6),DUB                                                      
         ZAP   DUB,WORK(6)                                                      
         CVB   R0,DUB                                                           
         STH   R0,NPGMSDAY                                                      
*                                                                               
VREX12   LA    R2,PRGFTIMH-PRGFDAYH(R2)  TIME HEADER                            
         GOTO1 ANY                 REQUIRED                                     
         BAS   RE,TIMEVAL                                                       
         MVC   NPGMSTIM,DUB                                                     
         LA    R2,PRGFDPTH-PRGFTIMH(R2)  DAYPART HEADER                         
         MVC   NPGMSDPT,PRGDPT       SET DEFAULT DAYPART FROM SCREEN            
         CLI   5(R2),0                                                          
         BE    ADDEL                                                            
         BAS   RE,DPTVAL                                                        
         MVC   NPGMSDPT,DUB                                                     
ADDEL    GOTO1 ADDELEM                                                          
         LA    R2,PRGSTA2H-PRGFDPTH(R2)  NEXT STATION HEADER                    
NEXTSTA  BCT   R6,VREX6                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY            RESET KEY TO PROGRAM RECORD                 
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                TO RESET RECORD FOR GENCON PUTREC            
         GOTO1 GETREC              TO RESET RECORD FOR GENCON PUTREC            
         XC    KEY,KEY                                                          
         MVC   KEY,SVKEY            RESET KEY TO PROGRAM RECORD                 
         MVC   AIO,AIO1                                                         
         OI    GENSTAT2,RETEQSEL   REDISPLAY SAME SELECTION                     
         B     DREC                                                             
*                                                                               
         EJECT                                                                  
********************************************************************            
*             DISPLAY THE KEY FOR NETWORK PROGRAM RECORD           *            
********************************************************************            
DKEY     DS    0H                                                               
         L     R4,AIO                                                           
         USING NPGMRECD,R4        NETWORK PROGRAM RECORD                        
         MVC   PRGNETW,NPGMKNET    NETWORK                                      
         OI    PRGNETWH+6,X'80'                                                 
         MVC   PRGPRGM,NPGMKID     PROGRAM ID                                   
         OI    PRGPRGMH+6,X'80'                                                 
         MVC   PGM,NPGMKID         SAVE PGM CODE                                
         DROP  R4                                                               
*------------------------------ ALL VALUES SET IN VKEY SET IN DKEY              
*                                  GET THE AGENCY/MEDIA                         
         GOTO1 MEDGET,DMCB,(=C'N',AGENCY),DATAMGR,WORK                          
         MVC   BAGYMD,WORK                                                      
*                                                                               
         MVC   SVKEY,KEY           STORE THE KEY                                
         MVC   AIO,AIO2                                                         
         LA    R2,PRGNETWH                                                      
         BAS   RE,VALINET                                                       
         MVC   NETBCODE,BSTA                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   KEY,SVKEY            RESTORE THE KEY                             
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
* DISPLAYING FOR DELETE - SHOW THE CONFIRMATION FIELDS                          
         CLI   THISLSEL,C'D'                                                    
         BE    *+12                                                             
         CLI   THISLSEL,DELSELQ                                                 
         BNE   DKEYX                                                            
*                                                                               
         OI    GENSTAT2,RETEQSEL                                                
         NI    PRGCONTH+1,X'FF'-X'0C'  SHOW CONFIRM DEL TEXT                    
         OI    PRGCONTH+1,X'08'        HILITE                                   
         OI    PRGCONTH+6,X'80'    AND XMT                                      
         NI    PRGCONFH+1,X'FF'-X'20'  UNPROT CONFIRM DEL FIELD                 
         OI    PRGCONFH+6,X'80'    AND XMT                                      
*                                                                               
         CLI   PRGCONFH+8,C'Y'                                                  
         BE    DK010                                                            
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSGNO1,16         'RECORD DISPLAYED - CONFIRM DELETE'          
         MVI   GTMSYS,2            SPOT                                         
         DROP  RF                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
DK010    LA    R2,PRGCONFH         POSITION CURSOR                              
         ST    R2,ACURFORC                                                      
DKEYX    BAS   RE,STAXFLDS         HIDE STAX FIELDS                             
         B     EXIT                                                             
         EJECT                                                                  
********************************************************************            
* GET ELEMENT DATA FOR THE NETWORK PROGRAM RECORD AND DISPLAY IT   *            
********************************************************************            
DREC     DS    0H                                                               
         LA    R6,PRGDESCH                                                      
         TWAXC (R6)       CLEAR OUT DATA FIELD                                  
         L     R3,AIO     RECORD AREA                                           
         USING NPGMEL01,R3  NETWORK PROGRAM DESCRIPTION RECORD                  
         MVI   ELCODE,X'01' STATION ELEMENT                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
* GET DATA FOR 01 ELEMENT - ONE OF THEM - FIXED LENGTH                          
         LA    R2,PRGDESCH                                                      
         MVC   PRGDESC,NPGMPGM     PROGRAM DESCRIPTION                          
         OI    PRGDESCH+6,X'80'                                                 
         GOTO1 CALLOV,DMCB,0,X'D9000A0F'  GET DAYUNPK ADDR                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(NPGMOOWR,NPGMDAY),WORK                                
*        GOTO1 UNDAY,DMCB,NPGMDAY,WORK                                          
         FOUT  PRGDAYH,WORK,5                                                   
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),NPGMSTR                                                  
         GOTO1 UNTIME,DMCB,WORK,PRGSTAR                                         
         FOUT  PRGSTARH                                                         
         MVC   WORK(2),NPGMEND                                                  
         GOTO1 UNTIME,DMCB,WORK,PRGEND                                          
         FOUT  PRGENDH                                                          
         FOUT  PRGDPTH,NPGMDPT,1                                                
         GOTO1 DATCON,DMCB,(2,NPGMKDAT),(5,WORK)                                
         FOUT  PRGKILLH,WORK,8                                                  
*                                  FORMAT STATION EXCEPTIONS                    
         BAS   RE,STAXFLDS         SHOW/HIDE STAX FIELDS                        
*                                                                               
DR10     L     R3,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         LHI   R6,30               FOR BCT                                      
         LA    R4,PRGFSTAH                                                      
*                                                                               
DR20     BAS   RE,GETEL                                                         
         BNE   DR100                                                            
         USING NPGMEL05,R3                                                      
DR25     FOUT  (R4),NPGMSTA,4                                                   
         LA    R4,PRGFDAYH-PRGFSTAH(R4)     BUMP TO DAY FIELD                   
         OC    NPGMSDAY,NPGMSDAY                                                
         BZ    DR30                                                             
         EDIT  NPGMSDAY,(2,8(R4)),0,FLOAT=-                                     
DR30     FOUT  (R4)                                                             
         LA    R4,PRGFTIMH-PRGFDAYH(R4)     BUMP TO TIME FIELD                  
         CLC   NPGMSTIM,=C'NO'                                                  
         BNE   *+14                                                             
         MVC   WORK+4(6),=C'NONE  '                                             
         B     DR60                                                             
         MVC   WORK(4),NPGMSTIM                                                 
         XC    WORK+2(20),WORK+2                                                
         GOTO1 UNTIME,DMCB,WORK,WORK+4                                          
*                                                                               
DR60     FOUT  (R4),WORK+4,6                                                    
         LA    R4,PRGFDPTH-PRGFTIMH(R4)     BUMP TO DPT FIELD                   
         FOUT  (R4),NPGMSDPT,1                                                  
         LA    R4,PRGSTA2H-PRGFDPTH(R4)     BUMP TO NEXT STATION FIELD          
         BAS   RE,NEXTEL                                                        
         BE    *+10                                                             
         BCTR  R6,0                                                             
         B     DR100                                                            
         BCT   R6,DR25                                                          
*                                                                               
         BAS   RE,NEXTEL                                                        
         CLI   0(R2),0                                                          
         BE    DR100                                                            
         DC    H'0'                TOO MANY ELEMS                               
*                                                                               
DR100    CLI   ACTNUM,ACTDEL                                                    
         BNE   DRECX                                                            
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMSGNO1,16         'RECORD DISPLAYED - CONFIRM DELETE'          
         MVI   GTMSYS,2            SPOT                                         
         DROP  RF                                                               
         LA    R2,PRGCONFH             POSITION CURSOR                          
         ST    R2,ACURFORC                                                      
         OI    GENSTAT2,USMYOK+USGETTXT                                         
DRECX    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
********************************************************************            
* DELETE RECORD - FORCE USER TO CONFIRM DELETE                     *            
********************************************************************            
DELREC   LA    R2,PRGCONFH                                                      
         CLI   5(R2),0                                                          
         BNE   *+12                                                             
         MVI   ERROR,MISSING                                                    
         B     DELRER                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   DELR010                                                          
         OI    PRGCONTH+1,X'0C'    HIDE CONFIRM DEL TEXT                        
         OI    PRGCONTH+6,X'80'    AND XMT                                      
         OI    PRGCONFH+1,X'20'    PROT CONFIRM DEL FIELD                       
         MVI   PRGCONFH+8,C' '     CLEAR IT (INCASE MULTI SEL)                  
         MVI   PRGCONFH+5,0                                                     
         OI    PRGCONFH+6,X'80'    AND XMT                                      
         LA    R2,PRGNETWH                                                      
         ST    R2,ACURFORC                                                      
         B     DELRX                                                            
*                                                                               
DELR010  CLI   ACTNUM,ACTSEL       ALLOW 'N' TO CANCEL LIST DELETE              
         BNE   *+12                                                             
         CLI   8(R2),C'N'                                                       
         BE    *+12                                                             
         MVI   ERROR,INVALID                                                    
         B     DELRER                                                           
         OI    PRGCONTH+1,X'0C'    HIDE CONFIRM DEL TEXT                        
         OI    PRGCONTH+6,X'80'    AND XMT                                      
         OI    PRGCONFH+1,X'20'    PROT CONFIRM DEL FIELD                       
         MVI   PRGCONFH+8,C' '     CLEAR IT (INCASE MULTI SEL)                  
         MVI   PRGCONFH+5,0                                                     
         OI    PRGCONFH+6,X'80'    AND XMT                                      
         LA    R2,PRGNETWH                                                      
         ST    R2,ACURFORC                                                      
         SR    R1,R1               PREVENT GENCON DOING DELETE                  
         IC    R1,SELLISTN                                                      
         MH    R1,=Y(6)                                                         
         LA    R1,LISTDIR(R1)                                                   
         MVI   0(R1),0             CANCEL THIS SELECT OURSELF                   
         MVC   ERRNUM,=AL2(995)    INFO ERROR - DELETE CANCELLED                
         B     MYERR                                                            
*                                                                               
DELRX    B     EXIT                                                             
*                                                                               
DELRER   CLI   ACTNUM,ACTSEL                                                    
         BNE   DELRERX                                                          
         SR    R1,R1               PREVENT GENCON DOING DELETE                  
         IC    R1,SELLISTN                                                      
         MH    R1,=Y(6)                                                         
         LA    R1,LISTDIR(R1)                                                   
         MVI   0(R1),C'D'          FORCE BACK THROUGH DKEY/DREC                 
DELRERX  B     ERRX                                                             
*                                                                               
*******************************************************************             
*  LIST RECORDS                                                   *             
*******************************************************************             
         SPACE                                                                  
LIST     LA    R6,LISTAR                                                        
         USING LLINED,R6                                                        
         LA    R4,KEY                                                           
         USING NPGMRECD,R4                                                      
         OC    KEY,KEY         TEST FOR 1ST PASS (KEY NULL)                     
         BNZ   LR10            GENCON GOT REC BUT READHI INCASE WAS DEL         
         MVC   KEY,SVKEY       SET INITIAL KEY                                  
LR10     GOTO1 HIGH                                                             
*                                                                               
LR20     CLC   KEY(NPGMKAGY-NPGMRECD+L'NPGMKAGY),SVKEY  REC TYPE + AGY          
         BNE   LREND                                                            
         CLI   LPGNTWKH+5,0     (NOTE: ANY NWK INPUT WAS A START POINT)         
         BE    *+14                                                             
         CLC   NPGMKNET,SVKEY+(NPGMKNET-NPGMRECD)                               
         BNE   LREND                                                            
*                                                                               
         CLI   LPGPGMH+5,0      ANY PROGRAM CHECK                               
         BE    LR30             NO                                              
         SR    R1,R1                                                            
         IC    R1,LPGPGMH+5                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   LPGPGM(0),NPGMKID  COMPARE EXACT LENGTH OF INPUT                 
         BNE   LR50                                                             
*                                                                               
LR30     GOTO1 GETREC                                                           
         L     R3,AIO           NETWORK DEFINITION RECORD                       
*                                                                               
         BRAS  RE,FILTREC                                                       
         BNE   LR50                                                             
*                                                                               
         MVC   LISTAR,SPACES                                                    
*                                                                               
         MVC   LSTPGM,NPGMKID      - PROGRAM                                    
         MVC   LSTNET,NPGMKNET     - NETWORK                                    
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING NPGMEL01,R3                                                      
         MVC   LSTDESC,NPGMPGM     - DESCRIPTION                                
         GOTO1 CALLOV,DMCB,0,X'D9000A0F'  GET DAYUNPK ADDR                      
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(NPGMOOWR,NPGMDAY),WORK                                
*        GOTO1 UNDAY,DMCB,NPGMDAY,WORK                                          
         MVC   LSTDAY,WORK         - DAY(S)                                     
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),NPGMSTR                                                  
         GOTO1 UNTIME,DMCB,WORK,LSTSTIME  - START TIME                          
         MVC   WORK(2),NPGMEND                                                  
         GOTO1 UNTIME,DMCB,WORK,LSTETIME  - END TIME                            
         MVC   LSTDPT,NPGMDPT      - DAYPART                                    
         GOTO1 DATCON,DMCB,(X'42',NPGMKDAT),(5,WORK)                            
         MVC   LSTKILL,WORK        - KILL DATE                                  
         DROP  R3                                                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   LSTEXCPT,C'Y'       INDICATE STATION EXCEPTIONS                  
*                                                                               
         MVC   LSTADD(6),=CL6'*UNKN*'     - DATE ADDED                          
         L     R3,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BAS   RE,GETEL                                                         
         BNE   LR45                                                             
         USING ACTVD,R3                                                         
         GOTO1 DATCON,DMCB,(X'43',ACTVADDT),(6,WORK)  MMM/YY                    
         MVC   LSTADD,WORK                                                      
*        CLI   ACTVCHNM,0                                                       
*        BE    LR45                                                             
*        CLC   ACTVCHDT(2),ACTVADDT                                             
*        BE    *+8                                                              
*        MVI   LSTADD+L'LSTADD,C'*'  CHANGED SINCE ADDED MMM/YY                 
LR45     EQU   *                                                                
*                                                                               
         GOTO1 LISTMON                                                          
LR50     GOTO1 SEQ                                                              
         LA    R4,KEY                                                           
         OC    FILTERS,FILTERS     ANY FILE RECORD FILTERING?                   
         BZ    LR20                                                             
*                                                                               
         GOTO1 GETFACT,DMCB,0      YEP - CHECK FOR MAXIO                        
         L     R1,DMCB                                                          
         USING FACTSD,R1                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,FATMAXIO       MAXIO COUNT IS "SOFT"                        
         MH    RF,=H'9'            SET LIMIT TO 90% OF MAX                      
         D     RE,=F'10'                                                        
         CLM   RF,3,FATIOCNT       COMPARE IT TO CURRENT COUNT OF IO'S          
         BH    LR20                                                             
         DROP  R1                                                               
         L     R1,SYSPARMS         MAXIO HIT                                    
         L     RE,0(R1)                                                         
         MVI   TIOBINDS-TIOBD(RE),TIOBALRM   BEEP                               
* HORRID WAY OF DOING MSGS BUT HOPEFULLY NEVER HIT MAXIO ANYWAY                 
         XC    CONHEAD,CONHEAD                                                  
         LA    R2,LPGNTWKH         GET USER TO INPUT NETWORK                    
         CLI   FHILD(R2),0         UNLESS ONE ALREADY INPUT                     
         BNE   *+14                WHEN GET THEM TO REMOVE FILTER(S)            
         MVC   CONHEAD(37),=C'** THESE FILTERS REQUIRE A NETWORK **'            
         B     *+14                                                             
         LA    R2,LPGFLTH                                                       
         MVC   CONHEAD(27),=C'** PLEASE REDUCE FILTERS **'                      
         GOTO1 ERREX2                                                           
*                                                                               
LREND    B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
*******************************************************************             
* CHKNSTA - ENSURE EXCEPTION STA IS IN NETWORK'S NETDEF RECORD                  
*******************************************************************             
CHKNSTA  NTR1                                                                   
         LR    R4,R3                ADDRESS OF 05 ELEMENT                       
         USING NPGMEL05,R4                                                      
         LR    R5,R2               SAVE FOR CURSOR - ON ERROR                   
         L     R3,AIO2             NETWORK DEFINITON RECORD                     
         MVI   ELCODE,X'01'    STATIONS                                         
         BAS   RE,GETEL                                                         
         CLI   0(R3),0                                                          
         BE    CHKNSERX                                                         
*                                                                               
CHKNS2   CLI   0(R3),X'01'                                                      
         BE    CHKNS4                                                           
CHKNS3   BAS   RE,NEXTEL                                                        
         BNE   CHKNSERX                                                         
CHKNS4   CLC   NPGMSTA,2(R3)                                                    
         BNE   CHKNS3                                                           
*                                                                               
CHKNSOKX CR    RB,RB               FOUND RETURN                                 
         B     *+6                                                              
CHKNSERX LTR   RB,RB                                                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*******************************************************************             
TIMEVAL  NTR1                                                                   
*                                  VALIDATE TIME EXPRESSION AND                 
*                                  RETURN VALUE IN DUB  R2 IS AT FLDHDR         
         CLC   8(4,R2),=C'NONE'                                                 
         BNE   TIMV5                                                            
         MVC   DUB(4),=C'NONE'     ALLOW NONE  - MEANS STATION                  
*                                  DOES NOT SHOW PROGRAM                        
         B     TIMEVX                                                           
*                                                                               
TIMV5    SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R5),8(R2)),DUB                                     
         CLI   DMCB,X'FF'                                                       
         BE    TIMERR                                                           
         CLC   DUB(4),=C'NONE'                                                  
         BE    TIMEVX                                                           
         CLC   DUB+2(2),=2X'00'                                                 
         BNE   TIMERR                                                           
*                                  ONLY ONE EXPRESSION ALLOWED                  
         B     TIMEVX                                                           
*                                                                               
TIMERR   MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
*                                                                               
TIMEVX   B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*******************************************************************             
DPTVAL   NTR1                                                                   
*                                  VALIDATE DAYPART                             
*                                  READ MENU 0                                  
         MVC   DMCB(2),AGENCY                                                   
         MVI   DMCB+2,C'T'                                                      
         MVI   DMCB+3,C'0'                                                      
         L     R5,DATAMGR                                                       
** CHECK??                                                                      
         GOTO1 DPTRD,DMCB,,AIO3,(0(RA),(R5))                                    
         CLI   DMCB+8,X'FF'                                                     
         BNE   DPTV4                                                            
* CHCK IF CORRECT                                                               
         MVI   ERROR,DISKERR       DISK ERROR                                   
         B     ERRX                                                             
*                                                                               
DPTV4    CLI   DMCB+8,X'08'                                                     
         BNE   DPTV6                                                            
DPTVERR  MVI   ERROR,NOTFOUND                                                   
         B     ERRX                                                             
*                                                                               
* CHECK??                                                                       
DPTV6    L     R5,AIO3                                                          
DPTV7    CLI   0(R5),0                                                          
         BNE   *+14                                                             
         MVC   ERRNUM,=AL2(SE#INDPT)                                            
         B     MYERR                                                            
         CLC   0(1,R5),8(R2)                                                    
         BE    DPTVX               DAYPART FOUND                                
         LA    R5,5(R5)                                                         
         B     DPTV7                                                            
*                                                                               
DPTVX    MVC   DUB(1),8(R2)                                                     
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*******************************************************************             
*  VALINET         VALIDATE THE NETWORK                           *             
* ENTRY - R2=A(NETWORK FIELD HDR)                                 *             
* EXIT  - BSTA SET                                                *             
*       - NETSEQ=NETWORK SEQUENCE NUMBER                          *             
*******************************************************************             
VALINET  NTR1  ,                                                                
         OC    8(4,R2),=C'    '                                                 
         LA    R4,KEY                                                           
         USING NDEFRECD,R4                                                      
         XC    KEY,KEY                                                          
         MVC   NDEFKTYP,=X'0D11'                                                
         MVC   NDEFKAGY,AGENCY                                                  
         MVC   NDEFKNET,8(R2)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRNODEF            NETWORK DEF REC NOT FOUND                    
*                                                                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   NETSEQ,NDEFNET-NDEFEL02(R3)     SEQUENCE NUMBER                  
                                                                                
* NEED BSTA FOR DEMDEF RECORD KEY WHEN DELETE/RESTORE SHOW                      
* NOTE: IF SHOWDEF IS 1ST ENQUIRY QMED WILL NOT BE SET BUT ANYWAY               
*       ALWAYS ENSURE SET TO 'N'ETWORK (AS PER DEMDEF/OVER VALIDATION)          
         XC    WORK2,WORK2         FORCE FAKE INPUT                             
         MVC   WORK2(9),=XL9'0900000000010000D5'  N FOR NETWORK                 
         LR    R3,R2                                                            
         LA    R2,WORK2                                                         
         GOTO1 VALIMED             NEED QMED FOR STAVAL !                       
         LR    R2,R3                                                            
         GOTO1 VALISTA             VALIDATE STATION (NEED BSTA)                 
VN10     XC    KEY,KEY                                                          
*                                                                               
VNX      B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SHOW/HIDE STATION EXCEPTION FIELDS ('HARDLY USED & CONFUSE PEOPLE') *         
* CALLED FROM VKEY-MAINT, DKEY-SEL, DREC-MAINT/SEL, VREC-ADD/CHANGE   *         
***********************************************************************         
STAXFLDS NTR1  ,                                                                
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,DMCB                                                          
         TM    FATSTAT6-FACTSD(RF),TST6SCRP                                     
         BNZ   STAXX               DON'T TOUCH FIELDS ON UPLOADS                
*                                                                               
         CLI   NETSEQ,NDEFCABQ     CABLE CANNOT HAVE EXCEPTIONS                 
         BE    STAXHALL            HIDE ALL                                     
*                                                                               
         BAS   RE,STAXSTXT         ENSURE TEXT SHOWN                            
         BAS   RE,STAXUFLD         ENSURE INPUT FIELD UNPROT                    
* VKEY/DKEY CALLS                                                               
         CLI   MODE,VALKEY                                                      
         BNE   STAX05                                                           
         CLI   ACTNUM,ACTCHA                                                    
         BNE   STAXHIDE            INITIALLY HIDE/PROT EXCEPTIONS               
* SPECIAL EXTRA CHECKS FOR CHANGE ACTION AS...                                  
* SELECT FOR DISP, ALTER ACTION TO CHANGE - GETS NO DISPREC!                    
         CLI   TWALACT,ACTSEL      ARE WE IN SELECT MODE?                       
         BNE   STAXHIDE                                                         
         CLC   TWALREC,RECNUM      FOR SAME RECORD TYPE                         
         BNE   STAXHIDE                                                         
         B     STAXX               LEAVE AS SET WHEN DID ORIG DISP              
STAX05   CLI   MODE,DISPKEY                                                     
         BE    STAXHIDE            INITIALLY HIDE/PROT EXCEPTIONS               
* VREC/DREC/(NOTE DREC IS CALLED AFTER VREC)                                    
STAX10   L     R3,AIO              CHECK REC FOR EXCEPTIONS                     
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   STAX20              NONE                                         
*                                  EXCEPTIONS PRESENT IN RECORD                 
         CLI   MODE,VALREC                                                      
         BNE   STAX15                                                           
* SPECIAL EXTRA CHECKS FOR CHANGE ACTION AS...                                  
* SELECT FOR DISP, ALTER ACTION TO CHANGE - GETS NO DISPREC!                    
* DISPLAY, ALTER ACTION TO CHANGE - GETS NO VALKEY OR DISPREC!                  
         CLI   ACTNUM,ACTCHA                                                    
         BE    STAX11                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   STAX15                                                           
* USER MAY HAVE ENTERED 'N' AND CLEARED ALL EXCEPTIONS                          
STAX11   TM    PRGSTXH+FHIID,FHIITH  USER JUST INPUT?                           
         BZ    STAX15                                                           
         CLI   PRGSTXH+FHDAD,C'N'    N INPUT                                    
         BNE   STAX15                                                           
* CHECK THEY ALSO CLEARED ALL EXCEPTION FIELDS                                  
         LA    R2,PRGFSTAH                                                      
         LHI   RF,MAXSTEXQ                                                      
STAX12   CLI   5(R2),0                                                          
         BNE   STAX15                                                           
         CLI   PRGFDAYH+FHILD-PRGFSTAH(R2),0  DAY FIELD HDR                     
         BNE   STAX15                                                           
         CLI   PRGFTIMH+FHILD-PRGFSTAH(R2),0  TIME HDR                          
         BNE   STAX15                                                           
         CLI   PRGFDPTH+FHILD-PRGFSTAH(R2),0  DAYPART HDR                       
         BNE   STAX15                                                           
         LA    R2,PRGSTA2H-PRGFSTAH(R2) TRY NEXT STA FIELDS                     
         BCT   RF,STAX12                                                        
         B     STAXHIDE                                                         
*                                                                               
STAX15   MVI   PRGSTXH+FHDAD,C'Y'  - FAKE Y INPUT                               
         MVI   PRGSTXH+FHILD,1                                                  
         B     STAXSHOW            - SHOW/UNPROT EXCEPTIONS                     
*                                  NO EXCEPTIONS PRESENT IN RECORD              
STAX20   CLI   MODE,VALREC         USER CAN ACTIVATE ON VALREC                  
         BNE   STAX30                                                           
         TM    PRGSTXH+FHIID,FHIITH     USER JUST INPUT?                        
         BZ    STAX30                                                           
* COULD BE DREC AFTER AN ADD/CHANGE                                             
* TEST ACTIVE ALREADY                                                           
         CLI   PRGSTX,C'Y'         TEST USER WANTS TO ACTIVATE                  
         BE    STAXSHOW            - YEP, LET THEM HAVE THEM                    
*                                  CURRENT STATE DETERMINES Y/N                 
STAX30   MVI   PRGSTXH+FHDAD,C'N'  - FAKE N INPUT                               
         TM    PRGFSTAH+FHATD,FHATPR DETERMINE IF ALREADY SHOWN                 
         BNZ   *+8                                                              
         MVI   PRGSTXH+FHDAD,C'Y'  - FAKE Y INPUT                               
         MVI   PRGSTXH+FHILD,1                                                  
         B     STAXX               LEAVE AS IS                                  
*                                                                               
* HIDE ALL FIELDS                                                               
STAXHALL BAS   RE,STAXHTXT         - HIDE TEXT                                  
         BAS   RE,STAXPFLD         - PROT FIELD                                 
*                                                                               
* HIDE STATION EXCEPTION (STAT/DAY/TIME/DPT) FIELDS                             
STAXHIDE OI    PRGXHD1H+FHATD,FHATLO   HEADINGS                                 
         OI    PRGXHD2H+FHATD,FHATLO                                            
         LA    RF,PRGFSTAH                                                      
         LA    RE,PRGLDPTH                                                      
STAXH10  OI    FHATD(RF),FHATPR        PROT INPUT FIELDS                        
         SR    R1,R1                                                            
         IC    R1,FHLND(RF)                                                     
         SHI   R1,FHDAD+1                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FHDAD(0,RF),SPACES      ENSURE CLEAR                             
         MVI   FHILD(RF),0                                                      
         OI    FHOID(RF),FHOITR                                                 
         SR    R1,R1                                                            
         IC    R1,FHLND(RF)                                                     
         AR    RF,R1                                                            
         CR    RF,RE                                                            
         BNH   STAXH10                                                          
         B     STAXTRNS                                                         
* SHOW STATION EXCEPTION (STAT/DAY/TIME/DPT) FIELDS                             
STAXSHOW NI    PRGXHD1H+FHATD,X'FF'-FHATLO    HEADINGS                          
         NI    PRGXHD2H+FHATD,X'FF'-FHATLO                                      
         LA    RF,PRGFSTAH                                                      
         LA    RE,PRGLDPTH                                                      
STAXS10  NI    FHATD(RF),X'FF'-FHATPR  UNPROT INPUT FIELDS                      
         OI    FHOID(RF),FHOITR                                                 
         SR    R1,R1                                                            
         IC    R1,FHLND(RF)                                                     
         AR    RF,R1                                                            
         CR    RF,RE                                                            
         BNH   STAXS10                                                          
STAXTRNS OI    PRGXHD1H+FHOID,FHOITR   TRANSMIT HEADINGS                        
         OI    PRGXHD2H+FHOID,FHOITR                                            
STAXX    B     EXIT                                                             
*                                                                               
* SHOW/HIDE ROUTINES                                                            
STAXHTXT OI    PRGSTXTH+FHATD,FHATLO       HIDE STATION EXCEPTION TEXT          
         B     *+8                                                              
STAXSTXT NI    PRGSTXTH+FHATD,X'FF'-FHATLO SHOW STATION EXCEPTION TEXT          
         OI    PRGSTXTH+FHOID,FHOITR                                            
         BR    RE                                                               
*                                                                               
STAXPFLD OI    PRGSTXH+FHATD,FHATPR       PROT STATION EXCEPTION FIELD          
         XC    PRGSTX,PRGSTX              CLEAR                                 
         MVI   PRGSTXH+FHILD,0                                                  
         B     *+8                                                              
STAXUFLD NI    PRGSTXH+FHATD,X'FF'-FHATPR UNPROT STAT EXCEPTION FIELD           
         OI    PRGSTXH+FHOID,FHOITR                                             
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
*  GETEL                                                                        
*********************************************************************           
         GETEL R3,24,ELCODE                                                     
         EJECT                                                                  
*********************************************************************           
*  ERROR EXITS                                                                  
*********************************************************************           
*                                                                               
ERRTSLOT MVC   ERRNUM,=AL2(INVTSLOT)  ERROR: TIME SLOT CROSSES OVER             
         B     MYERR                  THE CUT OFF TIME                          
*                                                                               
ERRNODEF MVC   ERRNUM,=AL2(INVNODEF)  NETWORK DEF RECORD NOT FOUND              
         B     MYERDING                                                         
*                                                                               
ALLERR   CLI   ERROR,0                SEE IF 'ERROR' MSG SET                    
         BNE   ERRX                   YEP                                       
         B     MYERR                  NOPE - MUST BE 'ERRNUM'                   
*                                                                               
MYERDING L     R1,SYSPARMS                                                      
         L     RE,0(R1)                                                         
         MVI   TIOBINDS-TIOBD(RE),TIOBALRM   GET MESSAGE TO BEEP                
MYERR    OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         DROP  RF                                                               
*                                                                               
ERRX     GOTO1 ERREX                                                            
         EJECT                                                                  
*********************************************************************           
*  TABLES AND CONSTANTS                                                         
*********************************************************************           
         SPACE                                                                  
*  PFKEY TABLE                                                                  
*                                                                               
MPFTABLE DS    0C                  PFKEY TABLE FOR MAINTENANCE SCREENS          
* PF04 = DEMDEF MAINT                                                           
         DC    AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'DEMDEF '        RECORD: DEMDEF                               
MPF04ACT DC    CL8'       '        ACTION: DEFAULT TO CURRENT                   
MPF04    DC    AL1(KEYTYTWA,L'PRGNETW-1),AL2(PRGNETW-T217FFD)                   
         DC    AL1(KEYTYTWA,L'PRGPRGM-1),AL2(PRGPRGM-T217FFD)                   
*   NOTE DEMDEF HANDLES FACT WE DON'T HAVE RTG SVC TO PASS                      
MPF04X   EQU   *                                                                
         DC    X'FF'                                                            
         SPACE                                                                  
CUTOFFHR DC    H'0600'             CUT OFF HOUR IS 6:00AM                       
*                                  NO SHOW CAN CROSS OVER 6:00AM                
ZEROS    DC    20C'0'                                                           
         EJECT                                                                  
*********************************************************************           
* EQUATES AND LITERAL POOL                                                      
*********************************************************************           
         SPACE                                                                  
LEN1     EQU   12                  8 + 4                                        
LEN2     EQU   10                  8 + 2                                        
LEN3     EQU   14                  8 + 6                                        
LEN4     EQU   9                   8 + 1                                        
MAXSTEXQ EQU   30                  MAX #STATION EXCEPTION FIELDS                
* ERROR EQUATES                                                                 
INVTSLOT EQU   410                 TIME SLOT CROSSES CUT OFF                    
INVNODEF EQU   795                 NETWORK DEFINITION REC NOT FOUND             
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* FILTVAL - LIST FILTER VALIDATION                                              
* ENTRY - R2=A(FILTER FLDHDR)                                                   
* EXIT  - NOT EQUAL IF ERROR (MESSAGE SET - ERROR OR ERRNUM)                    
**********************************************************************          
         SPACE                                                                  
FILTVAL  NTR1  BASE=*,LABEL=*                                                   
         MVI   ERROR,0                                                          
         GOTO1 SCANNER,DMCB,(R2),('FILTSMXQ',BLOCK),0                           
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         JZ    FLTVERIX           NO LINE / INVALID INPUT                       
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
FLTVAL05 LA    R4,FILTTAB                                                       
*                                                                               
FLTVAL10 CLI   0(R4),X'FF'                                                      
         JE    FLTVERIX            EOT                                          
         CLI   SC1STLEN,2                                                       
         JL    FLTVERIX            LEN < MIN                                    
         CLI   SC2NDLEN,1                                                       
         JL    FLTVERIX            CURRENTLY ALL ARE KEYWORD=SETTING            
         SR    RF,RF                                                            
         ICM   RF,1,SC1STLEN                                                    
         JZ    FLTVERIX                                                         
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   SC1STFLD(0),1(R4)                                                
         JE    FLTVAL20                                                         
         LA    R4,L'FILTTAB(R4)                                                 
         J     FLTVAL10                                                         
*                                                                               
FLTVAL20 SR    R1,R1                                                            
         IC    R1,0(R4)                                                         
         SHI   R1,1                -1                                           
         SLL   R1,2                *4                                           
         BAS   RE,FLTRTNS(R1)                                                   
         JNE   FLTVERX                                                          
         LA    R3,SCBLKLQ(R3)      NEXT SCANNER ENTRY                           
         JCT   R0,FLTVAL05                                                      
         J     FLTVOKX                                                          
*                                                                               
FLTRTNS  J     FLTVNAME                                                         
         J     FLTVDPT                                                          
         J     FLTVDAY                                                          
         J     FLTVSTIM                                                         
         J     FLTVETIM                                                         
         J     FLTVADTE                                                         
         J     FLTVUDTE                                                         
         DC    H'0'                                                             
*                                                                               
FLTVX2ER MVC   ERRNUM,=AL2(854)                                                 
         J     FLTVERX                                                          
FLTVERIX MVI   ERROR,INVALID                                                    
FLTVERX  LTR   RB,RB                                                            
         J     *+6                                                              
FLTVOKX  CR    RB,RB                                                            
         XIT1  ,                                                                
*                                                                               
* VALIDATE START TIME / END TIME FILTERS                                        
FLTVSTIM NTR1  ,                                                                
         OC    FILTSTIM,FILTSTIM                                                
         JNZ   FLTVX2ER                                                         
         LA    R5,FILTSTIM                                                      
         J     FLTVTIME                                                         
FLTVETIM NTR1  ,                                                                
         OC    FILTETIM,FILTETIM                                                
         JNZ   FLTVX2ER                                                         
         LA    R5,FILTETIM                                                      
FLTVTIME SR    RF,RF                                                            
         IC    RF,SC2NDLEN                                                      
         LR    RE,RF               ALLOW USER TO INPUT AM/PM NOT A/P            
         SHI   RE,2                                                             
         LTR   RE,RE                                                            
         JNP   FLTVTM10                                                         
         LA    RE,SC2NDFLD(RE)                                                  
         CLC   0(2,RE),=C'AM'                                                   
         JE    *+14                                                             
         CLC   0(2,RE),=C'PM'                                                   
         JNE   FLTVTM10                                                         
         SHI   RF,1                REDUCE LEN TO HIDE 'M' FROM TIMVAL           
FLTVTM10 GOTO1 TIMVAL,DMCB,((RF),SC2NDFLD),FULL                                 
         CLI   DMCB,X'FF'                                                       
         JE    FLTVTMER                                                         
         CLC   FULL,=C'NONE'                                                    
         JE    FLTVTMER                                                         
         CLC   FULL+2(2),=2X'00'                                                
         JNE   FLTVTMER                                                         
         MVC   0(2,R5),FULL                                                     
         J     FLTVOKX                                                          
FLTVTMER MVC   ERRNUM,=AL2(48)                                                  
         J     FLTVERX                                                          
*                                                                               
* VALIDATE DAY FILTER (INCL OUT-OF-WEEK ROTATOR)                                
FLTVDAY  NTR1  ,                                                                
         OC    FILTDAY,FILTDAY                                                  
         JNZ   FLTVX2ER                                                         
         GOTO1 DAYVAL,DMCB,(SC2NDLEN,SC2NDFLD),FILTDAY,BYTE                     
         CLI   FILTDAY,0                                                        
         JE    FLTVDYER                                                         
         MVC   HALF+1(1),BYTE                                                   
         NI    HALF+1,X'0F'                                                     
         CLI   HALF+1,0                                                         
         JE    FLTVOKX             NO END DAY                                   
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         SRL   RF,4                                                             
         STC   RF,HALF                                                          
         CLC   HALF(1),HALF+1                                                   
         JNH   *+10                                                             
         MVC   FILTOOWR,BYTE       OUT OF WEEK ST/END DAY                       
         J     FLTVOKX                                                          
FLTVDYER MVC   ERRNUM,=AL2(47)                                                  
         J     FLTVERX                                                          
*                                                                               
* VALIDATE DPT FILTER - SIMPLY ACCEPTS ANY 1 CHR INPUT                          
FLTVDPT  NTR1  ,                                                                
         OC    FILTDPT,FILTDPT                                                  
         JNZ   FLTVX2ER                                                         
         CLI   SC2NDLEN,1                                                       
         JNE   FLTVDPER                                                         
         MVC   FILTDPT,SC2NDFLD    DAYPART MUST BE 1 CHARACTER                  
         J     FLTVOKX                                                          
*                                                                               
FLTVDPER MVC   ERRNUM,=AL2(21)                                                  
         J     FLTVERX                                                          
*                                                                               
* VALIDATE NAME FILTER                                                          
FLTVNAME NTR1  ,                                                                
         OC    FILTNAME,FILTNAME                                                
         JNZ   FLTVX2ER                                                         
         CLI   SC2NDLEN,L'FILTNAME                                              
         JH    FLTVERX                                                          
         MVC   FILTNAME,SC2NDFLD                                                
         MVC   FILTNAML,SC2NDLEN                                                
         LA    RF,FILTNAME         CHECK FOR PUNC EXCLUDED FROM COMPARE         
         SR    RE,RE               (NOTE: FACT BEEN THROUGH SCANNER             
         IC    RE,FILTNAML                NEGATES POSSIBILITY OF ',')           
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         TRT   0(0,RF),TRTNMTAB    R1/R2 CORRUPTED BY TRT!                      
         JZ    FLTVOKX                                                          
         J     FLTVERIX                                                         
*                                                                               
* VALIDATE ADDED DATE FILTER                                                    
FLTVADTE NTR1  ,                                                                
         OC    FILTADTE,FILTADTE                                                
         JNZ   FLTVX2ER                                                         
         LA    R5,FILTADTQ                                                      
         BAS   RE,FLTVDTEQ                                                      
         GOTO1 DATVAL,DMCB,(2,SC2NDFLD),DUB                                     
         OC    DMCB(4),DMCB                                                     
         JZ    FLTVDTER                                                         
         GOTO1 DATCON,DMCB,(0,DUB),(3,FILTADTE)                                 
         CLI   DMCB,X'FF'                                                       
         JE    FLTVDTER                                                         
         J     FLTVOKX                                                          
*                                                                               
* VALIDATE USE UNTIL DATE FILTER                                                
FLTVUDTE NTR1  ,                                                                
         OC    FILTUDTE,FILTUDTE                                                
         JNZ   FLTVX2ER                                                         
         LA    R5,FILTUDTQ                                                      
         BAS   RE,FLTVDTEQ                                                      
         GOTO1 DATVAL,DMCB,(0,SC2NDFLD),DUB                                     
         OC    DMCB(4),DMCB                                                     
         JZ    FLTVDTER                                                         
         GOTO1 DATCON,DMCB,(0,DUB),(2,FILTUDTE)                                 
         CLI   DMCB,X'FF'                                                       
         JNE   FLTVOKX                                                          
FLTVDTER MVC   ERRNUM,=AL2(SE#INDTE)                                            
         J     FLTVERX                                                          
*                                                                               
* SUBROUTINE VALIDATES DATE +/- QUALIFIER                                       
FLTVDTEQ SR    R1,R1                                                            
         IC    R1,SC2NDLEN                                                      
         LR    RF,R1               ALLOW USER TO INPUT +/- QUALIFIER            
         SHI   RF,1                                                             
         LTR   RF,RF                                                            
         BNPR  RE                                                               
         LA    RF,SC2NDFLD(RF)                                                  
         CLI   0(RF),C'+'                                                       
         JE    *+10                                                             
         CLI   0(RF),C'-'                                                       
         BNER  RE                                                               
         MVC   0(1,R5),0(RF)       SAVE +/- QUALIFIER                           
         MVI   0(RF),C' '          HIDE +/- FROM DATVAL                         
         BR    RE                                                               
*                                                                               
* VALIDATION ROUTINE NUMBER / KEYWORD                                           
FILTTAB  DS    0CL6                                                             
         DC    AL1(1),C'NAME '                                                  
         DC    AL1(2),C'DPT  '                                                  
         DC    AL1(3),C'DAY  '                                                  
         DC    AL1(4),C'START'                                                  
         DC    AL1(5),C'END  '                                                  
         DC    AL1(6),C'ADDED'                                                  
         DC    AL1(7),C'UNTIL'                                                  
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3                                                               
**********************************************************************          
* FILTREC - APPLY FILE RECORD FILTERS                                           
* ENTRY - R3=A(RECORD)                                                          
* EXIT  - NOT EQUAL IF RECORD FAILS ANY FILTER                                  
**********************************************************************          
         SPACE                                                                  
         USING NPGMRECD,R3                                                      
FILTREC  NTR1  BASE=*,LABEL=*                                                   
* START TIME                                                                    
         OC    FILTSTIM,FILTSTIM                                                
         JZ    *+14                                                             
         CLC   FILTSTIM,NPGMSTR                                                 
         JNE   FLTRECX                                                          
* END TIME                                                                      
         OC    FILTETIM,FILTETIM                                                
         JZ    *+14                                                             
         CLC   FILTETIM,NPGMEND                                                 
         JNE   FLTRECX                                                          
* DPT                                                                           
         OC    FILTDPT,FILTDPT                                                  
         JZ    *+14                                                             
         CLC   FILTDPT,NPGMDPT                                                  
         JNE   FLTRECX                                                          
* DAY                                                                           
         OC    FILTDAY,FILTDAY                                                  
         JZ    *+14                                                             
         CLC   FILTDAY,NPGMDAY                                                  
         JNE   FLTRECX                                                          
* DAY OUT-OF-WEEK ROTATOR                                                       
         OC    FILTOOWR,FILTOOWR                                                
         JZ    *+14                                                             
         CLC   FILTOOWR,NPGMOOWR                                                
         JNE   FLTRECNX                                                         
* NAME                                                                          
         OC    FILTNAME,FILTNAME                                                
         JZ    FLTR20                                                           
*                                                                               
         MVC   ELEM2(80),SPACES         APPEASE SCANNER                         
         MVC   ELEM2(L'NPGMPGM),NPGMPGM                                         
         OC    ELEM2(L'NPGMPGM),SPACES  NAME NULL PADDED!                       
*                                                                               
         LA    RF,ELEM2                                                         
FLTR10   LHI   RE,L'NPGMPGM-1      REPLACE PUNC WITH COMMAS FOR SCANNER         
         EX    RE,*+8                                                           
         J     *+10                                                             
         TRT   0(0,RF),TRTNMTAB    CHANGE INVALID CHARS TO ","                  
         JZ    *+12                                                             
         MVI   0(R1),C','                                                       
         J     FLTR10                                                           
*                                                                               
         LA    RE,ELEM2+L'NPGMPGM-1                                             
         LA    RF,L'NPGMPGM-1                                                   
*                                                                               
FLTR11   CLI   0(RE),C','          LAST BYTE A COMMA?                           
         BNE   FLTR12              NO - DONE                                    
         MVI   0(RE),C' '          MOVE A SPACE IN                              
         BCTR  RE,0                DECREMENT RE                                 
         BCT   RF,FLTR11           LOOP BACK UNTIL WE HIT REAL CHAR             
*                                                                               
FLTR12   GOTO1 SCANNER,DMCB,(=C'C',ELEM2),('FILTSMXQ',BLOCK),0                  
         SR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         JZ    FLTRECNX            NO LINE / INVALID INPUT                      
         LA    R4,BLOCK                                                         
         USING SCANBLKD,R4                                                      
         SR    R5,R5                                                            
         IC    R5,FILTNAML                                                      
         LA    RE,FILTNAME(R5)                                                  
         SHI   RE,1                POINT TO FINAL CHR                           
         CLI   0(RE),C'*'                                                       
         JNE   FLTR15                                                           
         SHI   R5,1                -1 FOR WILDCARD                              
FLTR15   CLM   R5,1,SC1STLEN                                                    
         JH    FLTR17              LEN < TEXT INPUT                             
         JE    *+12                LEN = TEXT INPUT                             
         CLI   0(RE),C'*'          LEN > TEXT INPUT                             
         JNE   FLTR17              OK IF WILDCARD SEARCHING                     
         LR    RF,R5                                                            
         SHI   RF,1                -1 FOR EX                                    
         EX    RF,*+8                                                           
         J     *+10                                                             
         CLC   FILTNAME(0),SC1STFLD                                             
         JE    FLTR20                                                           
FLTR17   LA    R4,SCBLKLQ(R4)      NEXT SCANNER ENTRY                           
         JCT   R0,FLTR15                                                        
         J     FLTRECNX                                                         
         DROP  R4                                                               
*                                                                               
* ADDED DATE                                                                    
FLTR20   OC    FILTADTE,FILTADTE                                                
         JZ    FLTR30                                                           
         LA    RF,NPGMEL                                                        
         SR    RE,RE                                                            
FLTR22   CLI   0(RF),X'F1'         LOCATE ACTIVITY ELEM                         
         JE    FLTR25                                                           
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         CLI   0(RF),X'00'                                                      
         JNE   FLTR22                                                           
         CLI   FILTADTQ,C'-'       NO ACTV ELEM, PROBABLY VERY OLD              
         JE    FLTR30              OK IF DATE AND BEFORE                        
         J     FLTRECNX                                                         
         USING ACTVD,RF                                                         
FLTR25   CLC   FILTADTE(1),ACTVADDT       YEAR                                  
         JNE   FLTR27                                                           
         CLC   FILTADTE+1(1),ACTVADDT+1   MONTH                                 
         JNE   FLTR27                                                           
         J     FLTR30                                                           
         DROP  RF                                                               
FLTR27   JH    FLTR27A                                                          
         CLI   FILTADTQ,C'+'                                                    
         JNE   FLTRECNX                                                         
         J     FLTR30                                                           
FLTR27A  CLI   FILTADTQ,C'-'                                                    
         JNE   FLTRECNX                                                         
         J     FLTR30                                                           
*                                                                               
* USE UNTIL DATE                                                                
FLTR30   OC    FILTUDTE,FILTUDTE                                                
         JZ    FLTR40                                                           
         CLC   NPGMKDAT,=X'0000'   CHECK FOR ANY DATE SET                       
         JE    FLTRECNX                                                         
         CLC   FILTUDTE,NPGMKDAT                                                
         JE    FLTR40                                                           
         JH    FLTR35                                                           
         CLI   FILTUDTQ,C'+'                                                    
         JNE   FLTRECX                                                          
         J     FLTR40                                                           
FLTR35   CLI   FILTUDTQ,C'-'                                                    
         JNE   FLTRECX                                                          
*                                                                               
FLTR40   EQU   *                                                                
*                                                                               
FLTRECYX CR    RB,RB                                                            
         J     *+6                                                              
FLTRECNX LTR   RB,RB                                                            
FLTRECX  XIT1  ,                                                                
         LTORG                                                                  
         DROP  R3                                                               
* TRANSLATE AND TEST TABLE TO CLEAR PUNC CHRS IN PROG NAME                      
TRTNMTAB DC    256AL1(0)                                                        
*        ORG   TRTNMTAB+C','       NOTE: COMMA LEFT FOR SCANNER!                
*        DC    AL1(255)                                                         
         ORG   TRTNMTAB+C' '                                                    
         DC    AL1(255)                                                         
         ORG   TRTNMTAB+C'='                                                    
         DC    AL1(255)                                                         
         ORG   TRTNMTAB+C'/'                                                    
         DC    AL1(255)                                                         
         ORG   TRTNMTAB+C'.'                                                    
         DC    AL1(255)                                                         
         ORG   TRTNMTAB+C':'                                                    
         DC    AL1(255)                                                         
         ORG   TRTNMTAB+C'-'                                                    
         DC    AL1(255)                                                         
         ORG   TRTNMTAB+C'('                                                    
         DC    AL1(255)                                                         
         ORG   TRTNMTAB+C')'                                                    
         DC    AL1(255)                                                         
         ORG                                                                    
*********************************************************************           
* INCLUDED DSECTS                                                               
*********************************************************************           
         SPACE                                                                  
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE DDFH                                                           
       ++INCLUDE DDSCANBLKD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPOOLD               SPOOL DSECT                             
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENNPGM              NETWORK PROGRAM DSECT                   
       ++INCLUDE SPGENNDEF              NETWORK DEFINITION DSECT                
       ++INCLUDE SPGENNDOV                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE SPMSGEQUS                                                      
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMF0D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMF1D             LIST SCREEN                               
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*********************************************************************           
* OVERLAY STORAGE @ SYSSPARE                                                    
*********************************************************************           
         ORG   SYSSPARE                                                         
ERRNUM   DS    H                                                                
*                                                                               
WORK2    DS    CL48                WORK FOR EDIT                                
ELEM2    DS    CL256               SAVE AREA                                    
NETSEQ   DS    XL1                 NETWORK SEQ NO.                              
NETBCODE DS    XL2                 NETWORK CODE (BINARY VALUE)                  
PGM      DS    CL4                                                              
*                                  FOR PFKEY SPECIAL CODE FOR 'ADD'             
SVADDPFK DS    XL(L'PFKEY)                                                      
SVADDKEY DS    XL(L'KEY)                                                        
*                                  FOR FILTERS                                  
FILTERS  DS    0XL(FILTSLQ)                                                     
FILT1ST  EQU   *                                                                
FILTDAY  DS    XL(L'NPGMDAY)       DAY                                          
FILTOOWR DS    XL(L'NPGMOOWR)      DAY - OUT-OF-WEEK ROTATOR                    
FILTSTIM DS    XL(L'NPGMSTR)       START TIME                                   
FILTETIM DS    XL(L'NPGMEND)       END TIME                                     
FILTDPT  DS    XL(L'NPGMDPT)       DAYPART                                      
FILTNAME DS    XL10                NAME                                         
FILTNAML DS    XL1                 NAME - LENGTH                                
FILTADTE DS    XL(L'ACTVADDT)      ADDED DATE                                   
FILTADTQ DS    XL1                 ADDED DATE +/- QUALIFIER                     
FILTUDTE DS    XL(L'NPGMKDAT)      USE UNTIL (NEE KILL) DATE                    
FILTUDTQ DS    XL1                 UNTIL DATE +/- QUALIFIER                     
FILTSLQ  EQU   *-FILT1ST                                                        
FILTSMXQ EQU   480/SCBLKLQ         MAX #FILTERS CAN FIT SCANNER BLOCK           
*                                                                               
*********************************************************************           
* LOCAL DSECTS                                                                  
*********************************************************************           
*                                                                               
LLINED   DSECT                     ** LIST LINE **                              
LSTNET   DS    CL4                                                              
         DS    CL2                                                              
LSTPGM   DS    CL4                                                              
         DS    CL2                                                              
LSTDESC  DS    CL17                                                             
         DS    CL2                                                              
LSTDAY   DS    CL7                                                              
         DS    CL1                                                              
LSTSTIME DS    CL6                                                              
         DS    CL1                                                              
LSTETIME DS    CL6                                                              
         DS    CL1                                                              
LSTDPT   DS    CL1                                                              
         DS    CL2                                                              
LSTEXCPT DS    CL1                                                              
         DS    CL1                                                              
LSTADD   DS    CL6                                                              
         DS    CL2                                                              
LSTKILL  DS    CL8                                                              
         ORG   LLINED+(L'LPGLIN1-(*-LLINED)) DON'T ASSEMBLE IF OVERFLOW         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060SPSFM2E   08/17/10'                                      
         END                                                                    
