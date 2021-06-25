*          DATA SET ACCAP00    AT LEVEL 109 AS OF 03/26/12                      
*PHASE T61D00A                                                                  
*INCLUDE BMONVAL                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE RECUP                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
         TITLE 'T61D00 - COST ALLOCATION PROGRAM CONTROLLER'                    
***********************************************************************         
* CONTROLLER MODULE                                                   *         
*                                                                     *         
* HISTORY                                                             *         
* -------                                                             *         
*JSHA 096 120601-ADDED GENSTAT6 BYTE TO DDSPLWORKD.                   *         
*                CHANGED COST CONTROLLER (ACCAP00) TO TURN ON         *         
*                THE GESECOVR BIT IN GENSTAT6 SO GENCON KNOWS         *         
*                THAT A SECURITY OVERRIDE IS IN EFFECT.  CHANGED      *         
*                THE SECMASKS ON THE RECACT1 AND RECACT3 TABLES       *         
*                TO INCLUDE THE OVERRIDES IN THE 1ST POSITION.        *         
*                THESE TABLES ARE ALSO PASSED TO GENCON.  THIS        *         
*                NEW CODE CONFLICTS WITH THE CURRENT USES OF THE      *         
*                SECMASKS SO THEY CANNOT BE USED TOGETHER.            *         
*JSHA 097 012302-ADDED DELETE ACTION TO THEADER RECORD AND            *         
*                    REMOVED DELETE ACTION FROM TIME                  *         
*JFOS 103 070103-UNPROTECT OPTIONS FLD. TIME/ADD LEAVES IT PROTECTED  *         
*JFOS 103 290905-FIX BUG A(IO2) GETTING OVERWRITTEN                   *         
*JFOS 103 220906-<UKCR7999> NEW DD FOR MCS T/S STATUS DISPLAY         *         
*JSHA 103 031106-merge UK/US version                                  *         
*JFOS 75 151106 - <UKCR7999> NEW DD FOR MCS T/S 'IN PROGRESS' STS DISP*         
*MPEN 76 070608 - <LO01-7405> SAVE COMPANY FISCAL START DATE          *         
*MPEN 77 251009 - <LO01-9704> SAVE COMPANY EXTRA STATUS ELEMENT       *         
*MPEN 78 271011 - <PR002113> INCREASE COST PROFILE SPACE                        
*MPEN 79 230112 - <PR002517> THIRD PARTY USE FLAG                               
*                                                                     *         
***********************************************************************         
*                                                                               
T61D00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T61D00,R7,RR=R2,CLEAR=YES                                
*        ST    R2,RELO                                                          
*                                                                               
         L     RA,4(R1)            A(TWA)                                       
         USING T61DFFD,RA                                                       
         LR    R8,RC               SPOOL AREA                                   
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND         GEND                                         
         USING GEND,RC                                                          
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*        ST    R7,BASER7                                                        
         LR    R9,R1                                                            
         ST    R9,SYSPARMS                                                      
*                                                                               
         MVI   SPACES,C' '         INITIALIZE SPACES TO SPACES                  
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         ST    R2,BASERELO                                                      
         ST    R7,BASER7                                                        
         MVC   ATIOB,0(R1)         A(TIOB)                                      
*                                                                               
         L     RE,20(R1)                                                        
         MVC   FACFLAG,7(RE)       SAVE CONNECT FLAG                            
         MVC   FACUPD,10(RE)       AND UPDATIVE FACPAK ID                       
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE PROGRAM DEPENDENT VALUES          
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR SAVED MESSAGES                         
*                                                                               
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRANSTAT,RCHANG      THEN SET RCHANG FLAG                        
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRANSTAT,ACHANG      THEN SET ACHANG FLAG                        
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    *+18                                                             
         CLC   LP@CHA,CONACT       AND IT CHANGED TO 'CHA'                      
         BNE   *+8                                                              
         OI    TRANSTAT,USERCHA     THEN SET USER CAUSED 'CHA' FLAG             
*                                                                               
         NI    CONOPTH+1,X'FF'-X'20' ENSURE OPTIONS FIELD UNPROTECTED           
*                                                                               
         BAS   RE,GOGENCON         GO OFF TO GENCON                             
*                                                                               
         OI    CONRECH+4,X'20'     SET RECORD/ACTION FLDS VALID                 
         OI    CONACTH+4,X'20'                                                  
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        INITIALIZE PROGRAM DEPENDENT VALUES                         *          
**********************************************************************          
*                                                                               
SYSINIT  NTR1                                                                   
         LA    R1,SECBLK           SET ADDRESS OF SECRET BLOCK                  
         ST    R1,ASECBLK                                                       
*                                                                               
         LR    R1,R9               SAVE A(DISPLAY BLOCK)                        
         AH    R1,=Y(DISPBLK-SYSD)                                              
         ST    R1,ADISPBLK                                                      
*                                                                               
         LH    R1,=Y(OFFBLK-SYSD)                                               
         LA    R1,SYSD(R1)                                                      
         ST    R1,AOFFBLK                                                       
*                                                                               
                                                                                
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS10    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,BASERELO                                                      
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS10                                                         
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,PRGCOMM                                                       
         LA    R0,NPRGCOMM                                                      
*                                                                               
SYS20    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SYS20                                                         
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R4,COREFACS         POINT TO ADDRESS AREA                        
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)            A(COMFACS)                                   
         L     RF,CCALLOV-COMFACSD(R1)                                          
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS30    MVC   DMCB+7(1),0(R2)                                                  
         CLI   0(R2),0                                                          
         BE    SYS32                                                            
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
*                                                                               
SYS32    LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SYS30                                                         
*                                                                               
         MVI   SYSTEM,C'A'         ACCOUNT                                      
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VALUSER     ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=Y(L'ACTKEY)   KEY/STATUS AND DATADISP VALUES               
         MVC   LSTATUS,=Y(L'ACTRSTA)                                            
         MVC   DATADISP,=Y(L'ACTKEY+L'ACTRLEN+L'ACTRSTA+L'ACTRLNK)              
         MVC   SYSFIL,=C'ACCMST  '                                              
         MVC   SYSDIR,=C'ACCDIR  '                                              
         MVI   ACTELOPT,C'Y'       ADD ACTIVITY ELEMENT                         
         MVI   GETMSYS,6           USES GETMSG FOR SYSTEM 6                     
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'AC'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9061D00'    PRESET FOR SYSTEM CALLOVS               
         MVI   USEIO,C'N'                                                       
         LA    R1,STARTSV          SET SAVED STORAGE START                      
         ST    R1,ASTARTSV                                                      
*                                                                               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         OC    TWAVPRNT,TWAVPRNT   DON'T BOTHER IF OFFLINE                      
         BNZ   SYS40                                                            
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BE    SYS40                                                            
         LA    R1,RECACT1          DON'T ALLOW ALL RECORD TYPES                 
         ST    R1,ARECACT1                                                      
         LA    R1,RECACT3          OR ALL RECORD/ACTION COMBINATIONS            
         ST    R1,ARECACT3                                                      
*                                                                               
SYS40    LA    R1,CONRECH          SET EFH TAGS                                 
         ST    R1,EFHREC                                                        
         LA    R1,CONACTH                                                       
         ST    R1,EFHACT                                                        
         LA    R1,CONKEYH                                                       
         ST    R1,EFHKEY                                                        
         LA    R1,CONWHENH                                                      
         ST    R1,EFHWHEN                                                       
         LA    R1,CONOUTH                                                       
         ST    R1,EFHOUT                                                        
         LA    R1,CONDESTH                                                      
         ST    R1,EFHDEST                                                       
         LA    R1,CONOTHH                                                       
         ST    R1,EFHOTH                                                        
         LA    R1,CONTAGH                                                       
         ST    R1,EFHTAG                                                        
*                                                                               
         OI    GENSTAT1,NOSETEFH+OKADDEL+USKYMRG+APPLIC                         
         OI    GENSTAT2,DISTHSPG                                                
         OI    GENSTAT3,OKVALSEL+RESTXE00+USEDICT                               
         OI    GENSTAT4,NODELLST+CONFDEL+NODUPDIE+USEBIGKY+NEWSCRM              
         OI    GENSTAT4,USEAFRCH                                                
         OI    GENSTAT6,GESECOVR   USE SEC OVERRIDE-CONFLICTS W/SECMASK         
         OI    GENSTAT7,GES7DDS    DDS ONLY RECORDS                             
*                                                                               
         MVC   LSVTWA0,=AL2(MAXLTWA0)  L'STORAGE TO SAVE IN TWA0                
         MVI   NTWA,0                  DON'T SAVE ANY EXTRA PAGES               
         MVI   LRECACT,L'RECACT        SET L'RECACT TABLE ENTRY                 
         MVI   LDDSDISP,13         DISPL INTO RECACT TAB FOR FLAG FOR           
*                                  DDS ONLY RECORD/ACTIONS                      
*                                                                               
         OC    TWAVPRNT,TWAVPRNT       SKIP AROUND IF OFFLINE                   
         BNZ   XIT                                                              
*                                                                               
         L     R1,ATIOB            A(TIOB)                                      
         USING TIOBD,R1                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID          PICK UP PFKEY VALUE                          
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
*        ROUTINE HANDLES PASSING OF CONTROL TO AND FROM GENCON       *          
**********************************************************************          
*                                                                               
GOGENCON NTR1                                                                   
         BAS   RE,SETRD            SET RD SO GENCON ALWAYS RETURNS              
*                                                                               
GOG10    MVI   GOAGAIN,C'N'        INITIALIZE RETURN SWITCH                     
         OI    TRANSTAT,FRSTMODE    ALLOWS APPL TO DETECT FIRST MODE            
*                                                                               
GOG20    GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON - PASS A(W/S)                  
*                                                                               
         BAS   RE,CALLTSAR                                                      
         CLI   GOAGAIN,C'Y'        REQUEST BY APPLIC. TO GO BACK                
         BE    GOG10                                                            
         ICM   R1,15,AFRSTKEY      IF CURSOR IS AT FIRST KEY FIELD              
         BZ    GOG40                                                            
         TM    6(R1),X'40'                                                      
         BZ    GOG40                                                            
         CLI   OKNO,2              AND GENCON IS ASKING FOR MORE INPUT          
         BNE   GOG30                                                            
         CLI   GOAGAIN,C'K'        AND WE DIDN'T TRY THIS ALREADY               
         BE    GOG30                                                            
         CLI   ACTNUM,ACTDEL       AND IF ACTION IS NOT DELETE                  
         BNE   *+12                                                             
         CLI   RECNUM,RTHIS        EXCEPT FOR HISTORY REC                       
         BNE   GOG40                                                            
         CLI   ACTNUM,ACTADD       IF ADD                                       
         BNE   *+12                                                             
         CLI   RECNUM,RTEDT        AND FOR EDIT REC DON'T TRY                   
         BE    GOG40                                                            
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         BE    GOG40                                                            
*                                                                               
         MVI   CONKEY,C','         MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   CONKEYH+5,1         APPLICATION GETS A CHANCE TO FILL            
         MVI   GOAGAIN,C'K'        IN KEY FIELDS                                
         B     GOG20               GO BACK                                      
*                                                                               
GOG30    MVI   CONKEY,C' '         ERASE COMMA TO KEY FIELD                     
         MVI   CONKEYH+5,0                                                      
         OI    CONKEYH+6,X'80'                                                  
         CLI   5(R1),0             IF NOTHING IS IN FIRST KEY FIELD             
         BNE   *+12                                                             
         CLI   ERROR,MISSING       AND MISSING INPUT FIELD ERROR                
         BE    PLSENTER            SWITCH TO PLEASE ENTER FIELDS ...            
*                                                                               
GOG40    CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BNE   GOGX                                                             
         CLI   OKNO,16             IF GENCON MSG IS "END OF LIST - HIT          
         BE    SELFIRST            ENTER...", CHANGE TO SELECT OR HIT..         
         CLI   OKNO,15             IF MSG IS "LIST DISPLAYED - HIT              
         BE    SELNEXT             ENTER...", CHANGE TO SELECT OF HIT..         
*                                                                               
GOGX     B     XIT                 ALL THROUGH                                  
*                                                                               
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO WE GET CONTROL BACK                
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO SAVE TSAR BLOCK                                  *          
**********************************************************************          
*                                                                               
CALLTSAR NTR1                                                                   
         CLI   TSARSTAT,0                                                       
         BE    XIT                                                              
         TM    TSARSTAT,TSARSAVE   SAVE                                         
         BNO   XIT                                                              
*                                                                               
         LA    R3,TSRBLK1                                                       
         TM    TSARSTAT,TSARBUF2   ARE WE USING 2ND BUFFER                      
         BNO   *+8                                                              
         LA    R3,TSRBLK2                                                       
                                                                                
         USING TSARD,R3                                                         
         TM    TSINDS,TSIINIOK     ALREADY INITIALIZED                          
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   TSACTN,TSASAV       SAVE                                         
         NI    TSARSTAT,X'FF'-TSARSAVE                                          
*                                                                               
         GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
*        SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY              *          
**********************************************************************          
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VVALUSER                                                         
         B     VINITIAL                                                         
         B     VGETTWA                                                          
         B     VGETLDG                                                          
         B     VGTLEVNM                                                         
         B     VGETNME                                                          
         B     VMYERR                                                           
         B     VTSTSEC                                                          
         B     VELM20                                                           
         EJECT                                                                  
**********************************************************************          
*        LTORG                                                       *          
**********************************************************************          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        THIS ROUTINE GETS CALLED BY GENCON ON EVERY TRANSACTION     *          
*        BEFORE CALLING THE APPLICATION                              *          
**********************************************************************          
*                                                                               
VVALUSER DS    0H                                                               
*&&US                                                                           
         ZIC   R1,CONACTH+5                                                     
         LTR   R1,R1                                                            
         BZ    VUSR08                                                           
         BCTR  R1,0                                                             
         EXCLC R1,CONACT,AC@DISPU CHECK       CONNECT INFO IF AN                
         BE    VUSR08                         UPDATIVE ACTION. DISPLAY,         
         EXCLC R1,CONACT,AC@SELU              LIST, SELECT AND REPORT           
         BE    VUSR08                         ARE NOT UPDATIVE                  
         EXCLC R1,CONACT,AC@LIST                                                
         BE    VUSR08                                                           
         EXCLC R1,CONACT,AC@RPT                                                 
         BE    VUSR08                                                           
         TM    OPTION,XICSCADV     CONNECTED TO CSC?                            
         BO    VUSR08              SKIP CHECK                                   
         TM    FACFLAG,XITSTADV    NOT CSC BUT TEST FACPAK DON'T                
         BO    VUSR08              BOTHER WITH CHECK.                           
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    VUSR08                                                           
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    ERRNAUPD            YES                                          
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    ERRHMADV            YES                                          
         B     ERRNADVU            CONNECTED TO READ ONLY SYSTEM                
*                                                                               
VUSR08   DS    0H                                                               
*&&                                                                             
         NI    GENSTAT5,X'FF'-GENSELVR                                          
         CLI   RECNUM,RTTIM        TIME RECORD                                  
         BNE   *+8                                                              
         OI    GENSTAT5,GENSELVR                                                
*                                                                               
         TM    TRANSTAT,RACHANG     REC/ACTION CHANGED?                         
         BZ    *+8                                                              
         NI    PRGSTAT,X'FF'-PGMCALL                                            
*                                                                               
         CLI   MODE,NEWSCR                                                      
         BNE   VUSR60                                                           
         CLI   PHSCREEN,X'FB'      per2 SCREEN?                                 
         BNE   VUSR10                                                           
*&&DO                                                                           
         CLC   ALPHA,=C'DW'        FR DATES FIELD ONLY VALID FOR                
         BE    VUSR30              DWNY                                         
         OI    PESFRDWH+1,X'20'+X'0C'                                           
         OI    PESFRDTH+1,X'20'+X'0C'                                           
         B     VUSR30                                                           
*&&                                                                             
VUSR10   CLI   PHSCREEN,X'E2'      STD HRS DISPLAY SCREEN?                      
         BNE   VUSR30                                                           
         TM    PRGSTAT,CTSTEREO    AND IS USER RUNNING STEREO?                  
         BZ    VUSR30                                                           
         BAS   RE,EDITE2           EDIT SCREEN SO STEREO WILL WORK              
*                                                                               
VUSR30   CLI   RECNUM,RTEDT        IS THIS REC EDIT HRS?                        
         BE    VUSR40                                                           
         CLI   RECNUM,RTSTD        OR STD HRS?                                  
         BE    VUSR40                                                           
         CLI   RECNUM,RTPRO        OR PROFILE?                                  
         BNE   VUSR50              NO THEN FILL IN THE SCREEN                   
VUSR40   CLI   SVREC,RTPER         AND WAS THE PREV REC PERSON?                 
         BE    VUSRX                                                            
         CLI   SVREC,RTTIM         or time?                                     
         BE    VUSRX               YES-THEN DON'T FILL THE SCREEN               
VUSR50   BAS   RE,FILLSCR          FILL IN ANY SCREEN FIELDS                    
         B     VUSRX                                                            
*                                                                               
VUSR60   CLI   OFFLINE,C'Y'        ALWAYS IF OFFLINE                            
         BE    VUSR70                                                           
         CLI   TWAFIRST,0          ELSE IF FIRST TIME IN                        
         BNE   VUSR80                                                           
VUSR70   BAS   RE,PRGINT           INITIALIZE PROGRAM                           
*                                                                               
VUSR80   CLI   TWAFIRST,0          IF FIRST TIME IN                             
         BNE   VUSR90                                                           
         CLI   OFFLINE,C'Y'        AND IF OFFLINE THEN GENCON WILL SET          
         BE    VUSR90                                                           
         MVI   TWAFIRST,1          ELSE SET NO LONGER FIRST TIME                
*                                                                               
VUSR90   MVC   USERNAME,USRNME    GET USER ID INFO FROM SAVED STORAGE           
         MVC   USERADDR,USRADD                                                  
*                                                                               
         MVC   RECNUM,TWALREC     MOVE SAVED REC/ACT TO CURRENT IN CASE         
         MVC   ACTNUM,TWALACT     WE TAKE ERROR BEFORE GENCON SETS THEM         
*                                                                               
         CLI   RECNUM,RTTIM        TIME RECORD - SKIP LIST CHECK                
         BE    *+12                     CLEAR FIELD WHEN TIME/LIST              
         CLI   CONACT,C'L'         IF ACTION IS NOT LIST                        
         BE    VUSR100                                                          
         CLI   CONACT,C'R'         OR REPORT                                    
         BE    VUSR100                                                          
         CLI   CONWHENH+5,0        AND SOMETHING'S IN PRINT FIELD               
         BE    VUSR100                                                          
         XC    CONWHEN,CONWHEN     THEN CLEAR IT                                
         MVI   CONWHENH+5,0                                                     
         OI    CONWHENH+6,X'80'                                                 
*                                                                               
VUSR100  TM    TRANSTAT,RACHANG    IF RECORD OR ACTION FLD HAS CHANGED          
         BZ    VUSR110                                                          
         MVI   CALLSP,0            CLEAR STACK                                  
         MVI   TSARSTAT,0                                                       
         MVI   PFXSTAT,0                                                        
*                                                                               
VUSR110  TM    TRANSTAT,RCHANG     IF RECORD CHANGE                             
         BZ    VUSR120                                                          
         MVC   CONOPT,SPACES       CLEAR OPTIONS                                
         MVI   CONOPTH+5,0                                                      
         OI    CONOPTH+6,X'80'                                                  
         B     VUSR130                                                          
*                                                                               
VUSR120  DS    0H                  IF RECORD HASNT CHANGED                      
         CLI   SVREC,RTTIM         AND RECORD TYPE IS TIME THEN                 
         BNE   VUSR130                                                          
         OI    GENSTAT5,GENSELVR   ALWAYS GET VALREC FROM SELECT                
*                                                                               
VUSR130  TM    TRANSTAT,RACHANG+USERCHA IF USER HASN'T CHANGED REC/ACT          
         BNZ   VUSR140                                                          
         OC    SVACTION,SVACTION   AND SAVED ACTION EXISTS                      
         BZ    VUSR140                                                          
         MVC   CONACT,SVACTION     THEN RESTORE IT                              
*                                                                               
VUSR140  CLI   TWASCR,X'00'         ARE WE ON THE FF SCREEN                     
         BNE   VUSRX                                                            
         L     R2,=A(PFTABLE)                                                   
         A     R2,BASERELO                                                      
         GOTO1 INITIAL,DMCB,(X'40',(R2)),CONPFKYH                               
         CLI   CONRECH+5,0                                                      
         BE    PLSENTER            PLEASE ENTER FIELDS ...                      
*                                                                               
VUSRX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        EDIT STD HRS DISP SCREEN SO STEREO CAN HANDLE                          
**********************************************************************          
*                                                                               
EDITE2   NTR1                                                                   
         LA    R2,STDMTH1H         MONTH                                        
         OI    1(R2),X'10'         SUPPRESS TRANSMISSION                        
         LA    R2,STDNUM1H         #                                            
         OI    1(R2),X'10'                                                      
         LA    R2,STDPER1H         PERIOD                                       
         OI    1(R2),X'10'                                                      
         LA    R2,STDMTH2H                                                      
         OI    1(R2),X'10'                                                      
         LA    R2,STDNUM2H                                                      
         OI    1(R2),X'10'                                                      
         LA    R2,STDPER2H                                                      
         OI    1(R2),X'10'                                                      
         LA    R2,STDMTH3H                                                      
         OI    1(R2),X'10'                                                      
         LA    R2,STDNUM3H                                                      
         OI    1(R2),X'10'                                                      
         LA    R2,STDPER3H                                                      
         OI    1(R2),X'10'                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        FILL IN SCREEN WITH ANY LEDGER DESCRIPTIONS                 *          
**********************************************************************          
*                                                                               
FILLSCR  NTR1                                                                   
         MVC   AIO,AIO3                                                         
         GOTO1 GETLDG,DMCB,C'1R'   GET LEDGER RECORD AND INFO                   
         MVC   AIO,AIO1                                                         
*                                                                               
         LA    R2,LDGNM1           CHANGE TO LOWERCASE                          
         LA    R3,L'LDGNM1                                                      
         BAS   RE,LOWCASE                                                       
         LA    R2,LDGNM2                                                        
         BAS   RE,LOWCASE                                                       
         LA    R2,LDGNM3                                                        
         BAS   RE,LOWCASE                                                       
         LA    R2,LDGNM4                                                        
         BAS   RE,LOWCASE                                                       
*                                                                               
         LA    R2,CONHEADH         FILL IN LEDGER LEVEL NAMES IN SCREEN         
FILL10   CLI   0(R2),0             END OF SCREEN                                
         BE    FILLX                                                            
         TM    1(R2),X'02'         EXTENDED FIELD HEADER                        
         BNO   FILLNX                                                           
*                                                                               
         ZIC   R1,0(R2)            GET LENGTH OF MOVE                           
         SH    R1,=H'17'           8 + 8 + 1                                    
         BM    FILLNX                                                           
*                                                                               
         ZIC   R3,0(R2)            POINT TO EXTENDED HEADER                     
         SH    R3,=H'8'                                                         
         AR    R3,R2                                                            
*                                                                               
         LA    R4,FILLTAB                                                       
FILL15   CLC   0(1,R3),0(R4)       MATCH ON EXTENDED FIELD NUMBER               
         BE    FILL20                                                           
         LA    R4,3(R4)                                                         
         CLI   0(R4),0                                                          
         BNE   FILL15                                                           
         B     FILLNX                                                           
*                                                                               
FILL20   ZICM  R6,1(R4),2          MOVE IN TEXT                                 
         AR    R6,R9                                                            
         MVC   8(0,R2),0(R6)                                                    
         EX    R1,*-6                                                           
         OI    6(R2),X'80'                                                      
*                                                                               
FILLNX   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         B     FILL10                                                           
FILLX    B     XIT                                                              
*                                                                               
FILLTAB  DC    AL1(201),AL2(LDGNM1-SYSD)                                        
         DC    AL1(202),AL2(LDGNM2-SYSD)                                        
         DC    AL1(203),AL2(LDGNM3-SYSD)                                        
         DC    AL1(204),AL2(LDGNM4-SYSD)                                        
         DC    AL1(199),AL2(AC@CYR-SYSD)                                        
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        CHANGE STRING TO LOWERCASE                                  *          
*        R2 POINTS TO STRING , R3 HAS LENGTH                         *          
**********************************************************************          
*                                                                               
LOWCASE  NTR1                                                                   
*                                                                               
LOW05    LA    R2,1(R2)            NOT FIRST CHAR                               
         SH    R3,=H'1'                                                         
         BM    LOWX                                                             
         MVC   BYTE,0(R2)                                                       
LOW07    CLI   0(R2),X'40'         IS IT A SPACE                                
         BE    LOW10                                                            
         CLI   BYTE,X'40'          WAS LAST ONE A SPACE                         
         BE    LOW05               YES, THEN LEAVE CAPITAL                      
         B     LOW20               ELSE LOWERCASE IT                            
*                                                                               
LOW10    NI    0(R2),X'FF'-X'40'   SPACES TO NULLS- FOR GENCON KEYMERGE         
         LA    R2,1(R2)                                                         
         SH    R3,=H'1'                                                         
         BM    LOWX                                                             
         B     LOW07                                                            
*                                                                               
LOW20    CLI   0(R2),X'C1'         IS IT UPPERCASE                              
         BL    LOW05                                                            
         CLI   0(R2),X'E9'                                                      
         BH    LOW05                                                            
         NI    0(R2),X'FF'-X'40'   MAKE IT LOWERCASE                            
         B     LOW05                                                            
*                                                                               
LOWX     B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        PROGRAM INITIALIZATION - PROCESSED ONE TIME ONLY            *          
**********************************************************************          
*                                                                               
PRGINT   NTR1                                                                   
         L     R1,SYSPARMS         COMPANY CODE                                 
         MVC   CMPY,0(R1)                                                       
         LA    R2,CONHEADH         Set R2 for GENCON in case of error           
*                                                                               
         XC    BIGKEY,BIGKEY       GET AGENCY NAME & ADDR FROM ID REC.          
         LA    R3,BIGKEY                                                        
         USING CTIKEY,R3                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         MVI   USEIO,C'Y'                                                       
         MVC   FILENAME,=CL8'CTFILE'                                            
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',BIGKEY,AIO,0                  
*        GOTO1 READ                                                             
         XC    FILENAME,FILENAME                                                
         MVI   USEIO,C'N'                                                       
*                                                                               
         SR    R0,R0                                                            
         L     R3,AIO                                                           
         LA    R3,CTIDATA                                                       
*                                                                               
PRG10    CLI   0(R3),X'06'         AGENCY ALPHA ELEM                            
         BE    PRG30                                                            
         CLI   0(R3),X'30'         AGENCY NAME AND ADDRESS                      
         BE    PRG40                                                            
         CLI   0(R3),0                                                          
         BE    PRG50                                                            
PRG20    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     PRG10                                                            
*                                                                               
         USING CTAGYD,R3                                                        
PRG30    MVC   ALPHA,CTAGYID       AGENCY ALPHA                                 
         B     PRG20                                                            
*                                                                               
         USING CTDSTD,R3                                                        
PRG40    MVC   USRNME,CTDSTNAM     SAVE AGENCY NAME AND ADDRESS                 
         MVC   USRADD,CTDSTADD     AND ADDRESS                                  
         B     PRG20                                                            
*                                                                               
PRG50    GOTO1 GETFACT,DMCB,0      GET SOME DATA FROM GETFACT                   
         USING FACTSD,R3                                                        
         L     R3,DMCB                                                          
         CLI   FASYSID,1           CHECK FOR TEST SYSTEM                        
         BNE   *+8                                                              
         OI    PRGSTAT,TESTSYS                                                  
         TM    FATSTAT6,X'80'      USING STEREO?                                
         BZ    *+8                                                              
         OI    PRGSTAT,CTSTEREO                                                 
*                                                                               
         MVC   LINEID(8),FALINE    SAVE LINE ID                                 
         MVC   LANGCODE,FALANG     SAVE LANG CODE                               
         MVC   CTRYCODE,FACTRY     SAVE CTRY CODE                               
         MVC   GFACTST6,FATSTAT6   GETFACT STATUS 6                             
         TM    FATFLAG,X'08'       VALID PASSWORD                               
         BNO   *+10                                                             
         MVC   SECPID#,FAPASSWD    PASSWORD ID NUMBER                           
*                                                                               
         CLI   CTRYCODE,0          SET DEFAULT LANGUAGE                         
         BNE   *+8                                                              
*&&UK*&& MVI   CTRYCODE,LANGEUK                                                 
*&&US*&& MVI   CTRYCODE,LANGEUS                                                 
*                                                                               
         XC    DMCB(24),DMCB       INIT SECRET                                  
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK)                                 
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
         USING SECD,R1                                                          
         L     R1,ASECBLK                                                       
         MVC   SECALPHA,SECAGY     AGENCY USED FOR SECURITY                     
         DROP  R1                                                               
*                                                                               
         GOTO1 DICTATE,DMCB,C'L   ',DICTAB,DICLIST                              
         GOTO1 DICTATE,DMCB,C'LU  ',DICTABU,DICLISTU                            
*                                                                               
         MVI   TOPSCR,X'00'                                                     
         MVI   BOTSCR,X'00'                                                     
         XC    TSRBLK1(L'TSRBLK1+L'TSRBLK2),TSRBLK1   CLEAR BOTH BLKS           
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CPYRECD,R6          R6=A(ACCOUNT RECORD KEY)                     
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CMPY                                                     
         GOTO1 READ                READ THE RECORD IN BIG KEY                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         AH    R4,=Y(CPYRFST-CPYRECD)                                           
         SR    R0,R0                                                            
PRG60    CLI   0(R4),0                                                          
         BE    PRG66               TEST EOR                                     
         USING CPYELD,R4                                                        
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    PRG64                                                            
         CLI   CPYEL,CPXELQ                                                     
         BNE   PRG62                                                            
         MVC   BCCPXEL,0(R4)       MOVE WHOLE ELEMENT IN                        
         B     PRG66                                                            
*                                                                               
PRG62    SR    R0,R0                                                            
         IC    R0,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     PRG60                                                            
*                                                                               
PRG64    MVC   COMPYSEC,CPYBSEC    SECURITY                                     
         MVC   COMPYST1,CPYSTAT1   STATUS #1                                    
         MVC   COMPYST2,CPYSTAT2   STATUS #2                                    
         MVC   COMPYST3,CPYSTAT3   STATUS #3                                    
         MVC   COMPYST4,CPYSTAT4   STATUS #4                                    
         MVC   COMPYSTA,CPYSTATA   STATUS #A                                    
         MVC   COMPFST,CPYSFST     STORE COMP FISCAL START DATE                 
         CLI   CPYLN,CPYLN2Q                                                    
         BL    PRG66                                                            
         MVC   COMPYST5,CPYSTAT5   STATUS #5                                    
         MVC   COMPYST6,CPYSTAT6   STATUS #6                                    
         MVC   COMPYST7,CPYSTAT7   STATUS #7                                    
         B     PRG62                                                            
PRG66    DS    0H                                                               
         L     R1,AOFFBLK          INITIALISE OFFAL FOR OFFICE ACCESS           
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFATSAR,VTSAR                                                   
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVC   OFFACPY,CMPY                                                     
         MVC   OFFACST1(4),COMPYST1                                             
         MVC   OFFACST5(3),COMPYST5                                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVI   OFFAACT,OFFAINI                                                  
         OI    OFFACTRL,OFFACCNV   NEW STYLE RECORDS                            
         OC    OFFASAV(OFFASAVL),SVOFFAL                                        
         BZ    *+8                                                              
         MVI   OFFAACT,OFFARES                                                  
         OI    OFFAINDS,OFFAISEC+OFFAIOFF                                       
         GOTO1 VOFFVAL                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T INITIALISE OFFAL                
         MVC   SVOFFAL,OFFASAV                                                  
         B     XIT                                                              
         DROP  R1,R3,R4,R6                                                      
         EJECT                                                                  
**********************************************************************          
*        OVERLAY INITIALIZATION                                      *          
*        P1=A(PFKEY VAL. TABLE) OR ZEROS                             *          
*        P2=A(PFKEY LINE) OR ZEROS                                   *          
**********************************************************************          
*                                                                               
VINITIAL DS    0H                                                               
         MVC   INITBYTE,0(R1)      SAVE STATUS BYTE                             
         SR    R4,R4                                                            
         ICM   R4,7,1(R1)          IF PFKEY VALIDATION TABLE PASSED             
*                                                                               
         ICM   R3,15,4(R1)         IF PFKEY LINE PASSED - VALIDATE LIST         
         BZ    INIT05                                                           
         GOTO1 VPFLINE,DMCB,(R3)   HANDLE LOCAL PFKEY PRESENCE                  
*                                                                               
INIT05   LTR   R3,R4               IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT10                                                           
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
         GOTO1 PFVAL,DMCB,(R3)     HANDLE LOCAL PFKEY PRESENCE                  
         BE    DUMMYERR            TAKE DUMMY ERROR EXIT FOR GOAGAIN            
*                                                                               
INIT10   MVI   SCRSTAT,0           CLEAR SCREEN STATUS BYTE                     
*                                                                               
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
*                                                                               
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
*                                                                               
         MVC   BYTE,ACTNUM         MOVE CURRENT ACTION TO TEMP. W/S             
         CLI   BYTE,ACTCHA         IF CURRENT ACTION IS CHANGE                  
         BNE   *+16                                                             
         CLI   SVACT,ACTSEL        AND SAVED ACTION WAS SELECT                  
         BNE   *+8                                                              
         MVI   BYTE,ACTSEL         PRETEND CURRENT ACTION IS SELECT             
*                                                                               
         CLC   BYTE,SVACT          TEST ACTION CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,ACTCHG                                                   
*                                                                               
         TM    INITBYTE,X'80'      FORCE TO APPL. CLEAR STORAGE                 
         BO    INIT20                                                           
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    INIT20                                                           
         TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
         BZ    INIT30                                                           
         CLI   BYTE,ACTREP         ALWAYS CLEAR IF ACTION IS NOW REPORT         
         BE    INIT20                                                           
         CLI   SVACT,ACTSEL        IF LAST ACTION NOT SELECT                    
         BE    INIT30                                                           
         CLI   BYTE,ACTSEL         AND THIS ACTION NOT SELECT                   
         BE    INIT30                                                           
*                                                                               
INIT20   TM    INITBYTE,X'40'      FORCE NOT TO CLEAR STORAGE                   
         BO    INIT30                                                           
         LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'SYSSPARE)                                              
         XCEFL                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         SR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    *+10                                                             
         AR    RE,RF                                                            
         B     *-10                                                             
         LA    RE,3(RE)            BUMP PAST CONTROL BYTES                      
         LR    RF,RE                                                            
         SR    RF,RA                                                            
         SH    RF,=AL2(3520+64)    L'AVAIL TWA0 AS DEFINED IN DDGENTWA          
         LCR   RF,RF                                                            
         XCEFL ,                   CLEAR AREA AFTER SCREEN END                  
*                                                                               
INIT30   MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM                     RECORD                          
         MVC   SVACT,BYTE                       ACTION                          
*                                                                               
INITX    B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*        LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                      *          
*        P1  BYTES 1-3 = A(PFKEY VAL. TABLE)                         *          
**********************************************************************          
*                                                                               
PFVAL    NTR1                                                                   
         CLI   PFKEY,0             USER HIT ENTER?                              
         BE    XNO                 YES                                          
*                                                                               
         L     R2,0(R1)            R2=A(PFKEY TABLE)                            
         USING PFTABD,R2           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(R2),X'FF'                                                      
         BE    PFERR                                                            
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    R2,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    XNO                                                              
*                                                                               
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     XYES                IF RETURNS, RETURN CC EQUAL                  
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO PROCESS PFKEY REQUEST                            *          
*        INPUT                     R2=A(PFTABLE ENTRY)               *          
**********************************************************************          
*                                                                               
         USING PFTABD,R2           R2=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         MVI   PFKEY,0             CLEAR PFKEY FOR NEXT SCREEN                  
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
PFI4     TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    PFI5                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         CLI   CALLAGN,C'Y'                                                     
         BE    DUMMYERR            GO AGAIN TO GENCON                           
         B     DUMYERR1            TAKE DUMMY ERROR EXIT (NOT GOAGAIN)          
*                                                                               
PFI5     CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BNL   PFI8                                                             
         MVC   DUB(8),PFTREC                                                    
         GOTO1 DICTATE,DMCB,C'SU  ',DUB,0                                       
         MVC   CONREC,DUB          MOVE IT OUT                                  
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
*                                                                               
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
*                                                                               
PFI8     CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BNL   PFI10                                                            
         MVC   DUB(8),PFTACT                                                    
         GOTO1 DICTATE,DMCB,C'SU  ',DUB,0                                       
         MVC   CONACT,DUB          MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,8         SET L'I/P - cha from 5 to 8 on 2/00          
*                                                                               
PFI10    CLI   CONWHENH+5,0        DON'T BOTHER IF INPUT PRESENT                
         BNE   PFIX                                                             
         TM    PFTSTAT2,PFTSETPN   TEST PRESET OF PRINT FIELD TO NOW            
         BZ    PFIX                                                             
         MVC   CONWHEN(7),=C'NOW,TMS'                                           
         MVI   CONWHENH+5,7                                                     
         OI    CONWHENH+6,X'80'    TRANSMIT NEW PRINT FIELD                     
*                                                                               
PFIX     B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
*        ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS            *          
*        INPUT                     R3=A(PFKEY TABLE)                 *          
**********************************************************************          
*                                                                               
TESTSEL  NTR1                                                                   
         CLI   RECNUM,RTEDT        ARE WE IN DAILY EDIT                         
         BNE   TSEL2                                                            
         CLI   BOTSCR,X'EC'        HAVE WE LOADED THE SCREEN                    
         BNE   TSEL2                                                            
         CLI   ACTNUM,ACTCHA       OK IF CHANGE                                 
         BE    TSEL4                                                            
         CLI   ACTNUM,ACTDIS       OK IF DISPLAY                                
         BE    TSEL4                                                            
         CLI   ACTNUM,ACTSEL       OK IF SELECT                                 
         BE    TSEL4                                                            
TSEL2    CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
TSEL4    CLI   TWASCR,X'00'        IF ON FF SCREEN                              
         BE    TSELX               THEN NO LIST                                 
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
TSEL6    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL16                                                           
         TM    1(R2),X'20'         SKIP PROTECTED                               
         BO    TSEL16                                                           
*                                                                               
         XC    WORK,WORK           MOVE FIELD TO WORK                           
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         MVC   WORK(0),8(R2)                                                    
         EX    R1,*-6                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    WORK(0),SPACES                                                   
*                                                                               
         USING PFTABD,R5           R3 =A(START OF TABLE)                        
         LR    R5,R3               RESET TO START OF TABLE                      
TSEL8    CLI   0(R5),X'FF'                                                      
         BE    TSEL16                                                           
         CLI   PFTSEL,C' '                                                      
         BNL   TSEL14                                                           
         MVC   DUB(3),PFTSEL                                                    
         GOTO1 DICTATE,DMCB,C'SU  ',DUB,0                                       
         CLC   DUB(3),WORK         MATCH ON EXACT SELECT CODE                   
         BE    TSEL18                                                           
         CLI   RECNUM,RTEDT        ARE WE IN DAILY EDIT                         
         BNE   TSEL14                                                           
         CLI   ACTNUM,ACTCHA       OK IF CHANGE                                 
         BE    TSEL10                                                           
         CLI   ACTNUM,ACTDIS       OK IF DISPLAY                                
         BE    TSEL10                                                           
         CLI   ACTNUM,ACTSEL       OK IF SELECT                                 
         BNE   TSEL14                                                           
TSEL10   ZIC   R1,5(R2)            ACTUAL LENGTH OF INPUT                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),DUB         CHECK IF IT MATCHES                          
         BE    TSEL18              YES                                          
*                                                                               
TSEL14   ZIC   R1,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    R5,R1                                                            
         B     TSEL8                                                            
*                                                                               
TSEL16   ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL6               SELECT FIELD                                 
         B     TSEL16                                                           
*                                                                               
TSEL18   MVC   8(3,R2),SPACES      FOUND A MATCH - CLEAR SELECT FIELD           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    R6,R2               SAVE A(FIELD)                                
TSEL20   ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSEL22              (E-O-S)                                      
         TM    1(R2),X'20'                                                      
         BO    TSEL20                                                           
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BE    TSEL20                                                           
         ZIC   R1,0(R2)            ANYTING TO SELECT                            
         SH    R1,=H'9'                                                         
         TM    1(R2),X'02'                                                      
         BZ    *+8                                                              
         SH    R1,=H'8'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),SPACES                                                   
         BH    TSEL16                                                           
*                                                                               
TSEL22   MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    R6,RA                                                            
         STH   R6,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR    *          
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN        *          
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH  *          
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO *          
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE *          
* WORKING ON. WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL*          
* BE CALLED TO RESTORE THE SCREEN.                                   *          
**********************************************************************          
*                                                                               
CPROG    NTR1                                                                   
         SR    R3,R3               ALWAYS USE SAME SPOT FOR SAVED SCN           
         LA    RF,CALLSTCK(R3)                                                  
         MVC   0(1,RF),TWASCR                                                   
*                                                                               
         MVI   CALLSP,1            ALWAYS SET TO 1                              
*                                                                               
         L     RE,ATIA             SAVE SCREEN IN FIRST HALF OF TWA             
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),STARTSV)                                       
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON   *         
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG*         
***********************************************************************         
*                                                                               
RPROG    NTR1                                                                   
         CLI   CALLSP,0                                                         
         BE    PFERR               ERROR IF STACK IS EMPTY                      
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE SCREEN FROM 1ST HALF OF TWA          
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   ACTNUM,TWALACT      SPECIAL CODE TO KEEP SELECT GOING            
         MVC   RECNUM,TWALREC                                                   
         MVC   CONHEAD(30),=CL30'Came back from another screen.'                
         MVC   CONHEAD+30(30),=CL30'  Please continue ...'                      
*                                                                               
         LA    R2,3                READ TWA RECORD #3                           
         GOTO1 GETTWA,DMCB,((R2),STARTSV)                                       
*                                                                               
         ZIC   R3,CALLSP           DECREMENT STACK POINTER                      
         BCTR  R3,0                                                             
         STC   R3,CALLSP                                                        
*                                                                               
         LA    RF,CALLSTCK(R3)     EXTRACT TWASCR                               
         MVC   TWASCR,0(RF)                                                     
*                                                                               
         LA    R2,CONHEADH         TRANSMIT SCREEN                              
RP10     CLI   0(R2),0                                                          
         BE    RP20                FIND END OF TWA                              
         OI    6(R2),X'80'                                                      
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     RP10                                                             
RP20     MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         OI    TRANSTAT,RETURNED    SET THAT RETPROG HAS BEEN CALLED            
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD               *         
***********************************************************************         
*                                                                               
         USING PFTABD,R2           R2=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R5,WORK             R5=A(WORK)                                   
         ZIC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
*                                                                               
EXP10    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         CLI   KEYTYPE,KEYTYGLB    GLOBAL (STARTSV)                             
         BE    EXP15                                                            
         LA    RF,SYSSPARE                                                      
         CLI   KEYTYPE,KEYTYWS     W/S (SYSSPARE)                               
         BE    EXP15                                                            
         CLI   KEYTYPE,KEYTYCUR    CURSOR LOCATION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RF,CURDISP          ASSUME THIS IS A SELECT FIELD                
         AR    RF,RA               RF=A(FLD WHERE CURSOR IS)                    
         BAS   RE,BMPTOROW         BUMP TO FIRST FIELD FOR THIS ROW             
         BNE   PFERR                                                            
         L     RF,FULL             RETURNS ADDRESS IN FULL                      
*                                                                               
EXP15    AH    RF,KEYDISP          RF=A(DATA)                                   
         ZIC   RE,KEYLEN           RE=L'DATA-1                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RF)       MOVE TO WORK                                 
         CHI   RE,1                POSSIBLE OFFICE FIELD                        
         BNE   EXP16               NO - NOT 2 CHARACTERS LONG                   
         CLC   0(3,RF),AC@ALLU     CHECK IF DATA WAS ALL                        
         BNE   EXP16               NO                                           
         MVC   0(2,R5),SPACES      YES - REPLACE WITH SPACES FOR KEY            
*                                                                               
EXP16    AR    R5,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R5),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R5,0                                                             
         BCT   RE,*-10                                                          
         LA    R5,1(R5)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CH    R3,=H'1'            TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    MVI   0(R5),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R5,1(R5)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R5,R3               R5=L'TMPKEY FIELD                            
         CLM   R5,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R5,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     XIT                                                              
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
         DROP  R4,R2                                                            
         EJECT                                                                  
***********************************************************************         
*        BUMP TO FIRST FIELD IN ROW                                   *         
***********************************************************************         
*                                                                               
BMPTOROW NTR1                                                                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE                                                        
         BE    BMPT4                                                            
         ZIC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   BMPT2                                                            
         B     XNO                 RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    ZIC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     XYES                                                             
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO READ/WRITE TEMPSTR PAGES                          *         
*        INPUT                     P1, BYTE  0=BIT SETTING/PAGE NUMBER*         
*        INPUT                     P1, BYTES 1-3=READ/WRITE ADDRESS   *         
***********************************************************************         
*                                                                               
VGETTWA  DS    0H                                                               
         MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA2                                                            
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA2                                                            
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA2    NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
*                                                                               
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
*        GET LEDGER LEVEL LENGTHS AND NAMES                           *         
* INPUT        R1 = A(TWO BYTE UNIT LEDGER VARIABLE)                  *         
* OUTPUT       LDGLN1, LDGNM1, ETC. HAVE LEDGER INFO AS IN ELEMENT    *         
*              ABCDLEN HAS THE INDIVIDUAL LENGTHS OF EACH LEVEL       *         
*              CC = NO IF LEDGER NOT FOUND                            *         
***********************************************************************         
*                                                                               
VGETLDG  DS    0H                                                               
         CLC   LDGC,0(R1)          DO WE ALREADY HAVE LEDGER                    
         BE    GETLDGX             YES, SO EXIT                                 
         XC    LDGLEL,LDGLEL       CLEAR LEDGER RECORD INFO                     
         LA    R2,BIGKEY                                                        
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CMPY        COMPANY                                      
         MVC   LDGKUNT(2),0(R1)    UNIT/LEDGER                                  
         GOTO1 HIGH                                                             
         CLC   LDGKEY,KEYSAVE                                                   
         BNE   GETLDGN             NO LEDGER FOUND                              
         MVC   LDGLEL,LDGKUNT      SAVE LEDGER CODE                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         AH    R4,=Y(LDGRFST-LDGRECD)                                           
         SR    R0,R0                                                            
GETLDG3  CLI   0(R4),0                                                          
         BE    GETLDGX             TEST EOR                                     
         USING LDGELD,R4                                                        
         CLI   LDGEL,LDGELQ        TEST LEDGER ELEMENT                          
         BNE   GETLDG4                                                          
         MVC   LDGROFP,LDGOPOS                                                  
         B     GETLDG9                                                          
*                                                                               
         USING ACLELD,R4                                                        
GETLDG4  CLI   ACLEL,ACLELQ        TEST HIERARCHY ELEMENT                       
         BNE   GETLDG6                                                          
         MVC   LDGLEL,ACLEL        SAVE THE ELEMENT                             
*                                                                               
         LR    R6,R4                                                            
         ZIC   R1,1(R6)            TOTAL EL LENGTH                              
         SH    R1,=Y(ACLLN1Q)      - CODE AND LENGTH                            
         SR    R0,R0                                                            
         SR    R3,R3                                                            
         LA    R3,L'ACLVALS(R3)    LENGTH OF EACH ACCT DESC AND LENGTH          
         DR    R0,R3                                                            
         STC   R1,NUMLEVS          SAVE NUMBER OF ACCOUNT LEVELS                
*                                                                               
         ZIC   R5,NUMLEVS                                                       
         AH    R6,=Y(ACLLN1Q)      BUMP TO FIRST LENGTH                         
         LA    R3,ABCDLEN          FIELD TO CONTAIN ALL 4 LENGTHS               
         XC    ABCDLEN,ABCDLEN                                                  
         SR    R1,R1               ACUMULATES HOW MUCH TO SUBTRACT              
         ZIC   R2,0(R6)                                                         
GETLDG5  STC   R2,0(R3)                                                         
         AR    R1,R2               ACCUMULATE LENGTHS                           
         LA    R3,1(R3)                                                         
         LA    R6,L'ACLVALS(R6)    NEXT LENGTH                                  
         ZIC   R2,0(R6)                                                         
         SR    R2,R1                                                            
         BCT   R5,GETLDG5                                                       
         B     GETLDG9                                                          
*                                                                               
         USING RSTELD,R4                                                        
GETLDG6  CLI   RSTEL,RSTELQ         TEST STATUS ELEMENT                         
         BNE   GETLDG9                                                          
         MVC   LDGRSEC,RSTSECY+1                                                
         B     GETLDG9                                                          
*                                                                               
GETLDG9  SR    R0,R0                                                            
         IC    R0,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,R0                                                            
         B     GETLDG3                                                          
*                                                                               
*                                                                               
GETLDGN  MVC   GERROR,=AL2(ACELEDG)           INVALID LEDGER                    
GETLDGX  B     XIT                                                              
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
*                GET LEVEL NAME OF ACCOUNT RECORD                     *         
* INPUT        P1 = A(TWO BYTE UNIT/LEDGER VARIABLE)                  *         
*              P2 = A(12 BYTE ACCOUNT FOR DESIRED LEVEL)              *         
*              P3 = A(SCREEN HEADER TO PUT NAME) - OPTIONAL           *         
* OUTPUT       WORK WILL HAVE THE NAME OF THE LEVEL                   *         
*              CC = NO IF LEDGER NOT FOUND                            *         
***********************************************************************         
*                                                                               
VGTLEVNM DS    0H                                                               
         L     R5,8(R1)            SAVE SCREEN HEADER                           
         L     R4,4(R1)            ACCOUNT                                      
         LA    R2,BIGKEY                                                        
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CMPY        COMPANY                                      
         MVC   ACTKUNT(2),0(R1)    UNIT/LEDGER                                  
         MVC   ACTKACT,0(R4)       ACCOUNT                                      
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   GTLEVNMN            NO LEDGER FOUND                              
         GOTO1 GETREC                                                           
         GOTO1 GETNME,DMCB,AIO,(R5)                                             
         B     GTLEVNMX                                                         
*                                                                               
GTLEVNMN MVC   GERROR,=AL2(ACEACCT)           INVALID LEDGER                    
         B     XNO                                                              
GTLEVNMX B     XYES                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*                        GET NAME FROM RECORD                         *         
*        INPUT                  P1 = A(RECORD TO GET NAME FROM)       *         
*                               P2 = A(SCREEN FIELD HEADER) - OPTIONAL*         
***********************************************************************         
*                                                                               
VGETNME  LM    R4,R5,0(R1)         LOAD RECORD & SCREEN ADDRESS                 
         MVC   WORK,SPACES                                                      
         MVI   ELCODE,NAMELQ       SET ELEMENT CODE                             
         BAS   RE,GETEL                                                         
         BNE   XNO                 NOT FOUND                                    
         USING NAMELD,R4                                                        
         SR    R1,R1               MOVE NAME TO WORK                            
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q)      MINUS ELCODE AND LENGTH                      
         BCTR  R1,0                                                             
         MVC   WORK(0),NAMEREC                                                  
         EX    R1,*-6                                                           
*                                                                               
         LTR   R5,R5               TEST FIELD HEADER                            
         BZ    XIT                 NONE, OK TO EXIT                             
         ZIC   R3,0(R5)            LENGTH OF HEADER + DATA                      
         SH    R3,=H'9'            FIELD HEADER LENGTH W/O EXTENSION            
         TM    1(R5),X'02'         EXTENDED FIELD HEADER?                       
         BZ    *+8                                                              
         SH    R3,=H'8'            FIELD HEADER LENGTH WITH EXTENSION           
         OI    6(R5),X'80'         TRANSMIT FIELD                               
         LA    R5,8(R5)            R5 TO DATA FIELD                             
         MVC   0(0,R5),SPACES                                                   
         EX    R3,*-6                                                           
*                                                                               
         CR    R3,R1               GET SHORTEST IN R3                           
         BNH   *+6                                                              
         LR    R3,R1                                                            
         MVC   0(0,R5),WORK        NAME TO SCREEN FIELD                         
         EX    R3,*-6                                                           
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
*        BUILD PFKEY LIST WITH VALID KEYS FROM TABLE                  *         
***********************************************************************         
*                                                                               
VPFLINE  NTR1                                                                   
         L     R4,0(R1)            R4= A(PFKEY  FIELD)                          
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R0,0(R4)            LENGTH OF FIELD                              
         SH    R0,=H'8'            SUBTRACT FIELD HEADER R0=LEN                 
         TM    1(R4),X'02'                                                      
         BZ    *+8                                                              
         SH    R0,=H'8'                                                         
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
         MVC   8(0,R4),SPACES      CLEAR                                        
         EX    R1,*-6                                                           
         OI    6(R4),X'80'         XMIT                                         
*                                                                               
         CLI   PFKEY,13                                                         
         BNE   VPF05                                                            
         MVI   PFKEY,0                                                          
         TM    PFXSTAT,PFXS2ND     ALREADY DISPLAYED 2ND LINE                   
         BNO   *+12                                                             
VPF02    NI    PFXSTAT,X'FF'-PFXS2ND                                            
         B     *+8                                                              
         OI    PFXSTAT,PFXS2ND                                                  
*                                                                               
VPF05    L     R3,=A(SCRTABL)      TABLE OF SCREENS                             
         TM    PFXSTAT,PFXS2ND                                                  
         BZ    *+8                                                              
         L     R3,=A(SCRTABL2)     TABLE OF SCREENS                             
         A     R3,BASERELO                                                      
VPF10    CLC   =X'FFFF',0(R3)      EOT                                          
         BNE   VPF12                                                            
         TM    PFXSTAT,PFXS2ND                                                  
         BNO   VPFX                                                             
         B     VPF02               2ND LINE NOT FOUND = DISP 1ST LINE           
*                                                                               
VPF12    CLI   ACTEQU,ACTREP       SKIP TMS CHECK IF ACTION REPORT              
         BE    VPF15                                                            
         CLI   ACTEQU,ACTLIST      SKIP TMS CHECK IF ACTION LIST                
         BE    VPF15                                                            
         CLI   RECNUM,RTTIM        IF TIME RECORD                               
         BNE   VPF15                                                            
         CLI   BOTSCR,0                                                         
         BE    VPFX                                                             
         CLC   BOTSCR,0(R3)        THEN MATCH ON BOTSCR                         
         BE    VPF20                                                            
*                                                                               
VPF15    CLC   TWASCR,0(R3)        MATCH ON SCREEN                              
         BE    VPF20                                                            
         LA    R3,5(R3)                                                         
         B     VPF10                                                            
*                                                                               
         USING PFLISTD,R5                                                       
VPF20    ICM   R5,15,1(R3)         R5=ADDRESS OF PFKEYLINE TABLE                
         A     R5,BASERELO                                                      
         LA    R4,8(R4)            R4=WHERE TO START LINE                       
         LR    R3,R4                                                            
         AR    R3,R0               R3=WHERE TO STOP LINE                        
         B     VPF25                                                            
*                                                                               
VPF20NX  LA    R5,PFLLENQ(R5)      NEXT LINE IN TABLE                           
VPF25    CLI   PFLREC,X'FF'        EOT                                          
         BE    VPFX                                                             
*                                                                               
         TM    PFLSTAT,PFLSSEC     CHECK FOR SECURITY ON REC/ACT?               
         BNO   VPF30                                                            
         LA    R2,PFLACT                                                        
         ICM   R2,8,PFLREC         R3=(RECORD, A(ACTION))                       
         GOTO1 SECRET,DMCB,('SECPRACT',ASECBLK),(R2)                            
         BNE   VPF20NX             DON'T HAVE THE SECURITY                      
*                                                                               
VPF30    TM    PFLSTAT,PFLSRET     VALIDATE KEY DOESN'T = RETURN KEY            
         BNO   VPF40                                                            
         CLC   PFLSCR,CALLSTCK     COMPARE SCREEN TO STACKED SCREEN             
         BE    VPF20NX             SKIP IF =                                    
*                                                                               
VPF40    TM    PFLSTAT,PFLSNRET    IS RETURN VALID (ANY STACK?)                 
         BNO   VPF50                                                            
         CLI   RECNUM,RTTIM        ALWAYS PUT RETURN IF TIME RECORD             
         BNE   VPF45                                                            
         CLI   ACTEQU,ACTSEL       AND ACTION SELECT                            
         BE    VPF50                                                            
VPF45    CLI   CALLSP,0                                                         
         BE    VPF20NX             SKIP IF STACK IS EMPTY                       
*                                                                               
VPF50    TM    PFLSTAT,PFLSNXTS    ANY PENDING SELECTS                          
         BNO   VPF60                                                            
         CLI   ACTEQU,ACTSEL                                                    
         BNE   VPF20NX                                                          
         LA    R1,LISTDIR                                                       
         ZIC   R0,LISTNUM                                                       
VPF55    CLI   0(R1),0                                                          
         BH    VPF100              THERE'S MORE DISP ENTER=NEXT KEY             
         LA    R1,6(R1)                                                         
         BCT   R0,VPF55                                                         
         B     VPF20NX             NOTHING MORE                                 
*                                                                               
VPF60    TM    PFLSTAT,PFLSDDS     DISPLAY FOR DDS TERMINALS                    
         BNO   VPF100                                                           
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BNE   VPF20NX             SKIP IF NOT                                  
*                                                                               
VPF100   DS    0H                  WILL IT FIT                                  
         ZIC   R1,PFLPF#LN         LEN OF "PF#="                                
         ZIC   R0,PFLDLEN          LEN OF PFKEY DESCRIPTION                     
         AR    R1,R0                                                            
         AR    R1,R4               CURRENT POSITION                             
         CR    R3,R1                                                            
         BL    VPFX                NO MORE ROOM                                 
*                                                                               
         ZIC   R1,PFLPF#LN         LEN OF "PF#="                                
         BCTR  R1,0                                                             
         MVC   0(0,R4),PFLPF#                                                   
         EX    R1,*-6                                                           
         LA    R4,1(R1,R4)         BUMP                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),PFLDESC                                                  
         GOTO1 DICTATE,DMCB,C'S   ',WORK,0                                      
*                                                                               
         ZIC   R1,PFLDLEN          DESC LEN                                     
         BCTR  R1,0                                                             
         MVC   0(0,R4),WORK                                                     
         EX    R1,*-6                                                           
         LA    R4,2(R1,R4)         1 FOR EX + 1 FOR SPACE                       
         B     VPF20NX                                                          
*                                                                               
VPFX     B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* TEST ACCOUNT SECURITY                                               *         
*                                                                     *         
* NTRY - AIO MUST BE SET TO A 2000 BYTE I/O AREA                      *         
*      - BIGKEY MUST BE SET FOR READ                                  *         
*      - USES AIO2 AND AIO3                                           *         
***********************************************************************         
         SPACE 1                                                                
VTSTSEC  DS    0H                                                               
         MVC   FULL,AIO                                                         
         MVC   AIO,AIO3            READ ACCOUNT RECORD INTO IO3                 
         LR    R3,R1                                                            
         LA    R6,BIGKEY                                                        
         MVC   SAVEDKEY,BIGKEY     SAVE KEY                                     
         USING ACTRECD,R6          R6=A(ACCOUNT RECORD KEY)                     
         MVC   HALF,ACTKUNT                                                     
         GOTO1 GETLDG,HALF         GET LEDGER RECORD AND INFO                   
         MVC   BIGKEY,SAVEDKEY     RESTORE USER KEY                             
         GOTO1 HIGH                READ THE RECORD IN BIG KEY                   
         CLC   BIGKEY(L'ACTKEY),KEYSAVE                                         
         BE    TSTS10                                                           
         MVC   GERROR,=AL2(ACEACCT) INVALID ACCOUNT                             
         GOTO1 MYERR                                                            
*                                                                               
TSTS10   DS    0H                                                               
         GOTO1 GETREC                                                           
*        MVC   AIO2,AIO            SAVE AIO IN AIO2 FOR CMP READ                
*        L     R5,AIO2             * INST. ABOVE CLOBBERED A(IO2) *             
         L     R5,AIO                                                           
         AH    R5,=Y(ACTRFST-ACTRECD)                                           
         SR    R0,R0                                                            
         XC    ACCSECR,ACCSECR                                                  
TSTS20   CLI   0(R5),0                                                          
         BE    TSTS40              TEST EOR                                     
         USING RSTELD,R5                                                        
         CLI   RSTEL,RSTELQ        TEST STATUS ELEMENT                          
         BE    TSTS30                                                           
         SR    R0,R0                                                            
         IC    R0,1(R5)            BUMP TO NEXT ELEMENT                         
         AR    R5,R0                                                            
         B     TSTS20                                                           
TSTS30   MVC   ACCSECR,RSTSECY+1   SECURITY LEVEL                               
*                                                                               
TSTS40   DS    0H                                                               
*        TM    COMPYSEC,CPYBSSEC                                                
*        BZ    TSTS50                                                           
         CLC   LDGRSEC,TWAAUTH+1   TEST SECURITY LEVEL                          
         BH    TSTSECSL                                                         
         CLC   ACCSECR,TWAAUTH+1                                                
         BH    TSTSECSL                                                         
*                                                                               
TSTS50   TM    COMPYST1,CPYSOROE   TEST COMPANY USES OFFICES                    
         BZ    TSTSX                                                            
         CLC   TWAACCS,SPACES      TEST ANY LIMIT ACCESS                        
         BNH   TSTSX                                                            
*        TM    COMPYSEC,CPYBSOFF   TEST OVERRIDE OFFICE SECURITY                
*        BZ    TSTSX                                                            
         CLI   LDGROFP,LDGONONE    TEST NO OFFICE IN THIS LEDGER                
         BE    TSTSX                                                            
         CLI   LDGROFP,LDGOTRAN    TEST OFFICE IN TRANSACTIONS                  
         BE    TSTSX                                                            
         CLI   LDGROFP,LDGOPROF    TEST OFFICE IN PROD PROFILE ELM              
         BNE   TSTS90                                                           
         MVC   ACOFFC,SPACES                                                    
         LTR   R3,R3                                                            
         BZ    TSTS90                                                           
         MVC   ACOFFC,0(R3)        MOVE IN SJ OFFICE                            
*                                                                               
         USING CPYRECD,R1          R1=A(COMPANY RECORD KEY)                     
         LA    R1,BIGKEY                                                        
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CMPY                                                     
         MVC   AIO,AIO2            READ COMPANY RECORD INTO IO2                 
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
*                                                                               
         XC    BYTE,BYTE                                                        
         L     R4,AIO              R1=A(COMPANY RECORD)                         
         MVI   ELCODE,CPYELQ       X'10' - COMPANY ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                 NOT FOUND - DIE                              
         DC    H'0'                                                             
         USING CPYELD,R4                                                        
         MVC   BYTE,CPYBSEC        SAVE OFF SECURITY LEVELS                     
*&&UK                                                                           
*                                                                               
* FOR THE UK DONT BOTHER CHECKING THE MOS LOCK ELEMENT                          
* AND IN THE UK THE SECURITY LEVELS IS SAVED OFF IN CPYBSL                      
*                                                                               
         MVC   BYTE,CPYBSL         SAVE OFF SECURITY LEVELS                     
         B     TSTS80                                                           
*&&                                                                             
         DROP  R4                                                               
*                                                                               
         L     R4,AIO              R1=A(COMPANY RECORD)                         
         MVI   ELCODE,MSLELQ       X'18' - MOS LOCK ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   TSTS80              NOT FOUND - TEST CPY BYTE                    
         USING MSLELD,R4                                                        
         SR    R0,R0                                                            
         ICM   R0,1,MSLNUM         NUMBER OF MSLVALS ENTRIES                    
         BZ    TSTSX                                                            
*                                                                               
         LA    R1,MSLVTYP          R1=A(BATCH TYPE)                             
         USING MSLVALS,R1                                                       
*                                                                               
TSTS60   CLI   MSLVTYP,49          CHECK TYPE 49 FOR OFF SECURITY               
         BNE   TSTS70                                                           
         TM    MSLVSEC,MSLVSDEF+MSLVSSEC+MSLVSOFF  TEST IF NONE                 
         BZ    TSTSX                               IF IT IS - EXIT              
         TM    MSLVSEC,MSLVSOFF    TEST IF THERE IS OFFICE SECURITY             
         BO    TSTS100                                                          
         B     TSTS80                                                           
*                                                                               
TSTS70   LA    R1,3(R1)                                                         
         TM    MSLVSEC,MSLVSXTN    TEST IF EXTENDED                             
         BNO   *+8                                                              
         LA    R1,1(R1)                                                         
         BCT   R0,TSTS60                                                        
*                                                                               
TSTS80   TM    BYTE,CPYBSOFF       TEST IF THERE IS OFFICE SECURITY             
         BNO   TSTSX                                                            
         B     TSTS100                                                          
*                                                                               
TSTS90   CLI   LDGROFP,LDGOFLT1    TEST OFFICE IN FILTERS                       
         BNL   TSTSX                                                            
         MVC   ACOFFC,SPACES                                                    
         MVC   WORK(1),LDGROFP                                                  
         NI    WORK,X'FF'-LDGOKEY2                                              
         CLI   WORK,LDGOKEY                                                     
         BH    TSTSX                                                            
         SR    R1,R1                                                            
         IC    R1,WORK                                                          
*        L     R6,AIO2                                                          
         L     R6,AIO3             R6=A(ACCOUNT RECORD)                         
         LA    R1,ACTKACT-1(R1)                                                 
         MVC   ACOFFC+0(1),0(R1)                                                
         TM    LDGROFP,LDGOKEY2    TEST 2 CHARACTER OFFICE IN KEY               
         BZ    *+10                                                             
         MVC   ACOFFC+1(1),1(R1)                                                
*                                                                               
TSTS100  DS    0H                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
*        MVC   OFFAREC,AIO2        RECORD                                       
         MVC   OFFAREC,AIO3        A(ACCOUNT RECORD)                            
         MVC   OFFAOPOS,LDGROFP    OFFICE POSITION OF LEDGER                    
         MVC   OFFAOFFC,ACOFFC                                                  
         MVI   OFFAACT,OFFAPST                                                  
         GOTO1 VOFFVAL                                                          
         BE    TSTSX                                                            
*                                                                               
TSTSECSL MVC   GERROR,=AL2(ACSELOCK)        SECURITY LOCKOUT                    
         GOTO1 MYERR                                                            
TSTSX    MVC   AIO,FULL            RESET AIO                                    
         B     XIT                                                              
         DROP  R1,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ADDS X'20' NAME ELEMENT                                                       
* NTRY  - P1:  BYTE 0  LENGTH OF LAST NAME                                      
*              BYTES 1-3  A(LAST NAME)                                          
*         P2:  BYTE 0 LENGTH OF FIRST NAME                                      
*              BYTES 1-3  A(FIRST NAME)                                         
*         AIO MUST POINT TO RECORD                                              
***********************************************************************         
*                                                                               
VELM20   DS    0H                                                               
         MVC   HALF(1),0(R1)       SAVE LENGTHS IN HALF                         
         MVC   HALF+1(1),4(R1)                                                  
         ICM   R2,7,1(R1)          R2=A(LAST NAME), R3=A(FIRST NAME)            
         ICM   R3,7,5(R1)                                                       
*                                                                               
         USING NAMELD,R6                                                        
         MVI   ELCODE,NAMELQ       X'20'                                        
         GOTO1 REMELEM             REMOVE ANY IF THERE                          
*                                                                               
         MVC   WORK,SPACES         BUILD NAME IN WORK                           
         LA    R4,WORK                                                          
         LA    R5,0                LEN SO FAR                                   
*                                                                               
         ZIC   R1,HALF             LENGTH OF LAST NAME                          
         SH    R1,=H'1'                                                         
         BM    EL20NO                                                           
         MVC   0(0,R4),0(R2)       MOVE IN LAST NAME                            
         EX    R1,*-6                                                           
*                                                                               
         LA    R5,1(R1,R5)         R5 = LENGTH SO FAR                           
         AR    R4,R5               BUMP R4 POINTER IN WORK                      
         MVI   0(R4),C','          INSERT COMMA                                 
         LA    R4,2(R4)                                                         
         LA    R5,2(R5)            ADD 2 TO LEN                                 
*                                                                               
         ZIC   R1,HALF+1           LENGTH OF FIRST NAME                         
         SH    R1,=H'1'                                                         
         BM    EL20NO                                                           
         MVC   0(0,R4),0(R3)       MOVE IN FIRST NAME                           
         EX    R1,*-6                                                           
*                                                                               
EL20E    LA    R5,1(R1,R5)         ADD TO LEN                                   
         CH    R5,=H'36'           MAX LEN                                      
         BNH   *+8                                                              
         LA    R5,36                                                            
*                                                                               
         USING NAMELD,R6                                                        
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   NAMEL,NAMELQ        X'20'                                        
         AH    R5,=Y(NAMLN1Q)      ADD HEADER TO LEN                            
         STC   R5,NAMLN                                                         
         SH    R5,=Y(NAMLN1Q+1)                                                 
         BM    EL20YES                                                          
         MVC   NAMEREC(0),WORK                                                  
         EX    R5,*-6                                                           
         GOTO1 ADDELEM                                                          
*                                                                               
EL20YES  B     XYES                                                             
EL20NO   B     XNO                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS *         
***********************************************************************         
*                                                                               
VMYERR   BAS   RE,CALLTSAR         NEED TO SAVE TSAR RECS?                      
         OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
         MVI   GTMSYS,6            DEFAULT TO ACC SYSTEM                        
         OC    GMSYS,GMSYS         OVERRIDE?                                    
         BZ    *+10                                                             
         MVC   GTMSYS,GMSYS        WHAT SYSTEM # MESSAGES                       
         DROP  RF                                                               
*                                                                               
VMYERRX  GOTO1 ERREX                                                            
         EJECT                                                                  
***********************************************************************         
*        LOCAL EXIT/ERROR ROUTINES                                    *         
***********************************************************************         
*                                                                               
CANTPUSH MVC   GERROR,=AL2(ACEPFK)           PUSH ERROR                         
         B     RETCURS                                                          
PFERR    MVC   GERROR,=AL2(ACEPFK)           INVALID PF KEY                     
         B     RETCURS                                                          
ERRRACT  MVC   GERROR,=AL2(ACERCACT)  REC/ACT INVALID                           
         B     RETCURS                                                          
ERRNAUPD MVC   GERROR,=AL2(ACENAUPD)                                            
         B     RETCURS                                                          
ERRHMADV MVC   GERROR,=AL2(ACEHMADV)                                            
         MVI   GLTXT,L'FACUPD                                                   
         MVC   GATXT+1(L'FACUPD),FACUPD                                         
         B     RETCURS                                                          
ERRNADVU MVC   GERROR,=AL2(ACENADVU)                                            
         B     RETCURS                                                          
*                                                                               
RETCURS  LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         GOTO1 MYERR                                                            
*                                                                               
PLSENTER MVI   GERROR1,2           PLEASE ENTER FIELDS AS REQUIRED              
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         MVI   GMSYS,X'FF'                                                      
         L     R2,AFRSTKEY         R2 TO 1ST KEY FIELD                          
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         NI    TIOBINDS-TIOBD(RE),X'FF'-TIOBALRM   TURN OFF BEEP                
         GOTO1 MYERR                                                            
*                                                                               
DUMMYERR MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
DUMYERR1 LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
*                                                                               
SELFIRST MVI   GERROR1,10          SELECT OR HIT ENTER FOR FIRST                
         B     *+8                                                              
SELNEXT  MVI   GERROR1,9           SELECT OR HIT ENTER FOR NEXT                 
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         MVI   GMSYS,X'FF'                                                      
         MVI   OKNO,0              CLEAR OKNO SO WON'T LOOP ENDLESSLY           
         L     R2,AFRSTREC         R2 TO 1ST SEL FIELD                          
         GOTO1 MYERR                                                            
*                                                                               
XYES     SR    RC,RC                                                            
XNO      LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        STUFF                                                        *         
***********************************************************************         
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(BMONVAL)                                                       
         DC    V(HELLO)                                                         
         DC    V(HELEN)                                                         
         DC    V(RECUP)                                                         
         DC    V(DUMMY)                                                         
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QOFFAL)                                                      
*&&US*&& DC    AL1(QGETBROD)                                                    
*&&UK*&& DC    AL1(0)                                                           
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QPARSNIP)                                                    
         DC    AL1(QQSORT)                                                      
         DC    AL1(QGETCAP)                                                     
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
         EJECT                                                                  
**********************************************************************          
*        LTORG                                                       *          
**********************************************************************          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DICTIONARY LIST TABLE                                        *         
***********************************************************************         
*                                                                               
DICTAB   DS    0X                  LOWER CASE                                   
         DCDDL AC#ACTY,8           ACTIVITY                                     
         DCDDL AC#ADJ,6            ADJUST                                       
         DCDDL AC#ALL,3            ALL                                          
         DCDDL AC#BEN,7            BENEFIT                                      
         DCDDL AC#CHG,6            CHANGE                                       
         DCDDL AC#CRATS,6          CRATES                                       
         DCDDL AC#DATE,4           DATE                                         
         DCDDL AC#DEL,6            DELETE                                       
         DCDDL AC#DOWN,4           DOWN                                         
         DCDDL AC#FIRST,5          FIRST                                        
         DCDDL AC#HIST,7           HISTORY                                      
         DCDDL AC#IDR,8            INDIRECT                                     
         DCDDL AC#LAST,4           LAST                                         
         DCDDL AC#MTH,5            MONTH                                        
         DCDDL AC#NO,3             NO                                           
         DCDDL AC#CPC,4            PC                                           
         DCDDL AC#APG76,7          PENSION                                      
         DCDDL AC#CREM,6           REMOVE                                       
         DCDDL AC#RVRSL,8          REVERSAL                                     
         DCDDL AC#SLRY,6           SALARY                                       
         DCDDL AC#START,5          START                                        
         DCDDL AC#STDHR,8          STD HOUR                                     
         DCDDL AC#TOTAL,5          TOTAL                                        
         DCDDL AC#TOTLS,6          TOTALS                                       
         DCDDL AC#TYPE,4           TYPE                                         
         DCDDL AC#UP,3             UP                                           
         DCDDL AC#UPD,6            UPDATE                                       
         DCDDL AC#VALOP,13         VALID OPTIONS                                
         DCDDL AC#YES,3            YES                                          
         DCDDL AC#CYR,3            YR                                           
         DCDDL AC#SVNUM,25         NUMBER OF SAVED ITMS =                       
         DCDDL AC#SEL,3            SEL                                          
         DCDDL AC#TSSTS,14         T/SHEET STATUS                               
         DCDDL AC#FUAPR,16         FULLY APPROVED                               
*&&US*&& DCDDL AC#SUBMT,16         SUBMITTED (US)                               
*&&UK*&& DCDDL AC#ESSUB,16         SUBMITTED (UK)                               
*&&US*&& DCDDL AC#PAPPR,16         PART APPROVED (US)                           
*&&UK*&& DCDDL AC#ESPAP,16         PART APPROVED (UK)                           
         DCDDL AC#AWLMA,16         AWAITING LINE MGR APPROVAL                   
*&&US*&& DCDDL AC#REJEC,16         REJECTED (US)                                
*&&UK*&& DCDDL AC#ESREJ,16         REJECTED (UK)                                
         DCDDL AC#LIMA,16          LINE MANAGER APPROVED                        
         DCDDL AC#INPRO,16         IN PROGRESS                                  
         DCDDL AC#NAME,20          NAME                                         
         DCDDL AC#STEND,20         START/END DATES                              
                                                                                
DICTABX  DC    X'00'               EOT                                          
*                                                                               
DICTABU  DS    0X                  UPPER CASE                                   
         DCDDL AC#ALL,3            ALL                                          
         DCDDL AC#BEN,7            BENEFIT                                      
         DCDDL AC#CHG,6            CHANGE                                       
         DCDDL AC#CRATS,6          CRATES                                       
         DCDDL AC#DEL,6            DELETE                                       
         DCDDL AC#DSP,7            DISPLAY                                      
         DCDDL AC#FIRST,5          FIRST                                        
         DCDDL AC#HIST,7           HISTORY                                      
         DCDDL AC#HOURS,5          HOURS                                        
         DCDDL AC#HOURS,3          HRS                                          
         DCDDL AC#IDR,8            INDIRECT                                     
         DCDDL AC#LAST,4           LAST                                         
         DCDDL AC#MTH,5            MONTH                                        
         DCDDL AC#DNAMS,21         MONTUEWEDTHUFRISATSUN                        
         DCDDL AC#NO,3             NO                                           
         DCDDL AC#CPC,4            PC                                           
         DCDDL AC#APG76,7          PENSION                                      
         DCDDL AC#CREM,6           REMOVE                                       
         DCDDL AC#RVRSL,8          REVERSAL                                     
         DCDDL AC#SLRY,6           SALARY                                       
         DCDDL AC#START,5          START                                        
         DCDDL AC#TOTAL,5          TOTAL                                        
         DCDDL AC#TOTLS,6          TOTALS                                       
         DCDDL AC#TYPE,4           TYPE                                         
         DCDDL AC#CUNAS,10         UNASSIGNED                                   
         DCDDL AC#UPD,6            UPDATE                                       
         DCDDL AC#YES,3            YES                                          
         DCDDL AC#STDHR,8          STD HOUR                                     
         DCDDL AC#ANUAL,6          ANNUAL                                       
         DCDDL AC#HRATE,5          HRATE                                        
         DCDDL AC#CPHR,3           /HR                                          
         DCDDL AC#HALF,4           HALF                                         
         DCDDL AC#PAGE,4           PAGE                                         
         DCDDL AC#SORT,4           SORT                                         
         DCDDL AC#PRSN,6           PERSON                                       
         DCDDL AC#PERD,6           PERIOD                                       
         DCDDL AC#OFFC,6           OFFICE                                       
         DCDDL AC#DPT,6            DEPT                                         
         DCDDL AC#CSUB,6           SUBDPT                                       
         DCDDL AC#AMT,6            AMOUNT                                       
         DCDDL AC#ADD,6            ADD                                          
         DCDDL AC#ONLY,4           ONLY                                         
         DCDDL AC#LCKD1,9          LOCKED                                       
         DCDDL AC#OPT,6            OPTION                                       
         DCDDL AC#METH,6           METHOD                                       
         DCDDL AC#PRFL,7           PROFILE                                      
         DCDDL AC#ADJ,6            ADJUST                                       
*&&US                                                                           
         DCDDL AC#SEL,3            SEL                                          
         DCDDL AC#LIST,4           LIST                                         
         DCDDL AC#RPT,6            REPORT                                       
*&&                                                                             
         DCDDL AC#MON,9            MONDAY                                       
         DCDDL AC#TUE,9            TUESDAY                                      
         DCDDL AC#WED,9            WEDNESDAY                                    
         DCDDL AC#THU,9            THURSDAY                                     
         DCDDL AC#FRI,9            FRIDAY                                       
         DCDDL AC#SAT,9            SATURDAY                                     
         DCDDL AC#SUN,9            SUNDAY                                       
         DCDDL AC#DTMS,11          DAILY TIME                                   
DICTABUX DC    X'00'               EOT                                          
         EJECT                                                                  
***********************************************************************         
*        RECORD AND ACTION TABLE (RECORDS)                            *         
***********************************************************************         
*                                                                               
RECACT   DS    0CL16                                                            
*                                                                               
RECACT1  DC    X'01'               X'01' = AVAILABLE RECORDS                    
         DCDD  AC#CAL,8            EXPANDED RECORD NAME (CALENDAR)              
         DC    AL1(RTCAL)          RECORD NUMBER                                
         DC    X'0000'             PHASE NUM FOR DATA DICT/HELP SCREEN          
         DC    AL1(0,0,0,0)        SECURITY MASK BITS                           
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#METH,8           METHOD                                       
         DC    AL1(RTMTH)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#CPRSN,8          PERSON                                       
         DC    AL1(RTPER)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#RPER2,8          PER2                                         
         DC    AL1(RTPR2)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#CRATS,8          CRATES                                       
         DC    AL1(RTRAT)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#PAYRL,8          PAYROLL                                      
         DC    AL1(RTPAY)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#STDHR,8          STD HOUR                                     
         DC    AL1(RTSTD)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#STD,8            STANDARD                                     
         DC    AL1(RTSTD)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#HIST,8           HISTORY                                      
         DC    AL1(RTHIS)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#MAD,8            MAD                                          
         DC    AL1(RTMAD)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#PRFL,8           PROFILE                                      
         DC    AL1(RTPRO)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#TIME,8           TIME                                         
         DC    AL1(RTTIM)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#TEMP,8           TIME(FOR SCRIPT)                             
         DC    AL1(RTTIM)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#REDIT,8          EDIT HRS                                     
         DC    AL1(RTEDT)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#RDAY,8           DAY HRS                                      
         DC    AL1(RTDAY)                                                       
         DC    X'0000'                                                          
         DC    AL1(RTEDT,0,0,0)                                                 
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#COVER,8          OVERHEAD                                     
         DC    AL1(RTOVH)                                                       
         DC    X'0000'                                                          
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#TMPHD,8          TEMPO HEADER                                 
         DC    AL1(RTTHD)                                                       
         DC    X'0000'                                                          
         DC    AL1(RTTIM,0,0,0)    ALWAYS USE TIME OVERRIDE                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#TMPDT,8          TEMPO DETAIL                                 
         DC    AL1(RTTDT)                                                       
         DC    X'0000'                                                          
         DC    AL1(RTTIM,0,0,0)    ALWAYS USE TIME OVERRIDE                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#TMPCM,8          TEMPO COMMUTER CODE                          
         DC    AL1(RTTCM)                                                       
         DC    X'0000'                                                          
         DC    AL1(RTTIM,0,0,0)    ALWAYS USE TIME OVERRIDE                     
*                                                                               
         DC    X'01'                                                            
         DCDD  AC#TMPLN,8          TEMPO LINE INFO                              
         DC    AL1(RTTLN)                                                       
         DC    X'0000'                                                          
         DC    AL1(RTTIM,0,0,0)    ALWAYS USE TIME OVERRIDE                     
         EJECT                                                                  
***********************************************************************         
*        RECORD AND ACTION TABLE (ACTIONS)                            *         
***********************************************************************         
*                                                                               
         DC    X'02'               X'02' = AVAILABLE ACTIONS                    
         DCDD  AC#ADD,8            EXPANDED ACTION NAME (ADD)                   
         DC    AL1(ACTADD)         ACTION NUMBER                                
         DC    AL1(ACTADD)         ACTION EQUATE                                
         DC    AL1(0)              SPARE                                        
         DC    AL1(0,0,0,0)        SECURITY MASK BITS                           
*                                                                               
         DC    X'02'               X'02' = AVAILABLE ACTIONS                    
         DCDD  AC#UPLD,8           EXPANDED ACTION NAME (UPLOAD)                
         DC    AL1(ACTADD)         ACTION NUMBER                                
         DC    AL1(ACTADD)         ACTION EQUATE                                
         DC    AL1(0)              SPARE                                        
         DC    AL1(0,0,0,0)        SECURITY MASK BITS                           
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#CHG,8            CHANGE                                       
         DC    AL1(ACTCHA)                                                      
         DC    AL1(ACTCHA)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#DSP,8            DISPLAY                                      
         DC    AL1(ACTDIS)                                                      
         DC    AL1(ACTDIS)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#DEL,8            DELETE                                       
         DC    AL1(ACTDEL)                                                      
         DC    AL1(ACTDEL)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#LIST,8           LIST                                         
         DC    AL1(ACTLIST)                                                     
         DC    AL1(ACTLIST)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#RPT,8            REPORT                                       
         DC    AL1(ACTREP)                                                      
         DC    AL1(ACTREP)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#RSR,8            RESTORE                                      
         DC    AL1(ACTREST)                                                     
         DC    AL1(ACTREST)                                                     
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#SEL,8            SELECT                                       
         DC    AL1(ACTSEL)                                                      
         DC    AL1(ACTSEL)                                                      
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#INP,8            INPUT                                        
         DC    AL1(11)                                                          
         DC    AL1(11)                                                          
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    X'02'                                                            
         DCDD  AC#MOVE,8                                                        
         DC    AL1(8)                                                           
         DC    AL1(8)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0,0,0,0)                                                     
         EJECT                                                                  
***********************************************************************         
*        RECORD/ACTION COMBINATION TABLE                              *         
***********************************************************************         
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS 01=MY MAINT         
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                  XL4 SECURITY MASK BITS                       
*                                                                               
*                                                                               
RECACT3  DC    X'03',AL1(RTMTH,ACTADD),X'F706000080'         METHOD             
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTMTH,ACTCHA),X'F706000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTMTH,ACTDIS),X'F706000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTMTH,ACTSEL),X'F706000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTMTH,ACTLIST),X'F606000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTPER,ACTADD),X'F908000080'         PERSON             
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPER,ACTCHA),X'F908000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPER,ACTDIS),X'F908000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPER,ACTSEL),X'F908000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPER,ACTLIST),X'F808000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPER,ACTDEL),X'F908000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTPR2,ACTCHA),X'FB08000080'         PER2               
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPR2,ACTDIS),X'FB08000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTSTD,ACTADD),X'E211000080'         STD HOUR           
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTSTD,ACTCHA),X'E211000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTSTD,ACTDIS),X'E211000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTSTD,ACTSEL),X'E211000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTSTD,ACTLIST),X'E111000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTEDT,ACTADD),X'E211000080'         EDIT HOUR          
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTEDT,ACTCHA),X'E211000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTEDT,ACTDIS),X'E211000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTEDT,ACTSEL),X'E211000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTEDT,ACTLIST),X'E111000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTDAY,ACTCHA),X'EA12000080'         DAY HOUR           
         DC    C'    ',AL1(RTEDT,0,0,0)                                         
         DC    X'03',AL1(RTDAY,ACTDIS),X'EA12000080'                            
         DC    C'    ',AL1(RTEDT,0,0,0)                                         
*                                                                               
         DC    X'03',AL1(RTHIS,ACTADD),X'F302000080'         HISTORY            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTHIS,ACTCHA),X'F302000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTHIS,ACTDIS),X'F302000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTHIS,ACTSEL),X'F302000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTHIS,ACTLIST),X'F202000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTHIS,ACTDEL),X'D401000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTHIS,8),X'D401000081'                                 
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTPRO,ACTADD),X'E514000081'         PROFILE            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPRO,ACTCHA),X'E514000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPRO,ACTDIS),X'E514000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPRO,ACTSEL),X'E514000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPRO,ACTLIST),X'E414000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTPRO,ACTREP),X'E315001578'                            
         DC    C'PFPF',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTPAY,ACTLIST),X'FA0A000080'        PAYROLL            
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTMAD,11),X'F404000081'             MAD                
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTRAT,ACTDIS),X'F010000080'         CRATES             
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTCAL,ACTADD),X'FC05000081'         CALENDAR           
         DC    C'    ',AL1(0,SECDDS,0,0)                                        
         DC    X'03',AL1(RTCAL,ACTCHA),X'FC05000081'                            
         DC    C'    ',AL1(0,SECDDS,0,0)                                        
         DC    X'03',AL1(RTCAL,ACTDIS),X'FC05000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTCAL,ACTSEL),X'FC05000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTCAL,ACTLIST),X'D107000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTTIM,ACTADD),X'C030000081'         TIME               
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTTIM,ACTCHA),X'C130000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTTIM,ACTDIS),X'C130000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
*&&UK                                                                           
         DC    X'03',AL1(RTTIM,ACTDEL),X'C130000081'                            
         DC    C'    ',AL1(0,0,0,0)                                             
*&&                                                                             
         DC    X'03',AL1(RTTIM,ACTSEL),X'C130000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTTIM,ACTLIST),X'D230000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTTIM,ACTREP),X'D330003040'                            
         DC    C'TSTS',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTOVH,ACTADD),X'E716000080'         OVERHEAD           
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTOVH,ACTCHA),X'E716000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTOVH,ACTDIS),X'E716000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTOVH,ACTSEL),X'E716000080'                            
         DC    C'    ',AL1(0,0,0,0)                                             
         DC    X'03',AL1(RTOVH,ACTLIST),X'E616000080'                           
         DC    C'    ',AL1(0,0,0,0)                                             
*                                                                               
         DC    X'03',AL1(RTTHD,ACTADD),X'C730000081'         TEMPO              
         DC    C'    ',AL1(RTTIM,0,0,0)                      HEADER             
         DC    X'03',AL1(RTTHD,ACTDIS),X'C730000081'                            
         DC    C'    ',AL1(RTTIM,0,0,0)                                         
         DC    X'03',AL1(RTTHD,ACTCHA),X'C730000081'                            
         DC    C'    ',AL1(RTTIM,0,0,0)                                         
         DC    X'03',AL1(RTTDT,ACTADD),X'CA30000081'         TEMPO              
         DC    C'    ',AL1(RTTIM,0,0,0)                      DETAIL             
         DC    X'03',AL1(RTTDT,ACTDIS),X'CA30000081'                            
         DC    C'    ',AL1(RTTIM,0,0,0)                                         
         DC    X'03',AL1(RTTCM,ACTADD),X'CB30000081'         TEMPO              
         DC    C'    ',AL1(RTTIM,0,0,0)                      COMMUTER           
         DC    X'03',AL1(RTTCM,ACTDIS),X'CB30000081'                            
         DC    C'    ',AL1(RTTIM,0,0,0)                                         
         DC    X'03',AL1(RTTLN,ACTADD),X'CC30000081'         TEMPO              
         DC    C'    ',AL1(RTTIM,0,0,0)                      LINE               
         DC    X'03',AL1(RTTLN,ACTDIS),X'CC30000081'                            
         DC    C'    ',AL1(RTTIM,0,0,0)                                         
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        PFKEY TABLE                                                  *         
***********************************************************************         
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
*        PERSON LIST                                                            
*                                                                               
         DC    AL1(PPF01X-*,01,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#CPRSN,8                                                       
         DCDD  AC#LIST,8                                                        
PPF01X   EQU   *                                                                
*                                                                               
*        MAD INPUT                                                              
*                                                                               
         DC    AL1(PPF06X-*,06,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#MAD,8                                                         
         DCDD  AC#INP,8                                                         
PPF06X   EQU   *                                                                
*                                                                               
*        PROFILE LIST                                                           
*                                                                               
         DC    AL1(PPF09X-*,09,PFTCPROG,0,0)                                    
         DC    CL3' '                                                           
         DCDD  AC#PRFL,8                                                        
         DCDD  AC#LIST,8                                                        
PPF09X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLE OF SCREENS AND THE A(PFKEY LINE TABLE)                 *         
***********************************************************************         
*                                                                               
SCRTABL  DS    0C                                                               
         DC    X'C2',AL4(PFLINEC2)                                              
         DC    X'C3',AL4(PFLINEC3)                                              
         DC    X'C4',AL4(PFLINEC4)                                              
         DC    X'C5',AL4(PFLINEC5)                                              
         DC    X'C6',AL4(PFLINEC6)                                              
         DC    X'C7',AL4(PFLINEC7)                                              
         DC    X'C8',AL4(PFLINEC8)                                              
         DC    X'C9',AL4(PFLINEC9)                                              
         DC    X'CA',AL4(PFLINECA)                                              
         DC    X'CB',AL4(PFLINECB)                                              
         DC    X'CC',AL4(PFLINECC)                                              
         DC    X'D1',AL4(PFLINED1)                                              
         DC    X'D2',AL4(PFLINED2)                                              
         DC    X'D3',AL4(PFLINED3)                                              
         DC    X'D4',AL4(PFLINED4)                                              
         DC    X'F2',AL4(PFLINEF2)                                              
         DC    X'F3',AL4(PFLINEF3)                                              
         DC    X'F4',AL4(PFLINEF4)                                              
         DC    X'F5',AL4(PFLINEF5)                                              
         DC    X'F6',AL4(PFLINEF6)                                              
         DC    X'F7',AL4(PFLINEF7)                                              
         DC    X'F8',AL4(PFLINEF8)                                              
         DC    X'F9',AL4(PFLINEF9)                                              
         DC    X'FB',AL4(PFLINEFB)                                              
         DC    X'FC',AL4(PFLINEFC)                                              
         DC    X'F0',AL4(PFLINEF0)                                              
         DC    X'FA',AL4(PFLINEFA)                                              
         DC    X'00',AL4(PFLINEFF)                                              
         DC    X'E1',AL4(PFLINEE1)                                              
         DC    X'E4',AL4(PFLINEE4)                                              
         DC    X'E5',AL4(PFLINEE5)                                              
         DC    X'E6',AL4(PFLINEE6)                                              
         DC    X'E7',AL4(PFLINEE7)                                              
         DC    X'EA',AL4(PFLINEEA)                                              
         DC    X'FFFF'                                                          
*                                                                               
SCRTABL2 DS    0C                                                               
         DC    X'C2',AL4(PFLINXC2)                                              
         DC    X'C3',AL4(PFLINXC3)                                              
         DC    X'C4',AL4(PFLINXC4)                                              
         DC    X'C5',AL4(PFLINXC5)                                              
         DC    X'C6',AL4(PFLINXC6)                                              
         DC    X'C8',AL4(PFLINXC8)                                              
         DC    X'C9',AL4(PFLINXC9)                                              
         DC    X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
*        PFKEYS TABLES PER SCREEN FOR BUILDING PFKEY DISPLAY LINE     *         
***********************************************************************         
*                                                                               
PFLINEF2 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTPER,ACTDIS,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,10)                                            
         DCDD  AC#CPERD,10         Person/Dis                                   
         DC    CL(15-10)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTDIS,X'F3',PFLSSEC+PFLSRET)                          
         DC    CL6'PF3=  ',AL1(4,11)                                            
         DCDD  AC#CHISD,11         History/Dis                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(RTMTH,ACTLIST,X'F6',PFLSSEC+PFLSRET)                         
         DC    CL6'PF10= ',AL1(5,6)                                             
         DCDD  AC#METH,6           Method                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF3 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,12)                                            
         DCDD  AC#CHISL,12         History/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'Enter=',AL1(6,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF4 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,7)                                             
         DCDD  AC#CPERL,7          Per/List                                     
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTPER,ACTDIS,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,7)                                             
         DCDD  AC#CPERD,7          Per/Dis                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTHIS,ACTDIS,X'F3',PFLSSEC+PFLSRET)                          
         DC    CL6'PF3=  ',AL1(4,8)                                             
         DCDD  AC#CHISD,8          Hist/Dis                                     
         DC    CL(15-8)' '                                                      
*                                                                               
         DC    AL1(RTPAY,ACTLIST,X'FA',PFLSSEC+PFLSRET)                         
         DC    CL6'PF4=  ',AL1(4,7)                                             
         DCDD  AC#CPAYL,7          Pay/Lis                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTCAL,ACTDIS,X'F5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF5=  ',AL1(4,3)                                             
         DCDD  AC#CAL,4            Cal                                          
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF6=  ',AL1(4,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(RTRAT,ACTDIS,X'F0',PFLSSEC+PFLSRET)                          
         DC    CL6'PF10= ',AL1(5,6)                                             
         DCDD  AC#CRATS,6          CRates                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF5 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,3)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF6 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,12)                                            
         DCDD  AC#CHISL,12         History/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF7 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,12)                                            
         DCDD  AC#CHISL,12         History/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(RTMTH,ACTLIST,X'F6',PFLSSEC+PFLSRET)                         
         DC    CL6'PF10= ',AL1(5,11)                                            
         DCDD  AC#CMETL,11         Method/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'Enter=',AL1(6,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF8 DC    AL1(RTPER,ACTDIS,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,10)                                            
         DCDD  AC#CPERD,10         Person/disp                                  
         DC    CL(15-10)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,9)                                             
         DCDD  AC#CHISL,9          Hist/List                                    
         DC    CL(15-9)' '                                                      
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(RTRAT,ACTDIS,X'F0',PFLSSEC+PFLSRET)                          
         DC    CL6'PF10= ',AL1(5,6)                                             
         DCDD  AC#CRATS,6          Crates                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF9 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
*                                                                               
         DC    AL1(RTPR2,ACTDIS,X'FB',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,8)                                             
         DCDD  AC#CPR2D,8          Per2/Dis                                     
         DC    CL(15-8)' '                                                      
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'3=    ',AL1(2,12)                                            
         DCDD  AC#CHISL,12         History/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'6=    ',AL1(2,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             UP                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           DOWN                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(RTRAT,ACTDIS,X'F0',PFLSSEC+PFLSRET)                          
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#CRATS,6          CRates                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'Enter=',AL1(6,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEFB DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTPER,ACTDIS,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,6)                                             
         DCDD  AC#CPRSN,6          Person                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,12)                                            
         DCDD  AC#CHISL,12         History/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(RTRAT,ACTDIS,X'F0',PFLSSEC+PFLSRET)                          
         DC    CL6'PF10= ',AL1(5,6)                                             
         DCDD  AC#CRATS,6          CRates                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF11= ',AL1(5,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEF0 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTPER,ACTDIS,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,10)                                            
         DCDD  AC#CPERD,10         Person/Dis                                   
         DC    CL(15-10)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,12)                                            
         DCDD  AC#CHISL,12         History/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEFA DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,9)                                             
         DCDD  AC#CHISL,9          Hist/List                                    
         DC    CL(15-9)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF5=  ',AL1(4,6)                                             
         DCDD  AC#CBOTT,6          Bottom                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEFC DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,3)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'ENTER=',AL1(6,4)                                             
         DCDD  AC#NXT,4            NEXT                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEE1 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF2=  ',AL1(4,6)                                             
         DCDD  AC#CHG,6            Change                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF3=  ',AL1(4,7)                                             
         DCDD  AC#DSP,7            Display                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEE4 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(RTPRO,ACTDIS,X'E5',PFLSSEC+PFLSRET)                          
         DC    CL6'PF9=  ',AL1(4,11)                                            
         DCDD  AC#CPROD,11         Profile/Dis                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTMTH,ACTLIST,X'F6',PFLSSEC+PFLSRET)                         
         DC    CL6'PF10= ',AL1(5,6)                                             
         DCDD  AC#METH,6           Method                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEE5 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,10)                                            
         DCDD  AC#CPERL,10         Person/Lis                                   
         DC    CL(15-10)' '                                                     
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(RTPRO,ACTLIST,X'E4',PFLSSEC+PFLSRET)                         
         DC    CL6'PF9=  ',AL1(4,9)                                             
         DCDD  AC#CPROL,9          Prof/List                                    
         DC    CL(15-9)' '                                                      
*                                                                               
         DC    AL1(RTMTH,ACTLIST,X'F6',PFLSSEC+PFLSRET)                         
         DC    CL6'PF10= ',AL1(5,6)                                             
         DCDD  AC#METH,6           Method                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'Enter=',AL1(6,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEE6 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTPER,ACTDIS,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,10)                                            
         DCDD  AC#CPERD,10         Person/Dis                                   
         DC    CL(15-10)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTDIS,X'F3',PFLSSEC+PFLSRET)                          
         DC    CL6'PF3=  ',AL1(4,11)                                            
         DCDD  AC#CHISD,11         History/Dis                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(RTMTH,ACTLIST,X'F6',PFLSSEC+PFLSRET)                         
         DC    CL6'PF10= ',AL1(5,6)                                             
         DCDD  AC#METH,6           Method                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEE7 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTHIS,ACTLIST,X'F2',PFLSSEC+PFLSRET)                         
         DC    CL6'PF3=  ',AL1(4,12)                                            
         DCDD  AC#CHISL,12         History/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(RTOVH,ACTLIST,X'E6',PFLSSEC+PFLSRET)                         
         DC    CL6'PF4=  ',AL1(4,13)                                            
         DCDD  AC#COVHL,13         Overhead/List                                
         DC    CL(15-13)' '                                                     
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'Enter=',AL1(6,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEE9 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTDAY,ACTCHA,X'EA',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,6)                                             
         DCDD  AC#CHG,6            Change                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(RTDAY,ACTDIS,X'EA',PFLSSEC+PFLSRET)                          
         DC    CL6'PF3=  ',AL1(4,7)                                             
         DCDD  AC#DSP,7            Display                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
PFLINEEA DC    AL1(RTEDT,ACTLIST,X'E1',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#EDHRL,11         Edit hrs/List                                
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTEDT,ACTCHA,X'E2',PFLSSEC)                                  
         DC    CL6'PF2=  ',AL1(4,11)                                            
         DCDD  AC#EDHRC,11         Edit hrs/Change                              
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTEDT,ACTDIS,X'E2',PFLSSEC)                                  
         DC    CL6'PF3=  ',AL1(4,11)                                            
         DCDD  AC#EDHRD,11         Edit hrs/Display                             
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
PFLINEFF DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTMAD,11,X'F4',PFLSSEC+PFLSRET)                              
         DC    CL6'PF6=  ',AL1(4,3)                                             
         DCDD  AC#MAD,3            MAD                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(RTPRO,ACTLIST,X'E4',PFLSSEC+PFLSRET)                         
         DC    CL6'PF9=  ',AL1(4,12)                                            
         DCDD  AC#CPROL,12         Profile/List                                 
         DC    CL(15-12)' '                                                     
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC2 DS    0H                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF1=  ',AL1(4,3)                                             
         DCDD  AC#BDPL,3           BD+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'3=    ',AL1(2,3)                                             
         DCDD  AC#NBPL,3           NB+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'4=    ',AL1(2,1)                                             
         DCDD  AC#B,3              B                                            
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'5=    ',AL1(2,2)                                             
         DCDD  AC#BPL,3            B+                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'6=    ',AL1(2,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'9=    ',AL1(2,4)                                             
         DCDD  AC#SAVE,4           Save                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#UNMRK,6          UNMARK                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'11=   ',AL1(3,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'12=   ',AL1(3,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'13=   ',AL1(3,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINXC2 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF13= ',AL1(5,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'14=   ',AL1(3,7)                                             
         DCDD  AC#RFRSH,7          Refresh                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'15=   ',AL1(3,3)                                             
         DCDD  AC#TAX,3            Tax                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'19=   ',AL1(3,5)                                             
         DCDD  AC#TEMP,5            TEMPO                                       
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC3 DS    0H                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF1=  ',AL1(4,3)                                             
         DCDD  AC#BDPL,3           BD+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'2=    ',AL1(2,2)                                             
         DCDD  AC#NB,3             NB                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'4=    ',AL1(2,1)                                             
         DCDD  AC#B,3              B                                            
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'5=    ',AL1(2,2)                                             
         DCDD  AC#BPL,3            B+                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'6=    ',AL1(2,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'9=    ',AL1(2,4)                                             
         DCDD  AC#SAVE,4           Save                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#UNMRK,6          UNMARK                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'11=   ',AL1(3,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'12=   ',AL1(3,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'13=   ',AL1(3,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINXC3 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF13= ',AL1(5,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'14=   ',AL1(3,7)                                             
         DCDD  AC#RFRSH,7          Refresh                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'15=   ',AL1(3,3)                                             
         DCDD  AC#TAX,3            Tax                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'19=   ',AL1(3,5)                                             
         DCDD  AC#TEMP,5            TEMPO                                       
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC4 DS    0H                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF1=  ',AL1(4,3)                                             
         DCDD  AC#BDPL,3           BD+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'2=    ',AL1(2,2)                                             
         DCDD  AC#NB,3             NB                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'3=    ',AL1(2,3)                                             
         DCDD  AC#NBPL,3           NB+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'4=    ',AL1(2,1)                                             
         DCDD  AC#B,3              B                                            
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'6=    ',AL1(2,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'9=    ',AL1(2,4)                                             
         DCDD  AC#SAVE,4           Save                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#UNMRK,6          UNMARK                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'11=   ',AL1(3,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'12=   ',AL1(3,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'13=   ',AL1(3,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINXC4 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF13= ',AL1(5,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'14=   ',AL1(3,7)                                             
         DCDD  AC#RFRSH,7          Refresh                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'15=   ',AL1(3,3)                                             
         DCDD  AC#TAX,3            Tax                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'19=   ',AL1(3,5)                                             
         DCDD  AC#TEMP,5           TEMPO                                        
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC5 DS    0H                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF1=  ',AL1(4,3)                                             
         DCDD  AC#BDPL,3           BD+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'2=    ',AL1(2,2)                                             
         DCDD  AC#NB,3             NB                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'3=    ',AL1(2,3)                                             
         DCDD  AC#NBPL,3           NB+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'5=    ',AL1(2,2)                                             
         DCDD  AC#BPL,3            B+                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'6=    ',AL1(2,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'9=    ',AL1(2,4)                                             
         DCDD  AC#SAVE,4           Save                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#UNMRK,6          UNMARK                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'11=   ',AL1(3,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'12=   ',AL1(3,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'13=   ',AL1(3,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINXC5 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF13= ',AL1(5,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'14=   ',AL1(3,7)                                             
         DCDD  AC#RFRSH,7          Refresh                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'15=   ',AL1(3,3)                                             
         DCDD  AC#TAX,3            Tax                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'19=   ',AL1(3,5)                                             
         DCDD  AC#TEMP,5           TEMPO                                        
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC6 DS    0H                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF1=  ',AL1(4,3)                                             
         DCDD  AC#BDPL,3           BD+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'2=    ',AL1(2,2)                                             
         DCDD  AC#NB,3             NB                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'3=    ',AL1(2,3)                                             
         DCDD  AC#NBPL,3           NB+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'4=    ',AL1(2,1)                                             
         DCDD  AC#B,3              B                                            
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'5=    ',AL1(2,2)                                             
         DCDD  AC#BPL,3            B+                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'6=    ',AL1(2,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'9=    ',AL1(2,4)                                             
         DCDD  AC#SAVE,4           Save                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#UNMRK,6          UNMARK                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'11=   ',AL1(3,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'12=   ',AL1(3,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'13=   ',AL1(3,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINXC6 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF13= ',AL1(5,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'14=   ',AL1(3,7)                                             
         DCDD  AC#RFRSH,7          Refresh                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'15=   ',AL1(3,3)                                             
         DCDD  AC#TAX,3            Tax                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'19=   ',AL1(3,5)                                             
         DCDD  AC#TEMP,5            TEMPO                                       
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC7 DS    0H                                                               
         DC    AL1(RTTCM,0,X'CB',PFLSSEC+PFLSRET)                               
         DC    CL6'PF2=  ',AL1(4,4)                                             
         DCDD  AC#TMPCM,4           COMMUTER CODE SCREEN                        
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(RTTDT,0,X'CA',0)                                             
         DC    CL6'PF3=  ',AL1(4,7)                                             
         DCDD  AC#TMPDT,7           LINE DETAIL SCREEN                          
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTTLN,0,X'CC',0)                                             
         DC    CL6'PF4=  ',AL1(4,5)                                             
         DCDD  AC#TMPLN,5           LINE INFO SCREEN                            
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12=   ',AL1(5,6)                                           
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC8 DS    0H                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF1=  ',AL1(4,3)                                             
         DCDD  AC#BDPL,3           BD+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'2=    ',AL1(2,2)                                             
         DCDD  AC#NB,3             NB                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'3=    ',AL1(2,3)                                             
         DCDD  AC#NBPL,3           NB+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'4=    ',AL1(2,1)                                             
         DCDD  AC#B,3              B                                            
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'5=    ',AL1(2,2)                                             
         DCDD  AC#BPL,3            B+                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'6=    ',AL1(2,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'9=    ',AL1(2,4)                                             
         DCDD  AC#SAVE,4           Save                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#UNMRK,6          UNMARK                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'11=   ',AL1(3,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'12=   ',AL1(3,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'13=   ',AL1(3,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINXC8 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF13= ',AL1(5,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'14=   ',AL1(3,7)                                             
         DCDD  AC#RFRSH,7          Refresh                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'15=   ',AL1(3,3)                                             
         DCDD  AC#TAX,3            Tax                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINEC9 DS    0H                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF2=    ',AL1(4,2)                                           
         DCDD  AC#NB,3             NB                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'3=    ',AL1(2,3)                                             
         DCDD  AC#NBPL,3           NB+                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'4=    ',AL1(2,1)                                             
         DCDD  AC#B,3              B                                            
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'5=    ',AL1(2,2)                                             
         DCDD  AC#BPL,3            B+                                           
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'6=    ',AL1(2,6)                                             
         DCDD  AC#UPD,6            Update                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'7=    ',AL1(2,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'8=    ',AL1(2,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'9=    ',AL1(2,4)                                             
         DCDD  AC#SAVE,4           Save                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'10=   ',AL1(3,6)                                             
         DCDD  AC#UNMRK,6          UNMARK                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNXTS)                                              
         DC    CL6'11=   ',AL1(3,4)                                             
         DCDD  AC#NXT,4            Next                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'12=   ',AL1(3,6)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'13=   ',AL1(3,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINXC9 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF13= ',AL1(5,6)                                             
         DCDD  AC#ALTPF,6          ALTPFS                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'14=   ',AL1(3,7)                                             
         DCDD  AC#RFRSH,7          Refresh                                      
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'15=   ',AL1(3,3)                                             
         DCDD  AC#TAX,3            Tax                                          
         DC    CL(15-3)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSDDS)                                               
         DC    CL6'19=   ',AL1(3,5)                                             
         DCDD  AC#TEMP,5           TEMPO                                        
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINECA DS    0H                                                               
         DC    AL1(RTTHD,0,X'C7',PFLSSEC+PFLSRET)                               
         DC    CL6'PF1=  ',AL1(4,7)                                             
         DCDD  AC#TMPHD,7           HEADER SCREEN                               
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTTCM,0,X'CB',0)                                             
         DC    CL6'PF2=  ',AL1(4,4)                                             
         DCDD  AC#TMPCM,4           COMMUTER CODE SCREEN                        
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(RTTLN,0,X'CC',0)                                             
         DC    CL6'PF4=  ',AL1(4,5)                                             
         DCDD  AC#TMPLN,5           LINE INFO SCREEN                            
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12=   ',AL1(5,6)                                           
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINECB DS    0H                                                               
         DC    AL1(RTTHD,0,X'C7',PFLSSEC+PFLSRET)                               
         DC    CL6'PF1=  ',AL1(4,7)                                             
         DCDD  AC#TMPHD,7           HEADER SCREEN                               
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTTDT,0,X'CA',0)                                             
         DC    CL6'PF3=  ',AL1(4,7)                                             
         DCDD  AC#TMPDT,7           LINE DETAIL SCREEN                          
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTTLN,0,X'CC',0)                                             
         DC    CL6'PF4=  ',AL1(4,5)                                             
         DCDD  AC#TMPLN,5           LINE INFO SCREEN                            
         DC    CL(15-5)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12=   ',AL1(5,6)                                           
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINECC DS    0H                                                               
         DC    AL1(RTTHD,0,X'C7',PFLSSEC+PFLSRET)                               
         DC    CL6'PF1=  ',AL1(4,7)                                             
         DCDD  AC#TMPHD,7           HEADER SCREEN                               
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(RTTCM,0,X'CB',0)                                             
         DC    CL6'PF2=  ',AL1(4,4)                                             
         DCDD  AC#TMPCM,4           COMMUTER CODE SCREEN                        
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(RTTDT,0,X'CA',0)                                             
         DC    CL6'PF3=  ',AL1(4,7)                                             
         DCDD  AC#TMPDT,7           LINE DETAIL SCREEN                          
         DC    CL(15-7)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12=   ',AL1(5,6)                                           
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINED2 DC    AL1(RTPER,ACTLIST,X'F8',PFLSSEC+PFLSRET)                         
         DC    CL6'PF1=  ',AL1(4,11)                                            
         DCDD  AC#CPERL,11         Person/List                                  
         DC    CL(15-11)' '                                                     
*                                                                               
         DC    AL1(RTPER,ACTDIS,X'F9',PFLSSEC+PFLSRET)                          
         DC    CL6'PF2=  ',AL1(4,10)                                            
         DCDD  AC#CPERD,10         Person/Dis                                   
         DC    CL(15-10)' '                                                     
*                                                                               
         DC    AL1(0,0,0,PFLSSEC)                                               
         DC    CL6'PF3=  ',AL1(4,11)                                            
         DCDD  AC#CTIMR,11         TIME/REPORT                                  
         DC    CL(15-11)' '                                                     
*                                                                               
*        DC    AL1(0,0,0,PFLSNRET)                                              
*        DC    CL6'PF12= ',AL1(5,6)                                             
*        DCDD  AC#RETRN,6          Return                                       
*        DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINED3 DC    AL1(RTTIM,ACTLIST,X'D2',PFLSSEC)                                 
         DC    CL6'PF1=  ',AL1(4,9)                                             
         DCDD  AC#CTIML,9          TIME/LIST                                    
         DC    CL(15-9)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINED1 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,PFLSNRET)                                              
         DC    CL6'PF12= ',AL1(5,3)                                             
         DCDD  AC#RETRN,6          Return                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    X'FF'                                                            
*                                                                               
PFLINED4 DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF3=  ',AL1(4,4)                                             
         DCDD  AC#QUIT,4           QUIT                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF6=  ',AL1(4,6)                                             
         DCDD  AC#UPD,6            UPDATE                                       
         DC    CL(15-6)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF7=  ',AL1(4,2)                                             
         DCDD  AC#UP,4             Up                                           
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    AL1(0,0,0,0)                                                     
         DC    CL6'PF8=  ',AL1(4,4)                                             
         DCDD  AC#DOWN,4           Down                                         
         DC    CL(15-4)' '                                                      
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
*        SCREENS                                                     *          
**********************************************************************          
*                                                                               
       ++INCLUDE ACCAPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPFBD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE ACCAPE2D                                                       
         ORG   STDTAGH                                                          
       ++INCLUDE ACCAPEBD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
**********************************************************************          
*        ACCAPDESCT AND ACCAPWORKD                                   *          
**********************************************************************          
*                                                                               
       ++INCLUDE ACCAPDSECT                                                     
         EJECT                                                                  
       ++INCLUDE ACCAPWORKD                                                     
         EJECT                                                                  
**********************************************************************          
*        OTHER DSECTS                                                *          
**********************************************************************          
*                                                                               
PFLISTD  DSECT                                                                  
PFLREC   DS    XL1                 RECORD EQUATE                                
PFLACT   DS    XL1                 ACTION EQUATE                                
PFLSCR   DS    XL1                 SCREEN THAT WOULD BE LOADED                  
PFLSTAT  DS    XL1                                                              
PFLSSEC  EQU   X'80'               VALIDATE SECURITY                            
PFLSRET  EQU   X'40'               VALIDATE NOT = RETURN SCREEN                 
PFLSNRET EQU   X'20'               IS RETURN VALID                              
PFLSNXTS EQU   X'10'               ARE THERE ANY PENDING SELECTS                
PFLSDDS  EQU   X'08'               DISPLAY ONLY ON DDS TERMINAL                 
PFLS2ND  EQU   X'04'               DISPLAY 2ND LINE OF PFKEYS                   
PFLPF#   DS    CL6                 PF##=                                        
PFLPF#LN DS    XL1                 LEN TO MOVE FOR PF#=                         
PFLDLEN  DS    XL1                 DESCRIPTION LENGTH                           
PFLDESC  DS    CL15                DATA DICT DESCRIPTION                        
PFLLENQ  EQU   *-PFLISTD                                                        
         EJECT                                                                  
**********************************************************************          
*        EQUATES                                                     *          
**********************************************************************          
*                                                                               
LENSPOOL EQU   SPOOLEND-SPOOLD                                                  
LENGEND  EQU   GENDEND-GEND                                                     
LENSYSD  EQU   SYSDEND-SYSD                                                     
LENWORK  EQU   LENSPOOL+LENGEND+LENSYSD+LENIOAS+DISBLKLN                        
         EJECT                                                                  
**********************************************************************          
*        OTHER INCLUDES                                              *          
**********************************************************************          
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDLANGEQUS                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE ACOFFALD                                                       
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FAXTRAINF                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'109ACCAP00   03/26/12'                                      
         END                                                                    
