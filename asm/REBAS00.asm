*          DATA SET REBAS00    AT LEVEL 255 AS OF 05/01/02                      
*PHASE T82400A,*                                                                
*INCLUDE UNBOOK                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE UNUPGR                                                                 
*INCLUDE RETEXT                                                                 
*INCLUDE UNTEXT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE UPOUT                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T82400 - REBAS00 - REP BASIC (BAS)'                             
*                                                                               
*******************************************************************             
*                                                                 *             
*        REBAS00 --- REP BASIC BASE                               *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* 3/17/97 (DBU) - ORIGINAL ENTRY (USED RESFM00 AS A MODEL)        *             
*                                                                 *             
*******************************************************************             
*                                                                               
T82400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T82400,R7,R5,RR=R2,CLEAR=YES                           
         USING GEND,RC                                                          
         SPACE 1                                                                
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         SPACE 1                                                                
         ST    R2,RELO                                                          
         ST    RD,SAVERD                                                        
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(LENIOAS)      R9=A(BAS SYSTEM WORKING STORAGE)             
*                                  GRABBING 3 2096 BYTE I/O AREAS               
         SPACE 1                                                                
         ST    R1,SYSPARMS                                                      
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
         SPACE 1                                                                
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
         SPACE 1                                                                
         L     RA,4(R1)                                                         
         ST    RA,ATWA             RA=A(TWA)                                    
         USING CONHEADH-64,RA                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         SPACE 1                                                                
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    R7,SYSR7            SECOND BASE REGISTER                         
         ST    R5,SYSR5            THIRD BASE REGISTER                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
BAS20    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0         GO TO CALLOV                                 
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,BAS20                                                         
*                                                                               
         MVI   DMCB+7,QSTAPACK                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSTAPACK,DMCB        SAVE MODULE ADDRESS                         
*                                                                               
         LA    R0,GOMSPACK                                                      
         ST    R0,MSPACK                                                        
         LA    R0,GOMSUNPK                                                      
         ST    R0,MSUNPK                                                        
*                                                                               
         BAS   RE,CKGLOB                                                        
         SPACE 1                                                                
*        BAS   RE,SETPROFS                                                      
*        SPACE 1                                                                
         MVI   RETURNED,0          HELPS DETERMINE FROM WHENCE WE CAME          
         SPACE 1                                                                
         MVI   RACHANG,C'N'        SET FLAG TO IDICATE IF USER                  
         TM    CONRECH+4,X'20'     CHANGED RECORD/ACTION                        
         BZ    *+12                                                             
         TM    CONACTH+4,X'20'                                                  
         BO    *+8                                                              
         MVI   RACHANG,C'Y'                                                     
         SPACE 2                                                                
* CALL A ROUTINE THAT CALLS GENCON SO THAT GENCON WILL RETURN TO                
* NEXT INSTRUCTION AFTER THIS                                                   
* DON'T GO AGAIN UNLESS OVERLAY WANTS TO CALL ANOTHER OVERLAY                   
         SPACE 1                                                                
AGAIN    MVI   GOAGAIN,C'N'                                                     
         BAS   RE,CALLGENC                                                      
         SPACE 1                                                                
* IF OVERLAY WISHED TO CALL GENCON WITH A NEW RECORD AND ACTION,                
* THEN THIS FLAG WILL BE SET                                                    
         SPACE 1                                                                
         CLI   GOAGAIN,C'Y'                                                     
         BNE   DONE                                                             
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT HERE                 
         B     AGAIN                                                            
         SPACE 1                                                                
DONE     OI    CONRECH+4,X'20'                                                  
         OI    CONACTH+4,X'20'                                                  
         B     XIT                 ELSE EXIT BACK TO USER                       
         EJECT                                                                  
*THIS ROUTINE CALL GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON               
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.                
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW                    
* RECORD AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION              
* AND A SELECTION WAS MADE.                                                     
         SPACE 2                                                                
CALLGENC NTR1                                                                   
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
         SPACE 1                                                                
         GOTO1 GENCON,DMCB,(R8)    CALL GENCON-PASS A(WORKING STORAGE)          
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
*              INITIALIZE SYSTEM DEPENDENT VALUES                               
         SPACE 3                                                                
SYSINIT  NTR1                                                                   
         LA    R1,SECBLK           SET ADDRESS OF SECRET BLOCK                  
         ST    R1,ASECBLK                                                       
*                                                                               
         MVI   CTRLMAIN,C'N'       (RE)SET SYSTEM SE# FLAG                      
*              GET TERMINAL VALUES                                              
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
         SPACE 1                                                                
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
         SPACE 1                                                                
SYS20    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS20                                                         
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R1,VCOUNT                                                        
         SPACE 1                                                                
SYS40    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R1,SYS40                                                         
         SPACE 1                                                                
*                                  SET SYSTEM DEPENDENT VALUES                  
         SPACE 1                                                                
         LA    R1,STARTSAV                                                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'81'          SAVE 1 LARGE TWA                             
         MVI   SYSTEM,C'R'         REP                                          
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2096 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETREP      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'27'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         MVI   GETMSYS,8           USES GETMSG FOR SYSTEM 8                     
         MVC   LWORK,=Y(LENWORK)   SPACE TAKEN IN NMOD                          
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9082400'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
**       CLI   TWAOFFC,C'*'        IF NOT AT DDS                                
**       BE    *+12                DON'T ALLOW AVAILS FOR NOW                   
**       LA    R1,RECACT1                                                       
**       ST    R1,ARECACT1                                                      
                                                                                
         LA    R1,RECACT3          BAS X'03' ENTRIES                            
         ST    R1,ARECACT3                                                      
                                                                                
         SPACE 1                                                                
* SET UP CERTAIN ROUTINE ADDRESSES - CAN'T WAIT FOR GENCON                      
         SPACE 1                                                                
SYS50    L     R1,SYSPARMS                                                      
         LM    R2,R4,8(R1)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMAND,CDEMAND                                                   
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   DEMOUT,CDEMOUT                                                   
         SPACE                                                                  
         OI    GENSTAT3,USEKEYSV   USE KEYSAVE,KEY (INTEREP)                    
         SPACE 1                                                                
*   IF ONLINE,CHECK AUTHORIZATION VALUES FOR ACTION                             
         OC    TWAVPRNT,TWAVPRNT                                                
         BNZ   XIT                                                              
*                                                                               
* FOR RECORD DR (DIRECT RESPONSE) AND RECORD GOALN,                             
*     ACTIONS LIST AND REPORT ARE VALID FOR ALL                                 
*                                                                               
         CLC   =C'GOALN',CONREC    GOALN?                                       
         BE    INIT0020            YES - CHECK FOR REPORT OR LIST               
         CLC   =C'DR',CONREC       DR?                                          
         BE    INIT0020            YES - CHECK FOR REPORT OR LIST               
         CLC   =C'GOAL',CONREC     GOAL?                                        
         BE    INIT0040            YES - CHECK FOR LIST ONLY                    
         CLC   =C'DRN',CONREC      DRN?                                         
         BE    INIT0040            YES - CHECK FOR LIST ONLY                    
         B     INIT0080            ALL OTHERS - FULL CHECK                      
INIT0020 EQU   *                                                                
         CLC   =C'REP',CONACT                                                   
         BE    XIT                                                              
INIT0040 EQU   *                                                                
         LA    RF,CONACTH          A(ACTION FIELD HEADER)                       
         ZIC   RE,5(RF)            LOAD LENGTH OF INPUT                         
         BCTR  RE,0                                                             
         EX    RE,INIT0060         COMPARE ACTION TO 'LIST'                     
         BE    XIT                 EQUAL:  ACCEPTED                             
         B     INIT0080            NOT EQUAL:  CHECK RESTRICTS                  
INIT0060 CLC   8(0,RF),=C'LIST'                                                 
INIT0080 EQU   *                                                                
         SPACE 1                                                                
         LA    R1,AUTHTAB          SEE IF ACTION HAS RESTRICTIONS               
         LA    R2,CONRECH                                                       
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
INIT0100 EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),8(R2)                                                    
         BE    INIT0120                                                         
         CLI   0(R1),X'FF'                                                      
         BE    XIT                 NO RESTRICTIONS                              
         LA    R1,L'AUTHTAB(R1)                                                 
         B     INIT0100                                                         
         SPACE 1                                                                
INIT0120 MVC   BYTE,8(R1)          NOW SEE IF USER MEETS RESTRICTIONS           
         NC    BYTE,TWAAUTH                                                     
         BNZ   XIT                                                              
*                                                                               
INIT0140 DS    0H                                                               
         XC    CONHEAD,CONHEAD     NEED TO DO MY OWN ERR MSG                    
         MVC   CONHEAD(L'NOAUTH),NOAUTH                                         
         OI    6(R2),X'40'         POSITION CURSOR                              
         OI    CONHEADH+6,X'80'     TRANSMIT HEADER                             
         L     RD,SAVERD           BACK OUT ALL THE WAY                         
         B     XIT                                                              
         SPACE 1                                                                
NOAUTH   DC    C'* ERROR * SECURITY LOCKOUT'                                    
         EJECT                                                                  
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     RA,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         L     R7,SYSR7            2ND BASE REG - R7 MUST BE SET FIRST          
         L     R5,SYSR5            3RD BASE REG - R5 MUST BE SET FIRST          
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VUSER                                                            
         B     VTXT                                                             
         B     DTXT                                                             
         B     VRPT                REPORT                                       
         B     VSVC                SERVICE                                      
         B     VSTA                STATION                                      
         B     VDPT                DAYPART                                      
         B     VGRP                GROUP                                        
         B     VOFF                OFFICE                                       
         B     VADV                ADVERTISER                                   
         B     VBOOK               BOOKS WITH BOOKTYPE OPTION                   
         B     VBKL                BOOKS WITH LABEL OPTION                      
         B     DBKL                DISPLAY BOOKS WITH LABEL OPTION              
         B     VDEMO               DEMOS                                        
         B     DDEMO               DISPLAY DEMOS                                
         B     VCON                CONTRACT                                     
         B     DCON                DISPLAY CONTRACT VALUES                      
         B     SWISPT              SWITCH TO SPOT SYSTEM                        
         B     SWIREP              SWITCH TO REP SYSTEM                         
         B     VSID                VALIDATE SCHEME/PERIOD/(YEAR)                
         B     PACK                PACK                                         
         B     DLEN                DISPLAY LENGTH                               
         B     VSOURCE             VALIDATE SOURCE FIELD (I OR S)               
         B     VINV                VALIDATE INVENTORY NUMBER                    
         B     DINV                DISPLAY INVENTORY NUMBER                     
         B     DINVDTT             DISPLAY INVENTORY DAY/TIME/TITLE             
         B     VUPT                VALIDATE UPGRADE EXPRESSION                  
         B     CPROG               SWITCH BETWEEN OVERLAYS                      
         B     RPROG               RETURN TO PREVIOUS OVERLAY                   
         B     DPURDTT             DISPLAY PURE DAY/TIME/TITLE                  
         B     MYERR               CALL GETTXT FOR MESSAGE                      
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              USER REP                                                         
         SPACE 3                                                                
* VUSER - GET REP DATA FROM CONTROL FILE USER ID RECORD                         
*                                                                               
VUSER    CLI   TWAFIRST,0          FIRST TIME IN                                
         BNE   VU05                                                             
         XC    DMCB(24),DMCB       INIT SECRET                                  
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK)                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   TWAFIRST,1          ELSE SET NO LONGER FIRST TIME                
VU05     EQU   *                                                                
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG                                              
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VU10                                                             
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         SPACE 1                                                                
VU10     XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         DROP  R4                                                               
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    VUSERX              YES -- THERE CAN'T BE ANY PFKEYS             
         SPACE 1                                                                
         CLI   RACHANG,C'Y'        IF USER CHANGED RECORD/ACTION                
         BNE   *+8                                                              
         MVI   CALLSP,0            THEN RESET STACK POINTER                     
         SPACE 1                                                                
         CLI   PFKEY,0             WAS ENTER PRESSED?                           
         BE    VUSERX              YES -- NORMAL EXIT                           
         CLI   PFKEY,2             WAS PF2 PRESSED?                             
         BNE   VU20                NO                                           
         CLI   CALLSP,0            YES -- DO WE HAVE SOMEWHERE TO GO?           
         BE    VUSERX              NO -- NORMAL EXIT                            
         SPACE 1                                                                
         MVC   RETURNED,PFKEY      INDICATE THAT RETURN IS IN PROGRESS          
         MVI   PFKEY,0             THEN RESET PFKEY                             
         GOTO1 RETPROG             AND RETURN TO OVERLAY                        
                                                                                
VU20     DS    0H                                                               
         GOTO1 =A(VPFPROC),DMCB,(RC),RR=Y                                       
         B     VUSERX                                                           
                                                                                
VUSERX   B     XIT                                                              
         SPACE 3                                                                
*              HANDLE TEXT ELEMENTS                                             
         SPACE 3                                                                
VTXT     CLI   MODE,DISPREC                                                     
         BE    DTXT                                                             
         ZIC   R0,MAX              MAX FIELDS                                   
         LA    R4,1                SEQUENCE NUMBER                              
         GOTO1 REMELEM             TAKE OUT EXISTING X'02' ELEMENTS             
         SPACE 1                                                                
VTXT20   MVC   ELEMENT(2),=X'0289' BUILD NEW X'02' ELEMENTS                     
         MVC   ELEMENT+3(67),SPACES                                             
         CLI   5(R2),0                                                          
         BE    VTXT40                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+3(0),8(R2)  MOVE IN RIGHT HAND SIDE OF LINE              
VTXT40   STC   R4,ELEMENT+2        SEQUENCE NO.                                 
         SPACE 1                                                                
         BAS   RE,BUMP                                                          
         BCTR  R0,0                                                             
         MVC   ELEMENT+70(67),SPACES                                            
         CLI   5(R2),0                                                          
         BE    VTXT60                                                           
         ZIC   RE,5(R2)                                                         
         BCTR  RE,R0                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+70(0),8(R2)  MOVE IN LEFT HAND SIDE OF LINE              
         SPACE 1                                                                
VTXT60   GOTO1 ADDELEM                                                          
         BAS   RE,BUMP                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,VTXT20                                                        
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        NOW UPDATE X'01' ELEMENT                     
         BAS   RE,GETEL                                                         
         USING ROVRELEM,R6                                                      
         MVC   ROVRDATE,BTODAY     DATE                                         
         BAS   RE,GETTIME                                                       
         MVC   ROVRTIME,WORK       TIME                                         
         DROP  R6                                                               
         SPACE 1                                                                
         B     XIT                                                              
         SPACE 1                                                                
DTXT     ZIC   R0,MAX              DISPLAY UP TO MAX FIELDS                     
         L     R6,AIO              USER SUPPLIED ELCODE                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
DTXT20   BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                MUST HAVE MAX NUMBER OF TXT ELEMS            
         MVC   8(67,R2),3(R6)       PICK OUT DATA                               
         OI    6(R2),X'80'         AND TRANSMIT                                 
         BAS   RE,BUMP                                                          
         BCTR  R0,0                                                             
         MVC   8(67,R2),70(R6)                                                  
         OI    6(R2),X'80'         AND TRANSMIT                                 
         BAS   RE,BUMP                                                          
         BCT   R0,DTXT20                                                        
         B     XIT                                                              
         SPACE 1                                                                
         EJECT                                                                  
*  VALIDATE REPORT                                                              
         SPACE 1                                                                
VRPT     GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),2             MUST BE 2 CHARACTERS                         
         BNE   MYERR                                                            
         LA    R3,RPTLIST                                                       
RPT10    CLI   0(R3),X'FF'                                                      
         BE    MYERR                                                            
         CLC   0(2,R3),8(R2)                                                    
         BE    RPTX                                                             
         LA    R3,2(R3)                                                         
         B     RPT10                                                            
         SPACE 1                                                                
RPTX     B     XIT                                                              
         SPACE 3                                                                
RPTLIST  DS    0CL2                                                             
         DC    CL2'MO'                                                          
         DC    CL2'TU'                                                          
         DC    CL2'WE'                                                          
         DC    CL2'TH'                                                          
         DC    CL2'FR'                                                          
         DC    CL2'SA'                                                          
         DC    CL2'SU'                                                          
         DC    CL2'RA'              ROLLING AVERAGE                             
         DC    CL2'OT'              OTHER                                       
         DC    CL2'SS'              SATURDAY/SUNDAY                             
         DC    X'FF'                                                            
         EJECT                                                                  
*  VALIDATE SERVICE                                                             
         SPACE 1                                                                
VSVC     GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         CLI   5(R2),3             MUST BE 3 CHARACTERS                         
         BNE   MYERR                                                            
         LA    R3,SVCLIST                                                       
SVC10    CLI   0(R3),X'FF'                                                      
         BE    MYERR                                                            
         CLC   0(3,R3),8(R2)                                                    
         BE    SVCX                                                             
         LA    R3,3(R3)                                                         
         B     SVC10                                                            
         SPACE 1                                                                
SVCX     B     XIT                                                              
         SPACE 3                                                                
SVCLIST  DS    0CL3                                                             
         DC    CL3'ARB'                                                         
         DC    CL3'NSI'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
* VALIDATE STATION CALL LETTERS                                                 
* ON EXIT, CALL LETTERS ARE IN WORK                                             
*                              WORK+4  - A=AM F=FM C=CM T=BLANK                 
*                              WORK+10 - MARKET NAME                            
*                              WORK+40 - 1 OR 2 IF SATELLITE STATION            
*                              WORK+41 - GROUP/SUBGROUP CODE                    
         SPACE                                                                  
VSTA     DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVC   RERROR,=AL2(INVSTA)                                              
         LA    R4,BLOCK                                                         
*                                                                               
         CLI   0(R4),3                                                          
         BL    MYERR                                                            
         CLI   0(R4),4                                                          
         BH    MYERR                                                            
         TM    2(R4),X'40'         TEST ALPHA                                   
         BZ    MYERR                                                            
         MVC   WORK(4),12(R4)      SAVE CALL LETTERS                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)          DEFAULT = TV                                 
         BZ    VS100               YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV            TV LEAVE BLANK                               
         BE    VS100                                                            
         MVI   WORK+4,C'L'         L = LO POWER                                 
         EX    RE,STALP                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'A'         AM = A                                       
         EX    RE,STAAM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'F'         FM = F                                       
         EX    RE,STAFM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'C'         CM = C                                       
         EX    RE,STACM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C' '         MAY BE SATELLITE STATION                     
         EX    RE,STA1                                                          
         BNE   VS50                                                             
         MVI   WORK+4,C'1'                                                      
         B     VS100                                                            
VS50     EX    RE,STA2                                                          
         BNE   MYERR                                                            
         MVI   WORK+4,C'2'                                                      
*                                                                               
VS100    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY     REP                                          
         MVC   RSTAKSTA(4),WORK    STATION                                      
         OC    RSTAKSTA,SPACES                                                  
         CLI   WORK+4,C'1'         DON'T FILL FOR SATELLITES                    
         BE    VS110                                                            
         CLI   WORK+4,C'2'                                                      
         BE    VS110                                                            
*                                                                               
         MVC   RSTAKSTA+4(1),WORK+4                                             
*                                                                               
VS110    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         USING RSTAELEM,R6                                                      
         MVC   WORK+10(L'RSTAMKT),RSTAMKT                                       
         MVC   WORK+41(L'RSTAGRUP),RSTAGRUP                                     
         DROP  R4,R6                                                            
*                                                                               
VSEXT    B     XIT                                                              
*                                                                               
STATV    CLC   22(0,R4),=C'TV'                                                  
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACM    CLC   22(0,R4),=C'CM'                                                  
STALP    CLC   22(0,R4),=C'L'                                                   
STA1     CLC   22(0,R4),=C'1'                                                   
STA2     CLC   22(0,R4),=C'2'                                                   
         EJECT                                                                  
* VALIDATE DAYPART - RECORD READ INTO AIO2                                      
* R2 = INPUT HEADER                                                             
VDPT     GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVDPT)                                              
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVI   0(R4),X'24'                                                      
         MVC   24(2,R4),AGENCY     REP                                          
         MVC   26(1,R4),8(R2)      DAYPART                                      
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BE    DP120                                                            
         CLI   5(R2),1                                                          
         BNE   MYERR                                                            
         B     DP200                                                            
DP100    BAS   RE,NEXTEL                                                        
         BNE   MYERR                                                            
DP120    CLC   9(1,R2),2(R6)                                                    
         BNE   DP100                                                            
*                                                                               
DP200    MVC   AIO,AIO1                                                         
DPEXT    B     XIT                                                              
         EJECT                                                                  
*  VALIDATE GROUP - R2 POINTS AT SCREEN FIELD ON ENTRY                          
*                 - WORK HAS GROUP CODE, NAME & SUB GROUP NAME ON EXIT          
VGRP     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RGRPKEY,R4                                                       
         MVI   RGRPKTYP,7                                                       
         MVC   RGRPKREP,AGENCY     REP                                          
         MVC   RGRPKGRP,8(R2)                                                   
         OC    RGRPKGRP,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         MVC   WORK(2),RGRPKGRP    GROUP CODE                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK+10(10),RGRPNAME   GROUP NAME                                
         MVC   WORK+20(10),RGRPSBNM   SUB GROUP NAME                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  VALIDATE OFFICE - R2 POINTS AT SCREEN FIELD ON ENTRY                         
*                 - WORK HAS OFFICE CODE ON EXIT                                
*                 - EOFF... HAS ADDRESS INFO ON EXIT                            
         SPACE 1                                                                
VOFF     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ROFFKEY,R4                                                       
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,AGENCY     REP                                          
         MVC   ROFFKOFF,8(R2)                                                   
         OC    ROFFKOFF,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         MVC   WORK(2),ROFFKOFF    OFFICE CODE                                  
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK+2(20),ROFFNAME                                              
         MVC   EOFFADD1,ROFFADD1                                                
         MVC   EOFFADD2,ROFFADD2                                                
         MVC   EOFFSTT,ROFFSTT                                                  
         MVC   EOFFZIP,ROFFZIP                                                  
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  VALIDATE ADVERTISER - R2 POINTS AT SCREEN FIELD ON ENTRY                     
*                      - WORK HAS ADVERTISER CODE ON EXIT                       
*                      - WORK+10 HAS ADVERTISER NAMEON EXIT                     
VADV     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RADVKEY,R4                                                       
         MVI   RADVKTYP,8                                                       
         MVC   RADVKREP,AGENCY     REP                                          
         MVC   RADVKADV,8(R2)                                                   
         OC    RADVKADV,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         MVC   WORK(4),RADVKADV    GROUP CODE                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK+10(20),RADVNAME   GROUP NAME                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*  VALIDATE BOOKS WITH BOOK TYPE OPTION -  R2 POINTS TO BOOK FIELD              
*                                                                               
         SPACE 2                                                                
VBOOK    XC    CBKTYPE,CBKTYPE                                                  
         GOTO1 ANY                                                              
         GOTO1 BOOKVAL,DMCB,(CSOURCE,(R2)),(MAX,WORK),(C'B',SCANNER),  X        
               CBKTYPE                                                          
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   RERROR,=AL2(INVBOK)                                              
         B     MYERR                                                            
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         SPACE 1                                                                
         ZIC   R1,ACTUAL                                                        
         LA    R3,CBOOKS                                                        
         LA    R4,CBKTYPE                                                       
         LA    RE,WORK                                                          
BOOK10   MVC   0(3,R3),0(RE)       MOVE IN BOOK                                 
         MVC   3(1,R3),0(R4)       SAVE CBKTYPE IN BOOK+3                       
         LA    R3,4(R3)                                                         
         LA    R4,1(R4)                                                         
         LA    RE,3(RE)                                                         
         BCT   R1,BOOK10                                                        
         MVI   0(R3),X'FF'         END OF LIST MARKER                           
         B     XIT                                                              
         EJECT                                                                  
*  VALIDATE BOOKS WITH LABEL OPTION -  R2 POINTS TO BOOK FIELD                  
*                                                                               
         SPACE 2                                                                
VBKL     MVC   CBKLABEL,SPACES                                                  
         GOTO1 ANY                                                              
         GOTO1 BOOKVAL,DMCB,(CSOURCE,(R2)),(MAX,CBOOKS),(C'L',SCANNER),X        
               CBKLABEL                                                         
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   RERROR,=AL2(INVBOK)                                              
         B     MYERR                                                            
         SPACE 1                                                                
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY BOOKS WITH LABELS                                                     
* PARAMETER 1 = A(BOOK FIELD), '$' IN HIGH-ORD BYTE MEANS USE $, NOT -          
* PARAMETER 2 = A(LABEL ELEMENT (X'0B')), IF THERE IS ONE                       
* PARAMETER 3 = 0                                                               
* PARAMETER 4 = A(FILTERS), '+' IN HIGH-ORDER BYTE (OR ZERO)                    
         SPACE 2                                                                
DBKL     MVC   DMCB+12(4),12(R1)   FILTERS (IF ANY)                             
         L     R3,0(R1)            BOOK FIELD                                   
         MVC   DMCB+8(4),4(R1)     ADDRESS OF LABEL ELEMENT (IF ANY)            
         OC    DMCB+8(4),DMCB+8                                                 
         BZ    *+8                                                              
         MVI   DMCB+8,C'L'         THEN, INDICATE LABEL OPTION                  
         MVC   BYTE,DMCB                                                        
         GOTO1 VUNBOOK,DMCB,(MAX,0(R3)),(BYTE,0(R2))                            
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE DEMO(S)                                                              
         SPACE 1                                                                
VDEMO    GOTO1 ANY                                                              
         MVI   BYTE,0                                                           
         CLI   3(R1),1             1=VALIDATE CPM/CPP/PRIMARY DEMO              
         BNE   *+8                                                              
         MVI   BYTE,C'Y'                                                        
         LA    R3,BLOCK                                                         
         USING DBLOCKD,R3                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         GOTO1 DEMOVAL,PARAS,(CNDEMFLD,(R2)),(MAX,CDEMOS),(BYTE,(R3))           
         CLI   4(R1),0                                                          
         BNE   *+14                                                             
         MVC   RERROR,=AL2(INVDEM)                                              
         B     MYERR                                                            
         MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
* DISPLAY DEMOS                                                                 
* PARAMETER 1 POINTS TO LIST OF DEMOS                                           
         SPACE 2                                                                
DDEMO    L     R3,0(R1)                                                         
         ZIC   R4,MAX                                                           
DD10     CLI   1(R3),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R3),C'I'                                                       
         LA    R3,3(R3)                                                         
         BCT   R4,DD10                                                          
         L     R3,0(R1)                                                         
         LA    R4,BLOCK                                                         
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVI   DBSELMED,C'T'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 DEMOCON,DMCB,(MAX,0(R3)),(9,8(R2)),(0,DBLOCK)                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE CONTRACT - 1ST PARAMETER POINTS TO CONTRACT FIELD HEADER             
         SPACE 1                                                                
VCON     L     R2,0(R1)                                                         
         GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 VPACK                                                            
         LTR   R0,R0                                                            
         BZ    MYERR                                                            
         SPACE 1                                                                
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),DUB+3(5)                                              
         MVO   WORK(5),WORK+10(5)       CONTRACT NUM IN 9'S COMPLEMENT          
         SPACE 1                                                                
         MVC   CCONNUM,WORK                                                     
         PACK  CCONNUM(1),WORK+3(1)      REVERSE THE COMPLIMENT                 
         PACK  CCONNUM+1(1),WORK+2(1)    FOR AVL/PRO RECORDS LATER              
         PACK  CCONNUM+2(1),WORK+1(1)                                           
         PACK  CCONNUM+3(1),WORK(1)                                             
         SPACE 1                                                                
         MVC   RERROR,=AL2(NOTFOUND)                                            
         MVC   AIO,AIO2            PUT CONTRACT IN IO2                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RCONPTYP,R6                                                      
         MVI   RCONPTYP,X'8C'                                                   
         MVC   RCONPREP,AGENCY                                                  
         MVC   RCONPCON,WORK       CONTRACT NUMBER                              
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCONKEY,R6                                                       
         SPACE 1                                                                
         CLI   OFFLINE,C'Y'                                                     
         BE    CON10                                                            
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    CON10                                                            
         CLC   TWAACCS(2),=C'O='   TEST FOR OFFICE RESTRICTION                  
         BNE   CON10                                                            
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    CON10               TO ALL OFFICES                               
         CLC   RCONKOFF,TWAACCS+2  ELSE,COMPARE OFFICES                         
         BE    CON10                                                            
         MVC   RERROR,=AL2(SECLOCK) SECURITY LOCKOUT                            
         B     MYERR                                                            
         SPACE 1                                                                
CON10    MVC   CCONKSTA,RCONKSTA   STATION CALL LETTERS                         
         MVC   ESTATION(4),RCONKSTA ALSO SAVE IN PRINTABLE FORMAT               
         CLI   RCONKSTA+4,C' '     TV                                           
         BE    CON15                                                            
         LA    RE,ESTATION+3       FOR RADIO, SHOW -A,-F,-C                     
         CLI   0(RE),C' '                                                       
         BE    *+8                                                              
         LA    RE,1(RE)                                                         
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),RCONKSTA+4                                               
         SPACE 1                                                                
CON15    MVC   CCONKAGY(6),RCONKAGY  AGENCY CODE                                
         MVC   CCONKADV,RCONKADV   ADVERTISER CODE                              
         MVC   CCONKOFF,RCONKOFF                                                
         DROP  R6                                                               
         SPACE 1                                                                
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RCONELEM,R6                                                      
         MVC   CCONSAL,RCONSAL     SALESPERSON CODE                             
         MVC   CCONDAT,RCONDATE    START/END DATES                              
         GOTO1 DATCON,DMCB,(3,RCONDATE),(5,ECONDATE)     CONTRACT               
         MVI   ECONDATE+8,C'-'                           DATES                  
         GOTO1 (RF),(R1),(3,RCONDATE+3),(5,ECONDATE+9)                          
         MVC   CSOURCE,RCONRTGS    RATING SERVICE                               
         MVC   ECONBUYR,RCONBUYR   BUYER NAME                                   
         MVC   CCONWKS,RCONWKS     NUMBER OF WEEKS IN CONTRACT                  
         MVC   HALF,RCONCTGY       CATEGORY                                     
         CLC   RCONPRD,SPACES                                                   
         BE    *+14                                                             
         MVC   CCONPRD,RCONPRD                                                  
         B     CON16                                                            
         DROP  R6                                                               
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        GET PRODUCT NAME                             
         BAS   RE,GETEL                                                         
         USING RCONEXEL,R6                                                      
         MVC   EPRDNAME,RCONEXPR                                                
         DROP  R6                                                               
         SPACE 1                                                                
CON16    L     R6,AIO                                                           
         MVI   ELCODE,X'12'        GET SAR ELEMENT (IF ANY)                     
         BAS   RE,GETEL                 -SAVE BOOKS, DEMOS, LENGTHS             
         BE    CON18                                                            
*********CLC   AGENCY,=C'BL'       BL,B1 AND NB MUST HAVE SAR                   
*********BE    CON17                                                            
*********CLC   AGENCY,=C'B1'                                                    
*********BE    CON17                                                            
*********CLC   AGENCY,=C'NB'                                                    
*********BNE   CON20                                                            
         SPACE 1                                                                
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'R0AV'                                              
         MVC   WORK+20(2),AGENCY                                                
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         CLI   WORK,C'Y'           IS SAR INFO REQUIRED?                        
         BNE   CON20               NO                                           
         SPACE 1                                                                
CON17    CLC   CCONKADV,=CL4'GEN'  IF ADVERTISER IS 'GEN'. . .                  
         BNE   *+14                                                             
         CLC   HALF,=C'ZZ'         . . . AND CATEGORY IS 'ZZ'. . .              
         BE    CON20               . . . THEN SAR IS NOT REQUIRED               
         MVC   RERROR,=AL2(MISSSAR)                                             
         B     MYERR               OTHERWISE, SAR IS REQUIRED                   
         SPACE 1                                                                
         USING RSAREL,R6                                                        
CON18    CLC   RSARBKS(2),=C'DR'   FOR DIRECT RESPONSE - SKIP BOOKS             
         BE    *+16                AND DEMOS                                    
         MVC   CSARBKS,RSARBKS                                                  
         MVC   CSARDEM,RSARDEM                                                  
         MVC   CSARLEN,RSARRFRM                                                 
         DROP  R6                                                               
         SPACE 1                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'0B'        GET SAR BOOK LABEL ELEMENT (IF ANY)          
         BAS   RE,GETEL                                                         
         BNE   CON20                                                            
         ZIC   RE,1(R6)            LENGTH OF ELEMENT                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CSAR0B(0),0(R6)        SAVE ELEMENT                              
         SPACE 1                                                                
CON20    XC    KEY,KEY             GET MARKET NAME                              
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY                                                  
         MVC   RSTAKSTA,CCONKSTA                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EMKTNAME,RSTAMKT    MARKET NAME                                  
         EDIT  (2,RSTACHAN),(4,ESTACHAN),ALIGN=LEFT                             
         MVC   ESTAAFFL,RSTAAFFL         AFFILIATE                              
         DROP  R6                                                               
         SPACE 1                                                                
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   STNX                                                             
*  IF STATION IS USER, CHECK THAT THE STATION IS AUTHORIZED                     
*   TO SEE THIS CONTRACT                                                        
         MVC   RERROR,=AL2(SECLOCK) SECURITY LOCKOUT                            
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
STN10    BAS   RE,NEXTEL                                                        
         BNE   MYERR                                                            
         USING RSTASOEL,R6         RSTASID IS VALID SIGN-ON FOR THIS            
         CLC   TWAORIG,RSTASID     STATION'S CONTRACTS                          
         BNE   STN10                                                            
         DROP  R6                                                               
STNX     DS    0H                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET SALESPERSON NAME                         
         LA    R6,KEY                                                           
         USING RSALREC,R6                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,CCONSAL                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   ESALNAME,RSALNAME                                                
         MVC   ESALTEL,RSALTEL     TELEPHONE NUMBER                             
         DROP  R6                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET AGENCY NAME                              
         LA    R6,KEY                                                           
         USING RAGYREC,R6                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),CCONKAGY                                             
         MVC   RAGYKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    AGY75                                                            
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    AGY75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
AGY75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EAGYNAM1,RAGYNAM1                                                
         MVC   EAGYNAM2,RAGYNAM2                                                
         XC    EAGYADD1,EAGYADD1                                                
         MVC   EAGYADD1(20),RAGYADD1                                            
         MVC   EAGYCITY,RAGYCITY                                                
         MVC   EAGYSTAT,RAGYSTAT                                                
         MVC   EAGYZIP,RAGYZIP                                                  
         TM    RAGYFLAG,X'80'           EXPANDED ADDRESS?                       
         BZ    AGY80                    NO, SKIP READING AGY2 REC               
         DROP  R6                                                               
         MVI   KEY,X'1A'                AGY2 REC W/SAME KEY                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RAGY2KEY),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'             EXPANDED ADDRESS ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RAGY2AE1,R6                                                      
         MVC   EAGYADD1,RAGY2AD1        EXPANDED ADDRESS LINES                  
         MVC   EAGYADD2,RAGY2AD2                                                
         DROP  R6                                                               
AGY80    DS    0H                                                               
         SPACE 1                                                                
         XC    KEY,KEY             GET ADVERTISER NAME                          
         LA    R6,KEY                                                           
         USING RADVREC,R6                                                       
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,CCONKADV                                                
         MVC   RADVKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),KEYSAVE+25                                             
         BE    ADV75                                                            
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    ADV75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
ADV75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EADVNAME,RADVNAME                                                
         DROP  R6                                                               
         SPACE 1                                                                
         OC    EPRDNAME,EPRDNAME                                                
         BNE   PRDX                                                             
         XC    KEY,KEY             GET PRODUCT NAME                             
         LA    R6,KEY                                                           
         USING RPRDREC,R6                                                       
         MVI   RPRDKTYP,X'09'                                                   
         MVC   RPRDKADV,CCONKADV                                                
         MVC   RPRDKPRD,CCONPRD                                                 
         MVC   RPRDKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   KEY+25(2),AGENCY                                                 
         BE    PRD75                                                            
         CLC   KEY+25(2),=C'ZZ'                                                 
         BE    PRD75                                                            
         MVC   KEY+25(2),=C'ZZ'                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
PRD75    EQU   *                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   EPRDNAME,RPRDNAME                                                
         DROP  R6                                                               
PRDX     DS    0H                                                               
         SPACE 1                                                                
         XC    ECONNUM,ECONNUM                                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ECONNUM(0),8(R2)    SAVE EBCDIC CONTRACT NUMBER                  
         SPACE 1                                                                
         XC    KEY,KEY             NOW LOOK UP REP RECORD                       
         LA    R6,KEY                                                           
         USING RREPREC,R6                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   CPARREP,RREPPAR     SAVE PARENT REP                              
         DROP  R6                                                               
         SPACE 1                                                                
         MVC   AIO,AIO1            RESET IO AREA                                
         B     XIT                                                              
         EJECT                                                                  
* DISPLAY CONTRACT - FIRST PARA POINTS TO STATION CALL LETTERS FIELD            
         SPACE 1                                                                
*DCON     L     R2,0(R1)                                                        
*         MVC   8(L'ESTATION,R2),ESTATION                                       
*         OI    6(R2),X'80'         TRANSMIT TO SCREEN                          
*         BAS   RE,BUMP                                                         
*         MVC   8(L'EMKTNAME,R2),EMKTNAME                                       
*         OI    6(R2),X'80'                                                     
*         BAS   RE,BUMP                                                         
*         MVC   8(L'EAGYNAM1,R2),EAGYNAM1                                       
*         OI    6(R2),X'80'                                                     
*         BAS   RE,BUMP                                                         
*         MVC   8(L'ECONDATE,R2),ECONDATE                                       
*         OI    6(R2),X'80'                                                     
*         BAS   RE,BUMP                                                         
*         MVC   8(L'EADVNAME,R2),EADVNAME                                       
*         OI    6(R2),X'80'                                                     
*         BAS   RE,BUMP                                                         
*         MVC   8(L'ESALNAME,R2),ESALNAME                                       
*         OI    6(R2),X'80'                                                     
*         BAS   RE,BUMP                                                         
*         MVC   8(L'EPRDNAME,R2),EPRDNAME                                       
*         OI    6(R2),X'80'                                                     
DCON     DS    0H                                                               
         MVC   HDRSTN,ESTATION                                                  
         OI    HDRSTNH+6,X'80'     TRANSMIT TO SCREEN                           
         MVC   HDRMKTN,EMKTNAME                                                 
         OI    HDRMKTNH+6,X'80'                                                 
         MVC   HDRAGY,EAGYNAM1                                                  
         OI    HDRAGYH+6,X'80'                                                  
         MVC   HDRDATE,ECONDATE                                                 
         OI    HDRDATEH+6,X'80'                                                 
         MVC   HDRADV,EADVNAME                                                  
         OI    HDRADVH+6,X'80'                                                  
         MVC   HDRSAL,ESALNAME                                                  
         OI    HDRSALH+6,X'80'                                                  
         MVC   HDRPRO,EPRDNAME                                                  
         OI    HDRPROH+6,X'80'                                                  
         B     XIT                                                              
         EJECT                                                                  
*  SWITCH TO SPOT SYSTEM TO GET AGENCY/MEDIA CODE AND FOR RANSID                
         SPACE 1                                                                
SWISPT   DS    0H                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   SS10                                                             
         L     R3,TWAMASTC         GET A(MASTC)                                 
         L     R3,MCUTL-MASTD(R3)  GET A(UTL)                                   
         MVI   4(R3),X'32'         PUT SPOT SENUM IN UTL                        
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'SPOT',SPTFLIST,AIO2                   
         B     SS40                                                             
         SPACE                                                                  
SS10     L     RF,SWITCH                                                        
         XC    DMCB(8),DMCB                                                     
         MVI   DMCB,X'32'          SPOT Q SYSTEM NUMBER                         
         CLC   AGENCY,=C'SJ'       FOR TESTING                                  
         BNE   *+8                                                              
         MVI   DMCB,X'02'          USE SPOT 1 (SYSTEM NUMBER IS 2)              
         GOTO1 (RF),DMCB                                                        
         SPACE 1                                                                
         CLI   4(R1),2             TEST SYSTEM NOT OPERATIONAL                  
         BNE   *+14                                                             
         MVC   RERROR,=AL2(SPOTSTOP)                                            
         B     MYERR                                                            
         SPACE 1                                                                
         CLI   4(R1),0             ALL OTHER ERRORS ARE FATAL                   
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*  SET UP INTERNAL VALUES FOR SPOT                                              
SS40     MVI   SYSTEM,C'S'                                                      
         MVC   LKEY,=H'13'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'                                                  
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   REQFILE,=C'SPTREQ '                                              
         B     XIT                                                              
         EJECT                                                                  
* SWITCH BACK TO REP                                                            
SWIREP   DS    0H                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   SR20                                                             
         GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'SPOT'                                 
         L     R3,TWAMASTC         GET A(MASTC)                                 
         L     R3,MCUTL-MASTD(R3)  GET A(UTL)                                   
         CLI   CTRLMAIN,C'Y'       HAS SE# BEEN FOUND ALREADY?                  
         BE    SR10                YES                                          
         MVI   CTRLMAIN,C'Y'       SET SE# FOUND                                
         BAS   RE,CTRLSET          FIND SE#                                     
SR10     EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',REPFLIST,AIO2                    
*                                                                               
*   OPEN REP FILES *AND* CONTROL FILE UNDER REP SE #                            
*                                                                               
         B     SR40                                                             
*                                                                               
SR20     L     RF,SWITCH                                                        
         GOTO1 (RF),DMCB,=C'REP ',0                                             
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*  RESET INTERNAL VALUES FOR REP                                                
SR40     MVI   SYSTEM,C'R'                                                      
         MVC   LKEY,=H'27'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*  CTRLSET:  SETS REP SE # BY LOOKING IN CONTROL FILE.  DOES IT                 
*     ONE TIME BASED ON SWITCH CTRLMAIN.                                        
*         R3  =  A(UTL)                                                         
*                                                                               
CTRLSET  NTR1                                                                   
         MVI   4(R3),X'0A'         SET UTL SE TO CTFILE                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMOPEN'),=C'CONTROL',                +        
               =C'NCTFILE X',AIO2,0                                             
         XC    WORK,WORK                                                        
         MVI   WORK,C'5'           FIND CONTROL FILE ACCESS RECORD              
         MVC   WORK+23,AGENCY      INSERT POWER CODE                            
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,AIO2                     
         CLI   8(R1),0             FOUND?                                       
         BE    *+6                 YES                                          
         DC    H'0'                SHOULD HAVE BEEN THERE....                   
         L     R1,AIO2                                                          
         CLC   WORK(25),0(R1)      CHECK THE KEY                                
         BE    *+6                 SAME - OKAY                                  
         DC    H'0'                                                             
         LA    R1,28(R1)           FIND SYS AUTHORIZATION ELEMENT               
CTRL0010 EQU   *                                                                
         CLI   0(R1),X'21'         AUTH ELEMENT?                                
         BNE   CTRL0020            NO                                           
         CLI   2(R1),X'08'         IS IT 'REP' SYSTEM?                          
         BE    CTRL0030            YES                                          
CTRL0020 EQU   *                                                                
         ZIC   R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0             END OF RECORD?                               
         BNE   CTRL0010            NO                                           
         DC    H'0'                NO X'21' - DUMP IT OUT                       
CTRL0030 EQU   *                                                                
         ST    R1,FULL             SAVE A(X'21' ELEMENT)                        
*****>   GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'CONTROL'                              
*                                  CLOSE CONTROL FILE AS X'0A'                  
         L     R1,FULL             RESET A(X'21' ELEMENT)                       
         MVC   4(1,R3),3(R1)       OVERRIDE CONTROL FILE UTL                    
*                                     WITH REP UTL CODE                         
         B     XIT                                                              
         EJECT                                                                  
*  ROUTINE VALIDATES SCHEME/PERIOD FIELD                                        
*  ON ENTRY, R2 POINTS TO PJ OR SCHEME/PERIOD FIELD ON SCREEN                   
*     ENTRY CAN BE SCHEME/PERIOD                                                
*     OR SCHEME/PERIOD-YEAR (VALIDATE THROUGH RANSID IN SPOT)                   
         SPACE 1                                                                
VSID     DS    0H                                                               
         SPACE 1                                                                
         CLI   CAGYMED,0           DO WE ALREADY HAVE AGENCY/MEDIA              
         BNE   *+8                 YES                                          
         BAS   RE,GETAGY           NO, SO GO GET IT                             
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),C',=/='  CHANGE DELIMETER            
         LA    R4,BLOCK                                                         
         CLI   DMCB+4,2                                                         
         BL    SID60               MUST HAVE SCHEME AND PERIOD                  
         CLI   DMCB+4,3            ALSO CAN HAVE YEAR                           
         BH    SID60                                                            
         SPACE 1                                                                
         CLI   0(R4),2             SCHEME IS 2                                  
         BL    SID60                                                            
         CLI   0(R4),3             OR 3 CHARACTERS                              
         BH    SID60                                                            
         OC    CSCHEME,CSCHEME                                                  
         BZ    SID30                                                            
         MVC   CSCHEME2,12(R4)                                                  
         B     *+10                                                             
SID30    MVC   CSCHEME,12(R4)                                                   
         SPACE 1                                                                
         LA    R4,32(R4)                                                        
         CLI   0(R4),4             PERIOD IS UP TO 4 CHARACTERS                 
         BH    SID60                                                            
         OC    CPERIOD,CPERIOD                                                  
         BZ    SID40                                                            
         MVC   CPERIOD2,12(R4)                                                  
         B     *+10                                                             
SID40    MVC   CPERIOD,12(R4)                                                   
         SPACE 1                                                                
         CLI   DMCB+4,2                                                         
         BE    SID70                                                            
         LA    R4,32(R4)                                                        
         CLI   0(R4),2             YEAR IS 2 CHARACTERS                         
         BNE   SID60                                                            
         TM    2(R4),X'80'         MUST BE NUMERIC                              
         BZ    SID60                                                            
         OC    CYEAR,CYEAR                                                      
         BZ    SID50                                                            
         MVC   CYEAR2,7(R4)                                                     
         B     SID70                                                            
SID50    MVC   CYEAR,7(R4)         BINARY VALUE OF YEAR                         
         B     SID70                                                            
         SPACE 1                                                                
SID60    MVC   RERROR,=AL2(BADFMT)                                              
         B     MYERR                                                            
         SPACE 1                                                                
SID70    XC    WORK,WORK           READ SID PROFILE-MUST BE REP SCHEME          
         MVC   WORK+16(4),=C'S0SI'                                              
         MVC   WORK+20(2),AGENCY                                                
         MVI   WORK+22,C'T'                                                     
         OC    CSCHEME2,CSCHEME2                                                
         BZ    SID73                                                            
         CLC   CSCHEME2,=C'ALL'                                                 
         BE    SID75                                                            
         MVC   WORK+23(3),CSCHEME2                                              
         B     SID75                                                            
SID73    CLC   CSCHEME,=C'ALL'                                                  
         BE    *+10                                                             
         MVC   WORK+23(3),CSCHEME                                               
SID75    GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         OC    WORK(16),WORK       TEST PROFILE FOUND                           
         BZ    SID77                                                            
         CLI   WORK+1,C'Y'         ERROR IF NOT A Y                             
         BE    SID79                                                            
SID77    MVC   RERROR,=AL2(REPSCM)                                              
         B     MYERR                                                            
         SPACE 1                                                                
SID79    LA    R4,=A(BUFF-SYSD)    USE BUFF+2000 FOR RANSID BLOCK               
         LA    R4,1000(R4)                                                      
         LA    R4,1000(R4)         DON'T CREAM ITEMTAB                          
         USING SRBLKD,R4                                                        
         LA    RE,SRBLK            CLEAR BLOCK                                  
         LA    RF,SRBLKLN                                                       
         XCEF                                                                   
         SPACE 1                                                                
         MVC   SRASIR,AIO2         USI IO2 FOR NSID RECORDS                     
         MVC   SRACOM,ACOMFACS                                                  
         MVC   SRACLPAC,VCLPACK                                                 
         MVC   SRAMSUNP,MSUNPK                                                  
         MVC   SRADYUNP,UNDAY                                                   
         MVC   SRAUNTIM,UNTIME                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SRAMASTC,TWAMASTC                                                
         SPACE 1                                                                
         MVC   SRSELSCH,CSCHEME                                                 
         MVC   SRSELPER,CPERIOD                                                 
         MVC   SRSELYR,CYEAR                                                    
         OC    CSCHEME2,CSCHEME2                                                
         BZ    SID80                                                            
         MVC   SRSELSCH,CSCHEME2                                                
         MVC   SRSELPER,CPERIOD2                                                
         MVC   SRSELYR,CYEAR2                                                   
SID80    MVC   SRSELAM,CAGYMED                                                  
         MVC   SRSELAGY,AGENCY                                                  
         MVI   SRSELMED,C'T'                                                    
         SPACE 1                                                                
         GOTO1 RANSID,DMCB,(R4)                                                 
         SPACE 1                                                                
         CLI   SRERROR,SRNOERR     TEST FOR ERROR                               
         BE    SID110                                                           
         CLI   SRERROR,SRNOSCH                                                  
         BNE   SID90                                                            
         MVC   RERROR,=AL2(NOSCM)                                               
         B     MYERR                                                            
SID90    CLI   SRERROR,SRNOPER                                                  
         BNE   SID100                                                           
         MVC   RERROR,=AL2(NOPER)                                               
         B     MYERR                                                            
SID100   MVC   RERROR,=AL2(INVALID) SOMETHING ELSE IS WRONG                     
         B     MYERR                                                            
SID110   CLI   SRMODE,SRONEREC                                                  
         BE    XIT                                                              
         MVC   RERROR,=AL2(NOREC)                                               
         B     MYERR                                                            
         DROP  R4                                                               
         EJECT                                                                  
* PACK - R2 HAS ADDRESS OF HEADER                                               
         SPACE 1                                                                
PACK     SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    PACKX               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
         SPACE 1                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
* THIS ROUTINE DISPLAYS THE LENGTH FIELD                                        
*  PARAMETER 1 HAS AREA WITH 2-BYTE FIELDS,                                     
*                  WHERE BYTE 1 IS CLASS NUMBER IN BINARY                       
*                    AND BYTE 2 IS LENGTH IN SECONDS                            
*       (CLASS NUMBER IS OPTIONAL FOR AVAILS,                                   
*         AND IS ALWAYS ZERO FOR PROPOSALS)                                     
         SPACE 2                                                                
DLEN     L     R6,0(R1)            LENGTH FIELD                                 
         ZIC   R3,MAX              MAXIMUM NUMBER OF LENGTHS                    
         LA    R4,8(R2)                                                         
         OC    0(2,R6),0(R6)                                                    
         BZ    XIT                                                              
         B     DL6                 DON'T NEED COMMA FOR FIRST TIME              
DL2      OC    0(2,R6),0(R6)                                                    
         BZ    XIT                                                              
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
DL6      SR    RE,RE                                                            
         IC    RE,1(R6)            LENGTH                                       
         MH    RE,=H'10'           LEFT ONE POSITION                            
         XR    RF,RF                                                            
         IC    RF,0(R6)            CLASS                                        
         AR    RE,RF                                                            
         EDIT  (RE),(5,0(R4)),1,ALIGN=LEFT                                      
         SH    R0,=H'2'            R0 HAS NUM OF SIGNIF. CHARS                  
         AR    R4,R0               LENGTH OF OUTPUT                             
         CLI   ETYPE,C'P'                                                       
         BNE   DL10                                                             
         CLI   0(R4),C'.'          WIPE OUT CLASS FOR PROPOSALS                 
         BE    DL20                (WHEN COMING FROM SAR)                       
DL10     CLC   0(2,R4),=C'.0'                                                   
         BNE   *+14                                                             
DL20     MVC   0(2,R4),SPACES                                                   
         B     *+8                                                              
         LA    R4,2(R4)                                                         
         LA    R6,2(R6)                                                         
         BCT   R3,DL2                                                           
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES THE SOURCE FIELD (INVENTORY OR SID)                    
* R2 POINTS TO SOURCE FIELD, AND FIELD IS REQUIRED                              
         SPACE 1                                                                
VSOURCE  GOTO1 ANY                                                              
         MVC   RERROR,=AL2(INVALID)                                             
         MVI   ESOURCE,C'I'                                                     
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'INV'                                                  
         BE    XIT                                                              
         B     MYERR          ***  REMOVE THIS LINE WHEN SID IS VALID           
         SPACE 3                                                                
         MVI   ESOURCE,C'S'                                                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SID'                                                  
         BNE   MYERR                                                            
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE VALIDATES AN INVENTORY NUMBER AS FAR AS FORMAT, AND ALSO         
* MAKES SURE THAT THE INVENTORY RECORD EXISTS, AND THE INVENTORY                
* DATES FALL WITHIN THE CONTRACT DATE BOUNDARIES                                
*                                                                               
* ON EXIT, CBLOCK(3) HAS INVENTORY NUMBER IN HEX                                
*          CBLOCK+3(3) HAS INVENTORY DATE                                       
*          CBLOCK+6(1) HAS SATELLITE CODE                                       
         SPACE 1                                                                
VINV     DS    0H                                                               
         XC    CBLOCK(40),CBLOCK                                                
         MVC   RERROR,=AL2(INVALID)                                             
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK),C',=,-'                              
         SR    R4,R4                                                            
         IC    R4,DMCB+4                                                        
         LTR   R4,R4                                                            
         BZ    MYERR                                                            
         SPACE 1                                                                
         LA    R3,BLOCK                                                         
         CLC   12(3,R3),=C'MAN'    IF INVENTORY IS MANUAL                       
         BNE   *+14                                                             
         MVC   CBLOCK(3),=C'MAN'                                                
         B     VINVX                                                            
         CLC   0(2,R3),=X'0100'    1ST HALF OF FIELD LEN=1, NO 2ND HALF         
         BNE   VINV10                                                           
         CLI   12(R3),C'P'              PURE                                    
         BNE   MYERR                                                            
         MVI   BYTE,C'P'                                                        
         LA    R3,32(R3)                                                        
         BCT   R4,VINV10                                                        
         B     MYERR                                                            
         SPACE 1                                                                
VINV10   CLI   0(R3),3                  INVENTORY NUMBER                        
         BL    MYERR                                                            
         CLI   0(R3),4                                                          
         BH    MYERR                                                            
         SPACE 1                                                                
         CLI   12(R3),C'0'                                                      
         BL    MYERR                                                            
         CLI   12(R3),C'9'                                                      
         BH    MYERR                                                            
         CLI   13(R3),C'0'                                                      
         BL    MYERR                                                            
         CLI   13(R3),C'9'                                                      
         BH    MYERR                                                            
         SPACE 1                                                                
         PACK  DUB(8),12(2,R3)     QTR HOUR NUMBER                              
         CVB   R0,DUB                                                           
         STC   R0,CBLOCK                                                        
         SPACE 1                                                                
         CLI   14(R3),C'D'         TYPICAL                                      
         BE    VINV20                                                           
         CLI   14(R3),C'E'         WEEKEND                                      
         BE    VINV20                                                           
         CLI   14(R3),C'0'                                                      
         BL    MYERR                                                            
         CLI   14(R3),C'9'                                                      
         BH    MYERR                                                            
VINV20   SR    R1,R1                                                            
         IC    R1,14(R3)                                                        
         CLI   BYTE,C'P'          PURE                                          
         BNE   VINV50                                                           
         CLI   14(R3),C'D'         FOR TYPICAL                                  
         BE    VINV30                                                           
         CLI   14(R3),C'E'         AND WEEKEND                                  
         BNE   VINV40                                                           
VINV30   AH    R1,=H'9'            CONVERT CHARACTER TO NUMBER                  
VINV40   SLL   R1,28                                                            
         SRL   R1,24                                                            
VINV50   STC   R1,CBLOCK+1         DAY                                          
         SPACE 1                                                                
         CLI   BYTE,C'P'                                                        
         BE    VINV60                                                           
         MVI   CBLOCK+2,C'0'                                                    
         CLI   0(R3),3                                                          
         BE    VINV70                                                           
         MVC   CBLOCK+2(1),15(R3)                                               
         B     VINV70                                                           
         SPACE 1                                                                
VINV60   CLI   0(R3),3                                                          
         BE    VINV70                                                           
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,15(R3)           START WEEK                                   
         SLDL  RE,28               RE 4 HIGH ORDER                              
         SRL   RF,28               RF 4 LOW ORDER                               
         CH    RF,=H'7'                                                         
         BH    MYERR               0-7, A-G                                     
         SLL   RF,1                                                             
         EX    RF,*+8              BITES 4-6                                    
         B     *+8                                                              
         OI    CBLOCK+1,0                                                       
         SPACE 1                                                                
         CH    RE,=H'15'                                                        
         BNE   *+14                NOT NUMERIC                                  
         LTR   RF,RF                                                            
         BZ    *+16                INPUT ZERO                                   
         B     VINV70                                                           
         SPACE 1                                                                
         CH    RE,=H'12'                                                        
         BNE   MYERR               NOT A-G                                      
         OI    CBLOCK+1,1          LOW ORDER ON FOR ZERO AND A-G                
         SPACE 1                                                                
VINV70   MVC   CBLOCK+6(1),22(R3)  SATELLITE                                    
         CLI   CBLOCK+6,X'40'                                                   
         BNE   *+8                                                              
         MVI   CBLOCK+6,0                                                       
         SPACE 1                                                                
         LA    R3,32(R3)           OPTIONAL DATE                                
         CH    R4,=H'1'                                                         
         BE    VINV80                                                           
         GOTO1 DATVAL,DMCB,(0,12(R3)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    MYERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,CBLOCK+3)                                
         SPACE 1                                                                
*  NOW LOOK UP AN INVENTORY OR PURE NUMBER TO VERIFY THAT                       
*   IT EXISTS, AND THAT IT FALLS WITH IN THE DATE PARAMETERS                    
         SPACE 1                                                                
VINV80   GOTO1 DATCON,DMCB,(3,CCONDAT),(2,CBLOCK+20)                            
         GOTO1 DATCON,DMCB,(3,CCONDAT+3),(2,CBLOCK+22)                          
         SPACE 1                                                                
         MVC   AIO,AIO2            USE IO2 FOR PAV OR INV RECORD                
         LA    R3,ELEM                                                          
         SPACE 1                                                                
         CLI   8(R2),C'P'                                                       
         BE    VINV150                                                          
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         LA    R6,KEY              SET KEY FOR INV                              
         USING RINVD,R6                                                         
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RINVKSTA,CCONKSTA                                                
         MVC   RINVKSTA+4(1),CBLOCK+6                                           
         OI    RINVKSTA+4,X'40'                                                 
         CLI   RINVKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RINVKSTA+4,C'T'                                                  
         SPACE 1                                                                
         MVC   RINVKINV,CBLOCK                                                  
         MVC   RINVKSTD,CBLOCK+3                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE 1                                                                
VINV90   MVC   RERROR,=AL2(NOTFOUND)                                            
         CLC   KEYSAVE(21),KEY     SAME INVENTORY ITEM                          
         BNE   MYERR                                                            
         OC    CBLOCK+3(3),CBLOCK+3 ANY DATE GIVEN                              
         BZ    *+14                                                             
         CLC   KEYSAVE(24),KEY                                                  
         BNE   MYERR                                                            
         SPACE 1                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    *+14                                                             
         CLC   RINVPEFF+2(2),CBLOCK+20  INVENTORY ENDS                          
         BL    VINV100             BEFORE CONTRACT START                        
         SPACE 1                                                                
         CLC   RINVPEFF(2),CBLOCK+22    INVENTORY STARTS                        
         BH    VINV100             AFTER CONTRACT END                           
         MVC   CBLOCK+3(3),RINVKSTD      SAVE DATE, EVEN IF THEY DIDN'T         
         B     VINVX                     SUPPLY IT                              
         SPACE 1                                                                
VINV100  OC    CBLOCK+3(3),CBLOCK+3 ANY DATE GIVEN                              
         BNZ   MYERR                                                            
         LA    R6,KEY                                                           
         MVC   RINVKSRC(3),=X'FFFFFF'                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     VINV90                                                           
         DROP  R6                                                               
         SPACE 3                                                                
VINV150  XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRKEY,R6                                                         
         MVI   PRCODE,PRCODEQU                                                  
         MVC   PRMEDIA,CCONKSTA+4                                               
         CLI   PRMEDIA,C' '                                                     
         BNE   *+8                                                              
         MVI   PRMEDIA,C'T'                                                     
         SPACE 1                                                                
         MVC   PRSRC,CSOURCE                                                    
         MVC   PRSTAT(4),CCONKSTA                                               
         MVC   PRSTAT+4(1),PRMEDIA                                              
         CLI   CBLOCK+6,0                                                       
         BE    *+10                                                             
         MVC   PRSTAT+4(1),CBLOCK+6                                             
         SPACE 1                                                                
         LA    R3,CBOOKS                                                        
         OC    0(3,R3),0(R3)                                                    
         BNZ   INVALP0                                                          
         MVC   RERROR,=AL2(MISSBKS)                                             
         B     MYERR                                                            
INVALP0  LA    R1,6                AVAILS HAVE UP TO 6 BOOKS                    
         CLI   ETYPE,C'P'                                                       
         BNE   *+8                                                              
         LA    R1,1                PROPOSALS HAVE 1 BOOK                        
         SPACE 1                                                                
INVALP1  MVC   PRBOOK,1(R3)                                                     
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         BAS   RE,PAVHIGH                                                       
         L     R4,AIO                                                           
         CLC   PRKEY(PRSTYP-PRKEY),0(R4)                                        
         BNE   INVALP2                                                          
         SPACE 1                                                                
         MVC   PRSTIM(2),CBLOCK   (INVENTORY NUMBER)                            
         BAS   RE,DMFLHI                                                        
         CLC   PRKEY(PRRLEN-PRKEY),0(R4)                                        
         BE    VINVX                                                            
         SPACE 1                                                                
INVALP2  LA    R3,3(R3)                                                         
         OC    0(3,R3),0(R3)                                                    
         BZ    INVALP3                                                          
         XC    PRBOOK(PRFRSTEL-PRBOOK),PRBOOK                                   
         BCT   R1,INVALP1                                                       
         SPACE 1                                                                
INVALP3  MVC   RERROR,=AL2(NOTFOUND)                                            
         B     MYERR                                                            
         DROP  R6                                                               
         SPACE 2                                                                
VINVX    MVC   AIO,AIO1            RESET IO AREA                                
         B     XIT                                                              
         EJECT                                                                  
* THIS CODE (DOWN TO DMCHECK LABEL) WAS COPIED FROM DATA SET REPAVREAD          
* AT LEVEL 010 AS OF 11/23/81 AND AMENDED TO FIT HERE                           
*              COMMUNICATION WITH DATA MANAGER (PAVFL)                          
         SPACE 2                                                                
PAVREAD  MVC   COMMAND,=C'DMREAD'                                               
         B     PAVFILE                                                          
         SPACE 1                                                                
PAVSEQ   MVC   COMMAND,=C'DMRSEQ'                                               
         B     PAVFILE                                                          
         SPACE 1                                                                
PAVHIGH  MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
PAVFILE  NTR1                                                                   
         LA    R6,=C'PAVDIR'                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),(R6),KEY,AIO                      
         L     R2,AIO                                                           
         USING DMKEY,R2                                                         
         MVC   SVDXDA,DMNDXDA                                                   
         DROP  R2                                                               
         B     DMCHECK                                                          
         SPACE 1                                                                
DMFLHI   NTR1                                                                   
         LA    R6,=C'PAVFIL'                                                    
         MVC   COMMAND,=C'DMRDHI'                                               
         L     R2,AIO                                                           
         USING DMKEY,R2                                                         
         MVC   0(DMFRSTEL-DMKEY,R2),KEY                                         
         DROP  R2                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),(R6),SVDXDA,AIO                   
         B     XIT                                                              
         SPACE 2                                                                
DMCHECK  MVI   DMINBTS,X'00'                                                    
         MVC   BYTE,DMCB+8                                                      
         NC    BYTE,DMOUTBTS                                                    
         BZ    XIT                                                              
         SPACE                                                                  
         MVC   RERROR,=AL2(NOTFOUND)                                            
         B     MYERR                                                            
         EJECT                                                                  
* THIS ROUTINE WILL DISPLAY AN INVENTORY NUMBER                                 
* R2 POINTS TO INVENTORY NUMBER FIELD                                           
* INVENTORY NUMBER (RAVLDINV OR RPRPDINV) IS IN CBLOCK                          
* INVENTORY DATE (RAVLDATE OR RPRPDATE) IS IN CBLOCK+3                          
* SATELLITE (RAVLSAT OR RPRPDSAT) IS IN CBLOCK+6                                
         SPACE 1                                                                
DINV     DS    0H                                                               
         LA    R3,8(R2)                                                         
         MVC   0(6,R3),=C'MANUAL'                                               
         CLC   CBLOCK(3),=C'MAN'                                                
         BE    DINV50                                                           
         SPACE 1                                                                
         MVC   0(6,R3),SPACES                                                   
         CLI   CBLOCK+2,0                                                       
         BNE   *+14                NOT PURE                                     
         MVC   0(2,R3),=C'P,'      PURE                                         
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         XR    R1,R1                                                            
         IC    R1,CBLOCK                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(2,R3),DUB+1       QTR HOUR                                     
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         IC    R1,CBLOCK+1             DAY CODE                                 
         CLI   8(R2),C'P'          IS IT PURE                                   
         BNE   DINV30                                                           
         SRL   R1,4                                                             
         CLM   R1,1,=X'0D'         FOR TYPICAL OR                               
         BE    DINV10                                                           
         CLM   R1,1,=X'0E'         WEEKEND,                                     
         BNE   DINV20                                                           
DINV10   O     R1,=X'000000C0'     CONVERT NUMBER TO CHARACTER                  
         SH    R1,=H'9'                                                         
         B     DINV30                                                           
         SPACE 1                                                                
DINV20   O     R1,=X'000000F0'                                                  
         SPACE 1                                                                
DINV30   STC   R1,0(R3)                                                         
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
         CLI   8(R2),C'P'                                                       
         BE    DINV40                                                           
         CLI   CBLOCK+2,C'0'                                                    
         BE    DINV50                                                           
         MVC   0(1,R3),CBLOCK+2                                                 
         LA    R3,1(R3)                                                         
         B     DINV50                                                           
         SPACE 1                                                                
DINV40   TM    CBLOCK+1,X'0F'                                                   
         BZ    DINV50                                                           
         IC    R1,CBLOCK+1                                                      
         SLL   R1,28                                                            
         SRL   R1,29                                                            
         STC   R1,0(R3)                                                         
         OI    0(R3),X'C0'                                                      
         CLI   0(R3),X'C0'                                                      
         BE    *+12                                                             
         TM    CBLOCK+1,X'01'                                                   
         BO    *+8                                                              
         OI    0(R3),X'F0'                                                      
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
DINV50   CLI   CBLOCK+6,0                                                       
         BE    *+18                                                             
         MVI   0(R3),C'-'                                                       
         MVC   1(1,R3),CBLOCK+6                                                 
         LA    R3,2(R3)                                                         
         SPACE 1                                                                
         OC    CBLOCK+3(3),CBLOCK+3 DATE                                        
         BZ    DINVX                                                            
         MVI   0(R3),C','                                                       
         GOTO1 DATCON,DMCB,(3,CBLOCK+3),(5,1(R3))                               
         SPACE 1                                                                
DINVX    B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE WILL DISPLAY THE DAY, TIME, AND TITLE FROM THE                   
* INVENTORY RECORD                                                              
*                                                                               
* ON ENTRY, CBLOCK WILL HAVE INVENTORY NUMBER                                   
*           CBLOCK+3 WILL HAVE INVENTORY DATE                                   
*                                                                               
* ON EXIT, CBLOCK+10 WILL HAVE DAY                                              
*          CBLOCK+20 WILL HAVE TIME                                             
*          CBLOCK+40 WILL HAVE 1ST 27 CHARACTERS OF PROGRAM NAME                
         SPACE 2                                                                
DINVDTT  DS    0H                                                               
         XC    CBLOCK+10(57),CBLOCK+10                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RINVD,R6                                                         
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,CPARREP    USE PARENT REP, NOT AGENCY                   
         MVC   RINVKSTA,CCONKSTA                                                
         MVC   RINVKSTA+4(1),CBLOCK+6                                           
         OI    RINVKSTA+4,X'40'                                                 
         CLI   RINVKSTA+4,C' '                                                  
         BNE   *+8                                                              
         MVI   RINVKSTA+4,C'T'                                                  
         SPACE 1                                                                
         MVC   RINVKINV,CBLOCK                                                  
         MVC   RINVKSTD,CBLOCK+3                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   XIT                 INVENTORY WAS DELETED                        
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RINVPEL,R6                                                       
         MVC   CBLOCK+10(30),SPACES          DAY/TIME                           
         OC    RINVPADY,RINVPADY   IF AVAIL DAY OVERRIDE,                       
         BZ    DDTT10                                                           
         GOTO1 UNDAY,DMCB,RINVPADY,CBLOCK+10     USE IT                         
         B     DDTT20                                                           
DDTT10   GOTO1 UNDAY,DMCB,RINVPDAY,CBLOCK+10                                    
         SPACE 1                                                                
DDTT20   OC    RINVPATM,RINVPATM   IF AVAIL TIME OVERRIDE,                      
         BZ    DDTT30                                                           
         GOTO1 UNTIME,DMCB,RINVPATM,CBLOCK+20    USE IT                         
         B     DDTT40                                                           
         SPACE 1                                                                
DDTT30   GOTO1 UNTIME,DMCB,RINVPTIM,CBLOCK+20                                   
         SPACE 1                                                                
DDTT40   ZIC   RE,RINVPLEN                   PROGRAM                            
         SH    RE,=H'40'                                                        
         CH    RE,=H'27'                                                        
         BNH   *+8                                                              
         LA    RE,27                                                            
         SPACE 1                                                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CBLOCK+40(0),RINVPROG                                            
         SPACE 1                                                                
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*  THIS ROUTINE VALIDATES AN UPGRADE EXPRESSION                     *           
*  VALID UPGRADE EXPRESSIONS ARE UPT=PUT/MMMYY                      *           
*                                UPT=HUT/MMMYY                      *           
*                                UPT=HPT/MMMYY/INDEX                *           
*  ON ENTRY, R3 POINTS TO EITHER UPGRADE AREA 1 OR 2                *           
*-------------------------------------------------------------------*           
         SPACE 1                                                                
VUPT     DS    0H                                                               
         USING UPGD,R3                                                          
*         CLI   ESOURCE,C'S'                                                    
*         BE    VUPT10                                                          
         GOTO1 UPVAL,DMCB,(10,(R2)),WORK,ACOMFACS                               
         CLI   0(R1),0             TEST VALID EXPRESSION                        
         BNE   *+14                ERROR                                        
         MVC   RERROR,=AL2(BADUPGR)                                             
         B     MYERR                                                            
         CLI   0(R1),1             TEST ONLY ONE EXPRESSION                     
         BE    *+14                ERROR                                        
         MVC   RERROR,=AL2(ONEUPGR)                                             
         B     MYERR                                                            
         SPACE 1                                                                
         MVC   ELEM(14),WORK                                                    
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,ELEM,=C'ADD=CODE'                   
         CLI   DMCB+12,0                                                        
         BE    XIT                 NO ERROR                                     
         MVC   RERROR,=AL2(TOOLONG)                                             
         CLI   DMCB+12,5                                                        
         BE    MYERR                                                            
         DC    H'0'                BAD RETURN CODE                              
         SPACE 1                                                                
*VUPT10   MVC   RERROR,=AL2(INVALID)                                            
*         CLC   8(8,R2),=C'UPT=PUT/'                                            
*         BE    VUPT20                                                          
*         CLC   8(8,R2),=C'UPT=HUT/'                                            
*         BE    VUPT20                                                          
*         CLC   8(8,R2),=C'UPT=HPT/'                                            
*         BNE   MYERR                                                           
*VUPT20   XC    BLOCK(256),BLOCK                                                
*         XC    DMCB+8(4),DMCB+8                                                
*         GOTO1 SCANNER,DMCB,(24,(R2)),BLOCK                                    
*         LA    R4,BLOCK                                                        
* READ INPUT STRING FOR UPGRADE EXPRESSION AND BUILD FLDHDR                     
         SPACE 1                                                                
*         MVC   CUPPRG,22(R4)       MOVE DATA TO SAVE AREA                      
*         XC    ELEM,ELEM                                                       
*         IC    RE,1(R4)            GET EXPRESSION LENGTH                       
*         STC   RE,ELEM+5           SET INPUT STRING LENGTH                     
*         BCTR  RE,0                SET FOR EX                                  
*         EX    RE,*+8                                                          
*         B     *+10                                                            
*         MVC   ELEM+8(0),22(R4)    MOVE DATA                                   
*         LA    RE,9(RE)            ADJUST LEN TO INCLUDE FLDHDR                
*         STC   RE,ELEM             AND SET IN FLDHDR                           
         SPACE 1                                                                
* GET UPVAL ADDRESS                                                             
*         GOTO1 UPVAL,DMCB,ELEM,WORK,(C'/',ACOMFACS)                            
*         CLI   0(R1),0             TEST VALID EXPRESSION                       
*         BNE   *+14                ERROR                                       
*         MVC   RERROR,=AL2(BADUPGR)                                            
*         B     MYERR                                                           
*         MVC   CUPTYPE(8),WORK+4                                               
         SPACE 1                                                                
*         LA    R4,46(R4)           NON-STANDARD SCANNER                        
*         OC    0(2,R4),0(R4)       TEST ANY MORE FIELDS                        
*         BZ    VUPTX                                                           
         SPACE 1                                                                
*         CLC   =C'BK',12(R4)                                                   
*         BE    VUPT30                                                          
         SPACE 1                                                                
*         MVC   RERROR,=AL2(INVALID)                                            
*         B     MYERR                                                           
         SPACE 1                                                                
* EDIT OVERRIDE FOR SHARE BOOK                                                  
         SPACE 1                                                                
*VUPT30   CLI   1(R4),0                                                         
*         BNE   *+14                                                            
*VUPT40   MVC   RERROR,=AL2(BADOBOK)                                            
*         B     MYERR                                                           
*         XC    ELEM,ELEM                                                       
*         ZIC   RE,1(R4)                                                        
*         STC   RE,ELEM+5                                                       
*         BCTR  RE,0                                                            
*         EX    RE,*+8                                                          
*         B     *+10                                                            
*         MVC   ELEM+8(0),22(R4)    **EXECUTED**                                
*         LA    RE,9(RE)                                                        
*         STC   RE,ELEM                                                         
         SPACE 1                                                                
*         GOTO1 BOOKVAL,DMCB,(C'N',ELEM),(1,DUB),SCANNER                        
*         CLI   4(R1),0                                                         
*         BE    VUPT40                                                          
*         TM    DUB,X'BF'           TEST ANY GARBAGE OPTIONS SPECIFIED          
*         BNZ   VUPT40                                                          
*         MVC   CUPFBK,DUB+1                                                    
         SPACE 1                                                                
*VUPTX    B     XIT                                                             
*         DROP  R3                                                              
         EJECT                                                                  
* THIS ROUTINE FACILITATES THE ACTION OF SWITCHING, WITHIN A SINGLE             
* TRANSACTION, BETWEEN ONE OVERLAY AND ANOTHER BY CHANGING THE RECORD,          
* ACTION, PRINT AND KEY FIELDS ON THE SCREEN AND CALLING GENCON AGAIN.          
* FIRST IT OPTIONALLY SAVES THE CURRENT TWA IN TEMPSTR AND PUSHES THE           
* CURRENT OVERLAY NUMBER ONTO A STACK.  THEN IT CHANGES THE RECORD,             
* ACTION, PRINT AND KEY FIELDS TO THE DATA SPECIFIED IN THE PARAMETER           
* LIST.  FINALLY, IT SETS THE FLAG 'GOAGAIN' TO 'Y' AND TAKES AN ERROR          
* EXIT BACK TO GENCON WHICH RETURNS BACK TO THE CONTROLLER.  THE                
* CONTROLLER THEN RECOGNIZES THE FLAG AND CALLS GENCON AGAIN.  KEY              
* PARAMETERS ARE POSITIONAL, ENDING WITH A ZERO.                                
*                                                                               
* (FLAG,0),RECORD,ACTION,PRINT,(L'KEY1,KEY1),(L'KEY2,KEY2),...,0                
*              IF FLAG IS A 'Y', THEN SCREEN IS SAVED                           
*              RECORD, ACTION, AND KEY1 ARE REQUIRED                            
*                                                                               
*  (RA POINTS TO TWA)                                                           
CPROG    LR    R4,R1               SAVE POINTER TO PARMS                        
*                                                                               
         CLI   0(R4),C'Y'          SHOULD WE SAVE THE SCREEN?                   
         BE    *+12                YES                                          
         MVI   CALLSP,0            NO -- CLEAR STACK                            
         B     CP3                                                              
*                                                                               
         ZIC   R3,CALLSP           GET STACK POINTER                            
         LR    R2,R3               R2=ORIGINAL STACK POINTER VALUE              
         LA    RF,CALLSTCK(R3)     RF=A(NEXT POSITION)                          
         MVC   0(1,RF),MYSCRNUM    SLOT IN SCREEN NUMBER                        
         LA    R3,1(R3)                                                         
         STC   R3,CALLSP                                                        
         CLI   CALLSP,4            TEST MORE THAN 4 NEST LEVELS                 
         BNH   *+6                                                              
         DC    H'0'                YES-A REAL PROBLEM                           
*                                                                               
         SRL   R2,1                DIVIDE ORIGINAL LEVEL BY TWO                 
         LA    R2,3(R2)            ADD BACK THREE TWA PAGES                     
         SLL   R2,32-8             MOVE TO HIGH ORDER BYTE                      
         ICM   R2,3,2(RA)                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STC   R3,BYTE             SAVE STACK LEVEL                             
         L     RE,ATIA             RE=DESTINATION                               
         LA    RF,3072             MOVE/CLEAR HALF A TWA                        
         LA    R0,CONRECH          START AT RECORD HEADER                       
         LR    R1,RF               MOVE RECORD HEADER FOR 3072 BYTES            
         TM    BYTE,X'01'          TEST FOR ODD NUMBER                          
         BO    *+6                 YES                                          
         AR    RE,RF               NO-MOVE TO SECOND HALF OF TWA                
         MVCL  RE,R0                                                            
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R2),ATIA,0               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CP3      LA    R4,4(R4)            BUMP TO SECOND PARM                          
         XC    CONREC,CONREC       SET NEW RECORD TYPE                          
         OI    CONRECH+6,X'80'                                                  
         NI    CONRECH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONRECH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONREC(0),0(RF)                                                  
*                                                                               
         LA    R4,4(R4)            BUMP TO THIRD PARM                           
         XC    CONACT,CONACT       SET NEW ACTION TYPE                          
         OI    CONACTH+6,X'80'                                                  
         NI    CONACTH+4,X'DF'                                                  
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONACTH+5                                                     
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONACT(0),0(RF)                                                  
*                                                                               
         LA    R4,4(R4)            BUMP TO FOURTH PARM                          
         XC    CONWHEN,CONWHEN     SET NEW PRINT OPTION                         
         OI    CONWHENH+6,X'80'                                                 
         NI    CONWHENH+4,X'DF'                                                 
         L     RF,0(R4)                                                         
         ZIC   RE,0(R4)                                                         
         STC   RE,CONWHENH+5                                                    
         CLI   CONWHENH+5,0        IS THERE A PRINT OPTION?                     
         BE    CP5                 NO                                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONWHEN(0),0(RF)                                                 
*                                                                               
CP5      LA    R4,4(R4)            BUMP TO FIFTH PARM                           
         XC    CONKEY,CONKEY       SET NEW KEY FIELDS                           
         OI    CONKEYH+6,X'80'                                                  
         NI    CONKEYH+4,X'DF'                                                  
         LA    R2,CONKEY           BUILD KEY FIELD FROM FIFTH PARM ON           
         LR    R3,R2                                                            
*                                                                               
CP10     L     RF,0(R4)            ADD PARM TO KEY FIELD                        
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
         LA    R2,0(R2,RE)         BUMP R2 PAST IT                              
*                                                                               
         CLI   0(R2),C' '          BACKUP UNTIL NON-SPACE                       
         BH    *+10                                                             
         BCTR  R2,0                                                             
         B     *-10                                                             
         LA    R2,1(R2)                                                         
*                                                                               
         LA    R4,4(R4)            BUMP TO NEXT PARM                            
         OC    0(4,R4),0(R4)                                                    
         BZ    CPX                 IF PARM IS ZERO THEN NO MORE PARMS           
*                                                                               
         MVI   0(R2),C','          ELSE PUT COMMA BEFORE NEXT PARM              
         LA    R2,1(R2)            BUMP R2 TO WHERE NEXT PARM GOES              
         B     CP10                DISPLAY NEXT PARM                            
*                                                                               
CPX      SR    R2,R3               INSERT FAKE INPUT LEN INTO KEY FIELD         
         STC   R2,CONKEYH+5                                                     
*                                                                               
         MVI   RETURNED,0          CLEAR FLAG INDICATING RETURN                 
*                                                                               
         MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* THIS ROUTINE RESTORES THE TWA AND OVERLAY NUMBER TO THAT WHICH IS             
* ON THE TOP OF THE OVERLAY STACK.  IT THEN SETS THE 'GOAGAIN' FLAG             
* TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON.  WHEN THE CONTROLLER         
* GETS CONTROL BACK IT WILL CALL GENCON AGAIN WITH THE RESTORED SCREEN.         
* (RA POINTS TO TWA)                                                            
*                                                                               
RPROG    ZIC   R3,CALLSP           GET STACK POINTER                            
         BCTR  R3,0                DECREMENT POINTER TO POP STACK               
         STC   R3,CALLSP                                                        
         LA    RE,CALLSTCK(R3)                                                  
         MVC   MYSCRNUM,0(RE)      EXTRACT OVERLAY NUMBER                       
*                                                                               
         LR    R2,R3                                                            
         SRL   R2,1                DIVIDE LEVEL BY TWO                          
         LA    R2,3(R2)            START AT TWA PAGE 3                          
         SLL   R2,32-8             MOVE PAGE TO HOB                             
         ICM   R2,3,2(RA)                                                       
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R2),ATIA,0              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,CONRECH                                                       
         LA    RF,3072             MOVE RECORD HEADER FOR 3072 BYTES            
         L     R0,ATIA                                                          
         LR    R1,RF                                                            
         LA    R3,1(R3)            RESTORE ORIGINAL LEVEL                       
         STC   R3,BYTE                                                          
         TM    BYTE,X'01'          TEST FOR ODD LEVEL                           
         BO    *+6                 YES                                          
         AR    R0,R1               NO-MUST BE IN SECOND HALF OF PAGE            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         MVC   TWASCR,MYSCRNUM     DON'T LET GENCON RELOAD THE SCREEN           
         MVC   OVERLAY,MYSCRNUM    SET OVERLAY TOO                              
*                                                                               
         MVI   GOAGAIN,C'Y'        SET FLAG TO CALL GENCON AGAIN                
         SR    R2,R2                                                            
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* THIS ROUTINE WILL DISPLAY THE DAY, TIME, AND TITLE FROM THE                   
* PURE RECORD                                                                   
*                                                                               
* ON ENTRY, CBLOCK WILL HAVE PURE NUMBER                                        
*           CBLOCK+6 WILL HAVE SATELLITE                                        
*           CBLOCK+7 WILL HAVE BOOKS                                            
*                                                                               
* ON EXIT, CBLOCK+10 WILL HAVE DAY                                              
*          CBLOCK+20 WILL HAVE TIME                                             
*          CBLOCK+40 WILL HAVE 1ST 27 CHARACTERS OF PROGRAM NAME                
         SPACE 2                                                                
DPURDTT  DS    0H                                                               
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRKEY,R6                                                         
         MVI   PRCODE,C'P'                                                      
         MVI   PRMED,C'T'                                                       
         MVC   PRSRC,CSOURCE                                                    
         MVC   PRSTAT,CCONKSTA                                                  
         CLI   PRSTAT+4,C' '                                                    
         BNE   *+8                                                              
         MVI   PRSTAT+4,C'T'                                                    
         CLI   CBLOCK+6,0          SATELLITE OPTION                             
         BE    *+10                                                             
         MVC   PRSTAT+4(1),CBLOCK+6                                             
         MVC   PRBOOK,CBLOCK+7     BOOK                                         
         SPACE 1                                                                
         BAS   RE,PAVHIGH                                                       
         L     R4,AIO                                                           
         CLC   PRKEY(PRSTYP-PRKEY),0(R4)                                        
         BE    *+14                                                             
         MVC   CBLOCK+10(60),SPACES                                             
         B     DPURX                                                            
         SPACE 1                                                                
         MVC   PRSTIM(2),CBLOCK    INVENTORY NUMBER                             
         BAS   RE,DMFLHI                                                        
         CLC   PRKEY(PRRLEN-PRKEY),0(R4)                                        
         BE    *+14                                                             
         MVC   CBLOCK+10(60),SPACES                                             
         B     DPURX                                                            
         SPACE 1                                                                
         L     R4,AIO                                                           
         GOTO1 INVEDIT,DMCB,CBLOCK,WORK                                         
         MVC   CBLOCK+10(3),WORK           DAY                                  
         MVC   CBLOCK+20(6),WORK+3         TIME                                 
         LR    R6,R4                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,GETEL                                                         
         BNE   DPURX                                                            
         USING PPNELEM,R6                                                       
         ZIC   R1,PPNELN           LENGTH OF PROGRAM NAME                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CBLOCK+40(0),PPNNME                                              
         SPACE 1                                                                
DPURX    MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R6                                                               
         EJECT                                                                  
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL            
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG.  SINCE REBAS             
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE            
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                               
*                                                                               
MYERR    OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,RINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,RERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,RMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,RTXTLEN      LENGTH OF OPTIONAL TEXT                      
         MVC   GTATXT,RTXTADR      A(OPTIONAL TEXT)                             
         CLC   RERROR,=H'60'       IF MESSAGE NUMBER <= 60                      
         BH    *+8                                                              
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
         SPACE 1                                                                
         GOTO1 ERREX                                                            
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 1                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
GETTIME  DC    0H'0'                                                            
         ST    RE,FULL             SAVE RE                                      
         GOTO1 GETFACT,DMCB,0                                                   
         L     RF,0(R1)                                                         
         USING FACTSD,RF                                                        
         AP    FATIME,=P'80000'                                                 
         MVC   WORK(4),FATIME                                                   
         L     RE,FULL             RESTORE RE                                   
         BR    RE                                                               
         DROP  RF                                                               
         SPACE 2                                                                
* THIS ROUTINE WILL GET THE AGENCY/MEDIA CODE                                   
         SPACE 1                                                                
GETAGY   ST    RE,FULL                                                          
* FIRST GET AGENCY/MEDIA CODE                                                   
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),AGENCY                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVC   CAGYPROF,AGYPROF                                                 
         DROP  R6                                                               
                                                                                
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     GAM20                                                            
GAM10    BAS   RE,NEXTEL                                                        
GAM20    BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         USING AGYMEDEL,R6                                                      
         CLI   AGYMEDCD,C'T'        WANT TELEVISION                             
         BNE   GAM10                                                            
         MVC   CMED,AGYMEDCD                                                    
         MVC   CAGYMED,AGYMEDBT                                                 
         DROP  R6                                                               
                                                                                
         MVC   AIO,AIO1                                                         
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        CKGLOB --- CHECK FOR GLOBBER LOADER VARIABLES FROM THE                 
* CONTRACT PROGRAM                                                              
*                                                                               
CKGLOB   NTR1                                                                   
         CLI   TWAMODE,1               1=OFFLINE                                
         BE    CKGLGOOD                                                         
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGLOBBER,DMCB,=C'GETF',CONKEYH,,GLRCONNO                         
         CLI   DMCB+8,0                                                         
         BNE   CKGLOB50                                                         
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLRCONNO                                
* FULL HAS STATUS WORD                                                          
         GOTO1 CGLOBBER,DMCB,=C'GETD',PFKEY,L'PFKEY,GLRPFKEY                    
         GOTO1 CGLOBBER,DMCB,=C'GETD',FULL,L'FULL,GLRSTAT                       
                                                                                
         CLI   PFKEY,4                                                          
         BNE   *+10                                                             
         MVC   CONREC(5),=C'AHEAD'                                              
         CLI   PFKEY,5                                                          
         BNE   *+10                                                             
         MVC   CONREC(5),=C'PHEAD'                                              
         CLI   PFKEY,6                                                          
         BNE   *+10                                                             
         MVC   CONREC(5),=C'*OPEN'                                              
         OI    CONRECH+6,X'80'     XMIT                                         
         MVI   CONRECH+4,X'80'     TURN ON FIELD INPUT THIS TIME                
         MVI   CONRECH+5,5         SET INPUT LENGTH                             
         MVC   CONACT(3),=C'ADD'                                                
                                                                                
CKGLOB20 DS    0H                                                               
         OI    CONACTH+6,X'80'                                                  
         MVI   CONACTH+4,X'80'                                                  
         MVI   CONACTH+5,3                                                      
         ZIC   R1,CONKEYH+5        GET LENGTH OF KEY                            
         LA    R2,CONKEY                                                        
         AR    R2,R1               PT TO NEXT BLANK IN KEY                      
         OI    CONKEYH+6,X'80'                                                  
         MVI   CONKEYH+4,X'80'                                                  
         TM    FULL,X'80'          NOTE STATUS IS FULL WORD                     
         BZ    CKGLGOOD                                                         
         MVC   0(13,R2),=C',,SAR,SAR,SAR'                                       
         LA    R1,13(R1)                                                        
         STC   R1,CONKEYH+5                                                     
         B     CKGLGOOD                                                         
*                                                                               
* CHECK IF SWAPPED FROM CONTRACT TO DO BUYCOPY                                  
*                                                                               
CKGLOB50 DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   CKGLGOOD                                                         
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
*                                                                               
         LA    RF,WORK                                                          
         USING GLVXFRSY,RF                                                      
         CLC   =C'CON',GLVXFRPR                                                 
         BNE   CKGLGOOD            FROM THE CONTRACT PROGRAM?                   
         DROP  RF                                                               
*                                                                               
         GOTO1 CGLOBBER,DMCB,=C'GETF',CONKEYH,,GLRBCOPY                         
         CLI   DMCB+8,0                                                         
         BNE   CKGLGOOD                                                         
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLRBCOPY                                
*                                  INVOKE BUY COPY                              
         XC    CONREC,CONREC                                                    
         MVC   CONREC(8),=C'CONBCOPY'                                           
         OI    CONRECH+6,X'80'     XMIT                                         
         MVI   CONRECH+4,X'80'     TURN ON FIELD INPUT THIS TIME                
         MVI   CONRECH+5,8         SET INPUT LENGTH                             
         XC    CONACT,CONACT                                                    
         MVC   CONACT(3),=C'REP'                                                
         OI    CONACTH+6,X'80'                                                  
         MVI   CONACTH+4,X'80'                                                  
         MVI   CONACTH+5,3                                                      
*                                                                               
*        CKGLOB EXIT                                                            
*                                                                               
CKGLGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     XIT                                                              
         SPACE 3                                                                
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        SETPROFS --- GET AND SET BAS/REP PROFILES FROM THE REP REC             
*                                                                               
*ETPROFS NTR1                                                                   
*        LR    R2,RA               USE R2 TO COVER THE ENTRY                    
*        AH    R2,=AL2(BASPROFS-CONHEADH)                                       
*        USING SVDSECT,R2                                                       
*                                                                               
*- IF NOT ALREADY DONE, READ IN REP RECORD FOR PGM PROFILE                      
*        CLC   SVPGREP(2),AGENCY                                                
*        BNE   SETP005                                                          
*        CLI   SVPGP#,RREPQSFM                                                  
*        BE    SETP100             IN TWA FROM PRIOR HIT                        
*                                                                               
*ETP005  EQU   *                                                                
*        XC    SVPGENTY,SVPGENTY                                                
*                                                                               
*        CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
*        BNE   SETP010                                                          
*        MVC   SVPGREP(2),AGENCY                                                
*        MVI   SVPGP#,RREPQSFM                                                  
****>>   MVC   SVPGPBIT,=8X'FF'                                                 
*                                                                               
*   DON'T SET ALL BITS FOR DDS TERMINAL!!                                       
*                                                                               
*        MVI   CSTRT6AM,C'D'       READ REP PROFILE ONLY FOR 6AM PROF           
*                                  WE ARE CHEATING BY USING CSTRT6AM            
*                                  FOR A TEMPORARY FLAG                         
*ETP010  EQU   *                                                                
*        XC    KEY,KEY                                                          
*        MVI   KEY,X'01'                                                        
*        MVC   KEY+25(2),AGENCY                                                 
*        MVC   KEYSAVE,KEY                                                      
*        XC    DMCB(24),DMCB                                                    
*        GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'REPDIR',KEYSAVE,KEY,                  
*              0,0                                                              
*        CLI   DMCB+8,X'10'                                                     
*        BNE   *+6                                                              
*        DC    H'0'                REP RECORD NOT ON FILE?  HOW?                
*                                                                               
*        XC    DMCB(24),DMCB                                                    
*        GOTO1 DATAMGR,DMCB,=C'GETREC',=C'REPFILE',KEY+28,BUFF,                 
*              DMWORK,0                                                         
*                                                                               
*        LA    R6,BUFF                                                          
*        USING RREPREC,R6                                                       
*                                                                               
*        CLI   CSTRT6AM,C'D'       READ REP PROF AND EXIT                       
*        BNE   SETP015                                                          
*        MVC   CSTRT6AM,RREPPROF+4                                              
*        B     SETP017                                                          
*                                                                               
*ETP015  DS    0H                                                               
*        MVC   CSTRT6AM,RREPPROF+4                                              
*                                                                               
*ETP017  DS    0H                                                               
*        MVC   SVPGREP(2),AGENCY                                                
*        MVI   SVPGP#,RREPQSFM                                                  
*                                                                               
*        LA    RE,RREPREC          RECORD IS HERE                               
*        ZICM  RF,RREPLEN,2                                                     
*        DROP  R6                                                               
*        AR    RF,RE                                                            
*        MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
*        LA    RE,34(RE)           A(1ST ELEMENT)                               
*ETP020  EQU   *                                                                
*        CLI   0(RE),0             END OF RECORD W/O MATCH?                     
*        BE    SETP100                                                          
*                                                                               
*        CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
*        BE    SETP040                                                          
*                                                                               
*        ZIC   RF,1(RE)            GET NEXT ELEMENT                             
*        AR    RE,RF                                                            
*        B     SETP020                                                          
*                                                                               
*- FIND SFM PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                         
*ETP040  EQU   *                                                                
*        USING RREPPGMP,RE                                                      
*        ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
*        LA    RE,RREPPGM1         A(1ST UNIT)                                  
*        DROP  RE                                                               
*                                                                               
*ETP050  CLI   0(RE),RREPQSFM      LOOKING FOR SFM                              
*        BE    SETP060                                                          
*        LA    RE,RREPPGML(RE)     NEXT UNIT                                    
*        BCT   R0,SETP050                                                       
*        B     SETP100             NO MATCH. USE DEFAULTS                       
*                                                                               
*ETP060  MVC   SVPGPBIT(8),2(RE)   SAVE UNIT IN TWA.                            
*        SPACE                                                                  
*ETP100  EQU   *                                                                
*        LA    RE,BUFF                                                          
*        XC    0(250,RE),0(RE)                                                  
*        XC    250(250,R3),250(RE)                                              
*        XC    500(250,R3),500(RE)                                              
*        XC    750(250,R3),750(RE)                                              
*                                                                               
*ETPGOOD EQU   *                                                                
*        SR    R0,R0                                                            
*        B     XIT                                                              
*                                                                               
*        DROP  R2                                                               
*        EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*=============================================================*                 
* PROVIDE MSPACK/MSUNPK ENTRY POINTS FOR LINKAGE TO STAPACK                     
*=============================================================*                 
         SPACE 1                                                                
GOMSPACK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,SYSR7                                                         
         L     R5,SYSR5                                                         
         L     R8,ASPOOLD                                                       
         LR    R6,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,CAGYPROF+7                                              
         MVC   STAPMED,CMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R6)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R6)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R6)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
GOMSUNPK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,SYSR7                                                         
         L     R5,SYSR5                                                         
         L     R8,ASPOOLD                                                       
         LR    R6,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,CAGYPROF+7                                              
         MVC   STAPMED,CMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R6)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,4(R6)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R6)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R6),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SVDXDA   DS    F                   FOR PAV FILE READ                            
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(UNBOOK)                                                        
         DC    V(DUMMY)                                                         
         DC    V(CLPACK)                                                        
         DC    V(UNUPGR)                                                        
         DC    V(RETEXT)                                                        
         DC    V(UNTEXT)                                                        
         DC    V(RECUP)                                                         
         DC    V(UPOUT)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
*  TABLE OF CORE RESIDENT MODULE ADDRESSES                                      
CORETAB  DS    0X                                                               
         DC    X'30'               GENCON                                       
         DC    X'E0'               DEMOCON                                      
         DC    X'47'               RANSID                                       
         DC    X'26'               DEFINE                                       
         DC    X'21'               SPDEMLK (SPGETDEMO)                          
         DC    X'13'               UPVAL                                        
         DC    X'22'               SPDEMUP                                      
         DC    AL1(QMSPACK)        MSPACK                                       
         DC    AL1(QMSUNPK)        MSUNPK                                       
         DC    AL1(QQSORT)         QSORT                                        
         DC    X'08'               DEMUP                                        
         DC    X'09'               INVEDIT                                      
         DC    AL1(QGETBROD)       GETBROAD                                     
*                                                                               
CORES    EQU   (*-CORETAB)                                                      
         SPACE 3                                                                
SPTFLIST DC    CL8'NSPTFILE'                                                    
         DC    CL8'NSPTDIR'                                                     
         DC    CL8'NSTAFILE'                                                    
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         SPACE                                                                  
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
         EJECT                                                                  
*              TABLE OF AUTHORIZATION VALUES FOR VARIOUS ACTIONS                
         SPACE 2                                                                
         DS    0D                                                               
         SPACE 1                                                                
*                                  CL8 EXPANDED RECORD NAME                     
*                                  XL2 AUTHORIZATION VALUES                     
         SPACE 1                                                                
AUTHTAB  DS    0CL10                                                            
*        DC    C'DPT     ',X'800F'                                              
*        DC    C'PRGT    ',X'800F'                                              
*        DC    C'SDD     ',X'800F'                                              
*        DC    C'DR      ',X'900F'                                              
*        DC    C'DRN     ',X'900F'                                              
*        DC    C'GOALN   ',X'800F'                                              
*        DC    C'GOAL    ',X'800F'                                              
*        DC    C'COMM    ',X'400F'                                              
*        DC    C'SWITCH  ',X'200F'                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 3                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'AHEADER ',AL1(16),X'0000'                                
         DC    X'01',C'ADETAIL ',AL1(17),X'0000'                                
RECACT1  DC    X'01',C'STATION ',AL1(02),X'0000'                                
         DC    X'01',C'COMPETIN',AL1(03),X'0000'                                
         DC    X'01',C'OFFTEAM ',AL1(04),X'0000'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'ADD     ',AL1(01,02,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,04,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         DC    X'02',C'UPDATE  ',AL1(13,13,00)                                  
         DC    X'02',C'COPY    ',AL1(15,15,00)                                  
         DC    X'02',C'PCOPY   ',AL1(16,16,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 3                                                                
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
         SPACE 1                                                                
RECACT3  DC    X'03',AL1(02,01),X'AF02000080',C'OVOV'  STATION   DISPL          
         DC    X'03',AL1(02,02),X'AF02000080',C'OVOV'  STATION   ADD            
         DC    X'03',AL1(02,03),X'AF02000080',C'OVOV'  STATION   CHANG          
         DC    X'03',AL1(02,10),X'D002000080',C'OVOV'  STATION   LIST           
         DC    X'03',AL1(03,01),X'B203000080',C'OVOV'  COMPETIN  DISPL          
         DC    X'03',AL1(03,03),X'B203000080',C'OVOV'  COMPETIN  CHANG          
         DC    X'03',AL1(03,10),X'D203000080',C'OVOV'  COMPETIN  LIST           
         DC    X'03',AL1(04,01),X'B304000080',C'OVOV'  OFFTEAM   DISPL          
         DC    X'03',AL1(04,03),X'B304000080',C'OVOV'  OFFTEAM   CHANG          
         DC    X'03',AL1(04,10),X'D304000080',C'OVOV'  OFFTEAM   LIST           
         DC    X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,R7,R5                                                         
*****************************************************************               
* PROCESS PF KEYS                                                               
*****************************************************************               
         DS    0H                                                               
VPFPROC  NMOD1 0,**PFPC**,RR=R2                                                 
         L     RC,0(R1)                                                         
         ST    R2,RELO2                                                         
                                                                                
         LA    RE,PFTABLE          TABLE OF SCREENS AND PF KEYS                 
         MVC   HALF(1),MYSCRNUM    SAVED SCREEN NUMBER                          
         MVC   HALF+1(1),PFKEY     PRESSED PFKEY                                
                                                                                
VPF10    CLC   HALF,0(RE)          MATCH ON SCREEN/PFKEY?                       
         BE    VPF20               YES                                          
         LA    RE,8(RE)            BUMP TO NEXT ENTRY IN TABLE                  
         CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    VPFPROCX            YES                                          
         B     VPF10                                                            
                                                                                
VPF20    DS    0H                                                               
*                                                                               
* SPECIAL CHECK FOR DIRECT RESPONSE (NOTE) RECORDS                              
*                                                                               
         CLC   =X'C505',HALF       IF CHANGE SELECT, WE WANT TO                 
         BE    VPF30               STAY WITH SAME SCREEN AFTER A PFKEY          
         CLC   =X'C506',HALF       HAS BEEN HIT                                 
         BE    VPF30                                                            
         CLC   =X'C705',HALF                                                    
         BE    VPF30                                                            
         CLC   =X'C706',HALF                                                    
         BE    VPF30                                                            
*                                                                               
* SPECIAL CHECK FOR GOALN/GOAL RECORDS                                          
*                                                                               
         CLC   =X'CD05',HALF       IF CHANGE SELECT, WE WANT TO                 
         BE    VPF30               STAY WITH SAME SCREEN AFTER A PFKEY          
         CLC   =X'CD06',HALF       HAS BEEN HIT                                 
         BE    VPF30                                                            
         CLC   =X'DB05',HALF                                                    
         BE    VPF30                                                            
         CLC   =X'DB06',HALF                                                    
         BE    VPF30                                                            
         CLC   =X'DB07',HALF       PAGE UP                                      
         BE    VPF30                                                            
         CLC   =X'DB08',HALF       PAGE DOWN                                    
         BE    VPF30                                                            
*                                                                               
         MVI   PFKEY,0             SO WE DON'T LOOP                             
         MVI   CALLSP,0            CLEAR STACK POINTER                          
VPF30    L     RF,4(RE)            A(CALLPROG CALL)                             
         A     RF,RELO2                                                         
         BR    RF                                                               
                                                                                
VPF40    GOTO1 CALLPROG,BLOCK,0,=C'PDETAIL',=C'UPDATE',0,(8,ECONNUM),  X        
               (3,EHDRNUM),0                                                    
                                                                                
VPF50    GOTO1 CALLPROG,BLOCK,0,=C'PHEADER',=C'REPORT',=C'NOW,PRO',    X        
               (8,ECONNUM),(3,EHDRNUM),0                                        
                                                                                
VPF60    GOTO1 CALLPROG,BLOCK,0,=C'PHEADER',=C'CHANGE',0,(8,ECONNUM),  X        
               (3,EHDRNUM),0                                                    
                                                                                
VPF70    GOTO1 CALLPROG,BLOCK,0,=C'ADETAIL',=C'UPDATE',0,(8,ECONNUM),  X        
               (3,EHDRNUM),0                                                    
                                                                                
VPF80    GOTO1 CALLPROG,BLOCK,0,=C'AHEADER',=C'REPORT',=C'NOW,AVA',    X        
               (8,ECONNUM),(3,EHDRNUM),0                                        
                                                                                
VPF90    GOTO1 CALLPROG,BLOCK,0,=C'AHEADER',=C'CHANGE',0,(8,ECONNUM),  X        
               (3,EHDRNUM),0                                                    
                                                                                
VPF100   GOTO1 CALLPROG,BLOCK,0,=C'DRN',=C'DISPLAY',0,(7,EDRSTAT),     X        
               (17,EDRPERI),0                                                   
                                                                                
VPF110   GOTO1 CALLPROG,BLOCK,0,=C'DR ',=C'REPORT',=C'NOW,DIR',        X        
               (2,=C'..'),(7,EDRSTAT),(17,EDRPERI),0                            
                                                                                
VPF120   DS    0H                  STAY WITH SAME SCREEN IF PFKEY               
         CLC   =C'SEL',CONACT      ACTION SELECT?                               
         BNE   VPFPROCX                                                         
         CLI   THISLSEL,C'C'       'C' ENTERED AS LIST SELECTION?               
         BNE   VPF130                                                           
         CLI   PFKEY,5             ADD A LINE?                                  
         BE    VPF170                                                           
         CLI   PFKEY,6             DEL A LINE?                                  
         BE    VPF170                                                           
         CLI   PFKEY,7             PAGE UP?                                     
         BE    VPF170                                                           
         CLI   PFKEY,8             PAGE DOWN?                                   
         BE    VPF170                                                           
         B     VPFPROCX                                                         
                                                                                
VPF130   DS    0H                                                               
         CLI   THISLSEL,C'S'       'S' ENTERED AS LIST SELECTION?               
         BNE   VPFPROCX                                                         
         CLI   PFKEY,7             PAGE UP?                                     
         BE    VPF140                                                           
         CLI   PFKEY,8             PAGE DOWN?                                   
         BNE   VPFPROCX                                                         
                                                                                
* HERE'S A NEAT TRICK TO PAGE UP AND DOWN IN THE SELECT SCREEN:                 
* IF PF7 OR 8 IS HIT WE JUST PUT THE SELECTION ACTION BACK ON                   
* THE ACTION COLUMN.                                                            
                                                                                
VPF140   DS    0H                                                               
         LA    R3,LISTDIR          FIND WHERE WE ARE IN THE LIST SCREEN         
         ZIC   R0,LISTNUM            THAT IS BEING SELECTED                     
VPF150   CLC   LASTSEL,2(R3)       FIND MATCHING D/A                            
         BE    VPF160                                                           
         LA    R3,6(R3)                                                         
         BCT   R0,VPF150                                                        
         B     VPFPROCX                                                         
                                                                                
VPF160   DS    0H                                                               
         MVI   0(R3),C'S'          STAY ON THE SAME SCREEN!                     
         B     VPFPROCX                                                         
                                                                                
VPF170   DS    0H                                                               
         LA    R3,LISTDIR          FIND WHERE WE ARE IN THE LIST SCREEN         
         ZIC   R0,LISTNUM            THAT IS BEING SELECTED                     
VPF180   CLC   LASTSEL,2(R3)       FIND MATCHING D/A                            
         BE    VPF190                                                           
         LA    R3,6(R3)                                                         
         BCT   R0,VPF180                                                        
         B     VPFPROCX                                                         
                                                                                
VPF190   DS    0H                                                               
         MVI   0(R3),C'C'          STAY ON THE SAME SCREEN!                     
         CLI   PFKEY,5             ADD?                                         
         BE    VPF200                                                           
         CLI   PFKEY,6             DELETE?                                      
         BNE   VPFPROCX                                                         
                                                                                
VPF200   DS    0H                                                               
         OI    EGOLSTUS,EGOLLTPF   SO APPLICATION WILL GOTO VALREC              
         B     VPFPROCX                                                         
                                                                                
VPF210   GOTO1 CALLPROG,BLOCK,0,=C'DR ',=C'DISPLAY',0,(7,EDRSTAT),     X        
               (17,EDRPERI),0                                                   
                                                                                
VPF220   GOTO1 CALLPROG,BLOCK,0,=C'GOAL',=C'DISPLAY',0,(7,EGOLSTAT),   X        
               (17,EGOLPERI),0                                                  
                                                                                
VPF230   GOTO1 CALLPROG,BLOCK,0,=C'GOALN',=C'REPORT',=C'NOW,GOL',      X        
               (2,=C'..'),(7,EGOLSTAT),(17,EGOLPERI),0                          
                                                                                
VPF240   GOTO1 CALLPROG,BLOCK,0,=C'GOALN',=C'DISPLAY',0,(7,EGOLSTAT),  X        
               (17,EGOLPERI),0                                                  
                                                                                
VPF250   DS    0H                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
                                                                                
         LA    R1,DMCB                                                          
                                                                                
         CLI   HALF+1,3            PFKEY=3, FIRST RECORD SELECTED?              
         BNE   VPF260                                                           
         OC    PENDREC1,PENDREC1   CHECK IF WE HAVE A CONTRACT NUMBER           
         BZ    VPFPROCX                                                         
         LA    RE,PENDREC1         YES, PASS IT TO THE CONTRACT PROGRAM         
         ST    RE,4(R1)                                                         
         B     VPF280                                                           
                                                                                
VPF260   DS    0H                                                               
         CLI   HALF+1,4            PFKEY=4, SECOND RECORD SELECTED?             
         BNE   VPF270                                                           
         OC    PENDREC2,PENDREC2   CHECK IF WE HAVE A CONTRACT NUMBER           
         BZ    VPFPROCX                                                         
         LA    RE,PENDREC2         YES, PASS IT TO THE CONTRACT PROGRAM         
         ST    RE,4(R1)                                                         
         B     VPF280                                                           
                                                                                
VPF270   DS    0H                                                               
         CLI   HALF+1,5            PFKEY=5, THIRD RECORD SELECTED?              
         BNE   VPFPROCX                                                         
         OC    PENDREC3,PENDREC3   CHECK IF WE HAVE A CONTRACT NUMBER           
         BZ    VPFPROCX                                                         
         LA    RE,PENDREC3         YES, PASS IT TO THE CONTRACT PROGRAM         
         ST    RE,4(R1)                                                         
                                                                                
VPF280   DS    0H                                                               
         GOTO1 CGLOBBER,DMCB,=C'PUTD',,L'PENDREC1,GLRCONNO                      
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',PENDFILT,L'PENDFILT,GLRPFKEY              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
                                                                                
         LA    R2,CONSERVH         POINT AT SVC REQ FLD HDR                     
         OI    6(R2),X'C0'         XMIT FIELD                                   
         XC    CONSERV,CONSERV                                                  
         MVC   CONSERV(10),=C'+C,SV     '                                       
         B     VPFPROCX                                                         
*                                                                               
VPFPROCX DS    0H                                                               
         XMOD1                                                                  
         EJECT                                                                  
RELO2    DS    A                                                                
PFTABLE  DS    0F                                                               
                                                                                
* BYTE 1:    SAVED SCREEN NUMBER                                                
* BYTE 2:    PFKEY NUMBER                                                       
* BYTE 3-4:  SPARE                                                              
* BYTE 5-8:  A(CALLPROG CALL)                                                   
                                                                                
         DC    X'D1040000',A(VPF40)       PHEA CHA --> PF4 --> PDET UPD         
         DC    X'D1060000',A(VPF50)       PHEA CHA --> PF6 --> PHEA REP         
         DC    X'C2030000',A(VPF60)       PDET UPD --> PF3 --> PHEA CHA         
         DC    X'C2060000',A(VPF50)       PDET UPD --> PF6 --> PHEA REP         
         DC    X'D2030000',A(VPF60)       PDET CHA --> PF3 --> PHEA CHA         
         DC    X'D2060000',A(VPF50)       PDET CHA --> PF6 --> PHEA REP         
         DC    X'D3030000',A(VPF60)       PHEA REP --> PF3 --> PHEA CHA         
         DC    X'D3040000',A(VPF40)       PHEA REP --> PF4 --> PDET UPD         
         DC    X'D4040000',A(VPF70)       AHEA CHA --> PF4 --> ADET UPD         
         DC    X'D4060000',A(VPF80)       AHEA CHA --> PF6 --> AHEA REP         
         DC    X'C4030000',A(VPF90)       ADET UPD --> PF3 --> AHEA CHA         
         DC    X'C4060000',A(VPF80)       ADET UPD --> PF6 --> AHEA REP         
         DC    X'D5030000',A(VPF90)       ADET CHA --> PF3 --> AHEA CHA         
         DC    X'D5060000',A(VPF80)       ADET CHA --> PF6 --> AHEA REP         
         DC    X'D6030000',A(VPF90)       AHEA REP --> PF3 --> AHEA CHA         
         DC    X'D6040000',A(VPF70)       AHEA REP --> PF4 --> ADET UPD         
*                                                                               
         DC    X'C5030000',A(VPF100)      DR   CHA --> PF3 --> DRN  DIS         
         DC    X'C5040000',A(VPF110)      DR   CHA --> PF4 --> DR   REP         
         DC    X'C5050000',A(VPF120)      DR PFKEY ADD STAY IN SCRN C5          
         DC    X'C5060000',A(VPF120)      DR PFKEY DEL STAY IN SCRN C5          
         DC    X'C7030000',A(VPF210)      DRN  CHA --> PF3 --> DR   DIS         
         DC    X'C7040000',A(VPF110)      DRN  CHA --> PF4 --> DR   REP         
         DC    X'C7050000',A(VPF120)      DRN PFKEY ADD STAY IN SCRN C7         
         DC    X'C7060000',A(VPF120)      DRN PFKEY DEL STAY IN SCRN C7         
*                                                                               
         DC    X'CD030000',A(VPF220)    GOALN CHA --> PF3 --> GOAL  DIS         
         DC    X'CD040000',A(VPF230)    GOALN CHA --> PF4 --> GOAL  REP         
         DC    X'CD050000',A(VPF120)    GOALN PFKEY ADD STAY IN SCRN CD         
         DC    X'CD060000',A(VPF120)    GOALN PFKEY DEL STAY IN SCRN CD         
         DC    X'DB030000',A(VPF240)    GOAL  CHA --> PF3 --> GOALN DIS         
         DC    X'DB040000',A(VPF230)    GOAL  CHA --> PF4 --> GOALN REP         
         DC    X'DB050000',A(VPF120)    GOAL PFKEY ADD STAY IN SCRN DB          
         DC    X'DB060000',A(VPF120)    GOAL PFKEY DEL STAY IN SCRN DB          
         DC    X'DB070000',A(VPF120)    GOAL PFKEY PGUP STAY IN SCRN DB         
         DC    X'DB080000',A(VPF120)    GOAL PFKEY PGDN STAY IN SCRN DB         
*                                                                               
         DC    X'DE030000',A(VPF250)    PENDCOM JUMP TO CONTRACT PROG           
         DC    X'DE040000',A(VPF250)    PENDCOM JUMP TO CONTRACT PROG           
         DC    X'DE050000',A(VPF250)    PENDCOM JUMP TO CONTRACT PROG           
*                                                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
UPGD     DSECT                                                                  
CUPTYPE  DS    XL1                 UPGRADE TYPE                                 
*CUPTRTG EQU   2                   RATING UPGRADE                               
*CUPTHUT EQU   3                   HUT UPGRADE (CUPSTYP NE P)                   
*CUPTPUT EQU   3                   PUT UPGRADE (CUPSTYP EQ P)                   
*                                  CUPFLD1 = OLDHPT SOURCE BOOK                 
*CUPTNDX EQU   4                   INDEX UPGRADE                                
*CUPTHPT EQU   6                   H/P/T UPGRADE                                
CUPSTYP  DS    XL1                 UPGRADE SUB-TYPE (P=PUT UPGRADE)             
*                                  UGRADE BOOK/INDEX VALUES                     
CUPFLD1  DS    XL2                                                              
CUPFLD2  DS    XL2                                                              
CUPFLD3  DS    XL2                                                              
CUPFBK   DS    XL2                 FROM BOOK (SHARES)                           
CUPUDAY  DS    XL1                 DAY CODE                                     
CUPUTIM  DS    XL4                 START AND END TIMES (BINARY)                 
CUPSTA   DS    CL5                 STATION CALL LETTERS                         
         EJECT                                                                  
         EJECT                                                                  
*************                                                                   
* DDSPOOLD  *                                                                   
*************                                                                   
         SPACE 1                                                                
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
**************                                                                  
* DDSPLWORKD *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
************                                                                    
* REBASFFD *                                                                    
************                                                                    
         SPACE 1                                                                
       ++INCLUDE REBASFFD                                                       
         SPACE 1                                                                
************                                                                    
* REBASD1D *                                                                    
************                                                                    
         SPACE 1                                                                
         ORG   CONTAGH                                                          
       ++INCLUDE REBASD1D                                                       
         SPACE 1                                                                
**********************************************                                  
* DDGENTWA - DSECT TO COVER GENCON TWA AREAS *                                  
**********************************************                                  
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
**************                                                                  
* REBASWTWA  *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE REBASWTWA                                                      
         EJECT                                                                  
**************                                                                  
* REBASWORKD *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE REBASWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE - APPLICATIONS USE                             *         
***********************************************************************         
*        ORG   SYSSPARE                                                         
*ECBLK   DS    CL1024              SECURITY BLOCK FOR SECRET                    
         EJECT                                                                  
****************************                                                    
* REGENINV (RINVD DSECT)   *                                                    
* REGENOVR                 *                                                    
* REGENALLA                *                                                    
* DEDEMFILE                *                                                    
* CTGENFILE                *                                                    
* DDCOMFACS                *                                                    
* FAFACTS                  *                                                    
* FATIOB                   *                                                    
* DDREPMASTD               *                                                    
* DDCOREQUS                *                                                    
* DEDBLOCK (DBLOCKD DSECT) *                                                    
* SPRANSIDD (SRBLKD DSECT) *                                                    
* FASECRETD SECPARMD DSECT *                                                    
****************************                                                    
         SPACE 1                                                                
         PRINT OFF                                                              
RINVD    DSECT                                                                  
       ++INCLUDE REGENINV                                                       
       ++INCLUDE REGENOVR                                                       
       ++INCLUDE REGENALLA                                                      
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
SRBLKD   DSECT                                                                  
       ++INCLUDE SPRANSIDD                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE REGENAGY2                                                      
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'255REBAS00   05/01/02'                                      
         END                                                                    
