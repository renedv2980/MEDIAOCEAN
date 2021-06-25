*          DATA SET RENWR00    AT LEVEL 119 AS OF 01/22/13                      
*PHASE T83000B,*                                                                
*INCLUDE KHDUMMY                                                                
*          TITLE 'T83000 - REPPAK WRITER CONTROLLER'                            
           TITLE 'T83000 - REPPAK WRITER CONTROLLER'                            
***********************************************************************         
*                                                                     *         
*          TITLE  T83000 - REPPAK WRITER CONTROLLER                   *         
*  COMMENTS: REPPAK WRITER CONTROLLER                                 *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK REG                                              *         
*          R4 - WORK REG, KEY DSECT POINTER, GETEL REG                *         
*          R5 - POINTER TO VLPARMS                                    *         
*          R6 - POINTER TO ELEMENTS IN RECORD                         *         
*          R7 - POINTER TO RENWRIOD                                   *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (RENWR00-T23000)         *         
*                  - IN AIO FROM HYPER CONTROLLER (T00A30)            *         
*             AIO2 -                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
*  HISTORY OF CHANGES                                                 *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
         TITLE 'T83000 - REPPAK WRITER CONTROLLER - INIT'                       
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T83000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WKEND-WKST,T83000,RR=R2,CLEAR=YES                                
*                                                                               
         LR    R9,R1               SAVE SYSTEM PARAMETERS POINTER               
*                                                                               
         LR    R8,RC               ESTABLISH SPOOL WORKAREA                     
         USING SPOOLD,R8                                                        
*                                                                               
         LA    RC,SPOOLEND         ESTABLISH GENCON WORKAREA                    
         USING GEND,RC                                                          
*                                                                               
         ST    R9,SYSPARMS         SAVE SYSTEM PARAMETERS POINTER               
*                                                                               
         LA    R9,IO                                                            
         AH    R9,=H'4000'         GRABBING 2 2000 BYTE I/O AREAS               
*                                                                               
         LA    R9,16(R9)           NEED SPACE FOR 2 8 BYTE LABELS               
         ST    R9,ASUBSYSD                                                      
         ST    R9,ASYSD                                                         
*                                                                               
         USING SUBSYSD,R9          ESTABLISH WRITER WORKING STORAGE             
*                                                                               
         ST    R2,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         L     R1,SYSPARMS         ESTABLISH TWA                                
         L     RA,4(R1)                                                         
         ST    RA,ATWA                                                          
         USING T830FFD,RA                                                       
*                                                                               
         LA    R7,RENWRIOD         ESTABLISH RENWRIO WORKAREA                   
         USING RENWRIOD,R7                                                      
*                                                                               
         MVI   SPACES,C' '         INIT SPACES FIELD                            
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
*                                                                               
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT KEY FOR GENCON                  
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE FIELD IS ALWAYS MODIFIED             
         OI    CONSERVH+6,X'80'    FORCE RE-TRANSMISSION                        
*                                                                               
         BAS   RE,CHKTOP           CHECK TOP OF SCREEN                          
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*                                                                               
         CLI   CONWHEN+5,0         IF NOT A REPORT FUNCTION                     
         BNE   *+8                                                              
         OI    GENSTAT2,DISTHSPG      DON'T SKIP TO NEXT LIST PAGE              
*                                                                               
         MVI   FILTIDNO,11         FILTER ID NUM                                
*                                                                               
*******  OI    GENSTAT3,DIEONERR   SHOULD NOT BE ERROR MSGS                     
*                                                                               
         OI    GENSTAT5,GENPRVAL   HAVE GENPRG SET ALL FLDS VALID               
         OI    GENSTAT5,USERFPXT   USE EXTENDED SYMBOL TABLE FOR RFP            
         OI    GENSTAT5,GENSELVR   ALWAYS GO TO VALREC ON SELECT                
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   SKIP IF OFF-LINE                             
         BNZ   BRWSX                                                            
*                                                                               
*        HANDLE RETURNS FROM BROWSE WITH THIS CALL                              
*                                                                               
         GOTOX (RFBROWSE,VREPFACS),DMCB,ACOMFACS,SYSRD,0                        
*                                                                               
BRWSX    DS    0H                                                               
*                                                                               
         TITLE 'T83000 - REPPAK WRITER CONTROLLER - HELPINIT'                   
***********************************************************************         
*                                                                     *         
*        INITIALIZE HELP CONTROL BLOCK AND CHECK IF IN THE MIDST      *         
*           OF A HELP CALL                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HELPINIT DS    0H                                                               
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   SKIP IF OFF-LINE                             
         BNZ   HINI10                                                           
*                                                                               
         LA    R2,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R2                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         MVC   HCBMTIC,=AL2(RKQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D1'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN FIRST TWA PAGE                       
         MVC   HCBATWA,ATWA        SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
         MVC   HCBATIOB,ATIOB                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,HCBSYSRE    SET SYSTEM AS REP                            
         MVC   HCBATAB,ATIA        USE TIA FOR DDVAL TABLES                     
         LH    RF,=Y(HELPSAVE-T830FFD)  DISPLACEMENT OF SAVEAREA                
         LA    RF,T830FFD(RF)      HELP SAVE AREA                               
         ST    RF,HCBASAVE         PASS ADDRESS                                 
         MVI   HCBSEGS,34          34 SEGMENTS IN TABLE                         
         MVI   HCBQVAL,QREVAL      REVAL DDCOREQUS NUMBER                       
*                                                                               
         GOTO1 VREHELP,DMCB,0,0,HELPCB GO CHECK IF IN MIDDLE OF MENU            
*                                                                               
         TM    HCBRTRN,HCBRSELQ    IF SELECTION WAS MADE                        
         BO    EXIT                   EXIT FOR USER TO COMPLETE ENTRY           
*                                                                               
         DROP  R2                                                               
*                                                                               
HINI10   DS    0H                                                               
*                                                                               
INITX    DS    0H                  END OF INITIALIZATION                        
*                                                                               
         LR    R0,RC               SAVE WORIKING STORAGE POINTER                
*                                                                               
         GOTO1 =A(GOGENCON),RR=RELO  OFF TO GENCON                              
*                                                                               
*        IF OFF-LINE NEED TO FREE DPG BUFFER                                    
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    INITDPGX                                                         
*                                                                               
         ICM   R3,15,DRBFEND       GET SIZE OF GETMAIN AREA                     
         S     R3,DRBFSTRT         SAVED BUFFER START                           
*                                                                               
         LA    R4,DRBFSTRT         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         FREEMAIN EC,LV=(R3),A=(R4)  FREE CORE                                  
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
INITDPGX DS    0H                                                               
*                                                                               
GOGENCNX DS    0H                                                               
*                                                                               
         TITLE 'T83000 - REPPAK WRITER CONTROLLER - EXIT'                       
***********************************************************************         
*                                                                     *         
*        END OF PROCESSING - RETURN SCREEN                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - SYSINIT'                  
***********************************************************************         
*                                                                     *         
*              SYSTEM INITIALIZATION FOR GENCON                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSINIT  NTR1  LABEL=*                                                          
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'            ASSUME NON-DDS TERMINAL                      
*                                                                               
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'               SET TO DDS TERMINAL                       
*                                                                               
         MVC   TERM,TWATRM         SAVE TERMINAL ID                             
         MVC   WRIAUTH,TWAAUTH     SAVE PROGRAM AUTHORIZATION                   
         MVC   USERID,TWAORIG      SAVE USER ID                                 
         MVC   AGYALPHA,TWAAGY     SAVE AGENCY ALPHA                            
         MVC   AGENCY,TWAAGY       SAVE AGENCY ID                               
*                                                                               
         MVI   FILTIDNO,11         PROGRAM FILTER FIELD ID 10                   
         OI    GENSTAT5,GENPRVAL   HAVE GENPRG SET ALL FLDS VALID               
*                                                                               
         MVI   TWANSAVE,0          OUTSMART GENCON - DON'T RESTORE TWA          
*                                                                               
*        SET UP CERTAIN ADDRESSES                                               
*                                                                               
         L     R1,SYSPARMS                                                      
         MVC   ATIOB+1(3),1(R1)    P1 1-3 HAS A(TIOB)                           
*                                                                               
         LM    R3,R4,12(R1)        A(TIA) A(COMFACS)                            
*                                                                               
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
*                                                                               
         ST    R3,ATIA                                                          
*                                                                               
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   DEMOVAL,CDEMOVAL                                                 
         MVC   SCANNER,CSCANNER                                                 
*                                                                               
*        OBTAIN CORE-RESIDENT ADDRESSES                                         
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS4     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SYS4                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
*        SET UP SUB-ROUTINE ADDRESSES                                           
*                                                                               
         MVC   DMCB+4(4),=X'D9000AAD'   FIND A(GENERAL ROUTINES)                
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VRWGEN,DMCB         SAVE MODULE ADDRESS                          
*                                                                               
         L     R2,VRWGEN           A(GENERAL ROUTINES OVERLAY ADDRESS)          
*                                                                               
         SR    R3,R3               SUBROUTINE COUNTER                           
         LA    R4,SYSCOMM          START OF SUBROUTINE ADDRESSES                
         LA    R5,NSYSCOMM         NUMBER OF SUBROUTINES                        
*                                                                               
SYSCOMLP ST    R2,0(R4)            SET COMMON BRANCH ADDRESSES                  
         STC   R3,0(R4)                                                         
*                                                                               
         LA    R3,1(,R3)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R5,SYSCOMLP                                                      
*                                                                               
         LA    R4,WRICOMM                                                       
         LA    R5,NWRICOMM                                                      
*                                                                               
SYSWCOML ST    R2,0(R4)            SET COMMON BRANCH ADDRESSES                  
         STC   R3,0(R4)                                                         
*                                                                               
         LA    R3,1(,R3)                                                        
         LA    R4,4(,R4)                                                        
         BCT   R5,SYSWCOML                                                      
*                                                                               
*        LOAD ADDRESSES OF OFF-LINE ROUTINES                                    
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    SYSOFFX                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000AA9'   FIND A(OFFLINE ROUTINES)                
         GOTO1 CALLOV,DMCB,0                                                    
*                                                                               
         MVC   VRWOFF,DMCB         SAVE RENWROFF ADDRESS                        
*                                                                               
         L     R2,VRWOFF           GET GENERAL OFFLINE OVERLAY ADDR             
*                                                                               
         SR    R3,R3                                                            
         LA    R4,OFFCOMM                                                       
         LA    R5,NOFFCOMM                                                      
*                                                                               
SYSOFFLP ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
*                                                                               
         BCT   R5,SYSOFFLP                                                      
*                                                                               
SYSOFFX  DS    0H                                                               
*                                                                               
*              OTHER INITIALIZATION                                             
*                                                                               
*                                  CLEAR REQ RECORDS IN RENWRIOD                
*                                  FOR VALU2 ROUTINES                           
         MVC   QRECORD,SPACES                                                   
         MVC   QRECORD2,SPACES                                                  
         MVC   QRECORD3,SPACES                                                  
         SPACE                                                                  
*                                  SEED SYSD WITH DUMP COMMENTS                 
         SPACE                                                                  
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPTLVL,=C'*REPVAL*'                                            
         MVC   DUMPRPGN,=C'*RERPGN*'                                            
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         MVC   DUMPTLIO,=C'*SYSIOD*'                                            
*                                                                               
*        SET BUFFER ADDRESSES                                                   
*                                                                               
         LH    R1,=Y(BUFF-SUBSYSD)                                              
         LA    R1,SUBSYSD(R1)                                                   
         MVC   0(8,R1),=C'**DPG***'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADPGPROG                                                      
*                                                                               
         ST    R1,DRSTBUF                                                       
*                                                                               
         LA    R1,3000(R1)                                                      
         ST    R1,DRENDBUF                                                      
*                                                                               
         MVC   0(8,R1),=C'*DRONIO*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,DRTALIO                                                       
         LA    R1,1000(R1)                                                      
*                                                                               
         MVC   0(8,R1),=C'*COLFLT*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ACOLFILT                                                      
*                                                                               
*        IF OFF-LINE NEED TO REASSIGN DPG BUFFER TO GETMAIN                     
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    SYSIDPGX                                                         
*                                                                               
         ICM   R3,15,*+8           GET SIZE OF GETMAIN AREA                     
         B     *+8                                                              
         DC    AL4(20*1024)             20K BUFFER                              
*                                                                               
         LA    R4,DRBFSTRT         BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,DRBFSTRT      BUFFER START ADDRESS                         
         MVC   0(8,R1),=C'***DPG**'   FLAG AREA                                 
         LA    RF,8(R1)                                                         
         ST    RF,DRSTBUF          BUFFER START ADDRESS                         
         ST    RF,ADPGPROG                                                      
*                                                                               
         LA    R1,0(R3,R1)         BUFFER END ADDRESS                           
         ST    R1,DRENDBUF                                                      
         ST    R1,DRBFEND                                                       
*                                                                               
SYSIDPGX DS    0H                                                               
*                                                                               
*                                  SET SYSTEM DEPENDENT VALUES                  
         L     RF,=V(DUMMY)        END OF SYSTEM BASE                           
         A     RF,RELO                                                          
         ST    RF,SYSDUMMY                                                      
*                                                                               
         MVI   SYSTEM,C'R'         REP                                          
         MVI   MAXIOS,2            USES 2 I/O AREAS                             
         MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         L     RF,=A(VALUSER)                                                   
         A     RF,RELO                                                          
         ST    RF,GETUSER          ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'27'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   SYSFIL,=C'REPFILE '                                              
         MVC   REQFILE,=C'REPREQ '                                              
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   GETMSYS,71          USES GETMSG FOR SYSTEM 71-REPWRITER          
         MVC   LWORK,=AL4(WKEND-WKST) WE TOOK XXXXX BYTES IN NMOD               
         MVC   RCPROG(2),=C'RE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9083000'    PRESET FOR SYSTEM CALLOVS               
*                                                                               
         LA    RF,RECACTS            RECORD/ACTION DIRECTORY                    
         ST    RF,ARECACT            RECORD/ACTION DIRECTORY                    
*                                                                               
         LA    R1,RGD                                                           
         ST    R1,ASTARTSV                                                      
*                                                                               
         OI    GENSTAT1,RDUPAPPL   DON'T READ FOR UPDATE                        
*                                                                               
         LH    R1,=Y(VLPARMS-SUBSYSD)                                           
         LA    R1,SUBSYSD(R1)                                                   
         ST    R1,AVLPARMS         DDVAL PARAMETERS                             
*                                                                               
         LH    R1,=Y(VLTABC-SUBSYSD)                                            
         LA    R1,SUBSYSD(R1)                                                   
         ST    R1,AVLTAB           DDVAL TABLE AREA                             
*                                                                               
         L     RE,=A(BUFFWRKC-WKLND)   FILTER TABLE DISPLACEMENT                
         AR    RE,R8               FILTER TABLE ADDRESS                         
         MVC   0(8,RE),=CL8'**FLTTAB'    MARK WORK AREA                         
         LA    RE,8(RE)            POINT TO START OF TABLE                      
         ST    RE,AFLTTAB          SAVE COLUMN FILTER AREA ADDRESS              
*                                                                               
         L     RF,=A(BUFFWRKC-WKLND)   FILTER TABLE DISPLACEMENT                
         AR    RF,R8                                                            
         L     RE,=A(OPTPRTID-BUFFWRKD)   OPTIONS PRINT BUFFER DISP             
         AR    RE,RF               A(OPTIONS PRINT BUFFER)                      
         MVC   0(8,RE),=CL8'**OPTPRT'    MARK WORK AREA                         
         LA    RE,8(RE)            POINT TO START OF BUFFER                     
         ST    RE,OPTPRTBA         SAVE COLUMN BUFFER AREA ADDRESS              
*                                                                               
         GOTO1 INITDRON            INITIALIZE DRONE EARLY                       
*                                                                               
*        READ REP RECORD                                                        
*                                                                               
         XC    KEY,KEY             INIT KEY AREA                                
         LA    R4,KEY              ESTABLISH REPREC KEY                         
         USING RREPKEY,R4                                                       
*                                                                               
         MVI   RREPKTYP,RREPKTYQ   SET RECORD ID                                
         MVC   RREPKREP,AGENCY     SET REP ID                                   
*                                                                               
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=CL8'REPDIR',KEY,KEY                   
*                                                                               
         CLC   RREPKEY,KEYSAVE     MUST FIND IT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'GETREC',=CL8'REPFIL',KEY+28,IO,DMWORK          
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'RENWR01 - REP WRITER REPORT MASTER - SYGTPRF'                   
********************************************************************            
*                                                                  *            
*        SET REQUEST PROFILE FROM REP RECORD                       *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
SYGTPRF  DS    0H                                                               
*                                                                               
         XC    REREQPRF,REREQPRF   INIT REQ PROGRAM PROFILE                     
*                                                                               
         MVI   ELCODE,X'04'        SET TO FIND PROGRAM PROFILE ELM              
         LA    R6,IO                                                            
         BRAS  RE,GETEL                                                         
         BNE   SYGTPRFX            NO PROFILE FOUND                             
*                                                                               
*- FIND REQ PROGRAM PROFILE WITHIN PROGRAM PROFILE ELEMENT                      
*                                                                               
         USING RREPPGMP,R6         ESTABLISH PROGRAM PROFILE ELEMENT            
*                                                                               
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
*                                                                               
         CLI   0(RE),RREPQREQ      LOOKING FOR REQ PROGRAM PROFILE              
         BE    *+16                                                             
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,*-12                                                          
         B     *+10                SKIP PROFILE SAVE IF NO MATCH                
*                                                                               
         MVC   REREQPRF,2(RE)      SAVE PROFILE                                 
*                                                                               
         CLI   DDS,C'Y'            SKIP IF DDS TERMINAL                         
         BE    SYGTPRFX                                                         
*                                                                               
         TM    REREQPRF+1,X'80'    IF  TEMPLATES PROTECTED                      
         BNO   SYGTPRFX                                                         
*                                                                               
         TM    TWAAUTH,X'40'       AND IF NOT ALLOWED TO CHANGE FORMATS         
         BO    *+8                                                              
         OI    GENSTAT4,NODELLST      STOP DELETING FROM LIST                   
*                                                                               
SYGTPRFX DS    0H                                                               
*                                                                               
SYSINTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - CHKTOP'                   
***********************************************************************         
*                                                                     *         
*          ERASE WHEN FIELD IF ACTION IS NOT REPORT                   *         
*            STOPS ANNOYING ERROR MESSAGE                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CHKTOP   NTR1  LABEL=*                                                          
*                                  GET TERMINAL VALUES                          
         ZIC   RE,CONACTH+5                                                     
         CH    RE,=H'3'            CHECK MAX 3 CHARACTERS                       
         BNH   *+8                                                              
         LA    RE,3                                                             
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         BM    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=CL8'REPORT' IF ACTION IS NOT REPORT                   
         BE    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'LIST'  OR LIST                                      
         BE    CTX                                                              
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   CONACT(0),=C'SELECT' OR SELECT                                   
         BE    CTX                                                              
         XC    CONWHEN,CONWHEN     THEN ERASE PRINT OPTION FIELD                
         OI    CONWHENH+6,X'80'                                                 
         MVI   CONWHENH+5,0                                                     
CTX      LR    RE,R0                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T83000 - REPPAK WRITER CONTROLLER - GOGENCON'                   
***********************************************************************         
*                                                                     *         
*        INTERFACE TO GENCON                                          *         
*           SEPARATE NMOD ONLY TO GET MORE WORKING STORAGE            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GOGENCON NMOD1 GENCONWL,**#GEN,CLEAR=YES                                        
*                                                                               
         LR    RF,RC               SAVE NEW WORKING STORE PTR                   
         LR    RC,R0               RESTORE MASTER WORKING STORAGE PTR           
*                                                                               
         USING GENCONWD,RF         ESTABLISH GENCON WORK                        
*                                                                               
*        SET UP 3 IOAREAS                                                       
*                                                                               
         L     R1,=AL4(IOAREA1-GENCONWD)   DISPLACEMENT OF IOAREA               
         AR    R1,RF               RELOCATE ADDRESS                             
         SH    R1,=H'8'            BACK UP FOR ID                               
         MVC   0(8,R1),=C'**IOA1**'  IDENTIFY AREA                              
         LA    R1,8(R1)            RE-POINT TO IOAREA                           
         ST    R1,REAIO1           PASS TO REST OF PROGRAM                      
*                                                                               
         L     R1,=AL4(IOAREA2-GENCONWD)   DISPLACEMENT OF IOAREA               
         AR    R1,RF               RELOCATE ADDRESS                             
         SH    R1,=H'8'            BACK UP FOR ID                               
         MVC   0(8,R1),=C'**IOA2**'  IDENTIFY AREA                              
         LA    R1,8(R1)            RE-POINT TO IOAREA                           
         ST    R1,REAIO2           PASS TO REST OF PROGRAM                      
*                                                                               
         L     R1,=AL4(IOAREA3-GENCONWD)   DISPLACEMENT OF IOAREA               
         AR    R1,RF               RELOCATE ADDRESS                             
         SH    R1,=H'8'            BACK UP FOR ID                               
         MVC   0(8,R1),=C'**IOA3**'  IDENTIFY AREA                              
         LA    R1,8(R1)            RE-POINT TO IOAREA                           
         ST    R1,REAIO3           PASS TO REST OF PROGRAM                      
*                                                                               
*MN                                                                             
         L     R1,=AL4(IOAREA4-GENCONWD)   DISPLACEMENT OF IOAREA               
         AR    R1,RF               RELOCATE ADDRESS                             
         SH    R1,=H'8'            BACK UP FOR ID                               
         MVC   0(8,R1),=C'**IOA4**'  IDENTIFY AREA                              
         LA    R1,8(R1)            RE-POINT TO IOAREA                           
         ST    R1,REAIO4           PASS TO REST OF PROGRAM                      
*MN                                                                             
*                                                                               
         GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON                                
*                                                                               
GOGENCX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T83003 - REPPAK WRITER CONTROLLER - VALUSER '                   
***********************************************************************         
*                                                                     *         
*        VALIDATE USER ID                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALUSER  NTR1  BASE=*,LABEL=*      VALIDATE USER ID                             
*                                                                               
         USING SPOOLD,R8                                                        
*                                                                               
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD                                                         
         USING SUBSYSD,R9          SYSTEM SPECIFIC WORK                         
*                                                                               
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         L     R1,SYSPARMS                                                      
*        MVC   AGENCY,0(R1)                                                     
         MVC   AGYSIGN,SPACES                                                   
         XC    AGYALPHA,AGYALPHA                                                
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R4,CTIDATA                                                       
         SR    R3,R3                                                            
         SPACE                                                                  
VUSER10  CLI   0(R4),0                                                          
         BE    VUSER30                                                          
         CLI   0(R4),X'02'                                                      
         BE    VUSER24                                                          
         CLI   0(R4),X'06'                                                      
         BE    VUSER26                                                          
         CLI   0(R4),X'21'                                                      
         BE    VUSER28                                                          
         SPACE                                                                  
VUSER16  IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     VUSER10                                                          
         SPACE                                                                  
         USING CTDSCD,R4                                                        
VUSER24  MVC   AGYSIGN,CTDSC                                                    
         B     VUSER16                                                          
         SPACE                                                                  
         USING CTAGYD,R4                                                        
VUSER26  MVC   AGYALPHA,CTAGYID                                                 
         B     VUSER16                                                          
         SPACE                                                                  
         USING CTSYSD,R4                                                        
VUSER28  CLI   CTSYSNUM,8          THIS ONE'S FOR REPPAK                        
         BNE   VUSER16                                                          
         OC    TWAACCS(2),TWAACCS  ACCESS                                       
         BNZ   *+10                                                             
         MVC   TWAACCS,CTSYSLMT                                                 
         B     VUSER16                                                          
         SPACE                                                                  
VUSER30  XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         LA    R4,FACTWRK                                                       
****     GOTO1 GETFACT,DMCB,(R4)                                                
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,0(R1)            POINT TO FAFACTS AREA                        
         MVC   FACTWRK,0(R4)       SAVE FACTS                                   
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - CORETAB'                  
***********************************************************************         
*                                                                     *         
*              CORETAB                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    AL1(QGENCON)        GENCON                                       
         DC    AL1(QDRONE)         DRONE                                        
         DC    AL1(QQSORT)         QSORT                                        
         DC    AL1(QGETBROD)       GET BROADCAST DATES                          
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QREFETCH)                                                    
         DC    AL1(QREVAL)         VALIDATION MODULE                            
         DC    AL1(QREVAL2)        VALIDATION TABLES                            
         DC    AL1(QPRHELP)        HELP MENUS                                   
         DC    AL1(QREPFACS)       REP FACILITIES - MAINLY BROWSE               
         DC    AL1(QNEUTIL)        NETDATES                                     
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - RECACTS'                  
***********************************************************************         
*                                                                     *         
*              RECORD/ACTION TABLES                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECACTS  DS    0D                                                               
         SPACE                                                                  
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
         SPACE                                                                  
         DC    X'04',C'WRITER  ',AL1(01),X'0000'                                
         DC    X'04',C'AUR     ',AL1(02),X'0000'                                
         DC    X'04',C'NWRITER ',AL1(03),X'0000'                                
         DC    X'04',C'TEST    ',AL1(04),X'0000'                                
         DC    X'04',C'XFILE   ',AL1(05),X'0000'                                
         SPACE 3                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
         SPACE                                                                  
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'CREATE  ',AL1(12,13,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
         SPACE 3                                                                
* X'03' ENTRIES ARE OK REC/ACT COMBOS                                           
* CL1 RECORD NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 PHASE NUMBER FOR SCREEN                                                   
* CL1 PHASE NUMBER FOR EDIT                                                     
* CL1 PHASE NUMBER FOR SPECS                                                    
* CL1 PHASE NUMBER FOR REPORT                                                   
* CL1 WHEN OK BITS 80=SCREEN 40=NOW 20=SOON 10=OV 08=DDS                        
* CL2 CODE FOR REPORTS                                                          
* CL2 CODE FOR EOD HANDLING                                                     
         SPACE                                                                  
*                                 SC  SP  OK                                    
*                                   OV  RP                                      
         DC    X'03',AL1(01,12),X'F101000138',C'WRXW'  REPORT WRITER            
         DC    X'03',AL1(02,12),X'F020002038',C'WRAU'  AUR REPORT               
         DC    X'03',AL1(03,12),X'F101000138',C'WRXW'  NEW REP WRITER           
         DC    X'03',AL1(04,12),X'F101000138',C'WRTW'  NEW REP WRITER           
         DC    X'03',AL1(05,12),X'F101000118',C'WRXF'  XFILE REPORTS            
         DC    X'FF'                                                            
         SPACE 3                                                                
*              PHASE USED UP BY SYSTEM SO FAR                                   
         SPACE                                                                  
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*                                                                               
*        0X     Y  Y                                                            
*        1X                                                                     
*        FX        Y                                                            
*                                                                               
*                                                                               
         DROP                                                                   
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - DATELSTD'                 
***********************************************************************         
*                                                                     *         
*        DSECT FOR DATELISTS FOUND IN DATEAREA                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE RENWRDLSTD                                                     
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - GEND'                     
***********************************************************************         
*                                                                     *         
*              GENCON OVERALL WORKAREAS DSECT                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
WKLND    DSECT                                                                  
WKST     DS    CL(SPOOLEND-SPOOLD)                                              
         DS    CL((GENDEND+16)-GEND)                                            
         DS    CL2000              (SPACE FOR SECOND I/O)                       
         DS    CL(SYSEND-WRISYS)                                                
BUFFWRKC DS    CL(BUFFWRKL)                                                     
WKEND    EQU   *                                                                
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - GENCONWD'                 
***********************************************************************         
*                                                                     *         
*              GENCON EXTRA WORKAREA                                  *         
*              CONTAINS 3 IOAREAS                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GENCONWD DSECT                                                                  
         DS    XL8                 IOAREA ID                                    
IOAREA1  DS    XL4096              IOAREA 1                                     
         DS    XL8                 IOAREA ID                                    
IOAREA2  DS    XL4096              IOAREA 2                                     
         DS    XL8                 IOAREA ID                                    
IOAREA3  DS    XL4096              IOAREA 3                                     
*MN                                                                             
         DS    XL8                 IOAREA ID                                    
IOAREA4  DS    XL4096              IOAREA 4                                     
*MN                                                                             
*                                                                               
*                                                                               
GENCONWL EQU   *-GENCONWD          LENGTH OF WORKAREA                           
*                                                                               
         USING GENCONWD,RF         ESTABLISH GENCON WORK                        
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - RENWRWORKD'               
***********************************************************************         
*                                                                     *         
*              WRITER WORKING STORAGE                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE RENWRWORKD                                                     
*                                                                               
           TITLE 'T83000 - REPPAK WRITER CONTROLLER - DSECTS'                   
***********************************************************************         
*                                                                     *         
*              OTHER DSECTS ARE HIDDEN IN HERE                        *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*CTGENDIC                                                                       
         PRINT OFF                                                              
       ++INCLUDE CTGENDIC                                                       
         PRINT ON                                                               
DBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDBLOCK                                                       
         PRINT ON                                                               
*RENWRFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE RENWRFFD                                                       
         PRINT ON                                                               
*DDGENTWA                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
*RENWRF1D                                                                       
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE RENWRF1D                                                       
         PRINT ON                                                               
         EJECT                                                                  
T830FFD  DSECT                                                                  
         ORG   CONHEADH-64+X'3000'                                              
HELPSAVE DS    XL512               HELP SAVEAREA                                
*                                                                               
*RENWREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE RENWREQUS                                                      
         PRINT ON                                                               
*PRVALTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRVALTABD                                                      
         PRINT ON                                                               
*DDPERVALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*DDFLDIND                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDIND                                                       
         PRINT ON                                                               
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*DDCOREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*DRGLOBAL                                                                       
         PRINT OFF                                                              
       ++INCLUDE DRGLOBAL                                                       
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDWIDED                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
*CTGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*REGENALL1                                                                      
         PRINT OFF                                                              
       ++INCLUDE REGENALL1                                                      
         PRINT ON                                                               
*REGENPWC                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENPWC                                                       
         PRINT ON                                                               
*REGENSET                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENSET                                                       
         PRINT ON                                                               
*REGENDCT                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
*REGENDSP                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENDSP                                                       
*REPFACSQ                                                                       
         PRINT OFF                                                              
       ++INCLUDE REPFACSQ                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'119RENWR00   01/22/13'                                      
         END                                                                    
