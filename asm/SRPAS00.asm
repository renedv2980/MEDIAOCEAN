*          DATA SET SRPAS00    AT LEVEL 007 AS OF 06/22/16                      
*PHASE T11E00A                                                                  
*INCLUDE KHDUMMY                                                                
*INCLUDE TIMEOUT                                                                
*INCLUDE NUMVAL                                                                 
*INCLUDE DATTIM                                                                 
*INCLUDE PWDVAL                                                                 
         TITLE 'SRPAS00 - PERSONAL PASSWORD LOGON SERVICE REQUEST'              
SRPAS00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,**$PAS**,R9,R8,CLEAR=YES,RR=RE                           
         ST    RE,RELO                                                          
                                                                                
*        SAVE ADDRESSES PASSED ON THE PARAMETER LIST FROM FAMONITOR             
*                                                                               
         USING CONTROLD,RC                                                      
         ST    R1,APARMS           SAVE PARAMETER LIST ADDRESS                  
         L     RF,0(R1)            SAVE A(SYSFACS)                              
         ST    RF,ASYSFACS                                                      
         L     RE,VSSB-SYSFACD(RF)                                              
         ST    RE,ASSB             SAVE A(SSB)                                  
         L     RF,SSBTKADR-SSBD(RE)                                             
         ST    RF,ATCB             SAVE A(TCB)                                  
         L     RF,4(R1)            SAVE A(TIA)                                  
         ST    RF,ATIA                                                          
         L     RF,8(R1)            SAVE A(UTL)                                  
         ST    RF,AUTL                                                          
         L     RF,12(R1)           SAVE A(COMFACS)                              
         ST    RF,ACOMFACS                                                      
         L     RA,20(R1)           SAVE A(TWA)                                  
         ST    RA,ATWA                                                          
         USING SRPASFFD,RA                                                      
         L     RF,28(R1)           SAVE A(TIOB)                                 
         ST    RF,ATIOB                                                         
         ST    RD,SAVERD           SAVE STACK ADDRESS FOR EXIT                  
                                                                                
* MAIN (HIGHEST LEVEL ROUTINE)                                                  
*                                                                               
MAIN     BAS   RE,INITNMOD         INITIALIZE ADDRS OF NMOD RESOURCES           
         BAS   RE,INITRTNS                    ROUTINE ADDRESSES                 
         BAS   RE,INITMISC                    OTHER VARIABLES                   
         BAS   RE,READSTR          READ SAVED STORAGE                           
*                                                                               
*NOP*    BAS   RE,HELPSCAN         SCAN FOR HELP REQUEST                        
*                                                                               
         BAS   RE,VALSRV           PROCESS SERVICE REQUEST FIELD                
         BNE   MXERR                                                            
         BAS   RE,SWITSYS          SWITCH TO CONTROL SYSTEM                     
         BNE   MXERR                                                            
         BAS   RE,VALUID           VALIDATE USER ID FIELD                       
         BNE   MXERR                                                            
         BAS   RE,VALTRM           VALIDATE TERMINAL COMPATIBLE                 
         BNE   MXERR                                                            
         BAS   RE,VALAGY           VALIDATE AGENCY ACCESS CONTROL               
         BNE   MXERR                                                            
         BAS   RE,VALPID           VALIDATE PERSONAL ID FIELD                   
         BNE   MXERR                                                            
         BAS   RE,VALOPWD          VALIDATE OLD PASSWORD FIELD                  
         BNE   MXERR                                                            
         BAS   RE,VALNPWD          VALIDATE NEW PASSWORD FIELD                  
         BNE   MXERR                                                            
         TM    INPFLAGS,INPFNPWQ                                                
         BZ    MAIN010                                                          
         BAS   RE,UPPERSON         UPDATE PERSON RECORD                         
         BNE   BOMB                                                             
         BAS   RE,UPPASSWD         UPDATE PASSWORD RECORDS                      
         BNE   BOMB                                                             
*                                                                               
MAIN010  CLI   MODE,MODEPASQ       TEST FOR =PASS MODE                          
         BNE   MAIN020                                                          
         BAS   RE,RECON            CALL RECONNECT PROCESS                       
         BNE   MXERR                                                            
         B     MX                                                               
*                                                                               
MAIN020  BAS   RE,VALCSYS          VALIDATE CONNECT SYSTEM FIELD                
         BNE   MXERR                                                            
         BAS   RE,VALCPRG          VALIDATE CONNECT PROGRAM FIELD               
         BNE   MXERR                                                            
         BAS   RE,VALCDATA         VALIDATE CONNECT DATA FIELD                  
         BNE   MXERR                                                            
         BAS   RE,CONNECT          CALL CONNECT PROCESS                         
         BNE   MXERR                                                            
*                                                                               
         BAS   RE,WRITESTR         WRITE SAVED STORAGE                          
*                                                                               
MX       BAS   RE,SWITBACK         EXIT OK                                      
         XC    SRVMSG,SRVMSG                                                    
         XC    SRVPWD,SRVPWD                                                    
         XC    SRVPWDN,SRVPWDN                                                  
         MVI   SRVPWDH+5,0                                                      
         MVI   SRVPWDNH+5,0                                                     
         OI    SRVPWDH+6,X'80'                                                  
         OI    SRVPWDNH+6,X'80'                                                 
         B     XDIRECT                                                          
*                                                                               
MXERR    BAS   RE,SWITBACK         HERE IF INPUT ERROR                          
         LA    RE,SAVEDSTR         IF NOT INITIALISE AREA                       
         LA    RF,SAVEDL                                                        
         XCEF                                                                   
         BAS   RE,WRITESTR                                                      
         XC    SRVPWD,SRVPWD                                                    
         MVI   SRVPWDH+5,0                                                      
         OI    SRVPWDH+6,X'80'                                                  
         XC    SRVPWDN,SRVPWDN                                                  
         MVI   SRVPWDNH+5,0                                                     
         OI    SRVPWDNH+6,X'80'                                                 
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         SET CURSOR                                   
         B     XDIRECT             EXIT MODULE                                  
*                                                                               
BOMB     DC    H'0'                                                             
                                                                                
* SAVE ADDRESSES OF SECTIONS OF MEMORY CREATED BY THE CONTROLLER'S              
* NMOD INSTRUCTION.  ADDITIONALLY, SPLIT UP MEMORY USED FOR IO AREAS            
* INTO TWO AND SAVE THE ADDRESS OF EACH.                                        
*                                                                               
INITNMOD NTR1                                                                   
         ST    RB,ABASE1           SAVE A(FIRST BASE)                           
         ST    R9,ABASE2           SAVE A(SECOND BASE)                          
         ST    R8,ABASE3           SAVE A(THIRD BASE)                           
*                                                                               
         LR    RF,RC               BUMP PAST FIRST HALF OF CONTROLD             
         A     RF,=A(LENCON)                                                    
*                                                                               
         ST    RF,ASAVE            SAVE A(CONTROLLER SAVED MEMORY)              
         A     RF,=A(LENSAVE)      BUMP PAST                                    
*                                                                               
         ST    RF,AIOS             SAVE A(DATAMGR IO AREAS)                     
         A     RF,=A(LENIOS)       BUMP PAST                                    
*                                                                               
         ST    RF,APCTWASV         SAVE A(PC SCREEN TWA SAVE AREA)              
         A     RF,=A(LENTWASV)     BUMP PAST                                    
                                                                                
*        SPLIT UP IO AREA MEMORY INTO THREE AND SAVE THE ADDRESS                
*        OF EACH SECTION                                                        
*                                                                               
         MVC   AIO1,AIOS           SAVE A(IOAREA #1)                            
         L     RF,AIO1                                                          
         LA    RF,2048(RF)                                                      
         ST    RF,AIO2             SAVE A(IOAREA #2)                            
         LA    RF,2048(RF)                                                      
         ST    RF,AIO3             SAVE A(IOAREA #3)                            
*                                                                               
         MVC   AIO,AIO1            SAVE A(CURRENT IO AREA)                      
*                                                                               
         B     XIT                                                              
                                                                                
* SAVE ADDRESSES OF THE ROUTINES USED BY $PASS. THERE ARE FOUR                  
* TYPES OF ROUTINES:                                                            
*                                                                               
*        1) ROUTINES FOUND IN FACPAK'S COMFACS DSECT                            
*        2) ROUTINES LINKED WITH THE CONTROLLER                                 
*        3) ROUTINES FOUND IN FACPAK'S CORE                                     
*        4) ROUTINES FOUND IN THE CONTROLLER                                    
*                                                                               
* ROUTINES LINKED WITH THE CONTROLLER AND ROUTINES IN THE CONTROLLER            
* ITSELF MUST BE DYNAMICALLY RELOCATED BY ADDING THE RELOCATION                 
* CONSTANT (RELO) TO EACH ROUTINE ADDRESS.                                      
                                                                                
INITRTNS NTR1                                                                   
         L     RF,ACOMFACS         SAVE ADDRESSES OF COMFACS ROUTINES           
         USING COMFACSD,RF                                                      
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   SCANNER,CSCANNER                                                 
         MVC   TERMVAL,CTERMVAL                                                 
         MVC   DATCON,CDATCON                                                   
         MVC   ADDAY,CADDAY                                                     
         MVC   GETRET,CGETRET                                                   
         MVC   GETFACT,CGETFACT                                                 
         MVC   HEXOUT,CHEXOUT                                                   
         MVC   DICTATE,CDICTATE                                                 
         MVC   HELLO,CHELLO                                                     
         MVC   GETTXT,CGETTXT                                                   
         MVC   GETHELP,CGETHELP                                                 
         MVC   SWITCH,CSWITCH                                                   
         DROP  RF                                                               
                                                                                
*        SAVE ADDRESSES OF LINKED ROUTINES                                      
*                                                                               
         LA    R2,LINKED           R2=A(FIRST VTYPE ADDRESS)                    
         LA    R3,EXTERNS          R3=A(FIRST RELOCATED ADDRESS)                
         LA    R4,NLINKED          R4=NUMBER OF LINKED ROUTINES                 
*                                                                               
         LTR   R4,R4               IF NO LINKED ROUTINES THEN DONE              
         BZ    IR20                                                             
*                                                                               
IR10     L     R1,0(R2)            R1=V-TYPE ADDRESS                            
         A     R1,RELO             ADD RELOCATION CONSTANT                      
         ST    R1,0(R3)            SAVE RELOCATED ADDRESS                       
*                                                                               
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR10             LOOP BACK                                    
                                                                                
*        SAVE ADDRESSES OF CORE RESIDENT ROUTINES                               
*                                                                               
IR20     XC    DMCB,DMCB           SET UP PARAMETERS TO CALLOV                  
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
         LA    R2,CORERES          R2=A(FIRST CORERES ROUTINE EQUATE)           
         LA    R3,COREADRS         R3=A(FIRST CORERES ROUTINE ADDR)             
         LA    R4,NCORERES         R4=NUMBER OF CORERES ROUTINES                
*                                                                               
         LTR   R4,R4               IF NO CORERES ROUTINES THEN DONE             
         BZ    IR40                                                             
*                                                                               
IR30     MVC   DMCB+7(1),0(R2)     INSERT OVERLAY NUMBER INTO PARAMETER         
*                                                                               
         GOTO1 CALLOV,DMCB         CALL CALLOV                                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                DIE IF CALLOV RETURNS ERROR                  
*                                                                               
         MVC   0(4,R3),DMCB        SAVE ROUTINE ADDRESS                         
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT ADDRESS                         
         LA    R3,4(R3)                                                         
         BCT   R4,IR30             LOOP BACK                                    
                                                                                
* SAVE ADDRESSES OF CONTROLLER ROUTINES                                         
*                                                                               
* ALL CONTROLLER ROUTINES ARE ENTERED THROUGH AN INTERMEDIATE                   
* ROUTINE CALLED VCOMMON.  VCOMMON SETS UP THE BASE REGISTERS                   
* AND CALLS THE DESIRED ROUTINE.  THE WAY THIS WORKS IS THAT ALL                
* CONTOLLER ROUTINE ADDRESSES ARE REALLY THE ADDRESS OF VCOMMON WITH            
* A ROUTINE NUMBER IN THE HIGH ORDER BYTE TO TELL VCOMMON WHICH ROUTINE         
* TO BRANCH TO.                                                                 
*                                                                               
IR40     LA    R2,VCOMMON          R2=A(VCOMMON)                                
         SR    R3,R3               R3=0                                         
         LA    R4,COMMADRS         R4=A(FIRST ROUTINE ADDRESS)                  
         LA    R5,VCOUNT           R5=NUMBER OF CONTROLLER ROUTINES             
*                                                                               
         LTR   R5,R5               IF NO CONTROLLER ROUTINES THEN DONE          
         BZ    IR60                                                             
*                                                                               
IR50     ST    R2,0(R4)            SAVE A(VCOMMON) IN LAST 3 BYTES              
         STC   R3,0(R4)            SAVE ROUTINE NUMBER IN FIRST BYTE            
*                                                                               
         LA    R3,1(R3)            BUMP ROUTINE NUMBER                          
         LA    R4,4(R4)            BUMP TO NEXT ROUTINE ADDRESS                 
         BCT   R5,IR50             LOOP BACK                                    
*                                                                               
IR60     DS    0H                                                               
*                                                                               
IRX      B     XIT                                                              
                                                                                
* INITIALIZE OTHER MISCELLANEOUS VARIABLES                                      
*                                                                               
INITMISC NTR1                                                                   
         L     RF,ASSB             RF=A(SSB)                                    
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL      GET TEMPSTR RECORD LENGTH                    
         MVC   CHKDSP,=Y(CHKPTDSP) DISPLACEMENT TO CHECK POINT                  
         MVC   GLODSP,=Y(CHKPTGLD) DISPLACEMENT TO GLOBASS                      
         DROP  RF                                                               
*                                                                               
         L     RF,AUTL             RF=A(UTL)                                    
         USING UTLD,RF                                                          
         OI    TFLAG,TFLAGIRB      INHIBIT BROADCAST MESSAGES                   
         MVC   TRMUSER,TUSER       USER ID                                      
         MVC   TRMSYS,TSYS         CURRENT CONNECTED SYSTEM                     
         MVC   TRM,TNUM            TERMINAL NUMBER                              
         MVC   TRMAGY,TAGY                                                      
         MVC   TRMAGYSE,TAGYSEC                                                 
         MVC   TRMAGYPE,TAGYPER                                                 
         MVC   TRMFLAG,TFLAG                                                    
         OC    TPASSWD,TPASSWD     GET PASSWORD NUM IF SET                      
         BZ    INMS010                                                          
         MVC   TRMPWDNO,TPASSWD                                                 
INMS010  MVC   TRMTYP,TSTAT                                                     
         MVC   TRMTYP1,TTYPE                                                    
         MVC   TRMCTRY,TCTRY                                                    
         MVI   DDSFLAG,X'00'       INITIALISE TERMINAL FLAG                     
         TM    TSTAT1,TSTATDDS                                                  
         BZ    *+8                                                              
         OI    DDSFLAG,DDSTRM                                                   
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    DDSFLAG,DDSNEW      FORCE RELOAD                                 
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
*                                                                               
         MVC   USERNUM,TUSER       SAVE USER ID NUMBER                          
         DROP  RF                                                               
*                                                                               
         MVC   CSPHASE,=X'D9012100'   PHASE PARAMETER FOR CALLOV CALL           
         MVI   CSSP,0                 OVERLAY STACK POINTER                     
         MVC   CSNXTLOD,DUMMY         A(NEXT PLACE TO LOAD OVERLAY)             
*                                                                               
         GOTO1 DICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                             
*                                                                               
         SR    R0,R0               GET DATE AND TIME                            
         SR    R1,R1                                                            
         STAM  ARE,AR1,DMCB                                                     
         TIME  BIN                 R0=DATE,R1=TIME                              
         LAM   ARE,AR1,DMCB                                                     
         STM   R0,R1,MVSTIME                                                    
         OI    MVSDATE+3,X'0F'                                                  
*                                                                               
         XC    HELP,HELP           INITIALISE HELP SCAN                         
         MVC   HELPKEY,HELPID                                                   
*                                  GET TODAYS DATE COMPRESSED                   
         GOTO1 DATCON,DMCB,(X'05',0),(X'02',TODAY)                              
         MVC   TODAYC,FFILL                                                     
         XC    TODAYC,TODAY        CREATE COMPLEMENT                            
*                                                                               
         GOTO1 GETFACT,DMCB,0      GET TODAYS DATE 3 BYTE BINARY                
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SYSDATEB,FADATEB                                                 
         MVC   ASYSLST,FASYSLST                                                 
*                                                                               
         GOTO1 =V(DATTIM),DMCB,(X'01',DATETIME),0,RR=RELO                       
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DATETIMC,DATETIME                                                
         XC    DATETIMC,FFILL                                                   
*                                                                               
         MVI   CONNUM,X'0A'        SET CONTROL SE NUMBER                        
*                                                                               
         LA    R1,SRVPIDH                                                       
         ST    R1,CURSOR                                                        
*                                                                               
INX      B     XIT                                                              
                                                                                
* SWITCH TO CONTROL SYSTEM TO ALLOW CTFILE UPDATES                              
*                                                                               
SWITSYS  NTR1                                                                   
         ICM   R0,15,FFILL                                                      
         GOTO1 SWITCH,DMCB,(CONNUM,(R0)),0                                      
         CLI   4(R1),0                                                          
         BE    SSYSYES                                                          
         CLI   4(R1),2             CONTROL SYSTEM IS NO-OP                      
         BE    ERRSWSYS                                                         
         B     ERRSWSYS            UNKNOWN FLOP OUT                             
*                                                                               
SSYSNO   B     NO                                                               
SSYSYES  MVI   SWITCHED,X'0A'                                                   
         B     YES                                                              
                                                                                
* SWITCH BACK TO SERVICE SYSTEM AT END OF PROCESSING                            
*                                                                               
SWITBACK NTR1                                                                   
         CLI   SWITCHED,0                                                       
         BE    SBACNO                                                           
         ICM   R0,15,FFILL                                                      
         GOTO1 SWITCH,DMCB,(X'01',(R0)),0                                       
         B     SBACYES             SWITCHED BACK OK                             
*                                                                               
SBACNO   B     NO                                                               
SBACYES  L     RF,AUTL             TURN OFF HAVE SWITCHED FLAG IN UTL           
         OI    TFLAG-UTLD(RF),X'FF'-TFLAGSSW                                    
         B     YES                                                              
                                                                                
* VALIDATE USER ID FIELD                                                        
*                                                                               
VALUID   NTR1                                                                   
         L     R7,AUTL                                                          
         USING UTLD,R7                                                          
VUID010  CLI   MODE,MODEPASQ       IGNORE INPUT IF =PASS                        
         BNE   VUID020                                                          
         CLI   MODEPC,C'Y'                                                      
         BE    VUID020                                                          
         TM    TTYPE,TTYPEWAP                                                   
         BO    VUID020                                                          
         LA    R3,SRVSRVH                                                       
         ST    R3,CURSOR                                                        
         B     VUID040                                                          
VUID020  LA    R3,SRVUIDH          R3=A(USER ID FIELD)                          
         ST    R3,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          EXTRACT LENGTH INPUT FIELD                   
         BZ    VUID030                                                          
         STC   R1,UIDLEN                                                        
         MVC   USERID,SPACES                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   USERID(0),8(R3)                                                  
         MVI   IOCONT,0                                                         
         GOTO1 READUIC,DMCB,AIO1,USERID                                         
         BNE   ERRUID                                                           
         XC    SRVUID,SRVUID                                                    
         MVC   SRVUID(L'USERID),USERID                                          
         OI    SRVUIDH+6,X'80'                                                  
         OI    INPFLAGS,INPFUIDQ                                                
         B     VUIDOK                                                           
*                                                                               
VUID030  TM    DDSFLAG,DDSNEW      TEST FIRST PASS                              
         BZ    INFUID                                                           
VUID040  OC    TUSER,TUSER                                                      
         BZ    VUIDNO                                                           
         MVC   USERNUM,TUSER                                                    
         MVI   IOCONT,0                                                         
         GOTO1 READUIN,DMCB,AIO1,USERNUM                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    SRVUID,SRVUID                                                    
         MVC   SRVUID(L'USERID),USERID                                          
         OI    SRVUIDH+6,X'80'                                                  
         B     VUIDOK                                                           
*                                                                               
VUIDNO   B     NO                                                               
VUIDOK   B     YES                                                              
         DROP  R7                                                               
                                                                                
* VALIDATE TERMINAL COMPATIBLE WITH USER ID                                     
*                                                                               
VALTRM   NTR1                                                                   
         L     R7,AUTL                                                          
         USING UTLD,R7                                                          
         MVI   IOCONT,0                                                         
         GOTO1 READTRM,DMCB,AIO1                                                
         BNE   ERRTRM                                                           
*                                                                               
         L     R4,AIO1                                                          
         L     RF,APCTWASV         USE 10K WORK AREA FOR GETIDS                 
         GOTO1 AGETIDS,DMCB,(C'L',(R4)),(RF),(C'A',DATAMGR),USERID              
         TM    4(R1),X'01'         TEST GETIDS PARAMETER ERROR                  
         BNZ   ERRTRM                                                           
         TM    12(R1),X'01'        EXACT MATCH?                                 
         BO    VTRMOK              YES                                          
         TM    12(R1),X'02'        MATCH WITH "ALL"?                            
         BZ    VTRM010             NO                                           
         TM    TSTAT,TSTATDDS+TSTATWEB   ALL ONLY VALID DDS/WEB TERM            
         BNZ   VTRMOK              YES                                          
*                                                                               
VTRM010  LA    R1,CTTDATA-CTTREC(R4)                                            
         SR    R0,R0               FIND PRINCIPAL ID ELEMENT                    
VTRM020  CLI   0(R1),0             TEST E-O-R                                   
         BE    ERRTRM                                                           
         CLI   0(R1),X'1F'         PRINCIPAL ID                                 
         BE    *+14                                                             
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VTRM020                                                          
         CLC   USERID,CTPID-CTPIDD(R1)                                          
         BE    VTRMOK                                                           
*                                                                               
         L     R2,AIO2             READ PRINCIPAL ID RECORD                     
         LA    R5,KEY                                                           
         USING CTIREC,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,CTPID-CTPIDD(R1)                                          
         GOTO1 DATAMGR,DMCB,(IOCONT,DMREAD),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BNE   ERRTRM                                                           
*                                                                               
         L     RF,APCTWASV         USE 10K WORK AREA FOR GETIDS                 
         GOTO1 AGETIDS,DMCB,(C'L',(R2)),(RF),(C'A',DATAMGR),USERID              
         TM    4(R1),X'01'         TEST GETIDS PARAMETER ERROR                  
         BNZ   ERRTRM                                                           
         TM    12(R1),X'01'        EXACT MATCH?                                 
         BO    VTRMOK              YES                                          
         B     ERRTRM                                                           
*                                                                               
VTRMOK   B     YES                                                              
         DROP  R5,R7                                                            
                                                                                
* VALIDATE AGENCY ACCESS CONTROL                                                
*                                                                               
VALAGY   NTR1                                                                   
         MVC   HALF,AGYALP        SET AGENCY CODE                               
         MVI   AGYFLAG,0                                                        
         CLI   MODE,MODEPASQ                                                    
         BNE   VALAGY1                                                          
         TM    DDSFLAG,DDSTRM                                                   
         BZ    VALAGY1                                                          
         CLI   TRMSYS,0           TEST IF CONNECTED WITH DDS SEC AGY            
         BE    VALAGY1                                                          
         OC    TRMAGYPE,TRMAGYPE                                                
         BZ    VALAGY1                                                          
         MVC   HALF,TRMAGYPE      SET DDS TERMINAL AGENCY FOR PERSON            
         MVC   AGYSECP,HALF                                                     
         OI    AGYFLAG,X'80'                                                    
VALAGY1  MVI   IOCONT,0                                                         
         GOTO1 READAGY,DMCB,AIO1,HALF                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    AGYPFLG,CTAADPRQ   PERSON ID REQUIRED WITH PASSWORD              
         BO    VAGYOK                                                           
         CLI   MODE,MODEPASQ                                                    
         BE    ERRUIDPW                                                         
         B     ERRUID                                                           
*                                                                               
VAGYNO   B     NO                                                               
VAGYOK   B     YES                                                              
                                                                                
* VALIDATE PERSONAL ID FIELD                                                    
*                                                                               
VALPID   NTR1                                                                   
         L     R7,AUTL                                                          
         USING UTLD,R7                                                          
         LA    R3,SRVPIDH          R3=A(PERSONAL ID FIELD)                      
         ST    R3,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          EXTRACT LENGTH INPUT FIELD                   
         BZ    VPID010                                                          
         STC   R1,PIDLEN                                                        
         MVC   PERSONID,SPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERSONID(0),8(R3)                                                
         B     VPID100                                                          
*                                                                               
VPID010  TM    DDSFLAG,DDSNEW      TEST FIRST PASS                              
         BZ    INFPID                                                           
         OC    TPERSON,TPERSON                                                  
         BZ    VPIDNO                                                           
         MVC   PIDNUM,TPERSON                                                   
         MVI   IOCONT,0                                                         
         GOTO1 READPNO,DMCB,AIO3,PIDNUM                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         USING SAPALD,R3                                                        
         LA    R3,ELEMENT          UPDATE EFFECTIVE DATES ELEMENT               
         XC    ELEMENT,ELEMENT                                                  
         MVI   SAPALEL,SAPALELQ                                                 
         GOTO1 GETELS                                                           
         ICM   R3,15,AELEM                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   PERSONID,SAPALPID                                                
         DROP  R3                                                               
         XC    SRVPID,SRVPID                                                    
         MVC   SRVPID(L'PERSONID),PERSONID                                      
         OI    SRVPIDH+6,X'80'                                                  
         B     VPID100                                                          
*                                                                               
VPID100  MVI   IOCONT,IORUPD                                                    
         GOTO1 READPID,DMCB,AIO1,PERSONID                                       
         BNE   ERRPID                                                           
         CLI   PWDCNTL,SAPERPAQ                                                 
         BE    ERRPID                                                           
*NOP*    MVC   TPERSON,PIDNUM                                                   
         OI    INPFLAGS,INPFPIDQ                                                
         B     VPIDOK                                                           
*                                                                               
VPIDNO   B     NO                                                               
VPIDOK   B     YES                                                              
         DROP  R7                                                               
                                                                                
* VALIDATE OLD PASSWORD FIELD                                                   
*                                                                               
VALOPWD  NTR1                                                                   
         LA    R3,SRVPWDH          R3=A(OLD PASSWORD FIELD)                     
         ST    R3,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          EXTRACT LENGTH INPUT FIELD                   
         BZ    VOPW100                                                          
         STC   R1,OPWDLEN                                                       
         MVC   OPWDINPT,SPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   OPWDINPT(0),8(R3)                                                
         MVC   OPWDUPPR,OPWDINPT   COPY INPUT PASSWORD                          
*                                                                               
         XC    DMCB(20),DMCB       CONVERT INPUT COPY TO UPPER CASE             
         LA    RE,OPWDUPPR                                                      
         ST    RE,DMCB                                                          
         MVC   DMCB+0(1),OPWDLEN                                                
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
         XC    0(10,RE),0(RE)                                                   
         MVI   DMCB+4,0            RULE#0 TO CONVERT TO UPPER                   
         MVC   DMCB+8(4),ASSB                                                   
         MVI   DMCB+8,2            PROCESS ACTION                               
         GOTO1 =V(PWDVAL),DMCB,RR=RELO                                          
*                                                                               
         MVC   PIDPWDU,PIDPWD      COPY PID PASSWORD                            
         LA    R1,PIDPWDU+9                                                     
         LA    R0,10                                                            
         CLI   0(R1),C' '          FIND LENGTH OF PID PASSWORD COPY             
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         XC    DMCB(20),DMCB       CONVERT PID PASSWORD COPY TO UPPER           
         LA    RE,PIDPWDU                                                       
         ST    RE,DMCB                                                          
         STC   R0,DMCB+0           R0=L'PID PASSWORD COPY                       
         LA    RE,WORK                                                          
         ST    RE,DMCB+4                                                        
         XC    0(10,RE),0(RE)                                                   
         MVI   DMCB+4,0            RULE#0 TO CONVERT TO UPPER                   
         MVC   DMCB+8(4),ASSB                                                   
         MVI   DMCB+8,2            PROCESS ACTION                               
         GOTO1 =V(PWDVAL),DMCB,RR=RELO                                          
*                                                                               
VOPW005  LA    R0,1                DEBUG                                        
         CLC   OPWDINPT,PIDPWD     TEST IF MIXED CASE PASSWORDS MATCH           
         BE    VOPW008                                                          
         CLC   OPWDUPPR,PIDPWDU    TEST IF UPPER CASE PASSWORDS MATCH           
         BE    VOPW008                                                          
         CLI   MODEPC,C'Y'         FROM =PC?                                    
         BE    VOPW008             YES: MAY BE FORGOTTEN PASSWORD               
         B     ERRPWD                                                           
*                                                                               
VOPW008  MVI   IOCONT,IORUPD                                                    
         GOTO1 READPNO,DMCB,AIO3,PIDNUM                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO3                                                          
         USING SA0REC,R2                                                        
         LA    R4,SA0DATA                                                       
         USING SAPWHD,R4                                                        
LATEST   USING SAPWHD,R3                                                        
         XR    R3,R3                                                            
         XR    RF,RF                                                            
*                                                                               
VOPW010  CLI   SAPWHEL,0           RECORD END                                   
         BE    VOPW030                                                          
         CLI   SAPWHEL,SAPWHELQ                                                 
         BNE   VOPW020                                                          
         CLC   SAPWHDTE,TODAY      EFFECTIVE AFTER TODAY?                       
         BH    VOPW020             YES                                          
         LTR   R3,R3               FIRST TIME?                                  
         BNZ   *+6                 NO                                           
         LR    R3,R4                                                            
         CLC   LATEST.SAPWHDTE,SAPWHDTE                                         
         BH    VOPW020                                                          
         BL    *+14                                                             
         CLC   LATEST.SAPWHTME,SAPWHTME                                         
         BH    *+6                                                              
         LR    R3,R4               R3=MOST RECENT PASSWORD                      
*                                                                               
VOPW020  IC    RF,SAPWHLN                                                       
         AR    R4,RF                                                            
         B     VOPW010                                                          
         DROP  LATEST                                                           
*                                                                               
VOPW030  LTR   R4,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                  NO PASSWORD YET SET UP                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,SAPWHDTE),(0,DUB)                                 
         XR    RF,RF                                                            
         IC    RF,AGYPTOU                                                       
         GOTO1 ADDAY,DMCB,(C'D',DUB),DUB1,(RF)                                  
         GOTO1 DATCON,DMCB,(0,DUB1),(2,DUB)                                     
         CLC   TODAY,DUB                                                        
         BL    *+8                                                              
         MVI   BYTE,C'Y'           SET PASSWORD HAS EXPIRED                     
*                                                                               
VOPW040  XR    RF,RF               RF=LENGTH OF PASSWORD IN ELEMENT             
         IC    RF,SAPWHLN                                                       
         SHI   RF,SAPWHLNQ                                                      
         LA    R0,2                DEBUG                                        
         CLM   RF,1,OPWDLEN                                                     
         BNE   ERRPWD              ERROR IN LENGTHS DONT MATCH                  
         BCTR  RF,0                                                             
         LA    R0,3                DEBUG                                        
         EX    RF,VOPW041                                                       
         BE    VOPW050             OK IF MATCH ON MIXED CASE                    
         EX    RF,VOPW042                                                       
         BE    VOPW050             OK IF MATCH ON UPPER CASE                    
         B     ERRPWD                                                           
VOPW041  CLC   OPWDINPT(0),SAPWHPWD                                             
VOPW042  CLC   OPWDUPPR(0),SAPWHPWD                                             
*                                                                               
VOPW050  MVC   OPWDNUM,PIDNUM                                                   
         OI    INPFLAGS,INPFOPWQ                                                
         B     VOPWOK                                                           
*                                                                               
VOPW100  CLI   MODE,MODEPASQ       OLD PASSWORD NOT INPUT                       
         BNE   VOPW110                                                          
         B     INFPWD                                                           
VOPW110  TM    DDSFLAG,DDSNEW      TEST FIRST PASS                              
         BZ    INFPWD                                                           
         B     VOPWNO                                                           
*                                                                               
VOPWNO   B     NO                                                               
VOPWOK   B     YES                                                              
         DROP  R2,R4                                                            
                                                                                
* VALIDATE NEW PASSWORD FIELD                                                   
*                                                                               
VALNPWD  NTR1                                                                   
         CLI   MODEPC,C'Y'                                                      
         BE    VNPW010                                                          
         TM    DDSFLAG,DDSNEW                                                   
         BZ    VNPW010                                                          
         XC    SRVPWDN,SRVPWDN                                                  
         MVI   SRVPWDNH+5,0                                                     
         OI    SRVPWDNH+6,X'80'                                                 
         LA    RE,SAVEDSTR                                                      
         LA    RF,SAVEDL                                                        
         XCEF                                                                   
         B     VNPWOK                                                           
*                                                                               
VNPW010  CLC   OPWDUPPR,=CL10'DDS'                                              
         BE    VNPWNO                                                           
         XC    NPWDINPT,NPWDINPT                                                
         LA    R3,SRVPWDNH         R3=A(NEW PASSWORD FIELD)                     
         ST    R3,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          EXTRACT LENGTH INPUT FIELD                   
         BZ    VNPW100                                                          
         CHI   R1,10                                                            
         BH    ERRNPLG                                                          
         OC    AGYPMIN,AGYPMIN                                                  
         BZ    VNPW020                                                          
         CLM   R1,1,AGYPMIN                                                     
         BL    ERRNPST                                                          
*                                                                               
VNPW020  STC   R1,NPWDLEN          SET LENGTH OF NEW PASSWORD                   
         MVC   NPWDINPT,SPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NPWDINPT(0),8(R3)                                                
         MVC   NPWDUPPR,NPWDINPT   COPY NEW INPUT PASSWORD                      
*                                                                               
         XC    DMCB(20),DMCB       VALIDATE NEW INPUT PASSWORD                  
         LA    RE,NPWDINPT                                                      
         ST    RE,DMCB                                                          
         MVC   DMCB+0(1),NPWDLEN                                                
         LA    RE,WORK             TEXT AREA TO RETURN UPPER CASE VALUE         
         ST    RE,DMCB+4                                                        
         XC    0(10,RE),0(RE)                                                   
         MVC   DMCB+4(1),AGYPPVR   SET PASSWORD RULE NUMBER                     
         MVC   DMCB+8(4),ASSB                                                   
         MVI   DMCB+8,3            VALIDATE PASSWORD ON 1ST PASS                
         OC    SVNPWD,SVNPWD                                                    
         BZ    *+8                                                              
         MVI   DMCB+8,2            PROCESS PASSWORD ON 2ND PASS                 
         GOTO1 =V(PWDVAL),DMCB,RR=RELO                                          
         MVC   NPWDUPPR,WORK       SAVE UPPER CASE VERSION OF PASSWORD          
         SR    R2,R2                                                            
         ICM   R2,1,8(R1)          ERROR RETURN E2-EF=226-239                   
         BZ    VNPW030                                                          
*&&UK*&& B     ERRNPWD             USE ERR 306 - PASSWORD IS INVALID            
*&&UK*&& B     VNPW022             USE OPTION TO REVEAL PASSWORD RULE           
*&&US*&& B     VNPW022             USE OPTION TO REVEAL PASSWORD RULE           
*&&US*&& B     ERRNPWD             USE ERR 306 - PASSWORD IS INVALID            
*                                                                               
VNPW021  AHI   R2,116              ERROR MESSAGES SRV=342-355                   
         B     ERRNPWDV                                                         
*                                                                               
VNPW022  CHI   R2,231              ERROR RETURN E2-E7=226-231                   
         BH    *+12                                                             
         AHI   R2,116              ERROR MESSAGES SRV=342-347                   
         B     ERRNPWDV                                                         
         ICM   R2,1,AGYPPVR        PASSWORD RULE  NUM 001-012                   
         BNZ   *+12                                                             
         ICM   R2,1,8(R1)          IF NO RULE USE ORIGINAL ERROR RETURN         
         B     VNPW021                                                          
         AHI   R2,360              RULE MESSAGES  SRV 361-372                   
         B     ERRNPWDV                                                         
*                                                                               
VNPW030  OC    SVNPWD,SVNPWD                                                    
         BNZ   VNPW040                                                          
         L     R4,AIO3                                                          
         USING SA0REC,R4                                                        
         LA    R4,SA0DATA                                                       
         USING SAPWHD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
VNPW032  CLI   SAPWHEL,0           OK IF END OF RECORD AND NO MATCHES           
         BE    VNPW036                                                          
         CLI   SAPWHEL,SAPWHELQ                                                 
         BNE   VNPW034                                                          
         XR    RF,RF               RF=LENGTH OF PASSWORD IN ELEMENT             
         IC    RF,SAPWHLN                                                       
         SHI   RF,SAPWHLNQ                                                      
         LA    R0,4                DEBUG                                        
         CLM   RF,1,NPWDLEN                                                     
         BNE   VNPW034             OK IF LENGTHS DONT MATCH                     
         BCTR  RF,0                                                             
         LA    R0,5                DEBUG                                        
         EX    RF,VNPW032A                                                      
         BE    ERRNPPV             ERROR IF MATCH ON MIXED CASE                 
         EX    RF,VNPW032B                                                      
         BE    ERRNPPV             ERROR IF MATCH ON UPPER CASE                 
         B     VNPW034                                                          
VNPW032A CLC   NPWDINPT(0),SAPWHPWD                                             
VNPW032B CLC   NPWDUPPR(0),SAPWHPWD                                             
*                                                                               
VNPW034  IC    RF,SAPWHLN                                                       
         AR    R4,RF                                                            
         B     VNPW032                                                          
         DROP  R4                                                               
*                                                                               
VNPW036  EQU   *                                                                
         MVC   SVNPWD,NPWDINPT                                                  
         MVC   SVLUID,USERNUM                                                   
         MVC   SVLPID,PIDNUM                                                    
         CLI   MODEPC,C'Y'                                                      
         BE    VNPW050                                                          
         BAS   RE,WRITESTR                                                      
         XC    SRVPWDN,SRVPWDN                                                  
         MVI   SRVPWDNH+5,0                                                     
         OI    SRVPWDNH+6,X'80'                                                 
         B     INFRPWD                                                          
*                                                                               
VNPW040  EQU   *                                                                
         CLC   SVLUID,USERNUM                                                   
         BNE   VNPWER1                                                          
         CLC   SVLPID,PIDNUM                                                    
         BNE   VNPWER1                                                          
         CLC   SVNPWD,NPWDINPT                                                  
         BE    VNPW050                                                          
         B     ERRNSME                                                          
*                                                                               
VNPW050  EQU   *                                                                
         BAS   RE,WRITESTR                                                      
         OI    INPFLAGS,INPFNPWQ                                                
         B     VNPWOK                                                           
*                                                                               
VNPW100  CLI   MODE,MODEPASQ       NEW PASSWORD NOT INPUT                       
         BNE   VNPWOK                                                           
         B     INFNPWD                                                          
*                                                                               
VNPWER1  LA    R0,6                DEBUG                                        
         LA    R1,SRVPWDH                                                       
         ST    R1,CURSOR                                                        
         B     ERRPWD                                                           
*                                                                               
VNPWNO   B     NO                                                               
VNPWOK   B     YES                                                              
                                                                                
* VALIDATE CONNECT SYSTEM FIELD                                                 
*                                                                               
VALCSYS  NTR1                                                                   
         LA    R3,SRVSYSH          R3=A(SYSTEM FIELD)                           
         ST    R3,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          EXTRACT LENGTH INPUT FIELD                   
         BZ    VCSY010                                                          
         STC   R1,CSYSLEN                                                       
         MVC   CSYSDATA,SPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CSYSDATA(0),8(R3)                                                
         OI    INPFLAGS,INPFSYSQ                                                
         B     VCSYOK                                                           
*                                                                               
VCSY010  TM    DDSFLAG,DDSNEW      TEST FIRST PASS                              
         BZ    VCSYOK                                                           
         B     VCSYOK                                                           
*                                                                               
VCSYNO   B     NO                                                               
VCSYOK   B     YES                                                              
                                                                                
* VALIDATE CONNECT PROGRAM FIELD                                                
*                                                                               
VALCPRG  NTR1                                                                   
         LA    R3,SRVPRGH          R3=A(PROGRAM FIELD)                          
         ST    R3,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          EXTRACT LENGTH INPUT FIELD                   
         BZ    VCPR010                                                          
         STC   R1,CPRGLEN                                                       
         MVC   CPRGDATA,SPACES                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CPRGDATA(0),8(R3)                                                
         OI    INPFLAGS,INPFPRGQ                                                
         B     VCPROK                                                           
*                                                                               
VCPR010  TM    DDSFLAG,DDSNEW      TEST FIRST PASS                              
         BZ    VCPROK                                                           
         B     VCPROK                                                           
*                                                                               
VCPRNO   B     NO                                                               
VCPROK   B     YES                                                              
                                                                                
* VALIDATE CONNECT DATA FIELD                                                   
*                                                                               
VALCDATA NTR1                                                                   
         LA    R3,SRVDATAH         R3=A(CONNECT DATA FIELD)                     
         ST    R3,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R3)          EXTRACT LENGTH INPUT FIELD                   
         BZ    VCDA010                                                          
         STC   R1,CONDLEN                                                       
         MVC   CONDATA,SPACES                                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CONDATA(0),8(R3)                                                 
         OI    INPFLAGS,INPFCDAQ                                                
         B     VCDAOK                                                           
*                                                                               
VCDA010  TM    DDSFLAG,DDSNEW      TEST FIRST PASS                              
         BZ    VCDAOK                                                           
         B     VCDAOK                                                           
*                                                                               
VCDANO   B     NO                                                               
VCDAOK   B     YES                                                              
                                                                                
* VALIDATE SERVICE REQUEST FIELD                                                
*                                                                               
VALSRV   NTR1                                                                   
         L     R7,AUTL                                                          
         USING UTLD,R7                                                          
         OI    SRVUIDH+6,X'80'                                                  
         NI    SRVUIDH+1,X'FF'-X'0C'                                            
         LA    R3,SRVSRVH          R3=A(SERVICE REQUEST FIELD)                  
         ZIC   R4,5(R3)            EXTRACT LENGTH INPUT FIELD                   
         LA    R3,8(R3)            R3=A(DATA)                                   
         CLC   1(4,R3),=C'PASS'                                                 
         BE    VSRV010                                                          
         CLC   1(5,R3),=C'LOGON'                                                
         BE    VSRV020                                                          
         CLC   1(6,R3),=C'LOGOFF'                                               
         BE    VSRV030                                                          
         DC    H'0'                                                             
*                                                                               
VSRV010  CLC   1(6,R3),=C'PASSPC'  TEST SPECIAL PC VERSION                      
         BNE   *+12                                                             
         MVI   MODEPC,C'Y'                                                      
         B     VSRV015                                                          
         TM    TTYPE,TTYPEWAP                                                   
         BO    VSRV015                                                          
         OI    SRVUIHH+6,X'80'     SET USERID FIELDS INVISIBLE                  
         OI    SRVUIHH+1,X'0C'                                                  
         OI    SRVUIDH+6,X'80'                                                  
         OI    SRVUIDH+1,X'0C'                                                  
         CLI   TSYS,0              MUST BE LOGGED ON TO CHANGE PASSWORD         
         BE    ERRMBCPW                                                         
VSRV015  OI    SRVLINEH+6,X'80'    MAKE BOTTOM HALF SCREEN INVISIBLE            
         OI    SRVLINEH+1,X'04'                                                 
         OI    SRVSYHH+6,X'80'                                                  
         OI    SRVSYHH+1,X'0C'                                                  
         OI    SRVSYSH+6,X'80'                                                  
         OI    SRVSYSH+1,X'20'                                                  
         OI    SRVPRHH+6,X'80'                                                  
         OI    SRVPRHH+1,X'0C'                                                  
         OI    SRVPRGH+6,X'80'                                                  
         OI    SRVPRGH+1,X'20'                                                  
         OI    SRVDAHH+6,X'80'                                                  
         OI    SRVDAHH+1,X'0C'                                                  
         OI    SRVDATAH+6,X'80'                                                 
         OI    SRVDATAH+1,X'20'                                                 
         MVI   MODE,MODEPASQ                                                    
         B     VSRVOK                                                           
*                                                                               
VSRV020  CLI   TSYS,0              LOGON WAS INPUT                              
         BNE   VSRV100                                                          
         OI    SRVLINEH+6,X'80'                                                 
         NI    SRVLINEH+1,X'FF'-X'04'                                           
         OI    SRVSYHH+6,X'80'                                                  
         NI    SRVSYHH+1,X'FF'-X'0C'                                            
         OI    SRVSYSH+6,X'80'                                                  
         NI    SRVSYSH+1,X'FF'-X'20'                                            
         OI    SRVPRHH+6,X'80'                                                  
         NI    SRVPRHH+1,X'FF'-X'0C'                                            
         OI    SRVPRGH+6,X'80'                                                  
         NI    SRVPRGH+1,X'FF'-X'20'                                            
         OI    SRVDAHH+6,X'80'                                                  
         NI    SRVDAHH+1,X'FF'-X'0C'                                            
         OI    SRVDATAH+6,X'80'                                                 
         NI    SRVDATAH+1,X'FF'-X'20'                                           
         MVI   MODE,MODELONQ                                                    
         B     VSRVOK                                                           
*                                                                               
VSRV030  MVI   MODE,MODELOFQ       LOGOFF WAS INPUT                             
         B     VSRV100                                                          
*                                                                               
VSRV100  MVC   USERNUM,TUSER       SAVE USERID AND PERSON ID                    
         MVC   PIDNUM,TPERSON                                                   
         BAS   RE,DISCON           CALL DISCONNECT PROCESS                      
         BNE   VSRVNO                                                           
         MVC   TUSER,USERNUM       RESTORE USERID AND PERSON ID                 
         MVC   TPERSON,PIDNUM                                                   
         XC    SRVSRV,SRVSRV                                                    
         MVC   SRVSRV(6),=C'=LOGON'                                             
         OI    SRVSRVH+6,X'80'                                                  
         LA    R1,SRVUIDH                                                       
         ST    R1,CURSOR                                                        
         TWAXC SRVUIDH                                                          
         B     INFUID                                                           
*                                                                               
VSRVNO   B     NO                                                               
VSRVOK   B     YES                                                              
         DROP  R7                                                               
                                                                                
* UPDATE OLD PERSON RECORD                                                      
*                                                                               
UPPERSON NTR1                                                                   
         USING SAPEREC,R2                                                       
         L     R2,AIO1                                                          
UPPN10   EQU   *                                                                
*NOP*    GOTO1 SETACT,SAPEREC      SET ACTIVITY EL IN PERSON REC                
         LA    R3,ELEMENT                                                       
         USING SAPWDD,R3                                                        
         MVI   SAPWDEL,SAPWDELQ    BUILD PASSWORD POINTER ELEMENT               
         MVI   SAPWDLN,0                                                        
         GOTO1 DELELS,SAPEREC                                                   
         MVI   SAPWDLN,SAPWDLNQ                                                 
         MVC   SAPWDNUM,OPWDNUM                                                 
         MVC   SAPWDCOD,NPWDINPT                                                
         GOTO1 ADDELS,SAPEREC                                                   
         DROP  R3                                                               
*                                  UPDATE PERSON RECORD                         
         GOTO1 WRITPID,DMCB,AIO1                                                
*                                                                               
         CLC   SAPEDEF,TODAYC      FUTURE EFF DATE?                             
         BNL   UPEROK              NO - EXIT                                    
         SR    RE,RE                                                            
         ICM   RE,3,SAPEDEF                                                     
         AHI   RE,1                                                             
         STCM  RE,3,SAPEDEF                                                     
*                                                                               
         MVC   KEY,0(R2)                                                        
         GOTO1 DATAMGR,DMCB,('IORUPD',DMRDHI),CTFILE,KEY,(R2),0                 
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BE    *+14                                                             
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   KEY(SAPEDEF-SAPEKEY),0(R2)                                       
         BNE   UPEROK              EXIT IF NOT THE SAME PERSON                  
         B     UPPN10              CHG PASSWORD FOR THIS PERSON                 
*                                                                               
UPERNO   B     NO                                                               
UPEROK   B     YES                                                              
         DROP  R2                                                               
                                                                                
* UPDATE PASSWORD RECORDS                                                       
*                                                                               
UPPASSWD NTR1                                                                   
         USING SA0REC,R2                                                        
         L     R2,AIO3                                                          
         GOTO1 SETACT,SA0REC       SET ACTIVITY EL IN PASSWORD REC              
*                                                                               
UPWD010  LA    R4,SA0DATA                                                       
         USING SAPWHD,R4                                                        
         XR    RF,RF                                                            
         XR    R3,R3               R3=A(OLDEST PASSWORD)                        
OLDEST   USING SAPWHD,R3                                                        
         XR    R5,R5               R5=COUNT OF PASSWORDS ON RECORD              
         XR    RF,RF                                                            
*                                                                               
UPWD020  CLI   SAPWHEL,0           EOR?                                         
         BE    UPWD024             YES - SEE IF NEED TO LOSE AN ELEMENT         
         CLI   SAPWHEL,SAPWHELQ    PASSWORD ELEMENT?                            
         BNE   UPWD022             NO                                           
*                                                                               
         LTR   R3,R3               R3=A(OLDEST PASSWORD)                        
         BNZ   *+6                                                              
         LR    R3,R4                                                            
         CLC   OLDEST.SAPWHDTE,SAPWHDTE                                         
         BL    UPWD021                                                          
         BH    *+14                                                             
         CLC   OLDEST.SAPWHTME,SAPWHTME                                         
         BL    *+6                                                              
         LR    R3,R4               OLDER PASSWORD FOUND                         
*                                                                               
UPWD021  LA    R5,1(R5)            BUMP COUNT                                   
*                                                                               
UPWD022  IC    RF,SAPWHLN          NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     UPWD020                                                          
*                                                                               
UPWD024  CLM   R5,1,AGYPRUS        ELEMENT COUNT EXCEEDS REUSE NUMBER           
         BNH   UPWD030             NO                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(L'SAPWHDTE+1+L'SAPWHTME),OLDEST.SAPWHDTE                 
*                                                                               
         GOTO1 HELLO,DMCB,(C'D',CTFILE),('SAPWHELQ',SA0REC),           *        
               (L'SAPWHDTE+1+L'SAPWHTME,ELEMENT)                                
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     UPWD010             RETRY LOOP                                   
         DROP  OLDEST                                                           
*                                  ADD/UPDATE OLD STYLE PASSWORD                
UPWD030  EQU   *                   POINTER ELEMENT                              
         LA    R4,SA0DATA                                                       
         USING SAPASD,R4                                                        
         XR    RF,RF                                                            
*                                                                               
UPWD032  CLI   SAPASEL,0           EOR?                                         
         BE    UPWD034             YES - JUST ADD ELEMENT                       
         CLI   SAPASEL,SAPASELQ    OLD PASSWORD POINTER ELEMENT?                
         BE    *+14                                                             
         IC    RF,SAPASLEN         NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     UPWD032                                                          
         MVC   SAPASDTA(10),NPWDINPT                                            
         B     UPWD036                                                          
*                                                                               
UPWD034  EQU   *                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         MVI   SAPASEL,SAPASELQ                                                 
         MVI   SAPASLEN,X'0C'                                                   
         MVC   SAPASDTA(10),NPWDINPT                                            
         GOTO1 ADDELS,SA0REC                                                    
*                                                                               
UPWD036  EQU   *                                                                
         XC    ELEMENT,ELEMENT                                                  
         LA    R4,ELEMENT                                                       
         USING SAPWHD,R4                                                        
         MVI   SAPWHEL,SAPWHELQ                                                 
         MVC   SAPWHDTE,=X'FFFF'    SET DATE SO THIS ELEMENT GOES LAST          
         MVI   SAPWHFLG,0                                                       
         TIME  BIN                                                              
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  R0,7,SAPWHTME                                                    
         XR    RE,RE                                                            
         IC    RE,NPWDLEN                                                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAPWHPWD(0),NPWDINPT                                             
         AHI   RE,SAPWHLNQ+1                                                    
         STC   RE,SAPWHLN                                                       
         GOTO1 ADDELS,SA0REC                                                    
         L     R4,DMCB+16          POINT TO NEW ELEMENT                         
         MVI   SAPWHFLG,0          SET CHANGED BY =PASS                         
         MVC   SAPWHDTE,TODAY      SET CORRECT DATE                             
*                                                                               
         GOTO1 WRITPWD,DMCB,AIO3                                                
*                                                                               
         B     UPWDOK                                                           
*                                                                               
UPWDNO   B     NO                                                               
UPWDOK   B     YES                                                              
         DROP  R2,R4                                                            
                                                                                
* CONNECT PROCEDURE:                                                            
* PHYSICAL CONNECT. CHECKS CORRECT DATA ITEMS INPUT                             
* CALLS OVERLAYS AND SETS SYSTEM DATA FOR =CT CONNECT CALL                      
* EXIT TO FACPAK MONITOR IF SUCCESFUL CONNECT,                                  
* ELSE SYSTEM DATA AND PC TWA RESTORED AND NORMAL ERROR EXIT                    
*                                                                               
CONNECT  NTR1                                                                   
         L     RE,ATWA             SAVE PC SCREEN TWA IN WORKING STORE          
         L     R0,APCTWASV                                                      
         LA    R1,SRVLAST-SRPASFFD                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,ATWA             LOAD CONNECT BASE SCREEN                     
         LA    R6,64(R6)                                                        
         GOTO1 CALLOV,DMCB,(R6),X'D90110FF'                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 LOADFLD,DMCB,=C'=CT',3,1                                         
*                                                                               
CONN010  LA    R2,WORK                                                          
         LA    RE,USERID                                                        
         SR    R1,R1                                                            
         ICM   R1,1,UIDLEN                                                      
         BZ    CONN020                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)                                                    
         LA    R2,1(R1,R2)                                                      
         SR    R1,R1                                                            
         ICM   R1,1,PIDLEN                                                      
         BZ    CONN016                                                          
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         LA    RE,PERSONID                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)                                                    
         LA    R2,1(R1,R2)                                                      
*                                                                               
CONN016  LR    R3,R2               MOVE DATA TO USER ID TWA FIELD               
         LA    R2,WORK                                                          
         SR    R3,R2                                                            
         GOTO1 LOADFLD,DMCB,(R2),(R3),2                                         
*                                                                               
CONN020  LA    R2,CSYSDATA         MOVE DATA TO SYSTEM TWA FIELD                
         SR    R3,R3                                                            
         ICM   R3,1,CSYSLEN                                                     
         GOTO1 LOADFLD,DMCB,(R2),(R3),3                                         
*                                                                               
CONN030  LA    R2,CPRGDATA         MOVE DATA TO PROGRAM TWA FIELD               
         SR    R3,R3                                                            
         ICM   R3,1,CPRGLEN                                                     
         GOTO1 LOADFLD,DMCB,(R2),(R3),4                                         
*                                                                               
CONN040  OC    NPWDINPT,NPWDINPT                                                
         BZ    CONN042                                                          
         LA    R2,NPWDINPT                                                      
         SR    R3,R3                                                            
         ICM   R3,1,NPWDLEN                                                     
         B     CONN044                                                          
CONN042  LA    R2,OPWDINPT                                                      
         SR    R3,R3                                                            
         ICM   R3,1,OPWDLEN                                                     
CONN044  GOTO1 LOADFLD,DMCB,(R2),(R3),5                                         
*                                                                               
CONN050  LA    R2,CONDATA          MOVE DATA TO CONNECT DATA TWA FIELD          
         SR    R3,R3                                                            
         ICM   R3,1,CONDLEN                                                     
         GOTO1 LOADFLD,DMCB,(R2),(R3),6                                         
*                                                                               
CONN100  MVC   DMCB+4(4),=X'D9011000'                                           
         MVC   DMCB(4),CSNXTLOD    IN CASE NOT CORE RESIDENT ??                 
         GOTO1 CALLOV,DMCB,,,0                                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R4,0(R1)            GET CODE ENTRY POINT                         
                                                                                
* SET SYSTEM STATUS TABLE VALUES AS IF                                          
* FOR ENTRY DIRECT FROM FACPAK MONITOR AND GOTO CONNECT ROUTINE                 
*                                                                               
         BAS   RE,SWITBACK                                                      
         L     R2,AUTL             ADJUST UTL VALUES                            
         USING UTLD,R2                                                          
         L     R3,ATCB             ADJUST TCB VALUES                            
         USING TCBD,R3                                                          
         MVI   TSVCREQ,X'01'       INDICATE CT SERVICE REQUEST STATE            
         MVI   TSVCREQ+1,$CT                                                    
*                                                                               
         NI    TFLAG,255-TFLAGSSW-TFLAGNOP-TFLAGRCV                             
*                                                                               
         MVI   TCBPRG,$CT                                                       
*                                  INDICATE CONNECTED VIA =LOGON IN UTL         
         OI    TSTAT6,TST6LOGN                                                  
         DROP  R2,R3                                                            
*                                                                               
         LR    RF,R4               GET CODE ENTRY POINT                         
*                                                                               
         L     R1,APARMS           POINT TO ENTRY PARAMETERS                    
         BASR  RE,RF               GOTO CONNECT ROUTINE                         
*                                                                               
         B     XDIRECT             DIRECT EXIT                                  
*                                                                               
CONNNO   B     NO                                                               
CONNOK   B     YES                                                              
*                                                                               
$CT      EQU   X'10'               CONNECT PROGRAM #                            
                                                                                
* DISCONNECT PROCEDURE:                                                         
* PHYSICAL DISCONNECT. CHECKS CORRECT DATA ITEMS INPUT                          
* CALLS OVERLAYS AND SETS SYSTEM DATA FOR =CT CONNECT CALL                      
*                                                                               
DISCON   NTR1                                                                   
         L     RE,ATWA             SAVE PC SCREEN TWA IN WORKING STORE          
         L     R0,APCTWASV                                                      
         LA    R1,SRVLAST-SRPASFFD                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,ATWA             LOAD CONNECT BASE SCREEN                     
         LA    R6,64(R6)                                                        
         GOTO1 CALLOV,DMCB,(R6),X'D90110FF'                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 LOADFLD,DMCB,=C'=CT',3,1                                         
*                                                                               
         MVC   DMCB+4(4),=X'D9011000'                                           
         MVC   DMCB(4),CSNXTLOD    IN CASE NOT CORE RESIDENT ??                 
         GOTO1 CALLOV,DMCB,,,0                                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R4,0(R1)            GET CODE ENTRY POINT                         
                                                                                
* SET SYSTEM STATUS TABLE VALUES AS IF                                          
* FOR ENTRY DIRECT FROM FACPAK MONITOR AND GOTO CONNECT ROUTINE                 
*                                                                               
*NOP*    BAS   RE,SWITBACK                                                      
         L     R2,AUTL             ADJUST UTL VALUES                            
         USING UTLD,R2                                                          
         L     R3,ATCB             ADJUST TCB VALUES                            
         USING TCBD,R3                                                          
         MVC   TSRSAVE(2),TSVCREQ  SAVE CURRENT UTL PROGRAM STATE               
         MVI   TSVCREQ,X'01'       INDICATE CT SERVICE REQUEST STATE            
         MVI   TSVCREQ+1,$CT                                                    
         MVI   TSYS,0              FOR MONITOR                                  
         NI    TFLAG,X'FF'-TFLAGSSW                                             
         MVI   TCBPRG,$CT                                                       
         DROP  R2,R3                                                            
*                                                                               
         LR    RF,R4               GET CODE ENTRY POINT                         
         L     R1,APARMS           POINT TO ENTRY PARAMETERS                    
         BASR  RE,RF               GOTO CONNECT ROUTINE                         
         L     R2,AUTL             ADJUST UTL VALUES                            
         USING UTLD,R2                                                          
         L     R3,ATCB             ADJUST TCB VALUES                            
         USING TCBD,R3                                                          
         CLI   TSVCREQ,X'01'       TEST IF DISCONNECT SUCCESFUL                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TSYS,0              FOR MONITOR                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TSVCREQ(2),TSRSAVE  ELSE RESTORE UTL PROGRAM STATE               
         MVC   TCBPRG(1),TSVCREQ+1 AND RESTORE TCB PROGRAM STATE                
         DROP  R2,R3                                                            
*                                                                               
         L     R6,ATWA             RESTORE =PASS TWA SCREEN                     
         LA    R6,64(R6)           LOAD OVERLAY                                 
         GOTO1 CALLOV,DMCB,(R6),X'D9011EFF'                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R0,ATWA             MOVE SAVED TWA DATA FROM STORAGE             
         L     RE,APCTWASV                                                      
         LA    R1,SRVLAST-SRPASFFD                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     DISCOK              RETURN PCIACT ALREADY SET FOR ERROR          
*                                                                               
DISCNO   B     NO                                                               
DISCOK   B     YES                                                              
                                                                                
* RECONNECT PROCEDURE:                                                          
*                                                                               
RECON    NTR1                                                                   
         LA    RE,SAVEDSTR                                                      
         LA    RF,SAVEDL                                                        
         XCEF                                                                   
         BAS   RE,WRITESTR                                                      
         XC    SRVPWD,SRVPWD                                                    
         XC    SRVPWDN,SRVPWDN                                                  
         OI    SRVPWDH+6,X'80'                                                  
         OI    SRVPWDNH+6,X'80'                                                 
         LA    R1,SRVSRVH                                                       
         ST    R1,CURSOR                                                        
         XC    SRVSRV,SRVSRV                                                    
         MVC   SRVSRV(3),=C'=RE'                                                
         OI    SRVSRVH+6,X'80'                                                  
*                                                                               
         L     RF,AUTL             SET EXPIRY DAYS TO TIME OUT VALUE            
         MVC   TPASSEXP-UTLD(1,RF),AGYPTOU                                      
         NI    TSTAT7-UTLD(RF),255-(TST7PS0+TST7PS1+TST7PS2+TST7PSWN)           
         B     INFPWDOK                                                         
*                                                                               
RECONO   B     NO                                                               
RECOOK   B     YES                                                              
                                                                                
* SUBROUTINE TO INTERPRET ERROR MESSAGE IN TWA FIELD ONE                        
* SET VALUE OF PCIACT ON RETURN                                                 
*                                                                               
TRERROR  NTR1                                                                   
         LA    R3,SRVMSG                                                        
         MVC   CTSRVMSG,SRVMSG                                                  
         CLC   0(2,R3),=C'ED'      TEST FOR ERROR CODE IN FIELD                 
         BNE   TRERUND1            UNDEFINED ERROR CODE                         
*                                                                               
         LA    R2,4                READ 4 DIGIT CODE                            
         LA    R3,3(R3)                                                         
         GOTO1 TESTNUM,DMCB,(R3),(R2)                                           
         BNE   TRERUND1                                                         
*                                                                               
         BCTR  R2,0                CONVERT VALUE TO BINARY                      
         EX    R2,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R3)                                                      
         CVB   R2,DUB                                                           
*                                                                               
         LA    R3,TRERTAB          TRANSLATE VALUE FROM TABLE                   
TRERL1   CH    R2,0(R3)                                                         
         BE    TRERLX              FOUND                                        
         CLC   0(2,R3),=Y(0)                                                    
         BE    TRERUND2            NOT FOUND                                    
         LA    R3,L'TRERTAB(R3)                                                 
         B     TRERL1                                                           
*                                                                               
TRERLX   MVC   PCIACT,2(R3)        EXTRACT TRANSLATION                          
         B     TRERX                                                            
*                                                                               
TRERUND1 SR    R2,R2               SET DEFAULT VALUE                            
         B     TRERUND2                                                         
*                                                                               
TRERUND2 AH    R2,=Y(PIEERMSG) SET EXIT CODE                                    
         STH   R2,PCIACT                                                        
         B     TRERX                                                            
*                                                                               
TRERX    B     XIT                                                              
*                                                                               
TRERTAB  DS    0F                  ERROR CODE TRANSLATION TABLE                 
         DC    YL2(0031,PIEERMSG+0031)                                          
         DC    YL2(0032,PIEERMSG+0032)                                          
         DC    YL2(0033,PIEERMSG+0033)                                          
         DC    YL2(0)                                                           
                                                                                
PIEERMSG EQU   9100                                                             
                                                                                
                                                                                
* READ IN SAVED STORAGE                                                         
*                                                                               
READSTR  NTR1                                                                   
         L     R5,ATIA             TIA WORK AREA                                
         USING SRSD,R5                                                          
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),SRSD            
         CLI   8(R1),0             CHECK DMGR RETURN OK                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),=C'$PAS'  SEE IF GLOBAS SR SAVED                     
         BE    READ10                                                           
         LA    RE,SAVEDSTR         IF NOT INITIALISE AREA                       
         LA    RF,SAVEDL                                                        
         XCEF                                                                   
         B     READX                                                            
*                                                                               
READ10   LA    R0,SAVEDSTR         MOVE SAVED DATA FORM TIA TO WORKD            
         LA    RE,SRCOMWRK                                                      
         L     R1,=A(SAVEDL)                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    DDSFLAG,DDSNEW                                                   
         BZ    READX                                                            
         XC    SVNPWD,SVNPWD       CLEAR NEW PASSWORD CONTROLS                  
         XC    SVLUID,SVLUID                                                    
         XC    SVLPID,SVLPID                                                    
         BAS   RE,WRITESTR         WRITE BACK SAVED STORAGE                     
*                                                                               
READX    EQU   *                                                                
         XIT1                                                                   
         DROP  R5                                                               
                                                                                
* WRITE OUT SAVED STORAGE                                                       
*                                                                               
WRITESTR NTR1                                                                   
         L     R5,ATIA             TIA WORK AREA                                
         USING SRSD,R5                                                          
         MVC   IDENT,=C'$PAS'      MARK GLOBAL SR DATA                          
         LA    RE,SAVEDSTR                                                      
         LA    R0,SRCOMWRK                                                      
         L     R1,=A(SAVEDL)                                                    
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE SAVED DATA FROM WORKD TO TIA            
         LA    R2,SRPAGENO         WRITE DATA TO TWA#11                         
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(R2),SRSD                     
         CLI   8(R1),0             CHECK DMGR RETURN OK                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R5                                                               
                                                                                
* CALL GETHELP AND EXIT                                                         
*                                                                               
HELPOUT  NI    SRVPIDH+6,X'FF'-X'40'   UNSET THE CURSOR                         
         TWAXC SRVPIDH,PROT=Y                                                   
         LA    R1,HLPIND4                                                       
         CLI   HLPFLD,4            CHECK FOR SEL FIELDS                         
         BNL   HELP005                                                          
         SR    RF,RF                                                            
         IC    RF,HLPFLD           READ HELP FIELD                              
         SLL   RF,2                                                             
         EX    0,HLPIND0(RF)       SELECT WHICH TABLE                           
         B     HELP010                                                          
HLPIND0  DC    XL4'00'             NO FIELD NUMBER                              
         LA    R1,HLPIND1                                                       
         LA    R1,HLPIND2                                                       
         LA    R1,HLPIND3                                                       
*                                                                               
HELP005  L     RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC   SET CURSOR                                   
         MVI   TIOBCURI,X'01'      ONE PAST                                     
*NOP*    LA    RE,SRVOPTH          OPTION FIELD                                 
         SR    RE,RA                                                            
         STCM  RE,3,TIOBCURD                                                    
         XC    TIOBCURS,TIOBCURS                                                
         DROP  RF                                                               
*                                                                               
HELP010  CLC   0(2,R1),=H'0'                                                    
         BE    INFO2                                                            
         CLC   0(1,R1),ACTION      MATCH ACTION                                 
         BE    HELP020                                                          
         CLI   0(R1),X'FF'         FF MATCHES ANY                               
         BE    HELP020                                                          
         LA    R1,2(R1)            NEXT ENTRY                                   
         B     HELP010                                                          
HELP020  MVC   FLAG,1(R1)          FLAG=INDEX                                   
*                                  EXTRA TEXT STUFF                             
HELP025  LA    R1,HELPTAB                                                       
HELP030  CLC   FLAG,0(R1)                                                       
         BNE   HELP040                                                          
         TM    1(R1),X'80'         DDS PANEL ?                                  
         BNO   HELP031                                                          
         TM    HLPFLG,X'80'                                                     
         BNO   HELP040                                                          
HELP031  MVC   HELPNUM,2(R1)                                                    
         MVC   HELPPAG,HLPPAG                                                   
         SR    RF,RF                                                            
         TM    1(R1),X'40'         EXTRA TEXT ?                                 
         BNO   HELP032                                                          
*        L     RF,?                                                             
HELP032  L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         GOTO1 GETHELP,DMCB,(X'50',HELPKEY),QHDR,(C'B',0),(RF),0                
         DC    H'0'                GETHELP EXITS TO MONITOR                     
HELP040  LA    R1,L'HELPTAB(R1)                                                 
         CLI   0(R1),0                                                          
         BNE   HELP030                                                          
         B     INFO4                                                            
                                                                                
* INFO MESSAGES                                                                 
*                                                                               
INFUID   LA    R2,303              ENTER USER ID                                
         B     INFOX                                                            
INFPID   LA    R2,304              ENTER PERSONAL ID                            
         B     INFOX                                                            
INFPWD   LA    R2,305              ENTER PASSWORD                               
         B     INFOX                                                            
INFNPWD  LA    R2,306              ENTER NEW PASSWORD                           
         B     INFOX                                                            
INFRPWD  LA    R2,307              REENTER NEW PASSWORD TO CONFIRM              
         B     INFOX                                                            
INFPWDOK LA    R2,309              PASSWORD SUCESSFULLY CHANGED                 
         B     INFOX                                                            
INFO2    LA    R2,154              END OF REPORT                                
         B     INFOX                                                            
INFO4    LA    R2,4                HELP UNAVAILABLE                             
         L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         B     INFOX                                                            
                                                                                
* OUTPUT INFO MESSAGES - RE=MSG NUMBER                                          
*                                                                               
INFOX    BAS   RE,SWITBACK         EXIT OK                                      
         L     RD,SAVERD                                                        
         CLI   HLPFLD,0                                                         
         BNE   HELPOUT                                                          
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         CURSOR POS                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 GETTXT,DMCB,(R2),0,(C'I',0),0,0,0                                
         XC    SRVPWDN,SRVPWDN                                                  
         MVI   SRVPWDNH+5,0                                                     
         OI    SRVPWDNH+6,X'80'                                                 
         B     XDIRECT                                                          
                                                                                
* ERROR MESSAGES                                                                
*                                                                               
ERRMBC   LA    R2,SREMBC           MUST BE CONNECTED                            
         B     ERRX                                                             
ERRMBCPW LA    R2,320              MUST BE CONNECTED TO CHANGE PWD              
         B     ERRRE                                                            
ERRMIF   LA    R2,SREMIF                                                        
         B     ERRX                                                             
ERRIIF   LA    R2,SREIIF           INVALID INPUT FIELD                          
         B     ERRX                                                             
ERRUID   LA    R2,319              USER ID INVALID                              
         B     ERRX                                                             
ERRUIDPW LA    R2,321              CANT CHANGE PASSWORD FOR THIS USERID         
         B     ERRRE                                                            
ERRTRM   LA    R2,310              ID INCOMPATIBLE WITH PASSWORD                
         CLI   BYTE,0                                                           
         BE    ERRX                                                             
         LA    R2,311              AS ABOVE + GETIDS OVERFLOW                   
         B     ERRX                                                             
ERRPID   LA    R2,301              PERSONAL ID INVALID                          
         B     ERRX                                                             
ERRPWED  LA    R2,302              PASSWORD EFFECTIVE DATE INVALID              
         B     ERRX                                                             
ERRNPLG  LA    R2,303              NEW PASSWORD IS TOO LONG                     
         B     ERRX                                                             
ERRNPST  LA    R2,304              NEW PASSWORD IS TOO SHORT                    
         B     ERRX                                                             
ERRNSME  LA    R2,128              REENTERED NEW PASSWORD NOT SAME              
         B     ERRX                                                             
ERRNPPV  LA    R2,305              NEW PASSWORD PREVIOUSLY USED                 
         B     ERRX                                                             
ERRNPDR  LA    R2,308              NEW PASSWORD DDS RESERVED                    
         B     ERRX                                                             
ERRNPWDV B     ERRX                NEW PASSWORD ERROR NUM ALREADY SET           
ERRNPWD  LA    R2,306              NEW PASSWORD INVALID                         
         B     ERRX                                                             
ERRPWD   LA    R2,306              PASSWORD INVALID                             
         XC    SRVPWD,SRVPWD                                                    
         MVI   SRVPWDH+5,0                                                      
         OI    SRVPWDH+6,X'80'                                                  
         B     ERRX                                                             
ERRSWSYS LA    R2,317              CONTROL SYSTEM NOT OPERATIONAL               
         B     ERRX                                                             
*                                                                               
ERRRE    LA    R1,SRVSRVH          SET =RE IN S/R FIELD                         
         ST    R1,CURSOR                                                        
         XC    SRVSRV,SRVSRV                                                    
         MVC   SRVSRV(3),=C'=RE'                                                
         OI    SRVSRVH+6,X'80'                                                  
                                                                                
* OUTPUT ERROR MESSAGES - R2=MSG NUMBER                                         
*                                                                               
ERRX     BAS   RE,SWITBACK                                                      
         LA    RE,SAVEDSTR                                                      
         LA    RF,SAVEDL                                                        
         XCEF                                                                   
         BAS   RE,WRITESTR                                                      
         CLI   HLPFLD,0                                                         
         BNE   HELPOUT                                                          
         L     RD,SAVERD           ERRORS CAN BE CALLED FROM ANYWHERE           
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         CURSOR POS                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 GETTXT,DMCB,(R2),0,(C'E',0),0,0,0                                
         XC    SRVPWDN,SRVPWDN                                                  
         MVI   SRVPWDNH+5,0                                                     
         OI    SRVPWDNH+6,X'80'                                                 
         B     XDIRECT                                                          
                                                                                
* SCAN SCREEN INPUT FIELDS FOR HELP REQUEST                                     
*                                                                               
HELPSCAN NTR1                                                                   
         LA    R4,64(RA)           R4=A(FIRST FIELD)                            
         L     R6,ATIOB                                                         
         USING TIOBD,R6                                                         
         SR    R2,R2               CLEAR FIELD COUNT                            
HS01     SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         BZ    HSX                                                              
         TM    1(R4),X'20'         TEST PROT                                    
         BNO   HS03                                                             
         AR    R4,R0               NEXT FIELD                                   
         B     HS01                                                             
HS02     LTR   R0,R0                                                            
         BZ    HS02A                                                            
         TM    HLPFLG,X'80'                                                     
         BNO   *+6                                                              
         BCTR  R1,0                                                             
         LR    R5,R0                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
HS02A    SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         AR    R4,R0                                                            
         B     HS01                                                             
*                                                                               
HS03     EQU   *                   UNPROT FOUND                                 
         LA    R2,1(R2)            INC FIELD COUNT                              
         LA    R1,8(R4)                                                         
         SH    R0,=H'8'                                                         
         SR    R5,R5               POS COUNT ZERO                               
HS04     CLI   0(R1),C'?'                                                       
         BE    HS05                HELP REQUIRED                                
         CLI   TIOBAID,1           CHECK FOR PFKEY 1                            
         BNE   HS04A                                                            
         SR    RF,RF               CHECK IF CURSOR AT THIS FIELD                
         LH    RF,TIOBCURD                                                      
         AR    RF,RA                                                            
         CR    RF,R4                                                            
         BE    HS05                                                             
HS04A    LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,HS04                                                          
         B     HS02                NEXT FIELD                                   
*                                                                               
HS05     XC    HELP,HELP           CLEAR HELP                                   
         ST    R4,QHDR             SAVE ADDR                                    
         STC   R2,HLPFLD           SET FIELD NUM                                
         STC   R5,HLPPOS           POSITION                                     
         STC   R5,5(R4)            AND NEW FIELD LENGTH                         
         TM    DDSFLAG,DDSTRM                                                   
         BNO   HS06                                                             
         CLI   1(R1),C'*'          DDS CAN ENTER ?*                             
         BNE   HS06                                                             
         OI    HLPFLG,X'80'        AND GET DDS HELP                             
         LA    R1,1(R1)                                                         
HS06     SR    R3,R3               CHECK FOR PAGE NO                            
         TM    1(R1),X'F0'                                                      
         BNO   HS02                                                             
         LA    R3,1(R3)                                                         
         TM    2(R1),X'F0'                                                      
         BNO   HS07                                                             
         LA    R3,1(R3)                                                         
         TM    3(R1),X'F0'                                                      
         BNO   HS07                                                             
         LA    R3,1(R3)                                                         
*                                                                               
HS07     BCTR  R3,0                CONVERT PAGE NO                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R1)                                                      
         CVB   R3,DUB                                                           
         STC   R3,HLPPAG                                                        
         B     HS02                                                             
*                                                                               
HSX      XIT1                                                                   
         DROP  R6                                                               
                                                                                
                                                                                
* ENTRY POINT FOR THE CONTROLLER ROUTINES AVAILABLE TO CONTROLLER AND           
* OVERLAYS.  UPON ENTRY, RF HOLDS A(VCOMMON) IN ITS LOW ORDER THREE             
* BYTES AND THE ROUTINE NUMBER IN ITS HIGH ORDER BYTE.  VCOMMON WILL            
* USE THE ROUTINE NUMBER TO BRANCH TO THE DESIRED ROUTINE.                      
*                                                                               
VCOMMON  NTR1  BASE=ABASE1         RB=A(FIRST BASE)                             
         L     R9,ABASE2           R9=A(SECOND BASE)                            
         L     R8,ABASE3           R8=A(THIRD BASE)                             
*                                                                               
         SRL   RF,24               BRANCH TO DESIRED ROUTINE                    
         SLL   RF,2                                                             
         B     VBRANCH(RF)                                                      
                                                                                
* TABLE OF BRANCH ADDRESSES TO CONTROLLER ROUTINES                              
*                                                                               
VBRANCH  B     VREADPID                                                         
         B     VREADUIC                                                         
         B     VREADUIN                                                         
         B     VREADAGY                                                         
         B     VREADPWD                                                         
         B     VREADPNO                                                         
         B     VREADTRM                                                         
         B     VWRITPID                                                         
         B     VWRITPWD                                                         
         B     VADDPWD                                                          
         B     VADDELS                                                          
         B     VDELELS                                                          
         B     VGETELS                                                          
         B     VSETACT                                                          
         B     VGETACT                                                          
         B     VCALLSUB                                                         
         B     VREADTWA                                                         
         B     VWRTTWA                                                          
         B     VHXTOCHR                                                         
         B     VCHRTOHX                                                         
         B     VCONVOFF                                                         
         B     VLOADFLD                                                         
         B     VTESTNUM                                                         
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE PERSONAL ID RECORD ADDRESS ID CODE IN PARAMETER 2                         
*                                                                               
VREADPID LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY),R3=A(PID)              
         LA    R5,KEY              BUILD RECORD KEY                             
         USING SAPEKEY,R5                                                       
         XC    KEY,KEY                                                          
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGYSECP                                                  
         MVC   SAPEPID,0(R3)                                                    
         XC    SAPEDEF,SAPEDEF     READ MOST RECENT EFFECTIVE DATE              
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(IOCONT,DMRDHI),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BE    *+14                                                             
         TM    IOERR,IOEDEL        OK IF DELETED                                
         BNZ   *+6                                                              
         DC    H'0'                OTHER ERROR                                  
*                                                                               
         LR    R5,R2                                                            
         CLC   KEY(SAPEDEF-SAPEKEY),0(R5) IF KEY DOESN'T MATCH RECORD           
         BNE   RPIDNO              RECORD NOT FOUND                             
         MVC   KEY(L'SAPEKEY),0(R5)                                             
*                                                                               
         LA    R4,SAPEDATA                                                      
*                                                                               
         MVI   PWDCNTL,0                                                        
         MVI   PIDCNT,0            INVALID LOGON COUNT                          
         MVI   PIDFLG,0            LAST LOGON FLAGS                             
         XC    PIDDTE,PIDDTE       LAST LOGON DATE                              
         XC    PIDTME,PIDTME       LAST LOGON TIME                              
         XC    PIDNUM,PIDNUM       PID NUMBER IS REQUIRED                       
         MVC   PIDPWD,SPACES       PASSWORD IS REQUIRED                         
*                                                                               
RPID010  CLI   0(R4),0             IF REACHED END OF RECORD THEN ERROR          
         BE    RPID100                                                          
         CLI   0(R4),SALLOELQ      X'04' LAST LOGON                             
         BE    RPID030                                                          
         CLI   0(R4),SAPWDELQ      X'C4' PASSWORD                               
         BE    RPID040                                                          
         CLI   0(R4),SAPERELQ                                                   
         BE    RPID050                                                          
RPID020  LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     RPID010                                                          
*                                                                               
         USING SALLOD,R4                                                        
RPID030  MVC   PIDDTE,SALLODT      SAVE PID LAST LOGON DATE                     
         MVC   PIDTME,SALLOTME     SAVE PID LAST LOGON TIME                     
         MVC   PIDFLG,SALLOFLG     SAVE PID LAST LOGON FLAGS                    
         MVC   PIDCNT,SALLOCNT     SAVE PID INVALID LOGON COUNT                 
         B     RPID020                                                          
*                                                                               
         USING SAPWDD,R4                                                        
RPID040  MVC   PIDNUM,SAPWDNUM     SAVE PASSWORD NUMBER                         
         MVC   PIDPWD,SAPWDCOD     SAVE PASSWORD CODE                           
         B     RPID020                                                          
*                                                                               
         USING SAPERD,R4                                                        
RPID050  MVC   PWDCNTL,SAPERPCN    SAVE PASSWORD CONTROL VALUE                  
         B     RPID020                                                          
*                                                                               
RPID100  OC    PIDNUM,PIDNUM       PID NUMBER IS REQUIRED                       
         BZ    RPIDNO                                                           
         CLC   PIDPWD,SPACES       PASSWORD IS REQUIRED                         
         BNH   RPIDNO                                                           
         B     RPIDYES                                                          
         DROP  R4,R5                                                            
*                                                                               
RPIDNO   B     NO                                                               
RPIDYES  B     YES                                                              
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE USER ID RECORD ADDRESS USER ID CODE IN PARAMETER 2                        
*                                                                               
VREADUIC LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY),R3=A(USER ID)          
         LA    R5,KEY              BUILD RECORD KEY                             
         USING CTIKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,0(R3)                                                     
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(IOCONT,DMREAD),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BNE   RUICNO                                                           
         LR    R5,R2                                                            
         MVC   KEY(L'CTIKEY),0(R5)                                              
         LA    R5,CTIDATA          R5=A(RECORD DATA)                            
*                                                                               
RUIC010  CLI   0(R5),0             IF REACHED END OF RECORD THEN ERROR          
         BE    RUICOK                                                           
         CLI   0(R5),X'02'                                                      
         BE    RUIC030                                                          
         CLI   0(R5),X'06'                                                      
         BE    RUIC040                                                          
         CLI   0(R5),X'07'                                                      
         BE    RUIC050                                                          
RUIC020  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     RUIC010                                                          
*                                                                               
RUIC030  MVC   USERNUM,2(R5)       GET USER ID NUMBER                           
         B     RUIC020                                                          
*                                                                               
RUIC040  MVC   AGYALP,2(R5)        GET AGENCY ALPHA ID                          
         MVC   AGYSEC,2(R5)        INITIALIASE SECURITY AGENCY ID               
         MVC   AGYSECP,AGYSEC                                                   
         MVC   USERLANG,4(R5)      GET USER ID LANGUAGE                         
         B     RUIC020                                                          
*                                                                               
RUIC050  MVC   USEROPTS,2(R5)      GET USER ID OPTIONS FLAGS                    
         B     RUIC020                                                          
*                                                                               
RUICNO   B     NO                                                               
RUICOK   B     YES                                                              
                                                                                
* READ INTO THE MEMORY TERMINAL ID RECORD INFORMATION                           
*                                                                               
VREADTRM L     R2,0(R1)            R2=A(BLOCK OF MEMORY)                        
         L     R7,AUTL                                                          
         USING UTLD,R7                                                          
         LA    R5,KEY              BUILD RECORD KEY                             
         USING CTTKEY,R5                                                        
         XC    KEY,KEY                                                          
         XC    CTTKEY,CTTKEY       BUILD TERMINAL(ALPHA) KEY                    
         MVI   CTTKTYP,C'T'                                                     
         GETLA (R7),CTTKLINE,ADDR=ALPHA                                         
         GOTO1 DATAMGR,DMCB,(IOCONT,DMREAD),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BE    VREADT10                                                         
         MVC   CTTKADDR,=C'%%%T'   SEE IF WILD CARD MATCH                       
         GOTO1 DATAMGR,DMCB,(IOCONT,DMREAD),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         BNE   RTRMNO                                                           
*                                                                               
VREADT10 LR    R5,R2                                                            
         MVC   KEY(L'CTIKEY),0(R5)                                              
         LA    R5,CTTDATA          R5=A(RECORD DATA)                            
*                                                                               
         B     RTRMOK                                                           
*                                                                               
RTRMNO   B     NO                                                               
RTRMOK   B     YES                                                              
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE USER ID RECORD ADDRESS USER ID NUMBER IN PARAMETER 2                      
*                                                                               
VREADUIN LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY),R3=A(USER ID)          
         LA    R5,KEY              BUILD RECORD KEY                             
         USING CTIKEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R3)                                                    
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(IOCONT,DMREAD),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BNE   RUINNO                                                           
         LR    R5,R2                                                            
         MVC   KEY(L'CTIKEY),0(R5)                                              
         LA    R5,CTIDATA          R5=A(RECORD DATA)                            
*                                                                               
RUIN010  CLI   0(R5),0             IF REACHED END OF RECORD THEN ERROR          
         BE    RUINOK                                                           
         CLI   0(R5),X'02'                                                      
         BE    RUIN030                                                          
         CLI   0(R5),X'06'                                                      
         BE    RUIN040                                                          
         CLI   0(R5),X'07'                                                      
         BE    RUIN050                                                          
RUIN020  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     RUIN010                                                          
*                                                                               
RUIN030  MVC   USERID,2(R5)        GET USER ID CODE                             
         B     RUIN020                                                          
*                                                                               
RUIN040  MVC   AGYALP,2(R5)        GET AGENCY ALPHA ID                          
         MVC   AGYSEC,2(R5)        INITIALIASE SECURITY AGENCY ID               
         MVC   AGYSECP,AGYSEC                                                   
         MVC   USERLANG,4(R5)      GET USER ID LANGUAGE                         
         B     RUIN020                                                          
*                                                                               
RUIN050  MVC   USEROPTS,2(R5)      GET USER ID OPTIONS FLAGS                    
         B     RUIN020                                                          
*                                                                               
RUINNO   B     NO                                                               
RUINOK   B     YES                                                              
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE AGENCY ACCESS RECORD ADDRESS ALPHA ID IN PARAMETER 2                      
*                                                                               
VREADAGY LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY)                        
         NI    AGYFLAG,255-X'41'   R3=A(AGENCY ALPHA ID)                        
*                                                                               
RAGY005  LA    R5,KEY              BUILD RECORD KEY                             
         USING CT5KEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,0(R3)                                                   
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(IOCONT,DMREAD),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BNE   RAGYNO                                                           
         LR    R5,R2                                                            
         MVC   KEY(L'CTIKEY),0(R5)                                              
         LA    R5,CT5DATA          R5=A(RECORD DATA)                            
*                                                                               
RAGY010  CLI   0(R5),0             TEST END OF RECORD                           
         BE    RAGY060                                                          
         CLI   0(R5),CTAGDELQ                                                   
         BE    RAGY030                                                          
         CLI   0(R5),CTSEAELQ                                                   
         BE    RAGY040                                                          
         CLI   0(R5),CTAADELQ                                                   
         BE    RAGY050                                                          
RAGY020  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     RAGY010                                                          
*                                                                               
         USING CTAGDD,R5                                                        
RAGY030  TM    AGYFLAG,X'01'       TEST IF SECURITY AGENCY RECORD               
         BO    RAGY020             YES                                          
         MVC   AGYDDSA,CTAGDDA     SAVE DDS AGENCY ACCESS LEVEL FLAGS           
         CLI   CTAGDLEN,CTAGDL2Q   TEST EXTENDED LENGTH                         
         BL    RAGY020                                                          
         MVC   AGYCTRY,CTAGDCTY    SET AGENCY COUNTRY & CURRENCY                
         MVC   AGYCURR,CTAGDCUR                                                 
         B     RAGY020                                                          
*                                                                               
         USING CTSEAD,R5                                                        
RAGY040  TM    AGYFLAG,X'01'       TEST IF SECUROTY AGENCY RECORD               
         BO    RAGY020             YES                                          
         OI    AGYFLAG,X'40'       SET NEED TO READ SECURITY AGENCY             
         MVC   AGYSEC,CTSEAAID     EXTRACT AND SAVE THE SECURITY AGENCY         
         MVC   AGYSECP,AGYSEC                                                   
         TM    AGYFLAG,X'80'       TEST IF DDS SECURITY AGENCY OVERRIDE         
         BZ    *+10                                                             
         MVC   AGYSECP,TRMAGYPE                                                 
         B     RAGY020                                                          
*                                                                               
         USING CTAADD,R5                                                        
RAGY050  MVC   AGYATOU,CTAADATO    EXTRACT PPS PASSWORD DATA                    
         MVC   AGYPTOU,CTAADPTO                                                 
         MVC   AGYPMIN,CTAADPML                                                 
         MVC   AGYPFLG,CTAADFLG                                                 
         MVC   AGYPRUS,CTAADPRU                                                 
         MVC   AGYPPVR,CTAADPVR                                                 
         B     RAGY020                                                          
         DROP  R5                                                               
*                                                                               
RAGY060  TM    AGYFLAG,X'01'       EXIT IF JUST READ SECURITY AGENCY            
         BO    RAGYOK                                                           
         TM    AGYFLAG,X'40'       TEST IF NEED TO READ SECURITY AGY            
         BZ    RAGYOK                                                           
         LA    R3,AGYSEC           R3=A(SECURITY AGENCY CODE)                   
         OI    AGYFLAG,X'01'                                                    
         B     RAGY005             BACK TO READ SECURITY AGENCY                 
*                                                                               
RAGYNO   B     NO                                                               
RAGYOK   B     YES                                                              
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE PASSWORD RECORD ADDRESS PASSWORD CODE IN PARAMETER 2                      
*                                                                               
VREADPWD LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY),R3=A(PASSWORD)         
         LA    R5,KEY              BUILD RECORD KEY                             
         USING SA0KEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGYSECP                                                  
         MVC   SA0KCODE,0(R3)                                                   
         TM    AGYPFLG,CTAADPRQ                                                 
         BZ    RPWD020                                                          
         MVC   SA0KPID,PERSONID                                                 
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(IOCONT,DMRDHI),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BE    RPWD010                                                          
         TM    IOERR,IOERNF        EXIT IF NOT FOUND                            
         BO    RPWDNO                                                           
         TM    IOERR,IOEDEL                                                     
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RPWD010  LR    R5,R2               TEST KEY MATCHES                             
         CLC   KEY(SA0KDTM-SA0KEY),SA0KEY                                       
         BNE   RPWDNO              RECORD NOT FOUND                             
         CLC   KEY+SA0KCODE-SA0KEY(L'SA0KCODE),SA0KCODE                         
         BNE   RPWDNO              RECORD NOT FOUND                             
         MVC   PWDDTMC,SA0KDTM                                                  
         B     RPWD100                                                          
*                                                                               
RPWD020  MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(IOCONT,DMREAD),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BE    RPWD030                                                          
         TM    IOERR,IOERNF        EXIT IF NOT FOUND                            
         BO    RPWDNO                                                           
         TM    IOERR,IOEDEL        OK IF DELETED                                
         BO    *+6                                                              
         DC    H'0'                OTHER ERROR                                  
*                                                                               
RPWD030  LR    R5,R2                                                            
*                                                                               
RPWD100  MVC   KEY(L'SA0KEY),0(R5)                                              
         LA    R5,SA0DATA          R5=A(RECORD DATA)                            
*                                                                               
RPWD110  CLI   0(R5),0             IF REACHED END OF RECORD THEN ERROR          
         BE    RPWDOK                                                           
         CLI   0(R5),SAPEFELQ                                                   
         BE    RPWD130                                                          
         CLI   0(R5),SAPASELQ                                                   
         BE    RPWD140                                                          
RPWD120  ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     RPWD110                                                          
*                                                                               
         USING SAPEFD,R5                                                        
RPWD130  EQU   *                   PROCESS EFFECTIVE DATES ELEMENT              
         MVC   PWDSDAT,SAPEFSTA    SAVE START DATE                              
         MVC   PWDEDAT,SAPEFEND    SAVE END DATE                                
         B     RPWD120             DISPLAY DATES                                
*                                                                               
         USING SAPASD,R5                                                        
RPWD140  EQU   *                   PROCESS PASSWORD# PASSIVE ELEMENT            
         CLI   SAPASLEN,X'04'      CHECK VALID LENGTH                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PWDNUM,SAPASDTA     SAVE PASSWORD NUMBER                         
         B     RPWD120                                                          
         DROP  R5                                                               
*                                                                               
RPWDNO   B     NO                                                               
RPWDOK   B     YES                                                              
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE PASSWORD RECORD ADDRESS NUMBER IN PARAMETER 2                             
*                                                                               
VREADPNO LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY),R3=A(PASSWORD)         
         LA    R5,KEY              BUILD RECORD KEY                             
         USING SA0KEY,R5                                                        
         XC    KEY,KEY                                                          
         MVI   SA0KTYP,SA0KTYPQ                                                 
         MVC   SA0KAGY,AGYSECP                                                  
         MVC   SA0KNUM,0(R3)                                                    
         MVC   KEYSAVE,KEY         SAVE KEY                                     
         GOTO1 DATAMGR,DMCB,(IOCONT,DMRDHI),CTFILE,(R5),(R2),0                  
         MVC   IOERR,8(R1)         SAVE IO ERROR CODE                           
         CLI   IOERR,0                                                          
         BE    *+14                                                             
         TM    IOERR,IOEDEL        OK IF DELETED                                
         BNZ   *+6                                                              
         DC    H'0'                OTHER ERROR                                  
*                                                                               
         LR    R5,R2                                                            
         CLC   KEY(L'SA0KEY),0(R5) IF KEY DOESN'T MATCH RECORD                  
         BNE   RPNONO                RECORD NOT FOUND                           
         MVC   KEY(L'SA0KEY),0(R5)                                              
         B     RPNOYES                                                          
         DROP  R5                                                               
         SPACE 1                                                                
*                                                                               
RPNONO   B     NO                                                               
RPNOYES  B     YES                                                              
                                                                                
* WRITE FROM MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF            
* THE PERSONAL ID RECORD                                                        
*                                                                               
VWRITPID L     R2,0(R1)            R2=A(BLOCK OF MEMORY)                        
         LA    R5,KEY              BUILD RECORD KEY                             
         USING SAPEKEY,R5                                                       
         MVC   KEY(L'SAPEKEY),0(R2)                                             
         GOTO1 DATAMGR,DMCB,DMWRT,=C'CTFILE  ',(R5),(R2),0                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         B     WPIDYES                                                          
         DROP  R5                                                               
*                                                                               
WPIDNO   B     NO                                                               
WPIDYES  B     YES                                                              
                                                                                
* WRITE FROM MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF            
* THE PASSWORD AUTHORISATION RECORD                                             
*                                                                               
VWRITPWD L     R2,0(R1)            R2=A(BLOCK OF MEMORY)                        
         LA    R5,KEY              BUILD RECORD KEY                             
         USING SA0KEY,R5                                                        
         MVC   KEY(L'SA0KEY),0(R2)                                              
         GOTO1 DATAMGR,DMCB,DMWRT,CTFILE,(R5),(R2),0                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
         B     WPWDYES                                                          
         DROP  R5                                                               
*                                                                               
WPWDNO   B     NO                                                               
WPWDYES  B     YES                                                              
                                                                                
* ADD FROM MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF              
* THE PASSWORD AUTHORISATION RECORD                                             
*                                                                               
VADDPWD  L     R2,0(R1)            R2=A(BLOCK OF MEMORY)                        
         LA    R5,KEY              BUILD RECORD KEY                             
         USING SA0KEY,R5                                                        
         MVC   KEY(L'SA0KEY),0(R2)                                              
         GOTO1 DATAMGR,DMCB,DMADD,CTFILE,(R5),(R2),0                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
         B     APWDYES                                                          
         DROP  R5                                                               
*                                                                               
APWDNO   B     NO                                                               
APWDYES  B     YES                                                              
                                                                                
* ROUTINE TO ADD AN ELEMENT TO A RECORD                                         
* NTRY - R1=A(RECORD TO ADD ELEMENT TO) ELEMENT CONTAINS ELEMENT                
*                                                                               
VADDELS  LR    R0,R1                                                            
         GOTO1 HELLO,DMCB,(C'P',CTFILE),(R0),ELEMENT,0                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
ADDELSX  B     YES                                                              
                                                                                
* ROUTINE TO DELETE AN ELEMENT FROM A RECORD                                    
* NTRY - R1=A(RECORD TO DELETE ELEMENT FROM)                                    
* ELEMENT CONTAINS ELEMENT CODE OF ELEMENT TO BE DELETED                        
*                                                                               
VDELELS  LR    R0,R1                                                            
         GOTO1 HELLO,DMCB,(C'D',CTFILE),(ELEMENT,(R0)),                *        
               (ELEMENT+1,ELEMENT+2)                                            
         CLI   12(R1),6            TEST ELEMENT NOT FOUND                       
         BE    DELELSX                                                          
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
DELELSX  B     YES                                                              
                                                                                
* ROUTINE TO GET AN ELEMENT IN A RECORD                                         
* NTRY - R1=A(RECORD TO GET ELEMENT FROM)                                       
*        ELEMENT CONTAINS ELEMENT CODE AND DATA TO SEARCH FOR                   
* EXIT - AELEM  CONTAINS ADDRESS OF ELEMENT OR ZEROES IF NOT FOUND              
*                                                                               
VGETELS  LR    R0,R1                                                            
         GOTO1 HELLO,DMCB,(C'G',CTFILE),(ELEMENT,(R0)),                *        
               (ELEMENT+1,ELEMENT+2)                                            
         XC    AELEM(4),AELEM                                                   
         CLI   12(R1),0                                                         
         BNE   *+10                                                             
         MVC   AELEM(4),12(R1)                                                  
GETELSX  B     YES                                                              
                                                                                
* ROUTINE TO ADD AN ACTIVITY ELEMENT                                            
* NTRY - R1=A(RECORD TO ADD ELEMENT)                                            
*                                                                               
VSETACT  OC    ACTEL,ACTEL         TEST ACTVITY ELEMENT FOUND                   
         BNZ   SETACT1                                                          
         GOTO1 GETACT              NO - EXTRACT IT                              
         MVC   ACTEL,ELEMENT                                                    
*                                                                               
SETACT1  LA    R2,ACTEL                                                         
         USING SAACVD,R2                                                        
         MVI   SAACVEL,SAACVELQ                                                 
         MVI   SAACVLEN,SAACVLNQ                                                
         MVC   SAACVDT,SYSDATEB                                                 
         ICM   RF,3,SAACVSEQ                                                    
         LA    RF,1(RF)                                                         
         STCM  RF,3,SAACVSEQ                                                    
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVC   ELEMENT(1),ACTEL    SET ACTIVITY ELEMENT CODE                    
         GOTO1 DELELS              DELETE AND RE-ADD ELEMENT                    
         MVC   ELEMENT(L'ACTEL),ACTEL                                           
         GOTO1 ADDELS                                                           
         XC    ACTEL,ACTEL         CLEAR ACTIVITY ELEMENT                       
         DROP  R2                                                               
*                                                                               
SETACTX  B     YES                                                              
                                                                                
                                                                                
* ROUTINE TO GET ACTIVITY ELEMENT INTO ACTEL                                    
* NTRY - R1=A(RECORD)                                                           
* EXIT - ACTEL CONTAINS ACTIVITY ELEMENT OR BINARY ZEROES                       
*        ELEMENT CONTAINS ACTIVITY ELEMENT CODE                                 
*        FVMSGNO=ZERO IF ACTIVITY ELEMENT NOT FOUND                             
*        CC=EQUAL IF ELEMENT FOUND,NOT EQUAL IF NOT FOUND                       
*                                                                               
VGETACT  XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT,1                                                        
         XC    ACTEL,ACTEL                                                      
         GOTO1 GETELS                                                           
         ICM   R1,15,AELEM                                                      
         BNZ   *+6                                                              
         DC    H'0'                HERE IF NO ACTIVITY ELEMENT                  
         MVC   ACTEL,0(R1)         SAVE ELEMENT IN WORKING STORAGE              
GETACTX  B     YES                                                              
                                                                                
* READ OVERLAY INTO PROGRAMS AREA, BAS TO IT,AND WHEN IT RETURNS,PASS           
* CONTROL BACK TO THE CALLING OVERLAY.                                          
*                                                                               
VCALLSUB CLI   CSSP,4              DIE IF STACK OVERFLOWS                       
         BL    *+6                                                              
         DC    H'0'                                                             
         L     R2,0(R1)            R2=A(OVERLAY NUMBER)                         
         CLI   0(R2),0             EXIT IF OVERLAY NUMBER IS ZERO               
         BE    CSX                                                              
*                                                                               
         L     R3,CSNXTLOD         STORE ADDR IN FIRST PARM TO CALLOV           
         ST    R3,DMCB                                                          
         MVC   DMCB+4(3),CSPHASE   STORE PHASE DESCRIPTION IN SECOND            
         MVC   DMCB+7(1),0(R2)     PARM TO CALLOV                               
         GOTO1 CALLOV,DMCB,,,0                                                  
         OC    DMCB+9(3),DMCB+9    DIE IF CAN'T FIND OVERLAY                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             DIE IF OVERLAY NOT LOADED WHERE              
         CR    RF,R3               IT WAS WANTED                                
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    DMCB+10(2),DMCB+10                                               
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R4,R3               R4=ADDRESS TO LOAD NEXT OVERLAY              
         AH    R4,DMCB+10                                                       
         LA    R4,8(R4)                                                         
         SRL   R4,3                                                             
         SLL   R4,3                ROUND UP TO NEAREST DOUBLE WORD              
*                                                                               
         ZIC   RF,CSSP             SAVE CURRENT LOAD ADDRESS ON STACK           
         SLL   RF,2                                                             
         LA    RF,CSSTACK(RF)                                                   
         ST    R3,0(RF)                                                         
         ST    R4,CSNXTLOD         SAVE NEXT LOAD ADDRESS IN CSNXTLOD           
*                                                                               
         ZIC   RF,CSSP             INCREMENT STACK POINTER                      
         LA    RF,1(RF)                                                         
         STC   RF,CSSP                                                          
*                                                                               
         GOTO1 (R3),DMCB,(RC)      PASS CONTROL TO OVERLAY                      
*                                                                               
         ZIC   RF,CSSP             DECREMENT STACK POINTER                      
         BCTR  RF,0                                                             
         STC   RF,CSSP                                                          
         SLL   RF,2                RESTORE NEXT LOAD ADDRESS FROM STACK         
         LA    RF,CSSTACK(RF)                                                   
         MVC   CSNXTLOD,0(RF)                                                   
*                                                                               
CSX      B     XIT                 EXIT BACK TO CALLER                          
                                                                                
* READ INTO THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 THE CONTENTS OF         
* THE TWA RECORD NUMBER SPECIFIED BY PARAMETER 2.                               
*                                                                               
VREADTWA LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY),R3=A(TWA NUM)          
         SLL   R3,32-8             MOVE TO HIGH ORDER BYTE                      
         L     RF,ATWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM #            
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'TEMPSTR',(R3),(R2),0              
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
                                                                                
* WRITE FROM THE MEMORY ADDRESS SPECIFIED BY PARAMETER 1 INTO THE TWA           
* RECORD NUMBER SPECIFIED BY PARAMETER 2.                                       
*                                                                               
VWRTTWA  LM    R2,R3,0(R1)         R2=A(BLOCK OF MEMORY),R3=A(TWA NUM)          
         SLL   R3,32-8             MOVE TO HIGH ORDER BYTE                      
         L     RF,ATWA                                                          
         ICM   R3,3,2(RF)          SET TWO LOW ORDER BYTES TO TERM NUM          
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMWRT'),=C'TEMPSTR',(R3),(R2),0               
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
                                                                                
* THIS ROUTINE CONVERTS THE BINARY NUMBER IN PARAMETER ONE TO AN EIGHT          
* DIGIT '0' FILLED HEX CHARACTER STRING WHICH IS RETURNED IN DUB.  THE          
* ROUTINE RETURNS THE NUMBER OF SIGNIFIGANT CHARACTERS (I.E. 0-F = 1,           
* 10-FF = 2) IN PARAMETER ONE.                                                  
*                                                                               
VHXTOCHR L     R3,0(R1)            R3=BINARY NUMBER                             
         MVC   DUB,=C'00000000'    FILL DUB WITH '0'S                           
         LA    R4,DUB+7            R4=A(LAST BYTE IN DUB)                       
*                                                                               
HC10     SR    R2,R2               R3=R3/16,R2=REMAINDER                        
         D     R2,=F'16'                                                        
         C     R2,=F'10'           IF R2>=10                                    
         BL    *+12                                                             
         LA    R2,C'A'-10(R2)      THEN R2=R2+C'A'-10                           
         B     *+8                                                              
         LA    R2,C'0'(R2)         ELSE R2=R2+C'0'                              
         STC   R2,0(R4)            SAVE R2 IN DUB                               
*                                                                               
         LTR   R3,R3               IF R3=0 THEN WE ARE DONE                     
         BZ    HC90                                                             
         BCTR  R4,0                ELSE BACK UP POINTER TO DUB                  
         B     HC10                LOOP BACK                                    
*                                                                               
HC90     LA    RF,DUB+8            RETURN LENGTH OF STRING                      
         SR    RF,R5                                                            
         ST    RF,0(R1)                                                         
*                                                                               
HCX      B     XIT                                                              
                                                                                
* THIS ROUTINE CONVERTS THE HEX CHARACTER STRING WHOSE ADDRESS IS IN            
* PARAMETER ONE AND LENGTH IN PARAMETER TWO TO ITS BINARY VALUE WHICH           
* IT RETURNS IN PARAMETER ONE.  IF ANY OF THE CHARACTERS IN THE STRING          
* ARE NOT VALID HEX CHARACTERS, THE ROUTINE RETURNS 'NO'. OTHERWISE,            
* IT RETURNS 'YES'.                                                             
*                                                                               
VCHRTOHX LM    R2,R3,0(R1)         R2=A(STRING),R3=L'STRING                     
         SR    R5,R5                                                            
CH10     M     R4,=F'16'           R5=R5*16 (SHIFT DIGITS OVER ONE)             
         ZIC   RF,0(R2)            RF=CURRENT CHARACTER TO TRANSLATE            
*                                                                               
         CLM   RF,1,=C'A'          IF RF<'A' THEN ERROR                         
         BL    CHNO                                                             
         CLM   RF,1,=C'F'          ELSE IF RF<='F'                              
         BH    CH20                                                             
         LA    R0,C'A'-10          THEN R0='A'-10                               
         B     CH90                                                             
*                                                                               
CH20     CLM   RF,1,=C'0'          ELSE IF RF<'0' THEN ERROR                    
         BL    CHNO                                                             
         CLM   RF,1,=C'9'          ELSE IF RF>'9' THEN ERROR                    
         BH    CHNO                                                             
         LA    R0,C'0'             ELSE R0=RF-'0'                               
*                                                                               
CH90     SR    RF,R0               R5=R5+RF-R0 (ADD DIGIT IN)                   
         AR    R5,RF                                                            
         LA    R2,1(R2)            BUMP TO NEXT CHARACTER                       
         BCT   R3,CH10             REPEAT UNTIL NO MORE CHARACTERS              
*                                                                               
         ST    R5,0(R1)            RETURN BINARY NUMBER                         
*                                                                               
CHYES    B     YES                                                              
CHNO     B     NO                                                               
                                                                                
* THIS ROUTINE CONVERTS AN OFFICE CODE FROM 2 TO 8 BYTE FORMAT OR FROM          
* 8 TO 2 BYTE FORMAT.  IT DOES SO BY READING THE CONTROL FILE FOR THE           
* OFFICE RECORD.  IF IT CANNOT FIND THE OFFICE RECORD IT RETURNS 'NO'.          
* OTHERWISE IT DOES THE CONVERSION AND RETURNS 'YES'.                           
*                                                                               
* PARM 1 A(INPUT OFFICE CODE)                                                   
*        BYTE 0 0 = CONVERT FROM 2 TO 8 BYTE FORMAT                             
*               1 = CONVERT FROM 8 TO 2 BYTE FORMAT                             
*PARM 2  A(OUTPUT OFFICE CODE)                                                  
*                                                                               
VCONVOFF LM    R2,R3,0(R1)         R2=A(INPUT OFF),R3=A(OUTPUT OFF)             
         MVC   BYTE,0(R1)          BYTE = CONVERSION TYPE                       
*                                                                               
         LA    R4,KEY              BUILD CONTROL FILE USER ID KEY               
         USING CTIKEY,R4                                                        
         XC    KEY,KEY                                                          
         MVI   CTIKTYP,C'I'                                                     
*                                                                               
         CLI   BYTE,0              IF CONVERTING 2 TO 8 BYTE FORMAT             
         BNE   CO10                                                             
         XC    CTIKID(8),CTIKID    THEN PUT 2 BYTE FORMAT IN KEY                
         MVC   CTIKID+8(2),0(R2)                                                
         B     CO20                                                             
*                                                                               
CO10     MVC   CTIKID(8),0(R2)     ELSE PUT 8 BYTE FORMAT IN KEY                
         MVC   CTIKID+8(2),=CL2' '                                              
CO20     GOTO1 DATAMGR,DMCB,=C'DMRDHI',=CL8'CTFILE',KEY,AIO,0                   
         CLI   8(R1),0                                                          
         BNE   CONO                DISK ERROR                                   
*                                                                               
         L     R4,AIO              R4=A(RECORD)                                 
         CLC   KEY(25),0(R4)       IF KEY DOESN'T MATCH RECORD                  
         BNE   CONO                THEN ERROR RECORD NOT FOUND                  
         LA    R4,CTIDATA          R4=A(RECORD DATA)                            
         USING CTDSCD,R4                                                        
*                                                                               
CO30     CLI   0(R4),0             IF REACHED END OF RECORD THEN ERROR          
         BE    CONO                                                             
         CLI   0(R4),X'02'         ELSE IF FOUND DESC ELEMENT THEN DONE         
         BE    CO40                                                             
         ZIC   R0,1(R4)            ELSE BUMP TO NEXT ELEMENT                    
         AR    R4,R0                                                            
         B     CO30                LOOP BACK                                    
*                                                                               
CO40     CLI   BYTE,0              IF CONVERTING 2 TO 8 BYTE FORMAT             
         BNE   CO50                                                             
         MVC   0(8,R3),CTDSC       RETURN 8 BYTE FORMAT IN PARM 2               
         B     COYES                                                            
*                                                                               
CO50     MVC   0(2,R3),CTDSC       ELSE RETURN 2 BYTE FORMAT IN PARM 2          
         DROP  R4                                                               
*                                                                               
COYES    B     YES                                                              
CONO     B     NO                                                               
                                                                                
* LOAD STRING OF DATA INTO UNPROTECTED TWA FIELD AT GIVEN RELATIVE              
* POSITION SETTING VALUES IN FIELD HEADER                                       
* PARAM1 A(DATA)                                                                
* PARAM2 LENGTH DATA                                                            
* PARAM3 FIELD NUMBER                                                           
*                                                                               
VLOADFLD LM    R2,R4,0(R1)         R2=A(DATA),R3=LENGTH,R4=FLD NUM              
         SR    RF,RF               FIND UNPROTECTED FIELD                       
         L     RE,ATWA             IN TWA                                       
         LA    RE,64(RE)                                                        
         USING FLDHDRD,RE                                                       
*                                                                               
LF10     TM    FLDATB,FATBPROT                                                  
         BZ    LF30                                                             
*                                                                               
LF20     IC    RF,FLDLEN           BUMP TO NEXT FIELD                           
         AR    RE,RF                                                            
         B     LF10                                                             
*                                                                               
LF30     BCT   R4,LF20             BUMP TO NEXT FIELD                           
*                                  SET FIELD HEADER STATUS                      
         OI    FLDIIND,X'80'       INPUT THIS TIME                              
         STC   R3,FLDILEN          INPUT LENGTH                                 
         LTR   R3,R3                                                            
         BZ    LFX                                                              
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),0(R2)    MOVE IN DATA                                 
*                                                                               
         SR    R2,R2               SET NUM/ALPHA/HEX BITS                       
         IC    R2,FLDILEN                                                       
         LTR   R2,R2                                                            
         BZ    LFX                                                              
         LA    R3,FLDDATA                                                       
         OI    FLDIIND,B'00001110' SET BITS ON                                  
*                                                                               
LF40     TM    FLDATB,FATBLC       LOWER CASE TESTS                             
         BZ    LF50                                                             
         CLI   0(R3),X'81'         TEST LITTLE A                                
         BL    LF50A                                                            
         CLI   0(R3),X'A9'         TEST LITTLE Z                                
         BH    LF50                                                             
         NI    FLDIIND,B'11110101' FLD NOT NUM/HEX                              
         B     LF60                                                             
*                                                                               
LF50     CLI   0(R3),C'A'                                                       
         BNL   *+12                                                             
LF50A    NI    FLDIIND,B'11110001' FLD NOT NUM/ALPHA/HEX                        
         B     LF70                                                             
LF50B    CLI   0(R3),C'Z'                                                       
         BNH   LF50C                                                            
         NI    FLDIIND,B'11111011' FLD NOT ALPHA                                
         B     LF60                                                             
LF50C    NI    FLDIIND,B'11110111' FLD NOT NUM                                  
         CLI   0(R3),C'F'                                                       
         BNH   *+8                                                              
         NI    FLDIIND,B'11111101' FLD NOT HEX                                  
*                                                                               
LF60     LA    R3,1(R3)                                                         
         BCT   R2,LF40                                                          
*                                                                               
LF70     TM    FLDATB,FATBNUM      REQUIRED NUM FIELD                           
         BZ    LFX                 NO                                           
         TM    FLDIIND,X'08'       IS IT NUM                                    
         BO    LFX                 YES                                          
         OI    FLDIIND,X'10'       NO SET FLD INV                               
         B     LFX                                                              
*                                                                               
LFX      B     XIT                                                              
         DROP  RE                                                               
                                                                                
* TEST IF SOME NUMBER OF BYTES PAST A GIVEN ADDRESS ARE ALL NUMERIC             
* PARAM1 A(FIRST BYTE)                                                          
* PARAM2 NUMBER OF BYTES                                                        
* RETURN COND CODE IS EQ IF ALL BYTES ARE NUMERIC,NEQ OTHERWISE                 
*                                                                               
VTESTNUM LM    R2,R3,0(R1)         R2=A(FIRST BYTE),R3=NUM OF BYTES             
*                                                                               
TN10     CLI   0(R2),C'0'          TEST BYTE IS NOT NUMERIC                     
         BL    TN20                                                             
         CLI   0(R2),C'9'                                                       
         BH    TN20                                                             
         LA    R2,1(R2)            BUMP TO NEXT BYTE AND TRY AGAIN              
         BCT   R3,TN10                                                          
         CR    RF,RF               ALL BYTES NUMERIC - RETURN EQ                
         B     TNX                                                              
*                                                                               
TN20     LTR   RF,RF               SOME BYTE NOT NUMERIC - RETURN NEQ           
*                                                                               
TNX      B     XIT                                                              
                                                                                
* CONSTANTS USED FOR DATAMGR CALLS                                              
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READL    DC    CL8'READ'                                                        
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMADD    DC    CL8'DMADD'                                                       
PRTQUE   DC    CL8'PRTQUE'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
                                                                                
* TABLE OF LINKED ROUTINES ADDRESSES                                            
*                                                                               
LINKED   DS    0F                                                               
         DC    V(DUMMY)            AREA TO LOAD OVERLAYS                        
*                                                                               
NLINKED  EQU   (*-LINKED)/4                                                     
                                                                                
* TABLE OF CORE RESIDENT ROUTINE OVERLAY NUMBER EQUATES                         
*                                                                               
CORERES  DS    0X                                                               
*&&US*&& DC    X'FA'                                                            
*&&UK*&& DC    X'F9'                                                            
*                                                                               
NCORERES EQU   (*-CORERES)                                                      
*                                                                               
RELO     DS    A                   RELOCATION CONSTANT                          
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                                                                   
*                                                                               
XDIRECT  L     RD,SAVERD           EXIT MODULE                                  
         XMOD1 1                                                                
                                                                                
         LTORG                                                                  
                                                                                
* CONSTANTS AND EQUATES                                                         
*                                                                               
IORUPD   EQU   X'80'                                                            
IORUPDEL EQU   X'88'                                                            
IORDEL   EQU   X'08'                                                            
IOERNF   EQU   X'10'                                                            
IOEEOF   EQU   X'80'                                                            
IOEDEL   EQU   X'02'                                                            
*                                                                               
DDSSECP  DS    0CL2                                                             
*&&UK*&& DC    C'#E'                                                            
*&&US*&& DC    C'#N'                                                            
*                                                                               
FFILL    DC    32X'FF'                                                          
SPACES   DC    32C' '                                                           
                                                                                
* HELP INDEX 2 BYTES ACTION-INDEX 00=NO ACTION FF=ANY ACTION                    
*                                                                               
HLPIND1  DC    X'FF010000'                                                      
*                                                                               
HLPIND2  DC    X'FF020000'                                                      
*                                                                               
HLPIND3  DC    X'FF030000'                                                      
*                                                                               
HLPIND4  DC    X'FF040000'                                                      
                                                                                
* FLAGS  X'80' DDS ONLY PANEL                                                   
*        X'40' EXTRA TEXT BLOCK IN PQSAVE                                       
*                                                                               
HELPTAB  DS    0CL3                    INDEX/FLAGS/PANEL                        
         DC    X'01',X'00',X'01'                                                
         DC    X'02',X'00',X'02'                                                
         DC    X'03',X'00',X'03'                                                
         DC    X'04',X'00',X'04'                                                
         DC    X'00'                                                            
*                                                                               
HELPID   DC    XL10'0115FF00000000000000'                                       
                                                                                
DDDCLST  DS    0C                                                               
         DCDDL SR#UPD,9,L                                                       
         DCDDL SR#DSP,9,L                                                       
         DCDDL SR#RLEAS,9,L                                                     
         DCDDL SR#UNKNW,9,L                                                     
         DCDDL SR#PURGE,9,L                                                     
         DCDDL SR#LAST,9,L                                                      
         DCDDL SR#NEXT,9,L                                                      
         DCDDL SR#CLEAR,9,L                                                     
*                                                                               
DC@CODE  DC    CL8'CODE'                                                        
DC@DDS   DC    CL8'DDS'                                                         
DC@USR   DC    CL8'U'                                                           
DC@SWIT  DC    CL8'SWITCH'                                                      
                                                                                
CONTROLD DSECT                     WORKING STORAGE DSECT                        
                                                                                
* FIRST HALF OF WORKING STORAGE (NOT SAVED)                                     
*                                                                               
DMCB     DS    6F                  6 FULL WORD PARAMETER LIST                   
DUB      DS    D                   DOUBLE WORD                                  
DUB1     DS    D                   DOUBLE WORD                                  
FULL     DS    F                   FULL WORD                                    
HALF     DS    H                   HALF WORD                                    
BYTE     DS    B                   BYTE                                         
WORK     DS    XL64                WORK AREA FOR EDIT INSTRUCTION               
WORK80   DS    XL80                80 BYTE WORK AREA                            
BLOCK    DS    XL256               256 BYTE WORK AREA                           
SAVERD   DS    F                   SAVE REGISTER D ON ENTRY                     
*                                                                               
PCIACT   DS    H                   ??                                           
RECLEN   DS    H                   LENGTH OF TWA RECORD                         
CHKDSP   DS    H                   LENGTH OF TWA TO START OF CHKPNT             
GLODSP   DS    H                   LENGTH OF TWA TO START OF GLOBASS            
TODAY    DS    XL2                 TODAYS DATE BINARY COMPRESSED                
TODAYC   DS    XL2                 AS ABOVE 1S COMPLEMENTED                     
DATETIME DS    XL4                 DATE/TIME VALUE FROM DATTIM                  
DATETIMC DS    XL4                 DATE/TIME VALUE 1'S COMPLEMENTED             
SYSDATEB DS    XL3                 SYSTEM DATE BINARY (YMD,GETFACTS)            
MVSTIME  DS    F                   IBM TIME BINARY 100THS SECS.                 
MVSDATE  DS    F                   IBM DATE JULIAN                              
TSRSAVE  DS    XL2                 UTL TSVCREQ VALUE SAVE                       
CTSRVMSG DS    CL(L'SRVMSG)        =CT SERVICE REQUEST MESSAGE SAVE             
MSG      DS    CL80                                                             
EXT      DS    CL132               EXTRA TEXT FOR GETTXT CALLS                  
ASRVSE   DS    A                   ADDRESS TWA FIELD FOR CURSOR                 
CURSOR   DS    A                   ADDRESS TWA FIELD FOR CURSOR                 
ACTION   DS    X                   ??                                           
ADDATE   DS    CL8                 ADDAY DATE WORK SPACE YYMMDD EBCDIC          
MODE     DS    XL1                 MODE OF ENTRY SET FROM SRV REQ FIELD         
MODEPASQ EQU   1                   =PASS                                        
MODELONQ EQU   2                   =LOGON                                       
MODELOFQ EQU   3                   =LOGOFF                                      
MODEPC   DS    XL1                 =PC MODE OF ENTRY IF C'Y'                    
IOCONT   DS    XL1                 IO CONTROL BYTE PARAM 1 DATAMGR              
IOERR    DS    XL1                 IO ERROR RETURN CODE DATAMGR                 
PERSONID DS    CL8                 PERSONAL ID CODE READ FROM SCREEN            
*                                                                               
NPWDINPT DS    CL10                NEW PASSWORD AS INPUT                        
NPWDUPPR DS    CL10                NEW PASSWORD IN UPPER CASE                   
OPWDINPT DS    CL10                OLD PASSWORD AS INPUT                        
OPWDUPPR DS    CL10                OLD PASSWORD IN UPPER CASE                   
OPWDNUM  DS    XL2                 OLD PASSWORD NUMBER                          
OPWDSDAT DS    XL2                 OLD PASSWORD START EFFECTIVE DATE            
OPWDEDAT DS    XL2                 OLD PASSWORD END EFFECTIVE DATE              
*                                                                               
CONNUM   DS    XL1                 CONTROL SYSTEM NUMBER                        
CSYSDATA DS    CL(L'SRVSYS)        CONNECT SYSTEM FIELD DATA                    
CPRGDATA DS    CL(L'SRVPRG)        CONNECT PROGRAM FIELD DATA                   
CONDATA  DS    CL(L'SRVDATA)       CONNECT DATA FIELD DATA                      
SWITCHED DS    XL1                 SWITCHED TO CONTROL SYSTEM FLAG              
UIDLEN   DS    XL1                 USER ID INPUT LENGTH                         
PIDLEN   DS    XL1                 PERSONAL ID INPUT LENGTH                     
OPWDLEN  DS    XL1                 OLD PASSWORD INPUT LENGTH                    
NPWDLEN  DS    XL1                 NEW PASSWORD INPUT LENGTH                    
CSYSLEN  DS    XL1                 CONNECT SYSTEM INPUT LENGTH                  
CPRGLEN  DS    XL1                 CONNECT PROGRAM INPUT LENGTH                 
CONDLEN  DS    XL1                 CONNECT DATA INPUT LENGTH                    
INPFLAGS DS    XL1                 INPUT FLAGS                                  
INPFUIDQ EQU   X'01'               USER ID WAS INPUT                            
INPFPIDQ EQU   X'02'               PERSONAL ID WAS INPUT                        
INPFOPWQ EQU   X'04'               OLD PASSWORD WAS INPUT                       
INPFNPWQ EQU   X'08'               NEW PASSWORD WAS INPUT                       
INPFSYSQ EQU   X'10'               CONNECT SYSTEM WAS INPUT                     
INPFPRGQ EQU   X'20'               CONNECT PROGRAM WAS INPUT                    
INPFCDAQ EQU   X'40'               CONNECT DATA WAS INPUT                       
                                                                                
* ADDRESSES OF RESOURCES PASSED FROM FAKPAK TO THE CONTROLLER                   
* ON THE PARAMETER LIST                                                         
*                                                                               
APARMS   DS    A                   A(PARAMETER LIST)                            
ASYSFACS DS    A                   A(SYSFACS)                                   
ATIA     DS    A                   A(TIA)                                       
AUTL     DS    A                   A(UTL)                                       
ACOMFACS DS    A                   A(COMFACS)                                   
ATWA     DS    A                   A(TWA)                                       
ATIOB    DS    A                   A(TIOB)                                      
ASSB     DS    A                   A(SSB)                                       
ATCB     DS    A                   A(TCB)                                       
ASYSLST  DS    A                   A(SYSLST)                                    
                                                                                
* ADDRESSES OF THE BASE REGISTERS AND SECTIONS OF MEMORY                        
* ALLOCATED BY THE CONTROLLER'S NMOD INSTRUCTION                                
*                                                                               
ABASE1   DS    A                   A(BASE1)                                     
ABASE2   DS    A                   A(BASE2)                                     
ABASE3   DS    A                   A(BASE3)                                     
ASAVE    DS    A                   A(CONTROLLER SAVED MEMORY)                   
AOVER    DS    A                   A(OVERLAY SAVED MEMORY)                      
AIOS     DS    A                   A(DATAMGR IO AREAS)                          
AFREE    DS    A                   A(FREE MEMORY FOR OVERLAYS)                  
APCTWASV DS    A                   A(PC SCREEN TWA SAVE AREA)                   
         DS    20A                 SPARE                                        
                                                                                
* ADDRESSES OF COMFACS ROUTINES                                                 
*                                                                               
COMFADRS DS    0A                                                               
CALLOV   DS    A                                                                
DATAMGR  DS    A                                                                
SCANNER  DS    A                                                                
TERMVAL  DS    A                                                                
DATCON   DS    A                                                                
ADDAY    DS    A                                                                
GETRET   DS    A                                                                
GETFACT  DS    A                                                                
GETTXT   DS    A                                                                
GETHELP  DS    A                                                                
SWITCH   DS    A                                                                
HEXOUT   DS    A                                                                
DICTATE  DS    A                                                                
HELLO    DS    A                                                                
         DS    10A                 SPARE                                        
                                                                                
* ADDRESSES OF ROUTINES LINKED WITH THE CONTROLLER                              
*                                                                               
EXTERNS  DS    0V                                                               
DUMMY    DS    V                   DUMMY 32000 BYTE AREA FO OVERLAYS            
         DS    20V                 SPARE                                        
                                                                                
* ADDRESSES OF CORE RESIDENT ROUTINES                                           
*                                                                               
COREADRS DS    0V                                                               
AGETIDS  DS    V                   GETIDS                                       
         DS    19V                 SPARE                                        
                                                                                
* ADDRESSES OF CONTROLLER ROUTINES                                              
*                                                                               
COMMADRS DS    0A                                                               
READPID  DS    A                   READ PERSONAL ID RECORD                      
READUIC  DS    A                   READ USER ID CODE RECORD                     
READUIN  DS    A                   READ USER ID NUMBER RECORD                   
READAGY  DS    A                   READ AGENCY ALPHA ID RECORD                  
READPWD  DS    A                   READ PASSWORD RECORD                         
READPNO  DS    A                   READ PASSWORD NUMBER                         
READTRM  DS    A                   READ TERMINAL RECORD                         
WRITPID  DS    A                   WRITE PERSONAL ID RECORD                     
WRITPWD  DS    A                   WRITE PASSWORD RECORD                        
ADDPWD   DS    A                   ADD PASSWORD RECORD                          
ADDELS   DS    A                                                                
DELELS   DS    A                                                                
GETELS   DS    A                                                                
SETACT   DS    A                                                                
GETACT   DS    A                                                                
CALLSUB  DS    A                   LOAD OVERLAY INTO MEMORY AND BAS IT          
READTWA  DS    A                   READ TWA INTO MEMORY                         
WRTTWA   DS    A                   WRITE MEMORY TO TWA                          
HEXTOCHR DS    A                   CONVERT HEX TO CHARACTER                     
CHRTOHEX DS    A                   CONVERT CHARACTER TO HEX                     
CONVOFF  DS    A                   CONVERT OFFICE CODE BETWEEN FORMATS          
LOADFLD  DS    A                   LOAD TWA FIELD AND HEADER                    
TESTNUM  DS    A                   TEST N CONTIGUOUS BYTES ARE NUMERIC          
         DS    18A                 SPARE                                        
                                                                                
* VARIABLES SET FROM UTL                                                        
*                                                                               
TRMTYP   DS    XL(L'TSTAT)                                                      
TRMTYP1  DS    XL(L'TTYPE)                                                      
TRMCTRY  DS    XL(L'TCTRY)                                                      
TRMUSER  DS    XL(L'TUSER)                                                      
TRMPWDNO DS    XL(L'TPASSWD)                                                    
TRMAGY   DS    XL(L'TAGY)                                                       
TRMAGYSE DS    XL(L'TAGYSEC)                                                    
TRMAGYPE DS    XL(L'TAGYPER)                                                    
TRMFLAG  DS    XL(L'TFLAG)                                                      
TRMSYS   DS    XL(L'TSYS)                                                       
TRM      DS    XL2                                                              
TRMLUID  DS    CL8                                                              
DDSFLAG  DS    X                   TERMINAL STATUS FLAG                         
DDSTRM   EQU   X'20'                                                            
DDSACT   EQU   X'20'                                                            
DDSNEW   EQU   X'04'                                                            
                                                                                
* WORK AREA FOR HELP PROCESSING                                                 
*                                                                               
QHDR     DS    A                   HELP ROUTINE WORK AREAS                      
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
HELP     DS    0CL4                                                             
HLPFLD   DS    C                                                                
HLPPOS   DS    C                                                                
HLPPAG   DS    C                                                                
HLPFLG   DS    C                                                                
FLAG     DS    C                                                                
                                                                                
* VARIABLES USED BY READPID & READPWD                                           
*                                                                               
PIDNUM   DS    XL2                 PASSWORD NUMBER FROM PERSON ID               
PIDPWD   DS    CL10                PASSWORD CODE FROM PERSONAL ID               
PIDPWDU  DS    CL10                PASSWORD CODE FROM PID IN UPPER CASE         
PIDCNT   DS    XL1                 PERSON INVALID LOGON COUNT                   
PIDFLG   DS    XL1                 PERSON LAST LOGON FLAGS                      
PIDDTE   DS    XL3                 PERSON LAST LOGON DATE                       
PIDTME   DS    XL3                 PERSON LAST LOGON TIME                       
PWDSDAT  DS    XL2                 PASSWORD EFFECTIVE START DATE                
PWDEDAT  DS    XL2                 PASSWORD EFFECTIVE END DATE                  
PWDDTMC  DS    XL4                 PASSWORD EFFECTIVE DATE/TIME 1'S COM         
PWDNUM   DS    XL2                 PASSWORD NUMBER                              
PWDCNTL  DS    CL1                 PASSWORD CONTROL                             
                                                                                
* VARIABLES USED BY READUIC & READUIN                                           
*                                                                               
IDOSID   DS    CL10                SYNONYM ID NAME                              
USERID   DS    CL10                USERID CODE                                  
USERNUM  DS    XL2                 USERID NUMBER                                
USERLANG DS    XL1                 USERID LANGUAGE CODE                         
USEROPTS DS    XL2                 USERID OPTIONS X'80'=PASSWORD REQD           
                                                                                
* VARIABLES USED BY READAGY                                                     
*                                                                               
AGYALP   DS    CL2                 AGENCY ALPHA ID                              
AGYSEC   DS    CL2                 AGENCY SECURITY ALPHA ID                     
AGYSECP  DS    CL2                 AGENCY SECURITY ALPHA ID FOR PERSON          
AGYDDSA  DS    CL1                 AGENCY DDS ACCESS LEVEL CODE                 
AGYCTRY  DS    XL1                 AGENCY COUNTRY CODE                          
AGYCURR  DS    CL3                 AGENCY CURRENCY CODE                         
AGYATOU  DS    XL2                 AGENCY ADV TIME OUT VALUE                    
AGYPTOU  DS    XL1                 AGENCY PASSWORD TIMEOUT VALUE                
AGYPMIN  DS    XL1                 AGENCY PASSWORD MINIMUM LENGTH               
AGYPFLG  DS    XL1                 AGENCY ACCESS CONTROL FLAGS                  
AGYPRUS  DS    XL1                 AGENCY PASSWORD REUSE COUNT                  
AGYPPVR  DS    XL1                 AGENCY PASSWORD VALIDATION RULE              
AGYFLAG  DS    XL1                 AGENCY SECURITY FLAGS                        
                                                                                
* VARIABLES USED BY RECORD ELEMNT PROCESSING SUBROUTINES                        
*                                                                               
ELEMENT  DS    XL256               ELEMENT DATA SAVE                            
AELEM    DS    A                   ADDRESS ELEMENT                              
ACTEL    DS    XL(SAACVLNQ)        ACTIVITY ELEMENT SAVE                        
                                                                                
* VARIABLES USED BY CALLSUB                                                     
*                                                                               
CSPHASE  DS    XL4                 PHASE PARAMETER FOR CALLOV CALL              
CSSP     DS    X                   OVERLAY STACK POINTER                        
CSSTACK  DS    4F                  OVERLAY STACK                                
CSNXTLOD DS    A   SAVE            A(NEXT PLACE TO LOAD OVERLAY)                
                                                                                
* VARIABLES IN DATAMGR CALLS                                                    
*                                                                               
KEY      DS    XL48                DATAMGR KEY                                  
KEYSAVE  DS    XL48                BACK UP OF KEY                               
AIO      DS    A                   A(CURRENT IO AREA)                           
AIO1     DS    A                   A(FIRST IO AREA)                             
AIO2     DS    A                   A(SECOND IO AREA)                            
AIO3     DS    A                   A(THIRD IO AREA)                             
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
                                                                                
* SECOND HALF OF WORKING STORAGE (SAVED IN TWA1)                                
*                                                                               
         ORG   CONTROLD+2048                                                    
SAVEWHAT DS    F                                                                
SAVEDSTR DS    0F                  SAVED STORAGE VARIABLES (COPY)               
IDENT    DS    CL4                 $PAS INDENTIFIER                             
SVLUID   DS    XL2                 SAVED LAST USER ID NUMBER                    
SVLPID   DS    XL2                 SAVED LAST PERSON PASSWORD NUMBER            
SVNPWD   DS    CL10                SAVED NEW PASSWORD                           
SAVEDL   EQU   *-SAVEDSTR                                                       
                                                                                
* SRPASFFD                                                                      
SRPASFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRPASFFD                                                       
                                                                                
* FADSECTS                                                                      
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
* DDGETRETD                                                                     
* CTGENFILE                                                                     
* SEACSFILE                                                                     
* FAFACTS                                                                       
* FASYSLSTD                                                                     
* FACHKPT                                                                       
* SRDDEQUS                                                                      
* SRERREQUS                                                                     
* DDFLDHDR                                                                      
       ++INCLUDE FADSECTS                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FACHKPT                                                        
       ++INCLUDE SRDDEQUS                                                       
       ++INCLUDE SRERREQUS                                                      
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDGETRETD                                                      
         PRINT ON                                                               
                                                                                
LENCON   EQU   2048                FIRST HALF OF CONTROLLER STORAGE             
LENSAVE  EQU   2048                SECOND HALF OF CONTROLLER STORAGE            
LENIOS   EQU   3*2048              3*2K IO AREAS FOR USE IN DATAMGR             
LENTWASV EQU   10*1024             SPACE FOR PC SCREEN TWA SAVE ON              
                                                                                
LENWORK  EQU   LENCON+LENSAVE+LENIOS+LENTWASV                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SRPAS00   06/22/16'                                      
         END                                                                    
