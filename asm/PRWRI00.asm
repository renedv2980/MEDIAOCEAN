*          DATA SET PRWRI00    AT LEVEL 144 AS OF 04/02/12                      
*PHASE T40500A                                                                  
*INCLUDE COVAIL                                                                 
*INCLUDE KHDUMMY                                                                
*                                                                               
*        TITLE  PRWRI00 - PRINT WRITER ROOT PHASE                               
         TITLE 'PRWRI00 - CHANGE LOG '                                          
*  BOBY 04/27/93 SPLIT OUT GENERAL VALIDATION ROUTINES TO OVERLAY 02            
         TITLE 'PRWRI00 - PRINTPAK WRITER CONTROLLER'                           
         PRINT NOGEN                                                            
T40500   CSECT                                                                  
         NMOD1 (LENWORK+4000),**T40500,RR=RE,CLEAR=YES                          
         LR    R9,R1               SAVE SYSTEM PARAMETERS                       
         LR    R8,RC               SET UP WORKING STORAGE                       
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 4000 BYTE I/O AREAS               
         ST    R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R1,SYSPARMS                                                      
         L     RA,4(R1)                                                         
         ST    RA,ATWA                                                          
         USING T405FFD,RA                                                       
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*                                                                               
         ST    RE,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         STM   R0,RC,FOOTREGS      SAVE REGISTERS FOR FOOT ROUTINE              
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         LR    RE,RD                                              L02           
         LA    R1,4000                      REDUCE BY LEN OF DATE WORK          
         SR    RE,R1                        AREA                  L02           
         ST    RE,PDADATES                  FOR DATE CONSTRUCTION L02           
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
*                                                                               
         NI    CONKEYH+1,X'FF'-X'20'  UNPROTECT ACTION FOR GENCON               
         OI    CONSERVH+1,X'01'     SERVICE FIELD IS ALWAYS MODIFIED            
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         BAS   RE,CHKTOP           CHECK TOP OF SCREEN                          
*                                                                               
         CLI   CONWHEN+5,0         IF NOT A REPORT FUNCTION                     
         BNE   *+8                                                              
         OI    GENSTAT2,DISTHSPG      DON'T SKIP TO NEXT LIST PAGE              
*                                                                               
         MVI   FILTIDNO,8          FILTER ID NUM                                
         OI    GENSTAT5,GENPRVAL   HAVE GENPRG SET ALL FLDS VALID               
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   SKIP IF OFF-LINE                             
         BNZ   INIT10                                                           
*                                                                               
         LA    R2,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R2                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D1'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN FIRST TWA PAGE                       
         MVC   HCBATWA,ATWA        SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
         MVC   HCBATIOB,ATIOB                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEN AS PRINT                          
         MVC   HCBATAB,ATIA        USE TIA FOR DDVAL TABLES                     
         LH    RF,=Y(HELPSAVE-T405FFD)  DISPLACEMENT OF SAVEAREA                
         LA    RF,T405FFD(RF)      HELP SAVE AREA                               
         ST    RF,HCBASAVE         PASS ADDRESS                                 
         MVI   HCBSEGS,34          34 SEGMENTS IN TABLE                         
*                                                                               
         GOTO1 VPRHELP,DMCB,0,0,HELPCB GO CHECK IF IN MIDDLE OF MENU            
*                                                                               
         TM    HCBRTRN,HCBRSELQ    IF SELECTION WAS MADE                        
         BO    INITX                  EXIT FOR USER TO COMPLETE ENTRY           
*                                                                               
         DROP  R2                                                               
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
***                                                                             
* SET UP SECRET BLOCK SO OTHER OVERLAYS DONT NEED TO CALL IT                    
* IT CAN BE CALLED ONLY ONE TIME WITH 'SECPINIT'                                
***                                                                             
         OC    TWAVPRNT,TWAVPRNT   PRINTER MEANS OFF-LINE                       
         BNZ   INITSEC1              HANDLE DIFFERENTLY                         
****                                                                            
****     OC    T405FFD+4(2),T405FFD+4    ON NEW SECURITY?                       
****     BNZ   *+14                      YES                                    
****     OC    T405FFD+6(2),T405FFD+6    LIMIT ACCESS?                          
****     BZ    INITSECX                  NO, DONT NEED TO CALL SECRET           
****                                                                            
         LH    RF,=Y(SECBLK-T405FFD)                                            
         AR    RF,RA                                                            
         ST    RF,ASECBLK                                                       
*                                                                               
         L     R0,ASECBLK          INIT AREA FOR SECURITY BLOCK                 
         LHI   R1,L'SECBLK                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
         MVCL  R0,RE               CLEARS AREA                                  
*                                                                               
         B     INITSEC2                                                         
*                                                                               
INITSEC1 DS    0H                                                               
*                                                                               
         L     RF,TWAMASTC         POINT TO MASTER CONTROL BLOCK                
         MVC   ASECBLK,MCASECRT-MASTD(RF)  SAVE A(SECRET BLOCK)                 
         MVC   PBACCS,MCRHACCS-MASTD(RF)   PASS USER LIMIT ACCESS               
*                                                                               
INITSEC2 DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
*                                                                               
         GOTO1 CSECRET,DMCB,('SECPINIT',ASECBLK),0                              
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         DROP  RF                                                               
*                                                                               
INITSECX DS    0H                                                               
*                                                                               
         MVC   PBASECRT,ASECBLK    PASS A(SECRET BLOCK) TO PWRIIO               
*                                                                               
         GOTO1 INITDRON            INITIALIZE DRONE EARLY          L05          
         SR    RE,RE                                               L05          
         ICM   RE,1,CONRECH+5                                      L05          
         BZ    SYSINT1                                             L05          
         BCTR  RE,0                                                L05          
         EX    RE,WRITER           TEST USER RECORD                L05          
         BE    SYSINT1                                             L05          
         LA    R1,CONRECH                                          L05          
         ST    R1,DRSPTFLD                                         L05          
         OI    DRFLAGS,DREXPDIC                                    L05          
         MVI   DRACTION,DRROW                                      L05          
         GOTO1 DRONE,DMCB,DRGEN    VALIDATE THE RECORD             L05          
         CLI   DRERROR,0                                           L05          
         BNE   BADREC                                              L05          
         CLI   DRATTRIB,C'P'       CHECK ATTRIBUTE FOR REPORT INDICATOR         
         BNE   BADREC                                              L05          
         MVC   RPTOVLY,DRATTRIB+1  YES - SAVE THE REPORT OVERLAY NUMBER         
         CLI   RPTOVLY,0           IF ZERO THEN A SPECIAL TYPE     L05          
         BE    SYSINT1                OF NORMAL WRITER             L05          
         MVC   DPGFILE,DRATTRIB+2  SAVE DPG FILE NUMBER, IF ANY    L05          
         MVC   RPTSCRN,DRATTRIB+3  SAVE SCREEN NUMBER, IF ANY      L05          
*                                                                  L05          
SYSINT1  DS    0H                                                               
*                                                                               
         LR    R0,RC               SAVE WORKING STORAGE POINTER                 
*                                                                               
         GOTO1 =A(GOGENCON),RR=RELO GO TO GENCON                                
*                                                                               
*        IF OFF-LINE NEED TO FREE DPG BUFFER                                    
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    INITDPGX                                                         
*                                                                               
         ICM   R3,15,DRENDBUF      GET SIZE OF GETMAIN AREA                     
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
INITX    DS    0H                                                               
         XIT1                      THEN WE'RE THROUGH                           
*                                                                               
         EJECT                                                                  
*                                                                               
* INITIALIZE SYSTEM ADDRESSES                                                   
*                                                                               
SYSINIT  NTR1                                                                   
*                                  GET TERMINAL VALUES                          
         MVI   DDS,C'N'                                                         
*                                                                               
         CLC   =C'TEST',CONREC     TESTING IS ALWAYS NON-DDS TERMINAL           
         BE    SYSDDSX                                                          
*                                                                               
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
*                                  GET TERMINAL VALUES                          
SYSDDSX  DS    0H                                                               
*                                                                               
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
*                                                                               
*        SET UP CERTAIN ADDRESSES                                               
*                                                                               
         L     R1,SYSPARMS                                                      
         LM    R3,R4,12(R1)         A(TIA) A(COMFACS)                           
         MVC   ATIOB+1(3),1(R1)                                                 
***      =================  ***                                                 
         ST    R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
***      =================  ***                                                 
         ST    R3,ATIA                                                          
         MVC   CALLOV,CCALLOV                                                   
         MVC   DATAMGR,CDATAMGR                                                 
         MVC   GETMSG,CGETMSG                                                   
         MVC   HELLO,CHELLO                                                     
         MVC   SCANNER,CSCANNER                                                 
         MVC   GETPROF,CGETPROF                                                 
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
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
         L     R2,VPRGEN           GET GENERAL ROUTINES OVERLAY ADDR            
*                                                                               
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,NSYSCOMM                                                      
*                                                                               
SYS6     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS6                                                          
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   NO PRINTER MEANS ON-LINE                     
         BZ    SYSOFFX                                                          
*                                                                               
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000AA2'                                           
*                                                                               
         GOTO1 (RF),(R1),0                                                      
*                                                                               
         MVC   VPROFF,DMCB         SAVE PRWRIOFF ADDRESS                        
*                                                                               
         L     R2,VPROFF           GET GENERAL OFFLINE OVERLAY ADDR             
*                                                                               
         SR    R3,R3                                                            
         LA    R4,OFFCOMM                                                       
         LA    R5,NOFFCOMM                                                      
*                                                                               
SYS7     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
*                                                                               
         BCT   R5,SYS7                                                          
*                                                                               
SYSOFFX  DS    0H                                                               
*                                                                               
         EJECT                                                                  
* OTHER INITIALIZATION                                                          
*                                                                               
*----------------------------->    SEED SYSD WITH DUMP COMMENTS                 
         MVC   DUMPSYSD,=C'**SYSD**'                                            
         MVC   DUMPFCIL,=C'*FACILS*'                                            
         MVC   DUMPASRT,=C'*ASSORT*'                                            
         MVC   DUMPPRIO,=C'*PRNTIO*'                                            
         LH    R1,=Y(BUFF-8-SYSD)                                               
         LA    R1,SYSD(R1)                                                      
         MVC   0(8,R1),=C'**BUFF**'                                             
         LA    R1,8(R1)                                                         
         MVC   0(8,R1),=C'**DPG***'                                             
         LA    R1,8(R1)                                                         
         ST    R1,ADPGPROG                                                      
         ST    R1,DRSTBUF                                                       
         ST    R1,DRBFSTRT                                                      
         LA    R1,4000(R1)                                                      
         ST    R1,DRENDBUF                                                      
         MVC   0(8,R1),=C'**DPGIO*'                                             
         LA    R1,8(R1)                                                         
         ST    R1,DRPRTIO                                                       
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
         LA    R4,DRSTBUF          BUFFER ADDRESS SAVEAREA                      
*                                                                               
         GETMAIN EC,LV=(R3),A=(R4)  GET CORE                                    
*                                                                               
         LTR   RF,RF                                                            
         BZ    *+6                 CHECK FOR ERRORS                             
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,DRSTBUF       BUFFER START ADDRESS                         
         ST    R1,ADPGPROG                                                      
         ST    R1,DRBFSTRT         SAVE FOR FREEMAIN                            
*                                                                               
         LA    R1,0(R3,R1)         BUFFER END ADDRESS                           
         ST    R1,DRENDBUF                                                      
*                                                                               
SYSIDPGX DS    0H                                                               
*                                  SET SYSTEM DEPENDENT VALUES                  
         ICM   RF,15,DUMMY         END OF BASE ADDRESS                          
*****    A     RF,=A(8*4096)       AREA FOR GENERAL ROUTINES                    
         STCM  RF,15,SYSDUMMY      END OF SYSTEM BASE                           
         MVI   SYSTEM,C'P'         PRINT                                        
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 1000 BYTES                       
         MVC   GETUSER,VALAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'25'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'33'                                                  
         MVI   PBFINAN,0                                                        
         MVC   SYSDIR,=C'PRTDIR  '                                              
         MVC   SYSFIL,=C'PRTFILE '                                              
         MVI   GETMSYS,25          USES GETMSG FOR SYSTEM 25                    
         L     R1,=AL4(LENWORK)       WE TOOK XXXXX BYTES IN NMOD  L02          
         LA    R1,4000(R1)                                         L02          
         ST    R1,LWORK                                            L02          
         MVC   RCPROG(2),=C'PP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9040500'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACTS                                                       
         ST    R1,ARECACT            RECORD/ACTION DIRECTORY                    
         LH    R1,=Y(STARTSV-SYSD)                                              
         LA    R1,SYSD(R1)                                                      
         ST    R1,ASTARTSV                                                      
*                                                                               
         LH    R1,=Y(VLPARMS-SYSD)                                              
         LA    R1,SYSD(R1)                                                      
         ST    R1,AVLPARMS         DDVAL PARAMETERS                             
*                                                                               
         LH    R1,=Y(VLTABC-SYSD)                                               
         LA    R1,SYSD(R1)                                                      
         ST    R1,AVLTAB           DDVAL TABLE AREA                             
*                                                                               
         XC    DUB,DUB             PARAMETERS FOR GETPROF                       
*                                                                               
         MVI   DUB,C'P'            SET SYSTEM                                   
         MVC   DUB+2(2),=C'W0'     W0 - WRITER PROFILE                          
         MVC   DUB+4(2),PBQAGY     AGENCY                                       
*                                                                               
         GOTO1 GETPROF,DMCB,(X'C0',DUB),PBQPRFW0,DATAMGR                        
*                                                                               
         CLI   PBQPRFW0+5,C'Y'     SKIP IF SECURITY NOT BEING USED              
         BNE   SYSISECX                                                         
*                                                                               
         TM    AUTH,X'80'          IF NOT ALLOWED TO CHANGE FORMATS             
         BO    *+12                                                             
         OI    GENSTAT4,NODELLST      STOP DELETING FROM LIST                   
         OI    GENSTAT5,NOCHGLST      STOP CHANGING FROM LIST                   
*                                                                               
SYSISECX DS    0H                                                               
*                                                                               
         OI    GENSTAT4,CONFDEL    FORCE CONFIRMATION OF DELETES                
*                                                                               
SYSINTX  CR    RB,RB               NORMAL EXIT                     L05          
         B     SYSINITX                                            L05          
         SPACE 2                                                   L05          
BADREC   XC    CONHEAD,CONHEAD                                     L05          
         MVC   CONHEAD(14),=C'INVALID RECORD'                      L05          
         OI    CONHEADH+6,X'80'                                    L05          
         LTR   RB,RB                                              L05           
*                                                                  L05          
SYSINITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
WRITER   CLC   CONREC(0),=C'WRITER'                               L05           
         EJECT                                                     L05          
*                                                                               
* ROUTINE EXECUTED BEFORE GOING TO GENCON                                       
*                                                                               
CHKTOP   LR    R0,RE                                                            
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
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
*        INTERFACE FOR FOOTING ROUTINE BECAUSE IT COMES FROM SPOOL              
*          NOT DRIVER. RF ==> FOOTING                                           
*                                                                               
FOOTING  DS    0H                                                               
*                                                                               
         LM    R0,RC,FOOTREGS-FOOTING(RF)    RESTORE REGISTERS                  
*                                                                               
         GOTO1 VGENFT              USE NORMAL LINKAGE                           
*                                                                               
         XIT1                                                                   
*                                                                               
FOOTREGS DS    16F                 REGISTER SAVEAREA                            
*                                                                               
         EJECT                                                                  
*                                                                               
CORETAB  DS    0C                  TABLE OF CORE-RESIDENT MODULES               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QDRONE)                                                      
         DC    AL1(QPRNTIO)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QPRVAL)                                                      
         DC    AL1(QPRWRIGN)                                                    
         DC    AL1(QPRHELP)                                                     
         DC    AL1(QMINIO)                                                      
         DC    AL1(QPUBVAL)                                                     
         DC    AL1(QPUBEDIT)                                                    
         DC    AL1(QPGETADR)                                                    
         DC    AL1(QSOFDAT)                                                     
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
SYSVCON  DS    0A                  ADDRESSES OF LINKED ROUTINES                 
         DC    A(0)                SPARE                                        
         DC    V(COVAIL)                                                        
         DC    V(DUMMY)                                                         
         DC    A(FOOTING)                                                       
         EJECT                                                                  
         LTORG                                                                  
* TABLES OF RECORDS ACTIONS AND COMBINATIONS                                    
*                                                                               
RECACTS  DS    0D                                                               
*                                                                               
* X'01' ENTRIES ARE AVAILABLE REPORTS                                           
*                                                                               
* CL8 EXPANDED REPORT NAME                                                      
* CL1 REPORT NUMBER (01 IS RESERVED)                                            
* CL1 PHASE NUMBER FOR DATA DICTIONARY - LEAVE AS ZERO FOR NOW                  
* CL1 PHASE NUMBER FOR HELP SCREEN - LEAVE AS ZERO FOR NOW                      
*                                                                               
         DC    X'04',C'WRITER  ',AL1(01),X'0000'                                
         DC    X'04',C'TEST    ',AL1(02),X'0000'                                
         DC    X'04',C'PREBILL ',AL1(04),X'0000'                                
         DC    X'04',C'GFTAPE  ',AL1(05),X'0000'                  L05           
         DC    X'04',C'TRANSMIT',AL1(12),X'0000'                  L07           
         DC    X'04',C'CFAFTER ',AL1(13),X'0000'                  L07           
         DC    X'04',C'KRTAPE  ',AL1(14),X'0000'                                
         DC    X'04',C'IDESK   ',AL1(15),X'0000'                                
         DC    X'04',C'SCJE    ',AL1(16),X'0000'                                
         DC    X'04',C'SCJED   ',AL1(17),X'0000'                                
         SPACE 3                                                                
* X'02' ENTRIES ARE AVAILABLE ACTIONS                                           
*                                                                               
* CL8 EXPANDED ACTION NAME                                                      
* CL1 ACTION NUMBER                                                             
* CL1 ACTION EQUATE                                                             
* CL1 SPARE                                                                     
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
*                                                                               
*               X'03' ENTRIES ARE OK REC/ACT COMBOS                             
*                |       CL1 RECORD NUMBER                                      
*                |       |  CL1 ACTION EQUATE                                   
*                |       |  |     CL1 PHASE NUMBER FOR SCREEN                   
*                |       |  |     | CL1 PHASE NUMBER FOR EDIT                   
*                |       |  |     | | CL1 PHASE NUMBER FOR SPECS                
*                |       |  |     | | | CL1 PHASE NUMBER FOR REPORT             
*                |       |  |     | | | | CL1 WHEN OK BITS 80=SCREEN            
*                |       |  |     | | | | | 40=NOW 20=SOON 10=OV 08=DDS         
*                |       |  |     | | | | |     CL2 CODE FOR REPORTS            
*                |       |  |     | | | | |     | CL2 CODE FOR EOD-             
*                |       |  |     | | | | |     | |         HANDLING            
*                |       |  |     SC| SP| AC    | |                             
*                |       |  |     | OV| RP|     | |                             
         DC    X'03',AL1(01,01),X'F101000138',C'WRWR'  WRITER   REPORT          
         DC    X'03',AL1(01,02),X'F101000138',C'WRWR'  WRITER   REPORT          
         DC    X'03',AL1(01,03),X'F101000138',C'WRWR'  WRITER   REPORT          
         DC    X'03',AL1(01,04),X'F101000138',C'WRWR'  WRITER   REPORT          
         DC    X'03',AL1(01,05),X'F101000138',C'WRWR'  WRITER   REPORT          
         DC    X'03',AL1(01,06),X'F101000138',C'WRWR'  WRITER   REPORT          
         DC    X'03',AL1(01,10),X'F101000138',C'WRWR'  WRITER   REPORT          
         DC    X'03',AL1(01,12),X'F101000138',C'WRWR'  WRITER   REPORT          
*                                                                               
         DC    X'03',AL1(02,01),X'F101000138',C'WRWT'  TEST REPORT L01          
         DC    X'03',AL1(02,02),X'F101000138',C'WRWT'  TEST REPORT L01          
         DC    X'03',AL1(02,03),X'F101000138',C'WRWT'  TEST REPORT L01          
         DC    X'03',AL1(02,04),X'F101000138',C'WRWT'  TEST REPORT L01          
         DC    X'03',AL1(02,05),X'F101000138',C'WRWT'  TEST REPORT L01          
         DC    X'03',AL1(02,06),X'F101000138',C'WRWT'  TEST REPORT L01          
         DC    X'03',AL1(02,10),X'F101000138',C'WRWT'  TEST REPORT L01          
         DC    X'03',AL1(02,12),X'F101000138',C'WRWT'  TEST REPORT L01          
*                                                                               
         DC    X'03',AL1(04,01),X'F101000138',C'WRW1'  WRITER   REPORT          
         DC    X'03',AL1(04,02),X'F101000138',C'WRW1'  WRITER   REPORT          
         DC    X'03',AL1(04,03),X'F101000138',C'WRW1'  WRITER   REPORT          
         DC    X'03',AL1(04,04),X'F101000138',C'WRW1'  WRITER   REPORT          
         DC    X'03',AL1(04,05),X'F101000138',C'WRW1'  WRITER   REPORT          
         DC    X'03',AL1(04,06),X'F101000138',C'WRW1'  WRITER   REPORT          
         DC    X'03',AL1(04,10),X'F101000138',C'WRW1'  WRITER   REPORT          
         DC    X'03',AL1(04,12),X'F101000138',C'WRW1'  WRITER   REPORT          
*                                                                               
         DC    X'03',AL1(05,01),X'E901000138',C'GFGF'  GF TAPE     L05          
         DC    X'03',AL1(05,02),X'E901000138',C'GFGF'  GF TAPE     L05          
         DC    X'03',AL1(05,03),X'E901000138',C'GFGF'  GF TAPE     L05          
         DC    X'03',AL1(05,04),X'E901000138',C'GFGF'  GF TAPE     L05          
         DC    X'03',AL1(05,05),X'E901000138',C'GFGF'  GF TAPE     L05          
         DC    X'03',AL1(05,06),X'E901000138',C'GFGF'  GF TAPE     L05          
         DC    X'03',AL1(05,10),X'E901000138',C'GFGF'  GF TAPE     L05          
         DC    X'03',AL1(05,12),X'E901000138',C'GFGF'  GF TAPE     L05          
*                                                                               
         DC    X'03',AL1(06,01),X'D901000138',C'BTBT'  BT TAPE     L06          
         DC    X'03',AL1(06,02),X'D901000138',C'BTBT'  BT TAPE     L06          
         DC    X'03',AL1(06,03),X'D901000138',C'BTBT'  BT TAPE     L06          
         DC    X'03',AL1(06,04),X'D901000138',C'BTBT'  BT TAPE     L06          
         DC    X'03',AL1(06,05),X'D901000138',C'BTBT'  BT TAPE     L06          
         DC    X'03',AL1(06,06),X'D901000138',C'BTBT'  BT TAPE     L06          
         DC    X'03',AL1(06,10),X'D901000138',C'BTBT'  BT TAPE     L06          
         DC    X'03',AL1(06,12),X'D901000138',C'BTBT'  BT TAPE     L06          
*                                                                               
         DC    X'03',AL1(07,01),X'E701000138',C'PGPG'  PG TAPE     L07          
         DC    X'03',AL1(07,02),X'E701000138',C'PGPG'  PG TAPE     L07          
         DC    X'03',AL1(07,03),X'E701000138',C'PGPG'  PG TAPE     L07          
         DC    X'03',AL1(07,04),X'E701000138',C'PGPG'  PG TAPE     L07          
         DC    X'03',AL1(07,05),X'E701000138',C'PGPG'  PG TAPE     L07          
         DC    X'03',AL1(07,06),X'E701000138',C'PGPG'  PG TAPE     L07          
         DC    X'03',AL1(07,10),X'E701000138',C'PGPG'  PG TAPE     L07          
         DC    X'03',AL1(07,12),X'E701000138',C'PGPG'  PG TAPE     L07          
*                                                                               
         DC    X'03',AL1(08,01),X'D701000138',C'PTPT'  PM TAPE     L07          
         DC    X'03',AL1(08,02),X'D701000138',C'PTPT'  PM TAPE     L07          
         DC    X'03',AL1(08,03),X'D701000138',C'PTPT'  PM TAPE     L07          
         DC    X'03',AL1(08,04),X'D701000138',C'PTPT'  PM TAPE     L07          
         DC    X'03',AL1(08,05),X'D701000138',C'PTPT'  PM TAPE     L07          
         DC    X'03',AL1(08,06),X'D701000138',C'PTPT'  PM TAPE     L07          
         DC    X'03',AL1(08,10),X'D701000138',C'PTPT'  PM TAPE     L07          
         DC    X'03',AL1(08,12),X'D701000138',C'PTPT'  PM TAPE     L07          
*                                                                               
         DC    X'03',AL1(09,01),X'E801000138',C'PGPG'  PG EDIT     L07          
         DC    X'03',AL1(09,02),X'E801000138',C'PGPG'  PG EDIT     L07          
         DC    X'03',AL1(09,03),X'E801000138',C'PGPG'  PG EDIT     L07          
         DC    X'03',AL1(09,04),X'E801000138',C'PGPG'  PG EDIT     L07          
         DC    X'03',AL1(09,05),X'E801000138',C'PGPG'  PG EDIT     L07          
         DC    X'03',AL1(09,06),X'E801000138',C'PGPG'  PG EDIT     L07          
         DC    X'03',AL1(09,10),X'E801000138',C'PGPG'  PG EDIT     L07          
         DC    X'03',AL1(09,12),X'E801000138',C'PGPG'  PG EDIT     L07          
*                                                                               
         DC    X'03',AL1(10,01),X'F101000138',C'BIBI'  BILLING INTERFAC         
         DC    X'03',AL1(10,02),X'F101000138',C'BIBI'  BILLING INTERFAC         
         DC    X'03',AL1(10,03),X'F101000138',C'BIBI'  BILLING INTERFAC         
         DC    X'03',AL1(10,04),X'F101000138',C'BIBI'  BILLING INTERFAC         
         DC    X'03',AL1(10,05),X'F101000138',C'BIBI'  BILLING INTERFAC         
         DC    X'03',AL1(10,06),X'F101000138',C'BIBI'  BILLING INTERFAC         
         DC    X'03',AL1(10,10),X'F101000138',C'BIBI'  BILLING INTERFAC         
         DC    X'03',AL1(10,12),X'F101000138',C'BIBI'  BILLING INTERFAC         
*                                                                               
         DC    X'03',AL1(12,01),X'F101000138',C'TRTR'  TRANSMIT                 
         DC    X'03',AL1(12,02),X'F101000138',C'TRTR'  TRANSMIT                 
         DC    X'03',AL1(12,03),X'F101000138',C'TRTR'  TRANSMIT                 
         DC    X'03',AL1(12,04),X'F101000138',C'TRTR'  TRANSMIT                 
         DC    X'03',AL1(12,05),X'F101000138',C'TRTR'  TRANSMIT                 
         DC    X'03',AL1(12,06),X'F101000138',C'TRTR'  TRANSMIT                 
         DC    X'03',AL1(12,10),X'F101000138',C'TRTR'  TRANSMIT                 
         DC    X'03',AL1(12,12),X'F101000138',C'TRTR'  TRANSMIT                 
*                                                                               
         DC    X'03',AL1(13,01),X'F101000118',C'CFCF'  CASHFLOW AFTER           
         DC    X'03',AL1(13,02),X'F101000118',C'CFCF'  CASHFLOW AFTER           
         DC    X'03',AL1(13,03),X'F101000118',C'CFCF'  CASHFLOW AFTER           
         DC    X'03',AL1(13,04),X'F101000118',C'CFCF'  CASHFLOW AFTER           
         DC    X'03',AL1(13,05),X'F101000118',C'CFCF'  CASHFLOW AFTER           
         DC    X'03',AL1(13,06),X'F101000118',C'CFCF'  CASHFLOW AFTER           
         DC    X'03',AL1(13,10),X'F101000118',C'CFCF'  CASHFLOW AFTER           
         DC    X'03',AL1(13,12),X'F101000118',C'CFCF'  CASHFLOW AFTER           
*                                                                               
         DC    X'03',AL1(14,01),X'E901000138',C'KRKR'  KR TAPE     L05          
         DC    X'03',AL1(14,02),X'E901000138',C'KRKR'  KR TAPE     L05          
         DC    X'03',AL1(14,03),X'E901000138',C'KRKR'  KR TAPE     L05          
         DC    X'03',AL1(14,04),X'E901000138',C'KRKR'  KR TAPE     L05          
         DC    X'03',AL1(14,05),X'E901000138',C'KRKR'  KR TAPE     L05          
         DC    X'03',AL1(14,06),X'E901000138',C'KRKR'  KR TAPE     L05          
         DC    X'03',AL1(14,10),X'E901000138',C'KRKR'  KR TAPE     L05          
         DC    X'03',AL1(14,12),X'E901000138',C'KRKR'  KR TAPE     L05          
*                                                                               
         DC    X'03',AL1(15,01),X'F101000138',C'IXWR'  IDESK EXTRACT            
         DC    X'03',AL1(15,02),X'F101000138',C'IXWR'  IDESK EXTRACT            
         DC    X'03',AL1(15,03),X'F101000138',C'IXWR'  IDESK EXTRACT            
         DC    X'03',AL1(15,04),X'F101000138',C'IXWR'  IDESK EXTRACT            
         DC    X'03',AL1(15,05),X'F101000138',C'IXWR'  IDESK EXTRACT            
         DC    X'03',AL1(15,06),X'F101000138',C'IXWR'  IDESK EXTRACT            
         DC    X'03',AL1(15,10),X'F101000138',C'IXWR'  IDESK EXTRACT            
         DC    X'03',AL1(15,12),X'F101000138',C'IXWR'  IDESK EXTRACT            
*                                                                               
         DC    X'03',AL1(16,01),X'EA01000138',C'WRWR'  SCJE     ADD             
         DC    X'03',AL1(16,02),X'EA01000138',C'WRWR'  SCJE     CHANGE          
         DC    X'03',AL1(16,03),X'EA01000138',C'WRWR'  SCJE     DISPLAY         
         DC    X'03',AL1(16,04),X'EA01000138',C'WRWR'  SCJE     DELETE          
         DC    X'03',AL1(16,05),X'EA01000138',C'WRWR'  SCJE     SELECT          
         DC    X'03',AL1(16,06),X'EA01000138',C'WRWR'  SCJE     RESTORE         
         DC    X'03',AL1(16,10),X'EA01000138',C'WRWR'  SCJE     LIST            
         DC    X'03',AL1(16,12),X'EA01000138',C'WRWR'  SCJE     REPORT          
*                                                                               
         DC    X'03',AL1(17,01),X'EA01000138',C'WRWR'  SCJED    ADD             
         DC    X'03',AL1(17,02),X'EA01000138',C'WRWR'  SCJED    CHANGE          
         DC    X'03',AL1(17,03),X'EA01000138',C'WRWR'  SCJED    DISPLAY         
         DC    X'03',AL1(17,04),X'EA01000138',C'WRWR'  SCJED    DELETE          
         DC    X'03',AL1(17,05),X'EA01000138',C'WRWR'  SCJED    SELECT          
         DC    X'03',AL1(17,06),X'EA01000138',C'WRWR'  SCJED    RESTORE         
         DC    X'03',AL1(17,10),X'EA01000138',C'WRWR'  SCJED    LIST            
         DC    X'03',AL1(17,12),X'EA01000138',C'WRWR'  SCJED    REPORT          
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
*              PHASE USED UP BY SYSTEM SO FAR                                   
         SPACE 1                                                                
*              X0 X1 X2 X3 X4 X5 X6 X7 X8 X9 XA XB XC XD XE XF                  
*   PHASES                                                                      
*        0X     Y  Y  Y  Y     Y                                                
*        1X                 Y                 Y                                 
*        5X                 Y                 Y                                 
*        EX                                   Y                                 
*        FX        Y  Y  Y                                   Y  EDIT            
*                1234567890       123456789ABCDEF0123456          L01           
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        INTERFACE TO GENCON                                          *         
*           SEPARATE NMOD ONLY TO GET MORE WORKING STORAGE            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GOGENCON NMOD1 BUFFWRKL,**#GEN,CLEAR=YES                                        
*                                                                               
         LR    RF,RC               SAVE WORKING STORAGE ADDRESS                 
         LR    RC,R0               RESTORE OLD WORK AREA ADDRESS                
         USING BUFFWRKD,RF         ESTABLISH BUFFER WORKAREA                    
*                                                                               
         LH    RE,=Y(FLTTABBF-BUFFWRKD)   FILTER TABLE DISPLACEMENT             
         AR    RE,RF               FILTER TABLE ADDRESS                         
         ST    RE,AFLTTAB          SAVE COLUMN FILTER AREA ADDRESS              
         SH    RE,=H'8'                                                         
         MVC   0(8,RE),=CL8'**FLTTAB'    MARK WORK AREA                         
*                                                                               
         DROP  RF                                                               
*                                                                               
         GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON                                
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
         TITLE   'PRSYSDRIVE DSECTS / STORAGE'                                  
       ++INCLUDE PRWRIWORKD                                                     
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*        INCLUDE DDSPOOLD                                                       
*        INCLUDE DDSPLWORKD                                                     
*        INCLUDE PRWRIFFD                                                       
*        INCLUDE DDGENTWA                                                       
*        INCLUDE CTGENFILE                                                      
*        INCLUDE CTGENADVD                                                      
*        INCLUDE FAFACTS                                                        
*        INCLUDE FATIOB                                                         
*        INCLUDE DDCOMFACS                                                      
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE DDBIGBOX                                                       
*        INCLUDE DDWIDED                                                        
*        INCLUDE DDOFFICED                                                      
*        INCLUDE DRONEBLKHD                                                     
*        INCLUDE DDCOREQUS                                                      
*        INCLUDE DDMASTD                                                        
*        INCLUDE DDPERVALD                                                      
*        INCLUDE PRVALTABD                                                      
*        INCLUDE PRGLOBEQUS                                                     
*        INCLUDE FASECRETD                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE PRWRIFFD                                                       
*****    ORG   CONTAGH                                                          
*****  ++INCLUDE PRWRIF1D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENADVD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDWIDED                                                        
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DRONEBLKHD                                                     
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE PRVALTABD                                                      
       ++INCLUDE PRGLOBEQUS                                                     
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
         PRINT ON                                                               
T405FFD  DSECT                                                                  
         ORG   CONHEADH-64+X'3000'                                              
HELPSAVE DS    XL512               HELP SAVEAREA                                
SECBLK   DS    XL1024              FASECRET BLOCK                               
*                                                                               
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
       ++INCLUDE PRDATELSTD                                                     
WEELLENT EQU   *-WEEKLIST                                                       
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'144PRWRI00   04/02/12'                                      
         END                                                                    
