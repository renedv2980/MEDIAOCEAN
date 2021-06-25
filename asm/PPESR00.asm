*          DATA SET PPESR00    AT LEVEL 024 AS OF 10/05/11                      
*PHASE T42000A                                                                  
*INCLUDE SRCHCALL                                                               
*INCLUDE KHDUMMY                                                                
*        TITLE 'PPESR00 - PRINT ENHANCED SR - CONTROLLER'                       
         TITLE 'PPESR00 - CHANGE LOG'                                           
***********************************************************************         
*                                                                     *         
*        CHANGE LOG                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
* SMYE  02/05/08  FIX GLOBBER CALL CODE AT LABEL GLBCHK (SEE *NOP*)             
*                                                                               
* BOBY     06/05  BIG BANG                                                      
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - INIT'                  
***********************************************************************         
*                                                                     *         
*        PPESR00 - PRINT ENHANCED SR - CONTROLLER                     *         
*                                                                     *         
*              INITIALISATION                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PPESR00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,**SR00**,RR=R2,CLEAR=YES                                 
*                                                                               
         LR    R9,R1               SAVE INCOMING PARAMETERS POINTER             
*                                                                               
         LR    R8,RC               ESTABLISH SPOOL  WORKING STORAGE             
         USING SPOOLD,R8                                                        
*                                                                               
         MVI   SPACES,C' '         INIT SPACES FIELD                            
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         LA    RC,SPOOLEND         ESTABLISH GENCON WORKING STORAGE             
         USING GEND,RC                                                          
*                                                                               
         ST    R9,SYSPARMS         SAVE INCOMING PARAMETERS POINTER             
*                                                                               
         LA    R9,IO                                                            
         AHI   R9,LENIOAS          GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9             ESTABLISH SYTEM WORKING STORAGE              
         ST    R9,ASYSD            SAVE WORKING STORAGE POINTER                 
*                                                                               
         ST    R2,RELO             SAVE RELOCATION FACTOR                       
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD            CURRENT SPOT IN D CHAIN                      
*                                                                               
         L     RA,SYSPARMS         GET ATWA                                     
         L     RA,4(RA)            ESTABLISH SCREEN                             
         USING CONHEADH-64,RA                                                   
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE SYSTEM VARIABLES                  
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - PFKEYS'                
***********************************************************************         
*                                                                     *         
*        PPESR00 - PRINT ENHANCED SR - CONTROLLER                     *         
*                                                                     *         
*              HANDLE PFKEYS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   DS    0H                                                               
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,0(RF)            A(TIOB)                                      
         USING TIOBD,RF            ESTABLISH TIOB                               
*                                                                               
         CLI   TIOBAID,12          LOOK FOR PFKEY 12 HIT                        
         BE    *+8                 SKIP SWAP IF NOT FOUND                       
         CLI   TIOBAID,24          LOOK FOR PFKEY 24 HIT                        
         BNE   PFKEYSX             SKIP SWAP IF NOT FOUND                       
*                                                                               
         NI    GENSTAT2,X'FF'-RETEQSEL  STOP SELECT RE-DISPLAY                  
*                                                                               
         DROP  RF                                                               
*                                                                               
PFKEYSX  DS    0H                                                               
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - GLBCHK'                
***********************************************************************         
*                                                                     *         
*        PPESR00 - PRINT ENHANCED SR - CONTROLLER                     *         
*                                                                     *         
*              CHECK IF THIS IS A GLOBBER CALL                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GLBCHK   DS    0H                                                               
*                                                                               
         MVI   TRANSSW,0           INIT TRANSFER SWITCH                         
*                                                                               
         OC    VGLOBBER,VGLOBBER   SKIP IF NO GLOBBER ADDRESS                   
         BZ    GLBCHKX                                                          
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL  GET XCTL ELM             
*                                                                               
         CLI   DMCB+8,GLEGNF       SKIP IF NO ELM FOUND                         
         BE    GLBCHKX                                                          
*                                                                               
         CLI   DMCB+8,0            NO OTHER ERRORS TOLERATED                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,WORK             ESTABLISH XCTL ELM                           
         USING GLVXFRSY,R1                                                      
*                                                                               
         CLC   =C'PRI',GLVXTOSY    MAKE SURE PRINT/ESR WANTED                   
         BNE   GLBCHKX                                                          
         CLC   =C'ESR',GLVXTOPR                                                 
         BNE   GLBCHKX                                                          
*                                                                               
         DROP  R1                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*            BELOW GLOBBER CALLS ARE HANDLED BY LKIO                            
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*NOP*    GOTO1 (RF),(R1),=C'DELE'  DELETE TRANSFER ELM                          
*                                                                               
         MVI   TRANSSW,C'Y'        INDICATE WE ARE IN MIDST OF TRANSFER         
*                                                                               
*NOP*    GOTO1 VGLOBBER,DMCB,=C'GETF',CONRECH,,GLVXREC  GET RECORD              
*NOP*    GOTO1 (RF),(R1),=C'DELE'  DELETE RECORD                                
*                                                                               
*NOP*    GOTO1 VGLOBBER,DMCB,=C'GETF',CONACTH,,GLVXACT  GET ACTION              
*NOP*    GOTO1 (RF),(R1),=C'DELE'  DELETE ACTION                                
*                                                                               
*NOP*    GOTO1 VGLOBBER,DMCB,=C'GETF',CONKEYH,,GLVPRKEY GET KEY                 
*NOP*    GOTO1 (RF),(R1),=C'DELE'  DELETE KEY                                   
*                                                                               
*        CODE TO HANDLE GLOBBER CALL                                            
*                                                                               
GLBCHKX  DS    0H                                                               
*                                                                               
         BAS   RE,GOGENCON                                                      
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - GOGENCON'              
***********************************************************************         
*                                                                     *         
*        PPESR00 - PRINT ENHANCED SR - CONTROLLER                     *         
*                                                                     *         
*              START UP GENCON                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GOGENCON NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         BAS   RE,SETRD            RESET RETURN FROM GENCON                     
*                                                                               
GOGCONLP DS    0H                                                               
*                                                                               
         MVI   GOAGAIN,C' '        CLEAR GO AGAIN SWITCH                        
*                                                                               
         CLI   DDLNKSW,C'Y'        IF THIS A LINK CALL                          
         BNE   *+8                                                              
         OI    GENSTAT4,SVTWA0FF      SKIP WORKING STORAGE RESTORE              
*                                                                               
         GOTO1 VGENCON,DMCB,(R8)   GOTO GENERAL CONTROLLER                      
*                                                                               
GOGCONCN DS    0H                                                               
*                                                                               
         CLI   GOAGAIN,C'Y'        NEED TO RETURN TO GENCON?                    
         BE    GOGCONLP                                                         
*                                                                               
         CLI   DDLNKSW,C'Y'        CONTINUE IF LINK CALL                        
         BNE   GOGCONDN                                                         
*                                                                               
         CLI   DDLNKEOF,C'Y'       AND NOT END OF FILE                          
         BE    GOGCONDN                                                         
*                                                                               
         CLI   DDCOMSW,C'Y'        IF COMMENTS STILL TO DO                      
         BNE   *+8                                                              
         MVI   DDCOMSW,C'C'           SET TO DO COMMENTS NOW                    
*                                                                               
         MVI   TWASCR,X'FF'        MAKE GENCON START OVER                       
*                                                                               
         B     GOGCONLP                                                         
*                                                                               
GOGCONDN DS    0H                                                               
*                                                                               
GOGCONX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        RESET SYSRD SO GENCON RETURNS TO PROGRAM                               
*              AND NOT TO MONITOR                                               
*                                                                               
SETRD    NTR1  LABEL=*                                                          
*                                                                               
         ST    RD,SYSRD            RESET RD FOR GENCON                          
*                                                                               
SETRDX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - SYSINIT'               
***********************************************************************         
*                                                                     *         
*        PPESR00 - PRINT ENHANCED SR - CONTROLLER                     *         
*                                                                     *         
*              INITIALIZE SYSTEM DEPENDENT VARIABLES                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSINIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
*        SET UP ADDRESSES TO INCLUDED MODULES                                   
*                                                                               
         LA    R2,SYSV             ADDRS OF  INCLUDED ROUTINES                  
         LA    R3,SYSVCON          VCONS OF  INCLUDED ROUTINES                  
         LA    R4,NVTYPES          NUMBER OF INCLUDED ROUTINES                  
*                                                                               
SINVCONL DS    0H                                                               
*                                                                               
         L     R1,0(R3)            GET V(INCLUDED ROUTINE)                      
         A     R1,RELO             RELOCATE ADDRESS                             
         ST    R1,0(R2)            SAVE ADDRESS                                 
*                                                                               
SINVCONC DS    0H                                                               
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS STORAGE                 
         LA    R3,4(R3)            BUMP TO NEXT V-CON                           
         BCT   R4,SINVCONC         LOOP THROUGH LISTS                           
*                                                                               
SINVCOND DS    0H                                                               
*                                                                               
*        SET UP LIST OF COMMON ROUTINES                                         
*              ALL ARE LOCATED IN THIS MODULE                                   
*                                                                               
         LA    R2,VCOMMON          COMMON ENTRY POINT                           
         SR    R3,R3               INIT ROUTINE ID                              
         LA    R4,SYSCOMM          START OF ADDRESS AVEAREA                     
         LA    R5,VCOUNT           SET NUMBER OF ROUTINES                       
*                                                                               
SINCOMLP DS    0H                                                               
*                                                                               
         ST    R2,0(R4)            SET ENTRY POINT                              
         STC   R3,0(R4)            SET ROUTINE ID                               
*                                                                               
SINCOMCN DS    0H                                                               
*                                                                               
         LA    R3,4(R3)            BUMP ROUTINE ID                              
         LA    R4,4(R4)            BUMP TO NEXT ADDRESS SAVEAREA                
         BCT   R5,SINCOMLP         NEXT ROUTINER5,                              
*                                                                               
SINCOMDN DS    0H                                                               
*                                                                               
*        LOAD CORE-RESIDENT MODULES                                             
*                                                                               
         L     R1,SYSPARMS         RETRIEVE USCAN ADDRESS                       
         L     R1,8(R1)                                                         
         MVC   VUSCAN,44(R1)                                                    
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R2,CORETAB          CORE-RESIDENT IDS                            
         LA    R0,CORES            NUMBER OF CORE-RESIDENT PHASES               
         LA    R4,COREFACS         ADDRESS SAVEAREA                             
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)           A(COMFACS)                                   
         ST    R1,ACOMFACS         SAVE ADDRESS                                 
         USING COMFACSD,R1                                                      
         MVC   VGLOBBER,CGLOBBER   SAVE GLOBBER ADDRESS                         
         MVC   VRECUP,CRECUP       SAVE RECUP   ADDRESS                         
         MVC   VLINKIO,CLINKIO     SAVE LINKIO  ADDRESS                         
*                                                                               
         L     RF,CCALLOV          A(CALLOV)                                    
         DROP  R1                                                               
*                                                                               
         LA    R1,DMCB             INIT PHASE NAME AREA                         
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SINCORLP DS    0H                                                               
*                                                                               
         MVC   DMCB+7(1),0(R2)     SET PHASE ID                                 
*                                                                               
         GOTO1 (RF),(R1),0         LOAD PHASE                                   
*                                                                               
         CLI   DMCB+4,X'FF'        NO ERRORS TOLERATED                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
*                                                                               
SINCORCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SINCORLP                                                      
*                                                                               
SINCORDN DS    0H                                                               
*                                                                               
*****    B     SINLNKDN                                                         
*                                                                               
         XC    DMCB,DMCB                                                        
*                                                                               
         MVI   DMCB,X'05'          SET OVERLAY NUMBER                           
*                                                                               
         LA    R1,DMCB             LOAD A(T42005) - LINKIO RTNS                 
*                                                                               
         GOTO1 (RF),(R1),,0        LOAD PHASE                                   
*                                                                               
         CLI   DMCB+4,X'FF'        NO ERRORS TOLERATED                          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   VESR05,DMCB         SAVE MODULE ADDRESS                          
*                                                                               
*        SET UP LIST OF LINKIO ROUTINES                                         
*              ALL ARE LOCATED IN T42005                                        
*                                                                               
         L     R2,VESR05           COMMON ENTRY POINT                           
         SR    R3,R3               INIT ROUTINE ID                              
         LA    R4,LNKCONS          START OF ADDRESS SAVEAREA                    
         LA    R5,VLNKCT           SET NUMBER OF ROUTINES                       
*                                                                               
SINLNKLP DS    0H                                                               
*                                                                               
         ST    R2,0(R4)            SET ENTRY POINT                              
         STC   R3,0(R4)            SET ROUTINE ID                               
*                                                                               
SINLNKCN DS    0H                                                               
*                                                                               
         LA    R3,4(R3)            BUMP ROUTINE ID                              
         LA    R4,4(R4)            BUMP TO NEXT ADDRESS SAVEAREA                
         BCT   R5,SINLNKLP         NEXT ROUTINER5,                              
*                                                                               
SINLNKDN DS    0H                                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        SET SYSTEM DEPENDENT VALUES                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         MVI   NTWA,X'01'          TELL GENCON CON I NEED 1 TWA                 
*                                                                               
         MVI   SYSTEM,C'P'         PRINT                                        
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4096 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'25'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'33'     USUALLY PRINT FILE                           
         MVC   SYSFIL,=C'PRTFILE '                                              
         MVC   SYSDIR,=C'PRTDIR  '                                              
         MVI   GETMSYS,4           USES GETMSG FOR SYSTEM PRINT                 
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'PP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9042000'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         LA    R0,SVSTART          SET SAVED STORAGE START ADDR                 
         ST    R0,ASTARTSV                                                      
*                                                                               
*        SET GENCON OPTIONS                                                     
*                                                                               
         OI    GENSTAT1,USKYMRG    USE KEYMERGE FOR LIST SCREENS                
         OI    GENSTAT1,RDUPAPPL   APPLICATION CONTROLS READ FOR UPDATE         
         OI    GENSTAT2,DISTHSPG   STAY ON SAME LIST PAGE                       
*                                                                               
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEMENTS                         
         MVI   IOOPT,C'Y'          OVERLAY DOES I/O                             
*                                                                               
         MVC   QAGY,TWAAGY         SAVE AGENCY APLPHA                           
*                                                                               
         BRAS  RE,PID              GET PID                                      
*                                                                               
         BRAS  RE,CHKPFK           CHECK FOR PF KEY HIT                         
*                                                                               
SYSINITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - CONSTANTS'             
***********************************************************************         
*                                                                     *         
*        CONSTANTS TABLES, ETC                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSVCON  DS    0F                  INCLUDED MODULES                             
         DC    V(DUMMY)            END OF DUMP AREA                             
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4       NUMBER OF INCLUDED MODULES                   
*                                                                               
CORETAB  DS    0X                  CORE-RESIDENT PHASES                         
         DC    AL1(QGENCON)        GENCON                                       
         DC    AL1(QMINIO)         MINIO                                        
         DC    AL1(QLINUP)         LINE UP                                      
         DC    AL1(QOFFICER)       OFFICER                                      
         DC    AL1(QPRHELP)        PRINT HELP                                   
         DC    AL1(QPRVAL)         DATA VALIDATOR AND TABLES                    
         DC    AL1(QGETINS)        INSERTION EXPANDER                           
         DC    AL1(QPUBVAL)        PUB VALIDATION                               
         DC    AL1(QPUBEDIT)       PUB CODE DISPLAY                             
CORES    EQU   (*-CORETAB)         NUMBER OF PHASES                             
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - RECACT'                
***********************************************************************         
*                                                                     *         
*              DIRECTORY OF RECORDS AND ACTIONS                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECACT   DS    0D                                                               
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                                                               
         DC    X'01',C'ESR     ',AL1(02),X'00C2'                                
         DC    X'01',C'FAX     ',AL1(03),X'00C2'                                
         DC    X'01',C'STATUS  ',AL1(04),X'00C2'                                
         DC    X'01',C'ACTIVITY',AL1(05),X'00C2'                                
         SPACE 3                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,02,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,03,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,04,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,05,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,06,00)                                  
         DC    X'02',C'MAINT   ',AL1(07,07,00)                                  
         DC    X'02',C'ABDELETE',AL1(09,09,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'ABADD   ',AL1(13,13,00)                                  
         DC    X'02',C'SEND    ',AL1(14,14,00)                                  
         DC    X'02',C'REPLY   ',AL1(15,15,00)                                  
         DC    X'02',C'HISTORY ',AL1(16,16,00)                                  
         DC    X'02',C'STATUS  ',AL1(17,17,00)                                  
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
         DC    X'03',AL1(02,01),X'FD100010C0',C'    '  ESR      ADD             
         DC    X'03',AL1(02,02),X'FD100010C0',C'    '  ESR      CHANGE          
         DC    X'03',AL1(02,03),X'FD100010C0',C'    '  ESR      DISPLAY         
         DC    X'03',AL1(02,04),X'FD100010C0',C'    '  ESR      DELETE          
         DC    X'03',AL1(02,05),X'FD100010C0',C'    '  ESR      SELECT          
         DC    X'03',AL1(02,06),X'FD100010C0',C'    '  ESR      RESTORE         
         DC    X'03',AL1(02,07),X'FD100010C0',C'    '  ESR      MAINT           
         DC    X'03',AL1(02,09),X'FD100010C0',C'    '  ESR      ABDELET         
         DC    X'03',AL1(02,10),X'FE100010F8',C'    '           LIST            
         DC    X'03',AL1(02,12),X'FD10001078',C'SRSR'           REPORT          
         DC    X'03',AL1(02,13),X'FD100010C1',C'    '  ESR      ABADD           
*                                                                               
         DC    X'03',AL1(03,01),X'FC200020C0',C'    '  FAX      ADD             
         DC    X'03',AL1(03,02),X'FC200020C0',C'    '  FAX      CHANGE          
         DC    X'03',AL1(03,03),X'FC200020C0',C'    '  FAX      DISPLAY         
         DC    X'03',AL1(03,04),X'FC200020C0',C'    '  FAX      DELETE          
         DC    X'03',AL1(03,05),X'FC200020C0',C'    '  FAX      SEL             
         DC    X'03',AL1(03,07),X'FC200020C0',C'    '  FAX      MAINT           
         DC    X'03',AL1(03,10),X'FA200020F8',C'    '           LIST            
         DC    X'03',AL1(03,12),X'FC20002078',C'SRSR'           REPORT          
         DC    X'03',AL1(03,13),X'FC200020C1',C'    '  FAX      ABADD           
         DC    X'03',AL1(03,14),X'FC200020C1',C'    '  FAX      SEND            
         DC    X'03',AL1(03,15),X'FC200020C1',C'    '  FAX      REPLY           
*                                                                               
         DC    X'03',AL1(04,01),X'FB300030C0',C'    '  STATUS   ADD             
         DC    X'03',AL1(04,02),X'FB300030C0',C'    '  STATUS   CHANGE          
         DC    X'03',AL1(04,03),X'FB300030C0',C'    '  STATUS   DISPLAY         
         DC    X'03',AL1(04,04),X'FB300030C0',C'    '  STATUS   DELETE          
         DC    X'03',AL1(04,05),X'FB300030C0',C'    '  STATUS   SEL             
         DC    X'03',AL1(04,06),X'FB300030C0',C'    '  STATUS   RESTORE         
         DC    X'03',AL1(04,07),X'FB300030C0',C'    '  STATUS   MAINT           
         DC    X'03',AL1(04,10),X'F0300030F8',C'    '           LIST            
         DC    X'03',AL1(04,12),X'FB30003078',C'SRSR'           REPORT          
         DC    X'03',AL1(04,13),X'FB300030C1',C'    '  STATUS   ABADD           
         DC    X'03',AL1(04,16),X'FB300030C1',C'    '  STATUS   HISTORY         
         DC    X'03',AL1(04,17),X'FB300030C1',C'    '  STATUS   STATUS          
*                                                                               
         DC    X'03',AL1(05,03),X'F8400040C0',C'    '  ACTIVITY DISPLAY         
         DC    X'03',AL1(05,05),X'F8400040C0',C'    '  ACTIVITY SEL             
         DC    X'03',AL1(05,10),X'F8400040F8',C'    '           LIST            
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - BUMP'                  
***********************************************************************         
*                                                                     *         
*        ROUTINE TO BUMP TO NEXT FIELD ON SCREEN                      *         
*                                                                     *         
*              BUMP -  NEXT FIELD                                     *         
*              BUMPU - NEXT UNPROTECTED FIELD                         *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> CURRENT FIELD                                          *         
*                                                                     *         
*EXIT    R2==> NEXT (UNPROTECTED) FIELD                               *         
*        CC    NEQ - NOT END OF SCREEN                                *         
*              EQ  - END OF SCREEN                                    *         
*                                                                     *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     DS    0H                  BUMP TO NEXT FIELD                           
         SR    RF,RF                                                            
         ICM   RF,1,0(R2)          GET LENGTH OF TWA FIELD                      
         AR    R2,RF               POINT TO NEXT FIELD                          
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
*        THIS VERSION BUMPS TO NEXT UNPROTECTED FIELD                           
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BER   RE                                                               
*                                                                               
         TM    1(R2),X'20'         IF PROTECTED FIELD                           
         JNZ   BUMPU                  GO TO NEXT FIELD                          
*                                                                               
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - CLRSCRN'               
***********************************************************************         
*                                                                     *         
*        ROUTINE TO CLEAR FIELDS TO END OF SCREEN                     *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*NTRY    R2==> STARTING FIELD                                         *         
*                                                                     *         
*EXIT    ALL UNPROTECTED FIELDS ARE CLEARED TO END OF SCREEN          *         
*                                                                     *         
*NOTE: RF DESTROYED                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
CLRSCRLP DS    0H                                                               
*                                                                               
         BRAS  RE,CLRFLD           CLEAR UPPROTECTED FIELD                      
*                                                                               
CLRSCRCN DS    0H                                                               
*                                                                               
         BRAS  RE,BUMPU            BUMP TO NEXT UNPROTECTED FIELD               
         BZ    CLRSCRLP            MORE FIELDS ON SCREEN                        
*                                                                               
CLRSCRDN DS    0H                                                               
*                                                                               
CLRSCRNX DS    0H                  ALL DONE                                     
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - GETEL'                 
***********************************************************************         
*                                                                     *         
*        MACRO TO FIND NEXT ELEMENT IN RECORD - NON-MINIO             *         
*                                                                     *         
*              DOES NOT DEPEND ON ADDRESSABILITY                      *         
*                                                                     *         
*        GETEL FINDS FIRST INSTANCE OF ELEMENT CODE                   *         
*        NXTEL FINDS NEXT INSTANCE OF ELEMENT                         *         
*                                                                     *         
*                                                                     *         
*NTRY    R6==> START OF RECORD OR CURRENT ELEMENT                     *         
*        ELCODE HAS CODE FOR ELEMENT                                  *         
*                                                                     *         
*EXIT    R6==> NEXT (UNPROTECTED) FIELD                               *         
*        CC    NEQ - ELEMENT NOT FOUND                                *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VCOMMON'               
***********************************************************************         
*                                                                     *         
*        COMMON ENTRY POINT FOR GENERAL SYSTEM ROUTINES               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         ST    RD,COMMRD           SAVE RD CHAIN POINTER                        
*                                                                               
         L     R9,ASYSD            RESTORE WORKING STORAGE POINTER              
*                                                                               
         SRL   RF,24               SHIFT ROUTINE ID TO RIGHT NYBBLE             
         L     RF,VBRANCH(RF)      GET A(ROUTINE)                               
         A     RF,RELO             RELOCATE ADDRESS                             
*                                                                               
         BASR  RE,RF               GO TO ROUTINE                                
*                                                                               
VCOMMONX DS    0H                                                               
         XIT1                      RETURN TO CALLER                             
*                                                                               
*        COMMON ROUTINE ADDRESSES                                               
*                                                                               
VBRANCH  DS    0D                  ALIGNMENT                                    
         DC    A(VVALAGY)          VALIDATE AGENCY                              
         DC    A(VVALMED)          VALIDATE MEDIA                               
         DC    A(VDISMED)          DISPLAY  MEDIA                               
         DC    A(VVALCLT)          VALIDATE CLIENT                              
         DC    A(VDISCLT)          DISPLAY  CLIENT                              
         DC    A(VVALPRD)          VALIDATE PRODUCT                             
         DC    A(VDISPRD)          DISPLAY  PRODUCT                             
         DC    A(VVALEST)          VALIDATE ESTIMATE                            
         DC    A(VDISEST)          DISPLAY  ESTIMATE                            
         DC    A(VVALPER)          VALIDATE PERIOD                              
         DC    A(VDISPER)          DISPLAY  PERIOD                              
         DC    A(VVALPUB)          VALIDATE PUB                                 
         DC    A(VDISPUB)          DISPLAY  PUB                                 
         DC    A(VVALADC)          VALIDATE AD CODE                             
         DC    A(VDISADC)          DISPLAY  AD CODE                             
         DC    A(VMININIT)         INITIALIZE MINIO SET                         
         DC    A(VGETFLD)          GET FIELD FROM SCREEN                        
         DC    A(VADDELM)          ADD     ELEMENT TO   MINIO SET               
         DC    A(VWRTELM)          REPLACE ELEMENT IN   MINIO SET               
         DC    A(VDELELM)          DELETE  ELEMENT FROM MINIO SET               
         DC    A(VGETELM)          FIND          ELEMENT IN MINIO SET           
         DC    A(VNXTELM)          FIND NEXT     ELEMENT IN MINIO SET           
         DC    A(VPRVELM)          FIND PREVIOUS ELEMENT IN MINIO SET           
         DC    A(VERREXIT)         ERROR EXIT                                   
         DC    A(VNXTSR#)          GET NEXT INSERTION ORDER NUMBER              
         DC    A(VVALREP)          VALIDATE REP                                 
         DC    A(VDISREP)          DISPLAY  REP                                 
         DC    A(VPSSVS)           CREATE PASSIVE POINTERS                      
         DC    A(VTRNPID)          TRANSLATE PID TO A NAME                      
         DC    A(VGOPFM)           GO TO PFM                                    
         DC    A(VVALSR#)          VALIDATE SR#                                 
         DC    A(VDISSR#)          DISPLAY  SR#                                 
         DC    A(VVALSTA)          VALIDATE STATUS                              
         DC    A(VDISSTA)          DISPLAY  STATUS                              
         DC    A(VGETSCH)          GET SETUP RECORD                             
         DC    A(VFMTSR#)          FORMAT SR#                                   
         DC    A(VVALRV#)          VALIDATE REVISION NUMBER                     
         DC    A(VDISRV#)          DISPLAY  REVISION NUMBER                     
         DC    A(VACTPUT)          ADD ACTIVITY ELEMENT                         
         DC    A(VFNDSR#)          FIND SR# FOR DATE                            
         DC    A(VDELPSSV)         DELETE PASSIVE POINTERS                      
         DC    A(VPRSSR#)          PARSE EXPANDED SR# INTO COMPONENTS           
         DC    A(VFNDRV#)          FIND LATEST REVISION #                       
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VMED'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE AGENCY OF USER                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALAGY  NTR1  BASE=*,LABEL=*      GET AGY REC IN MEDIA VALIDATION              
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         CLI   MODE,NEWSCR         ANALYZE CALLING MODE                         
         BE    VAGYNEW               NEW SCREEN LOADED                          
*                                                                               
         B     VVALSECX            SECURITY CHECK NOT NEEDED                    
*                                                                               
         OC    TWASAGN,TWASAGN     TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS  TEST ANY LIMIT ACCESS                        
         BZ    VVALSECX                                                         
*                                                                               
         LHI   R6,SECBLK-T420FFD   DISPLACEMENT OF SECURITY BLOCK               
         LA    R6,T420FFD(R6)      POINT TO SECURITY BLOCK                      
         ST    R6,ASECBLK          SAVE A(SECURITY BLOCK)                       
*                                                                               
         L     R0,ASECBLK          CLEAR SECRET BLOCK                           
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*******  CALL TO FASECRET TO BUILD A SECURITY AUTHORIZATION TABLE               
*                                                                               
*        INITIALIZE SECURITY BLOCK                                              
*                                                                               
         GOTO1 SECRET,DMCB,('SECPINIT',ASECBLK),0                               
         BE    *+6                                                              
         DC    H'0'                BLOCK NOT BIG ENOUGH                         
*                                                                               
         MVI   TRFAGSW,0           CLEAR                                        
*                                                                               
         BAS   RE,CKTRAFID         TRAFFIC ID ?                                 
         BNE   VVALSECX            NO                                           
*                                                                               
         MVI   TRFAGSW,C'Y'        YES - TRAFFIC ID SIGN-ON                     
*                                                                               
VVALSECX DS    0H                                                               
*                                                                               
*******  B     VAGYLNKX                                                         
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF LINK CALL                            
         BE    VAGYLNK1                                                         
*                                                                               
         GOTOR LNKINI,DMCB,(RC)    CHECK IF BEING CALLED BY LINK                
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT A LINK CALL                      
         BNE   VAGYLNKX            NO                                           
*                                                                               
VAGYLNK1 DS    0H                                                               
*                                                                               
         CLI   DDCOMSW,C'C'        IF DOING COMMENTS NOW                        
         BNE   VAGYLNK2                                                         
*                                                                               
*        CALL COMMENT SCREEN WITH ACTION ADD                                    
*                                                                               
         MVC   CONREC(8),=CL8'COMMENT'    SET TO DO COMMENTS                    
         MVI   CONRECH+5,3                                                      
         MVC   CONACT(8),=CL8'ABADD'                                            
         MVI   CONACTH+5,3                                                      
*                                                                               
         B     VAGYLNK3                                                         
*                                                                               
VAGYLNK2 DS    0H                                                               
*                                                                               
         GOTOR LNKRA,DMCB,(RC)     FILL IN RECORD/ACTION                        
*                                                                               
         CLI   DDLNKEOF,C'Y'       IF END OF FILE                               
         BNE   *+8                                                              
         L     RD,SYSRD               BYPASS GENCON                             
*                                                                               
VAGYLNK3 DS    0H                                                               
*                                                                               
         OI    GENSTAT4,NEWSCRM    CALL WHEN NEW SCREEN LOADED                  
*                                                                               
VAGYLNKX DS    0H                                                               
*                                                                               
         B     VVALAGYX                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*        NEW SCREEN LOADED - PROBABLY BEING CALLED BY LINK            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VAGYNEW  DS    0H                                                               
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT A LINK CALL                      
         BNE   VAGYNEWX                                                         
*                                                                               
         CLI   DDCOMSW,C'C'        SKIP IF DOING COMMENTS NOW                   
         BE    VAGYNEWX                                                         
*                                                                               
VAGYGET  DS    0H                                                               
*                                                                               
         GOTOR LNKGET,DMCB,(RC)    FILL IN DATA FOR SCREEN                      
*                                                                               
         MVI   IO,0                STOPS KEYMERGE IN GENCON                     
*                                                                               
VAGYGETX DS    0H                                                               
*                                                                               
VAGYNEWX DS    0H                                                               
*                                                                               
VVALAGYX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - CKTRAFID'              
***********************************************************************         
*                                                                     *         
*        CHECKING IF SIGN ON ID IS FOR TRAFFIC ONLY                   *         
*                                                                     *         
*EXIT    EQ    CC - YES                                               *         
*        NEQ   CC - NO                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CKTRAFID NTR1                      CHECKING FOR TRAFFIC ID SIGN-ON              
*                                                                               
         L     R0,AIO2             CLEAR A 1600 BYTE WORKAREA                   
         LHI   R1,1600                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
*        READ CONTROL FILE ID RECORD                                            
*                                                                               
         L     R4,AIO2             ESTABLISH ID RECORD                          
         USING CTIREC,R4                                                        
*                                                                               
         XC    CTIKEY,CTIKEY       INIT KEY                                     
         MVI   CTIKTYP,CTIKTYPQ    SET KEY ID                                   
*                                                                               
         MVC   CTIKNUM,TWAORIG     ID NUMBER                                    
*                                  READ ID RECORD                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(R4),(R4)                 
         CLI   8(R1),0             MUST BE FOUND                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*        FIND AGENCY ALPHA ID ELEMENT                                           
*                                                                               
         LA    RE,CTIDATA          POINT TO FIRST ELEMENT IN RECORD             
         USING CTAGYD,RE           ESTABLISH AS AGENCY ALPHA ID ELEMENT         
         SR    RF,RF                                                            
*                                                                               
CKTRALP  DS    0H                                                               
*                                                                               
         CLI   CTAGYEL,0           CHECK FOR END OF RECORD                      
         BNE   *+6                                                              
         DC    H'0'                ELEM MUST BE PRESENT                         
*                                                                               
         CLI   CTAGYEL,CTAGYELQ    AGENCY ALPHA ID ELEMENT (X'06')              
         BE    CKTRAFD                                                          
*                                                                               
CKTRACN  DS    0H                                                               
*                                                                               
         IC    RF,CTAGYLEN         GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
*                                                                               
         B     CKTRALP                                                          
*                                                                               
CKTRAFD  DS    0H                                                               
*                                                                               
         CLI   CTAGYIDT,CTAGYTTQ   TRAFFIC ID (C'T')?                           
         BNE   CKTRIDNT                                                         
*                                                                               
         CR    RB,RB               SET EQUAL CC                                 
*                                                                               
         B     CKTRIDX                                                          
*                                                                               
CKTRIDNT LTR   RB,RB               NOT EQUAL (SIGN-ON IS NOT TRAFFIC)           
*                                                                               
CKTRIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VMED'                  
***********************************************************************         
*                                                                     *         
*        VALIDATE MEDIA CODE                                          *         
*                                                                     *         
*NTRY    R2==> MEDIA FIELD ON SCREEN                                  *         
*                                                                     *         
*EXIT    QMED = MEDIA CODE                                            *         
*        AIO1  A(PAGYREC)                                             *         
*        NE CC IF MEDIA CHANGED                                       *         
*              MEDIA CODE FIELD SET AS PREVIOUSLY VALIDATED           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALMED  NTR1  BASE=*,LABEL=*      GET AGY REC IN MEDIA VALIDATION              
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         XC    SVKEY,SVKEY         CLEAR SAVED KEY AREA                         
*                                                                               
         GOTO1 GETFLD                                                           
*                                                                               
         CLI   FLDH+5,1            INPUT LEN MUST BE 1                          
         BNE   VMEDERR                                                          
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY              ESTABLISH AGENCY RECORD KEY                  
         USING PAGYKEY,R4                                                       
*                                                                               
         MVC   PAGYKAGY,AGENCY     AGENCY ALPHA                                 
         MVC   PAGYKMED,FLD        MEDIA                                        
         MVI   PAGYKRCD,X'01'      AGYREC ID                                    
*                                                                               
         GOTO1 HIGH                READ AGENCY RECORD                           
*                                                                               
         CLC   KEY(25),KEYSAVE     RECORD MUST BE FOUND                         
         BNE   VMEDINVE                                                         
*                                                                               
         L     R4,AIO1             READ RECORD INTO IOA1                        
         ST    R4,AIO                                                           
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         LR    R6,R4               POINT TO START OF RECORD                     
         MVI   ELCODE,X'01'        FIND AGENCY ELEMENT                          
*                                                                               
         BRAS  RE,GETEL            GO LOOK FOR ELEMENT                          
         BNE   VMEDINVE            MUST FIND ELEMENT                            
*                                                                               
         USING PAGYELEM,R6         ESTABLISH AS AGENCY ELEMENT                  
*                                                                               
         MVC   MEDNM,PAGYMED       MEDIA NAME                                   
*                                                                               
         MVC   USERNAME,PAGYNAME   SAVE AGENCY NAME                             
         MVC   USERADDR,PAGYADDR   SAVE AGENCY ADDRESS                          
*                                                                               
         MVC   SVUSER,USERNAME     SAVE FOR FUTURE REF                          
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VMEDNAMX                                                         
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR MEDIA NAME FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDOLEN          GET OUTPUT LENGTH                            
*                                                                               
         CHI   RF,L'MEDNM          DEFAULT TO LESSER LENGTH                     
         BNH   *+8                                                              
         LHI   RF,L'MEDNM                                                       
*                                                                               
         STC   RF,FLDOLEN          SET OUTPUT LENGTH                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),MEDNM    DISPLAY MEDIA NAME                           
*                                                                               
VMEDNAMX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ CLEAR INDICATOR                            
*                                                                               
         CLC   QMED,PAGYKMED       IF MEDIA CODE CHANGED                        
         BE    *+12                                                             
         MVC   QMED,PAGYKMED          SAVE INPUT MEDIA CODE                     
         LTR   RB,RB                  SET NE CC                                 
*                                                                               
VVALMEDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VMEDINVE DS    0H                  INVALID MEDIA                                
         MVI   ERROR,PPEMEDNV      DEFAULT ERROR CODE                           
         B     VMEDERR                                                          
*                                                                               
VMEDERR  DS    0H                                                               
*                                                                               
         XC    QMED,QMED           CLEAR MEDIA SAVEAREA                         
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VMEDERRX                                                         
*                                                                               
         LR    R0,R2               SAVE MEDIA CODE POINTER                      
*                                                                               
         BRAS  RE,BUMP             BUMP TO MEDIA NAME FIELD                     
*                                                                               
         BRAS  RE,CLRFLD           CLEAR MEDIA NAME FIELD                       
*                                                                               
         LR    R2,R0               RESTORE MEDIA CODE POINTER                   
*                                                                               
VMEDERRX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ CLEAR INDICATOR                            
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISMED'               
***********************************************************************         
*                                                                     *         
*        DISPLAY MEDIA CODE AND NAME                                  *         
*                                                                     *         
*NTRY    QMED  MEDIA CODE                                             *         
*        R2==> MEDIA CODE FIELD ON SCREEN                             *         
*                                                                     *         
*EXIT          MEDIA CODE FILLED IN FOR SCREEN FIELD                  *         
*              MEDIA DESCRIPTION IS IN NEXT UNPROTECTED FIELD         *         
*              BOTH FIELDS SET TO BE REDISPLAYED                      *         
*              MEDIA CODE FIELD SET AS PREVIOUSLY VALIDATED           *         
*        AIO1  A(PAGYREC)                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISMED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR MEDIA CODE FIELD                       
*                                                                               
         LA    RF,L'QMED           SET LENGTH OF MEDIA CODE                     
         STC   RF,FLDILEN          SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),QMED     DISPLAY MEDIA CODE                           
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
         GOTOR VALMED              VALIDATE MEDIA - FILLS IN NAME               
*                                                                               
VDISMEDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VVALCLT'               
***********************************************************************         
*                                                                     *         
*        VALIDATE CLIENT CODE                                         *         
*              CLIENT RECORD MUST EXIST FOR MEDIA                     *         
*              C'***' STANDS FOR CLIENT 'VARIOUS'                     *         
*                                                                     *         
*NTRY    R2==> CLIENT FIELD ON SCREEN                                 *         
*                                                                     *         
*EXIT    QCLT = CLIENT CODE                                           *         
*        NE CC IF CLIENT CHANGED                                      *         
*        AIO1  A(PCLTREC)                                             *         
*              CLIENT CODE FIELD SET AS PREVIOUSLY VALIDATED          *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALCLT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         GOTOR GETFLD              READ CLIENT CODE INTO WORKAREA               
*                                                                               
*        READ CLIENT RECORD                                                     
*                                                                               
         XC    KEY,KEY             INIT KEY BUILD AREA                          
         LA    R4,KEY                                                           
         USING PCLTRECD,R4         ESTABLISH CLIENT RECORD KEY                  
*                                  BUILD CLIENT KEY                             
         MVC   PCLTKAGY,AGENCY     AGENCY                                       
         MVC   PCLTKMED,QMED       MEDIA                                        
         MVI   PCLTKRCD,PCLTKIDQ   CLIENT RECORD CODE                           
         MVC   PCLTKCLT,FLD        CLIENT CODE                                  
         OC    PCLTKCLT,SPACES     SPACE FILL CLIENT CODE                       
*                                                                               
         CLC   =C'***',FLD         IF CLIENT VARIOUS                            
         BNE   VCLTVARN                                                         
*                                                                               
         MVC   CLTNM,=CL20'VARIOUS' FILL IN NAME                                
         XC    SVCPROF,SVCPROF     SET  CLIENT PROFILE                          
         XC    SVCLTOFC,SVCLTOFC   SET  CLIENT OFFICE                           
*                                                                               
         B     VCLT90                                                           
*                                                                               
VCLTVARN DS    0H                                                               
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                READ CLIENT POINTER                          
*                                                                               
         CLC   PCLTKEY,KEYSAVE     MUST FIND CLIENT RECORD                      
         BNE   VCLTINVE                                                         
*                                                                               
         L     R4,AIO1             READ CLIENT RECORD INTO IOA1                 
         ST    R4,AIO                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
*        CHECK CLIENT'S SECURITY ACCESS                                         
*                                                                               
         B     VCLTSECX            SKIP SECURITY CHECK                          
*                                                                               
         CLI   OFFLINE,C'Y'        RUNNING OFFLINE REPORT ?                     
         BE    VCLTSECX            YES - SKIP LIMIT ACCESS                      
*                                                                               
         OC    TWASAGN,TWASAGN     TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    TWAACCS(2),TWAACCS  TEST ANY LIMIT ACCESS                        
         BZ    VCLTSECX            NONE - SKIP SECURITY TESTS                   
*                                                                               
         MVC   SVCLTOFC,PCLTOFF   SAVE CLIENT'S OFFICE                          
*                                                                               
*        CHECK FOR A CLIENT'S TRAFFIC OFFICE                                    
*                                                                               
VCLTTRA  DS    0H                                                               
*                                                                               
         CLI   TRFAGSW,C'Y'       SKIP IF NOT TRAFFIC ID SIGN-ON                
         BNE   VCLTTRAX                                                         
*                                 SEE IF TRAFFIC OFFICE EXISTS                  
         L     R6,AIO1            POINT TO CLIENT REC                           
         MVI   ELCODE,X'50'       CLIENT TRAFFIC OFFICE ELEM CODE               
         BRAS  RE,GETEL                                                         
         BNE   VCLTTRAX           NO TRAFFIC OFFICE FOUND                       
*                                                                               
         USING PCLTTOEL,R6         ESTABLISH CLIENT TRAF OFC ELM                
*                                                                               
         MVC   SVCLTOFC,PCLTTOFC  USE TRAFFIC OFFICE FOR TESTS                  
*                                                                               
VCLTTRAX DS    0H               *****  LIMIT ACCESS TESTING   *****             
*                                                                               
         XC    WORK,WORK          WORK MUST BE AT LEAST 48 BYTES                
         LA    R1,WORK            (LENGTH OF OFFICED IS 48 BYTES)               
         USING OFFICED,R1          ESTABLISH OFFICER CONTROL BLOCK              
*                                                                               
*        BUILD OFFICER CONTROL BLOCK                                            
*                                                                               
         MVI   OFCSYS,C'P'         SYSTEM IS PRINT                              
         MVC   OFCAUTH,TWAACCS     AGENCY ACCESS                                
         MVC   OFCAGY,AGENCY       AGENCY                                       
         MVC   OFCOFC,SVCLTOFC     CLT OR CLT TRAFFIC OFFICE CODE               
         MVC   OFCCLT,PCLTKCLT     CLIENT                                       
         MVC   OFCPMED,PCLTKMED    MEDIA                                        
         MVC   OFCLMT(4),TWAACCS   ACCESS BYTES                                 
         MVC   OFCSECD,ASECBLK     A("SECRET BLOCK")                            
*                                                                               
         GOTO1 VOFFICER,DMCB,(C'N',WORK),ACOMFACS  CHECK SECURITY               
         CLI   0(R1),0                                                          
         BNE   VCLTSECE           SECURITY LOCK OUT                             
*                                                                               
VCLTSECX DS    0H                                                               
*                                                                               
         MVC   CLTNM,PCLTNAME      SAVE CLIENT NAME                             
         MVC   SVCPROF,PCLTPROF    SAVE CLIENT PROFILE                          
         MVC   SVCLTOFC,PCLTOFF    SAVE CLIENT OFFICE                           
*                                                                               
VCLT90   DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VCLTNAMX                                                         
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR CLIENT NAME FIELD                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDOLEN          GET OUTPUT LENGTH                            
*                                                                               
         CHI   RF,L'CLTNM          DEFAULT TO LESSER LENGTH                     
         BNH   *+8                                                              
         LHI   RF,L'CLTNM                                                       
*                                                                               
         STC   RF,FLDOLEN          SET OUTPUT LENGTH                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),CLTNM    DISPLAY CLIENT NAME                          
*                                                                               
VCLTNAMX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ TURN OFF INDICATOR                         
*                                                                               
         CLC   QCLT,PCLTKCLT       IF CLIENT CODE CHANGED                       
         BE    *+12                                                             
         MVC   QCLT,PCLTKCLT          SAVE CLIENT CODE                          
         LTR   RB,RB                  SET NE CC                                 
*                                                                               
VVALCLTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VCLTINVE MVI   ERROR,PPECLTNV     INVALID CLIENT                                
         B     VCLTERR                                                          
*                                                                               
VCLTSECE MVI   ERROR,PPESECLK     SECURITY LOCK-OUT                             
         B     VCLTERR                                                          
*                                                                               
VCLTERR  DS    0H                                                               
*                                                                               
         XC    QCLT,QCLT           CLEAR CLIENT SAVEAREA                        
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VCLTERRX                                                         
*                                                                               
         LR    R0,R2               SAVE CLIENT CODE POINTER                     
*                                                                               
         BRAS  RE,BUMP             BUMP TO CLIENT NAME FIELD                    
         BRAS  RE,CLRFLD           CLEAR AND TRANSMIT NAME                      
*                                                                               
         LR    R2,R0               RESTORE CLIENT CODE POINTER                  
*                                                                               
VCLTERRX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ TURN OF INDICATOR                          
*                                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISCLT'               
***********************************************************************         
*                                                                     *         
*        DISPLAY CLIENT CODE AND NAME                                 *         
*                                                                     *         
*NTRY    P0    A(CLIENT CODE)                                         *         
*        R2==> CLIENT CODE FIELD ON SCREEN                            *         
*                                                                     *         
*EXIT          CLIENT CODE FILLED IN FOR SCREEN FIELD                 *         
*              CLIENT DESCRIPTION IS IN NEXT PROTECTED FIELD          *         
*              BOTH FIELDS SET TO BE REDISPLAYED                      *         
*              CLIENT CODE FIELD SET AS PREVIOUSLY VALIDATED          *         
*        AIO1  A(PCLTREC)                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISCLT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR CLIENT CODE FIELD                      
*                                                                               
         LA    RF,L'QCLT           SET LENGTH OF CLIENT CODE                    
         STC   RF,FLDILEN          SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),QCLT     DISPLAY CLIENT CODE                          
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
         GOTOR VALCLT              VALIDATE MEDIA - FILLS IN NAME               
*                                                                               
VDISCLTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VVALPRD'               
***********************************************************************         
*                                                                     *         
*        VALIDATE PRODUCT CODE                                        *         
*                                                                     *         
*NTRY    R2==> PRODUCT FIELD ON SCREEN                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALPRD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         XC    QPRD,QPRD           INIT PRODUCT CODE SAVEAREA                   
         XC    PRDNM,PRDNM         INIT PRODUCT NAME SAVEAREA                   
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VPRDCLRX                                                         
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
         BRAS  RE,CLRFLD           CLEAR PRODUCT NAME FIELD                     
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
VPRDCLRX DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BNZ   *+16                HAVE INPUT                                   
         CLI   FLDOPT,C'Y'         IF INPUT IS OPTIONAL                         
         BE    VVALPRDX               NO INPUT IS OKAY                          
         B     VPRD2E              ELSE INPUT REQUIRED                          
*                                                                               
         CHI   RF,2                LENGTH MUST BE 2 OR 3 CHARCTERS              
         BL    VPRD3E                                                           
         CHI   RF,3                                                             
         BH    VPRD3E                                                           
*                                                                               
         XC    KEY,KEY             BUILD PRODUCT RECORD KEY                     
         LA    R4,KEY                                                           
         USING PPRDKEY,R4          ESTABLISH PRODUCT RECORD KEY                 
*                                                                               
         MVC   PPRDKAGY,AGENCY     AGENCY                                       
         MVC   PPRDKMED,QMED       MEDIA                                        
         MVI   PPRDKRCD,PPRDKIDQ   PRODUCT IDENTIFIER                           
         MVC   PPRDKCLT,QCLT       CLIENT                                       
         MVC   PPRDKPRD,FLDDATA    PRODUCT                                      
         OC    PPRDKPRD,SPACES     BLANK FILL                                   
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READING FOR UPDATE                       
         GOTO1 HIGH                READ PRODUCT POINTER                         
*                                                                               
         CLC   PPRDKEY,KEYSAVE     PRODUCT NOT FOUND                            
         BNE   VPRDINVE                                                         
*                                                                               
         L     R4,AIO1             READ PRODUCT RECORD INTO IOA1                
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LR    R6,R4               POINT TO STAT OF RECORD                      
         MVI   ELCODE,X'06'        FIND PRODUCT DESCRIPTION ELEMENT             
*                                                                               
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BNE   VPRDINVE            MUST FIND ELEMENT                            
*                                                                               
         USING PPRDELEM,R6         ESTABLISH AS PRODUCT DESC ELM                
*                                                                               
         MVC   QPRD,PPRDKPRD       SAVE PRODUCT CODE                            
         MVC   PRDNM,PPRDNAME      SAVE PRODUCT NAME                            
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VPRDNAMX                                                         
*                                                                               
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
*                                                                               
         MVC   FLDDATA(L'PRDNM),PRDNM DISPLAY ESTIMATE NAME                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'PRDNM     SET LENGTH OF OUTPUT                         
*                                                                               
VPRDNAMX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ TURN OF INDICATOR                          
*                                                                               
VVALPRDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPRDINVE MVI   ERROR,PPEPRDNV     INVALID PRODUCT                               
         B     VPRDERR                                                          
*                                                                               
VPRD2E   MVI   ERROR,PPEFLDNE     PRODUCT MISSING                               
         B     VPRDERR                                                          
*                                                                               
VPRD3E   MVI   ERROR,PPEPRDNV     INVALID PRODUCT                               
         B     VPRDERR                                                          
*                                                                               
VPRDERR  DS    0H                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISPRD'               
***********************************************************************         
*                                                                     *         
*        DISPLAY PRODUCT CODE AND NAME                                *         
*                                                                     *         
*NTRY    P0    A(PRODUCT CODE)                                        *         
*        R2==> PRODUCT CODE FIELD ON SCREEN                           *         
*                                                                     *         
*EXIT          PRODUCT CODE FILLED IN FOR SCREEN FIELD                *         
*              PRODUCT DESCRIPTION IS IN NEXT PROTECTED FIELD         *         
*              BOTH FIELDS SET TO BE REDISPLAYED                      *         
*              PRODUCT CODE FIELD SET AS PREVIOUSLY VALIDATED         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISPRD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR PRODUCT CODE FIELD                     
*                                                                               
         LA    RF,L'QPRD           SET LENGTH OF PRODUCT CODE                   
         STC   RF,FLDILEN          SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),QPRD     DISPLAY PRODUCT CODE                         
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
         GOTOR VALPRD              VALIDATE MEDIA - FILLS IN NAME               
*                                                                               
VDISPRDX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VVALEST'               
***********************************************************************         
*                                                                     *         
*        VALIDATE ESTIMATE CODE                                       *         
*                                                                     *         
*NTRY    R2==> ESTIMATE FIELD ON SCREEN                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALEST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLIH SCREEN FIELD                        
*                                                                               
         XC    QEST,QEST           INIT ESTIMATE SAVEAREA                       
         XC    BEST,BEST           INIT ESTIMATE SAVEAREA                       
         XC    ESTNM,ESTNM         INIT ESTIMATE NAME                           
         XC    ESTDTST,ESTDTST     INIT ESTIMATE START DATE                     
         XC    ESTDTEND,ESTDTEND   INIT ESTIMATE END DATE                       
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VESTCLRX                                                         
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
         BRAS  RE,CLRFLD           CLEAR ESTIMATE NAME FIELD                    
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
VESTCLRX DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BNZ   *+16                HAVE INPUT                                   
         CLI   FLDOPT,C'Y'         IF INPUT IS OPTIONAL                         
         BE    VVALESTX               NO INPUT IS OKAY                          
         B     VEST2E              ELSE INPUT REQUIRED                          
*                                                                               
         BCTR  RF,0                SECREMMENT FOR EXECUTE                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)      PACK ESTIMATE NUMBER                         
*                                                                               
         TP    DUB                 MAKE SURE ITS A PACKED NUMBER                
         BNZ   VESTINVE            INVALID IF NOT                               
*                                                                               
         CP    DUB,=P'1000'        MUST BE < 1000                               
         BNL   VESTINVE                                                         
*                                                                               
         CP    DUB,=P'0'           MUST BE >0                                   
         BNH   VESTNAMX            SKIP                                         
*                                                                               
         CVB   RF,DUB              CVB                                          
         STCM  RF,3,BEST           SAVE ESTIMATE NUMBER                         
*                                                                               
         XC    KEY,KEY             GET ESTIMATE RECORD                          
         LA    R4,KEY                                                           
         USING PESTKEY,R4          ESTABLISH ESTIMATE KEY                       
*                                  BUILD PESTREC KEY                            
         MVC   PESTKAGY,AGENCY     AGENCY                                       
         MVC   PESTKMED,QMED       MEDIA                                        
         MVI   PESTKRCD,PESTKIDQ   ESTIMATE RECORD ID                           
         MVC   PESTKCLT,QCLT       CLIENT                                       
         MVC   PESTKPRD,QPRD       PRODUCT                                      
         MVC   PESTKEST,BEST       ESTIMATE NUMBER                              
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READING FOR UPDATE                       
         GOTO1 HIGH                READ ESTIMATE POINTER                        
*                                                                               
         CLC   PESTKEY,KEYSAVE     MUST FIND POINTER                            
         BNE   VESTINVE                                                         
*                                  READ RECORD INTO IOA1                        
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC              READ IN ESTIMATE RECORD                      
*                                                                               
         LR    R6,R4               POINT TO START OF RECORD                     
         MVI   ELCODE,X'07'        SET TO FIND DESCRIPTION ELEMENT              
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING PESTELEM,R6         ESTABLISH DESCRIPTION ELEMENT                
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,PESTKEST       GET ESTIMATE NUMBER                          
*                                                                               
         CVD   R0,DUB              SAVE ESTIMATE NUMBER                         
         OI    DUB+7,X'0F'           IN CHARACTER                               
         UNPK  QEST,DUB                                                         
*                                                                               
         MVC   ESTNM,PESTNAME      SAVE ESTIMATE NAME                           
         MVC   ESTDTST,PESTST      SAVE ESTIMATE START DATE                     
         MVC   ESTDTEND,PESTEND    SAVE ESTIMATE END DATE                       
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VESTNAMX                                                         
*                                                                               
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
*                                                                               
         MVC   FLDDATA(L'ESTNM),ESTNM DISPLAY ESTIMATE NAME                     
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'ESTNM     SET LENGTH OF OUTPUT                         
*                                                                               
VESTNAMX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ  TURN OFF INDICATOR                        
*                                                                               
VVALESTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VEST2E   MVI   ERROR,PPEFLDNE      INPUT REQUIRED                               
         B     VESTERR                                                          
*                                                                               
VESTINVE MVI   ERROR,PPEESTNV      INVALID ESTIMATE                             
         B     VESTERR                                                          
*                                                                               
VESTERR  DS    0H                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISEST'               
***********************************************************************         
*                                                                     *         
*        DISPLAY ESTIMATE CODE AND NAME                               *         
*                                                                     *         
*NTRY    P0    A(ESTIMATE CODE)                                       *         
*        R2==> ESTIMATE CODE FIELD ON SCREEN                          *         
*                                                                     *         
*EXIT          ESTIMATE CODE FILLED IN FOR SCREEN FIELD               *         
*              ESTIMATE DESCRIPTION IS IN NEXT PROTECTED FIELD        *         
*              BOTH FIELDS SET TO BE REDISPLAYED                      *         
*              ESTIMATE CODE FIELD SET AS PREVIOUSLY VALIDATED        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISEST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD HEADER                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR ESTIMATE CODE FIELD                    
*                                                                               
         LA    RF,L'QEST           SET LENGTH OF ESTIMATE CODE                  
         STC   RF,FLDILEN          SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),QEST     DISPLAY ESTIMATE CODE                        
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDIIND,FINPNUM     SET AS NUMERIC                               
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
         GOTOR VALEST              VALIDATE MEDIA - FILLS IN NAME               
*                                                                               
VDISESTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VVALPUB'               
***********************************************************************         
*                                                                     *         
*        VALIDATE PUBLICATION CODE                                    *         
*                                                                     *         
*NTRY    R2==> PUB      FIELD ON SCREEN                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALPUB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH PUB CODE FIELD                     
*                                                                               
         XC    QPUB,QPUB           INI PUB SAVEAREA                             
         XC    PUBNM,PUBNM         INIT PUB NAME                                
*                                                                               
         CLI   8(R2),C'='          IF INPUT STARTS WITH '='                     
         BNE   VPUBSCHX                                                         
*                                     NEED TO DO NAME SEARCHING                 
         S     R2,ATWA                DISPLACEMENT OF FIELD INTO TWA            
*                                                                               
         LA    R3,WORK                                                          
         USING DSPARM,R3           ESTABLISH SEARCH PARAMETER LIST              
         XC    DSPARM(DSPARML),DSPARM                                           
*                                                                               
         MVC   DSMEDCOD,QMED       PASS MEDIA CODE                              
*                                                                               
         GOTO1 =V(SRCHCALL),DMCB,(3,(R2)),(X'80',ATWA),ACOMFACS,       X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0,RR=RELO                         
*                                                                               
         A     R2,ATWA             RESTORE FIELD POINTER                        
*                                                                               
         DROP  R3                                                               
*                                                                               
VPUBSCHX DS 0H                                                                  
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VPUBCLRX                                                         
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
         BRAS  RE,CLRFLD           CLEAR PRODUCT NAME FIELD                     
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
VPUBCLRX DS    0H                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,FLDILEN        GET INPUT LENGTH                             
         BNZ   *+16                HAVE INPUT                                   
         CLI   FLDOPT,C'Y'         IF INPUT IS OPTIONAL                         
         BE    VVALPUBX               NO INPUT IS OKAY                          
         B     VPUBINVE            ELSE INPUT REQUIRED                          
*                                                                               
         GOTO1 VPUBVAL,DMCB,((R0),FLDDATA),QPUB                                 
*                                                                               
         CLI   0(R1),X'FF'         CHECK FOR ERRORS                             
         BE    VPUBINVE                                                         
*                                                                               
         CLC   =C'H7',AGENCY       IF GROUP M                                   
         BNE   *+14                                                             
         CLC   =X'00666666',QPUB   AND DUMMY PUB                                
         BE    VPUBINVE               NOT A VALID PUB                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH PUBREC KEY                         
         USING PUBRECD,R4                                                       
*                                  BUILD PUB REC KEY                            
         MVC   PUBKMED,QMED        MEDIA                                        
         MVC   PUBKPUB(6),QPUB     MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY      AGENCY                                       
         MVI   PUBKCOD,X'81'       RECORD CODE                                  
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READING FOR UPDATE                       
         MVC   FILENAME,=CL8'PUBDIR'  PUB DIRECTORY                             
         GOTO1 HIGH                READ PUB POINTER                             
         XC    FILENAME,FILENAME   RESET FILENAME                               
*                                                                               
         CLC   PUBKEY,KEYSAVE      ERROR IF PUB NOT ON FILE                     
         BNE   VPUBINVE                                                         
*                                                                               
         L     R4,AIO1             READ PUBREC INTO IAO1                        
         ST    R4,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         LR    R6,R4               POINT TO START OF RECORD                     
         MVI   ELCODE,PUBNAMQ      SEARCH FOR PUBNAME ELEMENT                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ELEMENT                            
*                                                                               
         USING PUBNAMEL,R6         ESTABLISH NAME ELEMENT                       
         MVC   PUBNM,PUBNAME       SAVE PUB NAME                                
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VPUBNAMX                                                         
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD                           
*                                                                               
         BRAS  RE,CLRFLD           CLEAR PUB NAME FIELD                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDOLEN          GET OUTPUT LENGTH                            
*                                                                               
         CHI   RF,L'PUBNM          DEFAULT TO LESSER LENGTH                     
         BNH   *+8                                                              
         LHI   RF,L'PUBNM                                                       
*                                                                               
         STC   RF,FLDOLEN          SET OUTPUT LENGTH                            
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),PUBNM    DISPLAY PUB NAME                             
*                                                                               
VPUBNAMX DS    0H                                                               
*                                                                               
         CLC   QPUB,PUBKPUB        IF PUB CODE CHANGED                          
         BE    *+12                                                             
         MVC   QPUB,PUBKPUB           SAVE PUB CODE                             
         LTR   RB,RB                  SET NE CC                                 
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ  TURN OFF INDICATOR                        
*                                                                               
VVALPUBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPUBINVE MVI   ERROR,PPEPUBNV      INVALID PUB                                  
         B     VPUBERR                                                          
*                                                                               
VPUBERR  DS    0H                                                               
*                                                                               
         XC    QPUB,QPUB           CLEAR PUB SAVEAREA                           
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VPUBERRX                                                         
*                                                                               
         LR    R0,R2               SAVE PUB CODE POINTER                        
*                                                                               
         BRAS  RE,BUMP             BUMP TO PUB NAME FIELD                       
         BRAS  RE,CLRFLD           CLEAR AND TRANSMIT NAME                      
*                                                                               
         LR    R2,R0               RESTORE PUB CODE POINTER                     
*                                                                               
VPUBERRX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ  TURN OFF INDICATOR                        
*                                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         XIT1                      FOR DOWNLOADING                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISPUB'               
***********************************************************************         
*                                                                     *         
*        DISPLAY PUB CODE AND NAME                                    *         
*                                                                     *         
*NTRY    P0    A(PUB CODE)                                            *         
*        R2==> PUB CODE FIELD ON SCREEN                               *         
*                                                                     *         
*EXIT          PUB CODE FILLED IN FOR SCREEN FIELD                    *         
*              PUB DESCRIPTION IS IN NEXT PROTECTED FIELD             *         
*              BOTH FIELDS SET TO BE REDISPLAYED                      *         
*              PUB CODE FIELD SET AS PREVIOUSLY VALIDATED             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISPUB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           PUBABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             PUBABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      PUBABLISH TWA                                
         USING GEND,RC             PUBABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          PUBABLISH FIELD HEADER                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR PUB CODE FIELD                         
*                                                                               
         GOTO1 VPUBEDIT,DMCB,(0,QPUB),(C'S',FLDDATA) DISPLAY FLD                
*                                                                               
         LA    RF,FLDDATA+14-1     POINT TO LAST BYTE OF CODE                   
         LA    R0,14               MAX LENGTH                                   
*                                                                               
         CLI   0(RF),C' '          FIND LAST NON-BLANK                          
         BH    *+16                                                             
         AHI   RF,-1               BACK UP A POSITION                           
         BCT   R0,*-12                                                          
         B     VDISPUBX            NO PUB TO DISPLAY                            
*                                                                               
         LA    RE,FLDDATA          CALCULATE LENGTH OF DATA                     
         SR    RF,RE                                                            
         AHI   RF,1                                                             
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
         GOTOR VALPUB              VALIDATE MEDIA - FILLS IN NAME               
*                                                                               
VDISPUBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VALADC'                
***********************************************************************         
*                                                                     *         
*        VALIDATE AD CODE                                             *         
*                                                                     *         
*NTRY    R2==> AD CODE  FIELD ON SCREEN                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALADC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         XC    QADCODE,QADCODE     INIT AD CODE SAVEAREA                        
*                                                                               
         MVI   FLDOPT,C'Y'         FIELD IS OPTIONAL                            
*                                                                               
         GOTO1 GETFLD              MOVE INPUT TO WORKAREA                       
         BZ    VALADCX             DONE IF NO ENTRY                             
*                                                                               
*        READ AD RECORD                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH JOBREC KEY                         
         USING PJOBRECD,R4                                                      
*                                  BUILD KEY FOR JOBREC                         
         MVC   PJOBKAGY,AGENCY     AGENCY                                       
         MVC   PJOBKMED,QMED       MEDIA                                        
         MVI   PJOBKRCD,PJOBKIDQ   RECORD CODE                                  
         MVC   PJOBKCLT,QCLT       CLIENT                                       
         MVC   PJOBKPRD,QPRD       PRODUCT                                      
         MVC   PJOBKJOB,FLD        AD/JOB CODE                                  
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READING FOR UPDATE                       
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   PJOBKEY,KEYSAVE     MUST FIND RECORD                             
         BNE   VADCINVE                                                         
*                                                                               
         MVC   QADCODE,PJOBKJOB    SAVE AD CODE                                 
*                                                                               
VALADCX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
VADCINVE MVI   ERROR,PPEFLDNV      INVALID AD CODE                              
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISAD'                
***********************************************************************         
*                                                                     *         
*        DISPLAY AD  CODE                                             *         
*                                                                     *         
*NTRY    P0    A(AD  CODE)                                            *         
*        R2==> AD  CODE FIELD ON SCREEN                               *         
*                                                                     *         
*EXIT          AD  CODE FILLED IN FOR SCREEN FIELD                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISADC  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VDISADCX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VVALPER'               
***********************************************************************         
*                                                                     *         
*        VALIDATE PERIODE                                             *         
*                                                                     *         
*NTRY    R2==> PERIOD   FIELD ON SCREEN                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         CLI   5(R2),0             SKIP IF PERIOD FIELD IS EMPTY                
         BE    VVALPERX                                                         
*                                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING PERVALD,R4          ESTABLISH PERVAL WORKAREA                    
*                                                                               
         MVC   PVALBSTA,BTODAY     PASS TODAY'S DATE                            
*                                                                               
         GOTO1 PERVAL,DMCB,(5(R2),8(R2)),('PVINTOD',(R4)) VAL PERIOD            
         CLI   4(R1),0             CHECK FOR ERRORS                             
         BNE   VPERINVE                                                         
*                                                                               
         MVC   QSTART,PVALESTA    SAVE START AND END DATES                      
         MVC   QEND,PVALEEND          YYMMDD                                    
         MVC   BSTART,PVALBSTA    SAVE BINARY FORMAT                            
         MVC   BEND,PVALBEND       YMD                                          
         MVC   QPER,PVALBSTA                                                    
*                                                                               
         CLC   QEND,QSTART         START MUST BE BEFORE END                     
         BL    VPERDTSE                                                         
*                                                                               
VVALPERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPERINVE MVI   ERROR,PPEDTENV      INVALID DATE                                 
         B     VPERERR                                                          
*                                                                               
VPERDTSE MVI   ERROR,PPESTEND      END DATE BEFORE START DATE                   
*                                                                               
VPERERR  DS    0H                                                               
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISPER'               
***********************************************************************         
*                                                                     *         
*        DISPLAY PERIOD                                               *         
*                                                                     *         
*NTRY    P0    A(START DATE) - BINARY YMD                             *         
*        R2==> PERIOD   FIELD ON SCREEN                               *         
*                                                                     *         
*EXIT          PERIOD   FILLED IN FOR SCREEN FIELD                    *         
*              FIELD SET TO BE REDISPLAYED                            *         
*              FIELD SET AS PREVIOUSLY VALIDATED                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISPER  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2 ESTABLISH FIELD ON SCREEN                             
*                                                                               
         L     R3,0(R1)            POINT TO BINARY PERIOD DATES                 
*                                                                               
         BRAS  RE,CLRFLD           INIT THE OPUTPUT FIELD                       
*                                                                               
         OC    0(3,R3),0(R3)       SKIP IF NO DATES GIVEN                       
         BZ    VDISPERX                                                         
*                                                                               
*        INPUT INDICATORS                                                       
*              X'80' - RETURN DATE IN P2(1)                                     
*              X'10' - START AND END DATES GIVEN                                
*              X'03' - DATES IN BINARY FORMAT                                   
*        OUTPUT INDICATORS                                                      
*              17    - MMMDD/YY-MMMDD/YY                                        
*                                                                               
         GOTO1 DATCON,ESRPARMS,(X'93',0(R3)),(17,FLDDATA)                       
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE RE-WRITE                               
         MVC   FLDOLEN,4(R1)       SET OUTPUT LENGTH                            
*                                                                               
VDISPERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VERREX'                
***********************************************************************         
*                                                                     *         
*        POSITION CURSOR TO CORRECT FIELD IN ERROR                    *         
*                                                                     *         
*        INPUTS R2=A(SCREEN HEADER)                                   *         
*                                                                     *         
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL  *         
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG.  SINCE PPESR   *         
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE  *         
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VERREXIT NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
*        MOVE ERROR NUMBER IN ERROR TO PERROR IF REQUIRED                       
*                                                                               
         OI    GENSTAT2,USGETTXT   GENCON MUST CALL GETTXT, NOT GETMSG          
         OI    GENSTAT2,USMYERSY   USE MY ERROR MESSAGES                        
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         CLI   ERROR,0             IF OLD STYLE MESSAGE NUMBER                  
         BE    *+10                                                             
         MVC   PERROR+1(1),ERROR      PUT IN NEW STYLE                          
*                                                                               
         MVC   GTINDX,PINDEX       MESSAGE INDEX                                
         MVC   GTMSYS,GETMSYS      MESSAGE SYSTEM                               
         MVC   GTMSGNO,PERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,PMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,PTXTLEN      LENGTH OF OPTIONAL TEXT                      
         MVC   GTATXT,PTXTADR      A(OPTIONAL TEXT)                             
*                                                                               
         MVC   GTMSYS,GETMSYS      DEFAULT TO HOST MESSAGES                     
*                                                                               
         OI    6(R2),X'40'         POSITION CURSOR TO THIS FIELD                
         OI    CONHEADH+6,X'80'    ALWAYS TRANSMIT HEADER                       
*                                                                               
         CLI   DDLNKSW,C'Y'        SKIP IF NOT A LINK CALL                      
         BNE   VERRLNKN            NO                                           
*                                                                               
*        DISPLAY MESSAGE ON SCREEN BEFORE GOING TO LINKIO                       
*                                                                               
         LA    RE,CONHEADH         POINT TO ERROR MESSAGE AREA                  
         STCM  RE,7,GTAOUT         PASS TO GETTXT                               
         MVI   GTMAXL,L'CONHEAD    SET MAX MESSAGE LENGTH                       
*                                                                               
         GOTO1 GETTXT,GETTXTCB     DISPLAY ERROR MESSAGE                        
*                                                                               
         GOTOR LNKERR,DMCB,(RC)    HANDLE ERROR MESSAGE                         
*                                                                               
         CLI   DDLNKACT,DDLK_INQ   IF NOT INQUIRY                               
         BE    *+8                                                              
         CLI   DDLNKACT,DDLK_DWN   NOR DOWNLOAD                                 
         BNE   VERRX2                  EXIT WITHOUT RE-DISPLAYING MSG           
*                                                                               
         XIT1                      ELSE RETURN TO CALLER                        
*                                                                               
VERRLNKN DS    0H                                                               
*                                                                               
         CLI   ERROR,X'FE'         SKIP IF ERROR MESSAGE ALREADY SET            
         BE    VERRX2                                                           
*                                                                               
         GOTO1 ERREX               SYSTEM MESSAGE                               
*                                                                               
VERRX2   GOTO1 ERREX2              MY OWN ERROR MESSAGE                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - GETFLD'                
***********************************************************************         
*                                                                     *         
*                                                                     *         
* GETFLD ROUTINE - EXTRACT DATA FROM SCREEN FIELD                     *         
*                                                                     *         
*              INPUTS              R2=A(FIELD HEADER)                 *         
*                                  FLDOPT=1 FIELD IS OPTIONAL         *         
*              OUTPUTS             FLDH  CONTAINS FIELD HEADER        *         
*                                  FLD   FIELD DATA SPACE FILLED      *         
*                                  R0    BINARY VALUE IF FIELD NUMERIC*         
*                                  R1    FIELD LENGTH                 *         
*                                  CONDITION CODE ZERO IF R1=0        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VGETFLD  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         SR    R0,R0               PRE-CLEAR REGISTER FOR NUMERIC VALUE         
         SR    R1,R1               PRE-SET FIELD LENGTH                         
*                                                                               
*****    CLI   5(R2),0             SKIP IF NO INPUT DATA                        
*****    BE    GTFERRCK                                                         
*                                                                               
         MVC   FLDH,0(R2)          SAVE FIELD HEADER                            
         MVI   FLD,C' '            PRE-FILL WORKAREA WITH SPACES                
         MVC   FLD+1(L'FLD-1),FLD  FILL WITH SPACES                             
*                                                                               
         ZIC   R1,0(R2)            LENGTH OF FIELD IN TWA                       
*                                                                               
         SH    R1,=H'8'            SUBTRACT LENGTH OF HEADER                    
*                                                                               
         TM    FLDH+1,X'02'        TEST FOR EXTENDED HEADER                     
         BNO   *+8                                                              
         SH    R1,=H'8'            (SUBTRACT 8 MORE)                            
*                                                                               
         BCTR  R1,0                DECREMENT FOR EXECUTE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R2)        MOVE INPUT TO WORKAREA                       
*                                                                               
         LA    RE,FLD(R1)          POINT RE AT LAST EXTRACTED BYTE              
         LA    R1,1(R1)            RESTORE DATA LENGTH                          
*                                                                               
GETFLDLP CLI   0(RE),C' '          FIND ACTUAL DATA LENGTH                      
         BH    GETFLDDN                                                         
*                                                                               
         MVI   0(RE),C' '          MAKE SURE BINARY ZERO GOES TO BLANK          
*                                                                               
         BCTR  RE,0                BACK UP A BYTE                               
         BCT   R1,GETFLDLP         DECREMENT BYTE COUNTER                       
*                                                                               
GETFLDDN STC   R1,FLDH+5           SET ACTUAL DATA LENGTH                       
*                                                                               
         LTR   R1,R1               SKIP IF EMPTY FIELD                          
         BZ    GTFERRCK                                                         
*                                                                               
GETFLD4  LR    RE,R1               GET FLD LENGTH-1 IN R3                       
         BCTR  RE,0                                                             
*                                                                               
         CH    RE,=H'7'            LIMIT TO MAX 8 DIGITS                        
         BH    GTNOTNUM                                                         
*                                                                               
         MVC   WORK(8),=8X'F0'     TEST FOR NUMERIC FIELD                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
*                                                                               
         CLC   WORK(6),=6X'F0'                                                  
         BNE   GTNOTNUM            NOT NUMMERIC                                 
*                                                                               
         OI    FLDH+4,FINPNUM      INDICATE INPUT IS NUMERIC                    
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)          PACK INPUT                                   
*                                                                               
         CVB   R0,DUB              CVB                                          
*                                                                               
         LTR   R0,R0               CK IF INPUT=0                                
         BZ    GTNOTNUM                                                         
*                                                                               
         B     GETFLDX                                                          
*                                                                               
GTNOTNUM SR    R0,R0               NON-NUMERIC. SO SET R0 TO 0                  
         NI    FLDH+4,X'FF'-FINPNUM MAKE SURE NOT FLAGGED AS NUMERIC            
*                                                                               
         B     GETFLDX                                                          
*                                                                               
GTFERRCK DS    0H                  NO INPUT                                     
*                                                                               
         CLI   FLDOPT,C'Y'         IS THIS OK?                                  
         BNE   GFLDERR             INPUT RERQUIRED                              
*                                                                               
         XC    FLD,FLD             CLEAR FIELD                                  
*                                                                               
         B     GETFLDX                                                          
*                                                                               
GETFLDX  MVI   FLDOPT,C'N'         RESET OPTIONAL BITS                          
         LTR   R1,R1               SET CC                                       
*                                                                               
         XIT1  REGS=(R0,R1)        RETURN R0 & R1                               
*                                                                               
GFLDERR  DS    0H                                                               
*                                                                               
         MVI   ERROR,PPEFLDNE      SET MISSING ERROR MSG                        
         GOTO1 ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VMININIT'              
***********************************************************************         
*                                                                     *         
*        INITIALIZE MINIO BLOCK                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VMININIT DS    0H                                                               
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R0,MNBLKCB          CLEAR MINBLOCK AREA                          
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    MNRTAB,MNRTAB       CLEAR RECORD  TABLE                          
         XC    MNELEM,MNELEM       CLEAR ELEMENT WORKAREA                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO BLOCK                        
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   MINRECUP,VRECUP     V(RECUP)                                     
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,SYSFIL       FILE NAME                                    
         MVC   MINDIR,SYSDIR       DIR NAME                                     
         MVC   MINFKLEN,LKEY+1     KEY LENGTH                                   
*                                                                               
         LA    R1,75               SET SPLIT PERCENTAGE TO 75%                  
         STCM  R1,3,MINSPCT        IE. FULL RECORD RETAINS 75% OF IT'S          
*                                  ELEMENTS ON SPLITING                         
*                                                                               
         MVI   MINNCTL,L'ESRDCNTL  2 CONTROL BYTES                              
         LHI   R1,2976                                                          
         STCM  R1,3,MINFRCLM       MAX RECORD LENGTH                            
*                                                                               
         MVI   MINEKLEN,L'ESRKELMK      ELEMENT KEY LENGTH                      
         MVI   MINEKDSP,ESRKELMK-ESRKEY ELEMENT KEY DISPLACEMENT                
*                                                                               
         MVC   MINBUFF,AIO3        A(FIRST MINIO BUFFER)                        
         MVI   MINNBUF,2           NUMBER OF BUFFERS                            
*                                                                               
         LA    R1,MNELEM           A(ELEMENT AREA)                              
         ST    R1,MINELEM                                                       
         LHI   R1,L'MNELEM         MAX ELEMENT/CLUSTER LENGTH                   
         STCM  R1,3,MINMAXEL                                                    
*                                                                               
         LA    R1,MNRTAB           A(RECORD TABLE)                              
         ST    R1,MINRTAB                                                       
         LHI   R1,L'MNRTAB                                                      
         STCM  R1,3,MINRTABL       LENGTH OF RECORD TABLE                       
*                                                                               
VMININIX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VADDELM'               
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - PARM 1 A(ELEMENT TO BE ADDED)                                *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT ADDED                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
VADDELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE ADDED                 
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE ADDED)                       
*                                                                               
         GOTO1 VMINIO,ESRPARMS,('MINADD',MINBLKD)                               
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
VADDELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDELELM'               
***********************************************************************         
* ROUTINE TO ADD AN ELEMENT TO A RECORD                               *         
*                                                                     *         
* NTRY - PARM 1 A(ELEMENT TO BE DELETED)                              *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT DELETED                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDELELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE DELETED               
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE DELETED)                     
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,MINEKLEN       GET KEY LENGTH                               
*                                                                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SET REST OF ELEMENT KEY                      
*                                                                               
         GOTO1 VMINIO,ESRPARMS,('MINDEL',MINBLKD)                               
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC FOR RETURN                            
*                                                                               
DELELSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VGETELM'               
***********************************************************************         
* ROUTINE TO GET AN ELEMENT IN A RECORD                               *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE FOUND)                         *         
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE    *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VGETELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINRD            DEFAULT TO DIRECT READ                       
*                                                                               
         CLI   1(R6),0             USE DEFAULT IF NO LENGTH GIVEN               
         BE    VGETELM1                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       USE READ IF GREATER THAN KEY LENGTH          
         BNL   VGETELM1                                                         
*                                                                               
         LA    R0,MINHI            SET FOR READ HI/EQUAL                        
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VGETELM1 DS    0H                                                               
*                                                                               
         ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTO1 VMINIO,ESRPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VGETELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VWRTELM'               
***********************************************************************         
* ROUTINE TO REPLACE ELEMENT IN A MINIO SET                           *         
*       DELETES AND ADDS NEW ELEMENT WILL NOT GET UPSET IF THERE      *         
*         IS NO PRIOR ELEMENT                                         *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF ELEMENT TO BE REPLACED)                      *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT REPLACED                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VWRTELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE REPLACED              
*                                                                               
         ICM   R0,15,MINELEM       SAVE A(ELEMENT RETURN AREA)                  
*                                                                               
         ST    R6,MINELEM          A(ELEMENT TO BE ADDED)                       
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         GOTO1 VMINIO,ESRPARMS,('MINWRT',MINBLKD)                               
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         STCM  R0,15,MINELEM       RESTORE A(ELEMENT RETURN AREA)               
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VWRTELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VNXTELM'               
***********************************************************************         
* ROUTINE TO GET NEXT ELEMENT IN A RECORD                             *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                    *         
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE    *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VNXTELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINSEQ           SEQUENTIAL READ                              
*                                                                               
         CLI   1(R6),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    VNXTELM1                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VNXTELM1 DS    0H                                                               
*                                                                               
         ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         GOTO1 VMINIO,ESRPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VNXTELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VPRVELM'               
***********************************************************************         
* ROUTINE TO GET PREVIOUS ELEMENT IN THE RECORD                       *         
*                                                                     *         
* NTRY - PARM 1 A(KEY OF LAST ELEMENT TO BE FOUND)                    *         
*               IF LENGTH OF ELEMENT PROVIDED KEY MATCHING IS DONE    *         
*                  FOR ONLY THAT AMOUNT OF KEY                        *         
*                                                                     *         
*        R7==>  MINIO BLOCK                                           *         
*                                                                     *         
* EXIT   CC    NEQ - ERROR OCCURRED                                   *         
*              EQ  - ELEMENT FOUND                                    *         
*                                                                     *         
*        MINELEM     A(ELEMENT FOUND)                                 *         
*                    ELEMENT FOUND WILL BE NULLS IF NOT FOUND         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VPRVELM  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         L     R6,0(R1)            POINT TO ELEMENT TO BE FOUND                 
*                                                                               
         IC    R2,MINFILTL         SAVE CURRENT FILTER VALUE                    
         MVI   MINFILTL,0          INIT FILTER LENGTH                           
*                                                                               
         ICM   RF,15,MINELEM       INIT ELEMENT RETURN AREA                     
         BZ    *+10                                                             
         XC    0(L'MNELEM,RF),0(RF)                                             
*                                                                               
         XC    MINEKEY,MINEKEY     INIT ELEMENT KEY AREA                        
*                                                                               
         MVC   MINEKEY(1),0(R6)    BUILD ELEMENT KEY                            
*                                                                               
         LA    R0,MINBSQ           BACKWARD SEQUENTIAL READ                     
*                                                                               
         CLI   1(R6),0             NO FILTERING IF NO LENGTH GIVEN              
         BE    VPRVELM1                                                         
*                                                                               
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         BCTR  RF,0                GET SEARCH LENGTH                            
*                                                                               
         CLM   RF,1,MINEKLEN       SKIP IF GREATER THAN KEY LENGTH              
         BNL   *+8                                                              
         STC   RF,MINFILTL         SET FILTER LENGTH                            
*                                                                               
VPRVELM1 DS    0H                                                               
*                                                                               
         ZIC   RF,MINEKLEN         GET KEY LENGTH                               
         SH    RF,=H'2'            DECREMENT FOR EXECUTE                        
         EX    RF,*+8              SET REST OF KEY                              
         B     *+10                                                             
         MVC   MINEKEY+1(0),2(R6)  SETS REST OF ELEMENT KEY                     
*                                                                               
         GOTO1 VMINIO,ESRPARMS,((R0),MINBLKD)                                   
*                                                                               
         STC   R2,MINFILTL         RESTORE CURRENT FILTER VALUE                 
*                                                                               
         CLI   MINERR,0            SET CC                                       
*                                                                               
VPRVELMX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VNXTSR#'               
***********************************************************************         
*                                                                     *         
*        FIND AND RESERVE THE NEXT AVAILABLE ESR     SERIAL NUMBER    *         
*                                                                     *         
*        IF THERE ARE NO ESR   S  ON FILE                             *         
*           START NUMBERING AT 1                                      *         
*                                                                     *         
*        ROUTINE READS FILE FOR PASSIVE POINTER THAT HAS              *         
*           SERIAL NUMBERS IN 2'S COMPLEMENT. READS LOWEST NUMBER     *         
*           (REALLY HIGHEST) FOR UPDATE AND THEN ADDS POINTER FOR     *         
*           NEXT NUMBER. THIS RESERVES NEXT NUMBER FOR THIS CALL TO   *         
*           THE SUBROUTINE.                                           *         
*           IF THIS RESULTS IN A DUPLICATE KEY THE PROCESS IS         *         
*           REPEATED.                                                 *         
*                                                                     *         
*        THE SETUP RECORD DESCRIBES THE RANGE WHERE                   *         
*              THE NUMBER IS UNIQUE                                   *         
*                                                                     *         
*EXIT    QSR#   =  FOUND NEW SERIAL NUMBER                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VNXTSR#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS INV SR# PASSIVE             
         USING ESR#SR#D,R4                                                      
*                                                                               
NXTSERLP DS    0H                                                               
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
*                                                                               
         MVC   ESR#AGY,QAGY        SET AGENCY                                   
         MVC   ESR#MED,QMED        SET MEDIA                                    
         MVI   ESR#RCD,ESR#RCDQ    SET RECORD TYPE                              
*                                                                               
*        CHECK SETUP HERE TO SEE IF CLIENT/PUB INCLUDED IN KEY                  
*          FOR NOW ASSUME CLIENT AND PUB ARE                                    
*                                                                               
         MVC   ESR#CLT,QCLT        SET CLIENT                                   
         MVC   ESR#PUB,=6X'FF'     SET PUB TO HIGH VALUES                       
*                                                                               
         MVC   ESR#SRYR,QPER       SET YEAR OF ESR    PERIOD                    
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         CLC   ESR#KEY(ESR#SRSQ-ESR#SR#D),KEYSAVE SKIP IF ONE FOUND             
         BE    NXTSER1             FOR YEAR                                     
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE STARTING KEY                         
*                                                                               
         MVC   ESR#SRSQ,=X'000001'   START SEQUENCE AT 1                        
         XC    ESR#SRSQ,=X'FFFFFF'   2'S COMPLEMENT                             
*                                                                               
         B     NXTSER2                                                          
*                                                                               
*        READ RECORD AND RESERVE NEXT SR#                                       
*                                                                               
NXTSER1  DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'88'       READ FOR UPDATE AND DELETED                  
*                                                                               
         GOTO1 READ                READ FOR UPDATE TO LOCK BLK OF REC'D         
*                                                                               
         NI    DMINBTS,X'FF'-X'88'       RESET                                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,ESR#SRSQ       GET CURRENT SEQ NUMBER                       
         SHI   RF,1                DECREMENT BY ONE                             
         STCM  RF,7,ESR#SRSQ       RESET SEQUENCE NUMBER                        
*                                                                               
NXTSER2  DS    0H                                                               
*                                                                               
         OI    GENSTAT4,NODUPDIE   DON'T DIE ON DUPLICATE KEY ON ADD            
*                                                                               
         GOTO1 ADD                 ADD TO FILE                                  
*                                                                               
         CLI   DMCB+8,0            DONE IF NO DMGR ERRORS                       
         BE    NXTSERDN                                                         
*                                                                               
         TM    DMCB+8,X'20'        OKAY IF DUPE KEY FOUND                       
         BO    *+6                                                              
         DC    H'0'                DUPE RECORD ONLY ERROR ALLOWED               
*                                                                               
NXTSERCN DS    0H                                                               
*                                                                               
         B     NXTSERLP            REPEAT SEARCH FOR NEXT #                     
*                                                                               
NXTSERDN DS    0H                                                               
*                                                                               
         MVC   QSR#SRYR,ESR#SRYR   SAVE SR# YEAR                                
         MVC   QSR#SRSQ,ESR#SRSQ   NEXT SERIAL NUMBER                           
         XC    QSR#SRSQ,=X'FFFFFF'   COMPLEMENT                                 
*                                                                               
         NI    GENSTAT4,X'FF'-NODUPDIE   RESET                                  
*                                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT KEY                          
         GOTO1 HIGH                RESTORE FILE POINTERS                        
*                                                                               
VNXTSERX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VVALREP'               
***********************************************************************         
*                                                                     *         
*        VALIDATE REP CODE                                            *         
*                                                                     *         
*                                                                     *         
*EXIT    QREP   =  REP CODE                                           *         
*        REPNM =   REP NAME                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VVALREP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         XC    QREP,QREP           INIT REP CODE SAVEAREA                       
         XC    REPNM,REPNM         INIT REP NAME SAVEAREA                       
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VREPCLRX                                                         
*                                                                               
         LR    R0,R2               SAVE FIELD POINTER                           
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
         BRAS  RE,CLRFLD           CLEAR REP NAME FIELD                         
         LR    R2,R0               RESTORE FIELD POINTER                        
*                                                                               
VREPCLRX DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BNZ   *+16                HAVE INPUT                                   
         CLI   FLDOPT,C'Y'         IF INPUT IS OPTIONAL                         
         BE    VVALREPX               NO INPUT IS OKAY                          
         B     VREP2E              ELSE INPUT REQUIRED                          
*                                                                               
*                                                                               
*        READ REP    RECORD                                                     
*                                                                               
         XC    KEY,KEY             INIT KEY BUILD AREA                          
         LA    R4,KEY                                                           
         USING PREPRECD,R4         ESTABLISH CLIENT RECORD KEY                  
*                                  BUILD CLIENT KEY                             
         MVC   PREPKAGY,AGENCY     AGENCY                                       
         MVC   PREPKMED,QMED       MEDIA                                        
         MVI   PREPKRCD,PREPKIDQ   CLIENT RECORD CODE                           
*                                                                               
         ZIC   R1,FLDILEN          R1=LENGTH OF ENTERED REP CODE                
         AHI   R1,-1                                                            
         EX    R1,VVREP10                                                       
         J     VVREP20                                                          
VVREP10  PACK  DUB,FLDDATA(0)                                                   
VVREP20  DS    0H                                                               
         UNPK  PREPKREP,DUB        REP CODE                                     
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTOR HIGH                READ CLIENT POINTER                          
*                                                                               
         CLC   PREPKEY,KEYSAVE     MUST FIND REP RECORD                         
         BNE   VREPINVE                                                         
*                                                                               
         L     R4,AIO1             READ REP RECORD INTO IOA1                    
         ST    R4,AIO                                                           
         GOTOR GETREC                                                           
*                                                                               
         LR    R6,R4               POINT TO STAT OF RECORD                      
         MVI   ELCODE,X'11'                                                     
*                                                                               
         BRAS  RE,GETEL            FIND ELEMENT                                 
         BNE   VREPINVE            MUST FIND ELEMENT                            
*                                                                               
         USING PREPELEM,R6         REP ELEMENT                                  
*                                                                               
         MVC   QREP,PREPKREP       SAVE REP CODE                                
         MVC   REPNM,PREPNAME      SAVE REP NAME                                
*                                                                               
         TM    VALOPT,VALNAMXQ     SKIP IF NAME NOT TO BE DISPLAYED             
         BO    VREPNAMX                                                         
*                                                                               
         BRAS  RE,BUMP             BUMP TO NAME FIELD                           
*                                                                               
         MVC   FLDDATA(L'REPNM),REPNM DISPLAY REP NAME                          
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
         MVI   FLDOLEN,L'REPNM     SET LENGTH OF OUTPUT                         
*                                                                               
VREPNAMX DS    0H                                                               
*                                                                               
         NI    VALOPT,X'FF'-VALNAMXQ TURN OF INDICATOR                          
*                                                                               
VVALREPX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VREP2E   LHI   RF,PPEFLDNE         REP IS REQUIRED                              
         B     VREPERR                                                          
*                                                                               
VREPINVE LHI   RF,PPEREPNV         INVALID REP                                  
         B     VREPERR                                                          
*                                                                               
VREPERR  DS    0H                                                               
*                                                                               
         XC    ERROR,ERROR                                                      
*                                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
         GOTOR ERREXIT                                                          
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDISREP'               
***********************************************************************         
*                                                                     *         
*        DISPLAY  REP CODE                                            *         
*                                                                     *         
*                                                                     *         
*EXIT    QREP   =  REP CODE                                           *         
*        REPNM =   REP NAME                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VDISREP  NTR1  BASE=*,LABEL=*                                                   
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR REP CODE FIELD                         
*                                                                               
*                                                                               
         LA    RF,L'QREP           SET LENGTH OF QREP CODE                      
         STC   RF,FLDILEN          SET FIELD LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),QREP     DISPLAY REP CODE                             
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDOLEN          GET OUTPUT LENGTH                            
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
         GOTOR VALREP              FILL IN REP NAME                             
*                                                                               
VDISREPX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VPSSVS '               
***********************************************************************         
*                                                                     *         
*        CREATE PASSIVE POINTERS                                      *         
*                                                                     *         
*NTRY    R7 ==> MINIO SET GETTING PASSIVES                            *         
*                                                                     *         
*EXIT                                                                 *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VPSSVS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT DIRECTORY KEY                   
*                                                                               
         IC    R0,DMINBTS          SAVE SETTING                                 
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO SET OPEN                       
         BE    PSVOPENX                                                         
*                                                                               
         GOTO1 VMINIO,ESRPARMS,('MINOPN',MINBLKD) OPEN MINIO SET                
*                                                                               
PSVOPENX DS    0H                                                               
*                                                                               
*        FIND DISK ADDRESS OF MASTER RECORD                                     
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS MASTER KEY                      
         USING ESRKEY,R4                                                        
*                                                                               
         MVC   ESRKEY,MINMKEY      COPY MINIO MASTER KEY                        
         MVC   ESRKELMK,=8X'FF'    SET TO MAX ELEMENT KEY                       
*                                                                               
         GOTO1 HIGH                READ MASTER KEY                              
*                                                                               
         CLC   ESRKEY,KEYSAVE      CHECK IF KEY FOUND                           
         BE    *+6                                                              
         DC    H'0'                MUST FIND KEY                                
*                                                                               
         MVC   QSR#,ESRKSR#        SAVE INVOICE SERIAL   #                      
         MVC   QREV#,ESRKRV#       SAVE INVOICE REVISION #                      
         MVC   QDISK,ESRDDISK      SAVE DISK ADDR OF MASTER REC                 
*                                                                               
*        READ HEADER ELEMENT - USED FOR MAJOR KEY FIELDS                        
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT          ESTABLISH WORK ELM AS HEADER                 
         USING ESRHDRD,R6                                                       
*                                                                               
         MVI   ESRHKCDE,ESRHKIDQ   SET FOR HEADER ELEMENT                       
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ HEADER ELEMENT                          
         BZ    *+6                 MUST FIND IT                                 
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
*        ADD FIRST PASSIVE KEY                                                  
                                                                                
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS 1ST PASSIVE                     
         USING ESR1KEY,R4                                                       
*                                                                               
         MVC   ESR1AGY,ESRKAGY-ESRKEY+MINMKEY  SET AGENCY                       
         MVC   ESR1MED,ESRKMED-ESRKEY+MINMKEY  SET MEDIA                        
         MVI   ESR1RCD,ESR1RCDQ    SET PASSIVE CODE                             
         MVC   ESR1CLT,ESRKCLT-ESRKEY+MINMKEY     SET CLIENT                    
         MVC   ESR1PRD,ESRHPRD                    SET PRODUCT                   
         MVC   ESR1PUB,ESRKPUB-ESRKEY+MINMKEY     SET PUB                       
         GOTOR DATCON,DMCB,(3,ESRHEND),(2,ESR1END)    SET END   DATE            
         GOTOR DATCON,DMCB,(3,ESRHSTRT),(2,ESR1STRT)  SET START DATE            
         MVC   ESR1SR#,ESRKSR#-ESRKEY+MINMKEY     SET SR#                       
         MVC   ESR1RV#,ESRKRV#-ESRKEY+MINMKEY     SET SR#                       
*                                                                               
         GOTO1 HIGH                READ PASSIVE                                 
                                                                                
         CLC   ESR1KEY,KEYSAVE     IF PASSIVE ON FILE                           
         BNE   PSVPS1NF                                                         
                                                                                
         TM    ESR1CNTL,ESRDDELQ      RESTORE IF DELETED                        
         BO    *+14                                                             
         CLC   ESR1DISK,QDISK         DONE IF DISK ADDR SAME                    
         BE    PSVPS1X                                                          
                                                                                
         MVC   ESR1DISK,QDISK         ELSE SET NEW DISK ADDR                    
         NI    ESR1CNTL,X'FF'-ESRDDELQ     FORCE NON-DELETED                    
                                                                                
         GOTO1 WRITE                  RE-WRITE THE PASSIVE                      
                                                                                
         B     PSVPS1X                                                          
                                                                                
PSVPS1NF DS    0H                  PASSIVE NOT ON FILE                          
                                                                                
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVC   ESR1DISK,QDISK      SET DISK ADDRESS OF MASTER REC               
                                                                                
         GOTO1 ADD                 ADD PASSIVE TO FILE                          
                                                                                
PSVPS1X  DS    0H                                                               
*                                                                               
         GOTO1 VMINIO,ESRPARMS,('MINCLS',MINBLKD) CLOSE MINIO SET               
*                                                                               
         STC   R0,DMINBTS          RESTORE SETTING                              
*                                                                               
VPSSVSX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42030 - INVOICE COMMENTS MAINT/LIST - TRNPID'                  
***********************************************************************         
*                                                                     *         
*        TRANSLATE PID TO A NAME                                      *         
*                                                                     *         
*NTRY    R2 ==>   SCREEN FIELD                                        *         
*        P1       A(PID)                                              *         
*        P2+0(1)  L'RETURN AREA  IF R2 = 0                            *         
*        P2+1(3)  A(RETURN AREA) IF R2 = 0                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VTRNPID  NTR1  BASE=*,LABEL=*      VALIDATE KEY ROUTINE                         
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         L     R5,0(R1)            POINT TO PID                                 
*                                                                               
         SR    R6,R6                                                            
         IC    R6,4(R1)            SAVE LENGTH OF RETURN AREA                   
         L     R3,4(R1)            POINT TO RETURN AREA                         
*                                                                               
         LTR   R2,R2               IF R2 GIVEN                                  
         BZ    *+8                                                              
         LA    R3,FLDDATA             USE DATAAREA OF SCREEN FIELD              
*                                                                               
         OC    0(2,R5),0(R5)       SKIP IF NO PID FOUND                         
         BZ    TPIDNOTF                                                         
*                                                                               
*        READ PERSON AUTH REC ON CTFILE                                         
*                                                                               
         LTR   R2,R2               IF R2 GIVEN                                  
         BZ    *+8                                                              
         BRAS  RE,CLRFLD              INIT OUTPUT                               
*                                                                               
         LA    R4,KEY                                                           
         USING CT0REC,R4           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
*                                                                               
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   CT0KAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   CT0KAGY,QAGY           USE BUYREC'S AGENCY                       
*                                                                               
         MVC   CT0KNUM,0(R5)       SET PID                                      
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         CLC   CT0KEY,KEYSAVE      SKIP IF RECORD NOT FOUND                     
         BNE   TPIDNOTF                                                         
*                                                                               
*        FIND USER'S ID                                                         
*                                                                               
*        FIND PERSON'S ID ELEMENT                                               
*                                                                               
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
TPIDCTLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDCTDN                                                         
*                                                                               
         CLI   0(RE),X'C3'         - MATCH ON ELEMENT CODE                      
         BE    TPIDCTFD                                                         
*                                                                               
TPIDCTCN DS    0H                                                               
*                                                                               
         IC    RF,1(RE)            GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDCTLP            GO FIND NEXT ELEMENT                         
*                                                                               
TPIDCTDN DS    0H                  NO PERSON ID FOUND                           
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDCTFD DS    0H                                                               
*                                                                               
*        FIND PERSON RECORD                                                     
*                                                                               
         LA    R4,KEY                                                           
         USING SAPEREC,R4          ESTABLISH KEY AS PERSON REC KEY              
         XC    SAPEKEY,SAPEKEY     INIT KEY                                     
*                                                                               
         MVI   SAPETYP,SAPETYPQ    SET RECORD TYPE                              
         MVI   SAPESUB,SAPESUBQ    SET RECORD SUB TYPE                          
*                                                                               
         MVC   SAPEAGY,SVSECAGY    SET SECURITY AGENCY                          
*                                                                               
         CLC   SAPEAGY,=CL2' '     IF SECURITY AGENCY NOT PRESENT               
         BH    *+10                                                             
         MVC   SAPEAGY,QAGY           USE BUYREC'S AGENCY                       
*                                                                               
         MVC   SAPEPID,2(RE)       SET USERID FROM PREVIOUS RECORD              
*                                                                               
         MVC   FILENAME,=CL8'CTFILE'    SET FILENAME                            
         MVC   AIO,AIO2                 READ RECORD INTO IOA2                   
         MVI   USEIO,C'Y'               READ INTO I/O AREA                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         XC    FILENAME,FILENAME   RESET FILE NAME                              
         MVI   USEIO,0             RESET SWITCH                                 
*                                                                               
         L     R4,AIO2             POINT TO FOUND RECORD                        
*                                                                               
         CLC   SAPEKEY(SAPEDEF-SAPEKEY),KEYSAVE SKIP IF REC NOT FOUND           
         BNE   TPIDNOTF                                                         
*                                                                               
         LA    RE,SAPEDATA         POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
*                                                                               
*        FIND NAME ELEMENT                                                      
*                                                                               
TPIDNMLP DS    0H                                                               
*                                                                               
         CLI   0(RE),0             CHECK FOR END OF RECORD                      
         BE    TPIDNMDN                                                         
*                                                                               
         USING SANAMD,RE           ESTABLISH AS NAME ELEMENT                    
*                                                                               
         CLI   SANAMEL,SANAMELQ    LOOKING FOR NAME ELEMENT                     
         BE    TPIDNMFD                                                         
*                                                                               
TPIDNMCN DS    0H                                                               
*                                                                               
         IC    RF,SANAMLN          GET ELEMENT LENGTH                           
         AR    RE,RF               BUMP TO NEXT ELEMENT                         
         B     TPIDNMLP            GO PROCESS NEXT ELEMENT                      
*                                                                               
TPIDNMDN DS    0H                  NAME ELEMENOT FOUND                          
*                                                                               
         B     TPIDNOTF                                                         
*                                                                               
TPIDNMFD DS    0H                                                               
*                                                                               
         SR    R0,R0               GET ELEMENT LENGTH                           
         IC    R0,SANAMLN                                                       
         AHI   R0,-SANAMLNQ        DECRMENT BY FIXED LENGTH                     
         BNP   TPIDNOTF            NO NAME IN ELEMENT                           
*                                                                               
         LA    RE,SANAMES          POINT TO START OF PERSON'S NAME              
         SR    RF,RF                                                            
         LA    R1,WORK             BUILD NAME IN WORKAREA                       
         XC    WORK,WORK                                                        
*                                                                               
TPIDFMLP DS    0H                  FORMAT PERSON'S NAME                         
*                                                                               
         USING SANAMES,RE          ESTABLISH NAMES SECTION                      
*                                                                               
         IC    RF,SANAMELN         GET LENGTH OF THIS PART OF NAME              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SANAME      MOVE OUT PART OF NAME                        
*                                                                               
         LA    R1,1(RF,R1)         BUMP TO NEXT OUTPUT AREA                     
*                                                                               
TPIDFMCN DS    0H                                                               
*                                                                               
         SR    R0,RF               DECREMENT REMAINING ELEMENT LENGTH           
         AHI   R0,-2               FOR NAME LENGTH BYTE & EX LENGTH             
         BNP   TPIDFMDN              END OF ELEMENT REACHED                     
*                                                                               
         LA    RE,2(RF,RE)         POINT TO NEXT PART OF NAME                   
         LA    R1,1(R1)            ADD IN A SPACING CHARACTER                   
*                                                                               
         B     TPIDFMLP                                                         
*                                                                               
TPIDFMDN DS    0H                                                               
*                                                                               
         B     TPIDSQSH                                                         
*                                                                               
TPIDNOTF DS    0H                  PRINT 'UNKNOWN' IF NO PID                    
*                                                                               
         MVC   WORK(7),=CL7'UNKNOWN'                                            
         LA    R1,WORK+7           POINT TO NEXT OUTPUT POSITION                
*                                                                               
TPIDSQSH DS    0H                                                               
*                                                                               
         LR    R0,R1               END OF OUTPUT MINUS START                    
         LA    RF,WORK             START OF WORKAREA                            
         SR    R0,RF               EQUALS OUTPUT LENGTH                         
*                                                                               
         GOTO1 SQUASHER,DMCB,WORK,(R0) SQUASH NAME                              
*                                                                               
*        MOVE NAME TO SCREEN                                                    
*                                                                               
         LTR   R2,R2               IF NO SCREEN FIELD GIVEN                     
         BNZ   TPIDSCR                                                          
*                                                                               
         LR    RF,R6                  GET RETURN AREA LENGTH                    
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES         INIT OUT PUT AREA                         
*                                                                               
         L     RF,4(R1)               SAVE SQUASHED LENGTH                      
*                                                                               
         CR    RF,R6                  IF NAME TOO LONG                          
         BNH   *+6                                                              
         LR    RF,R6                     USE MAX FOR RETURN AREA                
*                                                                               
         B     TPIDMVC                                                          
*                                                                               
TPIDSCR  DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
TPIDMVC  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        DISPLAY NAME                                 
*                                                                               
TRNPIDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR TRANSFER TO PFM - VGOPFM'                  
***********************************************************************         
*                                                                     *         
*        TRANSFER TO PFM                                              *         
*                                                                     *         
*        R7    ==> MINIO BLOCK                                        *         
*                                                                     *         
*        ROUTINE GLOBBERS TO PFM AND DISPLAYS CURRENT ELEMENT         *         
*        NEEDS TO FIND THE MINIO RECORD THAT HOLDS THE ELEMENT        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VGOPFM   NTR1  BASE=*,LABEL=*      DISPLAY RECORD                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON WORKING STORAGE             
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING SYSD,R9             ESTABLISH SYSTEM WORKING STORAGE             
         USING SPOOLD,R8           ESTABLISH SPOOL WORKING STORAGE              
*                                                                               
         USING MINBLKD,R7          ESTABLISH MINIO BLOCK                        
*                                                                               
*        TRANSFER TO PFM                                                        
*                                                                               
         XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R3,WORK                                                          
         USING GLVXFRSY,R3                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'ESR'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'PFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
         GOTO1 VGLOBBER,ESRPARMS,=C'PUTD',WORK,14,GLVXCTL SEND XCTL ELM         
*                                                                               
         XC    WORK,WORK                                                        
         LA    R3,WORK             ESTABLISH PFM CONTROL BLOCK                  
         USING GLPFMD,R3                                                        
*                                                                               
         MVI   GLPFMCD,GLPFMCDQ    SET TRANSFER CODE                            
         MVI   GLPFMLEN,56         SET CONTROL BLOCK LENGTH                     
         MVC   GLPFMFIL,=CL8'PRTDIR'  SET FILE                                  
*                                                                               
         MVC   GLPFMKEY(L'ESRKEY-L'ESRKELMK),MINMKEY  MASTER KEY                
*                                                                               
         GOTO1 VGLOBBER,ESRPARMS,=C'PUTD',GLPFMFIL,54,GLVPFM SEND DATA          
*                                                                               
VGOPFMX  DS    0H                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR VALIDATE SR# - VVALSR#'                    
***********************************************************************         
*                                                                     *         
*        VALIDATE ENHANCED SR #                                       *         
*                                                                     *         
*NTRY    R2==> SR# FIELD ON SCREEN                                    *         
*              ASSUMED TO BE OF FORM YYNNNN                           *         
*                ANY 6 DIGITS WILL BE ACCEPTED                        *         
*                                                                     *         
*EXIT    QSR#  - INSERTION ORDER NUMBER                               *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VVALSR#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         XC    QSR#,QSR#           INIT SR# SAVEAREA                            
*                                                                               
         CLI   FLDILEN,3           INPUT SHOULD BE 3-7 CHAR                     
         BL    VSR#NOTV                                                         
*                                                                               
         CLI   FLDILEN,7           INPUT SHOULD BE 3-7 CHAR                     
         BH    VSR#NOTV                                                         
*                                                                               
         PACK  DUB,FLDDATA(2)      PACK YEAR                                    
         TP    DUB                 MAKE SURE ITS A PACKED NUMBER                
         BNZ   VSR#NOTV            INVALID IF NOT                               
*                                                                               
         CVB   RF,DUB              CVB                                          
         AHI   RF,100              PUT IN THIS CENTURY                          
         STC   RF,QSR#SRYR         SAVE YEAR                                    
*                                                                               
         LLC   RF,FLDILEN          GET INPUT LENGTH                             
         SHI   RF,2                BYPASS YEAR DIGITS                           
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLDDATA+2(0)    PACK NUMBER                                  
*                                                                               
         TP    DUB                 MAKE SURE ITS A PACKED NUMBER                
         BNZ   VSR#NOTV            INVALID IF NOT                               
*                                                                               
         CVB   RF,DUB              CVB                                          
         STCM  RF,7,QSR#SRSQ       SAVE NUMBER                                  
*                                                                               
VVALSR#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
VSR#NOTV DS    0H                                                               
         LHI   RF,PPEIO#NV         STATUS CODE NOT VALID                        
         B     VSR#ERR                                                          
*                                                                               
VSR#ERR  DS    0H                                                               
         STCM  RF,3,PERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR VALIDATE STATUS - VDISSR#'                 
***********************************************************************         
*                                                                     *         
*        DISPLAY  ENHANCED SR #                                       *         
*                                                                     *         
*NTRY    R2==> SR# ON SCREEN                                          *         
*        QSR#  SR# TO BE DISPLAYED                                    *         
*                                                                     *         
*EXIT          SR# CODE FILLED IN FOR SCREEN FIELD                    *         
*              FIELD SET TO BE REDISPLAYED                            *         
*              SR# CODE FIELD SET AS PREVIOUSLY VALIDATED             *         
*              FULL SR# IN QSR#EXP                                    *         
*              QSR#EXP OF FORM M-CCCYYNNNN(N)-REVNNN                  *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VDISSR#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QSR#EXP,SPACES      INIT SR# EXPANSION                           
*                                                                               
         BRAS  RE,CLRFLD           INIT SR# FIELD                               
*                                                                               
*        SR# IS YYNNNN(N)                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QSR#SRYR         GET YEAR                                     
         SHI   RF,100              ALLOW FOR THIS CENTURY                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  FLDDATA(2),DUB      DISPLAY YEAR                                 
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,QSR#SRSQ       GET SEQUENCE NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                DEFAULT TO 4 DIGITS                          
         CP    DUB,=P'9999'        IF OVER 9999                                 
         BNH   *+8                                                              
         LA    RF,5                   USE 5 DIGITS                              
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                MOVE TO LEFT NYBBLE                          
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  FLDDATA+2(0),DUB    DISPLAY SEQUENCE NUMBER                      
*                                                                               
         SRL   RF,4                MOVE LENGTH TO RIGHT NYBBLE                  
         AHI   RF,1                TRUE LENGTH                                  
         AHI   RF,2                LENGTH OF YEAR DIGITS                        
         STC   RF,FLDILEN          SET INPUT LENGTH                             
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
*        EXPANDED SR# - MYYCLT0000(0)R000                                       
*                                                                               
         MVC   QSR#EXP(3),=C'SR-'  DISPLAY SR IDENTIFIER                        
         MVC   QSR#EXP+3(L'QMED),QMED DISPLAY MEDIA                             
*                                                                               
         MVI   QSR#EXP+4,C'-'                                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QSR#SRYR       GET YEAR NUMBER                              
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  QSR#EXP+5(2),DUB    DISPLAY YEAR                                 
*                                                                               
         MVC   QSR#EXP+7(3),QCLT   CLIENT                                       
*                                                                               
         CLI   QSR#EXP+9,C' '      REPLACE BLANK WITH DASH                      
         BH    *+8                                                              
         MVI   QSR#EXP+9,C'-'                                                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,QSR#SRSQ       GET SQN NUMBER                               
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                DEFAULT TO SHOWING 4 DIGITS                  
         CLC   QSR#SRSQ,=AL3(9999) IF OVER 9999                                 
         BNH   *+8                                                              
         LA    RF,5                   DISPLAY 5 DIGITS                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  QSR#EXP+10(0),DUB    DISPLAY SQN                                 
*                                                                               
         SRL   RF,4                RETURN LENGTH TO RIGHT NYBBLE                
*                                                                               
         OC    QREV#,QREV#         SKIP IF NO REVISION NUMBER                   
         BZ    DSR#REVX                                                         
*                                                                               
         LA    R1,QSR#EXP+11(RF)   POINT TO NEXT POSITION                       
*                                                                               
         MVI   0(R1),C'-'                                                       
*                                                                               
         MVC   1(3,R1),=C'REV'     SET REVISION ID                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QREV#          GET REVISION NUMBER                          
         CVD   RF,DUB              CVD                                          
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  4(3,R1),DUB         DISPLAY REVISION NUMBER                      
*                                                                               
DSR#REVX DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
*                                                                               
VDISSR#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR VALIDATE STATUS - VVALSTA'                 
***********************************************************************         
*                                                                     *         
*        DISPLAY  STATUS                                              *         
*                                                                     *         
*NTRY    R2==> STATUS ON SCREEN                                       *         
*                                                                     *         
*EXIT    QSTAT = STATUS CODE                                          *         
*              STATUS EXPANSION   IS RE-DISPLAYED                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VVALSTA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         XC    QSTAT,QSTAT         INIT SAVEAREA                                
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BZ    VSTAOK              OKAY IF CODE NOT ENTRED                      
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         OC    FLDDATA(0),SPACES   FORCE UPPERCASE                              
*                                                                               
*        FIND INPUT IN STATUS TABLE                                             
*                                                                               
         L     R3,=A(STATUSTB)          POINT R3 TO TABLE                       
         A     R3,RELO                                                          
         USING STATUSTD,R3         ESTABLISH TABLE ENTRY                        
*                                                                               
VSTALOOP DS    0H                                                               
*                                                                               
         CLI   STATCDE,X'FF'       END OF TABLE?                                
         BE    VSTANOTV            CODE NOT FOUND                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLDDATA(0),STATEXP     CHECK IF IN TABLE                         
         BE    VSTAFD                                                           
*                                                                               
VSTACONT DS    0H                                                               
*                                                                               
         LA    R3,STATUSLQ(R3)      BUMP TO NEXT ENTRY                          
         B     VSTALOOP                                                         
*                                                                               
VSTAFD   DS    0H                  STATUS IS IN TABLE                           
*                                                                               
         MVC   QSTAT,STATCDE       SAVE STATUS CODE                             
*                                                                               
VSTAOK   DS    0H                                                               
*                                                                               
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
         BRAS  RE,CLRFLD           CLEAR EXPANSION FIELD                        
*                                                                               
*        DISPLAY EXPANSION                                                      
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDOLEN          VALUE SET IN CLRFLD                          
*                                                                               
         CHI   RF,L'STATEXP                                                     
         BNH   *+8                                                              
         LHI   RF,L'STATEXP                                                     
*                                                                               
         STC   RF,FLDOLEN          SET OUTPUT LENGTH                            
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLDDATA(0),STATEXP                                               
*                                                                               
         OI    FLDOIND,FOUTTRN     SET TO BE TRANSMITTED                        
*                                                                               
VSTAEXPX DS    0H                                                               
*                                                                               
VVALSTAX DS    0H                                                               
         XIT1                                                                   
*                                                                               
VSTANOTV DS    0H                                                               
         LHI   RF,PPESTANV     INVALID STATUS CODE                              
         B     VSTAERR                                                          
*                                                                               
VSTAERR  DS    0H                                                               
         STCM  RF,3,PERROR     SET ERROR MESSAGE CODE                           
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR DISPLAY STATUS - VDISSTA'                  
***********************************************************************         
*                                                                     *         
*        DISPLAY STATUS NAME                                          *         
*                                                                     *         
*NTRY    R2==> STATUS FIELD ON SCREEN                                 *         
*                                                                     *         
*        P0-1 = AL1(L'OUTPUT) IF R2 = 0                               *         
*        P0==> A(OUTPUT) IF R2=0                                      *         
*                                                                     *         
*        QSTAT = STATUS CODE                                          *         
*                                                                     *         
*EXIT          STATUS      FILLED IN FOR SCREEN FIELD                 *         
*              STATUS      FIELD SET AS PREVIOUSLY VALIDATED          *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VDISSTA  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         LTR   R2,R2               IF NO FIELD GIVEN                            
         BNZ   DSTA05                                                           
*                                                                               
         LR    R5,R1                  SAVE PARAMETER LIST POINTER               
         L     R4,0(R5)               POINT TO OUTPUT AREA                      
         B     DSTA10                                                           
*                                                                               
DSTA05   DS    0H                                                               
*                                                                               
         BRAS  RE,CLRFLD           CLEAR STATUS CODE FIELD                      
         LA    R4,FLDDATA          POINT TO OUTPUT AREA                         
*                                                                               
DSTA10   DS    0H                                                               
*                                                                               
         OC    QSTAT,QSTAT         SKIP IF NO CODE AVAILABLE                    
         BZ    DSTAOK                                                           
*                                                                               
         L     R3,=A(STATUSTB)          POINT R3 TO TABLE                       
         A     R3,RELO                                                          
         USING STATUSTD,R3         ESTABLISH STATUS TABLE                       
*                                                                               
*        FIND CODE IN TABLE                                                     
*                                                                               
DSTALOOP DS    0H                                                               
*                                                                               
         CLI   STATCDE,X'FF'       END OF TABLE?                                
         BE    DSTAOK              CODE NOT IN TABLE                            
*                                                                               
         CLC   STATCDE,QSTAT       TEST FOR MATCH                               
         BE    DSTAFD                                                           
*                                                                               
DSTACONT DS    0H                                                               
         LA    R3,STATUSLQ(R3)     BUMP TO NEXT ENTRY                           
         B     DSTALOOP                                                         
*                                                                               
DSTAFD   DS    0H                                                               
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         LTR   R2,R2               IF NO SCREEN FIELD AVAILABLE                 
         BNZ   *+12                                                             
         IC    RF,0(R5)               GET LENGTH OF OUTPUT                      
         B     DSTAFD10                                                         
*                                                                               
         IC    RF,FLDOLEN          SET IN CLRFLD                                
         OI    FLDIIND,FINPVAL     SET AS PREVIOUSLY VALIDATED                  
*                                                                               
DSTAFD10 DS    0H                                                               
*                                                                               
         CHI   RF,L'STATEXP        MAKE SURE EXPANSION FITS                     
         BNH   *+8                                                              
         LHI   RF,L'STATEXP                                                     
*                                                                               
         STC   RF,FLDOLEN                                                       
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),STATEXP                                                  
*                                                                               
DSTAOK   DS    0H                                                               
*                                                                               
VDISSTAX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - STATUSTB'              
***********************************************************************         
*                                                                     *         
*        STATUS TABLE                                                 *         
*              CL1    CODE                                            *         
*              CL15   EXPANSION                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
STATUSTB DS    0CL116                                                           
         DC    AL1(ESRSGENQ),CL15'GENERATED' DEFAULT                            
         DC    AL1(0),CL15'GENERATED'      DEFAULT                              
         DC    AL1(ESRSAPPQ),CL15'APPROVED '                                    
         DC    AL1(ESRSDLVQ),CL15'DELIVERED'                                    
         DC    AL1(ESRSREJQ),CL15'REJECTED '                                    
         DC    AL1(ESRSRSTQ),CL15'RESENT  '                                     
         DC    AL1(ESRSRSTQ),CL15'RE-SENT '                                     
         DC    AL1(ESRSSNTQ),CL15'SENT '                                        
         DC    AL1(ESRSUDLQ),CL15'UNDELIVERED'                                  
         DC    AL1(ESRSACCQ),CL15'ACCESSED   '                                  
         DC    AL1(ESRSEXPQ),CL15'TIME EXPIRED'                                 
STATUSTX DC    X'FF'               END OF TABLE                                 
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR GET SETUP RECORD - VGETSCH'                
***********************************************************************         
*                                                                     *         
*        READ SETUP RECORD AND SAVE VARIABLES                         *         
*                                                                     *         
*NTRY    QAREA OF WORKING STORAGE HAS REQUIRED DATA                   *         
*                                                                     *         
*        USES IOAREA1                                                 *         
*                                                                     *         
*EXIT    QAREA FIELDS FILLED IN                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VGETSCH  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         XC    QSETUP(QSETUPL),QSETUP INIT SETUP DATA                           
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         LA    R4,KEY              ESTABLISH AS SETUP KEY                       
         USING SCHKKEY,R4                                                       
*                                                                               
         MVC   SCHKAGY,AGENCY      SET AGENCY                                   
         MVC   SCHKMED,QMED        MEDIA                                        
         MVI   SCHKRCD,SCHKRCDQ    RECORD CODE                                  
*                                                                               
         MVC   SCHKCLT,QCLT        CLIENT                                       
*                                                                               
         GOTOR HIGH                READ SETUP RECORD                            
*                                                                               
GSCHLOOP DS    0H                                                               
*                                                                               
         CLC   SCHKKEY(SCHKCLT-SCHKKEY),KEYSAVE DONE AT END OF MEDIA            
         BNE   GSCHDONE                                                         
*                                                                               
         CLC   SCHKCLT,QCLT        MATCH ON CLIENT                              
         BE    *+14                                                             
         OC    SCHKCLT,SCHKCLT     OR ALL CLIENTS                               
         BNZ   GSCHCONT                                                         
*                                                                               
GSCHCONT DS    0H                                                               
*                                                                               
         GOTOR SEQ                 READ NEXT RECORD                             
         B     GSCHLOOP                                                         
*                                                                               
GSCHDONE DS    0H                                                               
*                                                                               
         B     GSCHX               ALL DONE                                     
*                                                                               
GSCHFD   DS    0H                  HAVE SETUP RECORD                            
*                                                                               
         MVC   AIO,AIO1            READ INTO IOAREA1                            
         GOTOR GETREC              READ IN RECORD                               
*                                                                               
         L     R4,AIO              POINT TO FOUND RECORD                        
*                                                                               
         LR    R6,R4               FIND HEADER ELEMENT                          
         MVI   ELCODE,SCHHDRQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   GSCHX               NOT FOUND                                    
*                                                                               
         USING SCHHDRD,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   QACTDT,SCHSRACT     SAVE ACTIVATION DATE                         
*                                                                               
GSCHX    DS    0H                                                               
*                                                                               
         MVC   KEY,SVKEY           RESTORE ORIGINAL KEY                         
         GOTOR HIGH                RESET DMGR POINTERS                          
*                                                                               
VGETSCHX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR GET SETUP RECORD - VFMTSR#'                
***********************************************************************         
*                                                                     *         
*        FORMAT SR# FOR PRINTING                                      *         
*                                                                     *         
*NTRY    QAREA OF WORKING STORAGE HAS REQUIRED DATA                   *         
*                                                                     *         
*EXIT    QAREA FIELDS FILLED IN                                       *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VFMTSR#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         MVC   QSR#EXP,SPACES      INIT OUTPUT AREA                             
         LA    R2,QSR#EXP          START OF IO #                                
*                                                                               
         MVC   0(3,R2),=C'SR-'     ESR IDENTIFIER                               
         AHI   R2,3                                                             
*                                                                               
         MVC   0(1,R2),QMED        SET MEDIA                                    
         MVI   1(R2),C'-'                                                       
         AHI   R2,2                BUMP TO NEXT POSITION                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,QSR#SRYR         GET SR# YEAR                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(2,R2),DUB         SET YEAR                                     
*                                                                               
         AHI   R2,2                BUMP TO CLIENT PART                          
         MVC   0(3,R2),QCLT        SET CLIENT                                   
         AHI   R2,2                BUMP POINTER                                 
         CLI   0(R2),C' '          IF EMPTY                                     
         BH    *+8                                                              
         MVI   0(R2),C'-'             FILL IN WITH DASH                         
*                                                                               
         AHI   R2,1                 BUMP PAST LAST OF CODE                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,QSR#SRSQ       GET SEQUENCE NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         LA    RF,4                DISPLAY 4 DIGITS                             
         CP    DUB,=P'9999'        IF OVER 9999                                 
         BNH   *+8                                                              
         LA    RF,5                   DISPLAY 5 DIGITS                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         SLL   RF,4                SHIFT TO LEFT NYBBLE                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R2),DUB         SEQUENCE NUMBER                              
*                                                                               
         SRL   RF,4                RETURN LENGTH TO RIGHT NYBLE                 
*                                                                               
         LA    R2,1(RF,R2)         NEXT OUTPUT POSITION                         
*                                                                               
         OC    QREV#,QREV#         SKIP IF NO REVISION NUMBER                   
         BZ    FSR#50                                                           
*                                                                               
         MVI   0(R2),C'-'          SET DASH                                     
*                                                                               
         AHI   R2,1                NEXT OUTPUT AREA                             
*                                                                               
         MVC   0(3,R2),=C'REV'     REVISION INDICATOR                           
         AHI   R2,3                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QREV#          GET REVISION NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R2),DUB         SEQUENCE NUMBER                              
*                                                                               
         AHI   R2,3                NEXT OUTPUT AREA                             
*                                                                               
FSR#50   DS    0H                                                               
*                                                                               
VFMTSR#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR HEADER MAINT/LIST - VALRV#'                
***********************************************************************         
*                                                                     *         
*        VALIDATE REVISION NUMBER                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VVALRV#  NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         BZ    VVALRV#X            SKIP IF NOT ENTERED                          
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8              CVD                                          
         B     *+10                                                             
         PACK  DUB,FLDDATA(0)      PACK                                         
*                                                                               
         TP    DUB                 MAKE SURE ITS A PACKED NUMBER                
         BNZ   VALRVER1            INVALID IF NOT                               
*                                                                               
         CVB   RF,DUB                                                           
*                                                                               
         CHI   RF,255              MAX NUMBER ALLOWED                           
         BH    VALRVER2                                                         
*                                                                               
         STC   RF,QREV#                                                         
*                                                                               
VVALRV#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
VALRVER1 DS    0H                                                               
         LHI   RF,PPEFLDNV     INVALID REVISION                                 
         B     VALRVERR                                                         
*                                                                               
VALRVER2 DS    0H                                                               
         LHI   RF,PPEFLDNV     INVALID REVISION                                 
         B     VALRVERR                                                         
*                                                                               
VALRVERR DS    0H                                                               
         STCM  RF,3,PERROR     SET ERROR MESSAGE CODE                           
*                                                                               
         GOTOR ERREXIT                                                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR HEADER MAINT/LIST - DISRV#'                
***********************************************************************         
*                                                                     *         
*        DISPLAY  REVISION NUMBER                                     *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VDISRV#  NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         BRAS  RE,CLRFLD           CLEAR REVSION NUMBER FIELD                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,QREV#          GET REVISON NUMBER                           
         BZ    VDISRV#X            OKAY IF NONE                                 
*                                                                               
         CVD   RF,DUB              CVD                                          
*                                                                               
         OI    DUB+7,X'0F'         FORCE SIGN                                   
*                                                                               
         UNPK  FLDDATA(3),DUB      PACK                                         
*                                                                               
         MVI   FLDILEN,3           SET LENGTH                                   
*                                                                               
VDISRV#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42010 - ENHANCED SR HEADER MAINT/LIST - ACTPUT'                
***********************************************************************         
*                                                                     *         
*        ADD ACTIVITY ELEMENT TO MINIO SET                            *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VACTPUT  NTR1  BASE=*,LABEL=*      RESTORE RECORDS                              
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH ENHANCED SR MASTER KEY             
         USING ESRKEY,R4                                                        
*                                                                               
         XC    SVACTELM,SVACTELM   INIT NEXT ACTIVITY ELEMENT                   
*                                                                               
*        SCAN FOR MOST RECENT ACTIVITY ELEMENT                                  
*                                                                               
         LA    R6,ELEMENT           USE ELEMENT AREA FOR ACTIVITY ELEM          
         XC    ELEMENT,ELEMENT      CLEAR AREA                                  
         USING ESRACTHD,R6          PLACE A USING ON TOP OF IT                  
*                                                                               
         MVI   ESRAKCDE,ESRAKACQ    ACTIVITY ELEMENT ID                         
         MVI   ESRAKLEN,1           READ FOR ANY ACTIVITY ELEMENT               
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT  READ FIRST ACTIVITY ELEMENT                 
*                                                                               
ACPLOOP  DS    0H                                                               
*                                                                               
         BNZ   ACPDONE             END OF ACTIVITY ELEMENTS                     
*                                                                               
         ICM   R6,15,MINELEM        POINT R6 TO ELEMENT                         
*                                                                               
         ZIC   RF,ESRAKLEN          MOVE LENGTH TO R1                           
         AHI   RF,-1                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVACTELM(0),ESRAKEY  MOVING DATA TO SAVE AREA                    
*                                                                               
ACPCONT  DS    0H                                                               
*                                                                               
         GOTOR NXTELM,DMCB,ELEMENT  GET NEXT ACTIVITY ELEM                      
         B     ACPLOOP              IF FOUND GO INTO LOOP                       
*                                                                               
ACPDONE  DS    0H                                                               
*                                                                               
         LA    R6,SVACTELM         ESTABLISH AS ACTIVITY ELEMENT                
*                                                                               
         OC    SVACTELM,SVACTELM   SKIP IF OLD ELEMENT FOUND                    
         BNZ   ACPHDRX                                                          
*                                                                               
*        BUILD A NEW ACTIVITY ELEM                                              
*                                                                               
         XC    SVACTELM,SVACTELM   INIT ELEMENT                                 
*                                                                               
         MVI   ESRAKCDE,ESRAKACQ   SET ACTIVITY ELM ID                          
         MVI   ESRAKLEN,ESRACTLQ   SET ACTIVITY ELM LENGTH                      
*                                                                               
ACPHDRX  DS    0H                                                               
*                                                                               
         L     RF,WRTELM           ASSUME RE-WRITING ELEMENT                    
*                                                                               
         CLC   ESRAPID,SVESRPID    SKIP SQN BUMP IF SAME PID                    
         BNE   *+10                                                             
         CLC   ESRADTE,BTODAY      AND TODAY                                    
         BE    ACPSQNX                                                          
*                                                                               
*        BUMP SEQUENCE NUMBER                                                   
*                                                                               
         SR    RF,RF                CLEAR RF                                    
         ICM   RF,3,ESRAKSQN        INSERT SEQ NUMBER INTO RF                   
         AHI   RF,1                 INCREAMENT SEQ BY ONE                       
*                                                                               
         STCM  RF,3,ESRAKSQN        SEQ NUMBER INCREAMENTED BY ONE              
*                                                                               
         XC    ESRACHGS,ESRACHGS    INIT CHANGE INDICATORS                      
*                                                                               
         MVC   ESRAPID,SVESRPID     PID OF CHANGER                              
         MVC   ESRADTE,BTODAY       DATE OF CHANGE - BINARY                     
*                                                                               
         L     RF,ADDELM           SET TO ADD NEW ELEMENT                       
*                                                                               
ACPSQNX  DS    0H                                                               
*                                                                               
         OC    ESRACHGS,QACTCHGS    RECORD CHANGES                              
*                                                                               
         GOTOR (RF),DMCB,ESRAKEY    ADD HEADER ELEMENT                          
*                                                                               
VACTPUTX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VFNDSR#'               
***********************************************************************         
*                                                                     *         
*        FIND SR# FOR A GIVEN DATE USUALLY THAT OF AN INSERTION       *         
*                                                                     *         
*        PERIOD DATES WILL BE UPDATED TO THAT OF IO.                  *         
*                                                                     *         
*NTRY    R7 ==>    MINIO BLOCK                                        *         
*        P1     =  A(DATE)                                            *         
*                                                                     *         
*EXIT    QSR#   =  FOUND NEW SERIAL NUMBER                            *         
*        QREV#  =  CORRECT REVISION NUMBER                            *         
*        QDISK  =  DISK ADDRESS OF MASTER MINIO RECORD                *         
*        CC        ZERO     - IO FOUND                                *         
*                  NON ZERO - IO NOT FOUND                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VFNDSR#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LR    R3,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         L     RF,0(R3)            POINT TO DATE                                
*                                                                               
         GOTOR DATCON,ESRPARMS,(0,0(RF)),(2,HALF) COMPRESS DATE                 
*                                                                               
         LA    R4,KEY              ESTABLISH KEY AS PERIOD PASSIVE              
         USING ESR1KEYD,R4                                                      
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         XC    QSRKEY,QSRKEY       INIT IOKEY SAVEAREA                          
*                                                                               
         MVC   ESR1AGY,QAGY        SET AGENCY                                   
         MVC   ESR1MED,QMED        SET MEDIA                                    
         MVI   ESR1RCD,ESR1RCDQ    SET RECORD TYPE                              
*                                                                               
         MVC   ESR1CLT,QCLT        SET CLIENT                                   
         MVC   ESR1PRD,QPRD        SET PRODUCT                                  
         MVC   ESR1PUB,QPUB        SET PUB                                      
*                                                                               
         MVC   ESR1END,HALF        SET DATE                                     
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         LA    R7,MNBLKCB          ESTABLISH MINIO CONTROL BLOCK                
         USING MINBLKD,R7                                                       
*                                                                               
         MVC   AIO,AIO2            READ FIRST RECORD INTO IOA2                  
*                                                                               
FNDSR#LP DS    0H                                                               
*                                                                               
         CLC   ESR1KEY(ESR1END-ESR1KEYD),KEYSAVE DONE IF IO NOT FOUND           
         BNE   FNDSR#DN            FOR CLT/PRD/PUB                              
*                                                                               
         CLC   HALF,ESR1END        DATE MUST BE IN IO PERIOD                    
         BH    FNDSR#DN                                                         
         CLC   HALF,ESR1STRT                                                    
         BL    FNDSR#DN                                                         
*                                                                               
         OC    QSR#,QSR#           SKIP IF NO SR# GIVEN                         
         BZ    *+14                                                             
         CLC   ESR1SR#,QSR#        FILTER ON SR#                                
         BNE   FNDSR#CN                                                         
*                                                                               
         MVC   SVKEY,ESR1KEY       SAVE KEY                                     
*                                                                               
         GOTOR GETREC              READ IN FIRST RECORD IN MINIO SET            
*                                                                               
         L     RF,AIO              POINT TO FOUND RECORD                        
*                                                                               
         MVC   MINMKEY,0(RF)       GET MASTER KEY FOR MINIO SET                 
*                                                                               
         MVI   MINDELSW,C'Y'       PROCESS DELETES                              
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD) OPEN MINIO SET                
*                                                                               
         XC    ELEMENT,ELEMENT     ESTABLISH HEADER ELEMENT                     
         LA    R6,ELEMENT                                                       
         USING ESRHDRD,R6                                                       
*                                                                               
         MVI   ESRHKCDE,ESRHKIDQ   SET HEADER ELEMENT ID                        
*                                                                               
         GOTOR GETELM,DMCB,ELEMENT READ HEADER ELEMENT                          
         BNE   FNDSR#CN            SKIP - ELEMENT NOT FOUND                     
*                                                                               
         L     R6,MINELEM          POINT TO FOUND ELEMENT                       
*                                                                               
         CLC   QSTEW,ESRHSTEW      MATCH ON STEWARDSHIP                         
         BNE   *+10                                                             
         MVC   QSRKEY,SVKEY           SAVE KEY IF A MATCH                       
*                                                                               
FNDSR#CN DS    0H                                                               
*                                                                               
         MVC   KEY,SVKEY           RESTORE CURRENT KEY                          
*                                                                               
         GOTOR HIGH                RESTORE DIRECTORY POINTER                    
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTOR SEQ                 GET NEXT POINTER                             
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         B     FNDSR#LP                                                         
*                                                                               
FNDSR#DN DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1            RESTORE A(IOAREA)                            
         MVI   MINDELSW,0          TURN OFF PROCESS DELETES                     
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         OC    QSRKEY,QSRKEY       SKIP IF KEY FOUND                            
         BNZ   *+12                                                             
         LHI   R0,1                SET RETURN INDICATOR                         
         B     FNDIOX                                                           
*                                                                               
         LA    R4,QSRKEY           POINT TO FOUND RECORD                        
*                                                                               
         MVC   QDISK,ESR1DISK      SAVE MASTER RECORD DISK ADDR                 
         MVC   QSR#,ESR1SR#        SET SR#                                      
         MVC   QREV#,ESR1RV#       SET REVISION #                               
*                                                                               
         SR    R0,R0               SET RETURN CODE                              
*                                                                               
FNDIOX   DS    0H                                                               
*                                                                               
         LTR   R0,R0               SET RETURN CODE                              
*                                                                               
VFNDSR#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VDELPAAV'              
***********************************************************************         
*                                                                     *         
*        DELPSSV- DELETE PASSIVE KEYS                                 *         
*                                                                     *         
*               - ON INPUT R7 POINTS TO MINIOBLOCK                    *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VDELPSSV NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LA    R7,MNBLKCB          ESTABLSH MINIO CONTROL BLOCK                 
         USING MINBLKD,R7                                                       
*                                                                               
         LA    R4,MINMKEY          ESTABLISH INVOICE ESR    KEY                 
         USING ESRKEY,R4                                                        
*                                                                               
*        OPEN  MINIO                                                            
*                                                                               
         CLI   MINOPEN,C'Y'        SKIP IF MINIO SET OPEN                       
         BE    DPSOPENX                                                         
*                                                                               
         MVI   MINDELSW,C'Y'                                                    
*                                                                               
         MVC   ESRKEY,QSRKEY       SET MASTER KEY                               
*                                                                               
         GOTOR VMINIO,ESRPARMS,('MINOPN',MINBLKD)  OPEN MINIO SET               
*                                                                               
         CLI   MINERR,0                                                         
         BE    *+6                 NO ERRORS TOLERATED                          
         DC    H'0'                                                             
*                                                                               
DPSOPENX DS    0H                                                               
*                                                                               
         LA    R6,ELEMENT           BUILD ELM                                   
         XC    ELEMENT,ELEMENT      CLEAR ELM AREA                              
         USING ESRHDRD,R6           ESR    HEADER ELEM                          
         MVI   ESRHKCDE,ESRHKIDQ    ELEM ID                                     
*                                                                               
         GOTOR GETELM,DMCB,ESRHKEY  GET ELEMENT TO RECORD                       
         BNZ   DELPSSVX                                                         
*                                                                               
         ICM   R6,15,MINELEM        POINT TO FOUND ELEMENT                      
*                                                                               
         XC    KEY,KEY             GET PASSIVE 1 KEY                            
         LA    R4,KEY              ESTABLISH PASSIVE 1 RECORD KEY               
         USING ESR1KEY,R4                                                       
*                                                                               
         MVC   ESR1AGY,ESRKAGY-ESRKEY+MINMKEY  SET AGENCY                       
         MVC   ESR1MED,ESRKMED-ESRKEY+MINMKEY  SET MEDIA                        
         MVI   ESR1RCD,ESR1RCDQ                SET PASSIVE CODE                 
         MVC   ESR1CLT,ESRKCLT-ESRKEY+MINMKEY  SET CLIENT                       
         MVC   ESR1PRD,ESRHPRD                 SET PRODUCT                      
         MVC   ESR1PUB,ESRKPUB-ESRKEY+MINMKEY  SET PUB                          
         GOTOR DATCON,DMCB,(3,ESRHEND),(2,ESR1END)    SET END   DATE            
         GOTOR DATCON,DMCB,(3,ESRHSTRT),(2,ESR1STRT)  SET START DATE            
         MVC   ESR1SR#,ESRKSR#-ESRKEY+MINMKEY     SET SR#                       
         MVC   ESR1RV#,ESRKRV#-ESRKEY+MINMKEY     SET RV#                       
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   ESR1KEY,KEYSAVE     SKIP IF RECORD NOT FOUND                     
         BNE   DPSWRTX                                                          
*                                                                               
         TM    ESR1CNTL,ESRDDELQ   SKIP IF ITS ALREADY DELETED                  
         BO    DPSWRTX                                                          
*                                                                               
         OI    ESR1CNTL,ESRDDELQ   DELETE PASSIVE                               
*                                                                               
         GOTOR WRITE                                                            
*                                                                               
DPSWRTX  DS    0H                                                               
*                                                                               
DELPSSVX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T42000 - ENHANCED SR PARSE SR# - VPRSSR#'                       
***********************************************************************         
*                                                                     *         
*        PARSE    ENHANCED SR #                                       *         
*                                                                     *         
*NTRY    R2==> SR# ON SCREEN OF FORM SR-M-CCC-YYNNNN-REVNNNN          *         
*        P0+0  L'INPUT  IF R2=0                                       *         
*        P0    A(INPUT) IF R2=0                                       *         
*                                                                     *         
*EXIT          QIOMED  - MEDIA CODE                                   *         
*              QCLT    - CLIENT CODE                                  *         
*              QSR#    - INTERNAL SR#                                 *         
*              QREV#   - REVISION NUMBER                              *         
*                                                                     *         
***********************************************************************         
         DS    0D                                                               
VPRSSR#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         LR    R3,R1               SAVE A(PARAMTER LIST)                        
*                                                                               
         XC    QMED,QMED           INIT OUTPUT FIELDS - MEDIA                   
         XC    QCLT,QCLT           CLIENT                                       
         XC    QSR#,QSR#           SR#                                          
         XC    QREV#,QREV#         REVISION #                                   
*                                                                               
         SR    RF,RF                                                            
*                                                                               
         LTR   R2,R2               IF NOT USING A FIELD HEADER                  
         BNZ   PSR#05                                                           
*                                                                               
         ICM   RF,1,0(R3)             GET INPUT LENGTH                          
         BZ    PRSSR#X                NO INPUT                                  
*                                                                               
         L     R4,0(R3)               POINT TO INPUT                            
*                                                                               
         B     PSR#10                                                           
*                                                                               
PSR#05   DS    0H                                                               
*                                                                               
         ICM   RF,1,FLDILEN        GET LENGTH OF INPUT                          
         BZ    PRSSR#X             NO INPUT                                     
         LA    R4,FLDDATA          POINT TO INPUT                               
*                                                                               
PSR#10   DS    0H                                                               
*                                                                               
*        MEDIA CODE IS FIRST LETTER                                             
*                                                                               
         MVC   QMED,3(R4)          SAVE MEDIA CODE                              
*                                                                               
         LA    R4,5(R4)            BUMP TO SR#                                  
*                                                                               
*        YEAR IS NEXT 2 DIGITS                                                  
*                                                                               
         PACK  DUB,0(2,R4)         PACK YEAR                                    
         AP    DUB,=P'100'         ADJUST FOR 21ST CENTURY                      
         CVB   RE,DUB              CVB                                          
         STC   RE,QSR#SRYR         SAVE YEAR                                    
*                                                                               
         LA    R4,2(R4)            BUMP TO CLIENT CODE                          
*                                                                               
*        CLIENT CODE IS NEXT 2 OR 3 POSITIONS                                   
*                                                                               
         MVC   QCLT,0(R4)          SAVE CLIENT CODE                             
*                                                                               
         CLI   QCLT+2,C'-'         IF DASH                                      
         BNE   *+8                                                              
         MVI   QCLT+2,C' '         BLANK OUT THIRD LETTER                       
*                                                                               
         LA    R4,3(R4)            BUMP TO YEAR                                 
*                                                                               
*        SR# IS NEXT 4 OR 5 DIGITS                                              
*                                                                               
         LA    RF,4(R4)            POINT TO LAST POSSIBLE DIGIT                 
         CLI   0(RF),C'0'          IF NOT NUMERIC                               
         BNL   *+6                                                              
         BCTR  RF,0                   BACK UP A POSITION                        
*                                                                               
         SR    RF,R4               EXECUTE LENGTH OF SQN                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)         PACK SQN                                     
*                                                                               
         CVB   RE,DUB              CVB                                          
         STCM  RE,7,QSR#SRSQ       SAVE SEQUENCE NUMBER                         
*                                                                               
         LA    R4,1(RF,R4)         BUMP TO REVISION                             
*                                                                               
         CLC   =C'-REV',0(R4)      CHECK FOR A REVISION NUMBER                  
         BNE   PSR#OK                                                           
*                                                                               
         LA    R4,4(R4)            BUMP TO ACTUAL NUMBER                        
         PACK  DUB,0(3,R4)         PACK                                         
         CVB   RE,DUB              CVB                                          
         STC   RE,QREV#            SAVE REVISION NUMBER                         
*                                                                               
PSR#OK   DS    0H                                                               
*                                                                               
PRSSR#X  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - VFNDRV#'               
***********************************************************************         
*                                                                     *         
*        FIND LATEST REVISION NUMBER FOR AN IO                        *         
*                                                                     *         
*NTRY    R7 ==>    MINIO BLOCK                                        *         
*        QSR#   =  SR NUMBER                                          *         
*                                                                     *         
*EXIT    QREV#  =  CORRECT REVISION NUMBER                            *         
*        QDISK  =  DISK ADDRESS OF MASTER MINIO RECORD                *         
*        CC        ZERO     - RV FOUND                                *         
*                  NON ZERO - RV NOT FOUND                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VFNDRV#  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         LR    R3,R1               SAVE PARAMETER LIST POINTER                  
*                                                                               
         LA    R4,KEY              ESTABLISH ESR KEY                            
         USING ESRKEY,R4                                                        
*                                                                               
         XC    KEY,KEY             INIT KEY                                     
         XC    QSRKEY,QSRKEY       INIT IOKEY SAVEAREA                          
*                                                                               
         MVC   ESRKAGY,QAGY        SET AGENCY                                   
         MVC   ESRKMED,QMED        SET MEDIA                                    
         MVI   ESRKRCD,ESRKRCDQ    SET RECORD TYPE                              
*                                                                               
         MVC   ESRKCLT,QCLT        SET CLIENT                                   
         MVC   ESRKPUB,QPUB        SET PUB                                      
         MVC   ESRKSR#,QSR#        SET SR#                                      
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTO1 HIGH                READ FOR FIRST PASSIVE ON DIRECTORY          
*                                                                               
FNDRV#LP DS    0H                                                               
*                                                                               
         CLC   ESRKEY(ESRKRV#-ESRKEY),KEYSAVE DONE IF IO NOT FOUND              
         BNE   FNDRV#DN            FOR CLT/PRD/PUB                              
*                                                                               
         MVC   QSRKEY,ESRKEY       SAVE KEY                                     
*                                                                               
FNDRV#CN DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETED RECORDS                         
*                                                                               
         GOTOR SEQ                 GET NEXT POINTER                             
*                                                                               
         B     FNDRV#LP                                                         
*                                                                               
FNDRV#DN DS    0H                                                               
*                                                                               
         NI    DMINBTS,X'FF'-X'08'  RESET                                       
*                                                                               
         OC    QSRKEY,QSRKEY       SKIP IF KEY FOUND                            
         BNZ   *+12                                                             
         LHI   R0,1                SET RETURN INDICATOR                         
         B     FNDRVX                                                           
*                                                                               
         LA    R4,QSRKEY           POINT TO FOUND RECORD                        
*                                                                               
         MVC   QDISK,ESRDDISK      SAVE MASTER RECORD DISK ADDR                 
         MVC   QREV#,ESRKRV#       SET REVISION #                               
*                                                                               
         SR    R0,R0               SET RETURN CODE                              
*                                                                               
FNDRVX   DS    0H                                                               
*                                                                               
         LTR   R0,R0               SET RETURN CODE                              
*                                                                               
VFNDRV#X DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - CLRFLD'                
***********************************************************************         
*                                                                     *         
*        CLEARS A FIELD ON SCREEN AND FORCES RE-TRANSMITTAL           *         
*                                                                     *         
*NTRY   R2==>   FIELD ON SCREEN                                       *         
*                                                                     *         
*EXIT    FIELD CLEARED TO NULLS                                       *         
*        FIELD SET TO BE RE-TRANSMITTED                               *         
*        OUTPUT DATA LENGTH SET TO MAXIMUM                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CLRFLD   NTR1   BASE=*,LABEL=*                                                  
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         USING FLDHDRD,R2          ESTABLISH FIELD ON SCREEN                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           GET TOTAL LENGTH OF FIELD                    
         AHI   RF,-(FLDDATA-FLDHDRD)  DECREMENT BY HEADER LENGTH                
*                                                                               
         TM    FLDATB,FATBXHDR     IF THERE IS AN EXTENDED HEADER               
         BNO   *+8                                                              
         AHI   RF,-8                  DECREMENT BY EXTENDED HDR LENGTH          
*                                                                               
         STC   RF,FLDOLEN          SET MAX OUTPUT LENGTH                        
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         OI    FLDOIND,FOUTTRN     TRANSMIT FIELD                               
*                                                                               
CLRFLDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - PID'                   
***********************************************************************         
*   PID - THIS ROUTINE WILL GET TWO BYTES FROM FATBLOCK               *         
*         WHICH ARE "PERSONAL ID"                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
PID      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         XC    SVESRPID,SVESRPID   PASSWORD ID NUMBER CLEARED                   
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 CGETFACT,DMCB,(2,0),0,0   RETURN TIME IN TUS                     
         DROP  RF                                                               
*                                                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
*                                                                               
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
*                                                                               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         BZ    *+10                                                             
         MVC   SVESRPID,FAPASSWD   SAVE PASSWORD ID NUMBER                      
*                                                                               
*****    B     PIDX                                                             
*                                                                               
*        RETURN TO GETFACTS FOR SYSTEM NAME                                     
*                                                                               
         GOTO1 (RF),DMCB,(X'80',0),F#SSBD,0   EXTRACT SSB DATA                  
*                                                                               
         L     R1,0(R1)                                                         
         USING F@SSBD,R1           ESTABLISH SSB EXTRACT                        
*                                                                               
         MVC   SSYSNA(3),F@SSYSNA  SYSTEM NAME                                  
*                                                                               
PIDX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
HEXTAB   DC    C' 123456789ABCDE'  SYS# TO CHARACTER                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - CHKPFK'                
***********************************************************************         
*                                                                     *         
*        CHKPFK - CHECK FOR PFKEYS THAT FORCE CHANGE OF SCREEN        *         
*                                                                     *         
*        PF2/PF14  - INVOICE/LIST                                     *         
*        PF3/PF15  - DETAIL /LIST                                     *         
*        PF4/PF16  - COMMENT/DISPLAY                                  *         
*        PF5/PF17  - ACTIVITY/DISPLAY                                 *         
*                                                                     *         
***********************************************************************         
*                                                                               
CHKPFK   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOLER WORKING STORAGE            
         USING SYSD,R9             ESTABLISH SYSTEM  WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)            A(TIOB)                                      
         USING TIOBD,RF            ESTABLISH TOIB                               
*                                                                               
         CLI   TIOBAID,0           DONE IF ENTER HIT                            
         BE    CHKPFKDN                                                         
*                                                                               
         CLI   TIOBAID,24          SKIP IF NOT A PFKEY                          
         BH    CHKPFKDN                                                         
*                                                                               
*        CHECK IF PFKEY HAS AN ENTRY IN SCREEN TABLE                            
*                                                                               
         LA    R1,SCRTBL           ESTABLISH SCREEN TABLE                       
         USING SCRTBLD,R1                                                       
*                                                                               
CHKPFKLP DS    0H                                                               
*                                                                               
         CLI   SCTBPF1,X'FF'       DONE IF END OF TABLE REACHED                 
         BE    CHKPFKDN                                                         
*                                                                               
         CLC   SCTBPF1,TIOBAID     MATCH ON PFKEY ENTERED                       
         BE    CHKPFKFD                                                         
*                                                                               
         CLC   SCTBPF2,TIOBAID                                                  
         BE    CHKPFKFD                                                         
*                                                                               
CHKPFKCN DS    0H                                                               
*                                                                               
         LA    R1,SCRTBLLQ(R1)     BUMP TO NEXT TABLE ENTRY                     
*                                                                               
         B     CHKPFKLP                                                         
*                                                                               
*        MOVE RECORD AND ACTION TO TABLE                                        
*                                                                               
CHKPFKFD DS    0H                                                               
*                                                                               
         XC    CONREC,CONREC       CLEAR RECORD FIELD                           
         MVC   CONREC(3),SCTBREC   FIRST 3 CHS OF RECORD NAME                   
         MVI   CONRECH+5,3         INPUT LENGTH                                 
*                                                                               
         XC    CONACT,CONACT       CLEAR ACTION FIELD                           
         MVC   CONACT(3),SCTBACT   FIRST 3 CHS OF ACTION NAME                   
         MVI   CONACTH+5,3         INPUT LENGTH                                 
*                                                                               
CHKPFKDN DS    0H                                                               
*                                                                               
CHKPFKX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
SCRTBL   DS    0H                  SCREEN TABLE                                 
         DC    AL1(2,14)           PFKEYS - 2/14                                
         DC    CL8'ESR    '        RECORD - SR HEADER                           
         DC    CL8'LIST'           ACTION - LIST                                
*                                                                               
         DC    AL1(3,15)           PFKEYS - 3/15                                
         DC    CL8'FAX'            RECORD - DETAIL                              
         DC    CL8'LIST'           ACTION - LIST                                
*                                                                               
         DC    AL1(4,16)           PFKEYS - 4/16                                
         DC    CL8'STATUS '        RECORD - COMMENT                             
         DC    CL8'LIST   '        ACTION - LIST                                
*                                                                               
         DC    AL1(5,17)           PFKEYS - 5/17                                
         DC    CL8'ACTIVITY'       RECORD - ACTIVITY                            
         DC    CL8'DISPLAY'        ACTION - DISPLAY                             
*                                                                               
         DC    X'FF'               EOT                                          
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - STATUSTD'              
***********************************************************************         
*                                                                     *         
*        STATUS TABLE DSECT                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
STATUSTD DSECT                     STATUS TABLE DSECT                           
STATCDE  DS    CL1                 STATUS CODE                                  
STATEXP  DS    CL15                STATUS EXPANSION                             
STATUSLQ EQU   *-STATUSTD          LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - SCRTBLD'               
***********************************************************************         
*                                                                     *         
*        SCREEN TABLE DSECTS                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SCRTBLD  DSECT                     SCREEN TABLE ENTRY                           
SCTBPF1  DS    AL1                 FIRST PFKEY                                  
SCTBPF2  DS    AL1                 SECOND PFKEY                                 
SCTBREC  DS    CL8                 RECORD                                       
SCTBACT  DS    CL8                 ACTION                                       
SCRTBLLQ EQU   *-SCRTBLD           ENTRY LENGTH                                 
*                                                                               
         TITLE 'PPESR00 - PRINT ENHANCED SR CONTROLLER - DSECTS'                
***********************************************************************         
*                                                                     *         
*        VARIOUS DSECTS                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PPESRWRKD                                                      
         EJECT                                                                  
       ++INCLUDE PPESRFFD          HEADER SCREEN                                
         EJECT                                                                  
** DDGENTWA                                                                     
       ++INCLUDE DDGENTWA                                                       
*                                                                               
*        PROGRAM SAVED STORAGE AT BOTTOM OF TWA0                                
*                                                                               
         ORG   T420FFD+TWAENDLQ    ORG TO BOTTOM OF TWA0                        
*                                                                               
STSAVE   EQU   *                                                                
SECBLK   DS    CL1024              SECRET PARAMETER BLOCK                       
*                                                                               
SVSPARE  DS    CL(TWAMXLEN-(*-STSAVE))  SPARE - DEFINE NEW AREAS ABOVE          
*                                                                               
** DDCOMFACS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
** DDCOREQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
** FATIOB                                                                       
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
** FAFACTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
** PPSRCHPARM                                                                   
         PRINT OFF                                                              
       ++INCLUDE PPSRCHPARM                                                     
         PRINT ON                                                               
** DDGLOBEQUS                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
** PPERREQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PPERREQUS                                                      
         PRINT ON                                                               
** DDOFFICED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
** DDGLVXCTLD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
*DDGLPFMD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGLPFMD          PFM FILE INFO GLOBAL                         
         PRINT ON                                                               
** PRGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
** DDMINBLK                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDMINBLK                                                       
         PRINT ON                                                               
** FAGETTXTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
** FASECRETD                                                                    
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
** DDFLDHDR                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
** DDPERVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
** CTGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
** SEACSFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024PPESR00   10/05/11'                                      
         END                                                                    
