*          DATA SET RESUN00S   AT LEVEL 006 AS OF 05/01/02                      
*PHASE T81400A,*                                                                
*INCLUDE RECUP                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T81400 - RESUN00 - SONNET FILE MAINTENANCE '                    
*                                                                               
*******************************************************************             
*                                                                 *             
*        RESUN00 --- REP SONNET FILE MAINT BASE                  *              
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
*                                                                 *             
*HERE**************************************************************             
*                                                                               
T81400   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T81400,RR=R2,CLEAR=YES                                 
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
         AH    R9,=Y(LENIOAS)      R9=A(SFM SYSTEM WORKING STORAGE)             
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
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         SPACE 1                                                                
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R3,COREFACS         POINT TO ADDRESS AREA                        
         L     RF,CALLOV                                                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
SFM20    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0         GO TO CALLOV                                 
         MVC   0(4,R3),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R3,4(R3)            NEXT ADDRESS                                 
         BCT   R0,SFM20                                                         
*                                                                               
         BAS   RE,CKGLOB                                                        
         SPACE 1                                                                
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
********************************************************************            
*              INITIALIZE SYSTEM DEPENDENT VALUES                               
********************************************************************            
SYSINIT  NTR1                                                                   
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
*                                                                               
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS20    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS20                                                         
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R1,VCOUNT                                                        
*                                                                               
SYS40    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R1,SYS40                                                         
*                                                                               
         LA    R1,STARTSAV                                                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'81'          SAVE 1 LARGE TWA                             
         MVI   SYSTEM,C'R'         REP                                          
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4096 BYTES                       
         MVC   SYSDUMMY,VDUMMY                                                  
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
         MVC   SYSPHASE,=X'D9081400'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         EJECT                                                                  
********************************************************************            
* SET UP CERTAIN ROUTINE ADDRESSES - CAN'T WAIT FOR GENCON                      
********************************************************************            
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
         SPACE                                                                  
         OI    GENSTAT3,USEKEYSV   USE KEYSAVE,KEY (INTEREP)                    
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
**********************************************************************          
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         L     RA,ATWA                                                          
         L     R8,ASPOOLD                                                       
         L     R9,ASYSD            RESTORE POINTER TO SYSTEM STORAGE            
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  DS    0H                                                               
         B     VUSER                                                            
         B     VSTA                STATION                                      
         B     VOFF                OFFICE                                       
         B     MYERR                                                            
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
* VUSER - GET REP DATA FROM CONTROL FILE USER ID RECORD                         
************************************************************                    
VUSER    MVC   FILENAME,=CL8'CTFILE'                                            
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
******   GOTO1 RETPROG             AND RETURN TO OVERLAY                        
                                                                                
VU20     DS    0H                                                               
         GOTO1 =A(VPFPROC),DMCB,(RC),RR=Y                                       
         B     VUSERX                                                           
                                                                                
VUSERX   B     XIT                                                              
***************************************************************                 
* VALIDATE STATION CALL LETTERS                                                 
* ON EXIT, CALL LETTERS ARE IN WORK                                             
*                              WORK+4  - A=AM F=FM C=CM T=BLANK                 
*                              WORK+10 - MARKET NAME                            
*                              WORK+40 - 1 OR 2 IF SATELLITE STATION            
*                              WORK+41 - GROUP/SUBGROUP CODE                    
***************************************************************                 
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
         MVI   WORK+4,C'A'         AM = A                                       
         EX    RE,STAAM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'F'         FM = F                                       
         EX    RE,STAFM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'C'         CM = C                                       
         EX    RE,STACM                                                         
         BE    VS100                                                            
         MVI   WORK+4,C'1'         MAY BE SATELLITE STATION                     
         EX    RE,STA1                                                          
         BE    VS100                                                            
         MVI   WORK+4,C'2'                                                      
         EX    RE,STA2                                                          
         BE    VS100                                                            
         MVI   WORK+4,C'L'         LOW POWER STATION                            
         EX    RE,STAL                                                          
         BNE   MYERR                                                            
         MVI   WORK+4,C'H'         NHTI                                         
         EX    RE,STAH                                                          
         BNE   MYERR                                                            
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
*                                                                               
         CLI   TWAACCS,C'$'        ONLINE STATION?                              
         BNE   VSEXT               NO                                           
*                                                                               
         MVC   RERROR,=AL2(55)     SECURITY LOCKOUT                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'06'                                                     
         USING RSTASOEL,R6                                                      
         BAS   RE,GETEL                                                         
         BNE   MYERR                                                            
*                                                                               
VS120    DS    0H                                                               
         CLC   RSTASID,TWAORIG                                                  
         BE    VSEXT                                                            
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    VS120                                                            
         B     MYERR                                                            
*                                                                               
VSEXT    B     XIT                                                              
         DROP  R4,R6                                                            
*                                                                               
STATV    CLC   22(0,R4),=C'TV'                                                  
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACM    CLC   22(0,R4),=C'CM'                                                  
STA1     CLC   22(0,R4),=C'1 '                                                  
STA2     CLC   22(0,R4),=C'2 '                                                  
STAL     CLC   22(0,R4),=C'L '                                                  
STAH     CLC   22(0,R4),=C'H '                                                  
         EJECT                                                                  
***************************************************************                 
*  VALIDATE OFFICE - R2 POINTS AT SCREEN FIELD ON ENTRY                         
*                 - WORK HAS OFFICE CODE ON EXIT                                
*                 - EOFF... HAS ADDRESS INFO ON EXIT                            
***************************************************************                 
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
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***************************************************************                 
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL            
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG.  SINCE RESFM             
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE            
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                               
***************************************************************                 
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
*******************************************************************             
*        CKGLOB --- CHECK FOR GLOBBER LOADER VARIABLES FROM THE                 
* CONTRACT PROGRAM                                                              
*******************************************************************             
CKGLOB   NTR1                                                                   
         CLI   TWAMODE,1               1=OFFLINE                                
         BE    CKGLGOOD                                                         
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGLOBBER,DMCB,=C'GETD',WORK,24,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BNE   CKGLGOOD                                                         
         GOTO1 CGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
*                                                                               
*                                  MUST BE A GENERIC RETURN CALL                
*                                                                               
         MVC   CONSERV(4),=C'=RE ' FORCE SCREEN REPAINT                         
         MVI   CONSERVH+5,3                                                     
         OI    CONSERVH+6,X'80'                                                 
         L     RD,4(RD)            GET ALL THE WAY OUT OF HERE                  
         B     XIT                                                              
*                                                                               
CKGLGOOD EQU   *                   CKGLOB EXIT                                  
         SR    R0,R0                                                            
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
XIT      DS    0H                                                               
         XIT1                                                                   
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              CONSTANTS TABLES ETC                                             
***********************************************************************         
RELO     DS    A                                                                
SAVERD   DS    A                                                                
SVDXDA   DS    F                   FOR PAV FILE READ                            
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(RECUP)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
*---------------------------------------------                                  
*  TABLE OF CORE RESIDENT MODULE ADDRESSES                                      
*---------------------------------------------                                  
CORETAB  DS    0X                                                               
         DC    X'30'               GENCON                                       
CORES    EQU   (*-CORETAB)                                                      
         EJECT                                                                  
**********************************************************************          
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
**********************************************************************          
RECACT   DS    0D                                                               
         DC    X'01',C'SONNET  ',AL1(01),X'0000'   SONNET                       
         DC    X'01',C'BOXID   ',AL1(02),X'0000'   BOXID                        
         SPACE 2                                                                
**********************************************************************          
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
**********************************************************************          
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         EJECT                                                                  
**********************************************************************          
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
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
**********************************************************************          
         DC    X'03',AL1(01,01),X'F101000080',C'    '  SONNET    MAINT          
         DC    X'03',AL1(01,10),X'F201000080',C'    '  SONNET    LIST           
         DC    X'03',AL1(02,01),X'F302000080',C'SNSN'  BOXID     MAINT          
         DC    X'03',AL1(02,10),X'F402000080',C'SNSN'  BOXID     LIST           
         DC    X'FF'                                                            
         EJECT                                                                  
         DROP  RB                                                               
*****************************************************************               
* PROCESS PF KEYS                                                               
*****************************************************************               
         DS    0H                                                               
VPFPROC  NMOD1 0,**PFPC**                                                       
         L     RC,0(R1)                                                         
                                                                                
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
         MVI   PFKEY,0             SO WE DON'T LOOP                             
         MVI   CALLSP,0            CLEAR STACK POINTER                          
VPF30    L     RF,4(RE)            A(CALLPROG CALL)                             
         AR    RF,RB                                                            
         BR    RF                                                               
         EJECT                                                                  
*                                                                               
* PFKEY JUMPS FROM SONNET                                                       
*                                                                               
* PF=3                                                                          
VPF300   DS    0H                                                               
         BAS   RE,GETCONNO         GETS CONTRACT #/SENDS VIA GLOBBER            
         BNE   VPFPROCX                                                         
         XC    FULL,FULL                                                        
         MVI   FULL,C'D'           DISPLAY CONTRACT                             
         BAS   RE,DOFULL           SEND VIA GLOBBER/SET SVC REQ FLDHDR          
         B     VPFPROCX                                                         
                                                                                
* PF=4                                                                          
VPF320   DS    0H                                                               
         BAS   RE,GETCONNO         GETS CONTRACT #/SENDS VIA GLOBBER            
         BNE   VPFPROCX                                                         
         XC    FULL,FULL                                                        
         MVI   FULL,C'X'            DIS,DX IN CONTRACT PROGRAM                  
         CLI   SVTRAFIC,C'B'       BIAS STATION?                                
         BE    VPF322              YES                                          
         CLI   SVTRAFIC,C'W'       BIAS STATION?                                
         BE    VPF322              YES                                          
         CLI   SVTRAFIC,C'J'       JDS 2000?                                    
         BE    VPF322              YES                                          
*                                                                               
         MVI   FULL,C'R'            DIS,ORD IN CONTRACT PROGRAM                 
VPF322   DS    0H                                                               
         BAS   RE,DOFULL                                                        
         B     VPFPROCX                                                         
                                                                                
* PF=5                                                                          
VPF340   DS    0H                                                               
         BAS   RE,GETCONNO         GETS CONTRACT #/SENDS VIA GLOBBER            
         BNE   VPFPROCX                                                         
         XC    FULL,FULL                                                        
         MVI   FULL,C'R'           CONFIRM                                      
         BAS   RE,DOFULL                                                        
         B     VPFPROCX                                                         
                                                                                
* PF=6 - DISABLED IN PFTABLE                                                    
VPF360   DS    0H                                                               
         BAS   RE,GETCONNO                                                      
         L     R2,DUB              GET LISTLINE FRM GETCONNO                    
         USING LISTD,R2                                                         
         CLI   LSVERN+3,C'0'         IF MOD # THEN CONFIRMED                    
         BL    VPFPROCX                                                         
         CLI   LSVERN+3,C'9'                                                    
         BH    VPFPROCX                                                         
         DROP  R2                                                               
*                                                                               
         XC    FULL,FULL                                                        
         MVI   FULL,C'M'           MGL IN CONTRACT PROGRAM                      
         BAS   RE,DOFULL                                                        
         B     VPFPROCX                                                         
                                                                                
* PF=7                                                                          
VPF370   DS    0H                                                               
         BAS   RE,GETCONNO                                                      
         XC    FULL,FULL                                                        
         MVI   FULL,C'T'           DIS,DSM,TOT IN CONTRACT PROGRAM              
         BAS   RE,DOFULL                                                        
         B     VPFPROCX                                                         
                                                                                
VPFPROCX DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*****************************************************************               
* FOR SONNET                                                                    
* GETS AND PASSES PWOS CONTRACT NUMBER IN DUB TO GLOBBER                        
*****************************************************************               
GETCONNO NTR1                                                                   
         L     R2,ATIOB                                                         
         USING TIOBD,R2                                                         
         LH    R3,TIOBCURD         PICK UP RELATIVE DISPLACEMENT                
         DROP  R2                                                               
         L     RA,ATWA                                                          
         AR    R3,RA               RA=TWA                                       
         CLI   0(R3),11            IS THIS SEL FIELD                            
         BNE   GETCONNX            NO/EXIT                                      
         LR    R2,R3                                                            
         ZIC   R1,0(R3)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),83            IS THIS LIST FIELD                           
         BNE   GETCONNX            NO/EXIT                                      
         LA    R2,8(R2)                                                         
         OC    0(8,R2),SPACES                                                   
         CLC   0(8,R2),SPACES                                                   
         BH    *+10                                                             
         LTR   RB,RB               SET NE                                       
         B     GETCONNX                                                         
*                                                                               
         ST    R2,DUB              MAY WANT THIS LATER                          
         CR    RB,RB                                                            
*                                                                               
GETCONNX B     VPFPROCX                                                         
         EJECT                                                                  
*****************************************************************               
*PASSES ONE BYTE CODE IN FULL TO GLOBBER                                        
*****************************************************************               
DOFULL   NTR1                                                                   
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'REP'                                                 
         MVC   GLVXFRPR,=C'SON'                                                 
         MVC   GLVXTOSY,=C'REP'                                                 
         MVC   GLVXTOPR,=C'CON'                                                 
         OI    GLVXFLG1,GLV1GOTO+GLV1SEPS   CALL BASE ON TRANSFER               
         DROP  R1                                                               
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGLOBBER,DMCB,=C'PUTD',ELEM,14,GLVXCTL                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    RE,ELEM                                                          
         USING GLCONNUM,RE                                                      
*                                                                               
         L     R2,DUB                                                           
         USING LISTD,R2                                                         
         MVC   GLCONNUM,LSCON                                                   
         CLI   FULL,C'D'                                                        
         BNE   *+16                                                             
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DSM'                                               
         CLI   FULL,C'X'                                                        
         BNE   *+16                                                             
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DX '                                               
         CLI   FULL,C'R'                                                        
         BNE   *+16                                                             
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'ORD'                                               
         CLI   FULL,C'T'                                                        
         BNE   *+22                                                             
         MVC   GLCONCA(3),=C'DIS'                                               
         MVC   GLCONBA(3),=C'DSM'                                               
         MVC   GLCONBN(3),=C'TOT'                                               
         CLI   FULL,C'M'                                                        
         BNE   *+10                                                             
         MVC   GLCONCA(3),=C'MGL'                                               
         DROP  RE                                                               
*                                                                               
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGLOBBER,DMCB,=C'PUTD',ELEM,GLCONLNQ,GLRKACT                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         B     VPFPROCX                                                         
         EJECT                                                                  
*********************************************************************           
* BYTE 1:    SAVED SCREEN NUMBER                                                
* BYTE 2:    PFKEY NUMBER                                                       
* BYTE 3-4:  SPARE                                                              
* BYTE 5-8:  A(DISPLACEMENT TO PROGRAM CALL)                                    
*********************************************************************           
PFTABLE  DS    0F                                                               
         DC    X'F2030000',A(VPF300-VPFPROC)                                    
         DC    X'F2040000',A(VPF320-VPFPROC)                                    
         DC    X'F2050000',A(VPF340-VPFPROC)                                    
**       DC    X'F2060000',A(VPF360-VPFPROC)                                    
         DC    X'F2070000',A(VPF370-VPFPROC)                                    
         DC    X'FFFF'                                                          
         EJECT                                                                  
         LTORG                                                                  
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
* RESUNFFD *                                                                    
************                                                                    
         SPACE 1                                                                
       ++INCLUDE RESUNFFD                                                       
         SPACE 1                                                                
**********************************************                                  
* DDGENTWA - DSECT TO COVER GENCON TWA AREAS *                                  
**********************************************                                  
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
**************                                                                  
* RESUNWORKD *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE RESUNWRK                                                       
         EJECT                                                                  
****************************                                                    
* DDCOMFACS                *                                                    
* REGENALL2                *                                                    
* FAFACTS                  *                                                    
* FATIOB                   *                                                    
* DDCOREQUS                *                                                    
* DDGLOBEQUS               *                                                    
* DDGLVXCTLD               *                                                    
* REGLCON                  *                                                    
* FAGETTXTD                *                                                    
* CTGENFILE                *                                                    
****************************                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE REGENALL2                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE REGLCON                                                        
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006RESUN00S  05/01/02'                                      
         END                                                                    
