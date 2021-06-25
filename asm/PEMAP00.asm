*          DATA SET PEMAP00    AT LEVEL 035 AS OF 05/01/02                      
*PHASE TE1B00A                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'TE1B00 - MAP CONTROLLER'                                        
TE1B00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1500,**TE1B00,RR=R2                                              
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         USING TE1B00+4096,R7                                                   
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
         LA    RF,1500             CLEAR SELECTED STORAGE                       
         SLL   RF,3                                                             
         XCEF                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=H'2048'         GRABBING 2 1024 BYTE I/O AREAS               
         LA    R9,16(R9)           NEED SPACE FOR 2 8BYTE LABELS                
         USING SYSD,R9                                                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         L     R9,SYSPARMS                                                      
         L     R2,8(R9)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
*              INITIALIZE SYSTEM DEPENDENT VALUES                               
         SPACE 3                                                                
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,VCOUNT                                                        
         SPACE 1                                                                
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
         SPACE 1                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
         SPACE 1                                                                
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
         SPACE 1                                                                
*                                  SET SYSTEM DEPENDENT VALUES                  
         SPACE 1                                                                
         LA    R1,STARTSAV                                                      
         ST    R1,ASTARTSV                                                      
         MVI   SYSTEM,C'Q'         PERSONNEL                                    
         MVI   MAXIOS,2            USES 2 I/O AREAS                             
         MVC   SIZEIO,=F'1024'     EACH I/O IS 1024 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'36'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'44'     USUALLY PERFIL                               
         MVC   SYSFIL,=C'PERFIL  '                                              
         MVC   SYSDIR,=C'PERDIR  '                                              
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVC   LWORK,=F'12000'     WE TOOK 12000 BYTES IN NMOD                  
*&&UK*&& MVC   RCPROG(2),=C'PE'    PREFIX FOR REPORT NO.                        
*&&US*&& MVC   RCPROG(2),=C'MP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D90E1B00'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         B     XIT                                                              
         EJECT                                                                  
*              SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                   
         SPACE 3                                                                
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 1                                                                
VBRANCH  B     VUSER                                                            
         B     VCHAT                                                            
         B     DCHAT                                                            
         B     VPERS                                                            
         B     VSYS                                                             
         B     VSYSUSER                                                         
         B     VPROJ                                                            
         B     VTYPE                                                            
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
*              USER AGENCY                                                      
         SPACE 3                                                                
VUSER    L     R1,SYSPARMS                                                      
         MVC   AGENCY,0(R1)                                                     
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
         MVC   FILENAME,=CL8'CTFILE'                                            
         MVI   USEIO,C'Y'                                                       
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)                                               
         GOTO1 READ                                                             
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VUSER2                                                           
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         SPACE 1                                                                
VUSER2   XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         B     XIT                                                              
         EJECT                                                                  
*              HANDLE COMMENT TYPE ELEMENTS                                     
         SPACE 3                                                                
VCHAT    CLI   MODE,DISPREC                                                     
         BE    DCHAT                                                            
         ZIC   R0,MAX              MAX FIELDS                                   
         LA    R4,1                SEQUENCE NUMBER                              
         GOTO1 REMELEM             TAKE OUT EXISTING                            
         SPACE 1                                                                
VCHAT2   TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         BAS   RE,BUMP             SKIP PAST PROTECTED FIELD                    
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         MVC   ELEMENT(1),ELCODE   BUILD AN ELEMENT                             
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT+4(0),8(R2)  MOVE IN DATA                                 
         LA    R3,5(R3)                                                         
         STC   R3,ELEMENT+1        LENGTH OF L'DATA+4                           
         STC   R4,ELEMENT+2        SEQUENCE NO.                                 
         MVC   ELEMENT+3(1),OPTION USER CAN SUPPLY BYTE 4                       
         GOTO1 ADDELEM                                                          
         BAS   RE,BUMP                                                          
         LA    R4,1(R4)                                                         
         BCT   R0,VCHAT2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DCHAT    ZIC   R0,MAX              DISPLAY UP TO MAX FIELDS                     
         L     R6,AIO              USER SUPPLIED ELCODE                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE 1                                                                
DCHAT2   BAS   RE,NEXTEL                                                        
         BNE   DCHAT4                                                           
         MVC   WORK,SPACES                                                      
         ZIC   R1,1(R6)                                                         
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),4(R6)       PICK OUT DATA                                
         BAS   RE,GENDISP                                                       
         BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         BAS   RE,BUMP             SKIP PAST PROTECTED FIELD                    
         BCT   R0,DCHAT2                                                        
         B     XIT                                                              
         SPACE 1                                                                
DCHAT4   MVC   WORK,SPACES         CLEAR REST                                   
         BAS   RE,GENDISP                                                       
         BAS   RE,BUMP                                                          
         TM    1(R2),X'20'                                                      
         BZ    *+8                                                              
         BAS   RE,BUMP             SKIP PAST PROTECTED FIELD                    
         BCT   R0,DCHAT4                                                        
         B     XIT                                                              
         EJECT                                                                  
*              SYSTEM VALIDATION ROUTINES                                       
         SPACE 3                                                                
         USING MAPKEYD,R4                                                       
VPERS    GOTO1 ANY                 PERSON                                       
         BAS   RE,KEYSET                                                        
         MVI   PERKTYP,X'02'                                                    
         MVC   PERCODE,WORK                                                     
         MVI   ERROR,NOPERSON                                                   
         B     VTRYHARD                                                         
         SPACE 1                                                                
VSYSUSER CLI   OPTION,C'W'         SYSTEM USER                                  
         BE    VSU2                (MAY ALREADY BE IN WORK)                     
         GOTO1 ANY                                                              
         SPACE 1                                                                
VSU2     BAS   RE,KEYSET                                                        
         MVI   USRKTYP,X'03'                                                    
         MVC   USRCODE,WORK                                                     
         MVI   ERROR,NOUSER                                                     
         B     VTRYHARD            N. B. TRYSOFT BLITZES USRCODE                
         SPACE 1                                                                
VSYS     GOTO1 ANY                 SYSTEM                                       
         BAS   RE,KEYSET                                                        
         MVI   SYSKTYP,X'04'                                                    
         MVC   SYSCODE,WORK                                                     
         MVI   ERROR,NOSYSTEM                                                   
         ZIC   R3,5(R2)                                                         
         LA    R3,2(R3)                                                         
         BAS   RE,VTRYSOFT                                                      
         B     XIT                                                              
         SPACE 1                                                                
VPROJ    GOTO1 ANY                 PROJECT                                      
         BAS   RE,KEYSET                                                        
         MVI   PRJKTYP,X'05'                                                    
         MVC   PRJSYS,SAVSYSCD                                                  
         MVC   SAVPJCOD,WORK                                                    
         MVC   PRJCODE,WORK                                                     
         MVI   ERROR,NOPROJ                                                     
         B     VTRYHARD                                                         
         SPACE 1                                                                
VTYPE    GOTO1 ANY                                                              
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         LA    R1,TYPLIST                                                       
         MVI   ERROR,INVTYPE                                                    
         SPACE 1                                                                
VTYPE2   CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
         MVC   SAVTYPE,0(R1)                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),1(R1)                                                    
         BE    XIT                                                              
         LA    R1,8(R1)                                                         
         B     VTYPE2                                                           
         SPACE 1                                                                
TYPLIST  DC    AL1(10),CL7'DESIGN'                                              
         DC    AL1(20),CL7'SYSTEMS'                                             
         DC    AL1(30),CL7'TESTING'                                             
         DC    AL1(40),CL7'MANUAL'                                              
         DC    AL1(50),CL7'DOC.'                                                
         DC    AL1(60),CL7'INSTALL'                                             
         DC    AL1(70),CL7'CONTROL'                                             
         DC    AL1(90),CL7'OTHERS'                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 1                                                                
GENDISP  NTR1                                                                   
         CLI   OPTION,C'W'         OPTION TO RETURN DATA IN WORK                
         BE    XIT                                                              
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,GENCLC           IS DATA ON SCREEN ALREADY                    
         BE    XIT                                                              
         EX    R1,GENMVC           NO - SO MOVE IT THERE                        
         OI    6(R2),X'80'              AND TRANSMIT                            
         B     XIT                                                              
         SPACE 1                                                                
GENCLC   CLC   8(0,R2),WORK                                                     
GENMVC   MVC   8(0,R2),WORK                                                     
         SPACE 1                                                                
BUMP     ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BR    RE                                                               
         SPACE 1                                                                
KEYSET   MVC   SYSKEY,KEY          PRELIMINARY KEY HELP                         
         MVC   SYSKEYSV,KEYSAVE                                                 
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVC   MAPAGY,AGENCY                                                    
         MVI   MAPSYS,C'B'                                                      
         BR    RE                                                               
         SPACE 1                                                                
VTRYHARD LA    R3,35                                                            
         BAS   RE,VTRYSOFT                                                      
         B     XIT                                                              
         SPACE 1                                                                
VTRYSOFT NTR1                                                                   
         GOTO1 HIGH                SEE IF WE CAN GET RECORD                     
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(00),KEYSAVE                                                  
         BNE   TRAPERR             NO - OUT                                     
         CLI   SYSKTYP,X'04'       IF WE WERE GOING FOR A SYSTEM                
         BNE   *+10                                                             
         MVC   SAVSYSCD,SYSCODE    SAVE COMPLETE SYSTEM CODE                    
         MVC   KEY,SYSKEY          YES - RESTORE USERS VALUES                   
         MVC   KEYSAVE,SYSKEYSV                                                 
         B     XIT                                                              
         SPACE 1                                                                
TRAPERR  GOTO1 ERREX                                                            
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS TABLES ETC                                             
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
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
         DC    X'01',C'PERSON  ',AL1(02),X'00C2'                                
         DC    X'01',C'USER    ',AL1(03),X'00C2'                                
         DC    X'01',C'SYSTEM  ',AL1(04),X'00C2'                                
         DC    X'01',C'PROJECT ',AL1(05),X'00C2'                                
         DC    X'01',C'TASK    ',AL1(06),X'00C2'                                
         DC    X'01',C'DIARY   ',AL1(07),X'00C2'                                
         DC    X'01',C'SCRATCH ',AL1(08),X'00C2'                                
         DC    X'01',C'TODAY   ',AL1(08),X'00C2'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'04' ENTRIES ARE PROGRAM RECORDS            
*                                  SAME FORMAT AS X'01'                         
         DC    X'04',C'SUMMARY ',AL1(10),X'0000'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'CALENDAR',AL1(14,14,00)                                  
         DC    X'02',C'MANAGE  ',AL1(16,16,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
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
         DC    X'03',AL1(02,01),X'F202000080',C'    '  PERSON    MAINT          
         DC    X'03',AL1(02,10),X'E202000280',C'    '            LIST           
         DC    X'03',AL1(02,12),X'D212001238',C'MRMM'            REPORT         
         DC    X'03',AL1(03,01),X'F303000080',C'    '  USER      MAINT          
         DC    X'03',AL1(03,10),X'E303000380',C'    '            LIST           
         DC    X'03',AL1(03,12),X'0000000378',C'URMU'            REPORT         
         DC    X'03',AL1(04,01),X'F404000080',C'    '  SYSTEM    MAINT          
         DC    X'03',AL1(04,10),X'E404000480',C'    '            LIST           
         DC    X'03',AL1(04,12),X'0000000478',C'SRMR'            REPORT         
         DC    X'03',AL1(05,01),X'F505000080',C'    '  PROJECT   MAINT          
         DC    X'03',AL1(05,10),X'E505000580',C'    '            LIST           
         DC    X'03',AL1(05,12),X'D505000578',C'PRMP'            REPORT         
         DC    X'03',AL1(05,16),X'C515001578',C'PMMP'            MANAGE         
         DC    X'03',AL1(06,01),X'F606000080',C'    '  TASK      MAINT          
         DC    X'03',AL1(06,10),X'E606000680',C'    '            LIST           
         DC    X'03',AL1(06,12),X'D606000678',C'TRMT'            REPORT         
         DC    X'03',AL1(06,14),X'D616001638',C'CAMC'            CAL            
         DC    X'03',AL1(07,01),X'F707000080',C'    '  DIARY     MAINT          
         DC    X'03',AL1(07,10),X'E707000780',C'    '            LIST           
         DC    X'03',AL1(08,01),X'F808000080',C'    '  SCRATCH   MAINT          
         DC    X'03',AL1(08,10),X'E808000880',C'    '            LIST           
         DC    X'03',AL1(10,12),X'D80A000A38',C'SUMR'  SUMMARY   REPORT         
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PEMAPWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PEMAPFILE                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035PEMAP00   05/01/02'                                      
         END                                                                    
