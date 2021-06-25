*          DATA SET RESTR00S   AT LEVEL 021 AS OF 05/01/02                      
*PHASE T80E00A,*                                                                
*INCLUDE UNBOOK                                                                 
*INCLUDE CLPACK                                                                 
*INCLUDE UNUPGR                                                                 
*INCLUDE RETEXT                                                                 
*INCLUDE UNTEXT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE UPOUT                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T80E00 - RESTR00 - REP STRATEGY GOAL-ORIENTED SYSTEM'           
*                                                                               
*******************************************************************             
*                                                                 *             
*    RESTR00 --- REP STRATEGY GOAL-ORIENTED MANAGEMENT SYSTEM     *             
*                                                                 *             
* --------------------------------------------------------------- *             
* UPDATE HISTORY:                                                 *             
*                                                                 *             
* JAN13/94 (SKU) --- INITIAL RELEASE                              *             
*                                                                 *             
* MAR07/94 (SKU) --- ADD GROUP/SUBGROUP VALIDATION                *             
*                                                                 *             
* MAY09/94 (SKU) --- MADE PFKEYS CONSISTENT THRU OUT              *             
*                                                                 *             
*                    **  END TOMBSTONE  **                        *             
*HERE**************************************************************             
T80E00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T80E00,R7,R5,RR=R2,CLEAR=YES                           
         USING GEND,RC                                                          
                                                                                
         USING SPOOLD,R8                                                        
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
                                                                                
         ST    R2,RELO                                                          
         ST    RD,SAVERD                                                        
         LR    R8,RC               R8=A(SPOOLD)                                 
         LA    RC,SPOOLEND         RC=A(GENCON STORAGE)                         
         LA    R9,IO               START OF IO 1 + TOTAL IO AREA LEN            
         AH    R9,=Y(LENIOAS)      R9=A(SFM SYSTEM WORKING STORAGE)             
*                                  GRABBING 3 2096 BYTE I/O AREAS               
                                                                                
         ST    R1,SYSPARMS                                                      
         L     RF,0(R1)                                                         
         ST    RF,ATIOB                                                         
                                                                                
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         C     R0,=F'12'                                                        
         BNH   *+8                                                              
         S     R0,=F'12'                                                        
         STC   R0,PFKEY            PFKEY ADJUSTED TO 1..12 (0 = ENTER)          
         DROP  RF                                                               
                                                                                
         L     RA,4(R1)                                                         
         ST    RA,ATWA             RA=A(TWA)                                    
         USING CONHEADH-64,RA                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
                                                                                
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    R7,SYSR7            SECOND BASE REGISTER                         
         ST    R5,SYSR5            THIRD BASE REGISTER                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
                                                                                
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
                                                                                
         MVI   RETURNED,0          HELPS DETERMINE FROM WHENCE WE CAME          
                                                                                
         MVI   RACHANG,C'N'        SET FLAG TO IDICATE IF USER                  
         TM    CONRECH+4,X'20'     CHANGED RECORD/ACTION                        
         BZ    *+12                                                             
         TM    CONACTH+4,X'20'                                                  
         BO    *+8                                                              
         MVI   RACHANG,C'Y'                                                     
                                                                                
* CALL A ROUTINE THAT CALLS GENCON SO THAT GENCON WILL RETURN TO                
* NEXT INSTRUCTION AFTER THIS                                                   
* DON'T GO AGAIN UNLESS OVERLAY WANTS TO CALL ANOTHER OVERLAY                   
                                                                                
AGAIN    MVI   GOAGAIN,C'N'                                                     
         BAS   RE,CALLGENC                                                      
                                                                                
* IF OVERLAY WISHED TO CALL GENCON WITH A NEW RECORD AND ACTION,                
* THEN THIS FLAG WILL BE SET                                                    
                                                                                
         CLI   GOAGAIN,C'Y'                                                     
         BNE   DONE                                                             
         NI    CONRECH+6,X'BF'     RESET INSERT CURSOR BIT HERE                 
         B     AGAIN                                                            
                                                                                
DONE     OI    CONRECH+4,X'20'                                                  
         OI    CONACTH+4,X'20'                                                  
         B     XIT                 ELSE EXIT BACK TO USER                       
         EJECT                                                                  
*******************************************************************             
* THIS ROUTINE CALL GENCON BUT DOES NOT RETAIN CONTROL WHEN GENCON              
* EXITS.  INSTEAD, THE ROUTINE THAT CALLS THIS WILL GET CONTROL.                
* THIS ALLOWS THE CONTROLLER TO CALL GENCON AGAIN WITH A NEW                    
* RECORD AND ACTION IF, FOR INSTANCE, AN OVERLAY HAD A LIST ACTION              
* AND A SELECTION WAS MADE.                                                     
*******************************************************************             
CALLGENC NTR1                                                                   
         ST    RD,SYSRD            GENCON USES THIS TO EXIT ITSELF              
                                                                                
         GOTO1 GENCON,DMCB,(R8)    CALL GENCON-PASS A(WORKING STORAGE)          
         B     XIT                 THEN WE'RE THROUGH                           
         EJECT                                                                  
*******************************************************************             
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
*******************************************************************             
SYSINIT  NTR1                                                                   
*              GET TERMINAL VALUES                                              
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         MVC   TERM,TWATRM                                                      
         MVC   AUTH,TWAAUTH                                                     
         MVC   USERID,TWAORIG                                                   
         MVC   AGENCY,TWAAGY                                                    
                                                                                
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
                                                                                
SYS20    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS20                                                         
                                                                                
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R1,VCOUNT                                                        
                                                                                
SYS40    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R1,SYS40                                                         
*                                  SET SYSTEM DEPENDENT VALUES                  
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
         MVC   SYSPHASE,=X'D9080E00'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
                                                                                
* SET UP CERTAIN ROUTINE ADDRESSES - CAN'T WAIT FOR GENCON                      
                                                                                
         L     R1,SYSPARMS                                                      
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
         OI    GENSTAT3,USEKEYSV   USE KEYSAVE,KEY (INTEREP)                    
                                                                                
SYSXIT   DS    0H                                                               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*******************************************************************             
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
*******************************************************************             
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
                                                                                
                                                                                
VBRANCH  B     VUSER                                                            
         B     VACC                ACCESS CHECK                                 
         B     VOFF                VALIDATE OFFICE                              
         B     VADV                VALIDATE ADVERTISER                          
         B     VGRP                VALIDATE GROUP/SUBGROUP                      
         B     VSTA                VALIDATE STATION                             
         B     VPER                VALIDATE PERIOD                              
         B     DSGDC               DISPLAY SHARE GOAL AND DESCRIPTION           
         B     VCMT                VALIDATE STANDARD COMMENT                    
         B     DCMT                DISPLAY STANDARD COMMENT                     
         B     PACK                PACK                                         
         B     UPDT                UPDATE LAST CHANGE DATE                      
         B     CPROG               SWITCH BETWEEN OVERLAYS                      
         B     RPROG               RETURN TO PREVIOUS OVERLAY                   
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         NOP   *                                                                
         B     VINIT               INITIALIZE SYSSPARE                          
         B     MYERR               CALL GETTXT FOR MESSAGE                      
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*******************************************************************             
* VUSER - GET REP DATA FROM CONTROL FILE USER ID RECORD                         
*******************************************************************             
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
                                                                                
VU10     XC    FILENAME,FILENAME                                                
         MVI   USEIO,0                                                          
         DROP  R4                                                               
                                                                                
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    VUSERX              YES -- THERE CAN'T BE ANY PFKEYS             
                                                                                
         CLI   RACHANG,C'Y'        IF USER CHANGED RECORD/ACTION                
         BNE   *+8                                                              
         MVI   CALLSP,0            THEN RESET STACK POINTER                     
                                                                                
         CLI   PFKEY,0             WAS ENTER PRESSED?                           
         BE    VUSERX              YES -- NORMAL EXIT                           
***********************************************************************         
* PROCESS PFKEYS                                                                
***********************************************************************         
VU20     LA    RE,PFTABLE          TABLE OF SCREENS AND PF KEYS                 
         MVC   HALF(1),MYSCRNUM    SAVED SCREEN NUMBER                          
         MVC   HALF+1(1),PFKEY     PRESSED PFKEY                                
                                                                                
VU30     CLC   HALF,0(RE)          MATCH ON SCREEN/PFKEY?                       
         BE    VU40                YES                                          
         LA    RE,8(RE)            BUMP TO NEXT ENTRY IN TABLE                  
         CLC   =X'FFFF',0(RE)      END OF TABLE?                                
         BE    VUSERX              YES                                          
         B     VU30                                                             
***********************************************************************         
* IF CHANGE SELECT FROM LIST, WE WANT TO STAY WITH THE DISPLAY SCREEN           
* AFTER A PFKEY IS HIT, INSTEAD OF RETURNING TO THE LIST SCREEN                 
***********************************************************************         
VU40     DS    0H                                                               
         LA    RF,SPCPFKEY                                                      
VU42     CLC   HALF,0(RF)                                                       
         BE    VU43                                                             
         LA    RF,L'SPCPFKEY(RF)                                                
         CLC   =X'FFFF',0(RF)                                                   
         BNE   VU42                                                             
                                                                                
         MVI   PFKEY,0             SO WE DON'T LOOP                             
         MVI   CALLSP,0            CLEAR STACK POINTER                          
VU43     L     RF,4(RE)            A(CALLPROG CALL)                             
         A     RF,RELO                                                          
         BR    RF                                                               
                                                                                
SPCPFKEY DS    0XL2                                                             
*                                  XL1 PHASE NUMBER FOR SCREEN                  
*                                  XL1 PFKEY TO PROCESS                         
         DC    X'F102F103'         SITANA SCREEN                                
         DC    X'F302F303'         SAT SCREEN                                   
         DC    X'F502F503'         GKC SCREEN                                   
         DC    X'F702F703'         SAC SCREEN                                   
         DC    X'F902F903'         ACCOUNTS SCREEN                              
         DC    X'F90AF90B'         ACCOUNTS SCREEN                              
         DC    X'FFFF'                                                          
         EJECT                                                                  
VU120    DS    0H                  STAY WITH SAME SCREEN IF PFKEY               
         CLC   =C'SEL',CONACT      ACTION SELECT?                               
         BNE   VUSERX                                                           
         CLI   THISLSEL,C'C'       'C' ENTERED AS LIST SELECTION?               
         BNE   VU123                                                            
         CLI   PFKEY,2             ADD A LINE?                                  
         BE    VU127                                                            
         CLI   PFKEY,3             DEL A LINE?                                  
         BE    VU127                                                            
         CLI   PFKEY,10            PAGE UP?                                     
         BE    VU127                                                            
         CLI   PFKEY,11            PAGE DOWN?                                   
         BE    VU127                                                            
         B     VUSERX                                                           
                                                                                
VU123    DS    0H                                                               
         CLI   THISLSEL,C'S'       'S' ENTERED AS LIST SELECTION?               
         BNE   VUSERX                                                           
         CLI   PFKEY,10            PAGE UP?                                     
         BE    VU124                                                            
         CLI   PFKEY,11            PAGE DOWN?                                   
         BNE   VUSERX                                                           
                                                                                
* HERE'S A NEAT TRICK TO PAGE UP AND DOWN IN THE SELECT SCREEN:                 
* IF PF9 OR 10 IS HIT WE JUST PUT THE SELECTION ACTION BACK ON                  
* THE ACTION COLUMN.                                                            
                                                                                
VU124    DS    0H                                                               
         LA    R3,LISTDIR          FIND WHERE WE ARE IN THE LIST SCREEN         
         ZIC   R0,LISTNUM            THAT IS BEING SELECTED                     
VU125    CLC   LASTSEL,2(R3)       FIND MATCHING D/A                            
         BE    VU126                                                            
         LA    R3,6(R3)                                                         
         BCT   R0,VU125                                                         
         B     VUSERX                                                           
                                                                                
VU126    DS    0H                                                               
         MVI   0(R3),C'S'          STAY ON THE SAME SCREEN!                     
         B     VUSERX                                                           
                                                                                
VU127    DS    0H                                                               
         LA    R3,LISTDIR          FIND WHERE WE ARE IN THE LIST SCREEN         
         ZIC   R0,LISTNUM            THAT IS BEING SELECTED                     
VU128    CLC   LASTSEL,2(R3)       FIND MATCHING D/A                            
         BE    VU129                                                            
         LA    R3,6(R3)                                                         
         BCT   R0,VU128                                                         
         B     VUSERX                                                           
                                                                                
VU129    DS    0H                                                               
         MVI   0(R3),C'C'          STAY ON THE SAME SCREEN!                     
         CLI   PFKEY,2             ADD?                                         
         BE    VU129A                                                           
         CLI   PFKEY,3             DELETE?                                      
         BNE   VUSERX                                                           
                                                                                
VU129A   DS    0H                                                               
         OI    STRFLAGS,PFKEYHIT  SO APPLICATION WILL GOTO VALREC               
         B     VUSERX                                                           
                                                                                
VU150    GOTO1 CALLPROG,BLOCK,0,=C'SITANA',=C'REPORT',=C'NOW,STR',     X        
               (2,=C'..'),(7,STRSTAT),(13,STRPERI),0                            
*                                                                               
VU160    GOTO1 CALLPROG,BLOCK,0,=C'SITANA',=C'DISPLAY',0,(7,STRSTAT),  X        
               (13,STRPERI),0                                                   
                                                                                
VU170    GOTO1 CALLPROG,BLOCK,0,=C'SAT',=C'DISPLAY',0,(7,STRSTAT),     X        
               (13,STRPERI),0                                                   
                                                                                
VU180    GOTO1 CALLPROG,BLOCK,0,=C'GKC',=C'DISPLAY',0,(7,STRSTAT),     X        
               (13,STRPERI),0                                                   
                                                                                
VU190    GOTO1 CALLPROG,BLOCK,0,=C'SAC',=C'DISPLAY',0,(7,STRSTAT),     X        
               (13,STRPERI),0                                                   
                                                                                
VU200    GOTO1 CALLPROG,BLOCK,0,=C'ACCOUNTS',=C'DISPLAY',0,(7,STRSTAT),X        
               (13,STRPERI),0                                                   
                                                                                
VUSERX   B     XIT                                                              
         EJECT                                                                  
PFTABLE  DS    0F                                                               
                                                                                
* BYTE 1:    SAVED SCREEN NUMBER                                                
* BYTE 2:    PFKEY NUMBER                                                       
* BYTE 3-4:  SPARE                                                              
* BYTE 5-8:  A(CALLPROG CALL)                                                   
                                                                                
         DC    X'F1020000',A(VU120)       SIT  --> PF2 --> SIT  INS             
         DC    X'F1030000',A(VU120)       SIT  --> PF3 --> SIT  DEL             
         DC    X'F1040000',A(VU150)       SIT  --> PF4 --> SIT  REP             
         DC    X'F1060000',A(VU180)       SIT  --> PF6 --> GKC  DIS             
         DC    X'F1070000',A(VU170)       SIT  --> PF7 --> SAT  DIS             
         DC    X'F1080000',A(VU190)       SIT  --> PF8 --> SAC  DIS             
         DC    X'F1090000',A(VU200)       SIT  --> PF9 --> ACC  DIS             
                                                                                
         DC    X'F5020000',A(VU120)       GKC  --> PF2 --> GKC  INS             
         DC    X'F5030000',A(VU120)       GKC  --> PF3 --> GKC  DEL             
         DC    X'F5040000',A(VU150)       GKC  --> PF4 --> SIT  REP             
         DC    X'F5050000',A(VU160)       GKC  --> PF5 --> SIT  DIS             
         DC    X'F5070000',A(VU170)       GKC  --> PF7 --> SAT  DIS             
         DC    X'F5080000',A(VU190)       GKC  --> PF8 --> SAC  DIS             
         DC    X'F5090000',A(VU200)       GKC  --> PF9 --> ACC  DIS             
                                                                                
         DC    X'F3020000',A(VU120)       SAT  --> PF2 --> SAT  INS             
         DC    X'F3030000',A(VU120)       SAT  --> PF3 --> SAT  DEL             
         DC    X'F3040000',A(VU150)       SAT  --> PF4 --> SIT  REP             
         DC    X'F3050000',A(VU160)       SAT  --> PF5 --> SIT  DIS             
         DC    X'F3060000',A(VU180)       SAT  --> PF6 --> GKC  DIS             
         DC    X'F3080000',A(VU190)       SAT  --> PF8 --> SAC  DIS             
         DC    X'F3090000',A(VU200)       SAT  --> PF9 --> ACC  DIS             
                                                                                
         DC    X'F7020000',A(VU120)       SAC  --> PF2 --> SAC  INS             
         DC    X'F7030000',A(VU120)       SAC  --> PF3 --> SAC  DEL             
         DC    X'F7040000',A(VU150)       SAC  --> PF4 --> SIT  REP             
         DC    X'F7050000',A(VU160)       SAC  --> PF5 --> SIT  DIS             
         DC    X'F7060000',A(VU180)       SAC  --> PF6 --> GKC  DIS             
         DC    X'F7070000',A(VU170)       SAC  --> PF7 --> SAT  DIS             
         DC    X'F7090000',A(VU200)       SAC  --> PF9 --> ACC  DIS             
                                                                                
         DC    X'F9020000',A(VU120)       ACC  --> PF2 --> ACC  INS             
         DC    X'F9030000',A(VU120)       ACC  --> PF3 --> ACC  DEL             
         DC    X'F9040000',A(VU150)       ACC  --> PF4 --> SIT  REP             
         DC    X'F9050000',A(VU160)       ACC  --> PF5 --> SIT  DIS             
         DC    X'F9060000',A(VU170)       ACC  --> PF6 --> GKC  DIS             
         DC    X'F9070000',A(VU180)       ACC  --> PF7 --> SAT  DIS             
         DC    X'F9080000',A(VU190)       ACC  --> PF8 --> SAC  DIS             
         DC    X'F90A0000',A(VU120)       ACC  --> PF10--> ACC  PGUP            
         DC    X'F90B0000',A(VU120)       ACC  --> PF11--> ACC  PGDN            
         DC    X'FFFF'                                                          
         EJECT                                                                  
*******************************************************************             
*  CHECK STATION ACCESS                                                         
*  - ASSUMES STATION RECORD IN IO3                                              
*******************************************************************             
VACC     DS    0H                                                               
         MVC   SVELCODE,ELCODE                                                  
                                                                                
         CLI   TWAACCS,C'$'                                                     
         BNE   VACCYES                                                          
         L     R6,AIO3                                                          
         USING RSTASOEL,R6                                                      
         MVI   ELCODE,6            GET VALID SIGN ON ID ELEMENT                 
         BAS   RE,GETEL                                                         
         BNE   VACCYES                                                          
                                                                                
VACC10   DS    0H                                                               
         CLC   RSTASID,TWAORIG     VALID SIGN-ON?                               
         BE    VACCYES             YES, PROCEED                                 
         BAS   RE,NEXTEL           NOPE, CHECK NEXT ELEMENT                     
         BE    VACC10                                                           
         DROP  R6                  ALL DONE, NO MATCH, NOT VALID                
                                                                                
VACCNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
VACCYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         MVC   ELCODE,SVELCODE                                                  
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*  VALIDATE OFFICE                                                              
*  - R2 POINTS AT SCREEN FIELD ON ENTRY                                         
*  - WORK HAS OFFICE EXPANSION ON EXIT                                          
*  - USES IO3                                                                   
*******************************************************************             
VOFF     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ROFFKEY,R4                                                       
         MVI   ROFFKTYP,4                                                       
         MVC   ROFFKREP,AGENCY     REP                                          
         MVC   ROFFKOFF,8(R2)                                                   
         OC    ROFFKOFF,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VOFFNO                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK(20),ROFFNAME                                                
         DROP  R4                                                               
                                                                                
         MVC   AIO,SVAIO                                                        
         B     VOFFYES                                                          
*                                                                               
VOFFNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
VOFFYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
*  VALIDATE ADVERTISER                                                          
*  - R2 POINTS AT SCREEN FIELD ON ENTRY                                         
*  - WORK HAS ADVERTISER NAMEON EXIT                                            
*  USES IO3                                                                     
*******************************************************************             
VADV     DS    0H                                                               
         XC    KEY,KEY                                                          
         XC    WORK,WORK                                                        
         LA    R4,KEY                                                           
         USING RADVKEY,R4                                                       
         MVI   RADVKTYP,8                                                       
         MVC   RADVKREP,AGENCY     REP                                          
         MVC   RADVKADV,8(R2)                                                   
         OC    RADVKADV,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VADVNO                                                           
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVC   WORK(20),RADVNAME   GROUP NAME                                   
         DROP  R4                                                               
                                                                                
         MVC   AIO,SVAIO                                                        
         B     VADVYES                                                          
*                                                                               
VADVNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
VADVYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* VALIDATE GROUP/SUBGROUP                                                       
* WORK    - GROUP NAME                                                          
* WORK+10 - SUBGROUP NAME                                                       
* USES IO3                                                                      
*******************************************************************             
VGRP     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RGRPKEY,R6                                                       
         MVI   RGRPKTYP,7                                                       
         MVC   RGRPKREP,AGENCY     REP                                          
         MVC   RGRPKGRP,8(R2)                                                   
         OC    RGRPKGRP,SPACES                                                  
         MVI   RDUPDATE,C'N'                                                    
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
                                                                                
         CLC   KEY(27),KEYSAVE                                                  
         BNE   VGRPNO                                                           
                                                                                
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RGRPREC,R6                                                       
         MVC   WORK(10),RGRPNAME   GROUP NAME                                   
         MVC   WORK+10(10),RGRPSBNM   SUB GROUP NAME                            
         MVC   AIO,SVAIO                                                        
         B     VGRPYES                                                          
         DROP  R6                                                               
                                                                                
VGRPNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
VGRPYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* VALIDATE STATION CALL LETTERS                                                 
* WORK    - STATION CALL LETTERS                                                
* WORK+4  - A=AM F=FM C=CM T=BLANK L=LOW POWER TV                               
* WORK+10 - MARKET NAME                                                         
* WORK+40 - 1 OR 2 IF SATELLITE STATION                                         
* WORK+41 - GROUP/SUBGROUP CODE                                                 
* USES IO3                                                                      
*******************************************************************             
VSTA     DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         MVC   WORK(50),SPACES                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         MVC   RERROR,=AL2(INVSTA)                                              
         LA    R4,BLOCK                                                         
                                                                                
         CLI   0(R4),3                                                          
         BL    MYERR                                                            
         CLI   0(R4),4                                                          
         BH    MYERR                                                            
         TM    2(R4),X'40'         TEST ALPHA                                   
         BZ    MYERR                                                            
         MVC   WORK(4),12(R4)      SAVE CALL LETTERS                            
                                                                                
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)          DEFAULT = TV                                 
         BZ    VS100               YES                                          
         BCTR  RE,0                                                             
         EX    RE,STATV            TV LEAVE BLANK                               
         BE    VS100                                                            
         MVI   WORK+4,C'L'         LP = L                                       
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
         MVI   WORK+40,C'1'                                                     
         B     VS100                                                            
                                                                                
VS50     EX    RE,STA2                                                          
         BNE   MYERR                                                            
         MVI   WORK+40,C'2'                                                     
                                                                                
VS100    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING RSTAKEY,R4                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,AGENCY     REP                                          
         MVC   RSTAKSTA,WORK       STATION                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   MYERR                                                            
                                                                                
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,1                                                         
         BAS   RE,GETEL                                                         
         USING RSTAELEM,R6                                                      
         MVC   WORK+10(L'RSTAMKT),RSTAMKT                                       
         MVC   WORK+41(L'RSTAGRUP),RSTAGRUP                                     
         DROP  R4,R6                                                            
*                                                                               
VSEXT    DS    0H                                                               
         MVC   AIO,SVAIO                                                        
         B     XIT                                                              
*                                                                               
STATV    CLC   22(0,R4),=C'TV'                                                  
STAAM    CLC   22(0,R4),=C'AM'                                                  
STAFM    CLC   22(0,R4),=C'FM'                                                  
STACM    CLC   22(0,R4),=C'CM'                                                  
STALP    CLC   22(0,R4),=C'L'                                                   
STA1     CLC   22(0,R4),=C'1'                                                   
STA2     CLC   22(0,R4),=C'2'                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERIOD                                                               
* INPUT - R2 POINTS TO FIELD HEADER                                             
* OUTPUT - WORK(3) HAS START DATE YYMM99 9'S COMPLEMENT                         
*          WORK+3(3) HAS END DATE YYMM99 9'S COMPLEMENT                         
***********************************************************************         
VPER     DS    0H                                                               
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),BLOCK,C',=,-'                                  
         CLI   DMCB+4,0                                                         
         BE    VPERNO              ERROR ENCOUNTERED                            
*                                                                               
* VALIDATE START DATE                                                           
*                                                                               
         LA    R4,BLOCK                                                         
         GOTO1 DATVAL,DMCB,(2,12(R4)),WORK+10                                   
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    VPERNO                                                           
         GOTO1 DATCON,DMCB,WORK+10,(1,WORK)      START DATE                     
*                                                                               
* VALIDATE END DATE                                                             
* END DATE IS SECOND IN SCANNER BLOCK                                           
*                                                                               
         MVC   WORK+3(3),WORK                                                   
         CLI   1(R4),0                                                          
         BE    VPER10                                                           
                                                                                
         GOTO1 DATVAL,DMCB,(2,22(R4)),WORK+16                                   
         OC    DMCB(4),DMCB        ERROR?                                       
         BZ    VPERNO                                                           
         GOTO1 DATCON,DMCB,WORK+16,(1,WORK+3)     END DATE                      
                                                                                
VPER10   DS    0H                                                               
         ZAP   WORK+18(4),=P'0'                                                 
         MVO   WORK+18(4),WORK+3(3)  CHANGE TO PACK WITH SIGN                   
         ZAP   WORK+14(4),=P'999999'                                            
         SP    WORK+14(4),WORK+18(4) GET 9'S COMPLEMENT                         
         MVO   WORK+10(4),WORK+14(4)   CHANGE TO PWOS                           
         MVC   WORK+3(3),WORK+10                                                
*                                                                               
         ZAP   WORK+18(4),=P'0'                                                 
         MVO   WORK+18(4),WORK(3)   CHANGE TO PACK WITH SIGN                    
         ZAP   WORK+14(4),=P'999999'                                            
         SP    WORK+14(4),WORK+18(4) GET 9'S COMPLEMENT                         
         MVO   WORK+10(4),WORK+14(4)   CHANGE TO PWOS                           
         MVC   WORK(3),WORK+10                                                  
         B     VPERYES                                                          
*                                                                               
VPERNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
VPERYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
VPEREXIT B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY SHARE GOAL AND DESCRIPTION FROM SITUATION ANALYSIS RECORD             
* WITH THE SAME STATION AND PERIOD                                              
* P1 = ADDRESS OF SHARE GOAL FIELD HEADER                                       
* P2 = ADDRESS OF LAST UPDATED FIELD HEADER                                     
* P3 = ADDRESS OF DESCRIPTION FIELD HEADER                                      
* USES IO3                                                                      
***********************************************************************         
DSGDC    NTR1                                                                   
         L     R2,0(R1)            A(SHARE GOAL)                                
         L     R3,4(R1)            A(LAST UPDATED)                              
         L     R4,8(R1)            A(DESCRIPTION)                               
                                                                                
         LA    R6,KEY                                                           
         USING RSTRKEY,R6                                                       
         MVI   RSTRKSUB,RSTRSITQ                                                
         XC    RSTRKPG,RSTRKPG                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE     SITUATION ANALYSIS MISSING?           
         BNE   DSGDCNO                                                          
                                                                                
         MVC   SVAIO,AIO                                                        
         MVC   SVELCODE,ELCODE                                                  
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         USING RSTRDESD,R6                                                      
         MVI   ELCODE,RSTRDCDQ     DESCRIPTIVE ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE DESCRIPTIVE ELEMENT                
                                                                                
         EDIT  RSTRDSHR,(3,8(R2)),ALIGN=LEFT                                    
         OI    6(R2),X'80'         XMIT                                         
                                                                                
         GOTO1 DATCON,DMCB,(2,RSTRDUPT),(5,8(R3))                               
         OI    6(R3),X'80'         XMIT                                         
                                                                                
         CLI   RSTRDELN,RSTRDOV    ANY DESCRIPTION?                             
         BNH   DSGDC10                                                          
                                                                                
         ZIC   R1,RSTRDELN         DESCRIPTION HAS VARIABLE LENGTH              
         LA    RF,RSTRDOV          OVERHEAD LENGTH                              
         SR    R1,RF               DESC LEN = TOTAL LEN - OVERHEAD LEN          
                                                                                
         STC   R1,5(R4)            INSERT LENGTH                                
                                                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R4),RSTRDESC                                                 
         OI    6(R4),X'80'         XMIT                                         
         DROP  R6                                                               
                                                                                
DSGDC10  DS    0H                  RESTORE AIO AND ELCODE                       
         MVC   AIO,SVAIO                                                        
         MVC   ELCODE,SVELCODE                                                  
         B     DSGDCYES                                                         
                                                                                
DSGDCNO  LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
DSGDCYES SR    R1,R1                                                            
         LTR   R1,R1                                                            
         XMOD1 2                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE STANDARD COMMENT                                                     
* R2 POINTS TO LINE WITH STANDARD COMMENT                                       
***********************************************************************         
VCMT     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCMTKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,AGENCY                                                  
         MVC   RCMTKOFF,=X'FFFF'                                                
         OC    10(L'RCMTKCDE,R2),SPACES BLANK PAD                               
         MVC   RCMTKCDE,10(R2)                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BNE   VCMTNO              RECORD NOT FOUND                             
*                                                                               
         XC    18(64,R2),18(R2)    CLEAR REST                                   
         MVI   5(R2),10            SET LENGTH                                   
         B     VCMTYES                                                          
*                                                                               
VCMTNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
VCMTYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
VCMTEXIT B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DISPLAY STANDARD COMMENT                                                      
* R2 POINTS TO FIELD WITH COMMENT                                               
* USES IO3                                                                      
***********************************************************************         
DCMT     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RCMTKEY,R6                                                       
         XC    KEY,KEY                                                          
         MVI   RCMTKTYP,X'2E'                                                   
         MVC   RCMTKREP,AGENCY                                                  
         MVC   RCMTKOFF,=X'FFFF'                                                
         OC    10(L'RCMTKCDE,R2),SPACES BLANK PAD                               
         MVC   RCMTKCDE,10(R2)                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCMTKEY),KEYSAVE                                           
         BE    DISCMT10            RECORD NOT FOUND                             
*                                                                               
         MVC   20(28,R2),=C'* COMMENT RECORD NOT FOUND *'                       
         B     DISCMTX                                                          
*                                                                               
DISCMT10 DS    0H                                                               
         MVC   SVAIO,AIO                                                        
         MVC   SVELCODE,ELCODE                                                  
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING RCMTELM2,R6                                                      
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BE    DISCMT20                                                         
         MVC   20(28,R2),=C'* COMMENT RECORD NOT FOUND *'                       
         B     DISCMTX                                                          
*                                                                               
DISCMT20 DS    0H                                                               
         CLI   RCMT2LEN,3          DISPLAY FIRST NON-BLANK LINE                 
         BH    DISCMT25                                                         
         BAS   RE,NEXTEL                                                        
         BE    DISCMT20                                                         
         B     DISCMTX                                                          
*                                                                               
DISCMT25 DS    0H                                                               
         ZIC   R1,RCMT2LEN         TOTAL LEN                                    
         SH    R1,=H'2'            SUB 2 FOR CODE AND LEN                       
         CH    R1,=H'48'                                                        
         BNH   DISCMT30                                                         
         LA    R1,48               MAX 48 CHARACTERS TO DISPLAY                 
*                                                                               
DISCMT30 DS    0H                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   20(0,R2),RCMT2TXT                                                
*                                                                               
DISCMTX  DS    0H                                                               
         MVC   AIO,SVAIO                                                        
         MVC   ELCODE,SVELCODE                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PACK - R2 HAS ADDRESS OF HEADER                                               
***********************************************************************         
PACK     DS    0H                                                               
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BZ    PACKX               LENGTH ERROR                                 
         TM    4(R2),X'08'                                                      
         BZ    PACKX               NON-NUMERIC                                  
         BCTR  R1,0                                                             
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
PACKX    XIT1  REGS=(R0)                                                        
                                                                                
VARPACK  PACK  DUB,8(0,R2)                                                      
         EJECT                                                                  
***********************************************************************         
* UPDATE THE LAST CHANGE DATE IN SITUATION ANALYSIS RECORD                      
* USES IO3                                                                      
***********************************************************************         
UPDT     DS    0H                                                               
         LA    R6,KEY                                                           
         USING RSTRREC,R6                                                       
         MVI   RSTRKSUB,RSTRSITQ   UPDATE LAST UPDATE DATE IN SIT ANA           
         XC    RSTRKPG,RSTRKPG                                                  
         DROP  R6                                                               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'RSTRKEY),KEYSAVE                                           
         BNE   UPDTNO              SIT ANA RECORD NOT FOUND                     
                                                                                
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
                                                                                
         L     R6,AIO                                                           
         MVC   SVELCODE,ELCODE                                                  
         MVI   ELCODE,RSTRDCDQ     NEED DESCRIPTION ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   UPDTNO                                                           
                                                                                
         USING RSTRDESD,R6                                                      
         GOTO1 DATCON,DMCB,(5,0),(2,RSTRDUPT)  LAST UPDATE DATE                 
         DROP  R6                                                               
                                                                                
         GOTO1 PUTREC              UPDATE MASTER                                
         B     UPDTYES                                                          
                                                                                
UPDTNO   LA    R1,1                SET CONDITION CODES                          
         B     *+6                                                              
UPDTYES  SR    R1,R1                                                            
         LTR   R1,R1                                                            
         MVC   AIO,SVAIO           RESTORE                                      
         MVC   ELCODE,SVELCODE                                                  
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
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
*******************************************************************             
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
*******************************************************************             
* THIS ROUTINE RESTORES THE TWA AND OVERLAY NUMBER TO THAT WHICH IS             
* ON THE TOP OF THE OVERLAY STACK.  IT THEN SETS THE 'GOAGAIN' FLAG             
* TO 'YES' AND TAKES AN ERROR EXIT BACK TO GENCON.  WHEN THE CONTROLLER         
* GETS CONTROL BACK IT WILL CALL GENCON AGAIN WITH THE RESTORED SCREEN.         
* (RA POINTS TO TWA)                                                            
*******************************************************************             
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
*******************************************************************             
* ROUTINE TO INITIALIZE THE SYSSPARE AREA                                       
*******************************************************************             
VINIT    DS    0H                                                               
RECCHG   EQU   X'10'                                                            
SCRCHG   EQU   X'20'                                                            
                                                                                
         CLI   ACTNUM,ACTLIST      FOR ACTION LIST                              
         BNE   VINIT05                                                          
         CLI   TWAACCS,C'$'        IF STATION SIGN-ON                           
         BNE   VINIT10              CHECK IF ONLY SEL IS SPECIFIED              
         BAS   RE,CHCKSEL                                                       
         B     VINIT10                                                          
                                                                                
VINIT05  DS    0H                  IF NOT LIST                                  
         CLI   ACTNUM,ACTSEL       AND NOT SELECT                               
         BE    VINIT10                                                          
         CLI   TWAACCS,C'$'        AND IF STATION SIGN-ON                       
         BNE   VINIT10                                                          
         BAS   RE,CHCKDIS          CHECK IF ONLY DIS IS SPECIFIED               
                                                                                
VINIT10  DS    0H                                                               
         MVI   SCRSTAT,0                                                        
                                                                                
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
                                                                                
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
                                                                                
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    VINIT20                                                          
*        TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
*        BZ    VINITX                                                           
         B     VINITX                                                           
                                                                                
VINIT20  DS    0H                                                               
         LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'SYSSPARE)                                              
         XCEFL                                                                  
                                                                                
VINITX   DS    0H                                                               
         MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM        SAVE CURRENT RECORD                          
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* FOR STATION SIGN-ON, USER CAN ONLY SELECT LIST RECORDS                        
*******************************************************************             
CHCKSEL  NTR1                                                                   
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
CSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    CSEL6                                                            
         OC    8(3,R2),SPACES                                                   
         ZIC   R1,5(R2)            R1=L(SELECT INPUT)                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'SEL'     MATCH ON EXACT SELECT CODE                   
         BE    CSEL6                                                            
                                                                                
         XC    CONHEAD,CONHEAD     NEED TO DO MY OWN ERR MSG                    
         MVC   CONHEAD(L'SELONLY),SELONLY                                       
         OI    CONHEADH+6,X'80'    TRANSMIT HEADER                              
         OI    6(R2),X'40'         PLACE CURSOR AT ACTION COLUMN                
         L     RD,SAVERD           BACK OUT ALL THE WAY                         
         B     CSELX                                                            
                                                                                
SELONLY  DC    C'ONLY SELECT PERMITTED FOR A STATION SIGN-ON'                   
                                                                                
*                                                                               
CSEL6    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    CSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   CSEL2               SELECT FIELD                                 
         B     CSEL6                                                            
                                                                                
CSELX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* FOR STATION SIGN-ON, USER CAN ONLY DISPLAY RECORDS                            
*******************************************************************             
CHCKDIS  NTR1                                                                   
         LA    R2,CONACTH                                                       
         CLC   =C'DIS',8(R2)       MATCH ON DISPLAY CODE                        
         BE    CDISX                                                            
                                                                                
         XC    CONHEAD,CONHEAD     NEED TO DO MY OWN ERR MSG                    
         MVC   CONHEAD(L'DISONLY),DISONLY                                       
         OI    CONHEADH+6,X'80'    TRANSMIT HEADER                              
         OI    6(R2),X'40'         PLACE CURSOR AT ACTION                       
         L     RD,SAVERD           BACK OUT ALL THE WAY                         
         B     CDISX                                                            
                                                                                
DISONLY  DC    C'ONLY DISPLAY PERMITTED FOR A STATION SIGN-ON'                  
                                                                                
CDISX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
*******************************************************************             
* THIS ROUTINE INTERFACES TO GENCON'S ERREX ROUTINE AND SETS SPECIAL            
* PARAMETERS TO MAKE IT CALL GETTXT INSTEAD OF GETMSG.  SINCE RESTR             
* USES MESSAGE NUMBERS GREATER THAN 255, GETTXT MUST BE USED BECAUSE            
* GETMSG IS NOT DESIGNED TO HANDLE THESE NUMBERS.                               
*******************************************************************             
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
         GOTO1 ERREX                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XIT      XIT1                                                                   
         GETEL (R6),DATADISP,ELCODE                                             
*              CONSTANTS TABLES ETC                                             
RELO     DS    A                                                                
SAVERD   DS    A                                                                
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
         DC    AL1(QQSORT)         QSORT                                        
*                                                                               
CORES    EQU   (*-CORETAB)                                                      
                                                                                
REPFLIST DC    CL8'NREPFILE'                                                    
         DC    CL8'NREPDIR'                                                     
         DC    CL8'NCTFILE'                                                     
         DC    C'X'                                                             
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
         DC    X'01',C'SITANA  ',AL1(01),X'0000'                                
         DC    X'01',C'SAT     ',AL1(02),X'0000'                                
         DC    X'01',C'GKC     ',AL1(03),X'0000'                                
         DC    X'01',C'SAC     ',AL1(04),X'0000'                                
         DC    X'01',C'ACCOUNTS',AL1(05),X'0000'                                
                                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
                                                                                
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
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
         SPACE 1                                                                
         DC    X'03',AL1(01,01),X'F101000080',C'    '  SITANA    MAINT          
         DC    X'03',AL1(01,10),X'F201000080',C'    '  SITANA    LIST           
         DC    X'03',AL1(01,12),X'FB06000078',C'SYSY'  STRATEGY  REPORT         
         DC    X'03',AL1(02,01),X'F302000080',C'    '  SAT       MAINT          
         DC    X'03',AL1(02,10),X'F402000080',C'    '  SAT       LIST           
         DC    X'03',AL1(03,01),X'F503000080',C'    '  GKC       MAINT          
         DC    X'03',AL1(03,10),X'F603000080',C'    '  GKC       LIST           
         DC    X'03',AL1(04,01),X'F704000080',C'    '  SAC       MAINT          
         DC    X'03',AL1(04,10),X'F804000080',C'    '  SAC       LIST           
         DC    X'03',AL1(05,01),X'F905000080',C'    '  ACCOUNTS  MAINT          
         DC    X'03',AL1(05,10),X'FA05000080',C'    '  ACCOUNTS  LIST           
         DC    X'FF'                                                            
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
* RESTRFFD *                                                                    
************                                                                    
         SPACE 1                                                                
       ++INCLUDE RESTRFFD                                                       
         SPACE 1                                                                
**********************************************                                  
* DDGENTWA - DSECT TO COVER GENCON TWA AREAS *                                  
**********************************************                                  
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
**************                                                                  
* RESTRWTWA  *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE RESTRTWA                                                       
         EJECT                                                                  
**************                                                                  
* RESTRWORKD *                                                                  
**************                                                                  
         SPACE 1                                                                
       ++INCLUDE RESTRWORKD                                                     
         EJECT                                                                  
****************************                                                    
* REGENALLA                *                                                    
* DEDEMFILE                *                                                    
* CTGENFILE                *                                                    
* DDCOMFACS                *                                                    
* FAFACTS                  *                                                    
* FATIOB                   *                                                    
* DDREPMASTD               *                                                    
* DDCOREQUS                *                                                    
* DEDBLOCK (DBLOCKD DSECT) *                                                    
****************************                                                    
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE REGENALLA                                                      
       ++INCLUDE REGENCMT                                                       
       ++INCLUDE REGENSTR                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021RESTR00S  05/01/02'                                      
         END                                                                    
