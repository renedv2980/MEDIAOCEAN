*          DATA SET RENRG00A   AT LEVEL 064 AS OF 05/01/02                      
*          DATA SET RERRG00    AT LEVEL 030 AS OF 02/20/96                      
*PHASE T81500A,+0                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T81500 - REP NEW RRGON CONTROLLER'                              
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
***********************************************************************         
*                                                                     *         
* MAY01/96 (BG ) --- NEW RRGON                                        *         
*                                                                     *         
* JUN11/96 (BG ) -21 FIX ERRMSG TO TYPE E                             *         
*                                                                     *         
* AUG02/96 (BG ) -24 CK GROUP TO GROUP RECS, ALSO CK VALID REQ AFTER  *         
*                                                                     *         
* OCT30/96 (BG ) -25 ADD SET VALIDATION                               *         
*                                                                     *         
* DEC03/96 (BG ) -26 COPY RHV SWAPPING TO BROWSE                      *         
*                                                                     *         
* DEC13/96 (BG ) -27 FIX F *T RTN, ADD MORE SET VAL                   *         
*                                                                     *         
* DEC23/96 (BG ) -28 CK GRP/SUB IN VSTA IF ENTERED                    *         
*                                                                     *         
* JAN16/97 (BG ) -29 FIX AGY SET BLKS TO NULLS                        *         
*                                                                     *         
* JAN22/97 (BG ) -30 READ REP REC FOR SETS PROFILE/MASTER REP CODE    *         
*                                                                     *         
* FEB01/97 (BG ) -31 ADD OFFLINE FILE OPEN                            *         
*                                                                     *         
* FEB07/97 (BG ) -32 ADD SPECIAL CONTYPE/DEVCONT HARD CODE            *         
*                                                                     *         
* FEB24/97 (BG ) -33 ADD AQ, ENABLE BROWSE                            *         
*                                                                     *         
* MAR05/97 (BG ) -34 SET SAVED STORAGE TO 6144                        *         
*                                                                     *         
* MAR27/97 (BG ) -35 EDIT AGENCY FOR DASH (-)                         *         
*                                                                     *         
* APR06/97 (BG ) -36 ADD TEST FOR REP FN, GROUP F                     *         
*                                                                     *         
* MAY27/97 (BG ) -37 ADD SET OF SETS                                  *         
*                                                                     *         
* JUL16/97 (BG ) -38 CHANGE BLKS IN ADV/AGY TO NULLS, SV MKT CD-STA   *         
*                                                                     *         
* AUG13/97 (BG ) -40 CHANGE SCREENS/NMOD FROM 2500 TO 3000 SET TAB    *         
*                                                                     *         
* SEP05/97 (RHV)  43 SUPPORT BROWSE INTERFACE MODULE (REBROWSE)       *         
*           BG                                                        *         
* SEP22/97 (BG )  45 ADD TRUE OFFLINE TEST TO BYPASS BROWSE           *         
*                                                                     *         
* NOV14/97 (BG )  46 ADD NPTVNY AS NRGON USER                         *         
*                                                                     *         
* NOV18/97 (BG )  47 CHANGE FOR GROUP NAMES                           *         
*                                                                     *         
* MAR23/98 (BG )  48 ADD LOW POWERED TV -L                            *         
*                                                                     *         
* JUN04/98 (BG )  49 ADD SALESPERSON                                  *         
*                    DELETE LIMIT USER SIGNON                         *         
* MAY07/99 (BG )  50 MOVE SALESPERSON TO OPTIONS FIELD                *         
*                                                                     *         
* MAY26/99 (BG )  51 ADD GROUP N FOR NATIONAL PUBLIC RADIO            *         
*                                                                     *         
* JUN16/99 (RHV)  ADD REPFACS / FIX INITIALIZATION BUGS / BROWSE CALLS*         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
T81500   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 3072,**T81500,R7,RR=R2                                           
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
         LA    RF,2000             CLEAR SELECTED STORAGE                       
         SLL   RF,3                                                             
         XCEF                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=H'6000'         GRABBING 3 2000 BYTE I/O AREAS               
         LA    R9,24(R9)           NEED SPACE FOR 3 8BYTE LABELS                
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         L     RF,SYSPARMS                                                      
         L     RA,4(RF)            A(TWA)                                       
         USING CONHEADH-64,RA                                                   
         MVC   AGENCY,TWAAGY       SET REP CODE                                 
*                                  ESTABLISH V(REPFACS)                         
         L     R2,8(RF)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         GOTO1 (RF),DMCB,0,X'D9000AAC',0                                        
         MVC   VREPFACS,0(R1)      EXTERNAL ROUTINE, MISC. SUBROUTINES          
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   ONLY HAVE ADDRESS OFFLINE                    
         BNZ   INIT010              YES, NO BROWSE                              
*                                                                               
* HANDLE RETURNS FROM BROWSE WITH THIS CALL *                                   
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         GOTO1 (RFBROWSE,VREPFACS),DMCB,(RF),SYSRD,0                            
*                                                                               
INIT010  DS    0H                                                               
         L     RF,SYSPARMS                                                      
         L     R2,8(RF)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A30'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF               GO TO CALLOV                                 
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
         B     EXIT                THEN WE'RE THROUGH                           
         SPACE                                                                  
RELO     DS    A                                                                
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
***>     LA    R4,VCOUNT                                                        
         LA    R4,NVTYPES                                                       
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
* SET SYSTEM DEPENDENT VALUES *                                                 
         SPACE 1                                                                
         MVI   SYSTEM,C'R'         REP (HOPEFULLY)                              
         MVI   MAXIOS,3            USES 3 I/O AREAS                             
         MVC   SIZEIO,=F'2000'     EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET REP NAME AND ADDRESS          
*                                                                               
         MVC   LKEY,=H'48'         RRGON VALUES                                 
         MVC   AIO,AIO1                                                         
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVC   LSTATUS,=H'1'       REP VALUES                                   
         MVC   DATADISP,=H'34'                                                  
         MVC   SYSFIL,=C'REPFIL  '                                              
         MVC   SYSDIR,=C'REPDIR  '                                              
         MVC   FILENAME,=C'RRGNEW  '                                            
*                                                                               
         MVI   GETMSYS,8           GETMSG SYSTEM                                
         MVC   LWORK,=F'24576'     WE TOOK 24576 BYTES IN NMOD - 6K             
         MVC   RCPROG(2),=C'RG'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,PHASENM    PRESET FOR SYSTEM CALLOVS                    
         MVI   NTWA,1              1  SAVED TWA PAGES                           
         OI    NTWA,X'80'          RESTORE 6 K AREA                             
*        OI    NTWA,X'40'          RESTORE 14 K AREA                            
         OI    GENSTAT3,RESTXE00   RESTORE X'0E00' AFTER SELECT                 
         LA    R0,SVSTART          SET SAVED STORAGE START ADDR                 
         ST    R0,ASTARTSV                                                      
*        LH    R0,=AL2(SETABND-SYSD) SET SAVED STORAGE END ADDR                 
*        AR    R0,R9                                                            
*        ST    R0,AENDSV                                                        
         SPACE                                                                  
         MVC   LSVTWA0,=H'6144'    SET SAVED STORAGE LENGTH                     
*        NEW ADDITIONAL   4576                                                  
*        MVC   LSVTWA0,=H'10720'   SET SAVED STORAGE LENGTH                     
*        MVC   LSVTWA0,=AL2(MAXLTWA0)                                           
         SPACE                                                                  
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         XC    GBLOCK,GBLOCK       CLEAR GETTXT ERROR AREA                      
         SPACE                                                                  
         XC    SETCDES,SETCDES     ALL SET CODES                                
         LR    R0,R9                                                            
         AH    R0,=AL2(SET1TAB-SYSD)                                            
         LA    R1,(L'SET1TAB*3)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXIT                                                             
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
         SPACE                                                                  
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         SRL   RF,24                                                            
         CH    RF,=AL2(VCOUNT*4)                                                
         BNH   VBRANCH(RF)                                                      
         DC    H'0'                                                             
         SPACE                                                                  
VBRANCH  B     VUSER                                                            
         B     VSTAT                                                            
         B     VOFF                                                             
         B     VPY                                                              
         B     VPER                                                             
         B     VGRP                                                             
         B     VRGN                                                             
         B     VTEM                                                             
         B     VSTYP                                                            
         B     VTVB                                                             
         B     VOWN                                                             
         B     VCLASS                                                           
         B     VCTGY                                                            
         B     VCONT                                                            
         B     VRANK                                                            
         B     VMKT                                                             
         B     VAFF                                                             
         B     VADV                                                             
         B     VAGY                                                             
         B     VDCT                                                             
         B     VRRGERR                                                          
         B     VSAL                                                             
         SPACE 1                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         LTORG                                                                  
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO READ REP RECORD FOR REP NAME AND ADDRESS                    
*                                                                               
         SPACE                                                                  
VUSER    L     R3,ATWA                                                          
         SPACE                                                                  
         CLI   29(R3),0            TEST FIRST TIME                              
         BE    VUSER20             YES - READ DATA                              
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    VUSER20             YES - ALWAYS READ                            
         MVC   USERNAME(66),SVUSER   ELSE MOVE SAVED NAME                       
*                                                                               
VUSER20  MVC   FILENMSV,FILENAME                                                
         XC    FILENAME,FILENAME                                                
         MVC   USEIOSV,USEIO                                                    
         MVI   USEIO,C'N'                                                       
         MVC   SVAIO,AIO                                                        
         MVC   AIO,AIO2                                                         
         MVC   LKEYSV,LKEY                                                      
         MVC   LKEY,=H'27'                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,1                                                            
         MVC   KEY+25(2),AGENCY                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(27),KEY     FOUND?                                       
         BE    *+6                                                              
         DC    H'0'                REP NOT FOUND??!!                            
         GOTO1 GETREC              RETRIEVE THE RECORD                          
         L     R6,AIO                                                           
         USING RREPD,R6                                                         
         MVC   USERNAME,RREPNAME                                                
         MVC   USERADDR,RREPADDR                                                
         DROP  R6                                                               
         MVC   SVUSER(66),USERNAME                                              
         MVC   FILENAME,FILENMSV                                                
         BAS   RE,SETPROFS         ESTABLISH PROFILE                            
*                                                                               
         MVI   CMBOREP,C'N'        SET MASTER OR SUB TO 'NO'                    
         XC    CMBOMSTR,CMBOMSTR                                                
         SPACE                                                                  
* CHECK REP REC FOR MASTER STATUS/SET PROFILE *                                 
         SPACE                                                                  
         L     R6,AIO                                                           
         LA    R6,34(,R6)                                                       
         CLI   0(R6),01            IS THIS A GENUINE REP REC                    
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RREPELEM,R6                                                      
         CLC   RREPMAST,SPACES                                                  
         BE    VUSER50                                                          
         OC    RREPMAST,RREPMAST                                                
         BZ    VUSER50                                                          
         CLI   RREPMAST,X'FF'      THIS IS MASTER                               
         BE    VUSER50                                                          
         SPACE                                                                  
         MVI   CMBOREP,C'Y'        SET ON IS MASTER                             
         MVC   CMBOMSTR,RREPMAST                                                
         DROP  R6                                                               
VUSER50  EQU   *                                                                
         MVC   USEIO,USEIOSV                                                    
         MVC   AIO,SVAIO                                                        
         MVC   LKEY,LKEYSV                                                      
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BNE   EXIT                 NO - NO FILE OPEN                           
         SPACE                                                                  
         L     R2,AIO1                                                          
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'REP',RRGLST,(R2)                      
         B     EXIT                                                             
         DS    0D                                                               
RRGLST   DC    CL8'NRRGNEW'                                                     
         DC    C'X'                                                             
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*        SETPROFS ---  SET RRG/REP PROFILES FROM THE REP REC                    
*                                                                               
SETPROFS NTR1                                                                   
         LR    R2,RA               USE R2 TO COVER THE ENTRY                    
         AH    R2,=AL2(RRGPROFS-CONHEADH)                                       
         USING SVDSECT,R2                                                       
         XC    SVPGENTY,SVPGENTY                                                
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   SETP010                                                          
         MVC   SVPGREP(2),AGENCY                                                
         MVI   SVPGP#,RREPQRRG                                                  
*****>>  MVC   SVPGPBIT,=8X'FF'    NOT FORCING ALL BITS ON!!                    
*                                                                               
SETP010  EQU   *                                                                
         MVC   SVPGREP(2),AGENCY                                                
         MVI   SVPGP#,RREPQRRG                                                  
*                                                                               
         L     RE,AIO              RECORD IS HERE                               
         USING RREPD,RE                                                         
         ZICM  RF,RREPLEN,2                                                     
         DROP  RE                                                               
         AR    RF,RE                                                            
         MVI   0(RF),0             FORCE 0 AT END OF RECORD                     
*                                                                               
         LA    RE,34(RE)           A(1ST ELEMENT)                               
SETP020  EQU   *                                                                
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BE    SETP100                                                          
*                                                                               
         CLI   0(RE),X'04'         PROGRAM PROFILE ELEMENT?                     
         BE    SETP040                                                          
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     SETP020                                                          
*                                                                               
*- FIND RRG PROGRAM UNIT WITHIN PROGRAM PROFILE ELEMENT                         
SETP040  EQU   *                                                                
         USING RREPPGMP,RE                                                      
         ZIC   R0,RREPPGM#         NUMBER OF PROGRAM UNITS                      
         LA    RE,RREPPGM1         A(1ST UNIT)                                  
         DROP  RE                                                               
*                                                                               
SETP050  CLI   0(RE),RREPQRRG      LOOKING FOR RRG                              
         BE    SETP060                                                          
         LA    RE,RREPPGML(RE)     NEXT UNIT                                    
         BCT   R0,SETP050                                                       
         B     SETP100             NO MATCH. USE DEFAULTS                       
*                                                                               
SETP060  MVC   SVPGPBIT(8),2(RE)   SAVE UNIT IN TWA.                            
         SPACE                                                                  
SETP100  EQU   *                                                                
*                                                                               
SETPGOOD EQU   *                                                                
         SR    R0,R0                                                            
         B     EXIT                                                             
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE STATION CALL LETTERS                               
*        - ON EXIT QSTA IS SET                                                  
*                                                                               
VSTAT    CLI   8(R2),C'*'          THIS A SET                                   
         BNE   VSTA10                                                           
         SPACE                                                                  
         MVC   FULL(2),=C'ST'                                                   
         MVC   FULL+2(2),=AL1(QLSTA,5)                                          
         BAS   RE,VSET             GO VALIDATE SET                              
         MVI   QSTA,C'*'                                                        
         MVI   EXMKTNAM,C'*'                                                    
         MVC   EXMKTNAM+1(L'EXMKTNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VSTA10   XC    BLOCK(64),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
         LA    R4,BLOCK                                                         
         CLI   0(R4),3                                                          
         BL    STAERR                                                           
         CLI   0(R4),6                                                          
         BH    STAERR                                                           
         TM    2(R4),X'40'         TEST ALPHA                                   
         BZ    STAERR                                                           
         LA    R5,3                                                             
         CLI   0(R4),3                                                          
         BNE   *+6                                                              
         BCTR  R5,0                                                             
         EX    R5,STAMOVE          SAVE CALL LETTERS                            
         LA    R5,QSTA+1(R5)                                                    
         MVI   0(R5),C'-'                                                       
         CLI   0(R4),4                                                          
         BNH   VSTA14                                                           
         ZIC   RF,0(R4)                                                         
         SH    RF,=H'5'                                                         
         LA    R1,16(R4)                                                        
         BAS   RE,VALMED                                                        
         CLI   1(R4),0                                                          
         BNE   STAERR                                                           
         B     VSTA20                                                           
*                                                                               
VSTA14   CLI   1(R4),0                                                          
         BNE   *+12                                                             
         LA    R6,=C'TV'                                                        
         B     VSTA20                                                           
         CLI   1(R4),2                                                          
         BH    STAERR                                                           
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LA    R1,22(R4)                                                        
         BAS   RE,VALMED                                                        
*                                                                               
VSTA20   MVC   1(2,R5),0(R6)                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(4),QSTA                                                   
         CLI   KEY+25,C'-'                                                      
         BNE   *+8                                                              
         MVI   KEY+25,C' '                                                      
         MVC   KEY+26(1),0(R6)                                                  
         CLI   0(R6),C'T'                                                       
         BNE   *+8                                                              
         MVI   KEY+26,C' '                                                      
         MVC   SVSTAKEY,KEY                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   STAERR                                                           
         OC    QGROUP,QGROUP       WAS GROUP ENTERED                            
         BNZ   VSTA30                                                           
         CLI   QLIST,QLSTA         STATION LIST                                 
         BE    EXIT                 YES - DON'T GET EXPANSION                   
         SPACE                                                                  
VSTA30   DS    0H                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RSTARECD,R4                                                      
*                                                                               
         LA    R6,RSTAREC          CHECK IF WE'VE GOT A MASTER REC              
         MVI   ELCODE,X'51'        CURRENT REP?                                 
         BAS   RE,GETEL                                                         
         BE    VSTA35                                                           
         LA    R6,RSTAREC                                                       
         MVI   ELCODE,X'52'        PREVIOUS REP?                                
         BAS   RE,GETEL                                                         
         BNE   VSTA40                                                           
VSTA35   DS    0H                                                               
         CLI   KEY+26,C'C'         COMBO STATION                                
         BNE   VSTA38              NOT ALLOWED FOR MASTER                       
         MVC   GERROR,=Y(INVMREP)                                               
         GOTO1 RRGERR                                                           
*                                                                               
VSTA38   DS    0H                                                               
         MVC   KEY+20(2),2(R6)     SUBSID REPCODE                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
*                                                                               
VSTA40   DS    0H                                                               
         CLI   RSTAELEM,X'01'      ACTUAL STATION RECORD?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    QGROUP,QGROUP       WAS GROUP ENTERED                            
         BZ    VSTA70                                                           
*                                                                               
         CLI   QGROUP,C'*'         THIS A SET                                   
         BE    VSTA50                                                           
         CLC   QGROUP(1),RSTAGRUP                                               
         BNE   GRPERR                                                           
         CLI   QGROUP+1,C' '                                                    
         BNH   VSTA70                                                           
*                                                                               
         CLC   QGROUP+1(1),RSTAGRUP+1                                           
         BNE   GRPERR                                                           
         B     VSTA70                                                           
*                                                                               
VSTA50   LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AH    RF,=AL2(SET1TAB-SYSD)                                            
VSTA52   CLI   0(RE),QLGRP       THIS THE CODE                                  
         BE    VSTA54                                                           
         LA    RE,L'SETCDE(,RE)    NEXT CODE                                    
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,VSTA52                                                        
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
         SPACE                                                                  
VSTA54   ZIC   R1,1(RE)            GET THE LENGTH                               
         SPACE                                                                  
VSTA56   CLC   RSTAGRUP,0(RF)      CHECK THE STATION GROUP                      
         BE    VSTA58                                                           
         LA    RF,0(R1,RF)                                                      
         OC    0(2,RF),0(RF)       ANOTHER ENTRY                                
         BNZ   VSTA56                                                           
         B     GRPERR                                                           
         SPACE                                                                  
VSTA58   TM    SET1FLG-SET1CDE(RE),X'08'  EXCLUDE SET                           
         BO    GRPERR               YES, ERROR                                  
         SPACE                                                                  
* IF NOT EXCLUDE, OKAY, NOW GET MARKET NAME                                     
         SPACE                                                                  
VSTA70   DS   0H                                                                
         XC    STAMKT,STAMKT                                                    
*                                                                               
         MVC   EXMKTNAM,RSTAMKT                                                 
         LA    R6,RSTAELEM                                                      
         MVI   ELCODE,X'08'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   EXIT                                                             
         MVC   STAMKT,RSTAMKTC-RSTAXXEL(R6)                                     
         OC    STAMKT,STAMKT                                                    
         BZ    EXIT                                                             
         DROP  R4                                                               
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'2B'                                                        
         MVC   KEY+21(2),AGENCY                                                 
         MVC   KEY+23(4),STAMKT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RMKTRECD,R4                                                      
         MVC   EXMKTNAM,RMKTNAME                                                
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
STAERR   MVI   ERROR,INVSTAT                                                    
         B     TRAPERR                                                          
*                                                                               
STAMOVE  MVC   QSTA(0),12(R4)      ** EXECUTED                                  
         SPACE 2                                                                
VALMED   LR    R0,RE                                                            
         LA    R6,MEDTAB                                                        
VALMED2  CLI   0(R6),0                                                          
         BE    STAERR                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),0(R6)                                                    
         BE    *+12                                                             
         LA    R6,2(R6)                                                         
         B     VALMED2                                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
MEDTAB   DC    CL2'TV'                                                          
         DC    CL2'AM'                                                          
         DC    CL2'FM'                                                          
         DC    CL2'CM'                                                          
         DC    XL2'D300'                                                        
         DC    XL1'00'                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE OFFICE                                             
*        - ON EXIT QOFF IS SET                                                  
*                                                                               
VOFF     CLI   8(R2),C'*'          THIS A SET                                   
         BNE   VOFF10                                                           
         SPACE                                                                  
         MVC   FULL(2),=C'OF'                                                   
         MVC   FULL+2(2),=AL1(QLOFF,L'QOFF)                                     
         BAS   RE,VSET             GO VALIDATE SET                              
         MVI   QOFF,C'*'                                                        
         MVI   EXOFFNAM,C'*'                                                    
         MVC   EXOFFNAM+1(L'EXOFFNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VOFF10   CLI   5(R2),2                                                          
         BNE   OFFERR                                                           
         MVC   QOFF,8(R2)                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),QOFF                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   OFFERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING ROFFRECD,R4                                                      
         MVC   EXOFFNAM,ROFFNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
OFFERR   MVI   ERROR,INVOFF                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE PER/YTD                                            
*        - ON EXIT QPY IS SET                                                   
*                                                                               
VPY      CLI   5(R2),0                                                          
         BE    EXIT                                                             
         CLI   5(R2),3                                                          
         BH    PYERR                                                            
         MVC   QPY,8(R2)                                                        
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,PYCOMP1                                                       
         BE    EXIT                                                             
         EX    RE,PYCOMP2                                                       
         BE    EXIT                                                             
*                                                                               
PYERR    MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
PYCOMP1  CLC   8(0,R2),=C'PER'                                                  
PYCOMP2  CLC   8(0,R2),=C'YTD'                                                  
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE PERIOD                                             
*        - ON EXIT QSTART AND QEND ARE SET                                      
*                                                                               
VPER     XC    BLOCK(64),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(1,BLOCK),C',=,-'                              
         CLI   4(R1),0                                                          
         BE    PERERR                                                           
         LA    R4,BLOCK                                                         
         GOTO1 DATVAL,DMCB,(2,12(R4)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    PERERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,DUB)                                     
         MVC   QSTART,DUB                                                       
         CLI   1(R4),0                                                          
         BNE   VPER2                                                            
         MVC   QEND,QSTART                                                      
         B     EXIT                                                             
*                                                                               
VPER2    GOTO1 DATVAL,DMCB,(2,22(R4)),WORK                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    PERERR                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(3,DUB)                                     
         MVC   QEND,DUB                                                         
         CLC   QSTART,QEND                                                      
         BH    PERERR                                                           
         CLC   QSTART(1),QEND      CAN'T OVERLAP YEARS                          
         BNE   PERERR                                                           
         B     EXIT                                                             
*                                                                               
PERERR   MVI   ERROR,INVPER                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE GROUP                                              
*        - ON EXIT QGROUP IS SET                                                
*                                                                               
VGRP     CLI   8(R2),C'*'          THIS A SET                                   
         BNE   VGRP10                                                           
         SPACE                                                                  
         MVC   FULL(2),=C'GS'                                                   
         MVC   FULL+2(2),=AL1(QLGRP,L'QGROUP)                                   
         BAS   RE,VSET             GO VALIDATE SET                              
         MVI   QGROUP,C'*'                                                      
         MVI   EXGRPNAM,C'*'                                                    
         MVC   EXGRPNAM+1(L'EXGRPNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VGRP10   CLI   5(R2),1                                                          
         BNE   VGRP20                                                           
         MVC   QGROUP(1),8(R2)                                                  
         B     VGRP30                                                           
*                                                                               
VGRP20   CLI   5(R2),2                                                          
         BNE   GRPERR                                                           
         MVC   QGROUP,8(R2)                                                     
VGRP30   XC    KEY,KEY                                                          
         MVI   KEY,7                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),QGROUP                                                 
         OI    KEY+26,C' '                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GRPERR                                                           
         SPACE                                                                  
         CLC   AGENCY,=C'FN'                                                    
         BNE   *+16                                                             
         CLI   QGROUP,C'F'                                                      
         BE    VGRP40                                                           
         B     GRPERR                                                           
         SPACE                                                                  
         CLI   QGROUP,C'T'                                                      
         BE    VGRP40                                                           
         CLI   QGROUP,C'R'                                                      
         BE    VGRP40                                                           
         CLI   QGROUP,C'A'         INTNY                                        
         BE    VGRP40                                                           
         CLI   QGROUP,C'I'         INTNY                                        
         BE    VGRP40                                                           
         CLI   QGROUP,C'P'         PETRY                                        
         BE    VGRP40                                                           
         CLI   QGROUP,C'N'         NATIONAL PUBLIC RADIO                        
         BNE   GRPERR                                                           
         SPACE                                                                  
VGRP40   CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RGRPRECD,R4                                                      
         MVC   EXGRPNAM(L'RGRPNAME),RGRPNAME                                    
         MVC   LISTAR+25(10),2(R6)                                              
         LA    R0,10                                                            
         LA    R1,EXGRPNAM                                                      
VGRP50   CLI   0(R1),C' '                                                       
         BE    VGRP60                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,VGRP50                                                        
         SPACE                                                                  
VGRP60   MVC   1(10,R1),RGRPSBNM                                                
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
GRPERR   MVI   ERROR,INVGRP                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE REGION                                             
*        - ON EXIT QREGION IS SET                                               
*                                                                               
VRGN     CLI   5(R2),2                                                          
         BNE   RGNERR                                                           
         MVC   QREGION,8(R2)                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,3                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),QREGION                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   RGNERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RRGNRECD,R4                                                      
         MVC   EXRGNNAM,RREGNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
RGNERR   MVI   ERROR,INVRGN                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE TEAM                                               
*        - ON EXIT QTEAM IS SET                                                 
*                                                                               
VTEM     CLI   5(R2),2                                                          
         BNE   TEMERR                                                           
         MVC   QTEAM,8(R2)                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,5                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),QTEAM                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   TEMERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RTEMRECD,R4                                                      
         MVC   EXTEMNAM,RTEMNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
TEMERR   MVI   ERROR,INVTEM                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE STATION TYPE                                       
*        - ON EXIT QSTATY IS SET                                                
*                                                                               
VSTYP    CLI   5(R2),1                                                          
         BNE   STYPERR                                                          
         MVI   QSTATY,C'1'                                                      
         CLI   8(R2),C'C'                                                       
         BE    EXIT                                                             
         MVI   QSTATY,C'2'                                                      
         CLI   8(R2),C'N'                                                       
         BE    EXIT                                                             
         MVI   QSTATY,C'3'                                                      
         CLI   8(R2),C'O'                                                       
         BE    EXIT                                                             
*                                                                               
STYPERR  MVC   GERROR,=AL2(INVSTYP)                                             
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE TVB REGION CODE                                    
*        - ON EXIT QTVB IS SET                                                  
*                                                                               
VTVB     CLI   5(R2),2                                                          
         BNE   TVBERR                                                           
         LA    R1,TVBLST                                                        
*                                                                               
VTVB2    CLI   0(R1),X'FF'                                                      
         BE    TVBERR                                                           
         CLC   0(2,R1),8(R2)                                                    
         BNE   *+14                                                             
         MVC   QTVB,8(R2)                                                       
         B     EXIT                                                             
         LA    R1,L'TVBLST(R1)                                                  
         B     VTVB2                                                            
*                                                                               
TVBERR   MVC   GERROR,=AL2(INVTVB)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE OWNERSHIP CODE                                     
*        - ON EXIT QOWNER IS SET                                                
*                                                                               
VOWN     CLI   5(R2),3                                                          
         BH    OWNERR                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ROWNRECD,R4                                                      
         MVI   ROWNKTYP,X'2A'                                                   
         MVC   ROWNKREP,AGENCY                                                  
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ROWNKOWN,8(R2)                                                   
         OC    ROWNKOWN,SPACES                                                  
         MVC   QOWNER,ROWNKOWN                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'ROWNKEY),KEYSAVE                                           
         BNE   OWNERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING ROWNRECD,R4                                                      
         MVC   EXOWNNAM,ROWNNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
OWNERR   MVC   GERROR,=AL2(INVOWN)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE CLASS                                              
*        - ON EXIT QCLASS IS SET                                                
*                                                                               
VCLASS   CLI   5(R2),2                                                          
         BH    CLSERR                                                           
         MVC   QCLASS,SPACES                                                    
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   QCLASS(0),8(R2)                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCLSRECD,R4                                                      
         MVI   RCLSKTYP,X'0D'                                                   
         MVC   RCLSKREP,AGENCY                                                  
         MVC   RCLSKCLS,QCLASS                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCLSKEY),KEYSAVE                                           
         BNE   CLSERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RCLSRECD,R4                                                      
         MVC   EXCLSNAM,RCLSNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
CLSERR   MVI   ERROR,INVCLS                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE CATEGORY                                           
*        - ON EXIT QCTGY IS SET                                                 
*                                                                               
VCTGY    CLI   5(R2),2                                                          
         BH    CTGERR                                                           
         MVC   QCTGY,8(R2)                                                      
         OC    QCTGY,SPACES                                                     
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCTGRECD,R4                                                      
         MVI   RCTGKTYP,X'0F'                                                   
         MVC   RCTGKREP,AGENCY                                                  
         MVC   RCTGKCTG,8(R2)                                                   
         OC    RCTGKCTG,SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCTGKEY),KEYSAVE                                           
         BNE   CTGERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RCTGRECD,R4                                                      
         MVC   EXCTGNAM,RCTGNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
CTGERR   MVI   ERROR,INVCTG                                                     
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE CONTRACT TYPE                                      
*        - ON EXIT QCONTY IS SET                                                
*                                                                               
VCONT    CLI   8(R2),C'*'          THIS A SET                                   
         BNE   VCONT20                                                          
         SPACE                                                                  
         MVC   FULL(2),=C'CT'                                                   
         MVC   FULL+2(2),=AL1(QLCON,1)                                          
         BAS   RE,VSET             GO VALIDATE SET                              
         MVI   QCONTY,C'*'                                                      
         MVI   EXCTYNAM,C'*'                                                    
         MVC   EXCTYNAM+1(L'EXCTYNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VCONT20  CLI   5(R2),1                                                          
         BNE   CONTERR                                                          
         MVC   QCONTY(1),8(R2)                                                  
         MVI   QCONTY+1,C' '                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RCTYRECD,R4                                                      
         MVI   RCTYKTYP,RCTYKTYQ                                                
         MVC   RCTYKREP,AGENCY                                                  
         MVC   RCTYKCTY,8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'RCTYKEY),KEYSAVE                                           
         BNE   CONTERR                                                          
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RCTYRECD,R4                                                      
         MVC   EXCTYNAM,RCTYDESC                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
CONTERR  MVC   GERROR,=AL2(INVCTY)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE MARKET RANK                                        
*        - ON EXIT QRANK IS SET                                                 
*                                                                               
VRANK    CLI   5(R2),1                                                          
         BNE   CONTERR                                                          
         MVC   QRANK,8(R2)                                                      
         CLI   QRANK,C'1'                                                       
         BL    RANKERR                                                          
         CLI   QRANK,C'9'                                                       
         BNH   EXIT                                                             
*                                                                               
RANKERR  MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE MARKET                                             
*        - ON EXIT QMKT IS SET                                                  
*                                                                               
VMKT     CLI   5(R2),1                                                          
         BL    MKTERR                                                           
         CLI   5(R2),4                                                          
         BH    MKTERR                                                           
         XC    QMKT,QMKT                                                        
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   QMKT(0),8(R2)                                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RMKTRECD,R4                                                      
         MVI   RMKTKTYP,X'2B'                                                   
         MVC   RMKTKREP,AGENCY                                                  
         MVC   RMKTKMKT,QMKT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'RMKTKEY),KEYSAVE                                           
         BNE   MKTERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RMKTRECD,R4                                                      
         MVC   EXMK2NAM,RMKTNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
MKTERR   MVC   GERROR,=AL2(INVMKT)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE AFFILIATE                                          
*        - ON EXIT QAFF IS SET                                                  
*                                                                               
VAFF     CLI   5(R2),3                                                          
         BNE   AFFERR                                                           
         MVC   QAFF,8(R2)                                                       
         B     EXIT                                                             
*                                                                               
AFFERR   MVC   GERROR,=AL2(INVAFF)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE ADVERTISER                                         
*        - ON EXIT QADV IS SET                                                  
*                                                                               
VADV     CLI   8(R2),C'*'          THIS A SET                                   
         BNE   VADV20                                                           
         SPACE                                                                  
         MVC   FULL(2),=C'AD'                                                   
         MVC   FULL+2(2),=AL1(QLADV,L'QADV)                                     
         BAS   RE,VSET             GO VALIDATE SET                              
         MVI   QADV,C'*'                                                        
         MVI   EXADVNAM,C'*'                                                    
         MVC   EXADVNAM+1(L'EXADVNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VADV20   CLI   5(R2),1                                                          
         BL    ADVERR                                                           
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   VADV30                                                           
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         GOTO1 (RFBROWSE,VREPFACS),DMCB,(RF),SYSRD,(R2),0,             +        
               (0,C' ADV'),0                                                    
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VADV30   CLI   5(R2),4                                                          
         BH    ADVERR                                                           
         XC    QADV,QADV                                                        
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,VADVMVC                                                       
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RADVRECD,R4                                                      
         MVI   RADVKTYP,X'08'                                                   
         MVC   RADVKADV,QADV                                                    
         OC    RADVKADV,=CL6' '                                                 
         MVC   RADVKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'RADVKEY),KEYSAVE                                           
         BNE   ADVERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RADVRECD,R4                                                      
         MVC   EXADVNAM,RADVNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
VADVMVC  MVC   QADV(0),8(R2)                                                    
*                                                                               
ADVERR   MVC   GERROR,=AL2(INVADV)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE AGENCY                                             
*        - ON EXIT QAGY IS SET                                                  
*                                                                               
VAGY     CLI   8(R2),C'*'          THIS A SET                                   
         BNE   VAGY20                                                           
         SPACE                                                                  
         MVC   FULL(2),=C'AG'                                                   
         MVC   FULL+2(2),=AL1(QLAGY,L'QAGY)                                     
         BAS   RE,VSET             GO VALIDATE SET                              
         MVI   QAGY,C'*'                                                        
         MVI   EXAGYNAM,C'*'                                                    
         MVC   EXAGYNAM+1(L'EXAGYNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VAGY20   CLI   5(R2),1                                                          
         BL    AGYERR                                                           
*                                                                               
         CLI   8(R2),C'='          BROWSE REQUEST?                              
         BNE   VAGY30                                                           
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         GOTO1 (RFBROWSE,VREPFACS),DMCB,(RF),SYSRD,(R2),0,             +        
               (X'80',C' AGY'),0                                                
         DC    H'0'           BROWSE SHOULD HAVE TAKEN IT FROM HERE             
*                                                                               
VAGY30   DS    0H                                                               
         CLI   5(R2),6                                                          
         BH    AGYERR                                                           
         XC    QAGY,QAGY                                                        
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,VAGYMVC                                                       
         SPACE                                                                  
* SEARCH FOR AGY-OFF                                                            
         SPACE                                                                  
         LA    RE,1(,RE)           GET FULL LENGTH                              
         LA    RF,8(,R2)           START OF FIELD                               
         MVC   DUB,=CL6' '         BLANK FILL                                   
         LA    R1,DUB                                                           
VAGY40   CLI   0(RF),C'-'                                                       
         BE    VAGY50                                                           
         MVC   0(1,R1),0(RF)       MOVE ALL UP TO DASH                          
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   RE,VAGY40                                                        
         B     VAGY60                                                           
         SPACE                                                                  
VAGY50   LA    R1,DUB+4            STARTING SPOT FOR OFFICE                     
         LA    RF,1(,RF)                                                        
         BCT   RE,VAGY54                                                        
         B     AGYERR                                                           
         SPACE                                                                  
VAGY54   MVC   0(1,R1),0(RF)       MOVE IN OFFICE                               
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   RE,VAGY54                                                        
         MVC   QAGY,DUB                                                         
         SPACE                                                                  
VAGY60   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RAGYRECD,R4                                                      
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY(6),QAGY                                                 
         OC    RAGYKAGY(6),=CL6' '                                              
         MVC   RAGYKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(23),KEYSAVE                                                  
         BNE   AGYERR                                                           
         CLC   RAGYKREP,KEYSAVE+RAGYKREP-RAGYRECD                               
         BNE   AGYERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RAGYRECD,R4                                                      
         MVC   EXAGYNAM,RAGYNAM1                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
VAGYMVC  MVC   QAGY(0),8(R2)                                                    
*                                                                               
AGYERR   MVC   GERROR,=AL2(INVAGY)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE DEVELOPMENTAL CONTRACT TYPE                        
*        - ON EXIT QDCT IS SET                                                  
*                                                                               
VDCT     CLI   8(R2),C'*'          THIS A SET                                   
         BNE   VDCT20                                                           
         SPACE                                                                  
         MVC   FULL(2),=C'DT'                                                   
         MVC   FULL+2(2),=AL1(QLDCT,L'QDCT)                                     
         BAS   RE,VSET             GO VALIDATE SET                              
         MVI   QDCT,C'*'                                                        
         MVI   EXDCTNAM,C'*'                                                    
         MVC   EXDCTNAM+1(L'EXDCTNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VDCT20   CLI   5(R2),1                                                          
         BL    DCTERR                                                           
         CLI   5(R2),2                                                          
         BH    DCTERR                                                           
         XC    QDCT,QDCT                                                        
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,VDCTMVC                                                       
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RDCTRECD,R4                                                      
         MVI   RDCTKTYP,RDCTKTYQ                                                
         MVC   RDCTKREP,AGENCY                                                  
         MVC   RDCTKCTY,QDCT                                                    
         GOTO1 HIGH                                                             
         CLC   RDCTKEY,KEYSAVE                                                  
         BNE   DCTERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RDCTRECD,R4                                                      
         MVC   EXDCTNAM,RDCTDESC                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
VDCTMVC  MVC   QDCT(0),8(R2)                                                    
*                                                                               
DCTERR   MVC   GERROR,=AL2(INVDCT)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
*                                                                               
*        ROUTINE TO VALIDATE SALESPERSON                                        
*        - ON EXIT QSAL IS SET                                                  
*                                                                               
VSAL     DS   0H                                                                
         CLI   QSAL,C'*'           THIS A SET                                   
         BNE   VSAL20                                                           
         SPACE                                                                  
         MVC   FULL(2),=C'SP'                                                   
         MVC   FULL+2(2),=AL1(QLSAL,L'QSAL)                                     
         BAS   RE,VSET             GO VALIDATE SET                              
         SPACE                                                                  
*        MVI   QSAL,C'*'                                                        
         MVI   EXSALNAM,C'*'                                                    
         MVC   EXSALNAM+1(L'EXSALNAM-1),WORK                                    
         B     EXIT                                                             
         SPACE                                                                  
VSAL20   CLI   BYTE,1                                                           
         BL    SALERR                                                           
         CLI   BYTE,3                                                           
         BH    SALERR                                                           
         OC    QSAL,SPACES                                                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSALRECD,R4                                                      
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,AGENCY                                                  
         MVC   RSALKSAL,QSAL                                                    
         GOTO1 HIGH                                                             
         CLC   RSALKEY,KEYSAVE                                                  
         BNE   SALERR                                                           
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   EXIT                 NO  - DON'T GET EXPANSION                   
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RSALRECD,R4                                                      
         MVC   EXSALNAM,RSALNAME                                                
*                                                                               
         DROP  R4                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
SALERR   MVC   GERROR,=AL2(INVSAL)                                              
         B     VRRGERR0                                                         
         EJECT                                                                  
* VALIDATE SETS AND STORE TABLE HERE                                            
         SPACE                                                                  
VSET     NTR1                                                                   
         SPACE                                                                  
         CLC   =C'CT',FULL         COULD THIS BE SPECIAL SETS                   
         BNE   VSET010              NO                                          
         CLI   9(R2),C'&&'                                                      
         BE    VSET100                                                          
         SPACE                                                                  
VSET010  XC    KEY,KEY                                                          
         XC    ELEM,ELEM           FOR SET OF SETS                              
         SPACE                                                                  
         LA    R0,3                3 SETS (OR ANY FILTERS) MAX                  
         LA    R3,SET1CDE          FIRST SET CODE                               
         LR    R5,R9                                                            
         AH    R5,=AL2(SET1TAB-SYSD)                                            
         LR    RF,R5                                                            
         SPACE                                                                  
VSET014  OC    0(2,R3),0(R3)       EMPTY ENTRY                                  
         BZ    VSET016                                                          
         LA    R3,L'SETCDE(,R3)                                                 
         LA    RF,L'SET1TAB(,RF)                                                
         BCT   R0,VSET014                                                       
         B     SETSERR             TOO MANY SETS                                
VSET016  MVC   0(2,R3),FULL+2      STORE SET CODE/LEN                           
         LR    R5,RF               START OF TABLE ADDR                          
         SPACE                                                                  
         LA    R4,KEY                                                           
         USING RSETREC,R4                                                       
         MVI   RSETKTYP,RSETKTYQ                                                
         MVC   RSETKREP,AGENCY                                                  
         MVC   RSETKSET,FULL                                                    
         MVC   RSETKID,9(R2)                                                    
         SPACE                                                                  
         CLC   =C'SP',FULL                                                      
         BNE   *+10                                                             
         MVC   RSETKID(4),WORK+1                                                
         SPACE                                                                  
         OC    RSETKID,SPACES                                                   
         SPACE                                                                  
         CLC   =C'SJ',AGENCY                                                    
         BE    VSET030                                                          
         SPACE                                                                  
* FOR SETS CT, OF, ST             DO NOT NEED TO CHECK MASTER REP               
         SPACE                                                                  
* FOR SETS FOR AG, AD, DT, GS,    NEED TO CHECK MASTER REP                      
         SPACE                                                                  
         CLI   CMBOREP,C'Y'        THIS A MASTER/SUB REP                        
         BNE   VSET030              NO                                          
         CLC   =C'CT',FULL         CONTRACT TYPE                                
         BE    VSET030                                                          
         CLC   =C'OF',FULL         OFFICE                                       
         BE    VSET030                                                          
         CLC   =C'ST',FULL         STATION                                      
         BE    VSET030                                                          
         CLC   =C'AG',FULL         AGENCY                                       
         BE    VSET020                                                          
         CLC   =C'AD',FULL         ADVERTISER                                   
         BE    VSET020                                                          
         CLC   =C'DT',FULL         DEVELOPMENTAL CONTRACT TYPE                  
         BE    VSET020                                                          
         CLC   =C'GS',FULL         GROUP SUB                                    
         BE    VSET020                                                          
         DC    H'0'                                                             
VSET020  MVC   RSETKREP,CMBOMSTR                                                
         SPACE                                                                  
VSET030  GOTO1 HIGH                                                             
         CLC   RSETKEY,KEYSAVE                                                  
         BNE   SETERR                                                           
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         SPACE                                                                  
         LA    R6,34(,R6)                                                       
         SPACE                                                                  
         CLI   0(R6),01            THIS SET DESCRIPTOR ELEM?                    
         BNE   VSET038                                                          
         SPACE                                                                  
         TM    RSET1FLG-RSET1DES(R6),X'08' IS THIS AN EXCLUDE SET               
         BZ    *+8                                                              
         OI    2(R3),X'08'                                                      
         SPACE                                                                  
         TM    RSET1FLG-RSET1DES(R6),X'80' IS THIS A SET OF SETS                
         BZ    VSET036                                                          
         MVI   ELCODE,RSETMCDQ                                                  
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)            GET LENGTH                                   
         SH    R1,=H'4'            EL CODE, EL LEN, TYPE PLUS 1 FOR MVC         
         EX    R1,VSETMVC                                                       
         SPACE                                                                  
VSET034  MVC   RSETKID,ELEM                                                     
         MVC   ELEM(252),ELEM+4                                                 
         XC    ELEM+252(4),ELEM+252                                             
         SPACE                                                                  
         GOTO1 HIGH                                                             
         CLC   RSETKEY,KEYSAVE                                                  
         BNE   SETERR                                                           
         DROP  R4                                                               
         SPACE                                                                  
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         LA    R6,34(,R6)                                                       
         B     VSET036                                                          
         SPACE                                                                  
VSETMVC  MVC   ELEM(0),3(R6)                                                    
         SPACE                                                                  
VSET036  CLI   0(R6),01                                                         
         BNE   VSET038                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         SPACE                                                                  
VSET038  MVC   WORK,SPACES         FOR SET DISCRIPTION                          
         CLI   0(R6),RSETDCDQ                                                   
         BNE   SETEMPER            SET EMPTY ERROR                              
         ZIC   RF,1(R6)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,SETMVCD                                                       
         SPACE                                                                  
         MVI   ELCODE,RSETMCDQ                                                  
         BAS   RE,NEXTEL                                                        
         BNE   SETEMPER            SET EMPTY ERROR                              
         SPACE                                                                  
VSET050  ZIC   RE,1(R6)            GET LENGTH                                   
         SH    RE,=H'4'                                                         
         SPACE                                                                  
         CLC   2(1,R6),FULL+3      BETTER BE SAME LENGTH                        
         BE    *+6                                                              
         DC    H'0'               WHAT, DIFF LEN FROM CALL AND SET REC?         
         LR    RF,R5                                                            
         SPACE                                                                  
         ZIC   R0,2(R6)            GET ENTRY LENGTH                             
VSET054  CLI   0(RF),0             FIND NEXT EMPTY SLOT IN TABLE                
         BE    VSET056                                                          
         AR    RF,R0                                                            
         B     VSET054                                                          
         SPACE                                                                  
VSET056  DS    0H                                                               
         LA    R0,L'SET1TAB(,RF)   MAX SIZE                                     
         LA    R1,1(RE,RF)                                                      
         CR    R1,R0               CK IF OVER TABLE SIZE                        
         BNH   VSET060                                                          
         DC    H'0'                EXCEEDED MAX TABLE SIZE                      
         DC    CL8'SETTABSZ'                                                    
         SPACE                                                                  
VSET060  EX    RE,SETMVC                                                        
         SPACE                                                                  
         BAS   RE,NEXTEL           IS THERE ANOTHER ELEM                        
         BE    VSET050                                                          
         SPACE                                                                  
         OC    ELEM,ELEM           IS THIS SET OF SETS                          
         BNZ   VSET034                                                          
         SPACE                                                                  
         ZIC   R0,FULL+3           GET LENGTH                                   
         LR    RF,R5               TABLE START                                  
         SR    R3,R3               ENTRY COUNT                                  
VSET070  CLI   0(RF),0                                                          
         BE    VSET074                                                          
         CLI   FULL+2,QLAGY        IF AGY                                       
         BNE   VSET071                                                          
         CLI   4(RF),C' '          SEE IF SPACE                                 
         BH    VSET072                                                          
         XC    4(2,RF),4(RF)       SET TO NULLS                                 
         B     VSET072                                                          
         SPACE                                                                  
VSET071  CLI   FULL+2,QLADV        IF ADV                                       
         BNE   VSET072                                                          
         CLI   1(RF),C' '          SEE IF SPACE                                 
         BH    *+8                                                              
         MVI   1(RF),0             SET TO NULLS                                 
         CLI   2(RF),C' '          SEE IF SPACE                                 
         BH    *+8                                                              
         MVI   2(RF),0             SET TO NULLS                                 
         CLI   3(RF),C' '          SEE IF SPACE                                 
         BH    *+8                                                              
         MVI   3(RF),0             SET TO NULLS                                 
         SPACE                                                                  
VSET072  AR    RF,R0                                                            
         BCT   R3,VSET070                                                       
         DC    H'0'                                                             
VSET074  LPR   R3,R3                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
* GET CORRES ADDR OF QSORT                                                      
         SPACE                                                                  
         XC    DMCB,DMCB                                                        
         LA    R1,DMCB                                                          
         L     RF,CALLOV                                                        
         MVC   DMCB+4(4),=X'D9000A50'                                           
         GOTO1 (RF),(R1),0                                                      
         ICM   RF,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),DMCB,(R5),(R3),(R0),(R0),0                                  
         SPACE                                                                  
* NOW ELIMINATE DUPLICATES *                                                    
         SPACE                                                                  
         LR    R4,R0               SAVE LENGTH                                  
         BCTR  R4,0                                                             
         LR    R6,R5                                                            
         BCT   R3,VSET080                                                       
         B     EXIT                                                             
         SPACE                                                                  
VSET080  LA    R6,1(R4,R6)                                                      
         SPACE                                                                  
VSET082  EX    R4,VSETCLC          COMPARE THIS TO NEXT ENTRY                   
         BL    VSET086              OK                                          
         BE    *+6                                                              
         DC    H'0'                OUT OF ORDER!                                
         LA    R0,1(,R3)           GET CURRENT COUNT + 1                        
         LR    RE,R5                                                            
         LR    RF,R6                                                            
VSET084  EX    R4,VSETMVCA                                                      
         LA    RE,1(R4,RE)                                                      
         LA    RF,1(R4,RF)                                                      
         BCT   R0,VSET084                                                       
         SPACE                                                                  
         BCTR  R3,0                                                             
         LTR   R3,R3                                                            
         BNZ   VSET082                                                          
         B     EXIT                                                             
         SPACE                                                                  
VSET086  LA    R5,1(R4,R5)                                                      
         BCT   R3,VSET080                                                       
         B     EXIT                                                             
         SPACE                                                                  
VSETCLC  CLC   0(0,R5),0(R6)                                                    
VSETMVCA MVC   0(0,RE),0(RF)                                                    
         SPACE                                                                  
* BUILD SET VALUES FOR SPECIAL CON TYPE/DEVTYP HERE *                           
         SPACE                                                                  
VSET100  LA    R0,3                3 SETS (OR ANY FILTERS) MAX                  
         LA    R1,SET1CDE          FIRST SET CODE                               
         LR    R2,R9                                                            
         AH    R2,=AL2(SET1TAB-SYSD)                                            
         LR    RF,R2                                                            
         SPACE                                                                  
VSET140  OC    0(2,R1),0(R1)       EMPTY ENTRY                                  
         BZ    VSET144                                                          
         LA    R1,L'SETCDE(,R1)                                                 
         LA    RF,L'SET1TAB(,RF)                                                
         BCT   R0,VSET140                                                       
         B     SETSERR             TOO MANY SETS                                
         SPACE                                                                  
* EVEN THOUGH ONLY 1 IS USED NOW, BOTH QLCON AND QLDCT ARE NEEDED               
* FOR CONTYPE S WITH DCT NB                                                     
         SPACE                                                                  
VSET144  CH    R0,=H'2'            MUST BE AT LEAST 2 ENTRIES LEFT              
         BL    SETSERR              TOO MANY SETS                               
         MVC   0(2,R1),=X'2101'                                                 
         MVC   0(5,RF),=C'ADFNY'                                                
         B     EXIT                                                             
         SPACE                                                                  
SETMVCD  MVC   WORK(0),2(R6)       MOVE SET DESCRIP                             
SETMVC   MVC   0(0,RF),3(R6)       MOVE CODES                                   
         SPACE                                                                  
SETERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SETERRMS),SETERRMS                                     
         GOTO1 ERREX2                                                           
SETEMPER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SETEMPMS),SETEMPMS                                     
         GOTO1 ERREX2                                                           
SETSERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SETSMSG),SETSMSG                                       
         GOTO1 ERREX2                                                           
         EJECT                                                                  
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS           
*                                                                               
VRRGERR0 MVI   GMSGTYPE,C'E'                                                    
         SPACE 2                                                                
VRRGERR  OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
*                                                                               
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    *+8                 NO -- USE CONTROL SYSTEM MESSAGES            
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
*                                                                               
         B     TRAPERR                                                          
         EJECT                                                                  
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
EXIT     XIT1                                                                   
         SPACE                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
* CONSTANTS TABLES, ETC *                                                       
         SPACE                                                                  
STEREO   DC    C'815'                                                           
         SPACE                                                                  
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(GETBROAD)                                                      
         SPACE 1                                                                
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 1                                                                
PHASENM  DC    X'D9081500'    PRESET FOR SYSTEM CALLOVS                         
SETERRMS DC    C'** ERROR ** SET NOT FOUND *'                                   
SETEMPMS DC    C'** ERROR ** NO DATA IN SET *'                                  
SETSMSG  DC    C'** ERROR ** ONLY 3 FILTERS/SETS ALLOWED *'                     
         EJECT                                                                  
*        DIRECTORY OF PROGRAMS                                                  
*                                                                               
*                          ACTION           PROG SCRN                           
*                                                                               
*        SALES             DIS               02*  F2                            
*                          LIST              02   E2                            
*                                                                               
*  *-DELETE NOT ALLOWED  +-NO SELECT FUNCTION  &-NO ADD-CHANGE DOES IT          
*                                                                               
         SPACE 3                                                                
*              DIRECTORY OF RECORDS AND ACTIONS                                 
         SPACE 1                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'01',C'SALES   ',AL1(02),X'00C2'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
         DC    X'02',C'*ISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
         SPACE 1                                                                
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
*                                 SC  SP  AC                                    
*                                   OV  RP                                      
         DC    X'03',AL1(02,01),X'F2020000C0',C'    '  SALES    MAINT           
         DC    X'03',AL1(02,10),X'E2020002C0',C'RGRG'           LIST            
         DC    X'FF'                                                            
       ++INCLUDE RETVBTAB                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  R7,RB,RC                                                         
***********************************************************************         
* CKKGLOB - (CHECK GLOBBER) ROUTINE TO TRAP RETURN CALLS BY GLOBBER   *         
*  P1 = RC                                                            *         
***********************************************************************         
CKGLOB   NMOD1 0,*CKGLOB*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
*                                                                               
* CHECK FOR GLOBBER CONTROL ELEM                                                
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,24,GLVXCTL                               
         TM    DMCB+8,X'10'                                                     
         BNZ   CKGLBX                                                           
         GOTO1 (RF),DMCB,=C'DELE',,,GLVXCTL                                     
*                                                                               
         LA    R3,ELEM             INCASE CONTRACT PICKS UP ITS OWN             
         USING GLVXFRSY,R3         OUT GOING GLOBBER CONTROL KEY                
*                                                                               
         CLC   =C'BRO',GLVXFRPR    RETURN CALL FROM BROWSE?                     
         BE    CKGLB100            YES                                          
*                                                                               
         B     CKGLBX                                                           
         DROP  R3                                                               
*                                                                               
CKGLB100 DS    0H                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         USING COMFACSD,RF                                                      
         L     RF,CGLOBBER                                                      
         DROP  RF                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,GLBRWLNQ,GLRBRWSE                        
         TM    DMCB+8,X'10'        NO BROWSE ELEM = USER CANCEL                 
         BZ    CKGLB102                                                         
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         USING COMFACSD,RF                                                      
         L     RF,CGETTXT                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB,162,0,(C'I',0),0,X'44',0                               
         B     CKGLB210                                                         
*                                                                               
CKGLB102 DS    0H                                                               
         GOTO1 (RF),DMCB,=C'DELE',,,GLRBRWSE                                    
*                                                                               
         LA    R3,ELEM                                                          
         USING GLBRWKW,R3                                                       
         OC    GLBRWKW,SPACES                                                   
         CLC   GLBRWKW,SPACES      NO RECORD RETURNED?                          
         BNE   CKGLB105                                                         
*                             OUR OWN LITTLE ERROR ROUTINE                      
         LA    R3,633       #633 = NO MATCHING RECORDS FOUND                    
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         USING COMFACSD,RF                                                      
         L     RF,CGETTXT                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB+12,(R3),0,(C'E',DMCB),0,0,0                            
         L     RD,SYSRD                                                         
         B     CKGLBX                                                           
*                                                                               
CKGLB105 DS    0H                                                               
         CLC   =C'AGY',GLBRWREC  **CASE OF AGY REC RETURNED**                   
         BNE   CKGLB110                                                         
         MVC   RGEAGY,GLBRWKW          MOVE AGY CODE TO SCREEN                  
         MVI   RGEAGYH+5,L'RGEAGY                                               
         NI    RGEAGYH+4,X'FF'-X'20'   SET NOT VALIDATED                        
         OI    RGEAGYH+6,X'80'                                                  
         B     CKGLB200                                                         
*                                                                               
CKGLB110 DS    0H                                                               
         CLC   =C'ADV',GLBRWREC   **CASE OF ADV REC RETURNED**                  
         BNE   CKGLB120                                                         
         MVC   RGEADV,GLBRWKW           MOVE ADV CODE TO SCREEN                 
         MVI   RGEADVH+5,L'RGEADV                                               
         NI    RGEADVH+4,X'FF'-X'20'    SET NOT VALIDATED                       
         OI    RGEADVH+6,X'80'                                                  
         B     CKGLB200                                                         
*                                                                               
CKGLB120 DS    0H                                                               
         DC    H'0'                NO MORE POSSIBLE RECORD TYPES                
*                                                                               
         DROP  R3                                                               
CKGLB200 DS    0H                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)           COMFACS                                      
         USING COMFACSD,RF                                                      
         L     RF,CGETTXT                                                       
         DROP  RF                                                               
         GOTO1 (RF),DMCB,161,0,(C'I',0),0,X'44',0                               
CKGLB210 DS    0H                                                               
         MVC   CONSERV(4),=C'=RE '     FORCE SCREEN REFRESH                     
         MVI   CONSERVH+5,4                                                     
         OI    CONSERVH+6,X'80'                                                 
         L     RD,SYSRD                GET ALL THE WAY OUT OF HERE              
CKGLBX   XMOD1                                                                  
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE REGLBRW                                                        
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
RREPD    DSECT                                                                  
       ++INCLUDE REGENREPA                                                      
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RENRGWKN                                                       
         EJECT                                                                  
       ++INCLUDE RENRGFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RENRGE2D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
***************                                                                 
*  RERRGWTWA  *                                                                 
***************                                                                 
       ++INCLUDE RERRGWTWA                                                      
         EJECT                                                                  
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
ROWNRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENOWN                                                       
         PRINT ON                                                               
         SPACE 1                                                                
RCLSRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENCLS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
RCTGRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENCTG                                                       
         PRINT ON                                                               
         SPACE 1                                                                
RCTYRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENCTY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
RMKTRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENMKT                                                       
         PRINT ON                                                               
RSTARECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
ROFFRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENOFF                                                       
         PRINT ON                                                               
RGRPRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENGRP                                                       
         PRINT ON                                                               
RRGNRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENREG                                                       
         PRINT ON                                                               
RTEMRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENTEM                                                       
         PRINT ON                                                               
RADVRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENADV                                                       
         PRINT ON                                                               
RAGYRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENAGY                                                       
         PRINT ON                                                               
RDCTRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENDCT                                                       
         PRINT ON                                                               
RSALRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSAL                                                       
         PRINT ON                                                               
RSETRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSET                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064RENRG00A  05/01/02'                                      
         END                                                                    
