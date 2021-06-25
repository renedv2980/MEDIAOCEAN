*          DATA SET PPEZF00    AT LEVEL 092 AS OF 05/01/02                      
*PHASE T43000A,+0                                                               
*INCLUDE PUBEDIT                                                                
*INCLUDE PUBVAL                                                                 
*INCLUDE SRCHCALL                                                               
*INCLUDE PZTABS                                                                 
*INCLUDE KHDUMMY                                                                
*        TITLE 'T43000 - PRINT EPIC FILE MAINTENANCE BASE'                      
         TITLE 'T43000 - PRINT EPIC FILE MAINTENANCE BASE'                      
***********************************************************************         
*                                                                     *         
*        TITLE 'T43000 - PRINT EPIC FILE MAINTENANCE BASE'            *         
*                                                                     *         
***********************************************************************         
         SPACE 3                                                                
***********************************************************************         
*                                                                     *         
*  BILL, AGYNAME & OFFNAME RECORDS ON CONTROL FILE - GENDIR           *         
*                                                                     *         
*  CLINAME, PRDNAME, & STATION EQUIVALENCY RECORDS ON MPL FILE        *         
*   ADV1 USES MPL1, ADV2=2, ADV3=3, ADV4=4, ADV5=5 TST=MPL1           *         
*                                                                     *         
***********************************************************************         
         SPACE 3                                                                
T43000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T43000,R7,RR=RE,CLEAR=YES                                
         LR    R9,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         ST    R9,ASYSD            SAVE ADDRESS                                 
         ST    RC,SVRC                                                          
         ST    RE,RELO00                                                        
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         L     R3,SYSPARMS                                                      
         L     RA,4(R3)            A(TWA)                                       
         USING T430FFD,RA                                                       
         L     R2,8(R3)            A(COMFACS)                                   
         L     RF,4(R2)            (CALLOV)                                     
         ST    RF,CALLOV                                                        
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),(RA)  LOAD PHASE T43010 - PZMOD            
*                                                                               
         CLI   DMCB+4,X'FF'        MUST FIND PZMOD                              
         BNE   *+6                                                              
         DC    H'0'                WHERE IS T43010                              
*                                                                               
         L     RF,DMCB             SAVE A(PZMOD)                                
         ST    RF,VPZMOD                                                        
*                                                                               
         L     R1,=A(WRKFBUFR-SYSD)   SET UP WORKER BUFFER ADDRESS              
         LA    R1,SYSD(R1)                                                      
         ST    R1,WRKFBUFA                                                      
*                                                                               
*        SPECIAL PROCESSING FOR CONVERT RECORD                                  
*        IF INSERTION DETAILS SCREEN COMING IN USE RECACT3 TABLE                
*        IF LIST SCREEN AND A SELECT CODE IS FOLLOWED BY 'D'                    
*           USE RECACT3 TABLE                                                   
*                                                                               
UNRACT   DS    0H                                                               
*                                  SKIP IF OFF-LINE                             
         OC    TWAVPRNT-T430FFD(,RA),TWAVPRNT-T430FFD(RA)                       
         BNZ   UNRACTX                                                          
*                                                                               
         CLI   TWASCR-T430FFD(RA),X'D8'   IF DETAILS SCREEN                     
         BE    UNRACT3             SET TO RECACT3                               
*                                                                               
         CLI   TWASCR-T430FFD(RA),X'E8'   IF LIST    SCREEN                     
         BNE   UNRACT3X                                                         
*                                                                               
         LA    R2,LINSELH          POINT TO FIRST SELECT FIELD                  
         SR    RF,RF                                                            
*                                                                               
UNRACTLP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE IF END OF SCREEN                        
         BE    UNRACT3X                                                         
*                                                                               
         CLI   5(R2),1             SKIP IF ENTRY AT MOST 1 LONG                 
         BNH   UNRACTCN                                                         
*                                                                               
         CLI   8+1(R2),C'D'        LOOKING FOR 'D' IN POSITION 2                
         BNE   UNRACTCN                                                         
*                                                                               
         CLI   8+0(R2),C'S'        CHANGE 'S' TO 'C'                            
         BNE   *+8                                                              
         MVI   8+0(R2),C'C'                                                     
*                                                                               
         CLI   8+0(R2),C'X'        CHANGE 'X' TO 'C'                            
         BNE   *+12                                                             
         MVI   0+8(R2),C'C'                                                     
         MVI   1+8(R2),C'X'        CHANGE 'D' TO 'X'                            
*                                                                               
         MVI   NEWKEY,C'Y'         RESET NEWKEY SWITCH                          
*                                                                               
         B     UNRACT3             FOUND - USE SECOND RECACT TABLE              
*                                                                               
UNRACTCN DS    0H                                                               
*                                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
*                                                                               
         CLI   0(R2),0             DONE IF END OF SCREEN                        
         BE    UNRACT3X                                                         
*                                                                               
         IC    RF,0(R2)            BUMP TO NEXT SEL FIELD ON SCREEN             
         LA    R2,0(RF,R2)                                                      
*                                                                               
         B     UNRACTLP                                                         
*                                                                               
UNRACT3  DS    0H                                                               
*                                                                               
         LA    RF,RECACT3          USE SPECIAL RECORD/ACTION TABLE              
         ST    RF,ARECACT3                                                      
*                                                                               
UNRACT3X DS    0H                                                               
*                                                                               
*        IF DETAILS MAINT SCREEN                                                
*        AND PFKEY HIT OR SEQUENCE NUMBER ENTERED                               
*        AND ACTION IS SELECT                                                   
*        THEN MAKE ACTION 'CHANGE' TO FORCE RETURN TO SCREEN                    
*                                                                               
         L     RF,SYSPARMS         POINT TO TIOBD                               
         L     RF,0(RF)                                                         
         USING TIOBD,RF            ESTABLISH TIOBD                              
*                                                                               
         CLI   TWASCR,X'D8'        SKIP IF NOT DETAILS MAINT SCREEN             
         BNE   UNPFK10                                                          
         CLC   =C'SEL',CONACT      SKIP IF ACTION IS NOT 'SELECT'               
         BNE   UNPFK10                                                          
*                                                                               
         TM    DTLDFTRH+4,X'20'    CHANGE ACTION IF FILTERS  ENTERED            
         BNO   UNPFKCHG                                                         
         TM    DTLSQNH+4,X'20'     CHANGE ACTION IF SQN      ENTERED            
         BNO   UNPFKCHG                                                         
         TM    DTLPCDH+4,X'20'     CHANGE ACTION IF PRODUCT  ENTERED            
         BNO   UNPFKCHG                                                         
         TM    DTLECDH+4,X'20'     CHANGE ACTION IF ESTIMATE ENTERED            
         BNO   UNPFKCHG                                                         
*                                                                               
UNPFK10  DS    0H                                                               
*                                                                               
         CLI   TIOBAID,0           SKIP IF NO PFKEY HIT                         
         NOP   UNPFKX              CREATES PROBLEMS IF 'BE'                     
*                                  DISABLES RETURN TO LIST ON 'ENTER'           
*                                                                               
         CLI   TWASCR,X'D8'        SKIP IF NOT DETAILS MAINT SCREEN             
         BNE   UNPFKX                                                           
         CLC   =C'SEL',CONACT      SKIP IF ACTION IS NOT 'SELECT'               
         BNE   UNPFKX                                                           
*                                                                               
         CLI   TIOBAID,12          SKIP IF PF12 OR PF24                         
         BE    *+8                                                              
         CLI   TIOBAID,24                                                       
         BE    UNPFKX                                                           
*                                                                               
UNPFKCHG DS    0H                                                               
*                                                                               
         XC    CONACT,CONACT       CLEAR ACTION FIELD                           
         MVC   CONACT(6),=C'CHANGE'   REPLACE ACTION WITH 'CHANGE'              
         MVI   CONACTH+5,6         SET INPUT LENGTH                             
*                                                                               
UNPFKX   DS    0H                                                               
*                                                                               
UNRACTX  DS    0H                                                               
*                                                                               
         MVI   DMCB+7,QLINUP       GET V(LINUP)                                 
         BAS   RE,INCALLOV                                                      
         L     RF,0(R1)                                                         
         ST    RF,VLINUP                                                        
*                                                                               
         MVI   DMCB+7,QTSAR        GET V(TSAR)                                  
         BAS   RE,INCALLOV                                                      
         L     RF,0(R1)                                                         
         ST    RF,VTSAR                                                         
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   SKIP IF OFF-LINE                             
         BNZ   INIT10                                                           
*                                                                               
         MVI   DMCB+7,QPRHELP      GET V(PRHELP)                                
         BAS   RE,INCALLOV                                                      
         L     RF,0(R1)                                                         
         ST    RF,VPRHELP                                                       
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
*        CALL GENCON                                                            
*                                                                               
         MVI   DMCB+7,QGENCON                                                   
*                                                                               
         BAS   RE,INCALLOV                                                      
*                                                                               
         L     RF,0(R1)            PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
*                                                                               
         B     EXIT                                                             
*                                                                               
INCALLOV LR    R0,RE                                                            
*                                                                               
         XC    DMCB(7),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
         GOTO1 CALLOV,DMCB                                                      
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES *                                          
*                                                                               
SYSINIT  NTR1                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,VCOUNT                                                        
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO00                                                        
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                                                               
* SET SYSTEM DEPENDENT VALUES *                                                 
*                                                                               
         L     R1,SYSPARMS                                                      
         L     RE,16(R1)                                                        
         ST    RE,ACOMFACS                                                      
         MVI   SYSTEM,C'Z'         EZ SYSTEM                                    
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4096 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'GENFIL  '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
         MVI   GETMSYS,30          USES GETMSG FOR SYSTEM 30                    
         MVC   LWORK(2),=A(LENWORK)   SET WORK AREA LENGTH                      
         MVC   RCPROG(2),=C'PP'    PREFIX FOR REPORT NO.                        
*                                                                               
         OI    GENSTAT1,APPLIC     USE PHASE NODE STRUCTURE                     
*                                                                               
         L     R1,ACOMFACS                                                      
*                                                                               
         L     RF,CGETFACT-COMFACSD(,R1)                                        
*                                                                               
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         USING FACTSD,RE                                                        
         DROP  RE                                                               
*                                                                               
         MVC   SYSPHASE,=X'D9043000'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         LA    R1,SVSTART          SET SAVED STORAGE START                      
         ST    R1,ASTARTSV                                                      
         MVI   NTWA,X'40'          RESTORE LARGE SYSD AREA                      
*        MVC   LSVTWA0,=AL2(SYSDEND-SVSTART)                                    
         MVC   LSVTWA0,=AL2(MAXLTWA0) IF OVER MAX                               
*        BNH   *+6                                                              
*        DC    H'0'                NEED TO LOOK AT SAVED STORAGE LENGTH         
*                                  AND NEED TO GRAB MORE AT NMOD1, IF           
*                                  OVER 7268, SET NTWA TO 1                     
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    R7,BASER7                                                        
         XC    UTL,UTL                                                          
         L     RE,SYSPARMS                                                      
         L     RF,4(,RE)                                                        
*                                                                               
*        MAKE SURE WE START AS A MEDIA SYSTEM                                   
*                                                                               
         MVI   CURSYST,C'M'        SET MEDIA SYSTEM ID                          
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA SYSTEM                       
*                                                                               
         OC    TWAVPRNT-T430FFD(,RF),TWAVPRNT-T430FFD(RF)                       
         BZ    EXIT                                                             
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         MVC   UTL,FAAUTL-FACTSD(RE)                                            
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
*                                                                               
         DS    0H                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
         SRL   RF,24                                                            
         LA    RF,VBRANCH(RF)      POINT TO BRANCH INSTRUCTION                  
         CLI   0(RF),0             IF ACTUALLY AN INSTRUCTION                   
         BNE   0(RF)                  GO TAKE THE BRANCH                        
         L     RF,0(RF)            ELSE ITS AN ADDRESS                          
         A     RF,RELO00           RELOCATE THE ADDRESS                         
         BASR  RE,RF               GO TO ROUTINE                                
         B     EXIT                                                             
*                                                                               
         DS    0A                                                               
VBRANCH  B     VUSER               VALIDATE USER                                
         B     VMED                         MEDIA                               
         B     VCLI                         CLIENT                              
         B     VPROD                        PRODUCT                             
         B     VPUB                         PUB                                 
         B     VFASWTCH            SWITCH TO M=MEDIA (SPOT/NET)                 
*                                            P=MEDIA PLANNING (MPL)             
*                                            C=CONTROL (GEN)                    
         DC    A(VFTR)             VALIDATE FILTERS                             
         B     VEST                VALIDATE ESTIMATE                            
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TEST TO SEE IF FILE NEEDS TO BE SWITCHED CONTROL/SPOT                         
*                                                                               
VFASWTCH CLI   CURSYST,C'M'        SWITCH TO MEDIA SYSTEM                       
         BNE   VFASWT10                                                         
         MVC   LKEY,=H'25'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'2'                                                    
         MVC   DATADISP,=H'33'                                                  
         LA    R2,=C'PRT'                                                       
         B     SW100                                                            
*                                                                               
* USE FASWITCH TO SWITCH TO CONTROL                                             
*                                                                               
VFASWT10 CLI   CURSYST,C'C'        SWITCH TO CONTROL SYSTEM                     
         BNE   VFASWT20                                                         
         LA    R2,=C'CON'                                                       
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         B     SW100                                                            
*                                                                               
* USE FASWITCH TO SWITCH TO MEDIA PLANNING                                      
*                                                                               
VFASWT20 CLI   CURSYST,C'P'        MEDIA PLANNING SYSTEM                        
         BE    VFASWT22                                                         
         DC    H'0'                                                             
VFASWT22 LA    R2,=C'MPL'                                                       
*                                                                               
         BAS   RE,MPLSW                                                         
         B     SW150                                                            
*                                                                               
SW100    L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF               IF OFFLINE, NOT AVAILABLE OR NEEDED          
         BZ    SW140                                                            
*                                                                               
         GOTO1 (RF),DMCB,(R2),0                                                 
         CLI   4(R1),0             ALL SWITCHES COME HERE                       
         BNE   SWERR                                                            
         B     SW150                                                            
*                                                                               
SW140    CLI   SAVSYS,0            EVER SWITCHED TO MPL                         
         BE    SW150                NO                                          
*                                                                               
         ICM   RF,15,UTL           MUST BE A UTL ADDRESS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   4(1,RF),SAVSYS      SAVE CURRENT SYSTEM                          
*                                                                               
SW150    CLI   0(R2),C'C'          DON'T CHANGE FOR CON SYSTEM                  
         BE    EXIT                                                             
         LA    R2,=C'PRT'                                                       
SW200    MVC   SYSDIR(3),0(R2)                                                  
         MVC   SYSFIL(3),0(R2)                                                  
         B     EXIT                                                             
         EJECT                                                                  
* GET AGENCY NAME/ADDRESS FROM CONTROL FILE ID RECORD *                         
*                                                                               
VUSER    DS    0H                                                               
         MVC   QAGY,TWAAGY         SET 2-BYTE AGENCY                            
         L     R3,ATWA                                                          
         CLI   29(R3),0            TEST FIRST TIME                              
         BE    *+14                YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER10                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   VUSER06                                                          
*                                                                               
         MVI   CURSYST,C'C'        SWITCH TO CONTROL FILE                       
         GOTO1 VALIFAS             SWITCH                                       
*                                                                               
VUSER06  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(R3)  FROM TWA                                     
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R4)                    
*                                                                               
         CLI   DMCB+8,0            ANY ERROR                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    *+8                                                              
*****    BE    *+6                                                              
         B     VUSER10                                                          
*****    DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         DROP  R6                                                               
*                                                                               
VUSER10  L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF               IF OFFLINE, CK IF MPL FILE OPEN              
         BZ    VUSER14                                                          
         LA    R1,DMCB                                                          
         MVC   0(4,R1),=X'FEFFFFFF'    SYS=FE GETS SYSFAC ADDRESS               
         BASR  RE,RF                                                            
*                                                                               
         L     RE,0(R1)                GET SYSFAC ADDRESS                       
         L     RF,VSSB-SYSFACD(,RE)                                             
         MVC   FACPAKRS,SSBSYSN1-SSBD(R1)                                       
VUSER14  LA    R2,CONRECH                                                       
         CLI   CONREC,C'?'         THIS A HELP REQUEST                          
         BE    VUSERX                                                           
         CLI   CONRECH+5,2         MUST BE 2 POSTION REC ID                     
         BL    VURECLN                                                          
*                                                                               
         ZIC   RF,CONRECH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,VUSERCLA         THIS CLINAME                                 
         BE    VUSER20                                                          
         EX    RF,VUSERCLB         THIS PRDNAME                                 
         BE    VUSER20                                                          
         EX    RF,VUSERCLE         THIS STATION EQUIVALENCY                     
         BE    VUSER20                                                          
         EX    RF,VUSERCLC         THIS AGYNAME                                 
         BE    VUSER16                                                          
         EX    RF,VUSERCLD         THIS OFFNAME                                 
         BE    VUSER16                                                          
         EX    RF,VUSERCLF         THIS BILL                                    
         BNE   VUSERX                                                           
VUSER16  MVC   SYSFIL,=C'GENFIL  '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
         LA    R2,=C'CON'                                                       
         MVI   CURSYST,C'C'        INDICATE CONTROL SYSTEM                      
         CLI   OFFLINE,C'Y'                                                     
         BNE   VUSER50                                                          
*                                                                               
* ONLY IF OFFLINE, OPEN CONTROL FILE IF NOT OPEN ALREADY *                      
*                                                                               
         ICM   RF,15,UTL           MUST BE A UTL ADDRESS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  OPEN CONTROL                                 
         CLI   CTLOPEN,C'Y'                                                     
         BE    VUSER50                                                          
*                                                                               
         MVC   SAVSYS,4(RF)        SAVE CURRENT SYSTEM                          
*                                                                               
         MVI   4(RF),CONSECTL                                                   
         L     R4,AIO1             USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=CL7'CONTROL',FILELST,(R4)                 
         MVI   CTLOPEN,C'Y'                                                     
         B     VUSER50                                                          
*                                                                               
VUSER20  MVC   SYSFIL,=C'MPLFIL  '                                              
         MVC   SYSDIR,=C'MPLDIR  '                                              
         LA    R2,=C'MPL'                                                       
         MVI   CURSYST,C'P'        INDICATE MEDIA PLANNING SYSTEM               
*                                                                               
         BAS   RE,MPLSW                                                         
         B     VUSERX                                                           
*                                                                               
VUSER50  CLI   OFFLINE,C'Y'                                                     
         BE    VUSERX                                                           
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,(R2),0                                                 
         CLI   4(R1),0             WAS SWITCH OK                                
         BNE   SWERR                                                            
*                                                                               
VUSERX   MVC   EPICWK,=CL8'EPICWK' USE NEW EPICWK FILE                          
         NOP   EXIT                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(2),10(R3)                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'GFILE'),=C'WRKFIL',WORK,AIO1,ATIA             
*                                                                               
         LA    R1,WORK                                                          
         USING UKRECD,R1                                                        
         MVC   EPICWK,UKUSRINF                                                  
         DROP  R1                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
VUSERCLA CLC   CONREC(0),CLINAME                                                
VUSERCLB CLC   CONREC(0),PRDNAME                                                
VUSERCLC CLC   CONREC(0),AGYNAMED                                               
VUSERCLD CLC   CONREC(0),OFFNAMED                                               
VUSERCLE CLC   CONREC(0),STAEQUIV                                               
VUSERCLF CLC   CONREC(0),BILLREC                                                
*                                                                               
VURECLN  MVI   ERROR,PZERECLN      RECORD FIELD AT LEAST 3                      
         B     VUERRX                                                           
*                                                                               
VUERRX   DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'PPEZF00 - VALIDATE MEDIA - VMED'                                
***********************************************************************         
*                                                                     *         
*        VALIDATE MEDIA                                               *         
*                                                                     *         
*NTRY    R2 ==>  MEDIA FIELD TO BE VAILDATED                          *         
*                                                                     *         
*                                                                     *         
*EXIT    QMED       MEDIA CODE                                        *         
*        MEDNM      MEDIA PRINTABLE NAME                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VMED     DS    0H                                                               
*                                                                               
         MVI   ERROR,PZEMEDNV                                                   
*                                                                               
         CLI   5(R2),1             INPUT LEN MUST BE 1                          
         BNE   TRAPERR                                                          
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING PAGYKEY,R4                                                       
         MVC   PAGYKAGY,AGENCY                                                  
         MVC   PAGYKMED,8(R2)      MEDIA                                        
         MVI   PAGYKRCD,X'01'                                                   
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,PZEMEDNV                                                   
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING PAGYELEM,R6                                                      
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   TRAPERR                                                          
*                                                                               
         MVC   USERNAME,PAGYNAME                                                
         MVC   USERADDR,PAGYADDR                                                
         MVC   SVUSER,USERNAME        SAVE FOR FUTURE REF                       
         MVC   QMED,8(R2)             SAVE INPUT MEDIA CODE                     
         MVC   MEDNM,PAGYMED          MEDIA NAME                                
*                                                                               
VMEDX    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* VALIDATE CLIENT - ON EXIT QCLT IS SET                                         
*                                                                               
VCLI     GOTO1 ANY                 CLIENT                                       
*                                                                               
         MVI   ERROR,PZECLTNF                                                   
         MVC   QCLT(3),WORK                                                     
         CLI   5(R2),3                                                          
         BH    TRAPERR                                                          
         CLI   5(R2),2                                                          
         BL    TRAPERR                                                          
*                                                                               
* READ CLIENT HEADER *                                                          
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
         XC    KEY,KEY                                                          
*                                                                               
         LA    R6,KEY                                                           
         USING PCLTRECD,R6                                                      
*                                                                               
         MVC   PCLTKAGY,QAGY                                                    
         MVC   PCLTKMED,QMED                                                    
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,QCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,NOTFOUND                                                   
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
*                                                                               
         MVC   FILENAME,=C'PRTFIL  '                                            
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1            RESET TO 1ST IO AREA                         
*                                                                               
         USING PCLTRECD,R6                                                      
*                                                                               
         MVC   WORK+4(20),PCLTNAME                                              
*                                                                               
         MVC   CLTNM,PCLTNAME          CLIENT NAME                              
*                                                                               
         MVI   ERROR,SECLOCK                                                    
*                                                                               
         OC    T430FFD+6(2),T430FFD+6  TEST ANY SECURITY LIMIT                  
         BZ    VCLIX                                                            
         CLI   T430FFD+6,C'*'          TEST OFFICE LOCKOUT                      
         BE    CL100                                                            
         CLI   T430FFD+6,C'$'          TEST OFFICE LIST LOCKOUT                 
         BE    CL200                                                            
         CLI   T430FFD+6,C'+'          TEST MARKET LOCKOUT                      
         BE    VCLIX                   YES - IGNORE                             
         CLC   T430FFD+6(3),QCLT       ELSE SINGLE CLIENT ACCESS                
         BNE   TRAPERR                                                          
         B     VCLIX                                                            
*                                                                               
CL100    CLC   T430FFD+7(1),PCLTOFF   MATCH OFFICE CODE                         
         BNE   TRAPERR                                                          
VCLIX    MVI   ERROR,0                                                          
         B     EXIT                                                             
*                                                                               
CL200    XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    DUB,DUB                                                          
         LA    R1,DUB                                                           
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'         SYSTEM ID                                    
         MVC   OFCAUTH,T430FFD+6   ID AUTH VALUE                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,PCLTOFF                                                   
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,DUB,ACOMFACS                                           
         CLI   0(R1),0                                                          
         BNE   TRAPERR                                                          
         B     VCLIX                                                            
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE PRD  - ON EXIT WORK(3)    = EBCDIC PRODUCT                           
*                         WORK+3(1)  = PRODUCT CODE                             
*                         WORK+4(20) = PRODUCT NAME                             
*                                                                               
VPROD    CLI   QCLT,C' '           WAS CLIENT ENTERED                           
         BNH   VPRCLTNE                                                         
*                                                                               
         GOTO1 ANY                 PRODUCT                                      
*                                                                               
         MVC   QPRD,WORK                                                        
         MVI   ERROR,PZEPRDNF                                                   
         CLI   5(R2),3                                                          
         BH    VPRDERR                                                          
         CLI   5(R2),2                                                          
         BL    VPRDERR                                                          
         CLC   =C'AAA',WORK                                                     
         BE    TRAPERR                                                          
*                                                                               
         MVC   FILENAME,=C'PRTDIR  '                                            
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PPRDRECD,R4                                                      
         MVC   PPRDKAGY,QAGY                                                    
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,QCLT                                                    
         OC    PPRDKCLT,SPACES                                                  
         MVC   PPRDKPRD,QPRD                                                    
         OC    PPRDKPRD,SPACES                                                  
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         MVI   ERROR,NOTFOUND                                                   
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VPRDERR                                                          
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         MVC   FILENAME,=C'PRTFIL  '                                            
         GOTO1 GETREC                                                           
         USING PPRDRECD,R6                                                      
         XC    FILENAME,FILENAME                                                
         MVC   WORK+4(20),PPRDNAME                                              
         MVC   AIO,AIO1            RESET TO 1ST IO AREA                         
         MVI   ERROR,0                                                          
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
VPRCLTNE MVI   ERROR,PZENOCLT      CLIENT REQUIRED FOR PROD VALIDATION          
         B     VPRDERR                                                          
*                                                                               
VPRDERR  CLI   ERROPT,C'Y'                                                      
         BE    EXIT                                                             
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* VALIDATE ESTIMATE                                                             
*                                                                               
VEST     CLI   QCLT,C' '           WAS CLIENT ENTERED                           
         BNH   VESCLTNE                                                         
         CLI   QPRD,C' '           AND PRODUCT                                  
         BNH   VESCLTNE                                                         
*                                                                               
         CLI   5(R2),1                                                          
         BL    VESESTLN                                                         
         CLI   5(R2),3                                                          
         BH    VESESTLN                                                         
*                                                                               
         MVI   MAX,0                                                            
         GOTO1 VALIDEC                                                          
         L     R1,FULL                                                          
         M     R0,=F'1'                                                         
         CH    R0,=H'999'                                                       
         BH    VESESTLN                                                         
         D     R0,=F'100'                                                       
         STCM  R1,3,BEST                                                        
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PESTRECD,R4                                                      
         MVC   PESTKAGY,QAGY                                                    
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,QCLT                                                    
         MVC   PESTKPRD,QPRD                                                    
         MVC   PESTKEST,BEST                                                    
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   VESESTNF                                                         
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
VESTX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
VESESTLN MVI   ERROR,PZEESTLN      ESTIMATE MUST BE 1 - 999                     
         B     VESERRX                                                          
*                                                                               
VESESTNF MVI   ERROR,PZEESTNF      ESTIMATE NOT ON FILE                         
         B     VESERRX                                                          
*                                                                               
VESCLTNE MVI   ERROR,PZECLTNE      NEED CLT/PRD FOR VALIDATION                  
         B     VESERRX                                                          
*                                                                               
VESERRX  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'PPEZF00 - VALIDATE PUB - VPUB'                                  
***********************************************************************         
*                                                                     *         
*        VALIDATE PUB ID                                              *         
*                                                                     *         
*NTRY    R2 ==>  PUB FIELD TO BE VALIDATE                             *         
*                                                                     *         
*                                                                     *         
*EXIT    QPUB       PUB INTERNAL ID                                   *         
*        PUBPRNT    PUB PRINTABLE FORM                                *         
*        PUBPNAME   PUB NAME                                          *         
*        PUBPZONE   PUB EDITION NAME                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VPUB     DS    0H                                                               
*                                                                               
         CLI   5(R2),3                                                          
         BNE   *+14                                                             
         CLC   =C'ALL',8(R2)       IS THIS AN ALL REQUEST                       
         BE    VPBALLER                                                         
*                                                                               
*        INIT HEADLINE PUB RELATED FIELDS                                       
*                                                                               
         XC    QPUB,QPUB           INTERNAL CODE                                
         MVC   PUBPRNT,SPACES      PRINTABLE PUB ID                             
         MVC   PUBPNM,SPACES       PUB NAME                                     
         MVC   PUBPZNM,SPACES      ZONE NAME                                    
         XC    PUBPTRTB,PUBPTRTB   PUB         TRANSLATION TABLE ENTRY          
         XC    PUBETRTB,PUBETRTB   PUB EDITION TRANSLATION TABLE ENTRY          
*                                                                               
         CLI   8(R2),C'='          IF INPUT STARTS WITH '='                     
         BNE   VPUBSCHX                                                         
*                                     NEED TO DO NAME SEARCHING                 
         S     R2,ATWA                DISPLACEMENT OF FIELD INTO TWA            
*                                                                               
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
*                                                                               
         MVC   DSMEDCOD,QMED                                                    
*                                                                               
         GOTO1 VSRCHCAL,DMCB,(3,(R2)),(X'80',ATWA),ACOMFACS,           X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0                                 
*                                                                               
         DC    H'0'                SHOULD NEVER RETURN HERE                     
*                                                                               
         DROP  R3                                                               
*                                                                               
VPUBSCHX DS    0H                                                               
*                                                                               
         ZIC   R0,5(R2)                                                         
         GOTO1 VPUBVAL,DMCB,((R0),8(R2)),QPUB                                   
*                                                                               
         MVI   ERROR,PZEPUBNF                                                   
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BE    TRAPERR                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PUBRECD,R4                                                       
*                                                                               
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),QPUB     MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ERROR,PZEPUBNF                                                   
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BNE   TRAPERR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   ELCODE,X'10'        FIND PUB NAME ELEMENT                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PUBNAMEL,R6         ESTABLISH PUBNAME ELEMENT                    
*                                                                               
*                                  SAVE PRINTED PUB ID                          
         GOTO1 VPUBEDIT,DMCB,(C'0',PUBPKPUB),(0,PUBPRNT)                        
*                                                                               
         MVC   PUBPNM,PUBNAME      SAVE PUB NAME                                
         MVC   PUBPZNM,PUBZNAME    SAVE PUB ZONE NAME                           
*                                                                               
*        FIND PZ PUB ID IN PUB TRANSLATION TABLE                                
*                                                                               
         L     R3,=V(PTRTAB)       ESTABLISH PUB TRANSLATION TABLE              
         A     R3,RELO00           RELOCATE ADDRESS                             
         USING PTRTABD,R3                                                       
*                                                                               
VPUBPZLP DS    0H                                                               
*                                                                               
         CLI   0(R3),X'FF'         ERROR IF END OF PUB REACHED                  
         BE    VPBNOTPZ                                                         
*                                                                               
         CLC   AGENCY,PTRUSCOD     MATCH ON AGENCY                              
         BNE   VPUBPZCN                                                         
*                                                                               
         CLC   QMED,PTRMED         MATCH ON MEDIA                               
         BNE   VPUBPZCN                                                         
*                                                                               
         CLC   QPUB,PTRPUB         MATCH ON FULL PUB ID                         
         BE    VPUBPZFD                                                         
*                                                                               
         CLC   QPUBPID,PTRPUB      IF SAME MASTER PUB                           
         BNE   VPUBPZCN                                                         
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,7,PTRETAB        AND THERE IS AN EDITION TABLE                
         BZ    VPUBPZCN                                                         
         A     R2,RELO00           RELOCATE ADDRESS                             
*                                                                               
VPUBETRL DS    0H                  FIND EDITION IN TABLE                        
*                                                                               
         USING ETRTABD,R2          ESTABLISH EDITION TABLE                      
*                                                                               
         CLI   0(R2),X'FF'         DONE IF END OF TABLE                         
         BE    VPUBETRD                                                         
*                                                                               
         CLC   QPUBZID,ETRZON      MATCH ON ZONE                                
         BNE   VPUBETRC                                                         
         CLC   QPUBEID,ETREDT      AND EDITION                                  
         BE    VPUBPZFD                                                         
*                                                                               
VPUBETRC DS    0H                                                               
*                                                                               
         LA    R2,ETRTABL(R2)      POINT TO NEXT EDITION TABLE ENTRY            
         B     VPUBETRL                                                         
*                                                                               
VPUBETRD DS    0H                                                               
*                                                                               
VPUBPZCN DS    0H                                                               
*                                                                               
         LA    R3,PTRTABL(R3)      BUMP TO NEXT PUB TABLE ENTRY                 
         B     VPUBPZLP                                                         
*                                                                               
VPUBPZFD DS    0H                  PUB FOUND IN TABLE                           
*                                                                               
         MVC   PUBPTRTB,0(R3)      SAVE PUB TABLE ENTRY                         
*                                                                               
VPUBX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
VPBALLER MVI   ERROR,PZEALLER      USE ' ' INSTEAD OF ALL                       
         B     VPBERRX                                                          
*                                                                               
VPBNOTPZ MVI   ERROR,PZENOTPZ      NOT AN EPIC PUB                              
         B     VPBERRX                                                          
*                                                                               
VPBERRX  DS    0H                                                               
         GOTO1 ERREX                                                            
         DROP  R6                                                               
*                                                                               
         EJECT                                                                  
* FIRST FIND WHICH ADVERTISER SYSTEM (OR TEST) WE ARE ON *                      
*                                                                               
MPLSW    NTR1                                                                   
*                                                                               
         MVC   LKEY,=H'32'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF               IF OFFLINE, CK IF MPL FILE OPEN              
         BZ    MPLSW40                                                          
         LA    R1,DMCB                                                          
         MVC   0(4,R1),=X'FEFFFFFF'    SYS=FE GETS SYSFAC ADDRESS               
         BASR  RE,RF                                                            
*                                                                               
         L     RE,0(R1)                GET SYSFAC ADDRESS                       
         L     RF,VSSB-SYSFACD(,RE)                                             
         MVC   FACPAKRS,SSBSYSN1-SSBD(RF)                                       
*                                                                               
         LA    R0,MPLTABLN                                                      
         LA    R1,MPLTAB                                                        
MPLSW20  CLC   FACPAKRS,0(R1)             SYSTEM ID - SEE SSB                   
         BE    MPLSW30                                                          
         LA    R1,MPLENTLN(,R1)           NEXT ENTRY                            
         BCT   R0,MPLSW20                                                       
         DC    H'0'                                                             
MPLSW30  MVC   DUB(7),1(R1)               SAVE SENAME                           
         L     RF,VSELIST-SYSFACD(,RE)                                          
         SR    R0,R0                                                            
         ICM   R0,3,0(RF)                                                       
         LA    R1,6(,RF)                                                        
MPLSW34  CLC   DUB(7),0(R1)        SAME SENAME                                  
         BE    MPLSW36                                                          
         AR    R1,R0                                                            
         C     R1,2(,RF)           AT END YET                                   
         BL    MPLSW34                                                          
         DC    H'0'                                                             
MPLSW36  DS    0H                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         LTR   RF,RF               IF OFFLINE, CK IF MPL FILE OPEN              
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,DMCB                                                          
         XC    0(24,R1),0(R1)                                                   
         BASR  RE,RF                                                            
         CLI   4(R1),0             ALL SWITCHES COME HERE                       
         BNE   SWERR                                                            
*                                                                               
         B     EXIT                                                             
*                                                                               
* ONLY IF OFFLINE, OPEN MPL FILE IF NOT OPEN ALREADY *                          
*                                                                               
MPLSW40  ICM   RF,15,UTL           MUST BE A UTL ADDRESS                        
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                  OPEN MPLDIR/MPLFIL                           
         CLI   MPLOPEN,C'Y'                                                     
         BE    MPLSW50                                                          
*                                                                               
         MVC   SAVSYS,4(RF)        SAVE CURRENT SYSTEM                          
*                                                                               
         L     R4,AIO1             USE AS WORK AREA FOR OPEN                    
         GOTO1 DATAMGR,DMCB,=C'OPEN',=CL7'MPL',FILELIST,(R4)                    
         MVI   MPLOPEN,C'Y'                                                     
         B     EXIT                                                             
*                                                                               
MPLSW50  DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
SWERR    CLI   4(R1),1             TEST USER NOT AUTHORIZED FOR SYSTEM          
         BE    SWERR10                                                          
         CLI   4(R1),2             TEST SYSTEM NOT OP                           
         BE    SWERR20                                                          
         DC    H'0'                                                             
SWERR10  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=C'USER NOT AUTHORIZED FOR SYSTEM'                   
         MVC   CONHEAD+31(3),0(R2)                                              
         B     SWERR30                                                          
*                                                                               
SWERR20  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(3),0(R2)                                                 
         MVC   CONHEAD+5(13),=C'SYSTEM NOT UP'                                  
         CLC   DUB(3),0(R2)        IF MPL, SHOW WHICH ONE                       
         BNE   SWERR30                                                          
         MVC   CONHEAD+3(1),DUB+3                                               
SWERR30  MVI   GENSTAT2,USMYOK                                                  
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  CLI   ERROPT,C'Y'                                                      
         BE    EXIT                                                             
         GOTO1 ERREX                                                            
*                                                                               
         DC    H'0'                                                             
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
* CONSTANTS TABLES, ETC *                                                       
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(PUBEDIT)                                                       
         DC    V(PUBVAL)                                                        
         DC    V(SRCHCALL)                                                      
         DC    V(PZTABS)                                                        
         DC    V(DUMMY)                                                         
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         EJECT                                                                  
* DIRECTORY OF PROGRAMS                                                         
*                                                                               
*        PROGRAM          ACTION            PROG SCRN                           
*                                                                               
*        AGYNAME          MAINT              01   FE                            
*                         LIST               01   EE                            
*        CLINAME          MAINT              02   FD                            
*                         LIST               02   ED                            
*        PRDNAME          MAINT              03   FC                            
*                         LIST               03   EC                            
*        OFFNAME          MAINT              04   FB                            
*                         LIST               04   EB                            
*        BATCH            MAINT              05   FA                            
*                         LIST               05   EA                            
*        INVOICE (DEFUNCT)MAINT              06   F9                            
*                         LIST               06   E9                            
*        CONVERT          MAINT              07   F8                            
*                         LIST               07   E8                            
*        ?????            MAINT              08                                 
*                         LIST               08                                 
*        CONVERT          REPORT             09   E6                            
*        BATCH            CLOSE              11   F6                            
*        STATION (EQUIV)  MAINT              12   F5                            
*                         LIST               12   E5                            
*        INVOICE          COUNTS             13   E4                            
*        STATION          COUNTS             14   E4                            
*        BATCH            MOVE               15   E7                            
*        EIXPR            BILLING            16   E3                            
*        BATCH            DUMP               17   E2                            
*        INVOICE          GENERATE           21   B1                            
*                                            22                                 
*        BILL             MAINT              23   B3                            
*                         LIST               23   A3                            
*                                                                               
*        FREE PROGRAMS    0A-0F 10 18-1F  23-2F                                 
*        FREE SCREENS     F7 F4 F3 F2 B2-BF A1-AF                               
         EJECT                                                                  
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                                                               
RECACT   DS    0D                                                               
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                                                               
AGYNAMED EQU   *+1                                                              
         DC    X'01',C'AGYNAME ',AL1(02),X'00C2'                                
CLINAME  EQU   *+1                                                              
         DC    X'01',C'CLINAME ',AL1(03),X'00C2'                                
PRDNAME  EQU   *+1                                                              
         DC    X'01',C'PRDNAME ',AL1(04),X'00C2'                                
OFFNAMED EQU   *+1                                                              
         DC    X'01',C'OFFNAME ',AL1(05),X'00C2'                                
         DC    X'01',C'BATCH   ',AL1(06),X'00C3'                                
         DC    X'01',C'INVOICE ',AL1(07),X'00C4'                                
         DC    X'01',C'CONVERT ',AL1(08),X'00C4'                                
STAEQUIV EQU   *+1                                                              
         DC    X'01',C'STATION ',AL1(09),X'00C2'                                
         DC    X'01',C'EIEXPR  ',AL1(10),X'00C2'                                
BILLREC  EQU   *+1                                                              
         DC    X'01',C'BILL    ',AL1(11),X'00C2'                                
         DC    X'01',C'DETAILS ',AL1(12),X'00C4'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
         DC    X'01',C'CPP     ',AL1(13),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'CLOSE   ',AL1(11,11,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'HELP    ',AL1(00,00,00)                                  
         DC    X'02',C'GENERATE',AL1(13,13,00)                                  
         DC    X'02',C'COUNTS  ',AL1(14,14,00)                                  
         DC    X'02',C'MOVE    ',AL1(15,15,00)                                  
         DC    X'02',C'BILL    ',AL1(16,16,00)                                  
         DC    X'02',C'DUMP    ',AL1(17,17,00)                                  
         EJECT                                                                  
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                                                               
         DC    X'03',AL1(02,01),X'FE01000080',C'    '  AGYN     MAINT           
         DC    X'03',AL1(02,10),X'EE010000C0',C'    '           LIST            
         DC    X'03',AL1(03,01),X'FD02000080',C'    '  CLIN     MAINT           
         DC    X'03',AL1(03,10),X'ED020000C0',C'    '           LIST            
         DC    X'03',AL1(04,01),X'FC03000080',C'    '  PRDN     MAINT           
         DC    X'03',AL1(04,10),X'EC030000C0',C'    '           LIST            
         DC    X'03',AL1(05,01),X'FB04000080',C'    '  OFFN     MAINT           
         DC    X'03',AL1(05,10),X'EB040000C0',C'    '           LIST            
         DC    X'03',AL1(06,01),X'FA05000080',C'    '  BTCH     MAINT           
         DC    X'03',AL1(06,10),X'EA050000C0',C'    '           LIST            
         DC    X'03',AL1(06,15),X'E715000080',C'    '           MOVE            
         DC    X'03',AL1(06,11),X'F611000058',C'    '           CLOSE           
*        DC    X'03',AL1(07,10),X'E9060000C0',C'    '  INVOICE  LIST            
*        DC    X'03',AL1(07,01),X'F906000080',C'    '           MAINT           
         DC    X'03',AL1(07,13),X'B121002118',C'Z8Z8'           GENER           
         DC    X'03',AL1(07,14),X'E413001378',C'ZIZI'           COUNTS          
*        DC    X'03',AL1(07,12),X'E609000938',C'Z9Z9'           REPORT          
         DC    X'03',AL1(08,10),X'E8180000C0',C'    '  CONVERT  LIST            
         DC    X'03',AL1(08,01),X'F807000080',C'    '           MAINT           
         DC    X'03',AL1(08,12),X'E609000938',C'Z9Z9'           REPORT          
         DC    X'03',AL1(09,10),X'E5120000C0',C'    '  STA EQU  LIST            
         DC    X'03',AL1(09,01),X'F512000080',C'    '           MAINT           
         DC    X'03',AL1(09,14),X'E414001478',C'ZCZC'  STATION  COUNTS          
         DC    X'03',AL1(10,16),X'E316001618',C'ZBZB'  EIEXPR   BILLING         
         DC    X'03',AL1(06,17),X'E217001718',C'ZDZD'  BATCH    DUMP            
         DC    X'03',AL1(11,01),X'B323000080',C'    '  BILL     MAINT           
         DC    X'03',AL1(11,10),X'A3230000F8',C'    '           LIST            
         DC    X'03',AL1(12,10),X'E8180000C0',C'    '  DETAILS  LIST            
         DC    X'03',AL1(12,01),X'D808000080',C'    '           MAINT           
         DC    X'03',AL1(12,12),X'E609000978',C'Z9Z9'           REPORT          
         DC    X'03',AL1(13,12),X'FF99009918',C'9999'           REPORT          
         DC    X'FF'                                                            
*                                                                               
*        SECOND RECORD/ACTION TABLE FOR INSERTION DETAIL PROCESSING             
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                                                               
RECACT3  DS    0X                                                               
         DC    X'03',AL1(02,01),X'FE01000080',C'    '  AGYN     MAINT           
         DC    X'03',AL1(02,10),X'EE010000C0',C'    '           LIST            
         DC    X'03',AL1(03,01),X'FD02000080',C'    '  CLIN     MAINT           
         DC    X'03',AL1(03,10),X'ED020000C0',C'    '           LIST            
         DC    X'03',AL1(04,01),X'FC03000080',C'    '  PRDN     MAINT           
         DC    X'03',AL1(04,10),X'EC030000C0',C'    '           LIST            
         DC    X'03',AL1(05,01),X'FB04000080',C'    '  OFFN     MAINT           
         DC    X'03',AL1(05,10),X'EB040000C0',C'    '           LIST            
         DC    X'03',AL1(06,01),X'FA05000080',C'    '  BTCH     MAINT           
         DC    X'03',AL1(06,10),X'EA050000C0',C'    '           LIST            
         DC    X'03',AL1(06,15),X'E715000080',C'    '           MOVE            
         DC    X'03',AL1(06,11),X'F611000058',C'    '           CLOSE           
*****    DC    X'03',AL1(07,10),X'E9060000C0',C'    '  INVOICE  LIST            
*****    DC    X'03',AL1(07,01),X'F906000080',C'    '           MAINT           
         DC    X'03',AL1(07,13),X'B121002118',C'Z8Z8'           GENER           
         DC    X'03',AL1(07,14),X'E413001378',C'ZIZI'           COUNTS          
*****    DC    X'03',AL1(07,12),X'E609000978',C'Z9Z9'           REPORT          
         DC    X'03',AL1(08,10),X'E8180000C0',C'    '  CONVERT  LIST            
         DC    X'03',AL1(08,01),X'D808000080',C'    '           MAINT           
         DC    X'03',AL1(08,12),X'E609000978',C'Z9Z9'           REPORT          
*****    DC    X'03',AL1(09,10),X'E5120000C0',C'    '  STA EQU  LIST            
*****    DC    X'03',AL1(09,01),X'F512000080',C'    '           MAINT           
         DC    X'03',AL1(09,14),X'E414001478',C'ZCZC'  STATION  COUNTS          
         DC    X'03',AL1(10,16),X'E316001618',C'ZBZB'  EIEXPR   BILLING         
         DC    X'03',AL1(06,17),X'E217001718',C'ZDZD'  BATCH    DUMP            
         DC    X'03',AL1(11,01),X'B323000080',C'    '  BILL     MAINT           
         DC    X'03',AL1(11,10),X'A3230000F8',C'    '           LIST            
         DC    X'03',AL1(12,10),X'E8180000C0',C'    '  DETAILS  LIST            
         DC    X'03',AL1(12,01),X'D808000080',C'    '           MAINT           
         DC    X'03',AL1(12,12),X'E609000978',C'Z9Z9'           REPORT          
         DC    X'03',AL1(13,12),X'FF99009918',C'9999'           REPORT          
         DC    X'FF'                                                            
FILELIST DS    0D                                                               
         DC    CL8'UMPLDIR'                                                     
         DC    CL8'UMPLFIL'                                                     
         DC    CL10'X'                                                          
FILELST  DS    0D                                                               
         DC    CL8'UGENDIR'                                                     
         DC    CL8'UGENFIL'                                                     
         DC    CL10'X'                                                          
MPLOPEN  DC    X'00'                                                            
CTLOPEN  DC    X'00'                                                            
SAVSYS   DC    X'00'                                                            
MEDTAB   DC    CL11'MMAGAZINE'                                                  
         DC    CL11'NNEWSPAPER'                                                 
         DC    CL11'OOUTDOOR'                                                   
         DC    CL11'SSUPPLEMENT'                                                
         DC    CL11'TTRADE'                                                     
         DC    CL11' ??'                                                        
MEDTABL  EQU   11                                                               
MEDTABN  EQU   (*-MEDTAB)/MEDTABL                                               
         DC    X'FF',CL10'??'                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
CONSECTL EQU   X'0A'                                                            
MPLTAB   DC    C'1',CL7'MPL1'      ADV1 USE MPL1                                
         DC    C'2',CL7'MPL2'               MPL2                                
         DC    C'3',CL7'MPL3'               MPL3                                
         DC    C'4',CL7'MPL4'               MPL4                                
         DC    C'5',CL7'MPL5'               MPL5                                
         DC    C'6',CL7'MPL6'               MPL6                                
         DC    C'R',CL7'MPLQ'               MPLQ                                
         DC    C'T',CL7'MPL1'      TST USE MPL1 ALSO                            
         DC    C'M',CL7'MPL1'      MEL USE MPL1 ALSO                            
MPLENTLN EQU   8                                                                
MPLTABLN EQU   (*-MPLTAB)/8                                                     
*                                                                               
         DROP  R7                                                               
*                                                                               
         TITLE 'PPEZF00 - VALIDTE FILTERS  - VFTR'                              
***********************************************************************         
*                                                                     *         
*        VALIDATE FILTERS (IF ANY)                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0H                                                               
VFTR     NMOD1 0,**VFTR**                                                       
*                                                                               
         L     RC,SVRC             RE-ESTALISH WORKING STORAGE                  
         USING GEND,RC                                                          
*                                                                               
         XC    FILTERS,FILTERS     INIT FILTERS SAVEAREA                        
         XC    SVFLTRS,SVFLTRS     INIT FILTERS SAVEAREA                        
         MVI   SVFLTRSL,0          INIT LENGTH OF FILTERS SAVEAREA              
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VFTRX               NO                                           
*                                                                               
         TM    WHENOK,X'78'        ONLY ALLOWED ONLINE                          
         BM    *+8                                                              
         B     VFTEREPE                                                         
*                                                                               
*        INITIALIZE DDVAL PARAMETERS                                            
*                                                                               
         MVC   DMCB+4(4),=X'D9000A40'                                           
         GOTO1 CALLOV,DMCB,0       LOAD PHASE T00A40 - DDVAL                    
*                                                                               
         CLI   DMCB+4,X'FF'        MUST FIND DDVAL                              
         BNE   *+6                                                              
         DC    H'0'                WHERE IS DDVAL                               
*                                                                               
         L     RF,DMCB             SAVE A(VDDVAL)                               
         ST    RF,VDDVAL                                                        
*                                                                               
         XC    VLBLOCK(VLBLOCKL),VLBLOCK CLEAR DDVAL CONTROL BLOCK              
*                                                                               
         MVC   VLACFACS,ACOMFACS   A(COMFACS)                                   
         MVC   VLCTRY,CTRY         SET COUNTRY CODE                             
         MVC   VLLANG,LANG         LANGUAGE CODE                                
         MVC   VLAGENCY,TWAAGY     AGENCY                                       
         MVI   VLSYSTEM,VLSYSPRQ   PRINT SYSTEM                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,VLTODAYC) GET TODAY'S DATE                  
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BE    VFTRHLP                                                          
*                                                                               
         MVC   SVFLTRS,SPACES      INIT FILTERS SAVEAREA                        
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVFLTRS(0),8(R2)    SAVE ENTERED FILTERS                         
*                                                                               
         MVC   SVFLTRSL,5(R2)      SAVE LENGTH OF ENTERED FILTERS               
*                                                                               
         LA    R0,25               NON-STANDARD LENGTH                          
         MVI   BYTE,1                                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
*                                                                               
         GOTO1 SCANNER,DMCB,((R0),(R2)),(5,(R4)),0                              
         CLI   4(R1),0                                                          
         BE    VFTEMISS             SCANNER DIDN'T FIND ANYTHING                
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,DMCB+4         GET NUMBER OF BLOCKS                         
*                                                                               
*        VALIDATE FILTERS                                                       
*                                                                               
VFTRLOOP DS    0H                                                               
*                                                                               
*        CHECK FOR HELP REQUEST                                                 
*                                                                               
         CLI   12(R4),C'+'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   12(R4),C'?'         IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   13(R4),C'?'         IF HELP REQUESTED                            
         BNE   VFTRHLPX                                                         
*                                                                               
         LA    R6,HELPCBLK         ESTABLISH HELP CONTROL BLOCK                 
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         L     RF,=A(HELPSAVE-(CONHEADH-64))                                    
         LA    RF,0(RF,RA)                                                      
         ST    RF,HCBASAVE         PASS ADDRESS                                 
*                                                                               
         MVC   HCBMTIC,=AL2(PRQHLPMN)  USE PRINT HELP MENUS                     
         MVI   HCBSCRN,X'D1'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN SECOND TWA PAGE                      
         ST    RA,HCBATWA          SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
*                                                                               
         L     R1,SYSPARMS         POINT TO TIOBD                               
         MVC   HCBATIOB,0(R1)                                                   
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,4           SET SYSTEN AS PRINT                          
         MVI   HCBSEGS,10          DDVAL TABLE IS 10*256 BYTES LONG             
         MVC   HCBATAB,WRKFBUFA    USE BUFFER AREA FOR TABLE                    
*                                                                               
         GOTO1 VPRHELP,DMCB,=AL2(PRQHMPZF),(R2),HELPCBLK PUT OUT HELP           
*                                                                               
         NOP   VFTRHLPX            FOR TESTING                                  
         DROP  R6                                                               
*                                                                               
VFTRHLPX DS    0H                                                               
*                                                                               
         ZIC   R1,0(R4)            GET LENGTH                                   
*                                                                               
         CH    R1,=H'2'                                                         
         BL    VFTEFTLN                                                         
*                                                                               
*        SAVE ANY '+' OR '-' MODIFIERS                                          
*                                                                               
         LA    R5,12-1(R1,R4)      POINT TO LAST CHAR FOUND                     
*                                                                               
         CLI   0(R5),C'+'          PLUS                                         
         BE    *+8                  YES, SAVE IT                                
         CLI   0(R5),C'-'          MINUS                                        
         BNE   *+12                 NO, NEITHER                                 
         MVC   HOLDSIGN,0(R5)                                                   
         BCTR  R1,0                -1 MORE FOR GREATER/LESS THAN SIGN           
*                                                                               
*        VALIDATE FIELD ENTRY                                                   
*                                                                               
         LR    R0,R1               INPUT LENGTH                                 
         LH    R7,=Y(WVLTAB-SYSD)  GET A(TABLE BUILD AREA)                      
         LA    R7,SYSD(R7)                                                      
*                                                                               
         GOTO1 VDDVAL,VLPARMS,('VLPVALQ',=Y(PRQPZFTR)),                X        
               ((R0),12(R4)),(R7),0,0,0                                         
         CLI   VLPERR,0                                                         
         BNE   VFTEFTNV            INVALID FILTER                               
*                                                                               
         USING VLTABD,R7           ESTABLISH RETURNED TABLE ENTRY               
*                                                                               
*        ACTIVITY DATE                                                          
*                                                                               
VFTRACT  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFACT)   ACTIVITY DATE                            
         BNE   VFTRACTN                                                         
*                                                                               
         LA    R5,22(,R4)                                                       
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
*                                                                               
         OC    DMCB(4),DMCB        WAS DATE VALID                               
         BZ    VFTEDTNV             NO                                          
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(1,FTRDATE)                                 
*                                                                               
         MVC   FTRDATES,HOLDSIGN                                                
*                                                                               
         B     VFTRLPCN                                                         
         EJECT                                                                  
*                                                                               
VFTRACTN DS    0H                                                               
*                                                                               
*        CLIENT CODE FILTER 'CC='                                               
*              'X' MEANS ALL INVOICES WITHOUT OVERRIDE                          
*                  OR LOOKED UP CLIENT CODE                                     
*              ELSE REPRESENTS CLIENT CODE                                      
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*              C'ALL' TREATED AS '-X' INTERNALLY                                
*                                                                               
VFTRCC   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFCC)   CLIENT CODE (CC)                          
         BNE   VFTRCCN             NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VFTECCLN               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF CLIENT CODE                         
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VFTECCLN               NO CODE ENTERED                           
*                                                                               
         CH    RF,=H'1'            IF ENTRY IS 1 LONG                           
         BNE   VFTRCC7                                                          
*                                                                               
         CLI   0(R1),C'X'             IT MUST BE C'X' FOR MISSING CODES         
         BNE   VFTECCLN                                                         
*                                                                               
         MVC   FTRQCLT,=X'FFFFFF'     SET SPECIAL CODE                          
*                                                                               
         B     VFTRCC9                                                          
*                                                                               
VFTRCC7  DS    0H                                                               
*                                                                               
         CH    RF,=H'2'            CLIENT CODE MUST BE 2 OR 3 LONG              
         BL    VFTECCLN                                                         
         CH    RF,=H'3'                                                         
         BH    VFTECCLN                                                         
*                                                                               
         MVC   FTRQCLT,0(R1)       SAVE CLIENT CODE                             
*                                                                               
         CLC   FTRQCLT,=C'ALL'     IF 'ALL' CLIENT CODES                        
         BNE   *+14                                                             
         MVC   FTRQCLT,=X'FFFFFF'     TREAT AS '-X'                             
         MVI   FULL,C'-'                                                        
*                                                                               
         B     VFTRCC9                                                          
*                                                                               
VFTRCC9 DS     0H                                                               
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    FTRQCLT,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VFTRCCX  DS    0H                                                               
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRCCN  DS    0H                                                               
*                                                                               
*        CLIENT NAME FILTER 'CN='                                               
*              'X' MEANS ALL INVOICES WITHOUT OVERRIDE                          
*                  OR LOOKED UP                                                 
*                  OR SOURCE    CLIENT NAME                                     
*              ELSE REPRESENTS  CLIENT NAME                                     
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*        COMPARES MATCH ON LENGTH OF FILTER. AN EXACT MATCH IS NOT              
*              NECESSARY                                                        
*                                                                               
VFTRCN   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFCN)   CLIENT NAME (CN)                          
         BNE   VFTRCNN             NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VFTECNLN               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF CLIENT NAME                         
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VFTECNLN               NO NAME ENTERED                           
*                                                                               
         CH    RF,=H'1'            IF ENTRY IS 1 LONG                           
         BNE   VFTRCN7                                                          
*                                                                               
         CLI   0(R1),C'X'             IT MAY BE C'X' FOR MISSING NAMES          
         BNE   VFTRCN7                                                          
*                                                                               
         MVI   FTRCLTN,X'FF'          SET SPECIAL NAME                          
         MVI   FTRCLTNL,0             SET SPECIAL NAME EXECUTE LENGTH           
*                                                                               
         B     VFTRCN9                                                          
*                                                                               
VFTRCN7  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FTRCLTN(0),0(R1)    SAVE CLIENT NAME                             
*                                                                               
         STC   RF,FTRCLTNL         SAVE CLIENT NAME EXECUTE LENGTH              
*                                                                               
         B     VFTRCN9                                                          
*                                                                               
VFTRCN9 DS     0H                                                               
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    FTRCLTN,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VFTRCNX  DS    0H                                                               
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRCNN  DS    0H                                                               
*                                                                               
*        PRODUCT CODE FILTER 'PCODE='                                           
*              'X' MEANS ALL INVOICES WITHOUT OVERRIDE                          
*                  OR LOOKED UP PRODUCT CODE                                    
*              ELSE REPRESENTS PRODUCT CODE                                     
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*              C'ALL' TREATED AS '-X' INTERNALLY                                
*                                                                               
VFTRPC   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFPC)   PRODUCT CODE (PC)                         
         BNE   VFTRPCN             NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VFTEPCLN               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF PRODUCT CODE                        
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VFTEPCLN               NO CODE ENTERED                           
*                                                                               
         CH    RF,=H'1'            IF ENTRY IS 1 LONG                           
         BNE   VFTRPC7                                                          
*                                                                               
         CLI   0(R1),C'X'             IT MUST BE C'X' FOR MISSING CODES         
         BNE   VFTEPCLN                                                         
*                                                                               
         MVC   FTRQPRD,=X'FFFFFF'     SET SPECIAL CODE                          
*                                                                               
         B     VFTRPC9                                                          
*                                                                               
VFTRPC7  DS    0H                                                               
*                                                                               
         CH    RF,=H'2'            PRODUCT CODE MUST BE 2 OR 3 LONG             
         BL    VFTEPCLN                                                         
         CH    RF,=H'3'                                                         
         BH    VFTEPCLN                                                         
*                                                                               
         MVC   FTRQPRD,0(R1)       SAVE PRODUCT CODE                            
*                                                                               
         CLC   FTRQPRD,=C'ALL'     IF 'ALL' PRODUCT CODES                       
         BNE   *+14                                                             
         MVC   FTRQPRD,=X'FFFFFF'     TREAT AS '-X'                             
         MVI   FULL,C'-'                                                        
*                                                                               
         B     VFTRPC9                                                          
*                                                                               
VFTRPC9 DS     0H                                                               
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    FTRQPRD,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VFTRPCX  DS    0H                                                               
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRPCN  DS    0H                                                               
*                                                                               
*        PRODUCT NAME FILTER 'PN='                                              
*              'X' MEANS ALL INVOICES WITHOUT OVERRIDE                          
*                  OR LOOKED UP                                                 
*                  OR SOURCE    PRODUCT NAME                                    
*              ELSE REPRESENTS PRODUCT NAME                                     
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*        COMPARES MATCH ON LENGTH OF FILTER. AN EXACT MATCH IS NOT              
*              NECESSARY                                                        
*                                                                               
VFTRPN   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFPN)   PRODUCT NAME (PN)                         
         BNE   VFTRPNN             NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VFTEPNLN               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF PRODUCT NAME                        
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VFTEPNLN               NO NAME ENTERED                           
*                                                                               
         CH    RF,=H'1'            IF ENTRY IS 1 LONG                           
         BNE   VFTRPN7                                                          
*                                                                               
         CLI   0(R1),C'X'             IT MAY BE C'X' FOR MISSING NAMES          
         BNE   VFTRPN7                                                          
*                                                                               
         MVI   FTRPRDN,X'FF'          SET SPECIAL NAME                          
         MVI   FTRPRDNL,0             SET SPECIAL NAME EXECUTE LENGTH           
*                                                                               
         B     VFTRPN9                                                          
*                                                                               
VFTRPN7  DS    0H                                                               
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FTRPRDN(0),0(R1)    SAVE PRODUCT NAME                            
*                                                                               
         STC   RF,FTRPRDNL         SAVE PRODUCT NAME EXECUTE LENGTH             
*                                                                               
         B     VFTRPN9                                                          
*                                                                               
VFTRPN9 DS     0H                                                               
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    FTRPRDN,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VFTRPNX  DS    0H                                                               
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRPNN  DS    0H                                                               
*                                                                               
*        CONVERTED INVOICE FILTER 'CONV'                                        
*              REPORTS ON ALL CONVERTED INVOICES                                
*                                                                               
*        'UNCONV' FILTER MUST NOT BE ON AT THE SAME TIME                        
*                                                                               
VFTRCV   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFCNV)   CONVERTED (CONV)                         
         BNE   VFTRCVN                                                          
*                                                                               
         TM    FTRFLAG,FTRUCVQ     'UNCONV' NOT ALLOWED AT SAME TIME            
         BNZ   VFTEFTLN                                                         
*                                                                               
         OI    FTRFLAG,FTRCVQ                                                   
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRCVN  DS    0H                                                               
*                                                                               
*        RE-CONVERTED INVOICE FILTER 'RECONV'                                   
*              REPORTS ON ALL RE-CONVERTED INVOICES                             
*                                                                               
VFTRRCV  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFRCV)   RE-CONVERTS (RECONV)                     
         BNE   VFTRRCVN                                                         
*                                                                               
         OI    FTRFLAG,FTRRCVQ                                                  
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRRCVN DS    0H                                                               
*                                                                               
*        UNCONVERTED INVOICE FILTER 'UNCONV'                                    
*              REPORTS ON ALL UNCONVERTED INVOICES                              
*                                                                               
*        'CONV' FILTER MUST NOT BE ON AT THE SAME TIME                          
*                                                                               
VFTRUCV  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFUNC)   UNCONVERTED (UNCONV)                     
         BNE   VFTRUCVN                                                         
*                                                                               
         TM    FTRFLAG,FTRCVQ      'CONV' NOT AT SAME TIME                      
         BNZ   VFTEFTLN                                                         
*                                                                               
         OI    FTRFLAG,FTRUCVQ                                                  
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRUCVN DS    0H                                                               
*                                                                               
*        OVERRIDES INVOICE FILTER 'OVERRIDE'                                    
*              REPORTS ON ALL INVOICES WITH OVERRIDES                           
*                                                                               
VFTROVR  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFOVR)   OVERRIDDEN  (OVER)                       
         BNE   VFTROVRN                                                         
*                                                                               
         OI    FTRFLAG,FTROVR                                                   
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTROVRN DS    0H                                                               
*                                                                               
*        INVOICE FILTER 'INVOICE='                                              
*              REPRESENTS INVOICE                                               
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
*        COMPARES MATCH ON LENGTH OF FILTER. AN EXACT MATCH IS NOT              
*              NECESSARY                                                        
*                                                                               
VFTRINV  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFINV)   INVOICE=    (INV=)                       
         BNE   VFTRINVN            NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VFTEINVL               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF INVOICE                             
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VFTEINVL               NO NAME ENTERED                           
*                                                                               
         CH    RF,=H'10'           MAX 10 LONG                                  
         BH    VFTEINVL                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FTRINVNO(0),0(R1)    SAVE INVOICE                                
*                                                                               
         STC   RF,FTRINVLN         SAVE INVOICE EXECUTE LENGTH                  
*                                                                               
         B     VFTRINV9                                                         
*                                                                               
VFTRINV9 DS    0H                                                               
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    FTRINVNO,X'FF'-X'40'      MAKE FIRST LETTER LOWERCASE            
*                                                                               
VFTRINVX DS    0H                                                               
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRINVN DS    0H                                                               
*                                                                               
*                                                                               
*        SOURCE FILTER 'SOURCE='                                                
*              REPRESENTS SOURCE                                                
*                                                                               
*              IF IT STARTS WITH '-' TREATED AS NEGATIVE FILTER                 
*                                                                               
VFTRSRC  DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFSRC)   SOURCE=    (SRC=)                        
         BNE   VFTRSRCN            NO                                           
*                                                                               
         XC    FULL,FULL           INIT WORKAREA                                
         SR    RF,RF                                                            
*                                                                               
         ICM   RF,1,1(R4)          ENTRY LENGTH                                 
         BZ    VFTESRCL               MUST HAVE ONE                             
*                                                                               
         LA    R1,22(R4)           START OF SOURCE                              
*                                                                               
         CLI   0(R1),C'-'          IF NEGATIVE FILTER                           
         BNE   *+20                                                             
         MVI   FULL,C'-'              SET INDICATOR                             
         LA    R1,1(R1)               BUMP VALUE POINTER                        
         BCT   RF,*+8                 DECREMENT LENGTH                          
         B     VFTESRCL               NO NAME ENTERED                           
*                                                                               
         CH    RF,=H'4'            MAX 4 LONG                                   
         BH    VFTESRCL                                                         
*                                                                               
         MVC   FTRSRCE,0(R1)       SAVE SOURCE                                  
*                                                                               
         CLI   FULL,C'-'              IF NEGATIVE FILTER                        
         BNE   *+8                                                              
         NI    FTRSRCE,X'FF'-X'40'       MAKE FIRST LETTER LOWERCASE            
*                                                                               
VFTRSRCX DS    0H                                                               
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTRSRCN DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFDNE)  DONE (MEANS CONV/DEL INC KEEP)            
         BNE   VFTR60                                                           
*                                                                               
         OI    FTRFLAG,FTRDONE                                                  
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTR60   DS    0H                                                               
*                                                                               
         CLC   VLTICODE,=Y(PRQZFBAT)  BATCH DATES                               
         BNE   VFTR66                                                           
*                                                                               
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
*                                                                               
         ICM   RE,15,DMCB          WAS DATE VALID                               
         BZ    VFTEDTNV             NO                                          
*                                                                               
         MVC   WORK+6(6),WORK                                                   
*                                                                               
         CLM   RE,1,1(R4)          WAS ONLY 1 DATE ENTERED                      
         BE    VFTR64                                                           
*                                                                               
         LA    R5,1(RE,R5)                                                      
*                                                                               
         GOTO1 (RF),(R1),(0,(R5)),WORK+6                                        
*                                                                               
         OC    DMCB,DMCB                                                        
         BZ    VFTEDTNV                                                         
*                                                                               
VFTR64   GOTO1 DATCON,(R1),(0,WORK),(2,FTRBSDT)                                 
         GOTO1 (RF),(R1),(0,WORK+6),(2,FTRBEDT)                                 
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTR66   DS    0H                                                               
         CLC   VLTICODE,=Y(PRQZFMED)  MEDIA                                     
         BNE   VFTR70                                                           
*                                                                               
         LA    R5,22(,R4)                                                       
*                                                                               
         CLI   0(R5),C'M'          MAGAZINE                                     
         BE    VFTR68                                                           
         CLI   0(R5),C'N'          NEWSSPAPER                                   
         BE    VFTR68                                                           
         CLI   0(R5),C'O'          OUTDOOR                                      
         BE    VFTR68                                                           
         CLI   0(R5),C'S'          SUPPLEMENTS                                  
         BE    VFTR68                                                           
         CLI   0(R5),C'T'          TRADE                                        
         BNE   VFTEMDNV                                                         
*                                                                               
VFTR68   MVC   FTRMEDIA,0(R5)                                                   
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTR70   DS    0H                                                               
         CLC   VLTICODE,=Y(PRQZFTRA)  TRACE                                     
         BNE   VFTR72                                                           
*                                                                               
         OI    FTRFLAG,FTRDEL                                                   
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTR72   DS    0H                                                               
         CLC   VLTICODE,=Y(PRQZFDEL)  DELETED                                   
         BNE   VFTR74                                                           
*                                                                               
         OI    FTRFLAG,FTRDEL                                                   
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTR74   DS    0H                                                               
         CLC   VLTICODE,=Y(PRQZFUSR)  USER ID                                   
         BNE   VFTR80                                                           
*                                                                               
         CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
         BNE   VFTRHLP                                                          
*                                                                               
         CLI   1(R4),8                                                          
         BH    VFTEUSRL                                                         
*                                                                               
         MVC   FUID,22(R4)                                                      
*                                                                               
         MVI   FUIDNUM,X'FF'       ALL IDS                                      
*                                                                               
         CLC   FUID(3),=C'ALL'                                                  
         BE    VFTRLPCN                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'I'                                                         
         MVC   KEY+15(10),SPACES                                                
         ZIC   RE,1(R4)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   KEY+15(0),22(R4)                                                 
*                                                                               
         L     R6,AIO1                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,(R6)                    
         CLI   8(R1),0                                                          
         BNE   VFTEUSER                                                         
*                                                                               
         MVI   ELCODE,X'02'                                                     
         LA    R6,28(,R6)                                                       
         BAS   RE,VFNEXTEL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FUIDNUM,2(R6)       BINARY USER ID (ORIGIN)                      
*                                                                               
         B     VFTRLPCN                                                         
*                                                                               
VFTR80   DS    0H                                                               
         CLC   VLTICODE,=Y(PRQZFMOS)  MONTH OF SERVICE                          
         BNE   VFTEFTNV                                                         
*                                                                               
         LA    R5,22(,R4)                                                       
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   VFTEMSNV             YES, ERROR                                  
*                                                                               
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    VFTEMSNV             NO, ERROR                                   
         GOTO1 DATCON,(R1),(0,WORK),(6,FTRMOS)                                  
*                                                                               
VFTRLPCN ZIC   RE,BYTE             UP FIELD NUMBER CTR                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,47(R4)           NOTE- NON-PUBNDARD LENGTH                    
         MVI   HOLDSIGN,0          RESET GREATER/LESS THAN SIGN                 
*                                                                               
         BCT   R3,VFTRLOOP         FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VFTRX    XIT1                                                                   
*                                                                               
VFNEXTEL CLI   0(R6),0                                                          
         BE    VFNEXTX                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    R6,RF                                                            
*                                                                               
VFNEXT2  CLC   ELCODE,0(R6)                                                     
         BER   RE                                                               
*                                                                               
         B     VFNEXTEL                                                         
*                                                                               
VFNEXTX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
VFTEREPE MVI   ERROR,PZEREPER      REPORT CAN'T HAVE FILTERS                    
         B     VFTERRX                                                          
VFTEUSER MVI   ERROR,PZEUSRID      INVALID USER ID                              
         B     VFTERRX                                                          
VFTEUSRL MVI   ERROR,PZEUSRLN      USER ID AT MOST 4 CHARACTERS                 
         B     VFTERRX                                                          
VFTEMDNV MVI   ERROR,PZEMEDNV      INVALID MEDIA                                
         B     VFTERRX                                                          
VFTEFTLN MVI   ERROR,PZEFTRLN      FILTER CODE MUST BE AT LEAST 2 LONG          
         B     VFTERRX                                                          
VFTECCLN MVI   ERROR,PZECCLN       CLIENT CODE LENGTH ERROR                     
         B     VFTERRX                                                          
VFTECNLN MVI   ERROR,PZECNLN       CLIENT NAME LENGTH ERROR                     
         B     VFTERRX                                                          
VFTEPCLN MVI   ERROR,PZEPCLN       PRODUCT CODE LENGTH ERROR                    
         B     VFTERRX                                                          
VFTEPNLN MVI   ERROR,PZEPNLN       PRODUCT NAME LENGTH ERROR                    
         B     VFTERRX                                                          
VFTEINVL MVI   ERROR,PZEINVLN      INVOICE LENGTH ERROR                         
         B     VFTERRX                                                          
VFTESRCL MVI   ERROR,PZESRCLN      SOURCE LENGTH ERROR                          
         B     VFTERRX                                                          
VFTEMSNV MVI   ERROR,PZEMOSNV      INVALID MONTH OF SERVICE                     
         B     VFTERRX                                                          
VFTEMISS MVI   ERROR,MISSING                                                    
         B     VFTERRX                                                          
VFTEFTNV MVI   ERROR,PZEFTRNV      INVALID FILTER CODE                          
         B     VFTERRX                                                          
VFTEDTNV MVI   ERROR,INVDATE                                                    
         B     VFTERRX                                                          
*                                                                               
VFTERRX  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
VFTRHLP  LA    R1,FTRHELP                                                       
VFTRERX  XC    CONHEAD,CONHEAD                                                  
         BCTR  R1,0                BACK UP TO LENGTH BYTE                       
         CLI   0(R1),L'CONHEAD-1                                                
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,0(R1)                                                         
         EX    RF,*+8                                                           
         B     VFTRERY                                                          
         MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
VFTRERY  GOTO1 ERREX2                                                           
*                                                                               
         DC    AL1(L'FTRHELP-1)                                                 
FTRHELP  DC    CL60'FILTERS=ACT/CC/CN/PC/PN/CONV/RECONV/INV/SOURCE/DEL/C        
               UNCONV *'                                                        
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
       ++INCLUDE PPEZFWORKD                                                     
       ++INCLUDE PPEZFCNVWD                                                     
         EJECT                                                                  
* PZBLOCK                                                                       
         PRINT OFF                                                              
       ++INCLUDE PZBLOCK                                                        
         PRINT ON                                                               
* PZGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PZGLOBEQUS                                                     
         PRINT ON                                                               
       ++INCLUDE PPEZFFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PPEZFE8D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PPEZFD8D                                                       
*                                                                               
         ORG   CONHEADH-64                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         ORG   CONHEADH-64+X'2000'                                              
HELPSAVE DS    XL512                                                            
IOA4     DS    XL4096                                                           
*                                                                               
         SPACE 2                                                                
* PPSRCHPARM                                                                    
* DDCOMFACS/DDSPOOLD/DDSPLWORKD/DDCNTRL/PCLTREC                                 
* PPRDREC/PAGYREC/PUBREC/SPGENEZ/CTGENFILE                                      
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE PPSRCHPARM                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE DDCNTRL                                                        
         EJECT                                                                  
       ++INCLUDE DMWRKFK                                                        
         EJECT                                                                  
         PRINT ON                                                               
*PRHELPCB                                                                       
         PRINT OFF                                                              
       ++INCLUDE PRHELPCB                                                       
         PRINT ON                                                               
*PRVALTABD                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRVALTABD                                                      
         PRINT ON                                                               
*PRGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
*PRGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGLOBEQUS                                                     
         PRINT ON                                                               
* EPIC FILE DSECTS WORKER INDEX/AGENCY OFFICE/CLIENT/PRODUCT                    
         PRINT OFF                                                              
       ++INCLUDE PPGENPZ                                                        
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDOFFICED                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDOFFICED                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'092PPEZF00   05/01/02'                                      
         END                                                                    
