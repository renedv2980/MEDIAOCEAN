*          DATA SET NEWRI00    AT LEVEL 086 AS OF 03/14/18                      
*PHASE T32000B,+0                                                               
*INCLUDE GETPROF                                                                
*INCLUDE KHDUMMY                                                                
         TITLE 'T32000 - NETWORK DRIVER REPORT CONTROLLER'                      
         SPACE 3                                                                
* ORGANIZATION OF MEMORY: USED BY ENTIRE NETWORK SYSTEM                         
*                                                                               
*      22904 BYTES                     TWA                                      
*      ---------------                ---------------                           
* R8-> ' SPOOLD      '           RA-> ' SYSTEM TWA  '                           
*      ' (3144 BYTES)'                '             '                           
*      '-------------'                '-------------'                           
* RC-> ' GEND        '                ' APPLICATION '                           
*      ' (2792 BYTES)'                '        TWA  '                           
*      '-------------'                '-------------'                           
*      ' IO AREA     '                                                          
*      ' (6000 BYTES)'                                                          
*      '-------------'                '-------------'                           
*      ' IO AREA     '                                                          
*      ' (6000 BYTES)'                                                          
*      '-------------'                                                          
* R9-> ' NETSYSD     '                                                          
*      '  (424 BYTES)'                                                          
*      '-------------'                                                          
*      ' NETBLOCK    '                                                          
*      ' (1024 BYTES)'                                                          
*      '-------------'                                                          
*      ' DRIVEBLOCK  '                                                          
*      ' (1376 BYTES)'                                                          
*      '-------------'                                                          
*      ' DRONEBLOCK  '                                                          
*      ' (580 BYTES) '                                                          
*      '-------------'                                                          
*      ' STATION LST '                                                          
*      '  2008 BYTES '                                                          
*      '-------------'                                                          
*      ' APPLICATION '                                                          
*      '   COMMON    '                                                          
*      '  (W/S 1)    '                                                          
*      ' (2008 BYTES)                                                           
*      '-------------'                                                          
*      ' LOCAL W/S   '                                                          
*      '   (W/S 2)   '                                                          
*      '-------------'                                                          
******************************************************                          
* 9/22/08 GENSTAT NO DELETE FROM LIST REMOVED                                   
*         USERS USE ACCESS AWARE                                                
* UPGRADE TO 2 6000 BYTE I/O AREAS                                              
*                                                                               
*                                                                               
* 11/17 CHANGED NMOD FROM 4125 TO 4325 TO GIVE MORE STORAGE                     
* AT END FOR NEW EXTEND BLOCK TO PASS VIA NETBLOCK (SCHT)                       
******************************************************                          
*                                                                               
         EJECT                                                                  
T32000   CSECT                                                                  
         PRINT NOGEN                                                            
*        NMOD1 4125,**T32000,RR=R2,CLEAR=YES                                    
         NMOD1 4325,**T32000,RR=R2,CLEAR=YES  NEW EXTEND BLOCK                  
         ST    R2,RELO                                                          
         LR    R6,R1                                                            
         L     RA,4(R6)          ATWA                                           
         USING T320FFD,RA                                                       
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
******   LA    RF,2500             CLEAR SELECTED STORAGE                       
******   SLL   RF,3                                                             
******   XCEF                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         SPACE 1                                                                
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         LA    R9,IO                                                            
         AH    R9,=H'12000'         GRABBING 2 6000 BYTE I/O AREAS              
         LA    R9,16(R9)            NEED SPACE FOR 1 8BYTE LABEL                
         USING NETSYSD,R9                                                       
         ST    R6,SYSPARMS                                                      
         L     R0,0(R6)            FOR CURSOR POSITIONING                       
         ST    R0,ATIOB                                                         
         SPACE 1                                                                
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    R9,ASYSD                                                         
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*******  BAS   RE,CHKSEC           CHECK/SET SECURITY                           
         BAS   RE,CHKSTEW          CHECK IF COMING FROM STEWARD                 
         L     R6,SYSPARMS                                                      
         L     R2,8(R6)            A(SYSLIST)                                   
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
*              CONTROLLER SAVES 1ST 2300 BYTES OF ASYSD                         
*              (OPTIONALLY STARTING AT ASTARTSV)                                
*              DON'T SAVE ADDRESS IN THIS AREA                                  
*              THEY WILL NOT BE RELOCATED NEXT TIME                             
*              CONTROLLER WILL CLEAR 2300 BYTES STARTING AT AENDSV              
         SPACE 1                                                                
SYSINIT  NTR1                                                                   
         LA    R2,NETSTRSV         START SAVING AFTER ADDRESSES                 
         ST    R2,ASTARTSV                                                      
         ST    R2,AENDSV                                                        
         L     R6,SYSPARMS                                                      
         L     R1,16(R6)           A(COMFACS)                                   
         USING COMFACSD,R1                                                      
         MVC   NDGLOBBR,CGLOBBER                                                
         L     R6,SYSPARMS                                                      
         L     R2,8(R6)            A(SYSLIST)                                   
         L     RF,4(R2)            A(CALLOV)                                    
         SPACE 1                                                                
         XC    DMCB,DMCB           CALLOV FOR A(NETIO)                          
         MVC   DMCB+4(4),=X'D9000A27'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   NSNETIO,DMCB                                                     
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(GENERAL ROUTINES)               
         MVC   DMCB+4(4),=X'D9000A42'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   NVVALID,DMCB                                                     
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(NETDATES)                       
         MVC   DMCB+4(4),=X'D9000A19'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   NDDATES,DMCB                                                     
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(OFFICER)                        
         MVC   DMCB+4(4),=X'D9000A38'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   AOFFICER,DMCB                                                    
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(MSUNPK)                         
         MVC   DMCB+4(4),=X'D9000A1C'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   MSUNPK,DMCB                                                      
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(MSPACK)                         
         MVC   DMCB+4(4),=X'D9000A1B'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   MSPACK,DMCB                                                      
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(CLUNPK)                         
         MVC   DMCB+4(4),=X'D9000A15'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   CLUNPK,DMCB                                                      
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(WRIGEN)                         
         MVC   DMCB+4(4),=X'D9000A4A'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         L     R1,DMCB                                                          
         LA    R2,NDWRIRS                                                       
         LA    R3,NDWRILST                                                      
         SR    R4,R4                                                            
         SPACE 1                                                                
SYSINIT2 ST    R1,0(R2)            SET UP ADDRESSES IN WRIGEN                   
         STC   R4,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         CR    R2,R3                                                            
         BL    SYSINIT2                                                         
         SPACE 1                                                                
         LA    R2,NDRVBEND         ADDRESSES OF WORKING STORAGE                 
         LA    R2,2008(R2)         FOR STATION LIST                             
         ST    R2,ANETWS1                                                       
         LA    R2,1008(R2)                                                      
         ST    R2,ANETWS2                                                       
         LA    R2,1008(R2)                                                      
         ST    R2,ANETWS3                                                       
         LA    R2,1008(R2)                                                      
         ST    R2,ANETWS4                                                       
*                                                                               
         LR    R1,R8               PREVIOUS NMOD WAS 4125                       
         A     R1,=A(33000)        NEW EXTEND BLOCK WILL BE AFTER IT            
         ST    R1,NBEXTEND         -SCHT 11/17                                  
*                                                                               
*                                  SET SYSTEM DEPENDENT VALUES                  
         MVI   SYSTEM,C'N'         NETWORK                                      
         MVI   FILTIDNO,2          PROGRAM FILTER FIELD ID NUMBER               
         MVI   FRSTLAST,C'N'       ONLY CALL REPORT MODULES ONCE                
         MVI   NTWA,1              N'SAVE STORAGE                               
         L     R1,=V(DUMMY)        END OF SYSTEM BASE                           
         A     R1,RELO                                                          
         ST    R1,SYSDUMMY                                                      
         MVC   GETUSER,NVVALID     ROUTINE TO GET AGY NAME,ADDR                 
         LA    R2,NVAGYOUT                                                      
         STC   R2,GETUSER                                                       
         MVC   SYSFIL,=C'UNTFIL  '                                              
         MVC   SYSDIR,=C'UNTDIR  '                                              
         MVC   LKEY,=H'20'         SET VALUES FOR UNIT FILE                     
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'27'                                                  
         MVI   GETMSYS,5           USES GETMSG FOR SYSTEM 5                     
         MVI   MAXIOS,2            USES 2 I/O AREA                              
         MVC   SIZEIO,=F'6000'     EACH I/O IS 3000 BYTES                       
         MVC   LWORK,=F'33000'     WE TOOK 33000 BYTES IN NMOD                  
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9032000'   PRESET FOR SYSTEM CALLOVS                
         L     R1,=A(RECACT)       RECORD/ACTION DIRECTORY                      
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         OI    GENSTAT1,RDUPAPPL                                                
         OI    GENSTAT4,CONFDEL                                                 
         OI    GENSTAT5,GENPRVAL   GENCON SETS PREVAL BIT IN                    
*                                  SAVED REQUEST REOCRD                         
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
CHKSEC   NTR1                            CHECK SECURITY                         
         CLI   TWAOFFC,C'*'              DDS TERMINAL                           
         BE    CHK30                    SKIP SECURITY CHECK                     
         MVI   NDSECFLG,0                                                       
         TM    TWAAUTH,X'40'       UNLESS AUTORIZATION ?                        
         BO    CHK30                                                            
******   OI    GENSTAT4,NODELLST   GENCON NOT ALLOW DELETE FROM LIST            
*                                  IN SAVED REQUEST RECORD                      
*****    OI    GENSTAT5,NOCHGLST   GENCON NOT ALLOW CHANGE FROM LIST            
                                                                                
CHK30    DS    0H                                                               
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*                                                                               
* ROUTINE TO TEST WHETHER WE'VE BEEN CALLED FROM $MAD                           
*                                                                               
CHKSTEW  NTR1                                                                   
         L     R6,SYSPARMS                                                      
         L     R1,16(R6)           A(COMFACS)                                   
         USING COMFACSD,R1                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         DROP  R1                                                               
*                                                                               
         ICM   RF,15,NDGLOBBR                                                   
         BZ    CHKSTEX                                                          
         GOTO1 (RF),DMCB,=C'GETD',WORK,24,GLVXCTL                               
         CLI   DMCB+8,GLEGNF                                                    
         BE    CHKSTEX                                                          
         CLI   DMCB+8,0         CAN'T HANDLE OTHER ERRORS                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                               DELETE 'OLD' TRANSFER ELEM                      
         GOTO1 NDGLOBBR,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,WORK                                                          
         USING GLVXFRSY,RE                                                      
         CLC   GLVXFRSY(6),=C'NETNNA'  TEST FROM NAVIGATOR                      
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RE                                                               
*                                                                               
*  AREA HANDLES CALLS FROM NAVIGATOR                                            
*                                                                               
*  GET BUY INFO PASSED FROM NENAV09                                             
*                                                                               
         LA    R3,IO                                                            
         A     R3,=F'2000'                                                      
         USING PHEADST,R3                                                       
         GOTO1 (RF),DMCB,=C'GETD',WORK,4,GLVBUY1                                
         CLI   DMCB+8,0         CAN'T HANDLE OTHER ERRORS                       
         BE    *+6                                                              
         DC    H'0'                                                             
* READ PHEADER DATA RECORD FROM TEMPSTR                                         
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+8(4),WORK      PAGE/TERMINAL NUMBER                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                            
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,(R3),0                      
         CLI   8(R1),0             BLOW ON ANY ERROR HERE                       
         BE    *+6                                                              
         DC    H'0'                                                             
* FILL RECORD FIELD                                                             
*******  MVI   CONRECH,15                                                       
         MVI   CONRECH+5,7                                                      
         MVC   CONREC(7),=CL7'PHEADST'                                          
         CLI   PHEDTYPE,C'P'                                                    
         BE    CHKST50                                                          
         MVC   CONREC(7),=CL7'EHEADST'                                          
         CLI   PHEDTYPE,C'E'                                                    
         BE    CHKST50                                                          
         DC    H'0'                                                             
* FILL REPORT                                                                   
CHKST50  DS    0H                                                               
*******  MVI   CONACTH,14                                                       
         MVI   CONACTH+5,6                                                      
         MVC   CONACT(6),=CL6'REPORT'                                           
* FILL WHEN                                                                     
         MVC   CONWHEN(8),PHEDPRNT                                              
         MVI   CONWHENH+5,8                                                     
* FILL DESTINATION                                                              
         CLI   PHEDDEST,X'40'                                                   
         BH    CHKST60                                                          
         CLI   PHEDTYPE,C'E'        EHEADER REQUEST DESTINATION NOT REQ         
         BE    CHKST70                                                          
         CLC   PHEDPRNT(4),=CL4'SOON'                                           
         BNE   CHKST70                                                          
         DC    H'0'                 SOON REQUEST MUST HAVE DESTINATION          
CHKST60  MVC   CONDEST(8),PHEDDEST                                              
         MVI   CONDESTH+5,8                                                     
* SET GENCON MODE TO RETURN AFTER SCREEN LOAD                                   
CHKST70  OI    GENSTAT4,NEWSCRM                                                 
*                                                                               
*  SET UP RETURN GLOBBER CALL                                                   
*                                                                               
         XC    WORK,WORK                                                        
         LA    RE,WORK                                                          
         USING GLVXFRSY,RE                                                      
         MVC   GLVXFRSY,=C'NET'    FROM NET/WRI                                 
         MVC   GLVXFRPR,=C'WRI'                                                 
         MVC   GLVXTOSY,=C'NET'    TO   NET/NNA                                 
         MVC   GLVXTOPR,=C'NNA'                                                 
*                                                                               
******   OI    GLVXFLG1,GLV1RETN                                                
         OI    GLVXFLG1,GLV1RETN+GLV1RETG                                       
         DROP  RE                                                               
*                                                                               
         GOTO1 NDGLOBBR,DMCB,=C'PUTD',WORK,24,GLVXCTL                           
*                                                                               
CHKSTEX  B     XIT                                                              
         EJECT                                                                  
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         SPACE 1                                                                
XIT      XIT1                                                                   
         SPACE 1                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
*              CONSTANTS AND LTORG                                              
         SPACE 3                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              RECORD/ACTION TABLES                                             
         SPACE 3                                                                
RECACT   DS    0D                                                               
         SPACE 1                                                                
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
         SPACE 1                                                                
         DC    X'04',C'SPECIAL ',AL1(01),X'0000'                                
         DC    X'04',C'ACCT    ',AL1(02),X'0000'                                
         DC    X'04',C'N2      ',AL1(03),X'0000'                                
         DC    X'04',C'N3      ',AL1(04),X'0000'                                
         DC    X'04',C'N4      ',AL1(05),X'0000'                                
         DC    X'04',C'P3      ',AL1(09),X'0000'                                
         DC    X'04',C'PILSBURY',AL1(09),X'0000'                                
         DC    X'04',C'PB1     ',AL1(10),X'0000'                                
         DC    X'04',C'BBDOPRE ',AL1(10),X'0000'                                
         DC    X'04',C'P4      ',AL1(11),X'00CF'                                
         DC    X'04',C'GEPOST  ',AL1(11),X'00CF'                                
******** DC    X'04',C'DEMOSEED',AL1(13),X'00CF'                                
         DC    X'04',C'N7      ',AL1(15),X'00CF'                                
         DC    X'04',C'DAYPART ',AL1(17),X'00CF'                                
         DC    X'04',C'P5      ',AL1(18),X'00CF'                                
         DC    X'04',C'SRATE   ',AL1(19),X'00CF'                                
         DC    X'04',C'WRITER  ',AL1(20),X'00CF'                                
         DC    X'04',C'FLOWCHRT',AL1(21),X'00CF'                                
         DC    X'04',C'CX      ',AL1(22),X'00CF'                                
         DC    X'04',C'PREBILL ',AL1(23),X'00CF'                                
         DC    X'04',C'CCTRANS ',AL1(24),X'00CF'                                
         DC    X'04',C'BRMFLOW ',AL1(25),X'00CF'                                
****     DC    X'04',C'FLIPPER ',AL1(26),X'00CF'                                
         DC    X'04',C'BRMECOST',AL1(27),X'00CF'                                
         DC    X'04',C'PECOST  ',AL1(27),X'00CF'                                
         DC    X'04',C'PNG     ',AL1(28),X'00CF'                                
         DC    X'04',C'GFTAPE  ',AL1(29),X'00CF'                                
         DC    X'04',C'TEXACO  ',AL1(30),X'00CF'                                
         DC    X'04',C'DELPROD ',AL1(31),X'00CF'                                
         DC    X'04',C'MMTRANS ',AL1(32),X'00CF'                                
         DC    X'04',C'UPDUNT  ',AL1(33),X'00CF'                                
******   DC    X'04',C'PWRITER ',AL1(34),X'00CF'                                
         DC    X'04',C'PHEADER ',AL1(35),X'00CF'                                
         DC    X'04',C'EDI     ',AL1(36),X'00CF'                                
         DC    X'04',C'CPACK   ',AL1(37),X'00CF'                                
         DC    X'04',C'TTRANS  ',AL1(38),X'00CF'                                
         DC    X'04',C'BINTF   ',AL1(39),X'00CF'                                
         DC    X'04',C'CPM     ',AL1(40),X'00CF'                                
         DC    X'04',C'BJNY    ',AL1(41),X'00CF'                                
*******  DC    X'04',C'PUPTRANS',AL1(42),X'00CF'                                
         DC    X'04',C'SCJMTAPE',AL1(43),X'00CF'                                
         DC    X'04',C'SCJITAPE',AL1(44),X'00CF'                                
         DC    X'04',C'UTILS   ',AL1(45),X'00CF'                                
         DC    X'04',C'BJTAPE  ',AL1(46),X'00CF'                                
         DC    X'04',C'CBRECAP ',AL1(47),X'00CF'                                
         DC    X'04',C'HDBILL  ',AL1(48),X'00CF'                                
         DC    X'04',C'CAT     ',AL1(49),X'00CF'                                
         DC    X'04',C'TRANSMIT',AL1(50),X'00CF'                                
         DC    X'04',C'PROGDEL ',AL1(51),X'00CF'                                
         DC    X'04',C'CFAFTER ',AL1(52),X'00CF'                                
         DC    X'04',C'HISTORY ',AL1(53),X'00CF'                                
         DC    X'04',C'COLGATE ',AL1(54),X'00CF'                                
***      DC    X'04',C'AUDIT   ',AL1(55),X'00CF'                                
         DC    X'04',C'ANALYSIS',AL1(56),X'00CF'                                
         DC    X'04',C'PRICING ',AL1(57),X'00CF'                                
         DC    X'04',C'TEST    ',AL1(58),X'00CF'                                
         DC    X'04',C'I2      ',AL1(59),X'00CF'                                
         DC    X'04',C'CUTIN   ',AL1(60),X'00CF'                                
         DC    X'04',C'GOALDOWN',AL1(61),X'00CF'                                
         DC    X'04',C'PROGDOWN',AL1(62),X'00CF'                                
         DC    X'04',C'KRTAPE  ',AL1(63),X'00CF'                                
         DC    X'04',C'GP      ',AL1(64),X'00CF'                                
         DC    X'04',C'EHEADER ',AL1(65),X'00CF'                                
         DC    X'04',C'PHEADST ',AL1(35),X'00CF'                                
         DC    X'04',C'EHEADST ',AL1(65),X'00CF'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
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
*******  DC    X'03',AL1(01,12),X'F111001138',C'SPWA'   SPECIAL                 
*                                                                               
         DC    X'03',AL1(01,01),X'F1110011C0',C'    '  SPECIAL  ADD             
         DC    X'03',AL1(01,02),X'F1110011C0',C'    '           CHANGE          
         DC    X'03',AL1(01,03),X'F1110011C0',C'    '           DISPLAY         
         DC    X'03',AL1(01,04),X'F1110011C0',C'    '           DELETE          
         DC    X'03',AL1(01,05),X'F111001118',C'SPWA'           SELECT          
         DC    X'03',AL1(01,06),X'F1110011C0',C'    '           RESTORE         
         DC    X'03',AL1(01,10),X'F1110011C0',C'    '           LIST            
         DC    X'03',AL1(01,12),X'F111001138',C'SPWA'           REPORT          
*                                                                               
*******  DC    X'03',AL1(02,12),X'F212001238',C'AWWA'   ACCOUNT WRITER          
*                                                                               
         DC    X'03',AL1(02,01),X'F2120012C0',C'    '  ACC WRT  ADD             
         DC    X'03',AL1(02,02),X'F2120012C0',C'    '           CHANGE          
         DC    X'03',AL1(02,03),X'F2120012C0',C'    '           DISPLAY         
         DC    X'03',AL1(02,04),X'F2120012C0',C'    '           DELETE          
         DC    X'03',AL1(02,05),X'F212001218',C'AWWA'           SELECT          
         DC    X'03',AL1(02,06),X'F2120012C0',C'    '           RESTORE         
         DC    X'03',AL1(02,10),X'F2120012C0',C'    '           LIST            
         DC    X'03',AL1(02,12),X'F212001238',C'AWWA'           REPORT          
*                                                                               
*******  DC    X'03',AL1(03,12),X'F313001338',C'N2WN'   N2                      
*                                                                               
         DC    X'03',AL1(03,01),X'F3130013C0',C'    '   N2      ADD             
         DC    X'03',AL1(03,02),X'F3130013C0',C'    '           CHANGE          
         DC    X'03',AL1(03,03),X'F3130013C0',C'    '           DISPLAY         
         DC    X'03',AL1(03,04),X'F3130013C0',C'    '           DELETE          
         DC    X'03',AL1(03,05),X'F313001318',C'N2WN'           SELECT          
         DC    X'03',AL1(03,06),X'F3130013C0',C'    '           RESTORE         
         DC    X'03',AL1(03,10),X'F3130013C0',C'    '           LIST            
         DC    X'03',AL1(03,12),X'F313001338',C'N2WN'           REPORT          
*                                                                               
*******  DC    X'03',AL1(04,12),X'F414001438',C'N3WN'   N3                      
*                                                                               
         DC    X'03',AL1(04,01),X'F4140014C0',C'    '   N3      ADD             
         DC    X'03',AL1(04,02),X'F4140014C0',C'    '           CHANGE          
         DC    X'03',AL1(04,03),X'F4140014C0',C'    '           DISPLAY         
         DC    X'03',AL1(04,04),X'F4140014C0',C'    '           DELETE          
         DC    X'03',AL1(04,05),X'F414001418',C'N3WN'           SELECT          
         DC    X'03',AL1(04,06),X'F4140014C0',C'    '           RESTORE         
         DC    X'03',AL1(04,10),X'F4140014C0',C'    '           LIST            
         DC    X'03',AL1(04,12),X'F414001438',C'N3WN'           REPORT          
*                                                                               
*******  DC    X'03',AL1(05,12),X'F515001538',C'N4WN'   N4                      
*                                                                               
         DC    X'03',AL1(05,01),X'F5150015C0',C'    '   N4      ADD             
         DC    X'03',AL1(05,02),X'F5150015C0',C'    '           CHANGE          
         DC    X'03',AL1(05,03),X'F5150015C0',C'    '           DISPLAY         
         DC    X'03',AL1(05,04),X'F5150015C0',C'    '           DELETE          
         DC    X'03',AL1(05,05),X'F515001518',C'N4WN'           SELECT          
         DC    X'03',AL1(05,06),X'F5150015C0',C'    '           RESTORE         
         DC    X'03',AL1(05,10),X'F5150015C0',C'    '           LIST            
         DC    X'03',AL1(05,12),X'F515001538',C'N4WN'           REPORT          
*                                                                               
*******  DC    X'03',AL1(08,12),X'F818001838',C'COWT'   COMMERCIAL              
*                                                                               
         DC    X'03',AL1(08,01),X'F8180018C0',C'    '   COMEMRCIAL ADD          
         DC    X'03',AL1(08,02),X'F8180018C0',C'    '           CHANGE          
         DC    X'03',AL1(08,03),X'F8180018C0',C'    '           DISPLAY         
         DC    X'03',AL1(08,04),X'F8180018C0',C'    '           DELETE          
         DC    X'03',AL1(08,05),X'F818001818',C'COWT'           SELECT          
         DC    X'03',AL1(08,06),X'F8180018C0',C'    '           RESTORE         
         DC    X'03',AL1(08,10),X'F8180018C0',C'    '           LIST            
         DC    X'03',AL1(08,12),X'F818001838',C'COWT'           REPORT          
*                                                                               
*******  DC    X'03',AL1(09,12),X'F919002938',C'P3WP'   P3 (PILLSBURY)          
*                                                                               
         DC    X'03',AL1(09,01),X'F9190029C0',C'    '   P3      ADD             
         DC    X'03',AL1(09,02),X'F9190029C0',C'    '           CHANGE          
         DC    X'03',AL1(09,03),X'F9190029C0',C'    '           DISPLAY         
         DC    X'03',AL1(09,04),X'F9190029C0',C'    '           DELETE          
         DC    X'03',AL1(09,05),X'F919002918',C'P3WP'           SELECT          
         DC    X'03',AL1(09,06),X'F9190029C0',C'    '           RESTORE         
         DC    X'03',AL1(09,10),X'F9190029C0',C'    '           LIST            
         DC    X'03',AL1(09,12),X'F919002938',C'P3WP'           REPORT          
*                                                                               
*******  DC    X'03',AL1(10,12),X'FA1A002A38',C'PBWP'   PB1 (BBDO PRE)          
*                                                                               
         DC    X'03',AL1(10,01),X'FA1A002AC0',C'    '   PB1   ) ADD             
         DC    X'03',AL1(10,02),X'FA1A002AC0',C'    '           CHANGE          
         DC    X'03',AL1(10,03),X'FA1A002AC0',C'    '           DISPLAY         
         DC    X'03',AL1(10,04),X'FA1A002AC0',C'    '           DELETE          
         DC    X'03',AL1(10,05),X'FA1A002A18',C'PBWP'           SELECT          
         DC    X'03',AL1(10,06),X'FA1A002AC0',C'    '           RESTORE         
         DC    X'03',AL1(10,10),X'FA1A002AC0',C'    '           LIST            
         DC    X'03',AL1(10,12),X'FA1A002A38',C'PBWP'           REPORT          
*                                                                               
******** DC    X'03',AL1(11,12),X'FB1B002B38',C'P4WP'   P4 (GE)                 
*                                                                               
         DC    X'03',AL1(11,01),X'FB1B002BC0',C'    '   P4 (GE) ADD             
         DC    X'03',AL1(11,02),X'FB1B002BC0',C'    '           CHANGE          
         DC    X'03',AL1(11,03),X'FB1B002BC0',C'    '           DISPLAY         
         DC    X'03',AL1(11,04),X'FB1B002BC0',C'    '           DELETE          
         DC    X'03',AL1(11,05),X'FB1B002B18',C'P4WP'           SELECT          
         DC    X'03',AL1(11,06),X'FB1B002BC0',C'    '           RESTORE         
         DC    X'03',AL1(11,10),X'FB1B002BC0',C'    '           LIST            
         DC    X'03',AL1(11,12),X'FB1B002B38',C'P4WP'           REPORT          
*                                                                               
*******  DC    X'03',AL1(13,12),X'E222002318',C'DSDS'   DEMOSEED                
*                                                                               
         DC    X'03',AL1(13,01),X'E2220023C0',C'    '   DEMOSEED ADD            
         DC    X'03',AL1(13,02),X'E2220023C0',C'    '           CHANGE          
         DC    X'03',AL1(13,03),X'E2220023C0',C'    '           DISPLAY         
         DC    X'03',AL1(13,04),X'E2220023C0',C'    '           DELETE          
         DC    X'03',AL1(13,05),X'E222002318',C'DSDS'           SELECT          
         DC    X'03',AL1(13,06),X'E2220023C0',C'    '           RESTORE         
         DC    X'03',AL1(13,10),X'E2220023C0',C'    '           LIST            
         DC    X'03',AL1(13,12),X'E222002318',C'DSDS'           REPORT          
*                                                                               
*******  DC    X'03',AL1(15,12),X'E427002738',C'N7WN'   N7 J-J REPORT           
*                                                                               
         DC    X'03',AL1(15,01),X'E4270027C0',C'    '   N7 JNJ  ADD             
         DC    X'03',AL1(15,02),X'E4270027C0',C'    '           CHANGE          
         DC    X'03',AL1(15,03),X'E4270027C0',C'    '           DISPLAY         
         DC    X'03',AL1(15,04),X'E4270027C0',C'    '           DELETE          
         DC    X'03',AL1(15,05),X'E427002718',C'N7WN'           SELECT          
         DC    X'03',AL1(15,06),X'E4270027C0',C'    '           RESTORE         
         DC    X'03',AL1(15,10),X'E4270027C0',C'    '           LIST            
         DC    X'03',AL1(15,12),X'E427002738',C'N7WN'           REPORT          
*                                                                               
                                                                                
*                                                                               
         DC    X'03',AL1(17,01),X'E6310031C0',C'    '   DPT WEEKLY  ADD         
         DC    X'03',AL1(17,02),X'E6310031C0',C'    '           CHANGE          
         DC    X'03',AL1(17,03),X'E6310031C0',C'    '           DISPLAY         
         DC    X'03',AL1(17,04),X'E6310031C0',C'    '           DELETE          
         DC    X'03',AL1(17,05),X'E631003118',C'DAWP'           SELECT          
         DC    X'03',AL1(17,06),X'E6310031C0',C'    '           RESTORE         
         DC    X'03',AL1(17,10),X'E6310031C0',C'    '           LIST            
         DC    X'03',AL1(17,12),X'E631003138',C'DAWP'           REPORT          
*                                                                               
******** DC    X'03',AL1(18,12),X'E736003718',C'P5P5'   P5                      
*                                                                               
         DC    X'03',AL1(18,01),X'E7360037C0',C'    '   P5      ADD             
         DC    X'03',AL1(18,02),X'E7360037C0',C'    '           CHANGE          
         DC    X'03',AL1(18,03),X'E7360037C0',C'    '           DISPLAY         
         DC    X'03',AL1(18,04),X'E7360037C0',C'    '           DELETE          
         DC    X'03',AL1(18,05),X'E736003718',C'P5P5'           SELECT          
         DC    X'03',AL1(18,06),X'E7360037C0',C'    '           RESTORE         
         DC    X'03',AL1(18,10),X'E7360037C0',C'    '           LIST            
         DC    X'03',AL1(18,12),X'E736003718',C'P5P5'           REPORT          
*                                                                               
******** DC    X'03',AL1(19,12),X'E838003918',C'SRWP'   SRATE                   
*                                                                               
         DC    X'03',AL1(19,01),X'E8380039C0',C'    '   SRATE   ADD             
         DC    X'03',AL1(19,02),X'E8380039C0',C'    '           CHANGE          
         DC    X'03',AL1(19,03),X'E8380039C0',C'    '           DISPLAY         
         DC    X'03',AL1(19,04),X'E8380039C0',C'    '           DELETE          
         DC    X'03',AL1(19,05),X'E838003918',C'SRWP'           SELECT          
         DC    X'03',AL1(19,06),X'E8380039C0',C'    '           RESTORE         
         DC    X'03',AL1(19,10),X'E8380039C0',C'    '           LIST            
         DC    X'03',AL1(19,12),X'E838003918',C'SRWP'           REPORT          
*                                                                               
*                                                                               
         DC    X'03',AL1(20,01),X'E0200000C0',C'    '   WRITER  ADD             
         DC    X'03',AL1(20,02),X'E0200000C0',C'    '   WRITER  CHANGE          
         DC    X'03',AL1(20,03),X'E0200000C0',C'    '   WRITER  DISPLAY         
         DC    X'03',AL1(20,04),X'E0200000C0',C'    '   WRITER  DELETE          
         DC    X'03',AL1(20,05),X'E020000038',C'WRW2'   WRITER  SELECT          
         DC    X'03',AL1(20,06),X'E0200000C0',C'    '   WRITER  RESTORE         
         DC    X'03',AL1(20,10),X'E0200000C0',C'    '   WRITER  LIST            
         DC    X'03',AL1(20,12),X'E020002038',C'WRW2'   WRITER  REPORT          
*                                                                               
*                                                                               
*******  DC    X'03',AL1(21,12),X'EC2C003C38',C'FLSW'   FLOWCHART               
         DC    X'03',AL1(21,01),X'EC2C003CC0',C'    '           ADD             
         DC    X'03',AL1(21,02),X'EC2C003CC0',C'    '           CHANGE          
         DC    X'03',AL1(21,03),X'EC2C003CC0',C'    '           DISPLAY         
         DC    X'03',AL1(21,04),X'EC2C003CC0',C'    '           DELETE          
         DC    X'03',AL1(21,05),X'EC2C003C18',C'FLSW'           SELECT          
         DC    X'03',AL1(21,06),X'EC2C003CC0',C'    '           RESTORE         
         DC    X'03',AL1(21,10),X'EC2C003CC0',C'    '           LIST            
         DC    X'03',AL1(21,12),X'EC2C003C38',C'FLSW'           REPORT          
*                                                                               
******** DC    X'03',AL1(22,12),X'D9C900A938',C'CXSW'   CX TAPE                 
         DC    X'03',AL1(22,01),X'D9C900A9C0',C'    '           ADD             
         DC    X'03',AL1(22,02),X'D9C900A9C0',C'    '           CHANGE          
         DC    X'03',AL1(22,03),X'D9C900A9C0',C'    '           DISPLAY         
         DC    X'03',AL1(22,04),X'D9C900A9C0',C'    '           DELETE          
         DC    X'03',AL1(22,05),X'D9C900A918',C'CXSW'           SELECT          
         DC    X'03',AL1(22,06),X'D9C900A9C0',C'    '           RESTORE         
         DC    X'03',AL1(22,10),X'D9C900A9C0',C'    '           LIST            
         DC    X'03',AL1(22,12),X'D9C900A938',C'CXSW'           REPORT          
*                                                                               
*******  DC    X'03',AL1(23,12),X'E020002038',C'WRW1'   PREBILL                 
         DC    X'03',AL1(23,01),X'E0200020C0',C'    '           ADD             
         DC    X'03',AL1(23,02),X'E0200020C0',C'    '           CHANGE          
         DC    X'03',AL1(23,03),X'E0200020C0',C'    '           DISPLAY         
         DC    X'03',AL1(23,04),X'E0200020C0',C'    '           DELETE          
         DC    X'03',AL1(23,05),X'E020002018',C'WRW1'           SELECT          
         DC    X'03',AL1(23,06),X'E0200020C0',C'    '           RESTORE         
         DC    X'03',AL1(23,10),X'E0200020C0',C'    '           LIST            
         DC    X'03',AL1(23,12),X'E020002038',C'WRW1'           REPORT          
*                                                                               
*******  DC    X'03',AL1(24,12),X'E931003118',C'CKWS'   COKE TRANSFER           
         DC    X'03',AL1(24,01),X'E9310031C0',C'    '           ADD             
         DC    X'03',AL1(24,02),X'E9310031C0',C'    '           CHANGE          
         DC    X'03',AL1(24,03),X'E9310031C0',C'    '           DISPLAY         
         DC    X'03',AL1(24,04),X'E9310031C0',C'    '           DELETE          
         DC    X'03',AL1(24,05),X'E931003118',C'CKWS'           SELECT          
         DC    X'03',AL1(24,06),X'E9310031C0',C'    '           RESTORE         
         DC    X'03',AL1(24,10),X'E9310031C0',C'    '           LIST            
         DC    X'03',AL1(24,12),X'E931003118',C'CKWS'           REPORT          
*                                                                               
*******  DC    X'03',AL1(25,12),X'ED3A003B38',C'FLSW'   BRMFLOW                 
         DC    X'03',AL1(25,01),X'ED3A003BC0',C'    '           ADD             
         DC    X'03',AL1(25,02),X'ED3A003BC0',C'    '           CHANGE          
         DC    X'03',AL1(25,03),X'ED3A003BC0',C'    '           DISPLAY         
         DC    X'03',AL1(25,04),X'ED3A003BC0',C'    '           DELETE          
         DC    X'03',AL1(25,05),X'ED3A003B18',C'FLSW'           SELECT          
         DC    X'03',AL1(25,06),X'ED3A003BC0',C'    '           RESTORE         
         DC    X'03',AL1(25,10),X'ED3A003BC0',C'    '           LIST            
         DC    X'03',AL1(25,12),X'ED3A003B38',C'FLSW'           REPORT          
*                                                                               
*******  DC    X'03',AL1(26,12),X'EA40004038',C'FPW9'   FLIPPER                 
***      DC    X'03',AL1(26,01),X'EA400040C0',C'    '           ADD             
***      DC    X'03',AL1(26,02),X'EA400040C0',C'    '           CHANGE          
***      DC    X'03',AL1(26,03),X'EA400040C0',C'    '           DISPLAY         
***      DC    X'03',AL1(26,04),X'EA400040C0',C'    '           DELETE          
***      DC    X'03',AL1(26,05),X'EA40004018',C'FPW9'           SELECT          
***      DC    X'03',AL1(26,06),X'EA400040C0',C'    '           RESTORE         
***      DC    X'03',AL1(26,10),X'EA400040C0',C'    '           LIST            
***      DC    X'03',AL1(26,12),X'EA40004038',C'FPW9'           REPORT          
*                                                                               
*******  DC    X'03',AL1(27,12),X'EB41004238',C'BEWB'   BRMECOST                
         DC    X'03',AL1(27,01),X'EB410042C0',C'    '           ADD             
         DC    X'03',AL1(27,02),X'EB410042C0',C'    '           CHANGE          
         DC    X'03',AL1(27,03),X'EB410042C0',C'    '           DISPLAY         
         DC    X'03',AL1(27,04),X'EB410042C0',C'    '           DELETE          
         DC    X'03',AL1(27,05),X'EB41004218',C'BEWB'           SELECT          
         DC    X'03',AL1(27,06),X'EB410042C0',C'    '           RESTORE         
         DC    X'03',AL1(27,10),X'EB410042C0',C'    '           LIST            
         DC    X'03',AL1(27,12),X'EB41004238',C'BEWB'           REPORT          
*                                                                               
*******  DC    X'03',AL1(27,12),X'EB41004238',C'PEWB'   PECOST(= BRME)          
         DC    X'03',AL1(27,01),X'EB410042C0',C'    '           ADD             
         DC    X'03',AL1(27,02),X'EB410042C0',C'    '           CHANGE          
         DC    X'03',AL1(27,03),X'EB410042C0',C'    '           DISPLAY         
         DC    X'03',AL1(27,04),X'EB410042C0',C'    '           DELETE          
         DC    X'03',AL1(27,05),X'EB41004218',C'PEWB'           SELECT          
         DC    X'03',AL1(27,06),X'EB410042C0',C'    '           RESTORE         
         DC    X'03',AL1(27,10),X'EB410042C0',C'    '           LIST            
         DC    X'03',AL1(27,12),X'EB41004238',C'PEWB'           REPORT          
*                                                                               
*******  DC    X'03',AL1(28,12),X'D143004638',C'PGWN'   PNG REPORT              
         DC    X'03',AL1(28,01),X'D1430046C0',C'    '           ADD             
         DC    X'03',AL1(28,02),X'D1430046C0',C'    '           CHANGE          
         DC    X'03',AL1(28,03),X'D1430046C0',C'    '           DISPLAY         
         DC    X'03',AL1(28,04),X'D1430046C0',C'    '           DELETE          
         DC    X'03',AL1(28,05),X'D143004618',C'PGWN'           SELECT          
         DC    X'03',AL1(28,06),X'D1430046C0',C'    '           RESTORE         
         DC    X'03',AL1(28,10),X'D1430046C0',C'    '           LIST            
         DC    X'03',AL1(28,12),X'D143004638',C'PGWN'           REPORT          
*                                                                               
******   DC    X'03',AL1(29,12),X'D244004538',C'GFGG'   GF TAPE                 
         DC    X'03',AL1(29,01),X'D2440045C0',C'    '           ADD             
         DC    X'03',AL1(29,02),X'D2440045C0',C'    '           CHANGE          
         DC    X'03',AL1(29,03),X'D2440045C0',C'    '           DISPLAY         
         DC    X'03',AL1(29,04),X'D2440045C0',C'    '           DELETE          
         DC    X'03',AL1(29,05),X'D244004518',C'GFGG'           SELECT          
         DC    X'03',AL1(29,06),X'D2440045C0',C'    '           RESTORE         
         DC    X'03',AL1(29,10),X'D2440045C0',C'    '           LIST            
         DC    X'03',AL1(29,12),X'D244004538',C'GFGG'           REPORT          
*                                                                               
*******  DC    X'03',AL1(30,12),X'D347004818',C'TXNX'   TEXACO                  
         DC    X'03',AL1(30,01),X'D3470048C0',C'    '           ADD             
         DC    X'03',AL1(30,02),X'D3470048C0',C'    '           CHANGE          
         DC    X'03',AL1(30,03),X'D3470048C0',C'    '           DISPLAY         
         DC    X'03',AL1(30,04),X'D3470048C0',C'    '           DELETE          
         DC    X'03',AL1(30,05),X'D347004818',C'TXNX'           SELECT          
         DC    X'03',AL1(30,06),X'D3470048C0',C'    '           RESTORE         
         DC    X'03',AL1(30,10),X'D3470048C0',C'    '           LIST            
         DC    X'03',AL1(30,12),X'D347004818',C'TXNX'           REPORT          
*                                                                               
*******  DC    X'03',AL1(31,12),X'D549004918',C'TXWB'   DELPROD                 
         DC    X'03',AL1(31,01),X'D5490048C0',C'    '           ADD             
         DC    X'03',AL1(31,02),X'D5490048C0',C'    '           CHANGE          
         DC    X'03',AL1(31,03),X'D5490048C0',C'    '           DISPLAY         
         DC    X'03',AL1(31,04),X'D5490048C0',C'    '           DELETE          
         DC    X'03',AL1(31,05),X'D549004818',C'TXWB'           SELECT          
         DC    X'03',AL1(31,06),X'D5490048C0',C'    '           RESTORE         
         DC    X'03',AL1(31,10),X'D5490048C0',C'    '           LIST            
         DC    X'03',AL1(31,12),X'D549004818',C'TXWB'           REPORT          
*                                                                               
*******  DC    X'03',AL1(32,12),X'D650005018',C'TXWB'   MMTRANS                 
         DC    X'03',AL1(32,01),X'D6500050C0',C'    '           ADD             
         DC    X'03',AL1(32,02),X'D6500050C0',C'    '           CHANGE          
         DC    X'03',AL1(32,03),X'D6500050C0',C'    '           DISPLAY         
         DC    X'03',AL1(32,04),X'D6500050C0',C'    '           DELETE          
         DC    X'03',AL1(32,05),X'D650005018',C'TXWB'           SELECT          
         DC    X'03',AL1(32,06),X'D6500050C0',C'    '           RESTORE         
         DC    X'03',AL1(32,10),X'D6500050C0',C'    '           LIST            
         DC    X'03',AL1(32,12),X'D650005018',C'TXWB'           REPORT          
*                                                                               
*******  DC    X'03',AL1(33,12),X'D751005118',C'TXWQ'   UPDUNT                  
         DC    X'03',AL1(33,01),X'D7510051C0',C'    '           ADD             
         DC    X'03',AL1(33,02),X'D7510051C0',C'    '           CHANGE          
         DC    X'03',AL1(33,03),X'D7510051C0',C'    '           DISPLAY         
         DC    X'03',AL1(33,04),X'D7510051C0',C'    '           DELETE          
         DC    X'03',AL1(33,05),X'D751005118',C'TXWQ'           SELECT          
         DC    X'03',AL1(33,06),X'D7510051C0',C'    '           RESTORE         
         DC    X'03',AL1(33,10),X'D7510051C0',C'    '           LIST            
         DC    X'03',AL1(33,12),X'D751005118',C'TXWQ'           REPORT          
*                                                                               
*******  DC    X'03',AL1(34,12),X'D020002038',C'WRW2'   PUP WRITER              
         DC    X'03',AL1(34,01),X'D0200020C0',C'    '           ADD             
         DC    X'03',AL1(34,02),X'D0200020C0',C'    '           CHANGE          
         DC    X'03',AL1(34,03),X'D0200020C0',C'    '           DISPLAY         
         DC    X'03',AL1(34,04),X'D0200020C0',C'    '           DELETE          
         DC    X'03',AL1(34,05),X'D020002018',C'WRW2'           SELECT          
         DC    X'03',AL1(34,06),X'D0200020C0',C'    '           RESTORE         
         DC    X'03',AL1(34,10),X'D0200020C0',C'    '           LIST            
         DC    X'03',AL1(34,12),X'D020002038',C'WRW2'           REPORT          
*                                                                               
*******  DC    X'03',AL1(35,12),X'D852005278',C'PHWH'   PKG HEADER              
         DC    X'03',AL1(35,01),X'D8520052C0',C'    '           ADD             
         DC    X'03',AL1(35,02),X'D8520052C0',C'    '           CHANGE          
         DC    X'03',AL1(35,03),X'D8520052C0',C'    '           DISPLAY         
         DC    X'03',AL1(35,04),X'D8520052C0',C'    '           DELETE          
         DC    X'03',AL1(35,05),X'D852005218',C'PHWH'           SELECT          
         DC    X'03',AL1(35,06),X'D8520052C0',C'    '           RESTORE         
         DC    X'03',AL1(35,10),X'D8520052C0',C'    '           LIST            
         DC    X'03',AL1(35,12),X'D852005278',C'PHWH'           REPORT          
*                                                                               
*****    DC    X'03',AL1(36,12),X'D353005318',C'EDED'   EDI                     
         DC    X'03',AL1(36,01),X'D3530053C0',C'    '           ADD             
         DC    X'03',AL1(36,02),X'D3530053C0',C'    '           CHANGE          
         DC    X'03',AL1(36,03),X'D3530053C0',C'    '           DISPLAY         
         DC    X'03',AL1(36,04),X'D3530053C0',C'    '           DELETE          
         DC    X'03',AL1(36,05),X'D353005318',C'EDED'           SELECT          
         DC    X'03',AL1(36,06),X'D3530053C0',C'    '           RESTORE         
         DC    X'03',AL1(36,10),X'D3530053C0',C'    '           LIST            
         DC    X'03',AL1(36,12),X'D353002318',C'EDED'           REPORT          
*                                                                               
*****    DC    X'03',AL1(37,12),X'DA54005418',C'CPWD'   CPACK                   
         DC    X'03',AL1(37,01),X'DA540054C0',C'    '           ADD             
         DC    X'03',AL1(37,02),X'DA540054C0',C'    '           CHANGE          
         DC    X'03',AL1(37,03),X'DA540054C0',C'    '           DISPLAY         
         DC    X'03',AL1(37,04),X'DA540054C0',C'    '           DELETE          
         DC    X'03',AL1(37,05),X'DA54005418',C'CPWD'           SELECT          
         DC    X'03',AL1(37,06),X'DA540054C0',C'    '           RESTORE         
         DC    X'03',AL1(37,10),X'DA540054C0',C'    '           LIST            
         DC    X'03',AL1(37,12),X'DA54005418',C'CPWD'           REPORT          
*                                                                               
*******  DC    X'03',AL1(38,12),X'DC65006538',C'TXWZ'   TTRANS(TALENT)          
         DC    X'03',AL1(38,01),X'DC650065C0',C'    '           ADD             
         DC    X'03',AL1(38,02),X'DC650065C0',C'    '           CHANGE          
         DC    X'03',AL1(38,03),X'DC650065C0',C'    '           DISPLAY         
         DC    X'03',AL1(38,04),X'DC650065C0',C'    '           DELETE          
         DC    X'03',AL1(38,05),X'DC65006518',C'TXWZ'           SELECT          
         DC    X'03',AL1(38,06),X'DC650065C0',C'    '           RESTORE         
         DC    X'03',AL1(38,10),X'DC650065C0',C'    '           LIST            
         DC    X'03',AL1(38,12),X'DC65006538',C'TXWZ'           REPORT          
*                                                                               
*******  DC    X'03',AL1(39,12),X'DB32003218',C'TXWQ'   BINTF                   
         DC    X'03',AL1(39,01),X'DB320032C0',C'    '           ADD             
         DC    X'03',AL1(39,02),X'DB320032C0',C'    '           CHANGE          
         DC    X'03',AL1(39,03),X'DB320032C0',C'    '           DISPLAY         
         DC    X'03',AL1(39,04),X'DB320032C0',C'    '           DELETE          
         DC    X'03',AL1(39,05),X'DB32003218',C'TXWQ'           SELECT          
         DC    X'03',AL1(39,06),X'DB320032C0',C'    '           RESTORE         
         DC    X'03',AL1(39,10),X'DB320032C0',C'    '           LIST            
         DC    X'03',AL1(39,12),X'DB32003218',C'TXWQ'           REPORT          
*                                                                               
*******  DC    X'03',AL1(40,12),X'DD64006438',C'CMW0'   CPM                     
         DC    X'03',AL1(40,01),X'DD640064C0',C'    '           ADD             
         DC    X'03',AL1(40,02),X'DD640064C0',C'    '           CHANGE          
         DC    X'03',AL1(40,03),X'DD640064C0',C'    '           DISPLAY         
         DC    X'03',AL1(40,04),X'DD640064C0',C'    '           DELETE          
         DC    X'03',AL1(40,05),X'DD64006418',C'CMW0'           SELECT          
         DC    X'03',AL1(40,06),X'DD640064C0',C'    '           RESTORE         
         DC    X'03',AL1(40,10),X'DD640064C0',C'    '           LIST            
         DC    X'03',AL1(40,12),X'DD64006438',C'CMW0'           REPORT          
*                                                                               
*******  DC    X'03',AL1(41,12),X'DD66006618',C'CMBJ'   BJNY                    
         DC    X'03',AL1(41,01),X'DD660066C0',C'    '           ADD             
         DC    X'03',AL1(41,02),X'DD660066C0',C'    '           CHANGE          
         DC    X'03',AL1(41,03),X'DD660066C0',C'    '           DISPLAY         
         DC    X'03',AL1(41,04),X'DD660066C0',C'    '           DELETE          
         DC    X'03',AL1(41,05),X'DD66006618',C'CMBJ'           SELECT          
         DC    X'03',AL1(41,06),X'DD660066C0',C'    '           RESTORE         
         DC    X'03',AL1(41,10),X'DD660066C0',C'    '           LIST            
         DC    X'03',AL1(41,12),X'DD66006618',C'CMBJ'           REPORT          
*                                                                               
*******  DC    X'03',AL1(42,12),X'DE67006718',C'CMPU'   PUP                     
         DC    X'03',AL1(42,01),X'DE670067C0',C'    '           ADD             
         DC    X'03',AL1(42,02),X'DE670067C0',C'    '           CHANGE          
         DC    X'03',AL1(42,03),X'DE670067C0',C'    '           DISPLAY         
         DC    X'03',AL1(42,04),X'DE670067C0',C'    '           DELETE          
         DC    X'03',AL1(42,05),X'DE67006718',C'CMPU'           SELECT          
         DC    X'03',AL1(42,06),X'DE670067C0',C'    '           RESTORE         
         DC    X'03',AL1(42,10),X'DE670067C0',C'    '           LIST            
         DC    X'03',AL1(42,12),X'DE67006718',C'CMPU'           REPORT          
*                                                                               
*******  DC    X'03',AL1(43,12),X'DF68006818',C'JWWJ'   SCJMTAPE                
         DC    X'03',AL1(43,01),X'DF680068C0',C'    '           ADD             
         DC    X'03',AL1(43,02),X'DF680068C0',C'    '           CHANGE          
         DC    X'03',AL1(43,03),X'DF680068C0',C'    '           DISPLAY         
         DC    X'03',AL1(43,04),X'DF680068C0',C'    '           DELETE          
         DC    X'03',AL1(43,05),X'DF68006818',C'JWWJ'           SELECT          
         DC    X'03',AL1(43,06),X'DF680068C0',C'    '           RESTORE         
         DC    X'03',AL1(43,10),X'DF680068C0',C'    '           LIST            
         DC    X'03',AL1(43,12),X'DF68006818',C'JWWJ'           REPORT          
*                                                                               
*******  DC    X'03',AL1(44,12),X'DF69006918',C'JWNK'   SCJITAPE                
         DC    X'03',AL1(44,01),X'DF690069C0',C'    '           ADD             
         DC    X'03',AL1(44,02),X'DF690069C0',C'    '           CHANGE          
         DC    X'03',AL1(44,03),X'DF690069C0',C'    '           DISPLAY         
         DC    X'03',AL1(44,04),X'DF690069C0',C'    '           DELETE          
         DC    X'03',AL1(44,05),X'DF69006918',C'JWNK'           SELECT          
         DC    X'03',AL1(44,06),X'DF690069C0',C'    '           RESTORE         
         DC    X'03',AL1(44,10),X'DF690069C0',C'    '           LIST            
         DC    X'03',AL1(44,12),X'DF69006918',C'JWNK'           REPORT          
*                                                                               
*******  DC    X'03',AL1(45,12),X'EF70007038',C'TXXX'   UTILS                   
         DC    X'03',AL1(45,01),X'EF700070C0',C'    '           ADD             
         DC    X'03',AL1(45,02),X'EF700070C0',C'    '           CHANGE          
         DC    X'03',AL1(45,03),X'EF700070C0',C'    '           DISPLAY         
         DC    X'03',AL1(45,04),X'EF700070C0',C'    '           DELETE          
         DC    X'03',AL1(45,05),X'EF70007018',C'TXXX'           SELECT          
         DC    X'03',AL1(45,06),X'EF700070C0',C'    '           RESTORE         
         DC    X'03',AL1(45,10),X'EF700070C0',C'    '           LIST            
         DC    X'03',AL1(45,12),X'EF70007038',C'TXXX'           REPORT          
*                                                                               
         DC    X'03',AL1(46,12),X'DF71007138',C'TXBJ'   BJTAPE                  
*******  DC    X'03',AL1(46,01),X'DF710071C0',C'    '           ADD             
         DC    X'03',AL1(46,02),X'DF710071C0',C'    '           CHANGE          
         DC    X'03',AL1(46,03),X'DF710071C0',C'    '           DISPLAY         
         DC    X'03',AL1(46,04),X'DF710071C0',C'    '           DELETE          
         DC    X'03',AL1(46,05),X'DF71007118',C'TXBJ'           SELECT          
         DC    X'03',AL1(46,06),X'DF710071C0',C'    '           RESTORE         
         DC    X'03',AL1(46,10),X'DF710071C0',C'    '           LIST            
         DC    X'03',AL1(46,12),X'DF71007138',C'TXBJ'           REPORT          
*                                                                               
*******  DC    X'03',AL1(47,12),X'EE72007238',C'CBW4'   CABRCP                  
         DC    X'03',AL1(47,02),X'EE720072C0',C'    '           CHANGE          
         DC    X'03',AL1(47,03),X'EE720072C0',C'    '           DISPLAY         
         DC    X'03',AL1(47,04),X'EE720072C0',C'    '           DELETE          
         DC    X'03',AL1(47,05),X'EE72007218',C'CBW4'           SELECT          
         DC    X'03',AL1(47,06),X'EE720072C0',C'    '           RESTORE         
         DC    X'03',AL1(47,10),X'EE720072C0',C'    '           LIST            
         DC    X'03',AL1(47,12),X'EE72007238',C'CBW4'           REPORT          
*                                                                               
******   DC    X'03',AL1(48,12),X'E173007338',C'TXHD'   HOMEDEPOT (HD)          
         DC    X'03',AL1(48,02),X'E1730073C0',C'    '           CHANGE          
         DC    X'03',AL1(48,03),X'E1730073C0',C'    '           DISPLAY         
         DC    X'03',AL1(48,04),X'E1730073C0',C'    '           DELETE          
         DC    X'03',AL1(48,05),X'E173007318',C'TXHD'           SELECT          
         DC    X'03',AL1(48,06),X'E1730073C0',C'    '           RESTORE         
         DC    X'03',AL1(48,10),X'E1730073C0',C'    '           LIST            
         DC    X'03',AL1(48,12),X'E173007338',C'TXHD'           REPORT          
*                                                                               
*******  DC    X'03',AL1(49,12),X'CF83008318',C'ATAT'   CAT                     
         DC    X'03',AL1(49,02),X'CF830083C0',C'    '           CHANGE          
         DC    X'03',AL1(49,03),X'CF830083C0',C'    '           DISPLAY         
         DC    X'03',AL1(49,04),X'CF830083C0',C'    '           DELETE          
         DC    X'03',AL1(49,05),X'CF83008318',C'ATAT'           SELECT          
         DC    X'03',AL1(49,06),X'CF830083C0',C'    '           RESTORE         
         DC    X'03',AL1(49,10),X'CF830083C0',C'    '           LIST            
         DC    X'03',AL1(49,12),X'CF83008318',C'ATAT'           REPORT          
*                                                                               
******   DC    X'03',AL1(50,12),X'E020002038',C'TRTR'   TRANSMIT                
         DC    X'03',AL1(50,01),X'E0200020C0',C'    '           ADD             
         DC    X'03',AL1(50,02),X'E0200020C0',C'    '           CHANGE          
         DC    X'03',AL1(50,03),X'E0200020C0',C'    '           DISPLAY         
         DC    X'03',AL1(50,04),X'E0200020C0',C'    '           DELETE          
         DC    X'03',AL1(50,05),X'E020002018',C'TRTR'           SELECT          
         DC    X'03',AL1(50,06),X'E0200020C0',C'    '           RESTORE         
         DC    X'03',AL1(50,10),X'E0200020C0',C'    '           LIST            
         DC    X'03',AL1(50,12),X'E020002038',C'TRTR'           REPORT          
*                                                                               
******   DC    X'03',AL1(51,12),X'CA56005818',C'TRXX'   PROGRAM REC DEL         
         DC    X'03',AL1(51,02),X'CA560058C0',C'    '           CHANGE          
         DC    X'03',AL1(51,03),X'CA560058C0',C'    '           DISPLAY         
         DC    X'03',AL1(51,04),X'CA560058C0',C'    '           DELETE          
         DC    X'03',AL1(51,05),X'CA56005818',C'TRXX'           SELECT          
         DC    X'03',AL1(51,06),X'CA560058C0',C'    '           RESTORE         
         DC    X'03',AL1(51,10),X'CA560058C0',C'    '           LIST            
         DC    X'03',AL1(51,12),X'CA56005818',C'TRXX'           REPORT          
*                                                                               
******   DC    X'03',AL1(52,12),X'E020002038',C'CFFL'   CFAFTER                 
         DC    X'03',AL1(52,02),X'E0200020C0',C'    '           CHANGE          
         DC    X'03',AL1(52,03),X'E0200020C0',C'    '           DISPLAY         
         DC    X'03',AL1(52,04),X'E0200020C0',C'    '           DELETE          
         DC    X'03',AL1(52,05),X'E020002018',C'CFFL'           SELECT          
         DC    X'03',AL1(52,06),X'E0200020C0',C'    '           RESTORE         
         DC    X'03',AL1(52,10),X'E0200020C0',C'    '           LIST            
         DC    X'03',AL1(52,12),X'E020002038',C'CFFL'           REPORT          
*                                                                               
*******  DC    X'03',AL1(53,12),X'CF88008518',C'HSHS'   HISTORY UPDATE          
         DC    X'03',AL1(53,02),X'CF880085C0',C'    '           CHANGE          
         DC    X'03',AL1(53,03),X'CF880085C0',C'    '           DISPLAY         
         DC    X'03',AL1(53,04),X'CF880085C0',C'    '           DELETE          
         DC    X'03',AL1(53,05),X'CF88008518',C'HSHS'           SELECT          
         DC    X'03',AL1(53,06),X'CF880085C0',C'    '           RESTORE         
         DC    X'03',AL1(53,10),X'CF880085C0',C'    '           LIST            
         DC    X'03',AL1(53,12),X'CF88008518',C'HSHS'           REPORT          
*                                                                               
*****    DC    X'03',AL1(54,12),X'D386008718',C'CLCG'   COLGATE                 
         DC    X'03',AL1(54,02),X'D3860087C0',C'    '           CHANGE          
         DC    X'03',AL1(54,03),X'D3860087C0',C'    '           DISPLAY         
         DC    X'03',AL1(54,04),X'D3860087C0',C'    '           DELETE          
         DC    X'03',AL1(54,05),X'D386008718',C'CLCG'           SELECT          
         DC    X'03',AL1(54,06),X'D3860087C0',C'    '           RESTORE         
         DC    X'03',AL1(54,10),X'D3860087C0',C'    '           LIST            
         DC    X'03',AL1(54,12),X'D386008718',C'CLCG'           REPORT          
*                                                                               
******   DC    X'03',AL1(56,12),X'CB91009038',C'ALAL'   ANALYSIS                
         DC    X'03',AL1(56,02),X'CB910090C0',C'    '           CHANGE          
         DC    X'03',AL1(56,03),X'CB910090C0',C'    '           DISPLAY         
         DC    X'03',AL1(56,04),X'CB910090C0',C'    '           DELETE          
         DC    X'03',AL1(56,05),X'CB91009018',C'ALAL'           SELECT          
         DC    X'03',AL1(56,06),X'CB910090C0',C'    '           RESTORE         
         DC    X'03',AL1(56,10),X'CB910090C0',C'    '           LIST            
         DC    X'03',AL1(56,12),X'CB91009038',C'ALAL'           REPORT          
*                                                                               
******   DC    X'03',AL1(57,12),X'E592009338',C'PRWN'   PRICING                 
         DC    X'03',AL1(57,02),X'E5920093C0',C'    '           CHANGE          
         DC    X'03',AL1(57,03),X'E5920093C0',C'    '           DISPLAY         
         DC    X'03',AL1(57,04),X'E5920093C0',C'    '           DELETE          
         DC    X'03',AL1(57,05),X'E592009318',C'PRWN'           SELECT          
         DC    X'03',AL1(57,06),X'E5920093C0',C'    '           RESTORE         
         DC    X'03',AL1(57,10),X'E5920093C0',C'    '           LIST            
         DC    X'03',AL1(57,12),X'E592009338',C'PRWN'           REPORT          
*                                                                               
*******  DC    X'03',AL1(58,12),X'E020002038',C'WRW3'   TEST                    
         DC    X'03',AL1(58,02),X'E0200020C0',C'    '           CHANGE          
         DC    X'03',AL1(58,03),X'E0200020C0',C'    '           DISPLAY         
         DC    X'03',AL1(58,04),X'E0200020C0',C'    '           DELETE          
         DC    X'03',AL1(58,05),X'E020002018',C'WRW3'           SELECT          
         DC    X'03',AL1(58,06),X'E0200020C0',C'    '           RESTORE         
         DC    X'03',AL1(58,10),X'E0200020C0',C'    '           LIST            
         DC    X'03',AL1(58,12),X'E020002038',C'WRW3'           REPORT          
*                                                                               
*******  DC    X'03',AL1(59,12),X'CF88008118',C'HSHS'   I2                      
         DC    X'03',AL1(59,02),X'CF880081C0',C'    '           CHANGE          
         DC    X'03',AL1(59,03),X'CF880081C0',C'    '           DISPLAY         
         DC    X'03',AL1(59,04),X'CF880081C0',C'    '           DELETE          
         DC    X'03',AL1(59,05),X'CF88008118',C'HSHS'           SELECT          
         DC    X'03',AL1(59,06),X'CF880081C0',C'    '           RESTORE         
         DC    X'03',AL1(59,10),X'CF880081C0',C'    '           LIST            
         DC    X'03',AL1(59,12),X'CF88008118',C'HSHS'           REPORT          
*                                                                               
******** DC    X'03',AL1(60,12),X'CD96009538',C'CUCU'   CUTIN                   
         DC    X'03',AL1(60,02),X'CD960095C0',C'    '           CHANGE          
         DC    X'03',AL1(60,03),X'CD960095C0',C'    '           DISPLAY         
         DC    X'03',AL1(60,04),X'CD960095C0',C'    '           DELETE          
         DC    X'03',AL1(60,05),X'CD96009518',C'CUCU'           SELECT          
         DC    X'03',AL1(60,06),X'CD960095C0',C'    '           RESTORE         
         DC    X'03',AL1(60,10),X'CD960095C0',C'    '           LIST            
         DC    X'03',AL1(60,12),X'CD96009538',C'CUCU'           REPORT          
*                                                                               
******   DC    X'03',AL1(61,12),X'E020002038',C'WRW2'   WRITER(GOAL)            
         DC    X'03',AL1(61,02),X'E0200020C0',C'    '           CHANGE          
         DC    X'03',AL1(61,03),X'E0200020C0',C'    '           DISPLAY         
         DC    X'03',AL1(61,04),X'E0200020C0',C'    '           DELETE          
         DC    X'03',AL1(61,05),X'E020002018',C'WRW2'           SELECT          
         DC    X'03',AL1(61,06),X'E0200020C0',C'    '           RESTORE         
         DC    X'03',AL1(61,10),X'E0200020C0',C'    '           LIST            
         DC    X'03',AL1(61,12),X'E020002038',C'WRW2'           REPORT          
*                                                                               
******** DC    X'03',AL1(62,12),X'E020002038',C'WRW2'   WRITER(PROG)            
         DC    X'03',AL1(62,02),X'E0200020C0',C'    '           CHANGE          
         DC    X'03',AL1(62,03),X'E0200020C0',C'    '           DISPLAY         
         DC    X'03',AL1(62,04),X'E0200020C0',C'    '           DELETE          
         DC    X'03',AL1(62,05),X'E020002018',C'WRW2'           SELECT          
         DC    X'03',AL1(62,06),X'E0200020C0',C'    '           RESTORE         
         DC    X'03',AL1(62,10),X'E0200020C0',C'    '           LIST            
         DC    X'03',AL1(62,12),X'E020002038',C'WRW2'           REPORT          
*                                                                               
*******  DC    X'03',AL1(63,12),X'D276007738',C'W5W5'   KRTAPE                  
         DC    X'03',AL1(63,02),X'D2760077C0',C'    '           CHANGE          
         DC    X'03',AL1(63,03),X'D2760077C0',C'    '           DISPLAY         
         DC    X'03',AL1(63,04),X'D2760077C0',C'    '           DELETE          
         DC    X'03',AL1(63,05),X'D276007718',C'W5W5'           SELECT          
         DC    X'03',AL1(63,06),X'D2760077C0',C'    '           RESTORE         
         DC    X'03',AL1(63,10),X'D2760077C0',C'    '           LIST            
         DC    X'03',AL1(63,12),X'D276007738',C'W5W5'           REPORT          
*                                                                               
*                                                                               
         DC    X'03',AL1(64,01),X'F9B600B7C0',C'    '   GP      ADD             
         DC    X'03',AL1(64,02),X'F9B600B7C0',C'    '           CHANGE          
         DC    X'03',AL1(64,03),X'F9B600B7C0',C'    '           DISPLAY         
         DC    X'03',AL1(64,04),X'F9B600B7C0',C'    '           DELETE          
         DC    X'03',AL1(64,05),X'F9B600B718',C'GPWP'           SELECT          
         DC    X'03',AL1(64,06),X'F9B600B7C0',C'    '           RESTORE         
         DC    X'03',AL1(64,10),X'F9B600B7C0',C'    '           LIST            
         DC    X'03',AL1(64,12),X'F9B600B738',C'GPWP'           REPORT          
*                                                                               
********             DOWNLOADABLE VERSION PHEADER       EHEADER                 
         DC    X'03',AL1(65,01),X'D8520052C0',C'    '           ADD             
         DC    X'03',AL1(65,02),X'D8520052C0',C'    '           CHANGE          
         DC    X'03',AL1(65,03),X'D8520052C0',C'    '           DISPLAY         
         DC    X'03',AL1(65,04),X'D8520052C0',C'    '           DELETE          
         DC    X'03',AL1(65,05),X'D852005218',C'EHWE'           SELECT          
         DC    X'03',AL1(65,06),X'D8520052C0',C'    '           RESTORE         
         DC    X'03',AL1(65,10),X'D8520052C0',C'    '           LIST            
         DC    X'03',AL1(65,12),X'D852005278',C'EHWE'           REPORT          
*              (NOTE) WR                                RESEARCH SYSTEM         
*              (NOTE) WC WF WG WI WL WP WU WW           WUNDERMAN               
         DC    X'FF'                                                            
         SPACE 1                                                                
*              PHASES CURRENTLY IN USE                                          
         SPACE 1                                                                
*              0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F                   
*    T3200X    Y     Y  Y* Y* Y* Y  Y* Y*                Y                      
*        1        Y  Y  Y  Y        Y  Y  Y  Y  Y  S  S  S  S                   
*        2        Y*             Y  Y  Y* Y  Y  Y  Y                            
*        3     Y  Y           Y  Y  Y  Y  Y        Y                            
*        B        Y*                                                            
*        E        Y  Y  Y  Y  Y  Y  Y                                           
*        F     S  Y  Y  Y  Y     Y  Y  Y  Y  Y  Y  S  S  S  Y                   
         SPACE 1                                                                
*      * T32003 DPG CODE FOR N2                                                 
*      * T32004 DPG CODE FOR N3                                                 
*      * T32005 DPG CODE FOR N4                                                 
*      * T32007 DPG CODE FOR N5                                                 
*      * T32008 DPG CODE FOR COMMERCIAL CHECKING                                
*      * T320B1 DEFINES DRIVER GLOBAL STORAGE                                   
*      * T32028 DPG CODE FOR N7                                                 
*      * T32038-9 SPECIAL RATE BREAKOUT REPORT                                  
*      * T32009 DPG CODE FOR PNG                                                
*      * T32074 DPG CODE FOR HD HOMEDEPOT                                       
*      * T32093 DPG CODE FOR PRICING REPORT                                     
         SPACE 1                                                                
*              NEGENINCLS                                                       
*              DDCOMFACS                                                        
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE NAVDSECTS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE NEWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE NETBLKXTND                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'086NEWRI00   03/14/18'                                      
         END                                                                    
