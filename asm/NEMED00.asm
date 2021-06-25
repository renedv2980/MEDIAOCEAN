*          DATA SET NEMED00    AT LEVEL 022 AS OF 02/25/20                      
*          DATA SET NEMED00    AT LEVEL 114 AS OF 08/24/95                      
*PHASE T31E00B,+0                                                               
*INCLUDE KHDUMMY                                                                
         TITLE 'T31E00 - NETWORK SPOOL CONTROLLER'                              
         SPACE 3                                                                
* ORGANIZATION OF MEMORY: USED BY ENTIRE NETWORK SYSTEM                         
*                                                                               
*      23904 BYTES TAKEN IN NMOD TO BE IN SYNC WITH NEWRI00                     
*      FOR EXPANDED I/O AREA TO 3000                                            
*      DON'T KNOW WHY COCKAMAMI NUMBER THERE IN FIRST PLACE BUT I'M             
*      AFRAID TO CLEAN IT UP - BETTER TO LEAVE AS IS - PXZ 4/30/96              
*                                                                               
*                                      TWA                                      
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
*      ' APPLICATION '                                                          
*      '   COMMON    '                                                          
*      '  (W/S 1)    '                                                          
*      ' (2008 BYTES)                                                           
*      '-------------'                                                          
*      ' LOCAL W/S   '                                                          
*      '   (W/S 2)   '                                                          
*      '-------------'                                                          
*                                                                               
* 11/17 CHANGED NMOD FROM 4125 TO 4325 TO GIVE MORE STORAGE                     
* AT END FOR NEW EXTEND BLOCK TO PASS VIA NETBLOCK (SCHT)                       
*                                                                               
         EJECT                                                                  
T31E00   CSECT                                                                  
         PRINT NOGEN                                                            
*        NMOD1 4125,**T31E00,RR=R2,CLEAR=YES                                    
         NMOD1 4325,**T32000,RR=R2,CLEAR=YES  NEW EXTEND BLOCK                  
         ST    R2,RELO                                                          
         LR    R6,R1                                                            
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         LR    RE,R8                                                            
         LA    RF,2500             CLEAR SELECTED STORAGE                       
         SLL   RF,3                                                             
         XCEF                                                                   
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         SPACE 1                                                                
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         LA    R9,IO                                                            
**       AH    R9,=H'3000'         GRABBING 1 3000 BYTE I/O AREAS               
**       LA    R9,16(R9)           NEED SPACE FOR 1 8BYTE LABEL                 
         AH    R9,=H'12000'        GRABBING 2 6000 BYTE I/O AREAS               
         LA    R9,16(R9)           NEED SPACE FOR 2 8BYTE LABEL                 
         USING NETSYSD,R9                                                       
         ST    R6,SYSPARMS                                                      
         SPACE 1                                                                
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    R9,ASYSD                                                         
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
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
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QMSUNPK                                                   
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   MSUNPK,DMCB                                                      
         SPACE 1                                                                
         XC    DMCB,DMCB                  FOR A(CLUNPK)                         
         MVC   DMCB+4(4),=X'D9000A15'                                           
         LA    R1,DMCB                                                          
         BASR  RE,RF                                                            
         MVC   CLUNPK,DMCB                                                      
         SPACE 1                                                                
         LA    R2,NDRVBEND         ADDRESSES OF WORKING STORAGE                 
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
         MVI   FRSTLAST,C'N'       ONLY CALL REPORT MODULES ONCE                
         MVI   MAXIOS,2            USES 1 I/O AREA                              
         MVI   NTWA,1              N'SAVE STORAGE                               
         MVC   SIZEIO,=F'6000'     EACH I/O IS 3000 BYTES                       
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
         MVC   SIZEIO,=F'6000'     EACH I/O IS 2000 BYTES                       
         MVC   LWORK,=F'33000'     WE TOOK 33000 (23904)BYTES IN NMOD           
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9031E00'   PRESET FOR SYSTEM CALLOVS                
         L     R1,=A(RECACT)       RECORD/ACTION DIRECTORY                      
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         B     XIT                                                              
         SPACE 1                                                                
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
         DC    X'01',C'STEWARD ',AL1(01),X'0031'                                
         DC    X'01',C'WEEKLY  ',AL1(02),X'0032'                                
         DC    X'01',C'LIST    ',AL1(03),X'0033'                                
         DC    X'01',C'EFF     ',AL1(04),X'0034'                                
         DC    X'01',C'CALENDAR',AL1(05),X'0035'                                
         DC    X'01',C'OVERNITE',AL1(06),X'0036'                                
         DC    X'01',C'SEED    ',AL1(07),X'0037'                                
         DC    X'01',C'FILEFIX ',AL1(08),X'0038'                                
         DC    X'01',C'GRP FLOW',AL1(09),X'0039'                                
         DC    X'01',C'ARMY    ',AL1(10),X'003A'                                
         DC    X'01',C'MKTFIX  ',AL1(11),X'003B'                                
         DC    X'01',C'FIS     ',AL1(12),X'003C'                                
         DC    X'01',C'AGYSUM  ',AL1(13),X'003D'                                
         DC    X'01',C'BPREP   ',AL1(14),X'003E'                                
         DC    X'01',C'EGS     ',AL1(15),X'003F'                                
         DC    X'01',C'UNITLIST',AL1(40),X'0070'                                
         DC    X'01',C'UPDATE  ',AL1(41),X'0071'                                
         DC    X'01',C'HUTLIST ',AL1(42),X'0072'                                
         DC    X'01',C'PROGLIST',AL1(43),X'0073'                                
*****->  DC    X'01',C'PACKAGE ',AL1(44),X'0074'                                
         DC    X'01',C'NETLOCK ',AL1(44),X'0074'                                
         DC    X'01',C'SUMMARY ',AL1(45),X'0075'                                
         DC    X'01',C'POST    ',AL1(46),X'0076'                                
*****->  DC    X'01',C'PERFORM ',AL1(47),X'0077'                                
         DC    X'01',C'DEMO    ',AL1(48),X'0078'                                
****->   DC    X'01',C'ESTIMATE',AL1(49),X'0079'                                
         DC    X'01',C'BRAND   ',AL1(50),X'007A'                                
*****->  DC    X'01',C'THREE   ',AL1(51),X'007B'                                
         DC    X'01',C'SCJFILE ',AL1(51),X'007B'                                
         DC    X'01',C'SCHEDULE',AL1(52),X'007C'                                
         DC    X'01',C'ECOST   ',AL1(53),X'007D'                                
         DC    X'01',C'DAYPART ',AL1(54),X'007E'                                
         DC    X'01',C'ACCOUNT ',AL1(55),X'007F'                                
         DC    X'01',C'PPOST   ',AL1(56),X'007F'                                
         DC    X'01',C'GOAL    ',AL1(57),X'007F'                                
****->   DC    X'01',C'AUDIENCE',AL1(58),X'007F'                                
         DC    X'01',C'DRIVER  ',AL1(59),X'0080'                                
         DC    X'01',C'UNIVLIST',AL1(60),X'0081'                                
         DC    X'01',C'PAV     ',AL1(61),X'0082'                                
         DC    X'01',C'PFLOW   ',AL1(62),X'0082'                                
         DC    X'01',C'CXTAPE  ',AL1(63),X'0082'                                
         DC    X'01',C'CLOSENET',AL1(65),X'0082'                                
         DC    X'01',C'DELPAK  ',AL1(66),X'0082'                                
         DC    X'01',C'UNCLOSE ',AL1(67),X'0082'                                
         DC    X'01',C'BBAL    ',AL1(69),X'0082'                                
         DC    X'01',C'YNRTAPE ',AL1(70),X'0082'                                
         DC    X'01',C'REALLOC ',AL1(71),X'0082'                                
         DC    X'01',C'QUICKFIX',AL1(72),X'0082'                                
         DC    X'01',C'AUTOSEED',AL1(73),X'0082'                                
         DC    X'01',C'ONEDSEED',AL1(73),X'0082'                                
         DC    X'01',C'ONEWSEED',AL1(73),X'0082'                                
         DC    X'01',C'THRDSEED',AL1(73),X'0082'                                
         DC    X'01',C'SJNFILE ',AL1(74),X'007B'                                
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
         SPACE 2                                                                
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
         SPACE 1                                                                
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
         DC    X'03',AL1(01,12),X'F1011121D8',C'STOW'  *** ON CODES             
         DC    X'03',AL1(02,12),X'F2021222D8',C'WEOW'                           
         DC    X'03',AL1(03,12),X'F3031323D8',C'LIO3'                           
         DC    X'03',AL1(04,12),X'F4041424F8',C'EFOD'                           
         DC    X'03',AL1(05,12),X'F505152518',C'CAOC'                           
         DC    X'03',AL1(06,12),X'F606162638',C'OVOO'                           
         DC    X'03',AL1(07,12),X'F707172738',C'SEOR'                           
         DC    X'03',AL1(08,12),X'DCCD51AA18',C'EQOQ'   FILEFIX                 
         DC    X'03',AL1(09,12),X'F909002918',C'GRO0'   NEW GRP FLOW            
         DC    X'03',AL1(10,12),X'F70A1A2A18',C'AROY'   ARMY TAPE               
         DC    X'03',AL1(11,12),X'FB0B000B18',C'MPO2'                           
         DC    X'03',AL1(12,12),X'FC0C1C2CD8',C'FIOE'                           
         DC    X'03',AL1(13,12),X'FD0D1D2DD8',C'AGOG'                           
         DC    X'03',AL1(14,12),X'FE0E1E2E18',C'BPOX'                           
         DC    X'03',AL1(15,12),X'4F0F1F2FD8',C'EGOZ'                           
         DC    X'03',AL1(40,12),X'E0405060D8',C'ULOL'                           
         DC    X'03',AL1(41,12),X'E141516118',C'UPOU'    UPDATE                 
         DC    X'03',AL1(42,12),X'E2425262D8',C'HEOH'                           
         DC    X'03',AL1(43,12),X'E343536338',C'PLO2'                           
         DC    X'03',AL1(44,12),X'E444546438',C'PAO5'                           
         DC    X'03',AL1(45,12),X'E5455565D8',C'SUO6'                           
         DC    X'03',AL1(46,12),X'E6465666D8',C'POOP'                           
         DC    X'03',AL1(47,12),X'E7475767F8',C'PEOB'                           
         DC    X'03',AL1(48,12),X'E848586838',C'DOOM'                           
         DC    X'03',AL1(49,12),X'E9495969D8',C'ESOE'                           
         DC    X'03',AL1(50,12),X'EA4A5A6AD8',C'BRO7'                           
**       DC    X'03',AL1(51,12),X'EB4B5B6BD8',C'THOT'    THREE IN ONE           
         DC    X'03',AL1(51,12),X'EB4B006B18',C'THOT'    SCJFILE                
         DC    X'03',AL1(52,12),X'EC4C5C6CD8',C'SKOS'                           
         DC    X'03',AL1(53,12),X'ED4D5D6D38',C'ECO8'                           
         DC    X'03',AL1(54,12),X'D080008038',C'DAO9'                           
         DC    X'03',AL1(55,12),X'D18191A1D8',C'ACOL'                           
         DC    X'03',AL1(56,12),X'D282008238',C'PPO9'                           
         DC    X'03',AL1(57,12),X'D3830083D8',C'GOOI'                           
         DC    X'03',AL1(58,12),X'D4840084F8',C'AUOD'                           
         DC    X'03',AL1(59,12),X'D585008518',C'DROD'                           
         DC    X'03',AL1(60,12),X'D6860086F8',C'GROM'                           
         DC    X'03',AL1(61,12),X'D7A700A738',C'AVOD'                           
         DC    X'03',AL1(62,12),X'D8A800A8D8',C'PFOA'                           
         DC    X'03',AL1(63,12),X'D9C900A918',C'CXOJ' *** SEE YNRTAPE           
         DC    X'03',AL1(65,12),X'DACA00CB18',C'CLOK'                           
         DC    X'03',AL1(66,12),X'DBCC00CC38',C'DEON'                           
         DC    X'03',AL1(67,12),X'DDCE00CE18',C'UNOZ'                           
         DC    X'03',AL1(68,12),X'DE34003418',C'N2OK'                           
         DC    X'03',AL1(69,12),X'EE870087D8',C'BBOK'                           
         DC    X'03',AL1(70,12),X'DE3400B218',C'YRO1'  *** YNRTAPE              
         DC    X'03',AL1(71,12),X'F888008818',C'UARA'  *** REALLOCATE           
         DC    X'03',AL1(72,12),X'DC89008918',C'NQQF'  QUICKFIX                 
         DC    X'03',AL1(73,12),X'F707172718',C'SEOR'  AUTOSEED                 
         DC    X'03',AL1(74,12),X'EB4B006E18',C'THOT'    SJNFILE                
*                                                                               
         DC    X'FF'                                                            
         SPACE 1                                                                
*              NETINCLS                                                         
*              DDCOMFACS                                                        
*              DDCOREQUS                                                        
         PRINT OFF                                                              
       ++INCLUDE NEGENINCLS                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NEMED00   02/25/20'                                      
         END                                                                    
