*          DATA SET NEWRI00S   AT LEVEL 065 AS OF 05/01/02                      
*          DATA SET NEWRI00    AT LEVEL 179 AS OF 02/27/96                      
*PHASE T32000A,+0                                                               
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
*      ' (3000 BYTES)'                                                          
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
         EJECT                                                                  
T32000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 2988,**T32000,RR=R2                                              
         ST    R2,RELO                                                          
         LR    R6,R1                                                            
         L     RA,4(R6)          ATWA                                           
         USING T320FFD,RA                                                       
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
         AH    R9,=H'3000'         GRABBING 1 3000 BYTE I/O AREAS               
         LA    R9,8(R9)            NEED SPACE FOR 1 8BYTE LABEL                 
         USING NETSYSD,R9                                                       
         ST    R6,SYSPARMS                                                      
         L     R0,0(R6)            FOR CURSOR POSITIONING                       
         ST    R0,ATIOB                                                         
         SPACE 1                                                                
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT                  
         ST    R9,ASYSD                                                         
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         BAS   RE,CHKSEC           CHECK/SET SECURITY                           
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
         SPACE 1                                                                
*                                  SET SYSTEM DEPENDENT VALUES                  
         MVI   SYSTEM,C'N'         NETWORK                                      
         MVI   FILTIDNO,2          PROGRAM FILTER FIELD ID NUMBER               
         MVI   FRSTLAST,C'N'       ONLY CALL REPORT MODULES ONCE                
         MVI   MAXIOS,1            USES 1 I/O AREA                              
         MVI   NTWA,1              N'SAVE STORAGE                               
         MVC   SIZEIO,=F'3000'     EACH I/O IS 3000 BYTES                       
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
         MVC   SIZEIO,=F'3000'     EACH I/O IS 3000 BYTES                       
         MVC   LWORK,=F'25000'     WE TOOK 25000 BYTES IN NMOD                  
         MVC   RCPROG(2),=C'NE'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9032000'   PRESET FOR SYSTEM CALLOVS                
         L     R1,=A(RECACT)       RECORD/ACTION DIRECTORY                      
         A     R1,RELO                                                          
         ST    R1,ARECACT                                                       
         OI    GENSTAT1,RDUPAPPL                                                
         OI    GENSTAT5,GENPRVAL   GENCON SETS PREVAL BIT IN                    
*                                  SAVED REQUEST REOCRD                         
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
CHKSEC   NTR1                            CHECK SECURITY                         
         CLI   TWAOFFC,C'*'              DDS TERMINAL                           
         BE    CHK30                    SKIP SECURITY CHECK                     
*******************************************************************             
         MVI   NDSECFLG,0                                                       
         XC    KEY,KEY             CHECK FOR SECURITY IN N0 PROFILE             
         MVC   KEY(4),=C'S0N0'                                                  
         MVC   KEY+4(2),TWAAGY                                                  
         MVI   KEY+6,C'N'                                                       
         L     R6,SYSPARMS                                                      
         L     R1,16(R6)           A(COMFACS)                                   
         USING COMFACSD,R1                                                      
         MVC   DATAMGR,CDATAMGR                                                 
         GOTO1 =V(GETPROF),DMCB,KEY,(C'M',NBUSER),DATAMGR,RR=YES                
         CLI   NBUSER+5,C'Y'       SECURITY PROFILE SET ?                       
         BNE   CHK20               NO                                           
         TM    TWAAUTH,X'40'       UNLESS AUTORIZATION ?                        
         BO    CHK20                                                            
         OI    GENSTAT4,NODELLST   GENCON NOT ALLOW DELETE FROM LIST            
*                                  IN SAVED REQUEST RECORD                      
         OI    GENSTAT5,NOCHGLST   GENCON NOT ALLOW CHANGE FROM LIST            
*                                  IN SAVED REQUEST RECORD                      
CHK20    XC    NBUSER,NBUSER       CLEAR NBUSER                                 
********************************************************************            
                                                                                
CHK30    DS    0H                                                               
         B     XIT                                                              
         DROP  R1                                                               
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
***->>   DC    X'04',C'COMCHECK',AL1(08),X'0000'                                
         DC    X'04',C'P3      ',AL1(09),X'0000'                                
         DC    X'04',C'PILSBURY',AL1(09),X'0000'                                
         DC    X'04',C'PB1     ',AL1(10),X'0000'                                
         DC    X'04',C'BBDOPRE ',AL1(10),X'0000'                                
         DC    X'04',C'P4      ',AL1(11),X'00CF'                                
         DC    X'04',C'GEPOST  ',AL1(11),X'00CF'                                
***      DC    X'04',C'FX      ',AL1(12),X'00CF'                                
         DC    X'04',C'DEMOSEED',AL1(13),X'00CF'                                
***->    DC    X'04',C'N6      ',AL1(14),X'00CF'                                
         DC    X'04',C'N7      ',AL1(15),X'00CF'                                
****->>  DC    X'04',C'N8      ',AL1(16),X'00CF'                                
         DC    X'04',C'DAYPART ',AL1(17),X'00CF'                                
         DC    X'04',C'P5      ',AL1(18),X'00CF'                                
         DC    X'04',C'SRATE   ',AL1(19),X'00CF'                                
         DC    X'04',C'WRITER  ',AL1(20),X'00CF'                                
         DC    X'04',C'FLOWCHRT',AL1(21),X'00CF'                                
         DC    X'04',C'CX      ',AL1(22),X'00CF'                                
         DC    X'04',C'PREBILL ',AL1(23),X'00CF'                                
         DC    X'04',C'CCTRANS ',AL1(24),X'00CF'                                
         DC    X'04',C'BRMFLOW ',AL1(25),X'00CF'                                
         DC    X'04',C'FLIPPER ',AL1(26),X'00CF'                                
         DC    X'04',C'BRMECOST',AL1(27),X'00CF'                                
         DC    X'04',C'PECOST  ',AL1(27),X'00CF'                                
         DC    X'04',C'PNG     ',AL1(28),X'00CF'                                
         DC    X'04',C'GFTAPE  ',AL1(29),X'00CF'                                
         DC    X'04',C'TEXACO  ',AL1(30),X'00CF'                                
         DC    X'04',C'DELPROD ',AL1(31),X'00CF'                                
         DC    X'04',C'MMTRANS ',AL1(32),X'00CF'                                
         DC    X'04',C'UPDUNT  ',AL1(33),X'00CF'                                
         DC    X'04',C'PWRITER ',AL1(34),X'00CF'                                
         DC    X'04',C'PHEADER ',AL1(35),X'00CF'                                
         DC    X'04',C'EDI     ',AL1(36),X'00CF'                                
         DC    X'04',C'CPACK   ',AL1(37),X'00CF'                                
         DC    X'04',C'TTRANS  ',AL1(38),X'00CF'                                
         DC    X'04',C'BINTF   ',AL1(39),X'00CF'                                
         DC    X'04',C'CPM     ',AL1(40),X'00CF'                                
         DC    X'04',C'BJNY    ',AL1(41),X'00CF'                                
         DC    X'04',C'PUPTRANS',AL1(42),X'00CF'                                
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
         DC    X'01',C'HELP    ',AL1(00),X'00CF'                                
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
         DC    X'03',AL1(01,12),X'F111001138',C'SPWA'   SPECIAL                 
         DC    X'03',AL1(02,12),X'F212001238',C'AWWA'   ACCOUNT WRITER          
         DC    X'03',AL1(03,12),X'F313001338',C'N2WN'   N2                      
         DC    X'03',AL1(04,12),X'F414001438',C'N3WN'   N3                      
         DC    X'03',AL1(05,12),X'F515001538',C'N4WN'   N4                      
         DC    X'03',AL1(08,12),X'F818001838',C'COWT'   COMMERCIAL              
         DC    X'03',AL1(09,12),X'F919002938',C'P3WP'   P3 (PILLSBURY)          
         DC    X'03',AL1(10,12),X'FA1A002A38',C'PBWP'   PB1 (BBDO PRE)          
         DC    X'03',AL1(11,12),X'FB1B002B38',C'P4WP'   P4 (GE)                 
***      DC    X'03',AL1(12,12),X'E121002138',C'FXWX'   FILE FX                 
         DC    X'03',AL1(13,12),X'E222002318',C'DSDS'   DEMOSEED                
***->    DC    X'03',AL1(14,12),X'E324002538',C'N6WN'   N6-PROD GOALS           
         DC    X'03',AL1(15,12),X'E427002738',C'N7WN'   N7 J-J REPORT           
***->    DC    X'03',AL1(16,12),X'E535003038',C'N8WN'   N8 MISS/MG              
         DC    X'03',AL1(17,12),X'E631003138',C'DAWP'   DAYPART WEEKLY          
         DC    X'03',AL1(18,12),X'E736003718',C'P5P5'   P5                      
         DC    X'03',AL1(19,12),X'E838003918',C'SRWP'   SRATE                   
         DC    X'03',AL1(20,12),X'E020002038',C'WRW2'   WRITER                  
         DC    X'03',AL1(21,12),X'EC2C003C38',C'FLSW'   FLOWCHART               
         DC    X'03',AL1(22,12),X'D9C900A938',C'CXSW'   CX TAPE                 
         DC    X'03',AL1(23,12),X'E020002038',C'WRW1'   PREBILL                 
         DC    X'03',AL1(24,12),X'E931003118',C'CKWS'   COKE TRANSFER           
         DC    X'03',AL1(25,12),X'ED3A003B38',C'FLSW'   BRMFLOW                 
         DC    X'03',AL1(26,12),X'EA40004038',C'FPW9'   FLIPPER                 
         DC    X'03',AL1(27,12),X'EB41004238',C'BEWB'   BRMECOST                
         DC    X'03',AL1(27,12),X'EB41004238',C'PEWB'   PECOST(= BRME)          
         DC    X'03',AL1(28,12),X'D143004638',C'PGWN'   PNG REPORT              
         DC    X'03',AL1(29,12),X'D244004538',C'GFGG'   GF TAPE                 
         DC    X'03',AL1(30,12),X'D347004818',C'TXNX'   TEXACO                  
         DC    X'03',AL1(31,12),X'D549004918',C'TXWB'   DELPROD                 
         DC    X'03',AL1(32,12),X'D650005018',C'TXWB'   MMTRANS                 
         DC    X'03',AL1(33,12),X'D751005118',C'TXWQ'   UPDUNT                  
         DC    X'03',AL1(34,12),X'D020002038',C'WRW2'   PUP WRITER              
         DC    X'03',AL1(35,12),X'D852005278',C'PHWH'   PKG HEADER              
         DC    X'03',AL1(36,12),X'D353005318',C'EDED'   EDI                     
         DC    X'03',AL1(37,12),X'DA54005418',C'CPWD'   CPACK                   
         DC    X'03',AL1(38,12),X'DC65006538',C'TXWZ'   TTRANS(TALENT)          
         DC    X'03',AL1(39,12),X'DB32003218',C'TXWQ'   BINTF                   
         DC    X'03',AL1(40,12),X'DD64006438',C'CMW0'   CPM                     
         DC    X'03',AL1(41,12),X'DD66006618',C'CMBJ'   BJNY                    
         DC    X'03',AL1(42,12),X'DE67006718',C'CMPU'   PUP                     
         DC    X'03',AL1(43,12),X'DF68006818',C'JWWJ'   SCJMTAPE                
         DC    X'03',AL1(44,12),X'DF69006918',C'JWNK'   SCJITAPE                
         DC    X'03',AL1(45,12),X'EF70007038',C'TXXX'   UTILS                   
         DC    X'03',AL1(46,12),X'DF71007138',C'TXBJ'   BJTAPE                  
         DC    X'03',AL1(47,12),X'EE72007238',C'CBW4'   CABRCP                  
         DC    X'03',AL1(48,12),X'E173007338',C'TXHD'   HOMEDEPOT (HD)          
         DC    X'03',AL1(49,12),X'CF83008318',C'ATAT'   CAT                     
         DC    X'03',AL1(50,12),X'E020002038',C'TRTR'   TRANSMIT                
         DC    X'03',AL1(51,12),X'CA56005618',C'TRXX'   PROGRAM REC DEL         
         DC    X'03',AL1(52,12),X'E020002038',C'CFFL'   CFAFTER                 
         DC    X'03',AL1(53,12),X'CF88008518',C'HSHS'   HISTORY UPDATE          
         DC    X'03',AL1(54,12),X'D386008718',C'CLCG'   COLGATE                 
******   DC    X'03',AL1(55,12),X'CF88008938',C'AUAU'   AUDIT REPORT            
         DC    X'03',AL1(56,12),X'CB91009038',C'ALAL'   ANALYSIS                
         DC    X'03',AL1(57,12),X'E592009338',C'PRWN'   PRICING                 
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
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE NEWRIFFD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065NEWRI00S  05/01/02'                                      
         END                                                                    
