*          DATA SET SRTTR00    AT LEVEL 003 AS OF 03/24/15                      
*PHASE T12400A                                                                  
*INCLUDE FAXPTAB                                                                
*&&      SET   NOP=N                                                            
         IEABRCX DEFINE                                                         
         IEABRCX DISABLE                                                        
*                                                                               
         TITLE '=TTR/=ADRFILE - DISPLAY ADRFILE EVENT LOG'                      
TTRC     CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WRKX-WRKD,**=TTR**,RR=RE,CLEAR=YES                               
         USING WRKD,RC                                                          
         J     START                                                            
*                                                                               
$$DATA   LOCTR ,                   DATA LOCATION CLOSE TO RB                    
                                                                                
***********************************************************************         
* RB = BASE1 LITERALS AND CONSTANTS                                   *         
* RA = RESERVED                                                       *         
* R9 = LOCAL BASE - USE WHEN NEEDED                                   *         
* CONSTANTS & LITERALS DEFINE LATER USING $$DATA LOCTR                *         
***********************************************************************         
$$CODE   LOCTR ,                   CODE STARTS AFTER DATA                       
*                                                                               
START    ST    RE,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RD,BASERD                                                        
         ST    RB,BASERB                                                        
         USING SRPARMD,R1                                                       
         MVC   SRPARS,0(R1)        SAVE PARMS                                   
*                                                                               
         L     R8,SRPAR6           A(TWA)                                       
         USING SRTTRFFD,R8                                                      
*                                                                               
         L     RF,SRPAR1           A(SYSFAC)                                    
         USING SYSFACD,RF                                                       
         MVC   ASELIST,VSELIST     COPY FROM SYSFACS                            
         MVC   ASSB,VSSB                                                        
         MVC   ATEMPTRC,VTEMPTRC                                                
*                                                                               
         L     RE,VADRBUFF         INIT BUFFER DETAIL                           
         ST    RE,AADRBUFF                                                      
         AHI   RE,-12                                                           
         ST    RE,AADRNEXT                                                      
         AHI   RE,-4                                                            
         MVC   BUFFMAX,0(RE)       GET RECORDS PER BLOCK                        
         MVC   BUFFREC,2(RE)       GET RECORD LENGTH                            
         LH    R0,BUFFMAX                                                       
         MH    R0,BUFFREC                                                       
         STH   R0,BUFFLEN          SET BLOCK LENGTH                             
*                                                                               
         L     RF,SRPAR4           COPY FROM COMFACS                            
         USING COMFACSD,RF                                                      
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   AGETHELP,CGETHELP                                                
         MVC   AGETFACT,CGETFACT                                                
         MVC   AHEXIN,CHEXIN                                                    
         MVC   ATERMVAL,CTERMVAL                                                
         DROP  RF                                                               
*                                                                               
         L     RF,SRQATIA          ASAVE=TIA                                    
         ST    RF,ASAVE                                                         
*                                                                               
         L     RF,SRQAUTL          EXTRACT UTL DATA                             
         USING UTLD,RF                                                          
         ST    RF,AUTL                                                          
         MVC   TRM,TNUM                                                         
         MVC   MYLUID,TSYM                                                      
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         JZ    INIT010                                                          
         OI    TSVCREQ,X'02'                                                    
         MVI   FLAG,C'I'           SET FLAG TO INIT                             
*                                                                               
INIT010  L     RF,SRQATIOB         EXTRACT TIOB DATA                            
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID                                                    
         DROP  R1                                                               
         CLI   PFKEY,3             PF3=RESET                                    
         JNE   *+8                                                              
         MVI   FLAG,C'I'           SET FLAG TO INIT                             
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,TIOBCURS                                                    
         ST    RE,ACURS                                                         
         MVC   CURI,TIOBCURI                                                    
*                                                                               
INIT015  SR    RE,RE               PF1 HELP                                     
         ICM   RE,3,TIOBCURD                                                    
         AR    RE,R8                                                            
         CLI   PFKEY,1             TEST HELP PFKEY                              
         JNE   INIT020                                                          
         ST    RE,AHELP            SAVE A(HELP FIELD)                           
         MVI   PFKEY,0                                                          
*                                                                               
INIT020  L     R1,ASELIST          EXTRACT DNEXT OF ADRFILE                     
         LA    R1,6(R1)                                                         
         CLC   0(7,R1),=C'SERVICE'                                              
         JNE   *+2                 MAKE SURE                                    
*                                                                               
         ICM   RE,15,SEFILES-SELISTD(R1)                                        
         L     RE,0(RE)                                                         
         SR    RF,RF                                                            
         ICM   RF,3,2(RE)          NUMBER OF FILES IN RF                        
         LA    RE,4(RE)                                                         
*                                                                               
INIT025  SR    R1,R1                                                            
         ICM   R1,7,5(RE)                                                       
         USING DTFPHD,R1                                                        
         CLI   DTFXNUM,X'F3'       FIND ADRFILE                                 
         JE    INIT026                                                          
         LA    RE,8(RE)            NEXT FILE                                    
         JCT   RF,INIT025                                                       
         DC    H'0'                                                             
*                                                                               
INIT026  MVC   ADRDNEXT,DNEXT      SAVE DNEXT                                   
         DROP  R1                                                               
*                                                                               
INIT030  L     R1,=A(CTREC-WRKD)   SET IOAREA POINTERS                          
         AR    R1,RC                                                            
         ST    R1,ACTREC                                                        
         L     R1,=A(IOBUFFX-WRKD)                                              
         AR    R1,RC                                                            
         ST    R1,AIOBUFFX                                                      
*                                                                               
         L     RE,ASSB             EXTRACT SSB VALUES                           
         USING SSBD,RE                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   MYALET,SSBALET                                                   
         MVC   AFID(4),SSBAFID                                                  
         MVC   SYSNAME,SSBSYSN4                                                 
         MVC   SYSNAME(4),SSBSYSN4 ADV NAME                                     
         LLC   R0,SSBSYSIX         AORNUM/ADVNUM                                
         DROP  RE                                                               
         CLI   SYSNAME+3,C' '                                                   
         JNE   *+8                                                              
         MVI   SYSNAME+3,C'/'                                                   
         SRL   R0,4                                                             
         STC   R0,TORAOR                                                        
         LTR   R0,R0                                                            
         JNZ   *+12                                                             
         LHI   R0,X'A3'            SET TOR LETTER T                             
         J     *+8                                                              
         AHI   R0,X'80'            SET AOR LETTER A-H                           
         STC   R0,SYSNAME+4                                                     
*                                                                               
         GOTO1 AGETFACT,DMCB,0     GET A(SYSLIST)                               
         L     R1,0(R1)                                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         ST    R1,ASYSLST                                                       
*                                                                               
         LA    R1,SRVP1H           SET CURSOR                                   
         ST    R1,CURSOR                                                        
*                                                                               
         JAS   RE,READSTR          READ SAVED STR                               
         CLI   FLAG,C'I'                                                        
         JNE   INIT100             ALREADY INTIALISED                           
*                                                                               
         MVC   TOPDA,=X'00000000'  RESET SAVED STUFF                            
         MVC   BOTDA,=X'00000000'                                               
         MVC   SCREENAD,=X'00000000'                                            
         XC    TOPDISP,TOPDISP                                                  
         XC    BOTDISP,BOTDISP                                                  
         XC    SAVELEFT,SAVELEFT                                                
         EJECT                                                                  
***********************************************************************         
* HANDLE PFKEYS ETC                                                   *         
***********************************************************************         
INIT100  CLI   PFKEY,4             PF4 TRK/UP                                   
         JE    TRKUP                                                            
         CLI   PFKEY,5             PF5 TRK/DOWN                                 
         JE    TRKDOWN                                                          
*                                                                               
         CLI   PFKEY,7             PF7 UP                                       
         JNE   *+8                                                              
         MVI   FLAG,C'U'                                                        
*                                                                               
         CLI   PFKEY,0             ENTER=E                                      
         JNE   *+8                                                              
         MVI   FLAG,C'E'                                                        
*                                                                               
         CLI   PFKEY,8             PF8 DOWN                                     
         JNE   INIT110                                                          
*                                                                               
         MVI   FLAG,C'D'                                                        
         TM    SAVEFLAG,SAVEEOT    UNLESS EOT FLAGED                            
         JZ    *+8                                                              
         MVI   FLAG,C'E'           CONVERT TO ENTER                             
         J     INIT110                                                          
*                                                                               
TRKUP    SR    RE,RE                                                            
         ICM   RE,3,TOPDA          BACK UP 1 TRACK                              
         JZ    ERRTOP                                                           
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         JZ    ERRTOP                                                           
         STCM  RE,3,TOPDA                                                       
         MVI   TOPDA+2,X'01'                                                    
         MVI   FLAG,C'E'                                                        
         J     INIT110                                                          
*                                                                               
TRKDOWN  ICM   RE,3,TOPDA          DOWN 1 TRACK                                 
         LA    RE,1(RE)                                                         
         STCM  RE,3,TOPDA                                                       
         MVI   TOPDA+2,X'01'                                                    
         MVI   FLAG,C'E'                                                        
         J     INIT110                                                          
*                                                                               
INIT110  LH    R1,SAVELEFT                                                      
         CLI   PFKEY,10            PF10 LEFT                                    
         JNE   *+8                                                              
         AHI   R1,-40              LEFT 40                                      
         CLI   PFKEY,11            PF11 RIGHT                                   
         JNE   *+8                                                              
         LA    R1,40(R1)           RIGHT 40                                     
         CHI   R1,80                                                            
         JL    *+8                                                              
         LA    R1,80                                                            
         LTR   R1,R1                                                            
         JNM   *+8                                                              
         LA    R1,0                                                             
         STH   R1,SAVELEFT                                                      
         EJECT                                                                  
***********************************************************************         
* DID WE ENTER WITH =ADRFILE                                          *         
***********************************************************************         
         MVI   SHORT,0                                                          
         CLC   SRVID(4),=C'=TTR'   SPECIAL SHORTCUT                             
         JE    INIT200                                                          
*                                                                               
         LA    R1,SRVP1            VALIDATE FILTERS                             
         CLI   0(R1),C'?'          CHECK FOR HELP                               
         JNE   *+8                                                              
         ST    R1,AHELP                                                         
         BRAS  RE,VALFILT                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R3,SRVHDR           PRINT HEADER LINE                            
         MVC   PLINE,STITLE1                                                    
         CLI   FPCPC,0                                                          
         JE    *+10                                                             
         MVC   PLINE+120(40),STITLE1P                                           
         BRAS  RE,LINEOUT                                                       
         LA    R3,SRVHDR1          PRINT HEADER LINE                            
         MVC   PLINE,STITLE2                                                    
         CLI   FPCPC,0                                                          
         JE    *+10                                                             
         MVC   PLINE+120(40),STITLE2P                                           
         BRAS  RE,LINEOUT                                                       
*                                                                               
         JAS   RE,MAINADR          DISPLAY TRANSACTION LOGS                     
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WE ENTER WITH =TTR (SCREEN TRACE SORTCUTS)                          *         
***********************************************************************         
INIT200  MVI   TRACE,C'Y'          SET TO READ TRACE TABLE                      
         LA    R1,SRVID+5                                                       
         JAS   RE,SHORTGET         GET SHORTCUT                                 
*                                                                               
         CLI   SHORT,C'T'          SCREEN TRACE SERVICES  =TTR,TRACE            
         JE    INIT120                                                          
         CLI   SHORT,C'N'          SCREEN TRACE ON        =TTR,ON               
         JE    INIT120                                                          
         CLI   SHORT,C'F'          SCREEN TRACE OFF       =TTR,OFF              
         JNE   INIT130                                                          
*                                                                               
INIT120  JAS   RE,TWATRC           GOTO SCREEN TRACE                            
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WE ENTER WITH =TTR,L  LOCKS                                         *         
***********************************************************************         
INIT130  CLI   SHORT,C'L'          SHOW LOCK TRACE (OLD TEMPTRC)                
         JNE   INIT150                                                          
*                                                                               
         LA    R3,SRVHDR           PRINT HEADER LINE                            
         MVC   PLINE,TTITLE1                                                    
         BRAS  RE,LINEOUT                                                       
         LA    R3,SRVHDR1          PRINT HEADER LINE                            
         MVC   PLINE,TTITLE2                                                    
         BRAS  RE,LINEOUT                                                       
*                                                                               
*        JAS   RE,TEMPT            GOTO TEMP TRACE                              
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* WE ENTER WITH =TTR  (OTHER)                                         *         
***********************************************************************         
INIT150  CLI   SHORT,0                                                          
         JNE   INIT160                                                          
         BRAS  RE,VALTRC           VALIDATE P1                                  
         JNE   INIT160                                                          
*                                                                               
         JAS   RE,TRCACTS          DO ACTION IF FOUND                           
         J     EXIT                                                             
*                                                                               
INIT160  LA    R3,SRVHDR           PRINT HEADER LINE                            
         MVC   PLINE,STITLE1                                                    
         BRAS  RE,LINEOUT                                                       
         LA    R3,SRVHDR1          PRINT HEADER LINE                            
         MVC   PLINE,STITLE2                                                    
         BRAS  RE,LINEOUT                                                       
*                                                                               
         LA    R1,SRVP1            VALIDATE FILTERS                             
         CLI   0(R1),C'?'          CHECK FOR HELP                               
         JNE   *+8                                                              
         ST    R1,AHELP                                                         
         BRAS  RE,VALFILT                                                       
         JNE   EXIT                                                             
*                                                                               
         CLI   SHORT,C'M'          HARD CODED MY LUID                           
         JNE   *+14                                                             
         MVI   FLUIDC,X'80'                                                     
         MVC   FLUIDC+1(8),MYLUID                                               
*                                                                               
         JAS   RE,MAINTRC                                                       
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR AND EXITS                                                     *         
***********************************************************************         
ERRTOP   LA    R1,=C'TOP OF FILE     '                                          
         J     ERRX                                                             
ERRFIL   LA    R1,=C'ERROR ON ADRFILE'                                          
         J     ERRX                                                             
*                                                                               
ERRX     L     RD,BASERD                                                        
         LA    RF,SRVMSG                                                        
         MVC   0(16,RF),=C'ED/9999 XXXXX - '                                    
         MVC   8(5,RF),SYSNAME                                                  
         MVC   16(16,RF),0(R1)                                                  
*                                                                               
ERRXX    J     EXIT                                                             
*                                                                               
EXIT     OC    AHELP,AHELP         USE HELPOUT IF AHELP NOT ZERO                
         JNZ   HELPOUT                                                          
         JAS   RE,WRITESTR         WRITE SAVED STR                              
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         AND SET CURSOR                               
*                                                                               
EXIT1    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM TO READ ADRFILE                                        *         
***********************************************************************         
MAINADR  NTR1                                                                   
         MVI   SAVEFLAG,0                                                       
*                                                                               
         LA    R3,SRVLST1          START AT TOP                                 
         CLI   FLAG,C'D'           DOWN FLAG                                    
         JNE   MAIN005                                                          
         MVC   DA,BOTDA            USE BOTTOM OF PREVIOUS                       
         MVC   DISP,BOTDISP                                                     
         MVC   TOPDA,DA            SAVE THIS AS NEW TOP                         
         MVC   TOPDISP,DISP                                                     
         J     MAIN014                                                          
*                                                                               
MAIN005  MVC   DA,TOPDA            USE TOP OF PREVIOUS                          
         MVC   DISP,TOPDISP                                                     
         MVC   BOTDA,DA            SAVE THIS AS BOTTOM                          
         MVC   BOTDISP,DISP                                                     
         CLI   FLAG,C'U'           UP FLAG SET?                                 
         JNE   MAIN014             NO - REDISPLAY OLD SCREEN                    
*                                                                               
         LA    R3,SRVLSTX          START AT BOTTOM                              
         J     MAIN014                                                          
*                                                                               
MAIN010  LA    RF,DMRSEQ           READ NEXT                                    
         J     *+8                                                              
MAIN014  LA    RF,DMRDIR           READ AGAIN                                   
*                                                                               
         L     RE,ASSB                                                          
         L     RE,SSBTKADR-SSBD(RE)                                             
         CLC   TCBIOCNT-TCBD(3,RE),MAXIOS                                       
         JNL   ERRIO                                                            
*                                                                               
MAIN015  OC    DA,DA               ZERO DA MEANS BUFFER READ                    
         JNZ   MAIN016                                                          
*                                                                               
         L     RE,AADRBUFF         SOURCE BUFFER                                
         L     RF,AADRNEXT                                                      
         L     RF,0(RF)                                                         
         SR    RF,RE               CALCULATE LEN                                
         LA    R0,IOBUFF                                                        
         L     R1,AIOBUFFX                                                      
         SR    R1,R0                                                            
         MVCL  R0,RE                                                            
         J     MAIN017             SKIP READ OF COURSE                          
*                                                                               
MAIN016  GOTO1 ADATAMGR,DMCB,(RF),ADRFILE,DA,IOBUFF,IOBUFF                      
         CLC   DA,ADRDNEXT                                                      
         JH    MAIN016A                                                         
         CLI   8(R1),0                                                          
         JE    MAIN017                                                          
         TM    8(R1),X'80'         EOF - USE BUFFER ZONE                        
         JNZ   MAIN016A                                                         
         CLI   8(R1),X'10'         NOT FOUND - BIZZARE - SKIP IT                
         JE    MAIN017A                                                         
         J     ERRFIL              ANYTHING ELSE ERROR EXIT                     
*                                                                               
MAIN016A XC    DA,DA                                                            
         J     MAIN015                                                          
*                                                                               
MAIN017  LA    R2,IOBUFF                                                        
         BRAS  RE,SETRECS          SET UP DISPLACEMENTS IF NEC                  
         CLI   0(R2),C'*'                                                       
         JNE   MAIN018                                                          
*                                                                               
MAIN017A CLI   FLAG,C'U'           SKIP WHOLE ADR RECORD                        
         JE    SKIPUP01                                                         
         J     SKIP01                                                           
*                                                                               
MAIN018  A     R2,DISP             LOCATE RECORD                                
*                                                                               
MAIN020  LA    R1,IOBUFF           CHECK FILTERS                                
         SR    R2,R1                                                            
         ST    R2,DISP                                                          
         AR    R2,R1                                                            
         CLI   0(R2),C'$'                                                       
         JE    SKIP                                                             
         BRAS  RE,FILTERS                                                       
         JNE   SKIP                                                             
*                                                                               
MAIN025  BRAS  RE,ADRDISP          CREATE PLINE FOR OUTPUT                      
         BRAS  RE,LINEOUT          OUT IT                                       
*                                                                               
         CLI   FLAG,C'U'           IS THIS UP                                   
         JE    *+12                                                             
         AHI   R3,87               BUMP TO NEXT LINE                            
         J     *+8                                                              
         AHI   R3,-87              BACK TO PREV LINE                            
*                                                                               
SKIP     CLI   FLAG,C'U'           GOTO SKIPUP FOR UP                           
         JE    SKIPUP                                                           
*                                                                               
         AH    R2,BUFFREC          NEXT RECORD                                  
         LA    R1,SRVLSTX                                                       
         CR    R3,R1               TEST FOR END OF SCREEN                       
         JH    MAINX                                                            
         C     R2,AIOBUFFX         TEST FOR END OF RECORD                       
         JL    MAIN020                                                          
SKIP01   XC    DISP,DISP           NEXT RECORD                                  
         OC    DA,DA                                                            
         JNZ   MAIN010             UNLESS IN BUFFER ZONE                        
         J     MAINX                                                            
*                                                                               
SKIPUP   SH    R2,BUFFREC          PREVIOUS RECORD                              
         LA    R1,SRVLST1                                                       
         CR    R3,R1               TEST FOR START OF SCREEN                     
         JL    MAINX                                                            
         LA    R1,IOBUFF           TEST FOR START OF RECORD                     
         CR    R2,R1                                                            
         JNL   MAIN020                                                          
         MVC   DISP,=F'6320'       LAST RECORD ON PREVIOUS BLOCK                
*                                                                               
SKIPUP01 OC    DA,DA               WERE WE IN BUFFER ZONE                       
         JNZ   *+14                                                             
         MVC   DA,ADRDNEXT         GOTO DNEXT NOW                               
         J     MAIN014                                                          
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,DA+2           BACK UP 1 BLOCK                              
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         JZ    SKIPUP02                                                         
         STCM  RE,1,DA+2                                                        
         J     MAIN014                                                          
*                                                                               
SKIPUP02 MVI   DA+2,8              BACK UP 1 TRACK                              
         XR    RE,RE                                                            
         ICM   RE,3,DA                                                          
         BCTR  RE,0                                                             
         LTR   RE,RE                                                            
         JZ    ERRTOP                                                           
         STCM  RE,3,DA                                                          
         J     MAIN014                                                          
*                                                                               
MAINX    CLI   FLAG,C'U'           UP FLAG?                                     
         JE    MAINXR                                                           
         MVC   BOTDA,DA            SAVE CURRENT AS BOTTOM                       
         MVC   BOTDISP,DISP                                                     
         J     MAINXX                                                           
*                                                                               
MAINXR   MVC   TOPDA,DA            SAVE CURRENT AS TOP                          
         MVC   TOPDISP,DISP                                                     
*                                                                               
MAINXX   MVC   SRVMSG(60),SRVMSG1  SORT OUT MESSAGE AND EXIT                    
         MVC   SRVPFK(79),SRVPFKS                                               
         MVC   SRVMSG+0(5),SYSNAME                                              
         OC    DA,DA                                                            
         JNZ   MAINXX1                                                          
         MVC   SRVMSG+9(8),=C'-BUFFER-'                                         
         J     XIT1                                                             
*                                                                               
MAINXX1  GOTO1 AHEXOUT,DMCB,DA,SRVMSG+9,4                                       
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM TO HANDLE TRACE ACTIONS                                *         
***********************************************************************         
TRCACTS  NTR1                                                                   
         CLI   TRCACT,C'S'                                                      
         JE    TRCACTST                                                         
         CLI   TRCACT,C'R'                                                      
         JE    TRCACTRS                                                         
         DC    H'0'                                                             
*                                                                               
TRCACTRS LAM   AR2,AR2,MYALET      GET A(TRACE BUFFER)                          
         SAC   512                                                              
         XR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         ICM   R2,15,DHATRCB       TRACE TABLE START                            
         JZ    TRCACTX                                                          
         AHI   R2,256                                                           
         XC    0(255,R2),0(R2)     ZAP TO ZERO FOR INIT                         
         SAC   0                                                                
*                                                                               
TRCACTST LA    R3,SRVHDR           PRINT HEADER LINE                            
         MVC   PLINE,ATITLE2                                                    
         BRAS  RE,LINEOUT                                                       
         LA    R3,SRVHDR1          PRINT HEADER LINE                            
         MVC   PLINE,ATITLE2                                                    
         BRAS  RE,LINEOUT                                                       
*                                                                               
         LAM   AR2,AR2,MYALET      GET A(TRACE BUFFER)                          
         SAC   512                                                              
         XR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         ICM   R2,15,DHATRCB       TRACE TABLE START                            
         JZ    TRCACTX                                                          
         LA    R3,SRVLST1          START AT TOP                                 
         AHI   R2,256                                                           
*                                                                               
         MVC   0(6,R3),=C'EMPTY '                                               
*                                                                               
         CLC   0(4,R2),=C'TRCB'                                                 
         JNE   *+10                                                             
         MVC   0(11,R3),=C'INITIALISED'                                         
*                                                                               
         CLC   0(4,R2),=C'TRCX'                                                 
         JNE   *+10                                                             
         MVC   0(11,R3),=C'OVERFLOWED '                                         
*                                                                               
         CLC   0(4,R2),=C'TRCF'                                                 
         JNE   *+10                                                             
         MVC   0(11,R3),=C'FAST TRACE '                                         
*                                                                               
TRCACT1  SAC   0                                                                
         L     R1,ACURS                                                         
         XR    R0,R0                                                            
         D     R0,=F'80'                                                        
         ST    R1,FULL                                                          
         CLC   FULL,=F'7'         CURSOR POS 7 TO 16                            
         JL    TRCACT1X                                                         
         CLC   FULL,=F'16'                                                      
         JH    TRCACT1X                                                         
         L     R1,FULL                                                          
         AHI   R1,-6                                                            
         CLI   CURI,40             2ND COL                                      
         JL    *+8                                                              
         LA    R1,16(R1)                                                        
         SAC   512                                                              
         XR    R2,R2                                                            
         ICM   R2,15,DHATRCB       TRACE TABLE START                            
         SLL   R1,3                                                             
         AR    R2,R1                                                            
         CLI   7(R2),C'N'          TOGGLE TRACE                                 
         JNE   *+12                                                             
         MVI   7(R2),C'Y'                                                       
         J     TRCACT1Z                                                         
         CLI   7(R2),C'Y'                                                       
         JNE   *+8                                                              
         MVI   7(R2),C'N'                                                       
*                                                                               
TRCACT1Z XR    R2,R2                                                            
         ICM   R2,15,DHATRCB       TRACE TABLE START                            
         XR    R1,R1                                                            
         ICM   R1,3,6(R2)          BUMP TABLE VERSION COUNT                     
         LA    R1,1(R1)                                                         
         STCM  R1,3,6(R2)                                                       
*                                                                               
TRCACT1X EQU   *                                                                
*                                                                               
         SAC   512                                                              
         LA    R3,SRVLST3                                                       
         XR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         ICM   R2,15,DHATRCB       TRACE TABLE START                            
         AHI   R2,8                                                             
*                                                                               
TRCACT01 OC    0(7,R2),0(R2)                                                    
         JZ    TRCACTX                                                          
         MVC   0(7,R3),0(R2)                                                    
         MVC   8(1,R3),7(R2)                                                    
         AHI   R2,8                                                             
         CLI   0(R2),X'FF'                                                      
         JE    TRCACTX                                                          
*                                                                               
TRCACTNX AHI   R3,87               BUMP TO NEXT LINE                            
         LA    R1,SRVLSTX                                                       
         CR    R3,R1               TEST FOR END OF SCREEN                       
         JNH   TRCACT01                                                         
         LA    R3,SRVLST3+40                                                    
         J     TRCACT01                                                         
*                                                                               
TRCACTX  SAC   0                                                                
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM TO READ TRACE TABLE                                    *         
***********************************************************************         
MAINTRC  NTR1                                                                   
         MVI   SAVEFLAG,0                                                       
*                                                                               
         LA    R3,SRVHDR           PRINT HEADER LINE                            
         MVC   PLINE,TTITLE1                                                    
         BRAS  RE,LINEOUT                                                       
         LA    R3,SRVHDR1          PRINT HEADER LINE                            
         MVC   PLINE,TTITLE2                                                    
         BRAS  RE,LINEOUT                                                       
*                                                                               
         LAM   AR2,AR2,MYALET      GET A(TRACE BUFFER)                          
         SAC   512                                                              
         SR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         ICM   R2,15,DHATRCB       TRACE TABLE START                            
         JZ    MAINTX                                                           
         AHI   R2,256                                                           
*                                                                               
         MVC   AIOBUFFX,12(R2)     SAVE END ADDRESS                             
         LA    R2,16(R2)                                                        
         ST    R2,AIOBUFF          SAVE START ADDRESS                           
*                                                                               
         OC    TOPDISP,TOPDISP     ANY TOP LINE YET                             
         JNZ   *+8                                                              
         ST    R2,TOPDISP          SET START AS TOPLINE                         
*                                                                               
         LA    R3,SRVLST1          START AT TOP                                 
         CLI   FLAG,C'D'           DOWN FLAG                                    
         JNE   MAINT005                                                         
*                                                                               
         MVC   TOPDISP,BOTDISP     TOP = BOTTOM                                 
         L     R2,TOPDISP                                                       
         CLC   12(3,R2),=C'*TR'                                                 
         JE    MAINT020                                                         
         L     R2,AIOBUFF          LOOP TO START ADDR                           
         J     MAINT020                                                         
*                                                                               
MAINT005 L     R2,TOPDISP          R2 = TOPDISP                                 
*                                                                               
         CLI   FLAG,C'U'           UP FLAG SET?                                 
         JNE   MAINT020            NO - REDISPLAY OLD SCREEN                    
*                                                                               
         L     R2,TOPDISP          R2 = TOPDISP                                 
         ST    R2,BOTDISP                                                       
         C     R2,AIOBUFF                                                       
         JNH   MAINTX                                                           
         LA    R3,SRVLSTX          START AT BOTTOM                              
         J     MAINT020                                                         
*                                                                               
MAINT020 BRAS  RE,TRCDISP          CREATE PLINE FOR OUTPUT                      
         SAC   0                                                                
         BRAS  RE,LINEOUT          OUT IT                                       
         SAC   512                                                              
*                                                                               
         CLI   FLAG,C'U'           IS THIS UP                                   
         JE    MAINT040                                                         
*                                                                               
MAINT030 AHI   R3,87               BUMP TO NEXT LINE                            
         LA    R1,SRVLSTX                                                       
         CR    R3,R1               TEST FOR END OF SCREEN                       
         JH    MAINTX                                                           
*                                                                               
         SR    R0,R0               NEXT ELEMENT                                 
         ICM   R0,3,2(R2)                                                       
         JZ    MAINTX                                                           
         AR    R2,R0                                                            
         ST    R2,BOTDISP                                                       
*                                                                               
MAINT034 CLC   12(3,R2),=C'*TR'    CHECK GOOD LINK                              
         JE    MAINT035                                                         
         LA    R2,1(R2)            KEEP ON TILL WE FIND ONE                     
         C     R2,AIOBUFFX                                                      
         JL    MAINT034                                                         
*                                                                               
MAINT035 C     R2,AIOBUFFX         TEST FOR END OF RECORD                       
         JL    MAINT020                                                         
         J     MAINTX                                                           
*                                                                               
MAINT040 AHI   R3,-87              BACK TO PREV LINE                            
         LA    R1,SRVLST1                                                       
         CR    R3,R1                                                            
         JL    MAINTX                                                           
*                                                                               
MAINT041 BCTR  R2,0                FIND PREVIOUS                                
         C     R2,AIOBUFF                                                       
         JL    MAINTX                                                           
         CLC   12(3,R2),=C'*TR'    USE STATUS ID                                
         JNE   MAINT041                                                         
         ST    R2,TOPDISP                                                       
         J     MAINT020                                                         
*                                                                               
MAINTX   MVC   SRVMSG(60),SRVMSG1  SORT OUT MESSAGE AND EXIT                    
         MVC   SRVPFK(79),SRVPFKS                                               
         SAC   0                                                                
         OC    DA,DA                                                            
         JNZ   MAINTXX                                                          
         MVC   SRVMSG+8(8),=C'-BUFFER-'                                         
         J     XIT1                                                             
*                                                                               
MAINTXX  GOTO1 AHEXOUT,DMCB,DA,SRVMSG+8,4                                       
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* HANDLE TWA TRACES =TTR,ON TTR,OFF TTR,LUID                          *         
***********************************************************************         
TWATRC   NTR1                                                                   
*                                                                               
         MVC   ACTION,SRVID+5      SAVE ACTION                                  
*                                                                               
         CLC   ACTION(2),=C'ON'    TRACE ON ME                                  
         JNE   *+12                                                             
         L     RF,AUTL                                                          
         OI    TSTAT4-UTLD(RF),TST4TWA                                          
*                                                                               
         CLC   ACTION(3),=C'OFF'   TRACE OFF ME                                 
         JNE   *+12                                                             
         L     RF,AUTL                                                          
         NI    TSTAT4-UTLD(RF),255-TST4TWA                                      
*                                                                               
         CLC   ACTION(5),=C'TRACE' READ TRACE RECORDS ME                        
         JNE   TWATRCX                                                          
*                                                                               
         LAM   AR2,AR2,MYALET      GET A(TRACE BUFFER)                          
         SAC   512                                                              
         SR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         ICM   R2,15,DHATRCB       TRACE TABLE START                            
         JZ    TWATRCX                                                          
         AHI   R2,256                                                           
*                                                                               
         MVC   AIOBUFFX,12(R2)     SAVE END ADDRESS                             
*                                                                               
         LA    R2,16(R2)                                                        
         ST    R2,AIOBUFF          SAVE START ADDRESS                           
*                                                                               
         ICM   R2,15,SCREENAD                                                   
         JNZ   TWAT010                                                          
         L     R2,AIOBUFF                                                       
*                                                                               
TWAT010  CLC   12(3,R2),=C'*TR'                                                 
         JNE   TWATRCX                                                          
*                                                                               
         CLC   10(2,R2),=X'0002'   IGNORE IF NOT SCREEN                         
         JE    TWAT020                                                          
*                                                                               
TWAT015  SR    R1,R1                                                            
         ICM   R1,3,2(R2)          NEXT ENTRY                                   
         AR    R2,R1                                                            
         C     R2,AIOBUFFX                                                      
         JL    TWAT010                                                          
         J     TWATRCX                                                          
*                                                                               
TWAT020  CLI   PFKEY,8                                                          
         JNE   *+12                                                             
         MVI   PFKEY,0                                                          
         J     TWAT015                                                          
*                                                                               
         CLI   PFKEY,7                                                          
         JNE   TWAT030                                                          
TWAT025  BCTR  R2,0                                                             
         C     R2,AIOBUFF                                                       
         JL    TWATRCX                                                          
         CLC   10(2,R2),=X'0002'                                                
         JNE   TWAT025                                                          
         CLC   12(3,R2),=C'*TR'                                                 
         JNE   TWAT025                                                          
*                                                                               
TWAT030  ST    R2,SCREENAD                                                      
         SR    R1,R1                                                            
         ICM   R1,3,2(R2)                                                       
         LA    R2,130(R2)          BUMP TO TWA                                  
         AHI   R1,-130                                                          
         LA    R0,SRVMSGH                                                       
         LR    R3,R1                                                            
         MVCL  R0,R2                                                            
         MVC   SRVID(5),=CL5'=TTR,'                                             
         MVC   SRVID+5(12),ACTION                                               
         MVI   SRVIDH+5,10                                                      
         OI    SRVIDH+6,X'81'                                                   
*                                                                               
         LA    RE,SRVMSGH                                                       
TWAT060  OI    6(RE),X'80'         SET ALL THE XMIT BITS ON                     
         SR    R0,R0                                                            
         ICM   R0,1,0(RE)                                                       
         JZ    TWATRCX                                                          
         AR    RE,R0                                                            
         J     TWAT060                                                          
*                                                                               
TWATRCX  SAC   0                                                                
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SHORTCUT ROUTINE R1=SHORTCUT SAVE CODE IN SHORT CL1                 *         
***********************************************************************         
SHORTGET NTR1                                                                   
*                                                                               
         MVI   SHORT,0                                                          
         LARL  RF,SHORTTAB         TABLE OF SHORTCUTS                           
         SR    RE,RE                                                            
SHORT010 ICM   RE,1,0(RF)                                                       
         JZ    SHORTX              EOT                                          
*                                                                               
         BCTR  RE,0                COMPARE TEXT                                 
         EXRL  RE,*+10                                                          
         J     *+10                                                             
         CLC   2(0,RF),0(R1)                                                    
         JE    SHORT020                                                         
         LA    RF,3(RE,RF)                                                      
         J     SHORT010                                                         
*                                                                               
SHORT020 MVC   SHORT,1(RF)         SAVE ACTION CODE                             
*                                                                               
SHORTX   J     XIT1                                                             
*                                                                               
SHORTTAB DC    AL1(02),C'M',C'ME'                                               
         DC    AL1(02),C'N',C'ON'                                               
         DC    AL1(03),C'F',C'OFF'                                              
         DC    AL1(05),C'T',C'TRACE'                                            
         DC    AL1(05),C'L',C'LOCKS'                                            
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
* READ IN SAVED STORAGE                                               *         
***********************************************************************         
READSTR  NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R2),SRSD                   
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),TRID    TEST FOR MY ID                               
         JNE   READX                                                            
         LA    R1,SRCOMWRK                                                      
         MVC   SAVEDSTR(SAVEDL),4(R1)                                           
READX    J     XIT1                                                             
                                                                                
***********************************************************************         
* WRITE OUT SAVED STORAGE                                             *         
***********************************************************************         
WRITESTR NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R1,SRCOMWRK                                                      
         MVC   0(4,R1),TRID                                                     
         MVC   4(SAVEDL,R1),SAVEDSTR                                            
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 ADATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         J     XIT1                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CALL GETHELP AND EXIT TO MONITOR                                    *         
***********************************************************************         
HELPOUT  L     R1,AHELP                                                         
         OI    6(R1),X'40'         SET CURSOR                                   
         LR    R0,R1                                                            
         MVC   HELPKEY,HELPID                                                   
*                                                                               
         LARL  RE,HELPTAB          FIND WHICH PANEL                             
         SR    RF,RF                                                            
         LA    RF,1                                                             
HELP010  EX    0,0(RE)             BY TESTING AHELP                             
         CR    R1,R0                                                            
         JE    HELP020                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),0                                                          
         JE    HELP020                                                          
         LA    RF,1(RF)                                                         
         J     HELP010                                                          
*                                                                               
HELP020  STC   RF,HELPNUM                                                       
         XC    DMCB(24),DMCB                                                    
         GOTO1 AGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0)                         
         DC    H'0'                                                             
HELPTAB  LA    R1,SRVP1H           POSSIBLE HELP FIELDS                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* OUTPUT TRACE ENTRY TO PLINE                                         *         
***********************************************************************         
TRCDISP  NTR1                                                                   
         USING TRACED,R2                                                        
         USING TRCLINED,PLINE                                                   
*                                                                               
         CLC   TRCSTAT(3),=C'*TR'                                               
         JNE   TRCDISPX                                                         
*NOP*    MVC   TRCLTYP,TRCTYPE                                                  
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),TRCREF      DISPLAY ADV OR JOB INFO                      
         MVI   FULL+2,0                                                         
         CLI   TRCTYPE,C'J'                                                     
         JNE   *+8                                                              
         OI    FULL+2,X'80'                                                     
         BRAS  RE,DISJOB                                                        
         MVC   TRCLREF,DUB1                                                     
*                                                                               
         MVC   FULL,TRCTIME        TIME                                         
         BRAS  RE,TIMEOUT                                                       
         MVC   TRCLTIME,WORK1                                                   
*                                                                               
         LA    R1,TRCTAB                                                        
TRCD010  CLC   0(2,R1),=X'FFFF'                                                 
         JE    TRCD020                                                          
         CLC   0(2,R1),TRCTRAC                                                  
         JE    *+12                                                             
         LA    R1,8(R1)                                                         
         J     TRCD010                                                          
*                                                                               
         MVC   TRCLCODE,2(R1)                                                   
         J     TRCD030                                                          
*                                                                               
TRCD020  GOTO1 AHEXOUT,DMCB,TRCTRAC,TRCLCODE,2                                  
*                                                                               
TRCD030  MVC   TRCLSTAT,TRCSTAT+3                                               
*                                                                               
***********************************************************************         
* INDIVIDUAL TRACE ENTRIES NOW                                        *         
***********************************************************************         
TRCD0001 CLC   TRCTRAC,=X'0001'    DNEXT                                        
         JNE   TRCD002                                                          
*                                                                               
TRCD002  CLC   TRCTRAC,=X'0002'    SCREEN                                       
         JNE   TRCD003                                                          
         MVC   TRCLDATA+0(8),TRCDATA+2                                          
         MVC   GTSYSSE,TRCDATA+11                                               
         BRAS  RE,GETSYS           GET A(PGMS)                                  
         MVC   TRCLDATA+10(8),GTSYSN                                            
         MVC   GTPROG,TRCDATA+12   PROGRAM                                      
         BRAS  RE,GETPROG                                                       
         MVC   TRCLDATA+20(8),GTPROGN                                           
*                                                                               
*                                                                               
TRCD003  CLC   TRCTRAC,=X'0003'    ENQ/DEQ                                      
         JNE   TRCD004                                                          
*                                                                               
         MVC   TRCLDATA+0(1),TRCDATA+2                                          
         MVC   TRCLDATA+2(5),TRCDATA+3                                          
         GOTO1 AHEXOUT,DMCB,TRCDATA+8,WORK,5                                    
         MVC   TRCLDATA+8(2),WORK                                               
         MVC   TRCLDATA+11(8),WORK+1                                            
*                                                                               
TRCD004  CLC   TRCTRAC,=X'0004'    WRKF GETCI                                   
         JNE   TRCD005                                                          
         MVC   TRCLDATA+2(5),=C'WRKF'                                           
         MVC   TRCLDATA+6(1),TRCDATA+2                                          
         EDIT  (B2,TRCDATA+3),(5,TRCLDATA+8)                                    
*                                                                               
TRCD005  CLC   TRCTRAC,=X'0005'    FULL IO TRACE                                
         JNE   TRCD006                                                          
         MVC   TRCLDATA(8),TRDIOCAL                                             
         GOTO1 AHEXOUT,DMCB,TRDIOOFF+2,TRCLDATA+9,2                             
*                                                                               
         MVC   TRCLDATA+14(7),TRDIOCOM                                          
         MVC   TRCLDATA+22(7),TRDIOFIL                                          
         GOTO1 AHEXOUT,DMCB,TRDIODA,TRCLDATA+30,4                               
*                                                                               
         MVC   DUB,TRDIOCHA                                                     
*                                                                               
         LARL  R1,CHANTAB                                                       
TRCD005A CLC   DUB(5),0(R1)                                                     
         JE    TRCD005B                                                         
         LA    R1,16(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         JNE   TRCD005A                                                         
*                                                                               
TRCD005B MVC   TRCLDATA+39(8),8(R1)                                             
*                                                                               
         MVC   TRCLDATA+48(6),TRDIOP1                                           
         MVC   TRCLDATA+56(7),TRDIOP2                                           
*                                                                               
*NOP     GOTO1 AHEXOUT,DMCB,TRDIOP3,TRCLDATA+65,32                              
*        GOTO1 AHEXOUT,DMCB,TRDIOP4,TRCLDATA+130,32                             
*                                                                               
*        GOTO1 AHEXOUT,DMCB,TRDIO1,TRCLDATA+195,40                              
*NOP     GOTO1 AHEXOUT,DMCB,TRDIO2,TRCLDATA+276,40                              
*                                                                               
         GOTO1 AHEXOUT,DMCB,TRDIO1,TRCLDATA+65,42                               
         GOTO1 AHEXOUT,DMCB,TRDIO2,TRCLDATA+146,42                              
*                                                                               
TRCD006  CLC   TRCTRAC,=X'0006'    WRKF GETCI                                   
         JNE   TRCD007                                                          
         MVC   TRCLDATA+2(3),=C'EOF'                                            
         MVC   TRCLDATA+6(1),TRDEOCAL                                           
         MVC   TRCLDATA+8(8),TRDEODD                                            
         GOTO1 AHEXOUT,DMCB,TRDEOSYS,TRCLDATA+20,4                              
         GOTO1 AHEXOUT,DMCB,TRDEODN,TRCLDATA+30,4                               
         GOTO1 AHEXOUT,DMCB,TRDEODC,TRCLDATA+40,4                               
*                                                                               
TRCD007  CLC   TRCTRAC,=X'000A'    WSSVR TRACE                                  
         JNE   TRCD008                                                          
         MVC   TRCLDATA(4),TRCDATA+2                                            
         CLI   TRCLDATA,C'A'                                                    
         JNL   TRCD007A                                                         
         GOTO1 AHEXOUT,DMCB,TRCDATA+2,TRCLDATA,4                                
TRCD007A MVC   TRCLDATA+9(1),TRCDATA+6                                          
         GOTO1 AHEXOUT,DMCB,TRCDATA+26,TRCLDATA+11,64                           
*                                                                               
TRCD008  EQU   *                                                                
*                                                                               
TRCDISPX J     XIT1                                                             
*                                                                               
CHANTAB  DC    X'0000000000000000',C'NONE    '                                  
         DC    X'2331080600000000',C'SSEQ+R/D'                                  
         DC    X'2331080500000000',C'SSEQ+W/D'                                  
         DC    X'3108060000000000',C'SEQ+R/D '                                  
         DC    X'3108050000000000',C'SEQ+W/D '                                  
         DC    X'31081D1D00000000',C'SEQ+WCKD'                                  
         DC    X'5108060000000000',C'SHI+R/D '                                  
         DC    X'FFFFFFFFFFFFFFFF',C'????????'                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE TRACE ACTIONS                                              *         
***********************************************************************         
VALTRC   NTR1                                                                   
         MVI   TRCACT,0                                                         
         LA    R1,SRVP1                                                         
         LARL  RF,TRCTABL                                                       
         SR    RE,RE                                                            
         ICM   RE,1,SRVP1H+5                                                    
         JZ    EXITNEQ                                                          
         AHI   RE,-1                                                            
*                                                                               
VALTRC1  EXRL  RE,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R1),0(RF)                                                    
         JE    VALTRC5                                                          
         LA    RF,8(RF)                                                         
         CLC   0(7,RF),=C'XXXXXXX'                                              
         JE    EXITNEQ                                                          
         J     VALTRC1                                                          
*                                                                               
VALTRC5  MVC   TRCACT,7(RF)                                                     
         J     EXITEQU                                                          
*                                                                               
TRCTABL  DC    C'STATUS ',C'S'                                                  
         DC    C'RESET  ',C'R'                                                  
         DC    C'XXXXXXX'                                                       
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILTERS                                                    *         
***********************************************************************         
VALFILT  NTR1                                                                   
         MVI   FETIMEX,X'FF'                                                    
         MVI   FSTIMEX,X'FF'                                                    
         MVI   FSINCX,X'FF'                                                     
         MVI   FLUIDCX,X'FF'                                                    
*                                                                               
         ST    RD,CARDRD                                                        
         LR    R2,R1                                                            
         LA    R1,78(R1)                                                        
         ST    R1,CARDEND          SAVE LAST CHR ADDR                           
         CLI   0(R2),C' '            IN COL 1 IS A COMMENT                      
         JNH   EXITEQU                                                          
         CLI   0(R2),C'*'          * IN COL 1 IS A COMMENT                      
         JE    EXITEQU                                                          
*                                                                               
VALF001  LARL  R4,CARDTAB                                                       
         ST    R2,CARDR2                                                        
*                                                                               
         LR    R1,R2                                                            
VALF002  CLI   0(R1),C'A'          TEST FOR ANOTHER                             
         JL    VALF003                                                          
         LA    R1,1(R1)                                                         
         C     R1,CARDEND          TEST FOR END OF CARD                         
         JL    VALF002                                                          
VALF003  SR    R1,R2               GET LEN FOR COMPARE                          
         BCTR  R1,0                                                             
*                                                                               
VALF010  EXRL  R1,*+10             EXECUTE KEYWORD TEST                         
         J     *+10                                                             
         CLC   0(0,R2),0(R4)                                                    
         JE    VALF020                                                          
         LA    R4,14(R4)           TRY NEXT ENTRY                               
         CLI   0(R4),0                                                          
         JNE   VALF010                                                          
         J     CERRKEY             ERROR INVALID KEYWORD                        
*                                                                               
VALF020  LA    R2,1(R2,R1)         POINT TO DELIMITER                           
*                                                                               
         LARL  RF,VALFDELS         DELIMITER TABLE                              
         J     *+8                                                              
VALF021  LA    RF,5(RF)                                                         
         CLI   0(RF),0                                                          
         JE    CERRDEL             END OF TABLE INVALID DELIMITER               
*                                                                               
         MVC   BYTE,4(RF)          AUTH BIT MUST BE ON                          
         CLI   BYTE,0              EXCEPT WHEN ZERO                             
         JE    *+14                                                             
         NC    BYTE,9(R4)                                                       
         JZ    VALF021                                                          
*                                                                               
         LLC   R1,2(RF)            GET EX LEN                                   
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R2),0(RF)       TEST DELIMITERS                              
         JNE   VALF021                                                          
*                                                                               
         MVC   BYTE,3(RF)          SAVE COMPARE CHR                             
         LA    R2,1(R1,R2)                                                      
         J     VALF025                                                          
*                                                                               
VALFDELS DC    C'= ',AL1(0),X'80',X'00'                                         
         DC    C'>=',AL1(1),X'B0',X'20'                                         
         DC    C'<=',AL1(1),X'D0',X'20'                                         
         DC    C'/=',AL1(1),X'70',X'40'                                         
         DC    C'< ',AL1(0),X'40',X'20'                                         
         DC    C'> ',AL1(0),X'20',X'20'                                         
         DC    X'00'                                                            
*                                                                               
VALF025  LR    R1,R2               GET LEN FOR MOVE                             
VALF026  CLI   0(R1),C','                                                       
         JE    VALF030                                                          
         CLI   0(R1),C' '                                                       
         JE    VALF030                                                          
         CLI   0(R1),0                                                          
         JE    VALF030                                                          
         LA    R1,1(R1)                                                         
         J     VALF026                                                          
*                                                                               
VALF030  SR    R1,R2                                                            
*                                                                               
VALF031  BCTR  R1,0                                                             
         SR    RF,RF                                                            
         ICM   RF,7,11(R4)         GET ADDRESS FOR MOVE                         
*                                                                               
         TM    9(R4),X'80'         IF ROUTINE                                   
         JZ    VALF035                                                          
         A     RF,RELO                                                          
         BASR  RE,RF               GOTO ROUTINE                                 
         JE    VALF090                                                          
         J     EXITNEQ                                                          
*                                                                               
VALF035  AR    RF,RC                                                            
         TM    9(R4),X'04'         IF LIST                                      
         JNO   VALF050                                                          
VALF040  CLI   0(RF),X'FF'         CHECK NOT FULL                               
         JE    CERRMAN                                                          
         CLI   0(RF),0             EMPTY ENTRY                                  
         JE    VALF050                                                          
         LLC   R0,8(R4)                                                         
         AR    RF,R0                                                            
         TM    9(R4),X'60'         /<=>                                         
         JZ    VALF040                                                          
         LA    RF,1(RF)            ONE MORE FOR CODE                            
         J     VALF040                                                          
*                                                                               
VALF050  TM    9(R4),X'60'         IF /<=>                                      
         JZ    *+14                                                             
         MVC   0(1,RF),BYTE        SAVE COMP CODE                               
         LA    RF,1(RF)                                                         
*                                                                               
         TM    9(R4),X'10'         HEX INPUT                                    
         JNO   VALF060                                                          
         LA    R0,1(R1)            SET R0 HEX INPUT LEN                         
         GOTO1 AHEXIN,DMCB,(R2),(RF),(R0)                                       
         ICM   R1,15,12(R1)                                                     
         JZ    CERRHEX                                                          
         J     VALF090                                                          
*                                                                               
VALF060  TM    9(R4),X'08'         DEC INPUT                                    
         JZ    VALF070                                                          
         LR    R4,R2                                                            
         LA    R3,1(R1)                                                         
         JAS   RE,VALNUM           VALIDATE NUMBER                              
         CLI   DUB,X'FF'                                                        
         JE    CERRDEC                                                          
         CVB   R1,DUB                                                           
         STCM  R1,15,0(RF)         SAVE FULLWORD (DEFAULT)                      
         J     VALF090                                                          
*                                                                               
VALF070  TM    9(R4),X'02'         TIME INPUT                                   
         JZ    VALF080                                                          
         JAS   RE,VALTIME                                                       
         MVC   0(4,RF),FULL                                                     
         J     VALF090                                                          
*                                                                               
VALF080  CLI   8(R4),0             DONT CARE                                    
         JE    *+12                                                             
         CLM   R1,1,8(R4)          CHECK MAX LEN                                
         JNL   CERRMAX                                                          
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   0(0,RF),0(R2)       MOVE TO OUTPUT AREA                          
*                                                                               
VALF090  CLI   0(R2),C','          TEST FOR ANOTHER                             
         LA    R2,1(R2)                                                         
         JE    VALF001             GO FIND TABLE ENTRY                          
         C     R2,CARDEND          TEST FOR END OF CARD                         
         JL    VALF090                                                          
         J     EXITEQU                                                          
*                                                                               
         EJECT                                                                  
CERRSYS  LA    R1,=C'INVALID SYSTEM  '                                          
         J     CERRX                                                            
CERRPRG  LA    R1,=C'INVALID PROGRAM '                                          
         J     CERRX                                                            
CERRSES  LA    R1,=C'INVALID SESYS   '                                          
         J     CERRX                                                            
CERRTIM  LA    R1,=C'INVALID TIME    '                                          
         J     CERRX                                                            
CERRDEC  LA    R1,=C'MUST BE HEX     '                                          
         J     CERRX                                                            
CERRHEX  LA    R1,=C'MUST BE DECIMAL '                                          
         J     CERRX                                                            
CERRKEY  LA    R1,=C'INVALID KEYWORD '                                          
         J     CERRX                                                            
CERRDEL  LA    R1,=C'INVALID DELIMITR'                                          
         J     CERRX                                                            
CERRMAX  LA    R1,=C'VALUE TOO LONG  '                                          
         J     CERRX                                                            
CERRMAN  LA    R1,=C'TOO MANY FILTERS'                                          
         J     CERRX                                                            
CERRUSER LA    R1,=C'INVALID USERID  '                                          
         J     CERRX                                                            
CERRPERS LA    R1,=C'INVALID PERSON  '                                          
         J     CERRX                                                            
*                                                                               
CERRX    L     RD,CARDRD                                                        
         L     R2,CARDR2                                                        
*                                                                               
CERRX1   LA    RF,SRVMSG                                                        
         MVC   0(16,RF),=C'ED/9999 XXXXX - '                                    
         MVC   8(5,RF),SYSNAME                                                  
         MVC   16(16,RF),0(R1)                                                  
         J     EXITNEQ                                                          
         EJECT                                                                  
***********************************************************************         
* GET FSESYS FROM 0(R2) (R1)=EX LEN                                   *         
***********************************************************************         
VALSYS   NTR1                                                                   
         LR    RF,R1                                                            
         L     R5,ASELIST          MUST HAVE SYSFACS                            
         JAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R5                                                       
VALSYS0  EXRL  RF,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R2),SENAME      TEST NAME                                    
         JE    VALSYS1                                                          
         JXLE  R5,R6,VALSYS0       NEXT                                         
         J     CERRSES             ERROR SE SYS NOT FOUND                       
*                                                                               
VALSYS1  MVC   FSESYS,SESYS        SET NUMBER                                   
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         J     EXITEQU                                                          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE PROGRAM NAME                                               *         
***********************************************************************         
VALPROG  NTR1                                                                   
         LR    RF,R1                                                            
         ICM   R5,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         JZ    CERRSYS                                                          
         JAS   RE,SETBXLE          SET BXLE                                     
         USING PGMLSTD,R5                                                       
VALPRG0  EXRL  RF,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R2),PGMNAME     TEST NAME                                    
         JE    VALPRG1                                                          
         JXLE  R5,R6,VALPRG0       NEXT                                         
*                                                                               
         GOTO1 AHEXIN,DMCB,(R2),FPROG,2                                         
         OC    12(4,R1),12(R1)     TRY FOR HEX LAST                             
         JNZ   EXITEQU                                                          
         J     CERRPRG                                                          
*                                                                               
VALPRG1  MVC   FPROG,PGMNUM        FOUND                                        
         J     EXITEQU                                                          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET TIME FROM 0(R2) (R1)=EX LEN  TIME=HH:MM:SS.TU                   *         
***********************************************************************         
VALTIME  NTR1                                                                   
         MVC   HALF,=C'00'         FIRST MAY BE 1:00 OR 02:00                   
         CLI   1(R2),C':'                                                       
         JNE   VALT010                                                          
         MVC   HALF+1(1),0(R2)     ASSUME 1:00                                  
         LA    R2,2(R2)                                                         
         J     VALT020                                                          
*                                                                               
VALT010  MVC   HALF+0(2),0(R2)     ASSUME 02:00                                 
         LA    R2,3(R2)                                                         
*                                                                               
VALT020  LA    R3,2                PREPARE FULL AND HALF                        
         LA    R4,HALF                                                          
         XC    FULL,FULL                                                        
*                                                                               
         JAS   RE,VALNUM           VALIDATE HOURS                               
         L     RF,TUHOUR                                                        
         JAS   RE,VALTADD                                                       
*                                                                               
         MVC   HALF,0(R2)          VALIDATE MINUTES                             
         JAS   RE,VALNUM                                                        
         L     RF,TUMINUTE                                                      
         JAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C':'          TEST FOR SECS                                
         JNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         JAS   RE,VALNUM           VALIDATE SECS                                
         L     RF,TUSECOND                                                      
         JAS   RE,VALTADD                                                       
*                                                                               
         CLI   2(R2),C'.'          TEST FOR TUS                                 
         JNE   EXITEQU                                                          
         LA    R2,3(R2)                                                         
         MVC   HALF,0(R2)                                                       
         JAS   RE,VALNUM           VALIDATE TUS                                 
         L     RF,TUMSEC                                                        
         JAS   RE,VALTADD                                                       
         J     EXITEQU                                                          
*                                                                               
VALTADD  CLI   DUB,X'FF'           TEST FOR INVALID NUMERIC                     
         JE    CERRTIM                                                          
         SR    R0,R0               CONVERT AND MULTIPLY BY RF                   
         CVB   R1,DUB                                                           
         MR    R0,RF                                                            
         A     R1,FULL                                                          
         ST    R1,FULL             ADD TO FULL                                  
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET FOVSYS FROM 0(R2) (R1)=EX LEN                                   *         
***********************************************************************         
VALOVS   NTR1                                                                   
         L     RE,ASYSLST          RE=SYSLST                                    
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
*                                                                               
VALS010  CLI   SYSLNUM,0           TEST FOR EOT                                 
         JE    VALS050                                                          
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   SYSLSHRT(0),0(R2)   TEST SHORT NAME                              
         JE    VALS990                                                          
VALS012  LA    RE,SYSLLEN(RE)      NEXT                                         
         J     VALS010                                                          
*                                                                               
VALS050  L     RE,ASYSLST          RE=SYSLST                                    
         LA    RE,6(RE)                                                         
VALS051  CLI   SYSLNUM,0           TEST FOR EOT                                 
         JE    VALS090                                                          
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   SYSLNAME(0),0(R2)   TEST LONG NAME                               
         JE    VALS990                                                          
         LA    RE,SYSLLEN(RE)      NEXT                                         
         J     VALS051                                                          
*                                                                               
VALS090  J     CERRSYS             SET CC NEQ NOT FOUND                         
VALS990  MVC   FOVSYS,SYSLNUM                                                   
         MVC   GTSYSSE,FOVSYS                                                   
         BRAS  RE,GETSYS           GET A(PGMS)                                  
         J     EXITEQU             SET CC EQU                                   
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE SPECIAL ROUTINE                                            *         
***********************************************************************         
VALSPEC  NTR1                                                                   
         MVC   FSPEC,0(R2)                                                      
         J     EXITEQU                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE USERID                                                     *         
***********************************************************************         
VALUSER  NTR1                                                                   
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID(10),SPACES                                                
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   CTIKID(0),0(R2)                                                  
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         JNE   CERRUSER                                                         
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
VALUSR10 AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST ID NUM ELEMENT                          
         JNE   *+14                                                             
         MVC   FUSERID,2(R7)       SET ID FILTER                                
         J     EXITEQU                                                          
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         JNE   VALUSR10            BUT DROP THROUGH IF LAST                     
         J     CERRUSER                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE PERSON ID USERID=USER,PERSON=PERS                          *         
* OR SA=AG,PERSON=PERS AG=AG,PERSON=PERS                              *         
***********************************************************************         
VALPERS  NTR1                                                                   
         L     R7,ACTREC           BUILD PERSON RECORD KEY                      
         USING SAPEREC,R7                                                       
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,FSA                                                      
         MVC   SAPEPID(8),SPACES                                                
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   SAPEPID(0),0(R2)                                                 
         GOTO1 ADATAMGR,DMCB,DMRDHI,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         JNE   CERRPERS                                                         
         LA    R7,SAPEDATA                                                      
         SR    RE,RE                                                            
VALPER10 AR    R7,RE                                                            
         CLI   0(R7),X'C4'         PASSWORD NUMBER ELEMENT                      
         JNE   *+14                                                             
         MVC   FPERSON,2(R7)       SET ID FILTER                                
         J     EXITEQU                                                          
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         JNE   VALPER10            BUT DROP THROUGH IF LAST                     
         J     CERRPERS                                                         
         EJECT                                                                  
***********************************************************************         
* CARD TABLE                                                          *         
* CL7'KEYWORD',AL1(KEYWRD LEN-1,OP LEN),X'FLAGS',AL3(OUTPUT)          *         
*                                                                     *         
* FLAGS  X'8000' A(OUTPUT) IS A(ROUTINE)                              *         
*        X'4000' ACCEPT =,/=                                          *         
*        X'2000' ACCEPT <,>,<=,=>                                     *         
*        X'1000' HEX VALUE                                            *         
*        X'0800' DEC VALUE                                            *         
*        X'0400' OUTPUT IS A LIST                                     *         
*        X'0200' TIME VALUE                                           *         
***********************************************************************         
CARDTAB  DS    0F                                                               
         DC    C'LUID   ',AL1(3,8),X'4400',AL3(FLUIDC-WRKD)                     
         DC    C'TERM   ',AL1(3,8),X'4400',AL3(FLUIDC-WRKD)                     
         DC    C'TRM    ',AL1(2,8),X'4400',AL3(FLUIDC-WRKD)                     
         DC    C'FLAGS  ',AL1(3,8),X'0000',AL3(FFLAGS-WRKD)                     
         DC    C'SCREEN ',AL1(5,8),X'4400',AL3(SLUIDC-WRKD)                     
         DC    C'SYSTEM ',AL1(5,0),X'C000',AL3(VALOVS)                          
         DC    C'SESYS  ',AL1(4,0),X'C000',AL3(VALSYS)                          
         DC    C'SE     ',AL1(1,0),X'C000',AL3(VALSYS)                          
         DC    C'PROGRAM',AL1(6,0),X'C000',AL3(VALPROG)                         
         DC    C'PRG    ',AL1(2,0),X'C000',AL3(VALPROG)                         
         DC    C'TASK   ',AL1(3,1),X'6000',AL3(FTASKC-WRKD)                     
         DC    C'SIN    ',AL1(2,4),X'7400',AL3(FSINC-WRKD)                      
         DC    C'STIME  ',AL1(4,4),X'6600',AL3(FSTIME-WRKD)                     
         DC    C'ETIME  ',AL1(4,4),X'6600',AL3(FETIME-WRKD)                     
         DC    C'CPUTIME',AL1(6,0),X'6800',AL3(FCPUC-WRKD)                      
         DC    C'QTIME  ',AL1(4,0),X'6800',AL3(FQTMC-WRKD)                      
         DC    C'XTIME  ',AL1(4,0),X'6800',AL3(EXTMC-WRKD)                      
         DC    C'IOCOUNT',AL1(6,0),X'6800',AL3(FIOCC-WRKD)                      
         DC    C'OVCOUNT',AL1(6,0),X'6800',AL3(FOVCC-WRKD)                      
         DC    C'RATE   ',AL1(3,0),X'6800',AL3(FOVCC-WRKD)                      
         DC    C'SPECIAL',AL1(6,0),X'C000',AL3(VALSPEC)                         
         DC    C'USERID ',AL1(5,0),X'C000',AL3(VALUSER)                         
         DC    C'PERSON ',AL1(5,0),X'C000',AL3(VALPERS)                         
         DC    C'AG     ',AL1(1,2),X'6000',AL3(FAGC-WRKD)                       
         DC    C'SA     ',AL1(1,2),X'6000',AL3(FSAC-WRKD)                       
         DC    C'PCPROG ',AL1(5,0),X'6800',AL3(FPCPC-WRKD)                      
         DC    C'LOCKS  ',AL1(4,0),X'6800',AL3(FLKSC-WRKD)                      
         DC    C'LOCKW  ',AL1(4,0),X'6800',AL3(FLKWC-WRKD)                      
         DC    C'ADDS   ',AL1(3,0),X'6800',AL3(FADDC-WRKD)                      
         DC    C'CHGS   ',AL1(3,0),X'6800',AL3(FCHGC-WRKD)                      
         DC    X'0000'                                                          
         EJECT                                                                  
***********************************************************************         
* GET GTSYS & GTSYSN FROM GTSYSSE                                     *         
***********************************************************************         
GETSYS   NTR1                                                                   
         L     R5,ASELIST          MUST HAVE SYSFACS                            
         JAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
GETSYS0  CLI   GTSYSSE,0           IF SYSSE IS ZERO                             
         JNE   GETSYS1                                                          
         CLC   GTSYS,SEOVSYS       TEST GTSYS WITH OVSYS                        
         JE    GETSYS2                                                          
GETSYS1  CLC   GTSYSSE,SESYS       TEST SE NUMBER                               
         JE    GETSYS2                                                          
         JXLE  R5,R6,GETSYS0       NEXT                                         
         MVI   GTSYS,0                                                          
         XC    GTSYSN,GTSYSN                                                    
         J     EXITNEQ             ERROR EXIT NOT FOUND                         
*                                                                               
GETSYS2  MVC   GTSYS,SEOVSYS       FOUND                                        
         MVC   GTSYSN,SENAME       SET NAME                                     
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         J     EXITEQU                                                          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET GTPROGN FROM GTPROG                                             *         
***********************************************************************         
GETPROG  NTR1                                                                   
         ICM   R5,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         JZ    GETPRG2                                                          
         JAS   RE,SETBXLE          SET BXLE                                     
         USING PGMLSTD,R5                                                       
GETPRG0  CLC   GTPROG,PGMNUM       TEST PROG NUMBER                             
         JNE   GETPRG0A                                                         
         CLI   PGMCTRY,0                                                        
         JE    GETPRG1                                                          
         CLC   PGMCTRY,MYTCTRY     TEST PROG COUNTRY                            
         JE    GETPRG1                                                          
GETPRG0A JXLE  R5,R6,GETPRG0       NEXT                                         
GETPRG2  XC    GTPROGN,GTPROGN     NOT FOUND SO HEXOUT NUMBER                   
         OC    GTPROG,GTPROG                                                    
         JZ    EXITEQU                                                          
         GOTO1 AHEXOUT,DMCB,GTPROG,GTPROGN,1                                    
         J     EXITEQU                                                          
GETPRG1  MVC   GTPROGN(7),PGMNAME  SET NAME                                     
         J     EXITEQU                                                          
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTSYS                                          *         
***********************************************************************         
GETOVS   NTR1                                                                   
         L     RE,ASYSLST          RE=SYSLST                                    
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
*                                                                               
GETS010  CLI   SYSLNUM,0           TEST FOR EOT                                 
         JE    GETS090                                                          
         CLC   GTSYS,SYSLNUM       TEST SYSTEM NUMBER                           
         JE    GETS990                                                          
         LA    RE,SYSLLEN(RE)      NEXT                                         
         J     GETS010                                                          
*                                                                               
GETS090  J     EXITNEQ             SET CC NEQ NOT FOUND                         
*                                                                               
GETS990  MVC   GTSYSN,SYSLNAME                                                  
         MVC   GTSYS1,SYSLRPLT     1 CHR SYS                                    
         LR    R1,RE               R1=A(SYSLST ENTRY)                           
         J     EXITEQU             SET CC EQU                                   
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET USERID FROM 2 CHR ID NUMBER                                     *         
***********************************************************************         
GETUSER  NTR1                                                                   
         CLC   GIUSER,GIUPREV                                                   
         JE    GETUSRX                                                          
         MVC   GIUPREV,GIUSER                                                   
*                                                                               
         MVC   GIUSERID,SPACES     IGNORE ZERO ID                               
         OC    GIUSER,GIUSER                                                    
         JZ    GETUSR20                                                         
*                                                                               
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),GIUSER                                               
         NI    CTIKID+8,X'7F'      UNSET GENERIC FLAG                           
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         JNE   GETUSR12            NNNNNN IF REC NOT FOUND                      
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
GETUSR10 AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST ID ELEMENT                              
         JNE   *+14                                                             
         MVC   GIUSERID,2(R7)      GET ID NAME                                  
         J     GETUSR20                                                         
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         JNE   GETUSR10            BUT DROP THROUGH IF LAST                     
*                                                                               
GETUSR12 EDIT  (B2,GIUSER),(8,GIUSERID),DUB=SDUB,WRK=SWORK1                     
*                                                                               
GETUSR20 LA    RF,0                                                             
         LA    RE,GIUSERID                                                      
GETUSR21 CLI   0(RE),X'40'                                                      
         JE    GETUSR30                                                         
         CLI   0(RE),0                                                          
         JE    GETUSR30                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CHI   RF,8                                                             
         JL    GETUSR21                                                         
GETUSR30 STC   RF,GIULEN                                                        
GETUSRX  J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* GET PERSON FROM 2 CHR ID PERSON ID                                  *         
***********************************************************************         
GETPERS  NTR1                                                                   
         CLC   GIPERS,GIPPREV                                                   
         JE    GETPERX                                                          
         MVC   GIPPREV,GIPERS                                                   
         L     R7,ACTREC           BUILD ID RECORD KEY                          
         USING CT0REC,R7                                                        
         XC    CT0KEY,CT0KEY                                                    
         MVI   CT0KTYP,CT0KTEQU                                                 
         MVC   CT0KAGY,GIPERS      2CHR AGY                                     
         MVC   CT0KNUM,GIPERS+2    PERSON NUM                                   
         GOTO1 ADATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7)                            
         CLI   8(R1),0                                                          
         JNE   GETPER12            NNNNNN IF REC NOT FOUND                      
         LA    R7,CT0DATA                                                       
         SR    RE,RE                                                            
GETPER10 AR    R7,RE                                                            
         CLI   0(R7),SAPALELQ      TEST PERSON ELEMENT                          
         JNE   *+14                                                             
         MVC   GIPERSON,2(R7)      GET ID NAME                                  
         J     GETPER20                                                         
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         JNE   GETPER10            BUT DROP THROUGH IF LAST                     
*                                                                               
GETPER12 EDIT  (B2,GIPERS+2),(8,GIPERSON),DUB=SDUB,WRK=SWORK1                   
*                                                                               
GETPER20 LA    RF,0                                                             
         LA    RE,GIPERSON                                                      
GETPER21 CLI   0(RE),X'40'                                                      
         JE    GETPER30                                                         
         CLI   0(RE),0                                                          
         JE    GETPER30                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CHI   RF,8                                                             
         JL    GETPER21                                                         
GETPER30 STC   RF,GIPLEN                                                        
GETPERX  J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* PC PROGRAM CONNECT DATA                                             *         
***********************************************************************         
GETPCVER NTR1                                                                   
         CLC   GIPCDATA,GIPCPREV                                                
         JE    PCVERX                                                           
         MVC   GIPCPREV,GIPCDATA                                                
         MVC   GIPCOUT,SPACES                                                   
         CLC   GIPCDATA,ZEROS                                                   
         JE    PCVERX                                                           
*                                                                               
         MVC   GIPCOUT(5),=C'PMGC '                                             
         LLC   R1,GIPCDATA         UNSET FLAGS IF OFF                           
         LA    RF,GIPCOUT                                                       
         SLL   R1,24                                                            
         LA    R0,4                                                             
PCV010   LTR   R1,R1                                                            
         JM    *+8                                                              
         MVI   0(RF),C'.'                                                       
         LA    RF,1(RF)                                                         
         SLL   R1,1                                                             
         JCT   R0,PCV010                                                        
*                                                                               
         LA    R2,GIPCOUT+5        OUTPUT PC PROGRAM INFO IF DEFINED            
         SR    R0,R0                                                            
         ICM   R0,3,GIPCDATA+1                                                  
         JZ    PCVER94                                                          
*                                                                               
         TM    GIPCDATA,X'80'      PC PROG                                      
         JZ    PCVER94                                                          
*                                                                               
         CVD   R0,DUB              OUTPUT PC APP PROGRAM NUMBER                 
         OI    DUB+7,X'0F'                                                      
         UNPK  0(5,R2),DUB                                                      
         L     RF,=V(FAXPTPC)      POINT TO PC PROGRAM TABLE                    
         A     RF,RELO                                                          
         USING FAXPTABD,RF                                                      
         LA    RF,L'FAXPNTRY(RF)   BUMP PAST FIRST ENTRY                        
*                                                                               
PCVER91  CLI   FAXPTYPE,0          SEARCH FOR NUMBER TO GET NAME                
         JE    PCVER94                                                          
         TM    FAXPTYPE,FAXPTPC                                                 
         JZ    PCVER92                                                          
         CLM   R0,3,FAXPNUM                                                     
         JE    PCVER93                                                          
PCVER92  LA    RF,L'FAXPNTRY(RF)                                                
         J     PCVER91                                                          
PCVER93  MVC   0(L'FAXPNAME,R2),FAXPNAME                                        
         DROP  RF                                                               
*                                                                               
PCVER94  OC    GIPCDATA+3(3),GIPCDATA+3 PROGRAM VERSION NUMBER                  
         JZ    XIT1                                                             
         MVI   GIPCOUT+16,C'V'                                                  
         LA    R2,GIPCOUT+17                                                    
         CLC   GIPCDATA+3(3),=X'FFFFFF' INVALID VALUE                           
         JNE   *+14                                                             
         MVC   0(7,R2),=C'?.?.?.?'                                              
         J     XIT1                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,14,GIPCDATA+3    A.B.CC.DD IN LEFT 3 BYTES                    
*                                                                               
         SR    RE,RE               A                                            
         SLDL  RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         MVI   2(R2),C'.'                                                       
         LA    R2,3(R2)                                                         
*                                                                               
         SR    RE,RE               B                                            
         SLDL  RE,4                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(2,R2),DUB                                                      
         MVI   2(R2),C'.'                                                       
         LA    R2,3(R2)                                                         
*                                                                               
         SR    RE,RE               CC                                           
         SLDL  RE,8                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R2),DUB                                                      
         MVI   3(R2),C'.'                                                       
         LA    R2,4(R2)                                                         
*                                                                               
         SR    RE,RE               DD                                           
         SLDL  RE,8                                                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R2),DUB                                                      
*                                                                               
PCVERX   J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* GET EXTRA PERFORMANCE DATA                                          *         
***********************************************************************         
GETXTRA  NTR1                                                                   
         MVC   GIXTOUT,SPACES                                                   
         CLC   GIXTDATA,ZEROS                                                   
         JE    XTRAX                                                            
*                                                                               
         EDIT  (B2,GIXTDATA+0),(5,GIXTOUT+00)                                   
         CLC   GIXTDATA+0(2),ZEROS                                              
         JNE   *+8                                                              
         MVI   GIXTOUT+04,C'0'                                                  
*                                                                               
         EDIT  (B2,GIXTDATA+2),(5,GIXTOUT+06)                                   
         CLC   GIXTDATA+2(2),ZEROS                                              
         JNE   *+8                                                              
         MVI   GIXTOUT+10,C'0'                                                  
*                                                                               
         EDIT  (B2,GIXTDATA+4),(5,GIXTOUT+12)                                   
         CLC   GIXTDATA+4(2),ZEROS                                              
         JNE   *+8                                                              
         MVI   GIXTOUT+16,C'0'                                                  
*                                                                               
         EDIT  (B2,GIXTDATA+6),(5,GIXTOUT+18)                                   
         CLC   GIXTDATA+6(2),ZEROS                                              
         JNE   *+8                                                              
         MVI   GIXTOUT+22,C'0'                                                  
*                                                                               
XTRAX    J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* FILTERS                                                             *         
***********************************************************************         
FILTERS  NTR1                                                                   
         MVI   FSPECWHY,0                                                       
         USING ADRRECD,R2                                                       
*                                                                               
         CLI   FAGC,0              AG FILTER                                    
         JE    FILT001                                                          
         CLC   ADRAGYID,FAG                                                     
         IC    R1,FAGC                                                          
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT001                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT001  CLI   FSAC,0              SA FILTER                                    
         JE    FILT002                                                          
         CLC   ADRAGYSC,FSA                                                     
         IC    R1,FSAC                                                          
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT002                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT002  CLI   FOVSYS,0            OV SYSTEM FILTER                             
         JE    *+14                                                             
         CLC   ADROVSYS,FOVSYS                                                  
         JNE   EXITNEQ                                                          
*                                                                               
         CLI   FSESYS,0            SE SYSTEM FILTER                             
         JE    *+14                                                             
         CLC   ADRSYSNO,FSESYS                                                  
         JNE   EXITNEQ                                                          
*                                                                               
         CLI   FPROG,0             PROGRAM FILTER                               
         JE    *+14                                                             
         CLC   ADRPRGNO,FPROG                                                   
         JNE   EXITNEQ                                                          
*                                                                               
         OC    FUSERID,FUSERID                                                  
         JZ    *+14                                                             
         CLC   ADRUSER,FUSERID                                                  
         JNE   EXITNEQ                                                          
*                                                                               
         OC    FPERSON,FPERSON                                                  
         JZ    *+14                                                             
         CLC   ADRPSWD,FPERSON                                                  
         JNE   EXITNEQ                                                          
*                                                                               
         LA    R1,FLUIDC           LUID FILTERS                                 
         CLI   0(R1),0                                                          
         JE    FILT020                                                          
FILT010  CLI   0(R1),X'FF'         TEST FOR EOT                                 
         JE    EXITNEQ                                                          
         CLI   0(R1),0             TEST FOR EOT                                 
         JE    EXITNEQ                                                          
*                                                                               
FILT011  LA    RF,8(R1)            CLUID                                        
         CLI   0(RF),0                                                          
         JNE   *+8                                                              
         JCT   RF,*-8                                                           
         SR    RF,R1                                                            
         BCTR  RF,0                                                             
         EXRL  RF,*+10             COMPARE FOR ENTRY LEN-1                      
         J     *+10                                                             
         CLC   ADRSYM(0),1(R1)                                                  
         IC    RF,0(R1)                                                         
         EXRL  RF,*+10                                                          
         J     *+8                                                              
         JC    0,FILT020                                                        
         LA    R1,9(R1)                                                         
         J     FILT010                                                          
*                                                                               
FILT020  CLI   FTASKC,0                                                         
         JE    FILT030                                                          
         CLC   ADRTASK,FTASK                                                    
         IC    R1,FTASKC                                                        
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT030                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT030  CLI   FIOCC,0                                                          
         JE    FILT050                                                          
         CLC   ADRIOCNT,FIOC+1                                                  
         IC    R1,FIOCC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT050                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT050  CLI   FOVCC,0                                                          
         JE    FILT060                                                          
         CLC   ADROVCNT,FOVC+3                                                  
         IC    R1,FOVCC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT060                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT060  CLI   FQTMC,0                                                          
         JE    FILT070                                                          
         MVC   FULL,ADRINTM                                                     
         ICM   R1,15,ADRSTTM                                                    
         S     R1,FULL                                                          
         CLM   R1,15,FQTM                                                       
         IC    R1,FQTMC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT070                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT070  CLI   FCPUC,0                                                          
         JE    FILT080                                                          
         CLC   ADRCPUTM,FCPU                                                    
         IC    R1,FCPUC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT080                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT080  LA    R1,FSINC                                                         
         CLI   0(R1),0                                                          
         JE    FILT090                                                          
FILT081  CLI   0(R1),0                                                          
         JE    FILT090                                                          
         CLI   0(R1),X'FF'                                                      
         JE    FILT090                                                          
         CLC   1(4,R1),ADRSIN                                                   
         IC    RF,0(R1)                                                         
         EXRL  RF,*+10                                                          
         J     *+8                                                              
         JC    0,EXITNEQ                                                        
         LA    R1,5(R1)                                                         
         J     FILT081                                                          
*                                                                               
FILT090  LA    R1,FSTIME                                                        
         CLI   0(R1),0                                                          
         JE    FILT100                                                          
FILT091  CLI   0(R1),0                                                          
         JE    FILT100                                                          
         CLI   0(R1),X'FF'                                                      
         JE    FILT100                                                          
         CLC   1(4,R1),ADRSTTM                                                  
         IC    RF,0(R1)                                                         
         EXRL  RF,*+10                                                          
         J     *+8                                                              
         JC    0,EXITNEQ                                                        
         LA    R1,5(R1)                                                         
         J     FILT091                                                          
*                                                                               
FILT100  LA    R1,FETIME                                                        
         CLI   0(R1),0                                                          
         JE    FILT110                                                          
FILT101  CLI   0(R1),0                                                          
         JE    FILT110                                                          
         CLI   0(R1),X'FF'                                                      
         JE    FILT110                                                          
         CLC   1(4,R1),ADRNDTM                                                  
         IC    RF,0(R1)                                                         
         EXRL  RF,*+10                                                          
         J     *+8                                                              
         JC    0,EXITNEQ                                                        
         LA    R1,5(R1)                                                         
         J     FILT101                                                          
*                                                                               
FILT110  CLI   EXTMC,0                                                          
         JE    FILT120                                                          
*                                                                               
         L     RF,ADRNDTM          END TIME                                     
         L     RE,ADRSTTM          START TIME                                   
         SR    RF,RE                                                            
         ST    RF,FULL             HALF=END-START                               
*                                                                               
         CLC   FULL,EXTM           EXECUTE TIME                                 
         IC    R1,EXTMC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT120                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT120  CLI   FSPEC,0              SPECIAL FILTERS                             
         JE    FILT140                                                          
         TM    ADRXPT,ADRXPTPC+ADRXPTMF                                         
         JZ    EXITNEQ                                                          
FILT121  CLC   ADRXPN,=H'1'         PCUNDEF                                     
         JNE   *+12                                                             
         MVI   FSPECWHY,C'1'                                                    
         J     FILT130                                                          
FILT122  CLC   ADRUSER,=H'0'        USERID ZERO                                 
         JNE   *+12                                                             
         MVI   FSPECWHY,C'2'                                                    
         J     FILT130                                                          
FILT123  CLC   ADRPSWD,=H'0'        PERSON ZERO                                 
         JNE   *+12                                                             
         MVI   FSPECWHY,C'3'                                                    
         J     FILT130                                                          
FILT124  CLC   ADRXPN,=H'6'         CM VIEWER                                   
         JNE   FILT125                                                          
         CLC   ADRXPV(2),=X'4203'   04.02.003                                   
         JNE   FILT125                                                          
         CLC   ADRAGYID,=C'UM'      CARAT                                       
         JNE   FILT125                                                          
         MVI   FSPECWHY,C'4'                                                    
         J     FILT130                                                          
FILT125  EQU   *                                                                
*                                                                               
FILT130  CLI   FSPECWHY,0                                                       
         JE    EXITNEQ                                                          
         CLI   FSPEC,C'0'           MATCH ON ANY SPECIAL                        
         JE    FILT140                                                          
         CLC   FSPEC,FSPECWHY       MATCH ON SPECIFIC                           
         JE    FILT140                                                          
         J     EXITNEQ                                                          
*                                                                               
FILT140  CLI   FLKSC,0              NUMBER OF LOCKER LOCKS                      
         JE    FILT142                                                          
         CLC   ADRLKRT,FLKS+2                                                   
         IC    R1,FLKSC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT142                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT142  CLI   FLKWC,0              NUMBER OF WAITED LOCKS                      
         JE    FILT144                                                          
         CLC   ADRLKRW,FLKW+2                                                   
         IC    R1,FLKWC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT144                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT144  CLI   FADDC,0              NUMBER OF RECOVERY ADDS                     
         JE    FILT146                                                          
         CLC   ADRRADD,FADD+2                                                   
         IC    R1,FADDC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT146                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT146  CLI   FCHGC,0              NUMBER OF RECOVERY CHANGES                  
         JE    FILT150                                                          
         CLC   ADRRCHG,FCHG+2                                                   
         IC    R1,FCHGC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT150                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT150  CLI   FPCPC,0              PC PROGRAM NUMBER                           
         JE    FILT160                                                          
         TM    ADRXPT,ADRXPTPC                                                  
         JZ    FILT160                                                          
         CLC   ADRXPN,FPCP+2                                                    
         IC    R1,FPCPC                                                         
         EXRL  R1,*+10                                                          
         J     *+8                                                              
         JC    0,FILT160                                                        
         J     EXITNEQ                                                          
*                                                                               
FILT160  LA    R1,FFLAGS           TASK FLAGS                                   
         LA    R0,8                                                             
FILT162  CLI   0(R1),C' '                                                       
         JNH   FILT164                                                          
         CLI   0(R1),C'U'          UPDATIVE                                     
         JNE   *+12                                                             
         TM    ADRFLAG1,ADRFUPD                                                 
         JZ    EXITNEQ                                                          
         CLI   0(R1),C'A'          ABENDED                                      
         JNE   *+12                                                             
         TM    ADRFLAG1,ADRFABND                                                
         JZ    EXITNEQ                                                          
         CLI   0(R1),C'L'          LOCKS REMOVED USING DMUNLK                   
         JNE   *+12                                                             
         TM    ADRFLAG2,ADRFUNLK                                                
         JZ    EXITNEQ                                                          
         CLI   0(R1),C'C'          =CT CONNECT                                  
         JNE   *+12                                                             
         TM    ADRFLAG2,ADRFCTC                                                 
         JZ    EXITNEQ                                                          
         CLI   0(R1),C'D'          =CT DISCONNECT                               
         JNE   *+12                                                             
         TM    ADRFLAG2,ADRFCTD                                                 
         JZ    EXITNEQ                                                          
         CLI   0(R1),C'E'          EXCLUDE CONNECTS/DISCONNECTS                 
         JNE   *+12                                                             
         TM    ADRFLAG2,ADRFCTC+ADRFCTD                                         
         JNZ   EXITNEQ                                                          
         CLI   0(R1),C'T'          TRANSACTIONS ONLY (NO SERVICE)               
         JNE   FILT164                                                          
         TM    ADRFLAG2,ADRFCTC+ADRFCTD                                         
         JNZ   EXITNEQ                                                          
         CLI   ADRSYSNO,1                                                       
         JE    EXITNEQ                                                          
FILT164  LA    R1,1(R1)                                                         
         JCT   R0,FILT162                                                       
*                                                                               
FILT170  J     EXITEQU              NO MORE FILTERS TO TEST                     
*                                                                               
FILTERN  J     EXITNEQ                                                          
FILTERY  J     EXITEQU                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* OUTPUT ADRREC AT R2 TO PLINE                                        *         
***********************************************************************         
ADRDISP  NTR1                                                                   
         USING ADRRECD,R2                                                       
         USING ADRLINED,PLINE                                                   
*                                                                               
         OC    0(100,R2),0(R2)     TEST EMPTY BUFFER                            
         JZ    ADREMPTY                                                         
*                                                                               
         LA    R1,=C'??MoTuWeThFrSaSu'                                          
         LLC   R0,ADRDAYNO                                                      
         SLL   R0,1                                                             
         AR    R1,R0                                                            
         MVC   ADLLDAY,0(R1)                                                    
*                                                                               
         MVC   FULL,ADRNDTM        END TIME                                     
         BRAS  RE,TIMEOUT                                                       
         MVC   ADLLTIME,WORK1                                                   
         MVC   ADLLTIME+8(2),WORK1+9                                            
*                                                                               
         MVC   ADLLUID,ADRSYM      LUID                                         
*                                                                               
         MVC   GTSYS,ADROVSYS      OV SYSTEM                                    
         BRAS  RE,GETOVS                                                        
         MVC   ADLSYS,GTSYS1       1 CHR NAME                                   
*                                                                               
         MVC   GTSYSSE,ADRSYSNO    SE SYSTEM                                    
         BRAS  RE,GETSYS                                                        
         MVC   ADLSEN(5),GTSYSN                                                 
         CLC   ADLSEN(3),=C'SER'                                                
         JNE   *+10                                                             
         MVC   ADLSEN+3(2),SPACES                                               
         CLC   ADLSEN(3),=C'CON'                                                
         JNE   *+10                                                             
         MVC   ADLSEN+3(2),SPACES                                               
*                                                                               
         MVC   GTPROG,ADRPRGNO     PROGRAM                                      
         BRAS  RE,GETPROG                                                       
         MVC   ADLPRG,GTPROGN                                                   
*                                                                               
         MVI   ADLTASK,C'#'        TASK                                         
         MVC   ADLTASK+1(1),ADRTASK                                             
*                                                                               
         EDIT  (B4,ADRSIN),(6,ADLSIN)                                           
*                                                                               
ADRD010  CLI   FSPEC,0             SPECIAL FILTERS                              
         JE    ADRD020                                                          
         CLI   FSPECWHY,0          ANY MATCH ON SPECIAL FILTER                  
         JE    ADRD020                                                          
         MVI   ADLTASK,C'>'        SPECIAL                                      
         MVC   ADLTASK+1(1),FSPECWHY                                            
         GOTO1 AHEXOUT,DMCB,ADRXPN+1,ADLSIN,1                                   
         GOTO1 AHEXOUT,DMCB,ADRXPV,ADLSIN+2,3                                   
ADRD020  EQU   *                                                                
*                                                                               
         MVC   FULL,ADRSTTM        CALCULATE PROCESS TIME                       
         L     R1,ADRNDTM                                                       
         S     R1,FULL                                                          
         ST    R1,FULL                                                          
         BRAS  RE,TIMEOUT                                                       
         BRAS  RE,T5CHR                                                         
         MVC   ADLPROC,WORK1                                                    
*                                                                               
         MVC   FULL,ADRINTM        CALCULATE WAIT TIME                          
         L     R1,ADRSTTM                                                       
         S     R1,FULL                                                          
         ST    R1,FULL                                                          
         BRAS  RE,TIMEOUT                                                       
         BRAS  RE,T5CHR                                                         
         MVC   ADLWAIT,WORK1                                                    
*                                                                               
         MVC   FULL,ADRCPUTM                                                    
         BRAS  RE,TIMEOUT                                                       
         BRAS  RE,T5CHR                                                         
         MVC   ADLCPU,WORK1                                                     
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),ADRIOCNT                                               
         EDIT  (B4,FULL),(5,ADLIOS)                                             
         OC    ADRIOCNT,ADRIOCNT                                                
         JNZ   *+8                                                              
         MVI   ADLIOS+4,C'-'                                                    
*                                                                               
         EDIT  (B1,ADROVCNT),(3,ADLOVS)                                         
         OC    ADROVCNT,ADROVCNT                                                
         JNZ   *+10                                                             
         MVC   ADLOVS,DOTS                                                      
*                                                                               
         EDIT  (B4,ADRCPUTK),(10,WORK),FILL=0                                   
         MVC   ADLRCPU(3),WORK+1                                                
         CLI   ADLRCPU+0,C'0'                                                   
         JNE   *+8                                                              
         MVI   ADLRCPU+0,C' '                                                   
         CLI   ADLRCPU+1,C'0'                                                   
         JNE   *+8                                                              
         MVI   ADLRCPU+1,C' '                                                   
         MVI   ADLRCPU+3,C'.'                                                   
         MVC   ADLRCPU+4(6),WORK+4                                              
*                                                                               
         MVC   ADLFLAGS,DOTS                                                    
         TM    ADRFLAG1,ADRFUPD    TEST UPDATIVE                                
         JZ    *+8                                                              
         MVI   ADLFLAGS+0,C'U'                                                  
         TM    ADRFLAG1,ADRFABND   TEST ABENDED                                 
         JZ    *+8                                                              
         MVI   ADLFLAGS+1,C'A'                                                  
         TM    ADRFLAG2,ADRFUNLK   TEST DID UNLOCK REMOVING LOCKS               
         JZ    *+8                                                              
         MVI   ADLFLAGS+2,C'L'                                                  
         TM    ADRFLAG2,ADRFCTC    TEST =CT CONNECT                             
         JZ    *+8                                                              
         MVI   ADLFLAGS+3,C'C'                                                  
         TM    ADRFLAG2,ADRFCTD    TEST =CT DISCONNECT                          
         JZ    *+8                                                              
         MVI   ADLFLAGS+3,C'D'                                                  
*                                                                               
         EDIT  (B4,ADROSIN),(6,ADLOSIN)                                         
*                                                                               
         MVC   ADLAGY,ADRAGYID     AGENCY ID                                    
*                                                                               
         MVC   GIUSER,ADRUSER                                                   
         BRAS  RE,GETUSER          GET USER ID                                  
         MVC   ADLUSER,SPACES                                                   
         MVC   ADLUSER,GIUSERID                                                 
*                                                                               
         MVC   ADLSEC,SPACES                                                    
         MVC   ADLSEC,ADRAGYSC     SECURITY AGENCY                              
         SR    R0,R0                                                            
         ICM   R0,3,ADRPSWD                                                     
         JZ    ADRD116                                                          
         CHI   R0,4096             DDS SECURITY                                 
         JH    *+10                                                             
*&&UK*&& MVC   ADLSEC,=C'#E'                                                    
*&&US*&& MVC   ADLSEC,=C'#N'                                                    
*                                                                               
ADRD116  MVC   GIPERS+0(2),ADRAGYSC                                             
         MVC   GIPERS+2(2),ADRPSWD PERSON                                       
         BRAS  RE,GETPERS                                                       
         MVC   ADLPERS,SPACES                                                   
         MVC   ADLPERS,GIPERSON                                                 
*                                                                               
         GOTO1 AHEXOUT,DMCB,ADRUDATA,ADLUHEX,2                                  
*                                                                               
ADRD120  CLI   FPCPC,0             DISPLAY PC PROGRAM DATA                      
         JE    ADRD130                                                          
         MVC   GIPCDATA,ADRXPI                                                  
         BRAS  RE,GETPCVER                                                      
         MVC   ADLPCINF,GIPCOUT                                                 
         J     ADRD140                                                          
*                                                                               
ADRD130  MVC   GIXTDATA,ADRTCB1    DISPLAY LOCKS,LOCKW,ADDS,CHGS                
         BRAS  RE,GETXTRA                                                       
         MVC   ADLPCINF,GIXTOUT                                                 
*                                                                               
ADRD140  J     ADRDISX                                                          
*                                                                               
ADREMPTY MVC   ADLLTIME,=C'EMPTY-->   '                                         
         OI    SAVEFLAG,SAVEEOT                                                 
*                                                                               
ADRDISX  J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* PUT PLINE OUT TO SCREEN                                             *         
***********************************************************************         
LINEOUT  NTR1                                                                   
         LA    R1,PLINE                                                         
         AH    R1,SAVELEFT                                                      
*                                                                               
         LA    R0,79                                                            
         LR    RF,R3                                                            
         LA    RE,TITLE                                                         
LINO010  CLI   0(RE),C'X'                                                       
         JE    LINO020                                                          
*                                                                               
         MVC   0(1,RF),0(R1)                                                    
         LA    RF,1(RF)                                                         
LINO020  LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         JCT   R0,LINO010                                                       
*                                                                               
LINEOUTX MVC   PLINE,SPACES                                                     
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* EDIT TUS IN FULL TO WORK HH:MM:SS.SS                                *         
***********************************************************************         
TIMEOUT  NTR1                                                                   
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,TUHOUR                                                        
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUMINUTE                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUSECOND                                                      
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,TUMSEC                                                        
         EDIT  (RF),(2,WORK1+9),FILL=0    100/SEC                               
         J     XIT1                                                             
*                                                                               
T5CHR    NTR1                                                                   
         CLC   WORK1(5),=C'00:00'         ZERO HH:MM                            
         JNE   *+14                                                             
         MVC   WORK1(5),WORK1+6           SET SS.00                             
         J     T5CHRX                                                           
*                                                                               
         CLC   WORK1(2),=C'00'            ZERO HH                               
         JNE   *+14                                                             
         MVC   WORK1(5),WORK1+3           SET MM:SS                             
         J     T5CHRX                                                           
*                                                                               
         MVC   WORK1(5),WORK1+1           SET H:MMH                             
         MVI   WORK1+4,C'H'                                                     
*                                                                               
T5CHRX   CLI   WORK1,C'0'                 REMOVE 1 LEADING ZERO                 
         JNE   *+8                                                              
         MVI   WORK1,C' '                                                       
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP RECORD DISPLACEMENTS FOR TRACE RECORDS                       *         
***********************************************************************         
SETRECS  NTR1                                                                   
         LA    R2,IOBUFF                                                        
         XC    RECDISP+000(200),RECDISP                                         
         XC    RECDISP+200(200),RECDISP+200                                     
         LA    R1,RECDISP          R1=A(DISP TABLE)                             
*                                                                               
SETR010  CLC   0(4,R2),=C'*TTRC'   ONLY DO THIS TO TRACE BUFFERS                
         JNE   SETRECX                                                          
         LA    RF,10               RF IS CUMULATIVE DISPLACEMENT                
         LR    R0,RF                                                            
         J     SETR025                                                          
*                                                                               
SETR020  SR    R0,R0                                                            
         ICM   R0,1,1(R2)          R0 = RECORD LEN                              
         JZ    SETRECX                                                          
         AR    RF,R0                                                            
SETR025  STCM  RF,3,0(R1)          SAVE OFFSET TO RECORD                        
         LA    R1,2(R1)                                                         
         AR    R2,R0                                                            
         J     SETR020                                                          
*                                                                               
SETRECX  J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* PUT PLINE OUT TO SCREEN                                             *         
***********************************************************************         
TMPDSP   NTR1                                                                   
         USING LINED,PLINE                                                      
         USING TMPLRECD,R2                                                      
*                                                                               
         MVC   FULL,TMPLTIME       DISP TIME                                    
         OC    FULL,FULL                                                        
         JZ    TMPD900                                                          
*                                                                               
         BRAS  RE,TIMEOUT                                                       
         MVC   LINETIM,WORK1                                                    
         MVC   LINELUID,TMPLLUID   DIPLAY LUID                                  
*                                                                               
         CLC   TMPLCODE,=X'0001'                                                
         JNE   TMPD100                                                          
*                                                                               
         CLI   TLTALLOC,C'A'                                                    
         JNE   *+10                                                             
         MVC   LINEDATA(7),=C'ALLOC  '                                          
         CLI   TLTALLOC,C'D'                                                    
         JNE   *+10                                                             
         MVC   LINEDATA(7),=C'DEALLOC'                                          
         LLC   R1,TLTSESS                                                       
         LA    R1,C'A'(R1)                                                      
         STC   R1,LINEDATA+8                                                    
         EDIT  (B1,TLTNUM),(2,WORK1),FILL=0                                     
         MVC   LINEDATA+10(2),WORK1                                             
*                                                                               
TMPD100  CLC   TMPLCODE,=X'0003'   WAIT DATA                                    
         JNE   XIT1                                                             
         CLI   TWTTYPE,C'W'                                                     
         JNE   *+14                                                             
         MVC   WAITTYPE(6),=C'WAIT  '                                           
         J     TMPD110                                                          
*                                                                               
         CLI   TWTTYPE,C'X'                                                     
         JNE   *+14                                                             
         MVC   WAITTYPE(6),=C'CLEAR '                                           
         J     TMPD110                                                          
*                                                                               
         CLI   TWTTYPE,C'P'                                                     
         JNE   *+14                                                             
         MVC   WAITTYPE(6),=C'POST  '                                           
         J     TMPD110                                                          
*                                                                               
         DC    H'0'                                                             
*                                                                               
TMPD110  GOTO1 AHEXOUT,DMCB,TWTLOCK,WAITLOCK,8                                  
*                                                                               
         GOTO1 AHEXOUT,DMCB,TWTECB,WAITECB,8                                    
         J     XIT1                                                             
*                                                                               
TMPD900  MVC   LINETIM(3),=C'==>'                                               
         J     XIT1                                                             
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADV/TASK OR JOB NUMBER INFO - FULL=AL2(NN),JOBFLG,ADVN      *         
***********************************************************************         
DISJOB   NTR1                                                                   
         TM    FULL+2,X'80'        TEST OFFLINE JOB                             
         JO    DJOB085                                                          
         MVC   FULL+3(1),FULL+0                                                 
         MVC   BYTE,FULL+0                                                      
         L     R1,AFID                                                          
         NI    BYTE,X'0F'                                                       
DJOB081  CLC   BYTE,4(R1)          FIND ADV SYSTEM                              
         JE    DJOB082                                                          
         LA    R1,8(R1)                                                         
         CLI   5(R1),X'FF'         CHECK EOT                                    
         JNE   DJOB081                                                          
         DC    H'0'                                                             
DJOB082  MVC   DUB1(4),0(R1)                                                    
         CLI   DUB1+3,C' '                                                      
         JE    *+14                                                             
         MVC   DUB1+2(1),DUB1+3                                                 
         MVI   DUB1+3,C' '                                                      
*                                                                               
         MVC   DUB1+4(2),=C'/#'                                                 
         MVC   DUB1+6(1),FULL+1                                                 
         TM    FULL+3,X'F0'                                                     
         JZ    DISJOBX                                                          
         LLC   R1,FULL+3                                                        
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,DUB1+3                                                        
         J     DISJOBX                                                          
*                                                                               
DJOB085  MVC   DUB1,=C'J       '   JOB NO                                       
         SR    R1,R1                                                            
         ICM   R1,3,FULL                                                        
*&&US*&& N     R1,=X'0000FFFF'                                                  
*&&UK*&& N     R1,=X'00007FFF'                                                  
         EDIT  (R1),(5,DUB1+1),FILL=0                                           
DISJOBX  J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND COMMON CODE USING RA                                  *         
***********************************************************************         
$$DATA   LOCTR                                                                  
* DDVALNUM                                                                      
       ++INCLUDE DDVALNUM                                                       
*                                                                               
SRVMSG1  DC    CL60'XXXXX DA='                                                  
*                                                                               
SRVPFKS  DC    CL40'PF3=Reset PF4=Tk/Up PF5=Tk/Down PF7=Up P'                   
         DC    CL39'F8=Down PF10=Left PF11=Right           '                    
*                                                                               
STITLE1  DC    CL40'Da End Time   Terminal S SEsys Prg T# Si'                   
         DC    CL40'n    Total Queue  Task  I/Os Task Flags '                   
         DC    CL40'CPU Actual Osin   Rte Ag Userid   SA Per'                   
         DC    CL40'sonId Data Locks Lockw Radds Rchgs      '                   
STITLE1P DC    CL40'nId Data PCfl PC Program PC Version     '                   
STITLE2  DC    CL40'-- ---------- -------- - ----- --- -- --'                   
         DC    CL40'----  time  time  time  ---- ---------- '                   
         DC    CL40'---------- ------ --- -- -------- -- ---'                   
         DC    CL40'----- ---- ----- ----- ----- -----      '                   
STITLE2P DC    CL40'--- ---- ---- ---------- -------------- '                   
*                                                                               
TTITLE1  DC    C'Job Id  Time        Type   Trace data   '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
TTITLE2  DC    C'------- ----------- ---- - -------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
*                                                                               
ATITLE1  DC    C'                                        '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
         DC    C'                                        '                      
ATITLE2  DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
         DC    C'----------------------------------------'                      
*                                                                               
MAXIOS   DC    AL3(1000)                                                        
MAXIOSA  DC    CL4'1000'                                                        
HELPID   DC    XL10'0124FF00010000000000'                                       
TRID     DC    CL17'=TTR '                                                      
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
DMRSEQ   DC    CL8'DMRSEQ'                                                      
ADRFILE  DC    CL8'ADRFILE'                                                     
TEMPSTR  DC    CL8'TEMPSTR'                                                     
CTFILE   DC    CL8'CTFILE'                                                      
SPACES   DC    CL160' '                                                         
DOTS     DC    12C'.'                                                           
ZEROS    DC    XL8'00'                                                          
TUHOUR   DC    F'138240000'        60*60*38400                                  
TUMINUTE DC    F'2304000'          60*38400                                     
TUSECOND DC    F'38400'            38400                                        
TUMSEC   DC    F'384'              384                                          
       ++INCLUDE FASYSLST                                                       
TRCTAB   DC    X'0001',C'DNEXT '                                                
         DC    X'0002',C'SCRN  '                                                
         DC    X'0003',C'ENQDEQ'                                                
         DC    X'0004',C'GETCI '                                                
         DC    X'0005',C'I/O   '                                                
         DC    X'FFFF',C'??????'                                                
*                                                                               
SETBXLE  LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         BR    RE                                                               
*                                                                               
EXITEQU  CR    RB,RB                                                            
         J     XIT1                                                             
EXITNEQ  LTR   RB,RB                                                            
*                                                                               
XIT1     XIT1                                                                   
*                                                                               
ERRIO    LA    R1,=C'1000 I/OS DONE  '                                          
         MVC   0(4,R1),MAXIOSA                                                  
         L     RD,BASERD                                                        
         L     RB,BASERB                                                        
         USING TTRC,RB                                                          
         J     ERRX                                                             
         LTORG                                                                  
*                                                                               
FILLER   DS    CL(4096-(FILLER-TTRC))                                           
$$CODE   LOCTR                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
SDUB     DS    D                                                                
FULL     DS    F                                                                
SFULL    DS    F                                                                
HALF     DS    H                                                                
FLAG     DS    X                                                                
BYTE     DS    X                                                                
SHORT    DS    X                                                                
TRACE    DS    X                                                                
         DS    XL2                                                              
*                                                                               
WORK     DS    XL32                                                             
WORK1    DS    XL32                                                             
SWORK1   DS    XL32                                                             
*                                                                               
         DS    0F                                                               
SRPARS   DS    0CL24                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
*                                                                               
CARDRD   DS    A                                                                
CARDR2   DS    A                                                                
CARDEND  DS    A                                                                
*                                                                               
DMCB     DS    6F                                                               
RELO     DS    A                                                                
BASERD   DS    A                                                                
BASERB   DS    A                                                                
SAVERE   DS    A                                                                
ACTREC   DS    A                                                                
ASYSLST  DS    A                                                                
AIOBUFF  DS    A                                                                
AIOBUFFX DS    A                                                                
ADATAMGR DS    A                                                                
AHEXOUT  DS    A                                                                
AGETHELP DS    A                                                                
AGETFACT DS    A                                                                
AHEXIN   DS    A                                                                
ASSB     DS    A                                                                
ASELIST  DS    A                                                                
ATEMPTRC DS    A                                                                
AADRBUFF DS    A                                                                
AADRNEXT DS    A                                                                
AHELP    DS    A                                                                
CURSOR   DS    A                                                                
ASAVE    DS    A                                                                
ATERMVAL DS    A                                                                
AUTL     DS    A                                                                
*                                                                               
DA       DS    F                                                                
DISP     DS    F                                                                
ADRDNEXT DS    F                                                                
*                                                                               
ACURS    DS    A                                                                
CURI     DS    X                                                                
PFKEY    DS    X                                                                
MYTCTRY  DS    X                   TCTRY                                        
RECLEN   DS    H                                                                
TRM      DS    H                                                                
BUFFMAX  DS    H                                                                
BUFFREC  DS    H                                                                
BUFFLEN  DS    H                                                                
TORAOR   DS    X                                                                
SYSNAME  DS    CL5                                                              
MYLUID   DS    CL8                                                              
MYSSB    DS    A                                                                
MYALET   DS    A                                                                
AFID     DS    A                                                                
*                                                                               
FLTSIN   DS    XL4                                                              
*                                                                               
GTSYS    DS    X                   OV SYSTEM                                    
GTSYSSE  DS    X                   SE SYSTEM                                    
GTSYSL   DS    X                   LEN FOR OVSYS NAME SEARCH                    
GTSYSN   DS    CL7                 SYS NAME                                     
GTSYS1   DS    C                   SYS NAME 1CHR                                
GTPROGL  DS    X                   LEN FOR PROG NAME SEARCH                     
GTPROGN  DS    CL8                 PROG NAME                                    
GTPROG   DS    X                   PROG NUMBER                                  
GTLANGN  DS    CL3                 LANGUAGE NAME                                
GTLANG   DS    X                   LANGUAGE CODE                                
GTAPGMS  DS    A                   A(PRGMS LIST FOR SYSTEM)                     
*                                                                               
GIUSER   DS    CL2                 WORK AREA FOR GETUSER                        
GIUPREV  DS    CL2                                                              
GIULEN   DS    CL1                                                              
GIUSERID DS    CL8                                                              
*                                                                               
GIPERS   DS    CL4                 WORK AREA FOR GETPERS                        
GIPPREV  DS    CL4                                                              
GIPLEN   DS    CL1                                                              
GIPERSON DS    CL8                                                              
*                                                                               
GIPCDATA DS    CL6                                                              
GIPCPREV DS    CL6                                                              
GIPCOUT  DS    CL30                PMGC CL10 NAME  V00.00.000.000               
*                                                                               
GIXTDATA DS    XL8                                                              
GIXTOUT  DS    CL30                LOCKS LOCLW RADDS RCHGS                      
*                                                                               
ACTION   DS    CL12                                                             
TRCACT   DS    CL1                                                              
*                                                                               
PLINE    DS    CL250                                                            
TITLE    DS    CL250                                                            
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
FAGC     DC    X'00'                                                            
FAG      DC    C'  '               AG FILTER                                    
FSAC     DC    X'00'                                                            
FSA      DC    C'  '               SA FILTER                                    
FTASKC   DC    X'00'                                                            
FTASK    DC    C' '                TASK FILTER                                  
FIOCC    DC    X'00'                                                            
FIOC     DC    XL4'00000000'       I/O COUNT FILTER                             
FOVCC    DC    X'00'                                                            
FOVC     DC    XL4'00000000'       OV COUNT FILTER                              
FQTMC    DC    X'00'                                                            
FQTM     DC    XL4'00000000'       QUEUE TIME                                   
EXTMC    DC    X'00'                                                            
EXTM     DC    XL4'00000000'       EXECUTE TIME                                 
FCPUC    DC    X'00'                                                            
FCPU     DC    XL4'0000'           CPU TIME                                     
FPCPC    DC    X'00'                                                            
FPCP     DC    XL4'00000000'       PC PROGRAM NUMBER                            
*                                                                               
FLKSC    DC    X'00'                                                            
FLKS     DC    XL4'00000000'       LOCKS FILTER                                 
FLKWC    DC    X'00'                                                            
FLKW     DC    XL4'00000000'       LOCKS WAITED FILTER                          
FADDC    DC    X'00'                                                            
FADD     DC    XL4'00000000'       RECOVERY ADDS FILTER                         
FCHGC    DC    X'00'                                                            
FCHG     DC    XL4'00000000'       RECOVERY CHGS FILTER                         
*                                                                               
FSPEC    DC    X'00'                                                            
FSPECWHY DC    X'00'                                                            
*                                                                               
FSESYS   DC    X'00'                                                            
FOVSYS   DC    X'00'                                                            
FPROG    DC    X'00'                                                            
*                                                                               
FUSERID  DS    X'0000'                                                          
FPERSON  DS    X'0000'                                                          
*                                                                               
FFLAGS   DC    CL8' '                                                           
*                                                                               
FLUIDC   DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
FLUIDCX  DC    X'FF'                                                            
*                                                                               
SLUIDC   DC    X'00',C'        '                                                
         DC    X'00',C'        '                                                
SLUIDCX  DC    X'FF'                                                            
*                                                                               
FSINC    DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
FSINCX   DC    X'FF'                                                            
*                                                                               
FSTIME   DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
FSTIMEX  DC    X'FF'                                                            
*                                                                               
FETIME   DC    X'00',XL4'00'                                                    
         DC    X'00',XL4'00'                                                    
FETIMEX  DC    X'FF'                                                            
*                                                                               
SAVEDSTR EQU   *                   SAVED PART OF W/S                            
TOPDA    DS    F                   TOP RECORD                                   
TOPDISP  DS    F                   TOP DISPLACEMENT                             
BOTDA    DS    F                   BOTTOM RECORD                                
BOTDISP  DS    F                   BOTTOM DISPLACEMENT                          
OLDDA    DS    F                                                                
SCREENAD DS    F                   CURRENT TRACE SCREEN                         
SAVELEFT DS    H                   CURRENT LH DISPLACEMENT                      
SAVEFLAG DS    X                                                                
SAVEEOT  EQU   X'80'                                                            
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
RECDISP  DS    400XL2                                                           
*                                                                               
IOBUFF   DS    6400C                                                            
IOBUFFX  EQU   *                                                                
CTREC    DS    4096C                                                            
*                                                                               
WRKX     EQU   *                                                                
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
ADRLINED DSECT                                                                  
ADLLDAY  DS  CL2                                                                
         DS  CL1                                                                
ADLLTIME DS  CL10                                                               
         DS  CL1                                                                
ADLLUID  DS  CL8                                                                
         DS  CL1                                                                
ADLSYS   DS  CL1                                                                
         DS  CL1                                                                
ADLSEN   DS  CL5                                                                
         DS  CL1                                                                
ADLPRG   DS  CL3                                                                
         DS  CL1                                                                
ADLTASK  DS  CL2                                                                
         DS  CL1                                                                
ADLSIN   DS  CL6                                                                
         DS  CL1                                                                
ADLPROC  DS  CL5                                                                
         DS  CL1                                                                
ADLWAIT  DS  CL5                                                                
         DS  CL1                                                                
ADLCPU   DS  CL5                                                                
         DS  CL1                                                                
ADLIOS   DS  CL5                                                                
         DS  CL1                                                                
ADLFLAGS DS  CL10                                                               
         DS  CL1                                                                
ADLRCPU  DS  CL10                                                               
         DS  CL1                                                                
ADLOSIN  DS  CL6                                                                
         DS  CL1                                                                
ADLOVS   DS  CL3                                                                
         DS  CL1                                                                
ADLAGY   DS  CL2                                                                
         DS  CL1                                                                
ADLUSER  DS  CL8                                                                
         DS  CL1                                                                
ADLSEC   DS  CL2                                                                
         DS  CL1                                                                
ADLPERS  DS  CL8                                                                
         DS  CL1                                                                
ADLUHEX  DS  CL4                                                                
         DS  CL1                                                                
ADLPCINF DS  CL30                                                               
*                                                                               
TRCLINED DSECT                                                                  
TRCLREF  DS  CL7                   ADV1A/#1                                     
         DS  CL1                                                                
TRCLTIME DS  CL11                  HH:MM:SS.00                                  
         DS  CL1                                                                
TRCLCODE DS  CL4                   SCRN ENQD GETC NNNN                          
         DS  CL1                                                                
TRCLSTAT DS  CL1                   STAT W=WRITE                                 
         DS  CL1                                                                
TRCLDATA DS  CL223                 DATA                                         
*                                                                               
LINED    DSECT                                                                  
LINELIN  DS  0CL79                                                              
LINETIM  DS  CL11                                                               
         DS  CL1                                                                
LINELUID DS  CL8                                                                
         DS  CL1                                                                
LINEDATA DS  CL58                                                               
*                                                                               
         ORG LINEDATA                                                           
WAITTYPE DS  CL8                   WAIT/POST/CLR                                
         DS  CL5                                                                
WAITLOCK DS  CL16                  LOCKTAB ENTRY                                
         DS  CL5                                                                
WAITECB  DS  CL16                  ECB                                          
         ORG                                                                    
*                                                                               
LINELEN  EQU *-LINED                                                            
*                                                                               
SRTTRFFD DSECT                                                                  
         DS    CL64                                                             
* SRTTRFFD                                                                      
       ++INCLUDE SRTTRFFD                                                       
         EJECT                                                                  
* DDTEMPREC                                                                     
       ++INCLUDE DDTEMPREC                                                      
* FAUTL                                                                         
* FASSB                                                                         
* FAADRRECA                                                                     
* FATIOB                                                                        
* FASYSLSTD                                                                     
* FASRPARM                                                                      
* FAPGMLST                                                                      
* FASELIST                                                                      
* FASRS                                                                         
* FASYSFAC                                                                      
* SEACSFILE                                                                     
* DMDTFPHD                                                                      
* CTGENFILE                                                                     
* DDCOMFACS                                                                     
* FAXPTABD                                                                      
* FAFACTS                                                                       
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FASSB                                                          
       ++INCLUDE FATCB                                                          
       ++INCLUDE FAADRREC                                                       
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FASRPARM                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FASRS                                                          
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE DMDSHDR                                                        
       ++INCLUDE DDTRACED                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAXPTABD                                                       
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SRTTR00   03/24/15'                                      
         END                                                                    
