*          DATA SET CTDEB00    AT LEVEL 016 AS OF 01/09/13                      
*PHASE TA0F00A                                                                  
*INCLUDE DECODE                                                                 
*INCLUDE DEBDIS                                                                 
*INCLUDE XSORT                                                                  
*&&      SET   NOP=N                                                            
         TITLE 'CTDEB00 - DEBUG MODULE'                                         
CTDEBUG  CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**DEB**,RA,R9,RR=R4,CLEAR=Y                          
         USING WORKD,RC                                                         
         ST    R4,RELO                                                          
         ST    RD,SAVERD                                                        
         MVC   SVPARMS,0(R1)       SAVE S/R PARM LIST                           
         L     R8,ATWA                                                          
         USING CTDEBFFD,R8         R9=A(TWA)                                    
         LR    R7,R8                                                            
         A     R7,=F'4096'                                                      
         USING SAVED,R7            R7=A(SAVED)                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF         RF=A(COMFACS)                                
         MVC   APROTOFF,CPROTOFF                                                
         GOTO1 APROTOFF            NO PROTECTION FOR DEBUG                      
         DROP  RF                  (only called once)                           
*                                                                               
         USING COMFACSD,RF         RF=A(COMFACS)                                
INIT000  L     RF,ACOMFACS                                                      
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   ATERMVAL,CTERMVAL                                                
         MVC   APROTOFF,CPROTOFF                                                
         MVC   APROTON,CPROTON                                                  
         MVC   AGETFACT,CGETFACT                                                
         MVC   AGETTXT,CGETTXT                                                  
         MVC   APARSNIP,CPARSNIP                                                
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         DROP  RF                                                               
*                                                                               
         CLI   INITFLG,C'Y'                                                     
         BE    INIT010                                                          
*                                                                               
         SR    R1,R1               CLEAR WS FIRST TIME ROUND                    
         SR    R0,R0                                                            
         LH    RF,=H'14336'                                                     
         LR    RE,R7                                                            
         MVCL  RE,R0                                                            
         MVI   INITFLG,C'N'        SET INIT FLAG TO NEW                         
*                                                                               
INIT010  L     R1,=A(IOAREA-WORKD) SET UP COMMON ADCONS                         
         AR    R1,RC                                                            
         ST    R1,AIO                                                           
         L     R1,=A(WKBUFF-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AWKBUFF                                                       
*                                                                               
         L     R1,=V(DECODE)                                                    
         A     R1,RELO                                                          
         ST    R1,ADECODE                                                       
*                                                                               
         L     R1,=A(WRITTWA)                                                   
         A     R1,RELO                                                          
         ST    R1,AWRITTWA                                                      
         L     R1,=A(READTWA)                                                   
         A     R1,RELO                                                          
         ST    R1,AREADTWA                                                      
         L     R1,=A(LOCATE)                                                    
         A     R1,RELO                                                          
         ST    R1,ALOCATE                                                       
         L     R1,=A(CLOCATE)                                                   
         A     R1,RELO                                                          
         ST    R1,ACLOCATE                                                      
         L     R1,=A(LOCPSW)                                                    
         A     R1,RELO                                                          
         ST    R1,ALOCPSW                                                       
         L     R1,=A(VALNUM)                                                    
         A     R1,RELO                                                          
         ST    R1,AVALNUM                                                       
         L     R1,=A(GETUSE)                                                    
         A     R1,RELO                                                          
         ST    R1,AGETUSE                                                       
         L     R1,=A(READMOD)                                                   
         A     R1,RELO                                                          
         ST    R1,AREADMOD                                                      
         L     R1,=A(RANDOM)                                                    
         A     R1,RELO                                                          
         ST    R1,ARANDOM                                                       
         L     R1,=A(SETCOM)                                                    
         A     R1,RELO                                                          
         ST    R1,ASETCOM                                                       
*                                                                               
         L     R1,ASYSFACS                                                      
         L     RF,VTCB-SYSFACD(R1)                                              
         ST    RF,ATCB                                                          
         L     RF,VSSB-SYSFACD(R1)                                              
         ST    RF,ASSB                                                          
         L     RF,VTICTOC-SYSFACD(R1)                                           
         ST    RF,ATICTOC                                                       
         L     RF,VLOCKSPC-SYSFACD(R1)                                          
         ST    RF,ALOCKSPC                                                      
         L     RF,VARREDIT-SYSFACD(R1)                                          
         ST    RF,AARREDIT                                                      
*                                                                               
INIT020  L     R1,ASYSFACS         EXTRACT SSB VALUES                           
         L     R1,VSSB-SYSFACD(R1)                                              
         USING SSBD,R1                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   FACID,SSBSYSID                                                   
         MVC   FACN4,SSBSYSN4                                                   
         CLI   FACN4+3,C' '                                                     
         BH    *+8                                                              
         MVI   FACN4+3,C'.'                                                     
         MVC   MYTCB,SSBTKADR                                                   
         MVC   LOCORE,SSBLOADR                                                  
         CLI   ADDFLAG,C'D'                                                     
         BNE   *+10                                                             
         MVC   DSPALET,SSBALET                                                  
         CLI   ADDFLAG,C'T'                                                     
         BNE   *+10                                                             
         MVC   DSPALET,SSBTBLET                                                 
         CLI   ADDFLAG,C'P'                                                     
         BNE   *+10                                                             
         MVC   DSPALET,SSBPGMTA                                                 
         CLI   ADDFLAG,C'M'                                                     
         BNE   *+10                                                             
         MVC   DSPALET,SSBMEDTA                                                 
         DROP  R1                                                               
*                                                                               
INIT030  L     R1,AUTL             EXTRACT UTL VALUES                           
         USING UTLD,R1                                                          
         MVC   TRM,TNUM                                                         
         MVC   USERID,TUSER                                                     
*                                                                               
         OI    TSTAT7,TST7DIR      DEFAULT TO TOR                               
*        OI    TTORAOR,X'02'       UNLESS =AOR USED                             
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R1,DEBACTH          SET INITIAL CURSOR POS                       
         ST    R1,CURSOR                                                        
*                                                                               
         OC    SCROLL,SCROLL                                                    
         BNZ   *+10                                                             
         MVC   SCROLL,=H'22'       SET DEFAULT SCROLL                           
*                                                                               
         L     RF,ATIOB                                                         
         MVC   PFKEY,TIOBAID-TIOBD(RF)                                          
*                                                                               
         CLI   PFKEY,9             PF9 SET SCREEN SPLIT                         
         BNE   INIT040                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,3,TIOBCURS-TIOBD(RF)                                          
         D     R0,=F'80'                                                        
         SHI   R1,2                                                             
         CHI   R1,5                5 IS MINIMUM                                 
         BH    *+8                                                              
         LA    R1,5                                                             
         STH   R1,SCROLL                                                        
         STH   R1,SIZE                                                          
*                                                                               
INIT040  CLI   INITFLG,C'N'        TEST FOR NEW SESSION                         
         BNE   INI050                                                           
         MVC   INFIELD(4),FACN4    AUTO LOAD SYSTEM ADDCONS                     
         CLC   FACN4(3),=C'ADV'                                                 
         BNE   *+10                                                             
         MVC   FACN4(4),=C'LADV'                                                
*NOP     BAS   RE,LOADER           DON'T AUTOLOAD                               
         MVC   INFIELD(3),=C'OFF'  SET ADDRESS CONTROL                          
         BAS   RE,SETACCS                                                       
         MVC   SIZE,SCROLL                                                      
         MVI   ACTION,4            DEFAULT TO DISPLAY                           
         MVI   OVERLY,1                                                         
         MVI   SCREEN,255                                                       
         MVI   INITFLG,C'Y'        SET INITIALISED                              
*                                                                               
INI050   CLI   PFKEY,12            PF12 GOTO =DEBUG                             
         BNE   INIT500                                                          
*                                                                               
GODEBUG  L     R1,AUTL             LOCATE UTL                                   
         L     R1,TBUFF-UTLD(R1)   LOCATE TERMINAL BUFFER                       
         MVI   0(R1),X'0B'         LENGTH FOR =DEBUG                            
         MVC   1(2,R1),DEBSRVH+2   INSERT SCREEN ADDRESSES                      
         MVC   3(7,R1),=C'=DEBUG ' =DEBUG  IN SRVID FIELD                       
         MVI   10(R1),0            END MARKER                                   
         MVC   DEBSRV(8),=C'=DEBUG  '                                           
         OI    DEBSRVH+6,X'80'     AND XMIT                                     
         B     XXXXX                                                            
*                                                                               
INIT500  CLI   PFKEY,1             PFK 2                                        
         BNE   INIT510                                                          
         MVI   COMMAND,8           SET GO COMMAND                               
*                                                                               
INIT510  CLI   PFKEY,2             PFK 2                                        
         BNE   INIT520                                                          
         MVI   COMMAND,12          SET STEP COMMAND                             
INIT520  EQU   *                                                                
*                                                                               
*NOP     CLI   PFKEY,2                                                          
*NOP     BNE   INI900                                                           
*NOP     MVC   STEPS,=F'100'                                                    
*                                                                               
INI900   BAS   RE,MAIN             EXECUTE MAIN PROG                            
*                                                                               
         ICM   R2,15,STEPS         IF MULTI STEP THEN LOOP                      
         BZ    XMOD1                                                            
*                                                                               
         L     R1,CURSOR           SET CURSOR POS                               
         OI    6(R1),X'40'                                                      
         GOTO1 AGETFACT,DMCB,(X'80',WORK),F#WRITE                               
         GOTO1 AGETFACT,DMCB,(X'80',=F'3840'),F#WAIT 1/10 SECOND                
         BCTR  R2,0                                                             
         ST    R2,STEPS                                                         
         B     INI900              LOOP BACK TO MAIN                            
*                                                                               
XMOD1    L     R1,CURSOR           SET CURSOR POS                               
         OI    6(R1),X'40'                                                      
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,ERROR          TEST FOR ERROR MESSAGE                       
         BNZ   ERRX                                                             
         ICM   R0,3,INFO           TEST FOR INFO MESSAGE                        
         BNZ   INFOX                                                            
         ICM   R0,3,MYERROR        TEST FOR ERROR MESSAGE                       
         BNZ   MYERRX                                                           
         ICM   R0,3,MYINFO         TEST FOR INFO MESSAGE                        
         BNZ   MYINFOX                                                          
         LHI   R0,4                JUST PUT CONTROL DEBUG MESSAGE THEN          
         B     MYINFOX                                                          
         B     XXXXX                                                            
*                                                                               
ERRX     XC    DMCB(24),DMCB       R0 HAS CONTROL SYSTEM ERR NUM                
         GOTO1 AGETTXT,DMCB,(R0),0,(C'E',0),(4,FACN4)                           
         B     XXXXX                                                            
*                                                                               
INFOX    XC    DMCB(24),DMCB       R0 HAS CONTROL SYSTEM INF NUM                
         GOTO1 AGETTXT,DMCB,(R0),0,(C'I',0),(4,FACN4),SUBST                     
         B     XXXXX                                                            
*                                                                               
MYERRX   BCTR  R0,0                INDEX BY 32 FOR ERROR LEN                    
         SLL   R0,5                                                             
         L     R1,=A(ERRORS-CTDEBUG)                                            
         AR    R1,R0                                                            
         AR    R1,RB                                                            
         MVC   DEBMSG,SPACES                                                    
         MVC   DEBMSG(7),=C'(....) '                                            
         MVC   DEBMSG+1(4),FACN4                                                
         MVC   DEBMSG+7(32),0(R1)                                               
         B     XXXXX                                                            
*                                                                               
MYINFOX  BCTR  R0,0                INDEX BY 32 FOR INFO LEN                     
         SLL   R0,5                                                             
         L     R1,=A(INFOS-CTDEBUG)                                             
         AR    R1,R0                                                            
         AR    R1,RB                                                            
         MVC   DEBMSG,SPACES                                                    
         MVC   DEBMSG(7),=C'(....) '                                            
         MVC   DEBMSG+1(4),FACN4                                                
         MVC   DEBMSG+7(32),0(R1)                                               
         B     XXXXX                                                            
*                                                                               
XXXXX    GOTO1 APROTON             Turn it back on                              
         XMOD1 1                   EXIT PROGRAM                                 
*                                                                               
XITEQU   CR    RB,RB               EXIT CC EQU                                  
         B     XIT1                                                             
XITNEQ   LTR   RB,RB               EXIT CC NEQ                                  
*                                                                               
XIT1     XIT1                      EXIT                                         
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
MAIN     NTR1                                                                   
         CLI   COMMAND,0                                                        
         BNE   MAIN030                                                          
*                                                                               
*NOP     CLI   COMMAND,12          ARE WE STEPPING                              
*NOP     BE    MAIN030                                                          
*                                                                               
*NOP     CLI   PFKEY,0             GO STRAIGHT TO COMMAND IF PFKEY              
*NOP     BNE   MAIN030                                                          
*                                                                               
*        MVI   COMMAND,0           ALWAYS CLEAR COMMAND                         
*                                                                               
MAIN010  MVI   PROT,C' '                                                        
         CLI   DEBACTH+5,0         TEST ANY INPUT                               
         BE    MAIN030                                                          
*                                                                               
         GOTO1 APARSNIP,DMCB,DEBACTH,(10,PARBLK),(X'50',SEPTAB)                 
         MVC   NPARMS,DMCB+4       SAVE NUMBER OF COMPONENTS                    
         LA    R1,PARBLK                                                        
         ST    R1,APARMS           SAVE A(FIRST PARM)                           
*                                                                               
MAIN015  BAS   RE,VALACT           VALIDATE ACTION                              
         BE    MAIN030                                                          
         CLI   OVERLY,0                                                         
         BNE   MAIN030                                                          
         MVC   ERROR,=H'11'        SET INVALID ACTION IF NO OVERLAY             
         B     MAINX                                                            
*                                                                               
MAIN030  CLI   COMMAND,0           IS THIS A COMMAND                            
         BE    MAIN040                                                          
         BAS   RE,COMM             DO THIS FIRST                                
*                                                                               
MAIN040  CLI   OVERLY,0            TEST WE ARE AT ROOT LEVEL                    
         BE    MAIN050                                                          
*                                                                               
         BAS   RE,LOADSCR          GO LOAD ANY SCREEN                           
*                                                                               
         GOTO1 ACALLOV,DMCB,(OVERLY,0),ATWA                                     
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF CAN'T LOAD PHASE                      
*                                                                               
         L     RF,0(R1)            CALL OVERLAY PASSING RC                      
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
*                                                                               
MAIN050  TM    FLAG1,FLRETQ        TEST FOR RETURN                              
         BNO   MAIN990                                                          
         NI    FLAG1,255-FLRETQ    TURN THE FUCKING THING OFF                   
*                                                                               
         LA    RF,ACTTAB           RESUME WITH NEW ACTION                       
MAIN055  OC    8(2,RF),8(RF)                                                    
         BZ    *+14                                                             
         CLC   10(1,RF),ACTION                                                  
         BE    MAIN060                                                          
         LA    RF,L'ACTTAB(RF)     TRY NEXT ENTRY                               
         CLI   0(RF),X'FF'                                                      
         BNE   MAIN055                                                          
         DC    H'0'                                                             
MAIN060  MVC   ACTION,10(RF)       SAVE ACTION NUMBER                           
         MVC   OVERLY,8(RF)        SAVE OVERLAY NUMBER                          
         MVC   SCREEN,9(RF)        SAVE SCREEN NUMBER                           
         MVC   ACTFLG,11(RF)       SAVE ACT FLAGS                               
         MVI   SUBACT,0                                                         
         B     MAIN030                                                          
*                                                                               
MAIN990  EQU   *                                                                
*                                                                               
MAINX    MVI   SUBACT,0                                                         
         XC    HEXFLD,HEXFLD                                                    
         CLI   PROT,C'*'           * PROTECTED                                  
         BE    XITEQU                                                           
         XC    DEBACT,DEBACT                                                    
         OI    DEBACTH+6,X'80'     AND XMIT                                     
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        PROCESS ROOT LEVEL COMMANDS                        *                   
*************************************************************                   
COMM     NTR1                                                                   
         CLI   COMMAND,1           COMM 1 LOAD WORKER FILE                      
         BNE   COMM010                                                          
         BAS   RE,LOADER                                                        
         B     COMMX                                                            
*                                                                               
COMM010  CLI   COMMAND,2           COMM 2 EQUATE                                
         BNE   COMM020                                                          
         BAS   RE,SETEQU                                                        
         B     COMMX                                                            
*                                                                               
COMM020  CLI   COMMAND,3           COMM 3 LOAD MODULE                           
         BNE   COMM030                                                          
         BAS   RE,LOADMOD                                                       
         B     COMMX                                                            
*                                                                               
COMM030  CLI   COMMAND,4           COMM 4 SET ACCESS REG                        
         BNE   COMM040                                                          
         BAS   RE,SETACCS                                                       
         B     COMMX                                                            
*                                                                               
COMM040  CLI   COMMAND,5           COMM 5 SET USING                             
         BNE   COMM050                                                          
         BAS   RE,SETUSE                                                        
         MVI   ACTION,6            FORCE LIST ACTION                            
         OI    FLAG1,FLNEWQ                                                     
         B     COMMX                                                            
*                                                                               
COMM050  CLI   COMMAND,6           GENERAL TEST COMMAND                         
         BNE   COMM060                                                          
         BAS   RE,TESTIT                                                        
         B     COMMX                                                            
*                                                                               
COMM060  CLI   COMMAND,7           SET UP DEBUG ENVIRONMENT                     
         BE    COMM066                                                          
         CLI   COMMAND,12          STEP 1 INSTRUCTION                           
         BE    COMM065                                                          
         CLI   COMMAND,8           GO RUN STOPPED PROGRAM                       
         BNE   COMM070                                                          
*                                                                               
COMM065  OC    DECFLD,DECFLD                                                    
         BZ    *+16                                                             
         MVC   STEPS,DECFLD                                                     
         XC    DECFLD,DECFLD                                                    
*                                                                               
COMM066  BAS   RE,DEBUG                                                         
         B     COMMXX                                                           
*                                                                               
COMM070  CLI   COMMAND,9           SAVE THIS SITUATION                          
         BNE   COMM080                                                          
         BAS   RE,SAVSIT                                                        
         B     COMMX                                                            
*                                                                               
COMM080  CLI   COMMAND,10          RESTORE PREVIOUS SITUATION                   
         BNE   COMM090                                                          
         BAS   RE,RESSIT                                                        
         OI    FLAG1,FLNEWQ                                                     
         B     COMMX                                                            
*                                                                               
COMM090  CLI   COMMAND,11          KILL THE DEBUG TRANSACTION                   
         BNE   COMM100                                                          
         BAS   RE,DEBUG                                                         
         B     COMMX                                                            
*                                                                               
COMM100  CLI   COMMAND,13          WATCH SOMETHING                              
         BNE   COMM110                                                          
         BAS   RE,WATCH                                                         
         B     COMMX                                                            
*                                                                               
COMM110  CLI   COMMAND,14          PATCH SOMETHING                              
         BNE   COMM120                                                          
         BAS   RE,PATCH                                                         
         B     COMMX                                                            
*                                                                               
COMM120  CLI   COMMAND,15          RESET SESSION                                
         BNE   COMM130                                                          
         BAS   RE,RESET                                                         
         B     COMMX                                                            
*                                                                               
COMM130  CLI   COMMAND,16          SET SHARED MEMORY TABLE BASE ADDRESS         
         BNE   COMM140                                                          
         BAS   RE,SETMEM                                                        
         B     COMMX                                                            
*                                                                               
COMM140  EQU   *                                                                
*                                                                               
COMMX    MVI   COMMAND,0           RESET COMM                                   
COMMXX   B     XITEQU              RETURN TO MAIN                               
         EJECT                                                                  
*************************************************************                   
*        LOAD A WORKER FILE (LKED OUTPUT FROM FAC LINK)     *                   
*************************************************************                   
LOADER   NTR1                                                                   
         L     R3,ATIA             LABEL IT                                     
         MVC   0(8,R3),=C'**ADDR**'                                             
         LA    R3,8(R3)                                                         
*                                                                               
         BAS   RE,READWK           GET FIRST WRKF RECORD                        
         BE    *+14                                                             
         MVC   ERROR,=H'58'        ERROR NO FILES FOUND                         
         B     LOADX                                                            
*                                                                               
         SR    R5,R5               RESET R2 COUNTER                             
         B     LOAD030                                                          
*                                                                               
LOAD020  BAS   RE,READNXT          GET NEXT                                     
         BNE   LOADX                                                            
*                                                                               
LOAD030  L     R4,AIO              LOAD CSECTS AND LABELS                       
         CLC   43(5,R4),=C'CSECT'                                               
         BE    LOAD040                                                          
         CLC   43(5,R4),=C'START'                                               
         BE    LOAD040                                                          
         CLC   46(5,R4),=C'LABEL'                                               
         BNE   LOAD020                                                          
         MVC   0(8,R3),27(R4)      ADD LO CORE ADDRESS                          
         GOTO1 AHEXIN,DMCB,(0,14(R4)),FULL,8                                    
         B     LOAD041                                                          
*                                                                               
LOAD040  MVC   0(8,R3),24(R4)      ADD LO CORE ADDRESS                          
         GOTO1 AHEXIN,DMCB,(0,14(R4)),FULL,8                                    
LOAD041  L     R1,LOCORE                                                        
         A     R1,FULL                                                          
         ST    R1,8(R3)                                                         
         LA    R3,12(R3)           NEXT ENTRY                                   
         LA    R5,1(R5)                                                         
         L     R1,ATIA                                                          
         A     R1,=F'18400'        18K - MARGIN FOR LABELS                      
         CR    R1,R3                                                            
         BH    LOAD020             BACK FOR NEXT                                
*                                                                               
LOADX    MVC   0(8,R3),=C'**ENDA**'                                             
         MVI   TWAPAGE,1           SAVE TO TWA 1                                
         GOTO1 AWRITTWA                                                         
*                                                                               
         EDIT  (R5),(4,DUB2+1),ALIGN=LEFT                                       
         LA    R1,1                                                             
         AR    R1,R0                                                            
         STC   R1,DUB2                                                          
         LA    R1,DUB2(R1)                                                      
         MVI   0(R1),0                                                          
         LA    R1,DUB2                                                          
         ST    R1,SUBST                                                         
         MVC   INFO,=H'131'        &1 ADDCONS LOADED                            
*                                                                               
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        BUILD AN ADDRESS LIST FOR EASY TABLE LOOKUP        *                   
*************************************************************                   
BUILDER  NTR1                                                                   
         L     R3,ATIA             LABEL IT                                     
         MVC   0(8,R3),=C'**ADDR**'                                             
         LA    R3,8(R3)                                                         
*                                                                               
         BAS   RE,READWK           GET FIRST WRKF RECORD                        
         BE    *+14                                                             
         MVC   ERROR,=H'58'        ERROR NO FILES FOUND                         
         B     BUILDX                                                           
*                                                                               
         SR    R5,R5               RESET R2 COUNTER                             
         B     BUILD030                                                         
*                                                                               
BUILD020 BAS   RE,READNXT          GET NEXT                                     
         BNE   BUILDX                                                           
*                                                                               
BUILD030 L     R4,AIO              BUILD CSECTS AND LABELS                      
         CLC   43(5,R4),=C'CSECT'                                               
         BE    BUILD040                                                         
         CLC   43(5,R4),=C'START'                                               
         BE    BUILD040                                                         
         CLC   46(5,R4),=C'LABEL'                                               
         BNE   BUILD020                                                         
         MVC   0(8,R3),27(R4)      ADD LO CORE ADDRESS                          
         GOTO1 AHEXIN,DMCB,(0,14(R4)),FULL,8                                    
         B     BUILD041                                                         
*                                                                               
BUILD040 MVC   0(8,R3),24(R4)      ADD LO CORE ADDRESS                          
         GOTO1 AHEXIN,DMCB,(0,14(R4)),FULL,8                                    
BUILD041 L     R1,LOCORE                                                        
         A     R1,FULL                                                          
         ST    R1,8(R3)                                                         
         LA    R3,12(R3)           NEXT ENTRY                                   
         LA    R5,1(R5)                                                         
         L     R1,ATIA                                                          
         A     R1,=F'18400'        18K - MARGIN FOR LABELS                      
         CR    R1,R3                                                            
         BH    BUILD020            BACK FOR NEXT                                
*                                                                               
BUILDX   MVC   0(8,R3),=C'**ENDA**'            1(MAIN)                          
         MVI   TWAPAGE,3           SAVE TO TWA 3(DMGR) 4(TABS)                  
         GOTO1 AWRITTWA                                                         
*                                                                               
         EDIT  (R5),(4,DUB2+1),ALIGN=LEFT                                       
         LA    R1,1                                                             
         AR    R1,R0                                                            
         STC   R1,DUB2                                                          
         LA    R1,DUB2(R1)                                                      
         MVI   0(R1),0                                                          
         LA    R1,DUB2                                                          
         ST    R1,SUBST                                                         
         MVC   INFO,=H'131'        &1 ADDCONS LOADED                            
*                                                                               
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        SET UP USER EQUATE                                 *                   
*************************************************************                   
SETEQU   ST    RE,SAVERE                                                        
         LA    R1,USERADS                                                       
SETQ005  CLC   0(8,R1),INFIELD     FIND ENTRY - OR                              
         BE    SETQ010                                                          
         CLI   0(R1),0             FIND FIRST FREE ENTRY                        
         BE    SETQ010                                                          
         LA    R1,12(R1)                                                        
         LA    RF,USERADX                                                       
         CR    RF,R1                                                            
         BH    SETQ005             ERROR TABLE FULL                             
         MVC   ERROR,=H'52'                                                     
         B     *+16                                                             
*                                                                               
SETQ010  MVC   0(8,R1),INFIELD     SET UP EQUATE                                
         MVC   8(4,R1),ADDRESS                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        LOAD A WORKER FILE MODULE                          *                   
*************************************************************                   
         SPACE 1                                                                
LOADMOD  NTR1                                                                   
         MVC   HEXWORK,SAVESIT     SAVE CURRENT SITUATION                       
*                                                                               
         L     R3,ATIA             LABEL IT                                     
         MVC   0(8,R3),=C'**MODS**'                                             
         LA    R3,8(R3)                                                         
         USING MODULED,R3                                                       
*                                                                               
         BAS   RE,READWK           GET FIRST WRKF RECORD                        
         BE    *+14                                                             
         MVC   ERROR,=H'58'        ERROR NO FILES FOUND                         
         B     LOMOX                                                            
         SR    R5,R5                                                            
         B     LOMO030                                                          
LOMO020  BAS   RE,READNXT          GET NEXT                                     
         BNE   LOMOX                                                            
*                                                                               
LOMO030  L     R4,AIO              LOAD CSECTS AND DSECTS                       
*                                                                               
         CLC   5(6,R4),=C'000000'  IGNORE NON ZERO CSECTS                       
         BNE   LOMO020                                                          
*                                                                               
         CLC   54(5,R4),=C'CSECT'                                               
         BE    LOMO040                                                          
         CLC   54(5,R4),=C'START'                                               
         BE    LOMO040                                                          
         CLC   54(5,R4),=C'DSECT'                                               
         BNE   LOMO020                                                          
*                                                                               
LOMO040  XC    MODULE,MODULE       FILL IN DETAIL                               
         MVC   MODNAME,45(R4)                                                   
         MVC   MODWRKF,WRKFNO                                                   
         MVC   MODREC,WRKFREC                                                   
         MVC   MODTYPE,54(R4)                                                   
*                                                                               
         GOTO1 AHEXIN,DMCB,(0,5(R4)),MODOFFS,6                                  
*                                                                               
         LA    R3,L'MODULE(R3)     NEXT ENTRY                                   
         LA    R5,1(R5)                                                         
         L     R1,ATIA                                                          
         A     R1,=F'18400'        18K - MARGIN FOR LABELS                      
         CR    R1,R3                                                            
         BH    LOMO020             BACK FOR NEXT                                
*                                                                               
LOMOX    MVC   SAVESIT,HEXWORK     RESTORE CURRENT SITUATION                    
         MVC   0(8,R3),=C'**ENDM**'                                             
         MVI   TWAPAGE,2           SAVE TO TWA 2                                
         GOTO1 AWRITTWA                                                         
*                                                                               
         EDIT  (R5),(4,DUB2+1),ALIGN=LEFT                                       
         LA    R1,1                                                             
         AR    R1,R0                                                            
         STC   R1,DUB2                                                          
         LA    R1,DUB2(R1)                                                      
         MVI   0(R1),0                                                          
         LA    R1,DUB2                                                          
         ST    R1,SUBST                                                         
         MVC   INFO,=H'132'        &1 MODULES LOADED                            
*                                                                               
         B     XITEQU                                                           
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
*************************************************************                   
*        SET SHARED MEMORY TABLE BASE ADDRESS               *                   
*************************************************************                   
SETMEM   NTR1                                                                   
         CLC   =C'PRTQ',INFIELD                                                 
         BNE   *+14                                                             
         MVC   SHMTADDR,PRTQADDR                                                
         B     SETMEM2                                                          
         CLC   =C'WRKF',INFIELD                                                 
         BNE   *+14                                                             
         MVC   SHMTADDR,WRKFADDR                                                
         B     SETMEM2                                                          
         CLC   =C'WRKZ',INFIELD                                                 
         BNE   *+14                                                             
         MVC   SHMTADDR,WRKZADDR                                                
         B     SETMEM2                                                          
*                                                                               
         OC    HEXFLD,HEXFLD                                                    
         BZ    SETMEMX                                                          
         MVC   SHMTADDR,HEXFLD                                                  
         B     SETMEMX                                                          
*                                                                               
SETMEM2  OC    SHMTADDR,SHMTADDR                                                
         BZ    SETMEMX                                                          
         MVC   ADDRESS,SHMTADDR                                                 
SETMEMX  B     XITEQU                                                           
         EJECT                                                                  
                                                                                
*************************************************************                   
*        SET ACCESS TO DATASPACE AND SET STORAGE MAP        *                   
*************************************************************                   
         SPACE 1                                                                
SETACCS  NTR1                                                                   
         CLC   INFIELD(2),=C'OFF'                                               
         BE    SETACOFF                                                         
         BNE   SETAR2                                                           
*                                                                               
SETACOFF MVI   ADDFLAG,0                                                        
         XC    DSPALET,DSPALET                                                  
         B     SETACMAP                                                         
*                                                                               
SETAR2   SAC   512                                                              
         SYSSTATE ASCENV=AR                                                     
         SAM31                                                                  
         L     R1,ASYSFACS         EXTRACT SSB VALUES                           
         L     R1,VSSB-SYSFACD(R1)                                              
         USING SSBD,R1                                                          
*                                                                               
         CLC   INFIELD(2),=C'TABS'                                              
         BNE   SETAR4                                                           
         LAM   AR2,AR2,SSBTBLET                                                 
         MVI   ADDFLAG,C'T'                                                     
         MVC   DSPALET,SSBTBLET                                                 
         XR    R2,R2                                                            
         B     SETAR6                                                           
*                                                                               
SETAR4   CLC   INFIELD(2),=C'DMGR'                                              
         BNE   SETAR5                                                           
         LAM   AR2,AR2,SSBALET                                                  
         MVI   ADDFLAG,C'D'                                                     
         MVC   DSPALET,SSBALET                                                  
         SR    R2,R2                                                            
*                                                                               
SETAR5   CLC   INFIELD(2),=C'PGMS'                                              
         BNE   SETAR6                                                           
         LAM   AR2,AR2,SSBPGMTA                                                 
         MVI   ADDFLAG,C'P'                                                     
         MVC   DSPALET,SSBPGMTA                                                 
         SR    R2,R2                                                            
*                                                                               
SETAR6   CLC   INFIELD(2),=C'MED'                                               
         BNE   SETAR7                                                           
         LAM   AR2,AR2,SSBMEDTA                                                 
         MVI   ADDFLAG,C'M'                                                     
         MVC   DSPALET,SSBMEDTA                                                 
         SR    R2,R2                                                            
*                                                                               
SETAR7   ST    R2,ADDRESS                                                       
         DROP  R1                                                               
         SAC   0                                                                
         SAM24                                                                  
         SYSSTATE ASCENV=P                                                      
*                                                                               
SETACMAP L     R3,=A(ADDMAP-SAVED) BUILD MAP FROM SSB                           
         AR    R3,R7                                                            
         ST    R3,FULL                                                          
*                                                                               
         CLI   ADDFLAG,C'D'        DMGR DATASPACE                               
         BE    SETAH4                                                           
         CLI   ADDFLAG,C'T'        TABS DATASPACE                               
         BE    SETAH4                                                           
         CLI   ADDFLAG,C'P'        PGMS DATASPACE                               
         BE    SETAH4                                                           
         CLI   ADDFLAG,C'M'        MED DATASPACE                                
         BE    SETAH4                                                           
*                                                                               
SETAH1   L     R1,ASYSFACS         SET FACPAK STORAGE BOUNDS                    
         L     R1,VSSB-SYSFACD(R1)                                              
         USING SSBD,R1                                                          
*                                  ADDMAP HAS ROOM FOR 256D ADDRESSES           
         MVC   0(4,R3),SSBLOADR                                                 
         MVC   4(4,R3),SSBHIADR                                                 
         MVC   8(4,R3),SSBXALO                                                  
         MVC   12(4,R3),SSBXAHI                                                 
         LA    R3,16(,R3)                                                       
*                                  NOW ADD BOUNDS FOR SHM SEGMENTS              
         LT    RE,SSBSHMTB         START OF SHARED MEMORY TABLES                
         BZ    SETAH1X                                                          
         DROP  R1                                                               
*                                                                               
         USING SHMTABD,RE                                                       
         USING SHMKEYD,RF                                                       
SETAH1A  DS    0H                                                               
         L     RF,SHMTKEY                                                       
         TM    SHMTFLAG,SHMTSE#    BY SE# EACH MEMORY SEGMENT?                  
         BO    SETAH1B                                                          
         LT    R0,SHMKADDR         HAVE WE ATTACHED THIS SHM SEGMENT?           
         BZ    SETAH1N             NO - CHECK NEXT                              
*                                                                               
         CLC   SHMTNAME,=C'PRTQ    '                                            
         JNE   *+10                                                             
         MVC   PRTQADDR,SHMKSTAR   SAVE ADDRESS FOR LATER                       
         CLC   SHMTNAME,=C'WRKF    '                                            
         JNE   *+10                                                             
         MVC   WRKFADDR,SHMKSTAR   SAVE ADDRESS FOR LATER                       
         CLC   SHMTNAME,=C'WRKZ    '                                            
         JNE   *+10                                                             
         MVC   WRKZADDR,SHMKSTAR   SAVE ADDRESS FOR LATER                       
*                                                                               
         ST    R0,0(,R3)           A(MIN)                                       
         A     R0,SHMKSIZE         + SIZE                                       
         ST    R0,4(,R3)           A(MAX)                                       
         LA    R3,8(,R3)                                                        
         J     SETAH1N                                                          
*                                                                               
* Check all keys attached by SE#                                                
*                                                                               
SETAH1B  LA    R1,254                                                           
         AHI   RF,SHMKEYQ          Start at key for SE=1, No SE=255             
SETAH1BA LT    R0,SHMKADDR         HAVE WE ATTACHED THIS SHM SEGMENT?           
         BZ    SETAH1BN            NO - CHECK NEXT                              
*                                                                               
         ST    R0,0(,R3)           A(MIN)                                       
         A     R0,SHMKSIZE         + SIZE                                       
         ST    R0,4(,R3)           A(MAX)                                       
         LA    R3,8(,R3)                                                        
*                                                                               
SETAH1BN AHI   RF,SHMKEYQ                                                       
         JCT   R1,SETAH1BA                                                      
*                                                                               
SETAH1N  LA    RE,SHMTABLQ(,RE)    NEXT TABLE ENTRY                             
         CLI   0(RE),X'FF'         END OF TABLE                                 
         BNE   SETAH1A                                                          
         DROP  RE,RF                                                            
*                                                                               
SETAH1X  XC    0(8,R3),0(R3)       MARK END OF ADDMAP                           
         LR    RF,R3                                                            
         S     RF,FULL                                                          
         SRL   RF,3                                                             
         L     R3,FULL                                                          
*                                                                               
         GOTO1 =V(XSORT),DMCB,(R3),(RF),8,8,0,0,RR=RELO                         
*                                                                               
         B     SETACX                                                           
*                                                                               
SETAH2   MVC   0(4,R3),=X'00000000'   DMGR STORAGE BONDS                        
         MVC   4(4,R3),=X'00450000'                                             
         XC    8(8,R3),8(R3)                                                    
         B     SETACX                                                           
*                                                                               
SETAH3   MVC   0(4,R3),=X'00000000'   TABS STORAGE BOUNDS                       
         MVC   4(4,R3),=X'00B00000'                                             
         XC    8(8,R3),8(R3)                                                    
         B     SETACX                                                           
*                                                                               
SETAH4   MVC   0(4,R3),=X'00000000'   DSPACE STORAGE BOUNDS                     
         SR    R2,R2                                                            
         SAC   512                                                              
         MVC   4(4,R3),60(R2)         UPPER BOUND IS AT+60                      
         SAC   0                                                                
         XC    8(8,R3),8(R3)                                                    
         B     SETACX                                                           
*&&NOP                                                                          
*****************************************************************               
* THE FOLLOWING NOOPED CODE ATTEMPTS TO USE TPROT IN SUPERVISOR *               
* STATE TO TEST THE PROTECTION OF THE ENTIRE CONTENTS OF CORE   *               
* OR DATASPACE AND BUILD A SOFT STORAGE MAP.... HOWEVER....     *               
* T'F'PROT APPEARS ONLY TO WORK ON AN OCCASIONAL BASIS AND      *               
* PRESUMABLY HAS MYSTERIOUS CONNECTIONS TO MVS AND ITS WORKINGS.*               
*****************************************************************               
         SPACE 1                                                                
SETACMAP L     R1,ASYSFACS         DISABLE TIMERS                               
         L     RF,VTICTOC-SYSFACD(R1)                                           
         GOTO1 (RF),DMCB,C'SSET'                                                
*                                  MODESET SUP FOR TPROT                        
         MODESET KEY=NZERO,MODE=SUP                                             
                                                                                
         SAM31                                                                  
         CLI   ADDFLAG,0           A/R MODE IF REQD                             
         BE    SETACM1                                                          
         LAM   AR2,AR2,DSPALET                                                  
         SAC   512                                                              
*                                                                               
         MVC   SAVSITS(16),=C'SAVESITSSAVESITS'                                 
*                                                                               
SETACM1  LA    R0,126              MAX 126+2 ADDRESS BREAKS                     
         L     R3,=A(ADDMAP-SAVED)                                              
         AR    R3,R7                                                            
         SR    R2,R2                                                            
         MVI   BYTE,0                                                           
SETACM2  LA    R1,0(R2)                                                         
         TPROT 0(R2),128           TEST PROTECTION                              
         BE    SETACM3                                                          
         BNO   SETACM4                                                          
         CLI   BYTE,C'Y'                                                        
         BNE   SETACM4                                                          
*                                                                               
SETACM3  CLI   BYTE,C'Y'           TEST STILL OK                                
         BE    SETACM9                                                          
         STCM  R2,15,0(R3)         SAVE ADDRESS                                 
         LA    R3,4(R3)                                                         
         BCT   R0,*+8              CHECK MAX                                    
         B     SETACMX                                                          
         MVI   BYTE,C'Y'           FLAG STORAGE OK                              
         B     SETACM9                                                          
*                                                                               
SETACM4  CLI   BYTE,0              DO NOT START WITH PROTECTED AREA             
         BE    SETACM4A                                                         
         CLI   BYTE,C'N'           TEST STILL PROTECTED                         
         BE    SETACM9                                                          
         STCM  R2,15,0(R3)         SAVE ADDRESS                                 
         LA    R3,4(R3)                                                         
         BCT   R0,*+8              CHECK MAX                                    
         B     SETACMX                                                          
SETACM4A MVI   BYTE,C'N'           FLAG PROTECTED                               
*                                                                               
SETACM9  CL    R2,=X'00025000'     BELOW 25000 BUMP 1 BYTE                      
         BNL   *+12                                                             
         LA    R2,1(,R2)                                                        
         B     *+8                                                              
         AH    R2,=H'1024'         ELSE BUMP 1K                                 
*                                                                               
         CL    R2,=X'80000000'     UNTIL 80000000                               
         BL    SETACM2                                                          
*                                                                               
SETACMX  XC    0(8,R3),0(R3)       ZERO END ADDRESSES                           
         SAC   0                                                                
         LA    RE,SETACMX1                                                      
         SLL   RE,1                                                             
         SRL   RE,1                                                             
         BSM   0,RE                                                             
SETACMX1 MODESET KEY=NZERO,MODE=PROB                                            
*                                                                               
*&&                                                                             
*                                                                               
SETACX   B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        SET UP USING                                       *                   
*************************************************************                   
         SPACE 1                                                                
SETUSE   ST    RE,SAVERE                                                        
         LA    R1,USINGS                                                        
SETU005  CLI   0(R1),0             FIND FIRST FREE ENTRY                        
         BE    SETU010                                                          
         LA    R1,16(R1)                                                        
         LR    RF,R1               USINGX                                       
         AH    RF,=Y(USINGX-USINGS)                                             
         CR    RF,R1                                                            
         BH    SETU005             ERROR TABLE FULL                             
         MVC   ERROR,=H'52'                                                     
         B     SETUXXX                                                          
*                                                                               
SETU010  CLI   INFCHR,C' '         IF DSECT ONLY                                
         BE    SETU020             SET TO ADDRESS                               
*                                                                               
         MVC   0(8,R1),INF01       SET UP USING                                 
         MVC   8(8,R1),INF02       WITH 2ND FIELD                               
         B     SETUXXX                                                          
*                                                                               
SETU020  MVC   0(8,R1),INF01       SET UP USING                                 
         XC    8(8,R1),8(R1)                                                    
         MVC   12(4,R1),ADDRESS    WITH ADDRESS                                 
*                                                                               
SETUXXX  L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        TEST SOMETHING                                     *                   
*************************************************************                   
         SPACE 1                                                                
TESTIT   ST    RE,SAVERE                                                        
*                                                                               
*        GOTO1 AGETFACT,DMCB,(X'80',FULL),F#BTWA                                
*        LA    R1,FULL                                                          
*        DC    H'0'                                                             
*                                                                               
*        MODESET EXTKEY=ZERO                                                    
         L     R1,ASYSFACS                                                      
         L     RF,VTICTOC-SYSFACD(R1)                                           
         GOTO1 (RF),DMCB,C'SSET'                                                
*                                                                               
         MODESET KEY=NZERO,MODE=SUP                                             
         SAM31                                                                  
*                                                                               
TESTIT0  L     R3,AWKBUFF                                                       
         L     R0,=F'3000'                                                      
         SR    R1,R1                                                            
TESTIT1  MVI   DUB,0                                                            
         TPROT 0(R1),128           TEST PROTECTION                              
         BE    *+8                                                              
         MVI   DUB,1                                                            
         CLC   DUB(1),DUB+1        TEST WITH PREV                               
         BE    TESTIT2                                                          
         STCM  R1,15,0(R3)         CHANGED SO SAVE ADDRESS                      
         LA    R3,4(R3)                                                         
         BCT   R0,TESTIT2          UNTIL 14K DONE                               
         L     R2,AWKBUFF                                                       
         B     TESTIT3                                                          
*                                                                               
TESTIT2  MVC   DUB+1(1),DUB        SAVE OLD CC                                  
         CL    R1,=X'00025000'                                                  
         BNL   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *+8                                                              
         AH    R1,=H'1024'                                                      
*                                                                               
         LTR   R1,R1                                                            
         BNZ   TESTIT1                                                          
*                                                                               
TESTIT3  MODESET KEY=NZERO,MODE=PROB                                            
         L     R3,AWKBUFF                                                       
         DC    H'0'                                                             
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        SET UP A WATCH                                     *                   
*************************************************************                   
         SPACE 1                                                                
WATCH    NTR1                                                                   
         LA    RF,WATCHTAB                                                      
*                                                                               
WATCH010 CLC   INFIELD(2),0(RF)    SCAN WATCHTAB                                
         BE    WATCH015                                                         
         LA    RF,9(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   WATCH010                                                         
         B     WATCH020                                                         
*                                                                               
WATCH015 XC    WATCHFLG,8(RF)      TOGGLE FLAG                                  
         B     WATCHX                                                           
*                                                                               
WATCH020 LA    R0,32                                                            
         LA    RF,WATCHES                                                       
WATCH021 OC    0(16,RF),0(RF)                                                   
         BZ    WATCH022                                                         
         LA    RF,16(RF)                                                        
         BCT   R0,WATCH021                                                      
         B     WATCHX                                                           
*                                                                               
WATCH022 MVC   0(16,RF),WATTEXT    COPY WHOLE ENTRY FROM WORK                   
*                                                                               
WATCHX   GOTO1 AWRITTWA                                                         
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        SET UP A PATCH FOR PATCHING                        *                   
*************************************************************                   
         SPACE 1                                                                
PATCH    NTR1                                                                   
         MVI   COMMAND,0                                                        
         CLI   INFLEN,6            CSPPNN                                       
         BE    PATCH015                                                         
         CLI   INFLEN,7            CSPPNNTT TO 0SPPOOCCTT                       
         BNE   PATCH2                                                           
*                                                                               
PATCH015 MVC   BYTE,INFIELD        SAVE LANG CHR                                
         MVI   INFIELD,C'0'        REPLACE WITH ZERO                            
*                                                                               
         XC    DUB,DUB                                                          
         GOTO1 AHEXIN,DMCB,INFIELD,DUB,6                                        
         MVC   DUB+3(1),BYTE       SET CTRY CODE                                
*                                                                               
         CLI   INFLEN,6            SET LEVEL IF THERE                           
         BE    PATCH020                                                         
         MVC   DUB+4(1),INFIELD+6                                               
*                                                                               
         USING LIBUFFD,LIBUFF      DATASPACE ARREDIT BLOCK                      
PATCH020 L     R1,ASYSFACS                                                      
         L     R1,VSSB-SYSFACD(R1)                                              
         USING SSBD,R1                                                          
         MVC   FULL,SSBPGMTA                                                    
*                                                                               
         GOTO1 ALOCKSPC,DMCB,X'20004001',WORK                                   
*                                                                               
         LAM   AR2,AR2,FULL        COPY BUFFER DETAIL FOR ARREDIT               
         SAC   512                                                              
         SAM31                                                                  
         LA    R1,WORK                                                          
         L     R2,DSPTFRST-DSPHDR(R1)                                           
         N     R2,=X'3FFFFFFF'                                                  
         MVC   LIBUFF,0(R2)                                                     
         SAC   0                                                                
         SAM24                                                                  
*                                                                               
         MVC   LIALET,FULL                                                      
         LA    R1,WORK                                                          
         MVC   WORK(5),DUB                                                      
         ST    R1,LIAREC                                                        
         MVI   LIACTN,LIAHIGH                                                   
         MVC   LIKEYL,=AL2(PSKEYL)                                              
         OI    LIFLAG1,LIF1ARS                                                  
         GOTO1 AARREDIT,DMCB,LIBUFF                                             
         L     R1,LIAREC                                                        
         USING PROGSPCD,R1                                                      
*                                                                               
         CLC   DUB(5),0(R1)        DID WE GET A MATCH                           
         BE    *+14                                                             
         MVC   MYERROR,=H'2'       PHASE NOT FOUND                              
         B     PATCHX                                                           
*                                                                               
         MVC   PHASENM,0(R1)       SAVE NAME AND PARMS                          
*                                                                               
         ICM   R2,15,PSADR                                                      
         STCM  R2,15,PHASEAD       SET PHASE ADDRESS                            
         ICM   R2,15,PSLEN                                                      
         STCM  R2,15,BASELEN       SET PHASE LENGTH                             
*                                                                               
         MVC   WORK(5),=X'0A0F03E300'                                           
         ST    R1,LIAREC           LOCATE TA0F03 CTDEB03 PHASE                  
         GOTO1 AARREDIT,DMCB,LIBUFF                                             
         L     R1,LIAREC                                                        
*                                                                               
         ICM   R2,15,PSADR                                                      
         STCM  R2,15,FULL          SAVE PHASE ADDRESS                           
         DROP  R1                                                               
*                                                                               
         MVC   INFIELD(5),=C'PGMS '                                             
         BAS   RE,SETACCS                                                       
         MVC   ADDRESS,FULL        SET IN ADDRESS NOW                           
*                                                                               
PATC040  LAM   AR2,AR2,DSPALET                                                  
         LAM   ARE,ARE,DSPALET                                                  
         SAC   512                                                              
         SAM31                                                                  
         L     R2,ADDRESS          SAVE A(PATCH AREA)                           
         A     R2,0(,R2)                                                        
         ST    R2,APATCHAR                                                      
*                                                                               
         L     R2,ADDRESS                                                       
         L     RE,ADDRESS                                                       
         A     RE,0(,R2)                                                        
*                                                                               
         OC    0(4,RE),0(RE)                                                    
         BZ    PATC045                                                          
         MVC   MYERROR,=H'1'       PATCH AREA ALREADY INUSE                     
*                                                                               
PATC045  MVC   0(4,RE),FACN4                                                    
         A     R2,4(,R2)                                                        
         SHI   R2,16                                                            
         CLC   0(16,R2),=C'***PATCH WORK***'                                    
         BE    PATC050                                                          
         DC    H'0'                CAN'T FIND PATCH WORK AREA                   
*                                                                               
PATC050  AHI   R2,16               BUMP PAST EYECATCHER                         
         ST    R2,BASEADDR         MAKE THIS THE BASE ADDRESS                   
         ST    R2,ADDRESS                                                       
         L     RE,PHASEAD                                                       
         L     RF,BASELEN                                                       
         L     R3,=A(MAXPATCH*K)   COPY UP TO n*K AND PAD                       
*                                                                               
         OC    MYERROR,MYERROR     DON'T COPY PHASE IF ERROR                    
         BZ    PATC054                                                          
         MVI   ACTION,4            JUST DISPLAY WHAT'S THERE                    
         B     PATC055                                                          
*                                                                               
PATC054  MVCL  R2,RE               COPY PHASE TO WORK AREA                      
         LAM   AR2,AR2,ZEROS                                                    
         LAM   ARE,ARE,ZEROS                                                    
         MVI   ACTION,3                                                         
*                                                                               
PATC055  SAC   0                                                                
         SAM24                                                                  
         OI    FLAG1,FLRETQ                                                     
         MVC   MYINFO,=H'1'                                                     
*                                                                               
PATCHX   GOTO1 AWRITTWA                                                         
         B     XITEQU                                                           
         EJECT                                                                  
************************************************************                    
* PATCH 2 - ANALIZE PATCHED STORAGE AND BUILD PATCH AREA   *                    
************************************************************                    
         SPACE 1                                                                
PATCH2   OC    PHASENM,PHASENM                                                  
         BZ    PATCH2X                                                          
*                                                                               
         CLI   INSCREEN,X'FE'                                                   
         BE    PATCH3                                                           
*                                                                               
         L     R1,ASYSFACS                                                      
         L     R1,VSSB-SYSFACD(R1)                                              
         USING SSBD,R1                                                          
         MVC   DSPALET,SSBPGMTA                                                 
         DROP  R1                                                               
*                                                                               
         LAM   AR2,AR2,DSPALET                                                  
         LAM   AR4,AR4,DSPALET                                                  
         LAM   AR6,AR6,DSPALET                                                  
*                                                                               
         L     R2,BASEADDR         A(PATCH WORK)                                
         L     R4,PHASEAD          A(PHASE)                                     
         L     R6,APATCHAR                                                      
*                                                                               
         SAC   512                                                              
         SAM31                                                                  
         USING PATCHWKD,R6                                                      
         ST    R4,PATCHPHS                                                      
         ST    R2,PATCHWRK                                                      
         MVC   PATCHLEN,BASELEN                                                 
*                                                                               
PATCH210 CLC   0(5,R4),=C'BOOK='   MUST FIND A BOOK WITHIN PHASE                
         BE    PATCH220                                                         
         CLC   0(6,R4),=C'LEVEL='  OR LEVEL                                     
         BE    PATCH220                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,PATCH210                                                      
         DC    H'0'                                                             
*                                                                               
PATCH220 S     R4,PATCHPHS                                                      
         ST    R4,PATCHOFS         AND WHERE IT CAN BE FOUND                    
*                                                                               
         SR    R1,R1                                                            
         L     R4,PHASEAD          A(PHASE)                                     
         L     R5,PATCHLEN         COMPARE PHASE WITH PATCHED VERSION           
         L     R3,PATCHLEN                                                      
PATCH230 CLCL  R2,R4               CLCL UNTIL MISSMATCH                         
         LTR   R3,R3                                                            
         BZ    PATCH250                                                         
*                                                                               
         LR    R0,R2               CALCULATE AND SAVE OFFSET                    
         S     R0,BASEADDR                                                      
         STH   R0,PATCHDET(R1)                                                  
         LR    R0,R2               CALCULATE AND SAVE OFFSET                    
*                                                                               
PATCH240 AHI   R2,1                BUMP AND COMPARE                             
         AHI   R4,1                                                             
         BCTR  R3,0                                                             
         BCTR  R5,0                                                             
         LTR   R3,R3               LOOP TRAP                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(R4)                                                    
         BNE   PATCH240                                                         
*                                                                               
         SR    R2,R0               CALCULATE AND SAVE LEN                       
         STH   R2,PATCHDET+2(R1)                                                
         AR    R2,R0                                                            
*                                                                               
         LA    R1,4(R1)            INDEX TO NEXT PATCH                          
         C     R1,=A(128*4)                                                     
         BL    PATCH230                                                         
         DC    H'0'                TOO MANY PATCHES                             
*                                                                               
PATCH250 SR    R0,R0               END OF PATCHES                               
         ST    R0,PATCHDET(R1)                                                  
*                                                                               
         LA    R2,PATCHSYS                                                      
         MVC   0(4,R2),=C'PROG'                                                 
         MVC   4(4,R2),PHASEAD                                                  
*                                                                               
         LAM   AR2,AR6,ZEROS                                                    
         SAM24                                                                  
         SAC   0                                                                
*                                                                               
         GOTO1 ASETCOM,22          SEND PATCH COMMAND TO ADVS                   
*                                                                               
         MVI   ACTION,13                                                        
         OI    FLAG1,FLRETQ                                                     
*                                                                               
PATCH2X  GOTO1 AWRITTWA                                                         
         B     XITEQU                                                           
*                                                                               
PATCH3   GOTO1 ASETCOM,23          SEND PATCHIT COMMAND TO ADVS                 
*                                                                               
         MVI   ACTION,13                                                        
         OI    FLAG1,FLRETQ                                                     
*                                                                               
         GOTO1 AWRITTWA                                                         
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        RESET ALL OF DEBUGS STORAGE                        *                   
*************************************************************                   
         SPACE 1                                                                
RESET    NTR1                                                                   
         MVI   COMMAND,0                                                        
*                                                                               
         MVI   INITFLG,C'N'        SET INIT FLAG TO NEW                         
*                                                                               
         L     RD,SAVERD                                                        
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,15,APATCHAR      ARE WE PATCHING ANYTHING                     
         BZ    RESET090                                                         
*                                                                               
         L     R1,ASYSFACS                                                      
         L     R1,VSSB-SYSFACD(R1)                                              
         LAM   AR2,AR2,SSBPGMTA-SSBD(R1)                                        
         SAC   512                                                              
         SAM31                                                                  
*                                                                               
         SHI   R2,16               CHECK WE HAVE PATCH PHASE                    
         CLC   0(16,R2),=C'***PATCH AREA***'                                    
         BNE   RESET019                                                         
         SHI   R2,16                                                            
         ST    R2,FULL             SAVE BASE ADDR OF PATCH PHASE                
         ICM   R1,15,0(R2)                                                      
         BZ    RESET019                                                         
         AR    R2,R1                                                            
         LH    R3,=H'4064'         PATH AREA IS 4096-32                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               ZAP TO ZERO                                  
*                                                                               
         L     R2,FULL             SAVE BASE ADDR OF PATCH PHASE                
         ICM   R1,15,4(R2)                                                      
         BZ    RESET019                                                         
         AR    R2,R1                                                            
         SHI   R2,16               CHECK WE HAVE PATCH PHASE                    
         CLC   0(16,R2),=C'***PATCH WORK***'                                    
         BNE   RESET019                                                         
         AHI   R2,16                                                            
         L     R3,=A(MAXPATCH*K)   PATCH WORK IS n*K                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE               ZAP TO ZERO                                  
*                                                                               
RESET019 SAM24                                                                  
         SAC   0                                                                
*                                                                               
RESET090 MVC   SAVSITS,SVPARMS     MAKE SURE WE KEEP PARMS                      
         MVC   ADDRESS,SAVERD      SAVERD                                       
         MVC   NEXTADD,RELO        AND RELO                                     
*                                                                               
         LR    R0,RC               ZAP WORKING STORAGE                          
         L     R1,=A(WORKX-WORKD)                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVPARMS,SAVSITS     RESTORE PARMS                                
         MVC   SAVERD,ADDRESS      SAVERD                                       
         MVC   RELO,NEXTADD        RELO                                         
*                                                                               
         MVI   DEBACTH+5,0         REMOVE RESET INPUT                           
         B     INIT000             START ALL OVER                               
         EJECT                                                                  
*************************************************************                   
*        SET UP DEBUG ENVIRONMENT                           *                   
*************************************************************                   
         SPACE 1                                                                
DEBUG    NTR1                                                                   
         ICM   R2,15,AMYEXIT       ARE WE HOOKEND INTO A SLOT ALREADY           
         BNZ   DEBUG010                                                         
         CLI   COMMAND,7           IS THIS DEBUG COMMAND                        
         BNE   DEBUG010                                                         
*                                                                               
         GOTO1 AGETFACT,DMCB,(X'80',FULL),F#SYSADS                              
         L     R1,FULL                                                          
         MVC   FULL,AEXITDB-SYSADSD(R1)                                         
*                                                                               
         XR    R1,R1               ADDRESS MY SLOT                              
         IC    R1,DECFLD+3                                                      
         SLL   R1,10                                                            
         A     R1,FULL                                                          
         ST    R1,AMYEXIT          SAVE ADDR OF MY EXIT                         
         LR    R2,R1                                                            
         USING EXAREAD,R2                                                       
*                                                                               
DEBUG010 L     R1,ASYSFACS         BXLE TCB LOOKING FOR DEBUG UTL               
         L     R1,VTCB-SYSFACD(R1)                                              
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
DEBUG020 CLI   TCBSEN-TCBD(R1),0   IGNORE NON ACTIVE                            
         BE    DEBUG021                                                         
         CLC   TCBSYM-TCBD(8,R1),EXLUID                                         
         BNE   DEBUG021                                                         
         MVC   DBUTL,TCBRUTL-TCBD(R1)                                           
         MVC   MYINFO,=H'3'                                                     
         B     DEBUG030                                                         
*                                                                               
DEBUG021 BXLE  R1,RE,DEBUG020                                                   
         EJECT                                                                  
************************************************************                    
*        TASK MUST HAVE COMPLETED OR DIED                  *                    
************************************************************                    
         SPACE 1                                                                
DEBUG025 ICM   R1,15,DBUTL         NOT ACTIVE SO TOGGLE FLAG                    
         BZ    DEBUGE                                                           
         XC    STEPS,STEPS         TURN OFF STEPPING                            
         MVC   INFO,=H'133'        ENTER TRANSACTION                            
         BO    DEBUGX                                                           
         MVC   INFO,=H'130'        DEBUG OFF                                    
         MVI   COMMAND,0           TURN OFF GO COMMAND                          
         MVI   ACTION,0                                                         
         MVI   SUBACT,0                                                         
         B     XMOD1                                                            
         EJECT                                                                  
************************************************************                    
*        DEBUG TASK FOUND                                  *                    
************************************************************                    
         SPACE 1                                                                
         USING TCBD,R6                                                          
DEBUG030 LR    R6,R1                                                            
         ST    R6,DBTCB                                                         
         CLI   COMMAND,11          TEST KILL COMMAND                            
         BNE   DEBUG031                                                         
*                                                                               
         NI    TCBFLAG3-TCBD(R1),255-TCBDEBUG                                   
         OI    TCBFLAG3-TCBD(R1),TCBKILL                                        
         MVI   COMMAND,8           SET TO GO                                    
*                                                                               
DEBUG031 CLI   COMMAND,8           IS THIS A GO COMMAND                         
         BNE   *+12                SET EXWATCH1 TO 0(RB),EQU,000000             
*NOP     MVC   EXWATCH1(8),=X'0B000000D9800000'                                 
         MVI   EXACTN,X'80'                                                     
         B     DEBUG032                                                         
*                                                                               
         CLI   COMMAND,12          IS THIS A STEP COMMAND                       
         BNE   DEBUG040            SET EXWATCH1 TO 0(RB),NEQ,000000             
*NOP     MVC   EXWATCH1(8),=X'0B000000D9700000'                                 
         MVI   EXACTN,X'20'                                                     
*                                                                               
DEBUG032 LA    R1,STARTECB         SET TASK FOR RESTART                         
         ST    R1,TCBSVECB-TCBD(R6)                                             
         NI    TCBFLAG3-TCBD(R6),255-TCBDBWAI                                   
*                                                                               
         L     R0,=F'60000'                                                     
DEBUG035 GOTO1 AGETFACT,DMCB,(X'80',=F'38'),F#WAIT 1/1000 SECOND                
         TM    TCBFLAG3-TCBD(R6),TCBDBWAI                                       
         BNZ   DEBUG040                                                         
         CLI   TCBSEN-TCBD(R6),0                                                
         BE    DEBUG025                                                         
*                                                                               
         BCT   R0,DEBUG035         TRY FOR 60 SECONDS                           
         DC    H'0'                                                             
*                                                                               
*        MAY NOT BE READY YET BUT BLOODY WELL SHOULD BE                         
*                                                                               
DEBUG040 L     R6,MYTCB                                                         
         XC    TCBTKWT,TCBTKWT     CLEAR MY TKWAIT LOOP COUNT                   
         L     R6,DBTCB                                                         
*                                                                               
         ST    R0,GOWAIT                                                        
         MVC   FULL,ADDRESS        REG AND PSW EQUATES                          
         LA    R6,TCBPSW+4-TCBD(R6)                                             
         MVC   INFIELD,SPACES                                                   
         MVC   INFIELD(3),=C'PSW'  SET PSW EQU                                  
         MVC   ADDRESS,0(R6)                                                    
*                                                                               
         MVC   FULL1,ADDRESS                                                    
         BAS   RE,SETEQU                                                        
*                                                                               
         MVC   INFIELD(3),=C'R0 '  SET REGISTER EQUS                            
DEBUG045 LA    R6,4(R6)                                                         
         MVC   ADDRESS,0(R6)                                                    
         BAS   RE,SETEQU                                                        
         IC    R1,INFIELD+1                                                     
         LA    R1,1(R1)                                                         
         STC   R1,INFIELD+1                                                     
         CLI   INFIELD+1,C'G'      TEST REG F DONE                              
         BE    DEBUG050                                                         
         CLI   INFIELD+1,X'FA'     TEST REG 9 DONE                              
         BL    DEBUG045                                                         
         MVI   INFIELD+1,C'A'      START FROM A                                 
         B     DEBUG045                                                         
*                                                                               
DEBUG050 MVC   ADDRESS,FULL        RESTORE IT                                   
*                                                                               
DEBUGX   CLI   ACTION,6            TEST FOR LIST ACTION                         
         BE    *+12                                                             
         CLI   ACTION,5            TEST FOR DISS ACTION                         
         BNE   DEBUGX1                                                          
*                                                                               
         MVI   SUBACT,2            FORCE LOCATE PSW                             
         MVC   INF01(3),=C'PSW'                                                 
*                                                                               
DEBUGX1  EQU   *                                                                
         B     XITEQU                                                           
*                                                                               
DEBUGE   MVC   ERROR,=H'51'        INVALID TERMINAL                             
         B     XITEQU                                                           
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        SAVE RESTORE SITUATIONS                            *                   
*************************************************************                   
         SPACE 1                                                                
SAVSIT   ST    RE,SAVERE                                                        
         L     RF,=A(SAVSITS-SAVED)                                             
         AR    RF,R7               RF=A(SITUATIONS)                             
*                                                                               
         LA    R1,15                                                            
SAVSIT1  MVC   64(64,RF),0(RF)     COPY ENTRY DOWN                              
         LA    RF,64(RF)                                                        
         BCT   R1,SAVSIT1          DO 15 TIMES                                  
*                                                                               
         L     RF,=A(SAVSITS-SAVED)                                             
         AR    RF,R7               RF=A(SITUATIONS)                             
         MVC   0(64,RF),SAVESIT    SAVE NEW TOP ENTRY                           
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
RESSIT   ST    RE,SAVERE                                                        
         L     RF,=A(SAVSITS-SAVED)                                             
         AR    RF,R7               RF=A(SITUATIONS)                             
*                                                                               
RESSIT1  MVC   SAVESIT,0(RF)       PF3 PULL FROM TOP                            
*                                                                               
         LA    R1,15               ROTATE OTHERS UP                             
RESSIT2  MVC   0(64,RF),64(RF)                                                  
         LA    RF,64(RF)                                                        
         BCT   R1,RESSIT2                                                       
         MVC   0(64,RF),SAVESIT    COPY CURRENT TO BOTTOM                       
*                                                                               
         OC    SAVESIT,SAVESIT                                                  
         BZ    RESSIT                                                           
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        LOAD OVERLAY SCREEN IF REQUIRED                    *                   
*************************************************************                   
         SPACE 1                                                                
LOADSCR  NTR1                                                                   
         CLI   SCREEN,0                                                         
         BE    LOADSX                                                           
         CLI   SCREEN,X'FF'                                                     
         BE    LOADSX                                                           
         CLC   INSCREEN(1),SCREEN                                               
         BE    LOADSX                                                           
*                                                                               
         LA    R6,DEBTABH                                                       
         MVC   DMCB+4(4),=X'D90A0FFF'                                           
         ST    R6,DMCB                                                          
         MVC   DMCB+7(1),SCREEN                                                 
*                                                                               
         GOTO1 ACALLOV,DMCB                                                     
         MVC   INSCREEN(1),SCREEN                                               
*                                                                               
LOADSX   B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        WRKF READ ROUTINE                                  *                   
*************************************************************                   
         SPACE 1                                                                
READWK   ST    RE,SAVERE                                                        
         MVC   WRKF,WRKFILE        SET UP WRKF CALL                             
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING UKRECD,R2                                                        
         MVC   UKUSRID,USERID                                                   
         MVC   UKSYSPRG(4),INFIELD                                              
RDWKNXT  GOTO1 ADATAMGR,DMCB,INDEX,WRKF,KEY,AIO,AWKBUFF                         
         CLI   8(R1),0                                                          
         BE    RDWK010                                                          
         CLI   8(R1),X'90'         TEST FOR EOF                                 
         BE    RDWKX                                                            
         DC    H'0'                                                             
*                                                                               
         USING W_RECD,R2                                                        
RDWK010  CLC   W_SYSPRG(4),INFIELD MATCH ON WRKF ID                             
         BNE   RDWKNXT                                                          
         MVC   WRKFNO,W_FILENO     SAVE FILE NUMBER                             
         XC    WRKFREC,WRKFREC     RESET RECORD NUMBER                          
         B     RDWK020                                                          
*                                                                               
READNXT  ST    RE,SAVERE           ENTRY POINT FOR READ NEXT                    
*                                                                               
RDWK020  ICM   RF,15,WRKFREC       BUMP RECORD NUMBER                           
         LA    RF,1(RF)                                                         
         STCM  RF,15,WRKFREC                                                    
         GOTO1 ADATAMGR,DMCB,READ,WRKF,KEY,AIO,AWKBUFF                          
         CLI   8(R1),0                                                          
         BE    RDWK030                                                          
         CLI   8(R1),X'90'         TEST FOR EOF                                 
         BE    RDWKNXT                                                          
         DC    H'0'                                                             
*                                                                               
RDWK030  L     RE,SAVERE                                                        
         CR    RB,RB                                                            
         BR    RE                                                               
RDWKX    L     RE,SAVERE                                                        
         LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        COMMON SUBROUTINES BITS IN BYTE TO DUB 1.1.1.1.    *                   
*************************************************************                   
         SPACE 1                                                                
TITBITS  ST    RE,SAVERE           SET BIT PATERN FROM BYTE IN DUB              
         LA    RE,DUB                                                           
         LA    R1,X'80'                                                         
TBITS1   MVI   0(RE),C'.'          DEFAULT TO . FOR CLEAR BIT                   
         EX    R1,*+8                                                           
         B     *+8                                                              
         TM    BYTE,0              TEST BIT                                     
         BNO   *+8                                                              
         MVI   0(RE),C'1'          SET TO 1 IF ON                               
         LA    RE,1(RE)                                                         
         SRL   R1,1                NEXT BIT                                     
         LTR   R1,R1                                                            
         BNZ   TBITS1                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        VALIDATE ACTION (1ST FIELD IN PARBLK) SET ACTION   *                   
*************************************************************                   
         SPACE 1                                                                
VALACT   NTR1                                                                   
         MVI   PROT,C' '                                                        
         LA    R2,PARBLK           POINT TO PARBLK                              
         USING PSND,R2                                                          
*                                                                               
VALA000  CLI   PSNTAG,PSNFLDQ                                                   
         BNE   VALACTE             MUST BE A FIELD (INVALID ACTION)             
*                                                                               
         CLI   COMMAND,13          WATCH SOMETHING                              
         BE    VALA100                                                          
*                                                                               
         L     RE,PSNCOMP                                                       
VALA005  LA    RF,ACTTAB                                                        
*                                                                               
         CLI   PSNLEN,1            MUST BE LENGTH 1                             
         BNE   VALA010                                                          
         CLI   0(RE),C'*'          * PROTECTS ACTIONS                           
         BNE   VALA010                                                          
         MVI   PROT,C'*'                                                        
         B     VALA990                                                          
*                                                                               
VALA010  SR    R1,R1                                                            
         IC    R1,PSNLEN                                                        
         BCTR  R1,0                                                             
         TM    FLAG1,FLNEWQ        ONLY ONE ACTION                              
         BO    VALA031                                                          
*                                                                               
VALA020  CLI   8(RF),X'02'         IF 02 PHASE ACTIONS                          
         BNE   VALA022                                                          
*                                                                               
         LA    R1,7(RF)            MATCH NEEDS TO BE EXACT                      
VALA021  CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         B     VALA021                                                          
         SR    R1,RF                                                            
*                                                                               
VALA022  TM    11(RF),X'F0'        ANY MIN LEN                                  
         BZ    *+14                                                             
         LLC   R1,11(RF)           USE THAT THEN                                
         SRL   R1,4                                                             
         EX    R1,*+8              SEARCH ACTTAB                                
         B     *+10                                                             
         CLC   0(0,RF),0(RE)                                                    
         BE    VALA040                                                          
*                                                                               
VALA030  LA    RF,L'ACTTAB(RF)     TRY NEXT ENTRY                               
         CLI   0(RF),X'FF'                                                      
         BNE   VALA010                                                          
*                                                                               
VALA031  CLI   SUBACT,1            TEST FOR FIND SUBACTION                      
         BE    VALA060                                                          
         CLI   ACTION,6            TEST FOR LIST ACTION                         
         BE    VALA060                                                          
         L     RE,PSNCOMP                                                       
         TM    PSNSTAT,PSNHEXQ     NO MATCH TEST HEX                            
         BO    VALA032                                                          
         CLC   0(2,RE),=C'X'''     OR SPECIFIC HEX                              
         BNE   VALA060                                                          
*                                                                               
VALA032  CLI   COMMAND,12          DON'T DISTURB HEXFLD WHILE STEPPING          
         BE    VALA033                                                          
*                                                                               
         MVC   WORK,=C'00000000'   HEX FIELD MUST BE ADDR                       
         L     RE,PSNCOMP                                                       
         CLI   PSNLEN,8            8 IS MAX HEX LEN                             
         BH    VALA060                                                          
         SR    RF,RF                                                            
         IC    RF,PSNLEN           WORK HEX INPUT INTO HEXFLD                   
         LA    R1,WORK+8                                                        
         SR    R1,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
         LA    RF,HEXFLD                                                        
         ST    RF,DMCB+4                                                        
         GOTO1 AHEXIN,DMCB,WORK,,8                                              
*                                                                               
VALA033  L     R1,PSNCOMP                                                       
         GOTO1 AVALNUM             MAYBE DECIMAL TOO                            
         ST    R1,DECFLD                                                        
*                                                                               
         CLI   PFKEY,0             IF JUST ENTER IS HIT                         
         BNE   VALA990                                                          
         CLI   SUBACT,0                                                         
         BNE   VALA990                                                          
*NOP     MVI   SUBACT,2            HEXFLD IS IMPLIED LOCATE                     
         B     VALA990                                                          
*                                                                               
VALA040  CLC   8(2,RF),=X'0000'    IS THIS A SUB ACTION                         
         BE    VALA050                                                          
         CLC   8(2,RF),=X'00FF'    IS THIS A ROOT COMMAND                       
         BE    VALA055                                                          
*                                                                               
         OI    FLAG1,FLNEWQ        FLAG NEW ACTION                              
*                                                                               
         MVC   ACTION,10(RF)       SAVE ACTION NUMBER                           
         MVC   OVERLY,8(RF)        SAVE OVERLAY NUMBER                          
         MVC   SCREEN,9(RF)        SAVE SCREEN NUMBER                           
         MVC   ACTFLG,11(RF)       SAVE ACT FLAGS                               
         B     VALA990                                                          
*                                                                               
VALA050  MVC   SUBACT,10(RF)       SAVE SUB ACTION                              
*NOP     OI    FLAG1,FLNEWQ        FLAG NEW ACTION                              
         B     VALA990                                                          
*                                                                               
VALA055  MVC   COMMAND,10(RF)      SAVE COMMAND                                 
         B     VALA990                                                          
*                                                                               
VALA060  MVC   WORK,SPACES                                                      
         L     RE,PSNCOMP                                                       
         SR    RF,RF                                                            
         IC    RF,PSNLEN           PUT FIELD LEN IN WORK                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         GOTO1 ADECODE,DMCB,(00,WORK),(X'40',INFIELD),0                         
         CLI   8(R1),X'FF'                                                      
         BE    VALA061                                                          
         CLI   ACTION,6            LIST ACTION TREAT ALL AS FREEFORM            
         BE    VALA061                                                          
*                                                                               
         MVC   INFLEN,11(R1)                                                    
         B     *+16                                                             
*                                                                               
VALA061  MVC   INFLEN,PSNLEN       FREEFORM FIELD LEN                           
         MVC   INFIELD,WORK                                                     
*                                                                               
VALA090  MVI   INFCHR,C' '         SET NO SPLIT                                 
         LA    R1,INFIELD                                                       
         LR    RF,R1                                                            
         LA    R0,59               SCAN 60 BYTES                                
         MVC   INF01,SPACES                                                     
         MVC   INF02,SPACES                                                     
VALA091  CLI   0(R1),C','          LOOK FOR SPLIT FIELD                         
         BE    VALA092                                                          
         CLI   0(R1),C'+'          "," OR "+  OR "-"                            
         BE    VALA092                                                          
         CLI   0(R1),C'-'          "," OR "+  OR "-"                            
         BE    VALA092                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,VALA091                                                       
         MVC   INF02(30),0(RF)     SAVE 2ND HALF                                
*                                                                               
         LA    R1,INF02                                                         
         GOTO1 AVALNUM             MAYBE NUMERIC                                
         ST    R1,DECFLD                                                        
*                                                                               
         CLI   INFCHR,C' '                                                      
         BNE   VALA990                                                          
         MVC   INF01,INF02         COPY BOTH IF 1 FIELD                         
         B     VALA990                                                          
*                                                                               
VALA092  MVC   INFCHR,0(R1)        SAVE SPLIT CHR                               
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   INF01(0),0(RF)      SAVE 1ST HALF                                
         LA    R1,2(RF,R1)                                                      
         LR    RF,R1                                                            
         B     VALA091             GO BACK FOR SECOND                           
*                                                                               
         B     VALA990                                                          
*                                                                               
VALA100  TM    PSNSTAT,PSNNUMQ     IF NUM WITH ATTRIBUTES                       
         BNO   VALA110                                                          
         OC    PSNATTR,PSNATTR     ASSUME N(RN)                                 
         BZ    VALA110                                                          
*                                                                               
         MVC   WATDISP,PSNNUM+1    SAVE DISPLACEMENT                            
         L     R1,PSNCOMP                                                       
         MVC   WATTEXT(6),0(R1)    SAVE THIS FOR TEXT                           
         L     R2,PSNATTR                                                       
         L     R1,PSNCOMP                                                       
         CLI   0(R1),C'R'          ATTRIB MUST BE RN                            
         BNE   VALACTE                                                          
         MVC   WATBASE,1(R1)                                                    
         NI    WATBASE,X'0F'                                                    
         CLI   1(R1),C'F'          CONVERT RA TO RF                             
         BH    *+16                                                             
         IC    R1,WATBASE                                                       
         LA    R1,9(R1)                                                         
         STC   R1,WATBASE                                                       
         MVI   WATCLEN,16                                                       
         MVI   WATADDR,C'R'                                                     
         B     VALA120                                                          
*                                                                               
VALA110  MVC   INFLEN,PSNLEN       FREEFORM WATCH FIELD                         
         L     R1,PSNCOMP                                                       
         MVC   INFIELD,0(R1)                                                    
         B     VALA120                                                          
*                                                                               
VALA120  CLI   NPARMS,3            IS THERE AN XL CL TYPE PARM                  
         BNE   VALACTX                                                          
*                                                                               
         LA    R2,32(R2)                                                        
         L     R1,PSNCOMP                                                       
VALA121  CLC   0(2,R1),=C'XL'      XLNN = NN BYTES HEX                          
         BE    VALA125                                                          
         CLC   0(2,R1),=C'CL'      CLNN = NN BYTES CHR                          
         BE    VALA125                                                          
         CLC   0(3,R1),=C'ICL'     ICLNN = NN BYTES CHR INDIRECT                
         BE    *+14                                                             
         CLC   0(3,R1),=C'IXL'     IXLNN = NN BYTES HEX INDIRECT                
         BNE   VALACTX                                                          
         MVI   WATADDR,C'I'                                                     
         LA    R1,1(R1)                                                         
         B     VALA121                                                          
*                                                                               
VALA125  MVC   WATCTYPE,0(R1)                                                   
         MVC   HALF,2(R1)                                                       
         CLI   HALF+1,C' '                                                      
         BH    *+14                                                             
         MVC   HALF+1(1),HALF                                                   
         MVI   HALF,C'0'                                                        
         PACK  DUB,HALF                                                         
         CVB   RF,DUB                                                           
         STC   RF,WATCLEN                                                       
         B     VALACTX                                                          
*                                                                               
VALA990  LA    R2,32(R2)           TEST FOR MORE                                
         IC    R1,NPARMS                                                        
         BCTR  R1,0                                                             
         STC   R1,NPARMS                                                        
         CLI   NPARMS,0                                                         
         BNE   VALA000                                                          
*                                                                               
VALACTX  B     XITEQU                                                           
VALACTE  MVC   ERROR,=H'11'                                                     
         B     XITEQU                                                           
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS & LITERALS                               *                   
*************************************************************                   
         SPACE 1                                                                
SPACES   DC    80C' '                                                           
DMWRT    DC    CL8'DMWRT '                                                      
DMREAD   DC    CL8'DMREAD'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
TEMPSTR  DC    CL8'TEMPSTR'                                                     
WRKFILE  DC    CL8'WRKFILE'                                                     
INDEX    DC    CL8'INDEX  '                                                     
READ     DC    CL8'READ   '                                                     
*                                                                               
SEPTAB   DC    X'01',C';'          PARSNIP OVERIDES                             
         DC    X'01',C' '                                                       
         DC    X'01',C';'                                                       
         DC    X'01',C';'                                                       
*                                                                               
         DS    0F                  STARTECB MUST BE FULLWORD                    
STARTECB DC    X'40000000'                                                      
ZEROS    DC    16X'00'                                                          
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        ACTION TABLE                                       *                   
*************************************************************                   
         SPACE 1                                                                
*              CL8'KEYWORD ',X'OV',X'SC',AL1(NUM),X'FL'                         
ACTTAB   DS    0CL12                                                            
*                                                                               
*        THE FOLLOWING ARE SUB ACTIONS OV/SC 0000                               
*                                                                               
         DC    CL8'FIND    ',X'00',X'00',AL1(001),X'00'                         
         DC    CL8'LOCATE  ',X'00',X'00',AL1(002),X'00'                         
         DC    CL8'XLOCATE ',X'00',X'00',AL1(002),X'01'                         
         DC    CL8'TAB     ',X'00',X'00',AL1(003),X'00'                         
*                                                                               
*        THE FOLLOWING ARE ACTIONS                                              
*                                                                               
         DC    CL8'TRANSFER',X'01',X'FF',AL1(002),X'00'                         
         DC    CL8'PAT     ',X'01',X'FF',AL1(003),X'00'                         
         DC    CL8'DISPLAY ',X'01',X'FF',AL1(004),X'00'                         
         DC    CL8'DISS    ',X'01',X'FF',AL1(005),X'00'                         
         DC    CL8'LIST    ',X'01',X'FF',AL1(006),X'00'                         
*                                                                               
         DC    CL8'MODS    ',X'02',X'FE',AL1(007),X'00'                         
         DC    CL8'USINGS  ',X'02',X'FE',AL1(008),X'00'                         
         DC    CL8'ADDRS   ',X'02',X'FE',AL1(009),X'00'                         
         DC    CL8'REGS    ',X'02',X'FE',AL1(010),X'00'                         
         DC    CL8'EQUS    ',X'02',X'FE',AL1(011),X'00'                         
         DC    CL8'TRAPS   ',X'02',X'FE',AL1(012),X'00'                         
         DC    CL8'PATCHES ',X'02',X'FE',AL1(013),X'00'                         
*                                                                               
         DC    CL8'TRAPS   ',X'02',X'FD',AL1(012),X'00'                         
*                                                                               
*        THE FOLLOWING ARE ROOT COMMANDS OV/SC 00FF                             
*                                                                               
         DC    CL8'LOAD    ',X'00',X'FF',AL1(003),X'00'                         
         DC    CL8'LOADFAC ',X'00',X'FF',AL1(001),X'00'                         
         DC    CL8'EQU     ',X'00',X'FF',AL1(002),X'00'                         
         DC    CL8'LOADMOD ',X'00',X'FF',AL1(003),X'00'                         
         DC    CL8'LMOD    ',X'00',X'FF',AL1(003),X'00'                         
         DC    CL8'ACCESS  ',X'00',X'FF',AL1(004),X'00'                         
         DC    CL8'USING   ',X'00',X'FF',AL1(005),X'00'                         
         DC    CL8'TESTIT  ',X'00',X'FF',AL1(006),X'00'                         
         DC    CL8'DEBUG   ',X'00',X'FF',AL1(007),X'00'                         
         DC    CL8'GO      ',X'00',X'FF',AL1(008),X'00'                         
         DC    CL8'.       ',X'00',X'FF',AL1(010),X'00'                         
         DC    CL8'..      ',X'00',X'FF',AL1(009),X'00'                         
         DC    CL8'KILL    ',X'00',X'FF',AL1(011),X'00'                         
         DC    CL8'STEP    ',X'00',X'FF',AL1(012),X'00'                         
         DC    CL8'WATCH   ',X'00',X'FF',AL1(013),X'00'                         
         DC    CL8'PATCH   ',X'00',X'FF',AL1(014),X'00'                         
         DC    CL8'RESET   ',X'00',X'FF',AL1(015),X'40'                         
         DC    CL8'MEMORY  ',X'00',X'FF',AL1(016),X'00'                         
         DC    X'FF'                                                            
ACTTABX  EQU   *                                                                
*                                                                               
WATCHTAB DS    0CL9                                                             
         DC    CL8'PSW     ',X'80'                                              
         DC    CL8'REGS    ',X'40'                                              
         DC    CL8'OPER    ',X'20'                                              
         DC    X'FF'                                                            
WATCHTBX EQU   *                                                                
         EJECT                                                                  
************************************************************                    
*        COMMON SUBROUTINES / BASE=*                       *                    
************************************************************                    
         SPACE 1                                                                
         DROP  RB,RA               DROP BASE REGS                               
         SPACE 1                                                                
*************************************************************                   
*        VALIDATE NUMERIC R1=ADDR                           *                   
*************************************************************                   
         SPACE 1                                                                
VALNUM   NTR1  BASE=*                                                           
         LA    R0,8                MAX LEN OF NUMBER                            
         LR    R5,R1                                                            
         CLC   0(2,R1),=C'X'''     CHECK FOR HEX NUMBER                         
         BE    VALS03                                                           
*                                                                               
VALS01   CLI   0(R1),X'F0'         TEST 0-9                                     
         BL    VALS02                                                           
         CLI   0(R1),X'F9'                                                      
         BH    VALS02                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VALS01                                                        
         B     VALXIT              TOO BIG                                      
*                                                                               
VALS02   ST    R1,FULL             SAVE A(END)                                  
         SR    R1,R5               R1=LEN                                       
         BZ    VALXIT                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R5)         ZONED TO PACKED                              
         CVB   R1,DUB                                                           
         B     VALXIT                                                           
*                                                                               
VALS03   LA    RF,2(R1)            BUMP PAST X'                                 
         LA    R0,9                MAX LEN OF NUMBER                            
         LR    R5,RF                                                            
VALS03A  CLI   0(RF),C''''         FIND TERMINATING '                           
         BE    VALS04                                                           
         LA    RF,1(RF)                                                         
         BCT   R0,VALS03A          X'00000000'                                  
         LA    R1,0                                                             
         B     VALXIT                                                           
*                                                                               
VALS04   SR    RF,R5                                                            
         MVC   WORK(8),=C'00000000'                                             
         LA    R1,8                                                             
         SR    R1,RF                                                            
         LA    R1,WORK(R1)                                                      
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R5)                                                    
*                                                                               
         GOTO1 AHEXIN,DMCB,WORK,FULL,8                                          
         L     R1,FULL                                                          
*                                                                               
VALXIT   XIT1  REGS=(R1)                                                        
         EJECT                                                                  
*************************************************************                   
*        LOCATE LABEL / CSECT                               *                   
*************************************************************                   
         SPACE 1                                                                
LOCATE   NTR1  BASE=*                                                           
         CLI   INF01,C'*'          * MEANS HERE                                 
         BE    LOCAYES                                                          
         CLI   INF01,C' '          " " MEANS HERE                               
         BE    LOCAYES                                                          
         CLC   INF01(3),=C'PSW'                                                 
         BNE   LOCL005                                                          
         GOTO1 ALOCPSW                                                          
         BE    LOCAYES                                                          
*                                                                               
LOCL005  LA    R3,USERADS          TEST LOCAL EQUATES FIRST                     
LOCL010  CLI   0(R3),0                                                          
         BE    LOCA000                                                          
         CLC   0(8,R3),INF01                                                    
         BE    LOCA090                                                          
         LA    R3,12(R3)                                                        
         LA    R1,USERADX                                                       
         CR    R1,R3                                                            
         BH    LOCL010                                                          
*                                                                               
LOCA000  L     R3,ATIA                                                          
         CLC   0(8,R3),=C'**ADDR**'                                             
         BE    LOCA010                                                          
         MVI   TWAPAGE,1           READ TWA PAGE FIRST TIME                     
         GOTO1 AREADTWA                                                         
         CLC   0(8,R3),=C'**ADDR**'                                             
         BNE   LOCA040             CHECK LOADED OK                              
LOCA010  LA    R3,8(R3)                                                         
*                                                                               
LOCA020  CLC   0(8,R3),=C'**ENDA**'                                             
         BE    LOCA040             TEST FOR END                                 
*                                                                               
         CLC   INF01(8),0(R3)      CHECK LABEL                                  
         BE    LOCA090                                                          
         LA    R3,12(R3)           TRY NEXT                                     
         B     LOCA020                                                          
*                                                                               
LOCA040  GOTO1 AGETUSE             LOOK FOR A USING                             
         BNE   LOCA050                                                          
         B     LOCA070                                                          
*                                                                               
LOCA050  GOTO1 ACLOCATE            LOOK WITHIN USINGS                           
         BNE   LOCAHEX                                                          
LOCA055  MVI   ACTION,6            SET ACTION LIST                              
         B     LOCAYES                                                          
*                                                                               
LOCA070  MVI   ACTION,6            SET ACTION LIST                              
         OI    FLAG1,FLNEWQ        FORCE RELOAD MODULE                          
         B     LOCAXXX                                                          
*                                                                               
LOCA090  MVC   ADDRESS,8(R3)       FOUND IT                                     
         MVI   ACTION,4            SET TO DISPLAY MODE                          
*                                                                               
LOCAYES  TM    ACTFLG,X'01'        IGNORE HI BYTE UNLESS XLOCATE                
         BO    *+8                                                              
         MVI   ADDRESS,0                                                        
*                                                                               
         CR    RC,RC                                                            
         B     LOCAXXX                                                          
*                                                                               
LOCAHEX  LA    RF,INF01            CHECK FOR A HEX ADDRESS                      
LOCAHEX1 CLI   0(RF),C' '                                                       
         BNH   LOCAHEX2                                                         
         LA    RF,1(RF)                                                         
         B     LOCAHEX1                                                         
*                                                                               
LOCAHEX2 LA    R1,INF01                                                         
         SR    RF,R1               RF=LEN                                       
         MVC   WORK,=C'00000000'                                                
         LA    R1,WORK+8                                                        
         SR    R1,RF                                                            
         MVC   0(8,R1),INF01                                                    
         GOTO1 AHEXIN,DMCB,WORK,FULL,8                                          
         MVC   ADDRESS,FULL                                                     
         OC    ADDRESS,ADDRESS                                                  
         BNZ   LOCAXXX                                                          
*                                                                               
LOCANO   LTR   RC,RC                                                            
*                                                                               
LOCAXXX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        LOCATE LABEL / WITHIN DSECT/CSECT                  *                   
*************************************************************                   
         SPACE 1                                                                
CLOCATE  NTR1  BASE=*                                                           
*                                                                               
CLOC001  L     R3,ATIA                                                          
         CLC   0(8,R3),=C'**MODS**'                                             
         BE    CLOC010                                                          
         MVI   TWAPAGE,2           READ TWA PAGE FIRST TIME                     
         GOTO1 AREADTWA                                                         
         CLC   0(8,R3),=C'**MODS**'                                             
         BNE   CLOCNO              CHECK LOADED OK                              
CLOC010  LA    R3,8(R3)                                                         
*                                                                               
CLOC015  LA    R4,USINGS           R4=A(USINGS)                                 
         LR    R5,R4                                                            
         AH    R5,=Y(USINGX-USINGS)                                             
*                                                                               
CLOC016  L     R3,ATIA             R3=A(MODULES)                                
         LA    R3,8(R3)                                                         
         MVC   DUB,0(R4)           DUB=USING LABEL                              
*                                                                               
CLOC020  CLC   0(8,R3),=C'**ENDM**'                                             
         BE    CLOC110             TEST FOR END                                 
*                                                                               
         CLC   DUB(8),0(R3)        CHECK LABEL                                  
         BE    CLOC040                                                          
CLOC030  LA    R3,L'MODULE(R3)     TRY NEXT MODULE                              
         B     CLOC020                                                          
*                                                                               
CLOC040  MVC   DUB+0(2),8(R3)      FILE NUMBER                                  
         MVC   DUB+4(4),10(R3)                                                  
         GOTO1 ARANDOM             OPEN WRKF FILE                               
         L     R2,AIO                                                           
         MVC   FULL,10(R3)                                                      
         B     CLOC101                                                          
*                                                                               
CLOC100  LA    R1,1                BUMP RECORD COUNT                            
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         GOTO1 AREADMOD            READ A RECORD                                
         BNE   CLOC110                                                          
CLOC101  CLC   INF01(8),45(R2)                                                  
         BNE   CLOC100                                                          
*                                                                               
         CLI   8(R4),0                                                          
         BE    CLOC012                                                          
         MVC   INF01,8(R4)                                                      
         GOTO1 ALOCATE                                                          
         B     *+10                                                             
CLOC012  MVC   ADDRESS,12(R4)                                                   
         MVC   WRKFNO,8(R3)                                                     
         MVC   WRKFBASE,10(R3)                                                  
         MVC   BASETY,14(R3)                                                    
         MVC   WRKFREC,FULL                                                     
         B     CLOCYES                                                          
*                                                                               
CLOC110  LA    R4,16(R4)                                                        
         CR    R4,R5                                                            
         BNL   CLOCNO                                                           
         B     CLOC016                                                          
*                                                                               
CLOCYES  CR    RC,RC                                                            
         B     CLOCXXX                                                          
CLOCNO   LTR   RC,RC                                                            
CLOCXXX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        LOCATE PSW                                         *                   
*************************************************************                   
         SPACE 1                                                                
LOCPSW   NTR1  BASE=*                                                           
*                                                                               
         XC    FULL1,FULL1                                                      
         MVC   HALF,=X'7FFF'                                                    
         LA    R0,512                                                           
*                                                                               
         ICM   R1,15,DBTCB         MUST HAVE TCB                                
         BZ    LOCPSWN                                                          
         MVC   FULL,TCBPSW+4-TCBD(R1)                                           
         NC    FULL,=X'7FFFFFFF'   REMOVE ANY 31 BIT STUFF                      
*                                                                               
         LA    R2,USINGS                                                        
LOCP010  OC    0(16,R2),0(R2)      MUST BE SOMETHING                            
         BZ    LOCP600                                                          
*                                                                               
LOCP020  CLI   8(R2),0             MUST BE AN ADDRESS                           
         BNE   LOCP600                                                          
         CLC   FULL,12(R2)         PSW MUST BE > ADDRESS                        
         BL    LOCP600                                                          
         L     R1,FULL                                                          
         S     R1,12(R2)                                                        
         CH    R1,HALF             FIND THE LOWEST OFFSET                       
         BH    LOCP600                                                          
*                                                                               
         LR    R4,R1               SAVE LOWSET OFFSET                           
         ST    R2,FULL1            AND A(ENTRY)                                 
*                                                                               
LOCP600  LA    R2,16(R2)           NEXT ENTRY                                   
         BCT   R0,LOCP010                                                       
*                                                                               
         ICM   R2,15,FULL1         LOCATE NEAREST USING                         
         BZ    LOCP900                                                          
         MVC   INF01(8),0(R2)                                                   
         MVC   INFIELD(8),0(R2)                                                 
         GOTO1 ACLOCATE            LOCATE CSECT                                 
         MVC   FULL,WRKFREC                                                     
*                                                                               
         XC    HALF,HALF                                                        
LOCP700  L     R2,AIO                                                           
         XC    FULL1,FULL1                                                      
         GOTO1 AREADMOD            READ A LINE                                  
         BNE   LOCP900                                                          
         LA    R1,1                                                             
         A     R1,FULL             BUMP COUNT                                   
         ST    R1,FULL                                                          
         GOTO1 AHEXIN,DMCB,(0,5(R2)),FULL1+1,6                                  
         OC    12(4,R1),12(R1)                                                  
         BZ    LOCP700                                                          
*                                                                               
         CH    R4,HALF             IF OFFSET DROPS THEN NEW CSECT               
         BL    LOCP900                                                          
         STH   R4,HALF             PUT OFFSET INTO HALF                         
*                                                                               
         LH    R1,FULL1+2                                                       
*        LA    R1,16(R1)           WITHIN 16 BYTES                              
         LA    R1,0(R1)            WITHIN 0 BYTES                               
         SH    R1,HALF                                                          
         BM    LOCP700                                                          
         MVC   WRKFREC,FULL        SET NEW POINTER                              
         MVI   ACTION,6            ??WAS NOP                                    
         B     LOCPSWY                                                          
*                                                                               
LOCP900  MVI   ACTION,5                                                         
         ICM   R1,15,DBTCB         MUST HAVE TCB                                
         ICM   RF,15,TCBPSW+4-TCBD(R1)                                          
         XC    BASEADDR,BASEADDR                                                
         ST    RF,ADDRESS                                                       
*                                                                               
LOCPSWY  CR    RC,RC                                                            
         B     LOCPSWX                                                          
LOCPSWN  LTR   RC,RC                                                            
LOCPSWX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WRKF INDEX/RANDOM AND READ SUB ROUTINES            *                   
*************************************************************                   
         SPACE 1                                                                
RANDOM   NTR1  BASE=*                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R2,KEY              DIRECT CALL BY FILE#                         
         USING UKRECD,R2                                                        
         MVC   UKUSRID,USERID                                                   
         MVC   UKFILENO,DUB        FILE NUMBER IN DUB(2)                        
         OI    UKFLAG,X'80'                                                     
         GOTO1 ADATAMGR,DMCB,=C'INDEX',=C'WRKFILE',KEY,AIO,AWKBUFF              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R2,AIO              READ RANDOM FOR RECNO                        
         XC    0(12,R2),0(R2)                                                   
         MVC   0(4,R2),DUB+4       WRKFREC IN DUB+4(4)                          
         MVC   4(4,R2),=C'REC '                                                 
         GOTO1 ADATAMGR,DMCB,=C'RANDOM',=C'WRKFILE',KEY,AIO,AWKBUFF             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RANDXXX  XIT1                                                                   
         LTORG                                                                  
*                                                                               
READMOD  NTR1  BASE=*                                                           
*                                                                               
         L     R2,AIO                                                           
         GOTO1 ADATAMGR,DMCB,=C'READ',=C'WRKFILE',KEY,AIO,AWKBUFF               
         CLI   8(R1),X'90'                                                      
         BE    READNO                                                           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   45(R2),C'*'         COMMENT IS OK                                
         BE    READYES                                                          
*                                                                               
         CLC   5(6,R2),=C'000000'  IGNORE NON ZERO CSECTS                       
         BNE   READYES                                                          
*                                                                               
         CLC   54(5,R2),=C'CSECT'                                               
         BE    READNO                                                           
         CLC   54(5,R2),=C'START'                                               
         BE    READNO                                                           
*                                                                               
         CLC   54(5,R2),=C'DSECT'                                               
         BE    READNO                                                           
*                                                                               
READYES  CR    RC,RC                                                            
         B     READXXX                                                          
READNO   LTR   RC,RC                                                            
READXXX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        GET USING ADDRESS                                  *                   
*************************************************************                   
         SPACE 1                                                                
GETUSE   NTR1  BASE=*                                                           
         LA    R1,USINGS                                                        
         LR    RF,R1               USINGX                                       
         AH    RF,=Y(USINGX-USINGS)                                             
GETU010  CLC   INF01(8),0(R1)      MATCH ON NAME                                
         BE    GETU020                                                          
         LA    R1,16(R1)                                                        
         CR    R1,RF                                                            
         BL    GETU010                                                          
         LTR   RC,RC               SET CC NEQ                                   
         B     GETUXXX                                                          
*                                                                               
GETU020  CLI   8(R1),0                                                          
         BE    GETU030                                                          
         MVC   INF01,8(R1)                                                      
         GOTO1 ALOCATE                                                          
         B     GETUXXX                                                          
*                                                                               
GETU030  MVC   ADDRESS,12(R1)      SET ADDRESS                                  
         B     GETUXXX                                                          
*                                                                               
GETUXXX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        READ/WRITE TEMPSTR TO TIA                          *                   
*************************************************************                   
         SPACE 1                                                                
READTWA  NTR1  BASE=*                                                           
         SR    RF,RF                                                            
         IC    RF,TWAPAGE                                                       
         SLL   RF,32-8                                                          
         ICM   RF,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADATAMGR,DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(RF),ATIA           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
WRITTWA  NTR1  BASE=*                                                           
         SR    RF,RF                                                            
         IC    RF,TWAPAGE                                                       
         SLL   RF,32-8                                                          
         ICM   RF,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADATAMGR,DMCB,(X'80',=C'DMWRT'),=C'TEMPSTR',(RF),ATIA            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        BUILD COMMAND FOR DATASPACE                        *                   
*************************************************************                   
         SPACE 1                                                                
SETCOM   NTR1  BASE=*                                                           
*                                                                               
         XC    WORK,WORK           BUILD COMLINE IN WORK                        
         LA    R4,WORK                                                          
         USING DSCOMM,R4                                                        
         MVC   DSCSORC,FACID       FROM ME                                      
         MVI   DSCSORC+1,0                                                      
         MVI   DSCDEST,0           TO ADNUM (ZERO FOR ALL)                      
         MVI   DSCDEST+1,0                                                      
         STH   R1,DSCCOMM          SET ACTION CODE                              
         MVC   DSCDATA,APATCHAR    PASS A(PATCH AREA)                           
*                                                                               
         TIME  BIN                 SAVE TIME IN DSCTIME                         
         ST    R0,DSCTIME                                                       
*                                                                               
         SR    R2,R2               SET TO DATASPACE                             
         USING DMDSHDR,R2                                                       
         L     R1,ASSB                                                          
         LAM   AR2,AR2,SSBALET-SSBD(R1)                                         
         LAM   AR3,AR3,SSBALET-SSBD(R1)                                         
         SAC   512                                                              
*                                                                               
SETCOM1A L     R3,DHAADVS          SET R3 TO ADV SYS BLOCK                      
         LA    R0,32                                                            
SETCOM2  CLI   0(R3),0             EMPTY ADV ENTRY                              
         BE    SETCOM6                                                          
         CLI   10(R3),0                                                         
         BE    SETCOM6                                                          
*TEMP                                                                           
         CLC   10(1,R3),FACID                                                   
         BNE   SETCOM6                                                          
*TEMP                                                                           
*                                                                               
SETCOM3  SR    R2,R2               POINT TO DSPACE DHACOMM                      
         LA    RE,256                                                           
         L     R2,4(,R2)                                                        
         AH    R2,=H'4096'         SKIP HEADERS                                 
SETCOM4  L     RF,=F'-1'           SET TO FFS TO GRAB ENTRY                     
         SR    R1,R1                                                            
         CS    R1,RF,0(R2)                                                      
         BNE   SETCOM5                                                          
         MVC   0(32,R2),WORK                                                    
         MVC   6(1,R2),10(R3)      SEND TO THIS ADV                             
*                                                                               
         MVC   WORK1,0(R3)                                                      
         SAC   0                   GO POST THE SSBOPECB                         
         BAS   RE,POSTIT                                                        
         BE    *+10                                                             
         XC    0(32,R2),0(R2)      IF POST FAILS THEN REMOVE IT                 
         SAC   512                                                              
*                                                                               
         B     SETCOM6                                                          
*                                                                               
SETCOM5  LA    R2,32(,R2)                                                       
         BCT   RE,SETCOM4          ALL ENTRIES FULL                             
*NOP     BAS   RE,CLEANUP                                                       
         DC    H'0'                                                             
*                                                                               
SETCOM6  LA    R3,32(,R3)                                                       
         BCT   R0,SETCOM2          ALL 32 DONE                                  
*                                                                               
SETCOMX  LAM   AR3,AR3,=F'0'         MUST CLEAR THIS                            
         SAC   0                                                                
         XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        DO X MEMORY POST TO FACPAK (DETAILS IN CARD)       *                   
*************************************************************                   
         SPACE 1                                                                
POSTIT   NTR1                                                                   
         LH    R4,WORK1+12         ASID IS AT +12                               
         L     R2,WORK1+16         SSBOPECB IS AT +16                           
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         BNZ   NOPOST                                                           
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   NOPOST                                                           
         L     R4,ASCBASSB                                                      
*                                                                               
         SAM31                                                                  
         L     R4,168(R4)          ASSBJSAB-ASSB(R4) R4 = A(JSAB)               
         USING JSAB,R4                                                          
         CLC   WORK1(8),JSABJBNM                                                
         SAM24                                                                  
         BNE   NOPOST                                                           
*                                                                               
         LA    R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         B     XITEQ                                                            
*                                                                               
NOPOST   MVC   WORK1+10(8),WORK1                                                
         MVC   WORK1+2(8),WORK1+10                                              
         MVC   WORK1+10(18),=C' SYSTEM NOT POSTED'                              
         MVC   WORK1+0(2),=H'26'                                                
         WTO   TEXT=WORK1                                                       
         B     XITNE                                                            
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4                                                            
XITEQ    CR    RB,RB               EXIT CC EQU                                  
         B     XIT2                                                             
XITNE    LTR   RB,RB               EXIT CC NEQ                                  
*                                                                               
XIT2     XIT1                      EXIT                                         
         EJECT                                                                  
*************************************************************                   
*        ERROR AND INFO EXITS                               *                   
*************************************************************                   
         SPACE 1                                                                
ERRORS   DC    CL32'Patch area is already in use    '   #1                      
         DC    CL32'Phase not found in PGMS         '   #2                      
         EJECT                                                                  
INFOS    DC    CL32'Patch data then enter PATCH     '   #1                      
         DC    CL32'Select patches req''d then PATCH '  #2                      
         DC    CL32'Debug has initialised            '  #2                      
         DC    CL32'Control Debug program            '  #2                      
         EJECT                                                                  
* DMDSHDR                                                                       
       ++INCLUDE DMDSHDR                                                        
         IHAASCB                                                                
         IAZJSAB                                                                
         EJECT                                                                  
*CTDEBWORK                                                                      
       ++INCLUDE CTDEBWORK                                                      
*DMSHMUSSD                                                                      
       ++INCLUDE DMSHMUSSD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016CTDEB00   01/09/13'                                      
         END                                                                    
