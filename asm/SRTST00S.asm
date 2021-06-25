*          DATA SET SRTST00S   AT LEVEL 019 AS OF 10/23/96                      
*PHASE T13600A,+0                                                               
         TITLE '$TEST - DISPLAY/ALTER TEST TABLE'                               
         PRINT NOGEN                                                            
TEST     CSECT                                                                  
         NMOD1 WORKX-WORKD,*$TST**,RA,R9,CLEAR=YES,RR=R4                        
         USING WORKD,RC                                                         
         ST    RD,SAVERD                                                        
         ST    R4,RELO                                                          
         LR    R2,R1                                                            
         USING SRPARMD,R2          R2=A(S/R PARAM LIST)                         
         L     R3,SRPARM6                                                       
         USING SRTSTFFD,R3         R3=A(TWA)                                    
         L     RF,SRQATIA                                                       
         ST    RF,ASAVE            USE TIA AS SAVE AREA                         
*                                                                               
INIT010  L     RF,SRQATIOB         EXTRACT TIOB DATA                            
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID                                                    
*                                                                               
INIT015  SR    RE,RE               PF1 HELP                                     
         ICM   RE,3,TIOBCURD                                                    
         AR    RE,R3                                                            
         CLI   PFKEY,1             TEST HELP PFKEY                              
         BNE   INIT020                                                          
         ST    RE,AHELP            SAVE A(HELP FIELD)                           
         MVI   PFKEY,0                                                          
*                                                                               
INIT020  L     R4,SRPARM4                                                       
         USING COMFACSD,R4         R4=A(COM FAC LIST)                           
         MVC   AHEXIN,CHEXIN                                                    
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   ADATAMGR,CDATAMGR                                                
         MVC   ACALLOV,CCALLOV                                                  
         MVC   AGETTXT,CGETTXT                                                  
         MVC   AGETHELP,CGETHELP                                                
         DROP  R4                                                               
*                                                                               
         L     R4,SRPARM1                                                       
         USING SYSFACD,R4          R4=A(SYS FAC LIST)                           
         MVC   ASSB,VSSB                                                        
         MVC   ATSTTAB,VTSTTAB                                                  
         MVC   AUTLTAB,VUTL                                                     
         DROP  R4                                                               
*                                                                               
         L     RF,SRQAUTL          EXTRACT UTL DATA                             
         ST    RF,AUTL                                                          
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BZ    *+8                                                              
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
         DROP  RF                                                               
*                                                                               
         L     RF,ASSB             EXTRACT SSB DATA                             
         MVC   RECLEN,SSBTWAL-SSBD(RF)                                          
         MVC   FACID(4),SSBSYSN4-SSBD(RF)                                       
         MVC   FACNA(3),SSBSYSNA-SSBD(RF)                                       
*                                                                               
         MVC   MSG,SPACES          CLEAR OUTPUT AREA                            
*                                                                               
         BAS   RE,READSTR          READ SAVED STR                               
*                                                                               
         GOTO1 ADATAMGR,DMCB,=C'DTFADD',TSTRCVR                                 
         L     RF,DMCB+12                                                       
         LA    RF,0(RF)                                                         
         ST    RF,ADTF             GET DTF FOR DADDS CALLS                      
*                                                                               
         BAS   RE,MAIN             CALL MAIN PROGRAM                            
*                                                                               
XMOD1    OC    AHELP,AHELP         USE HELPOUT IF AHELP NOT ZERO                
         BNZ   HELPOUT                                                          
         BAS   RE,WRITESTR         WRITE SAVED STR                              
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         AND SET CURSOR                               
         L     RD,SAVERD           RESET BASE RD                                
         XMOD1 1                   AND EXIT                                     
*                                                                               
EXITEQU  CR    RB,RB                                                            
         B     EXIT                                                             
EXITNEQ  LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        MAIN PROGRAM                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
*                                                                               
         CLI   PFKEY,3             PF3 MEANS EXIT TO LIST                       
         BE    LIST000                                                          
*                                                                               
         CLI   SVACTN,6            ACTION 6 DISPLAY TWAS                        
         BNE   MAIN001                                                          
         CLI   PFKEY,0             ENTER MEANS BACK TO LIST                     
         BE    LIST000                                                          
         MVI   ACTN,6              ELSE RESET ACTION AND ID                     
         MVC   ID,SVTSTID                                                       
         B     MAIN010                                                          
*                                                                               
MAIN001  BAS   RE,VALP1            VALIDATE TEST ID                             
*                                                                               
         CLC   ID,=C'ION '                                                      
         BE    IOON                                                             
         CLC   ID,=C'IOFF'                                                      
         BE    IOOFF                                                            
*                                                                               
         BAS   RE,VALP2            VALIDATE ACTION                              
         CLC   SVACTN,ACTN                                                      
         BE    MAIN010                                                          
         XC    SVDATA,SVDATA                                                    
*                                                                               
MAIN010  MVC   SVACTN,ACTN                                                      
         MVC   SVTSTID,ID                                                       
         CLC   ID,ALL              IF ID=ALL GOTO LIST                          
         BE    LIST000                                                          
*                                                                               
         BAS   RE,FINDTAB          FIND ENTRY FOR USER R5                       
*                                                                               
         CLI   ACTN,5              DISPLAY UPDATE LOG                           
         BE    MAIN090                                                          
         CLI   ACTN,6              DISPLAY TWA LOG                              
         BE    MAIN100                                                          
*                                                                               
         MVI   BYTE,X'FE'          MAKE SURE FE SCREEN IN                       
         BAS   RE,LOADSCRN                                                      
*                                                                               
         CLI   ACTN,2              ADD                                          
         BE    MAIN060                                                          
*                                                                               
         LTR   R5,R5               ENTRY MUST EXIST FOR DIS/CHA/DEL             
         BZ    ERR5                                                             
*                                                                               
         CLI   ACTN,1              DISPLAY                                      
         BE    MAIN050                                                          
         CLI   ACTN,3              CHANGE                                       
         BE    MAIN070                                                          
         CLI   ACTN,4              DELETE                                       
         BE    MAIN080                                                          
         CLI   ACTN,7              RESET                                        
         BE    MAIN085                                                          
         DC    H'0'                                                             
*                                                                               
MAIN050  BAS   RE,DISDATA          DISPLAY AND ENTRY                            
         B     MAINXXX                                                          
*                                                                               
MAIN060  BAS   RE,ADDDATA          ADD AN ENTRY                                 
         L     R5,AFREE                                                         
         BAS   RE,DISDATA          AND DISPLAY IT                               
         B     MAINXXX                                                          
*                                                                               
MAIN070  BAS   RE,SCHA             CHANGE AN ENTRY                              
         BAS   RE,DISDATA                                                       
         B     MAINXXX                                                          
*                                                                               
MAIN080  BAS   RE,DELETE           DELETE AN ENTRY                              
         B     MAINXXX                                                          
*                                                                               
MAIN085  BAS   RE,RESET            RESET AN ENTRY                               
         BAS   RE,DISDATA          AND DISPLAY IT                               
         B     MAINXXX                                                          
*                                                                               
MAIN090  MVI   BYTE,X'FD'          MAKE SURE FD SCREEN IN                       
         BAS   RE,LOADSCRN                                                      
         MVC   SRVPFK,PFKLIN2                                                   
*                                                                               
         BAS   RE,DUPD             DISPLAY UPDATES                              
         B     MAINXXX                                                          
*                                                                               
MAIN100  BAS   RE,TWADISP          DISPLAY TWAS                                 
         B     MAINXXX                                                          
*                                                                               
IOON     L     R6,AUTL                                                          
         USING UTLD,R6                                                          
         OI    TSTAT5,TST5IOCT     IO COUNT ON                                  
         MVI   INFO,7                                                           
         B     MAINXXX                                                          
*                                                                               
IOOFF    L     R6,AUTL                                                          
         USING UTLD,R6                                                          
         NI    TSTAT5,255-TST5IOCT                                              
         MVI   INFO,7                                                           
         DROP  R6                                                               
*                                                                               
MAINXXX  B     INFOX                                                            
         EJECT                                                                  
*************************************************************                   
*        LIST SELECT                                        *                   
*************************************************************                   
         SPACE 1                                                                
LIST000  MVI   BYTE,X'FF'          MAKE SURE FF SCREEN IN                       
         MVI   ACTN,1                                                           
         XC    SVDATA,SVDATA                                                    
*                                                                               
         CLI   PFKEY,4             PF4=TRACE                                    
         BE    GOTRACE                                                          
*                                                                               
         BAS   RE,LOADSCRN                                                      
         BAS   RE,REACT                                                         
         MVC   SRVP1(4),ALL                                                     
         MVC   ID,ALL                                                           
         MVC   SRVFTR,PFKLINE                                                   
*                                                                               
         BAS   RE,LSCREEN          LIST ALL TEST ENTRYS                         
*                                                                               
         BAS   RE,VALSUB           VALIDATE SUB ACTION FIELDS                   
         TM    FLAG,FLGSEL                                                      
         BNO   LISTXXX                                                          
*                                                                               
         TWAXC SRVLINEH,SRVXLINH,PROT=Y                                         
         B     MAIN010             IF SUB ACTIONS CLR SCRN AND PROCESS          
*                                                                               
LISTXXX  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        GOBACKS                                            *                   
*************************************************************                   
         SPACE 1                                                                
GOTRACE  L     R1,AUTL                                                          
         ICM   R6,15,TBUFF-UTLD(R1)                                             
         MVI   0(R6),9             SET LEN                                      
         MVC   1(2,R6),SRVIDH+2    SET ADDR                                     
         MVC   3(6,R6),=C'=TRACE'  SET =TRACE                                   
         MVI   9(R6),0                                                          
         B     GOEXIT                                                           
*                                                                               
GOTFM    GOTO1 AHEXOUT,DMCB,SVTOPDA,DUB,4                                       
         L     R1,AUTL                                                          
         ICM   R6,15,TBUFF-UTLD(R1)                                             
         MVI   0(R6),7             SET LEN                                      
         MVC   1(2,R6),SRVIDH+2    SET ADDR                                     
         MVC   3(4,R6),=C'=TFM'    SET =TFM                                     
         MVI   7(R6),10                                                         
         MVC   8(2,R6),SRVP1H+2    SET ADDR                                     
         MVC   10(7,R6),=C'TSTRCVR'                                             
         MVI   17(R6),13                                                        
         MVC   18(2,R6),SRVP2H+2    SET ADDR                                    
         MVC   20(2,R6),=C'A,'                                                  
         MVC   22(8,R6),DUB                                                     
         MVI   30(R6),0                                                         
*                                                                               
GOEXIT   MVC   SRVID,=C'=GOBACK '                                               
         B     XMOD1                                                            
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        SELECT AND ENTRY                                   *                   
*************************************************************                   
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
SELECT   ST    RE,SAVERE                                                        
         MVC   SRVP1(4),TSTACCS    SET UP FOR DISPLAY                           
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,1                                                           
         BAS   RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
CHANGE   ST    RE,SAVERE                                                        
         MVC   SRVP1(4),TSTACCS    SET UP FOR CHANGE                            
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,3                                                           
         BAS   RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
TRACE    NTR1                                                                   
         BAS   RE,GETUTL                                                        
         USING UTLD,R6                                                          
         XI    TSTAT5,TST5IOTR     SWITCH TRACE FLAG                            
TRACEX   B     EXIT                                                             
*                                                                               
UPDT     XI    TSTFLAGS,TSTFUPDX   SWITCH UPD FLAG                              
         BR    RE                                                               
*                                                                               
TWAS     XI    TSTFLAGS,TSTFSCRQ   SWITCH TWA FLAG                              
         BR    RE                                                               
*                                                                               
RESET    EQU   *                                                                
         XC    TSTCPTY,TSTCPTY     RESET TRACK                                  
         XC    TSTLAST,TSTLAST                                                  
         MVC   TSTLAST(2),TSTLOW                                                
         XC    TSTTRCIO,TSTTRCIO   DEFAULT TRACE # =0                           
         XC    INTVL,INTVL         INIT INTERVAL TO 0                           
         BR    RE                                                               
         SPACE 1                                                                
UUPDT    ST    RE,SAVERE                                                        
         MVC   SRVP1(4),TSTACCS    SET UP FOR DISPLAY                           
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,5                                                           
         BAS   RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
TTWAS    ST    RE,SAVERE                                                        
         MVC   SRVP1(4),TSTACCS    SET UP FOR DISPLAY                           
         MVC   ID(4),TSTACCS                                                    
         MVI   ACTN,6                                                           
         BAS   RE,REACT                                                         
         OI    FLAG,FLGSEL         FLAG GOBACK TO MAIN                          
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 1                                                                
         EJECT                                                                  
*************************************************************                   
*        ADD A NEW ENTRY AT R5                              *                   
*************************************************************                   
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
ADDDATA  NTR1                                                                   
         LTR   R5,R5               ADD USER ENTRY                               
         BNZ   ERR3                ERROR DUPLICATE                              
         L     R5,AFREE                                                         
         LTR   R5,R5                                                            
         BZ    ERR4                ERROR NO SPACE                               
         BAS   RE,GETXTNT                                                       
*                                                                               
         MVC   TSTACCS,ID                                                       
         MVC   TSTLOW,LTRK                                                      
         MVC   TSTHIGH,HTRK                                                     
         MVC   TSTLAST(2),LTRK                                                  
         XC    TSTTRCIO,TSTTRCIO   DEFAULT TRACE # =0                           
         XC    INTVL,INTVL         INIT INTERVAL TO 0                           
*                                                                               
         L     R6,AUTL                                                          
         USING UTLD,R6                                                          
         OI    TSTAT5,TST5IOTR     DEFAULT=IO TRACE ON                          
         MVC   TSTNUM,TNUM                                                      
         DROP  R6                                                               
*                                                                               
         BAS   RE,VALDATA          VALIDATE ENTRY DATA                          
*                                                                               
         MVI   INFO,1                                                           
         CLI   SRVIOIH+5,0                                                      
         BNE   SADD10                                                           
         BAS   RE,SETCNT           RESET TRACE BUFFER                           
         BAS   RE,SETSCT           RESET SCRIPT TRACE BUFFER                    
SADD10   B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        CHANGE ENTRY AT R5                                 *                   
*************************************************************                   
         SPACE 1                                                                
SCHA     NTR1                                                                   
         BAS   RE,GETXTNT                                                       
         BAS   RE,VALDATA          VALIDATE ENTRY DATA                          
         MVI   INFO,2                                                           
         B     EXIT                                                             
         SPACE 1                                                                
*************************************************************                   
*        DELETE ENTRY AT R5                                 *                   
*************************************************************                   
         SPACE 1                                                                
DELETE   NTR1                                                                   
*                                                                               
         MVC   FULL,TSTTRC                                                      
         MVC   FULL1,TSTSCT                                                     
         EX    R6,*+8              CLEAR ENTRY                                  
         B     *+10                                                             
         XC    0(0,R5),0(R5)                                                    
         MVC   TSTTRC,FULL         RESTORE TABLE ADDRESSES                      
         MVC   TSTSCT,FULL1                                                     
*                                                                               
         MVI   INFO,3                                                           
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        LIST ALL TSTTAB ENTRYS                             *                   
*************************************************************                   
         SPACE 1                                                                
LSCREEN  NTR1                                                                   
         MVC   SRVHDR0,HEADER0     DISPLAY TEST TAB LIST                        
         MVC   SRVHDR,HEADER1                                                   
         MVC   SRVHDR1,HEADER2                                                  
         L     R5,ATSTTAB          SET UP BXLE                                  
         LH    R6,0(R5)                                                         
         STH   R6,ENTLEN                                                        
         L     R7,2(R5)                                                         
         LA    R5,6(R5,R6)                                                      
*                                                                               
         LA    R4,SRVSELH          LOOP ROUND TSTTAB                            
         ST    R4,FULL                                                          
LSCR110  OC    0(2,R5),0(R5)                                                    
         BZ    LSCR115                                                          
         MVC   CURSOR,FULL                                                      
         BAS   RE,BUILDL           DISPLAY 1 LINE PER ENTRY                     
         LA    R4,94(R4)                                                        
LSCR115  BXLE  R5,R6,LSCR110                                                    
         B     LSCRXXX                                                          
*                                                                               
LSCRXXX  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD A LINE OF TSTTAB AT R4 R5=A(ENTRY)           *                   
*************************************************************                   
         SPACE 1                                                                
BUILDL   NTR1                                                                   
         USING TSTLINED,R4                                                      
         USING TSTTABD,R5                                                       
         BAS   RE,GETUTL           FIND UTL FOR THS ENTRY                       
         USING UTLD,R6                                                          
*                                                                               
         BAS   RE,GETXTNT                                                       
         EDIT  (B2,ENTRY),(2,TSTNO)                                             
         MVC   TSTTERM,TSYM                                                     
         MVC   TSTUSER,TSTACCS                                                  
*                                                                               
         MVC   TSTLOG,NO           UPDATE LOGS Y/N                              
         TM    TSTFLAGS,TSTFUPDX   TEST INHIBIT FLAG                            
         BNZ   *+10                                                             
         MVC   TSTLOG,YES                                                       
*                                                                               
         GOTO1 AHEXOUT,DMCB,TSTLOW,TSTLOWT,2                                    
         GOTO1 (RF),(R1),TSTHIGH,TSTHIGT,2                                      
         GOTO1 (RF),(R1),TSTLAST,TSTLAT,4                                       
         GOTO1 (RF),(R1),TSTCPTY,TSTCAPT,2                                      
*                                                                               
         MVC   TSTTRAC,NO          IO TRACE Y/N                                 
         TM    TSTAT5,TST5IOTR                                                  
         BZ    *+10                                                             
         MVC   TSTTRAC,YES                                                      
*                                                                               
         MVC   TSTSCR,NO           SCREEN TRACE Y/N                             
         TM    TSTFLAGS,TSTFSCRQ                                                
         BZ    *+10                                                             
         MVC   TSTSCR,YES                                                       
*                                                                               
         LA    RE,TSTPTCH1         RE=A(PATCH ENTRY)                            
         LA    RF,TSTPTCHX         RF=A(END OF PATCH ENTRY)                     
         SR    R1,R1                                                            
BUILD09  OC    0(3,RE),0(RE)       COUNT NUM OF PATCHES                         
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         LA    RE,L'TSTPTCH1(RE)                                                
         CR    RE,RF                                                            
         BL    BUILD09                                                          
         EDIT  (R1),(5,TSTPAT)                                                  
         LTR   R1,R1                                                            
         BNZ   *+10                                                             
         MVC   TSTPAT,NONE                                                      
*                                                                               
         DROP  R4,R5,R6                                                         
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        FIND TSTTAB ENTRY FOR ID                           *                   
*************************************************************                   
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
FINDTAB  XC    AFREE,AFREE         CLEAR FREE ENTRY ADDR                        
         L     R5,ATSTTAB                                                       
         LH    R6,0(R5)                                                         
         STH   R6,ENTLEN           SET UP FOR BXLE                              
         L     R7,2(R5)                                                         
         LA    R5,6(R5,R6)                                                      
*                                                                               
         LA    R4,SRVSELH                                                       
FIND110  OC    0(2,R5),0(R5)       IS ENTRY FREE                                
         BZ    FIND111                                                          
         CLC   TSTACCS,ID          IS IT FOR THIS USER                          
         BER   RE                  YES EXIT R5=A(ENTRY)                         
         B     FIND115             NO BXLE TO NEXT                              
*                                                                               
FIND111  OC    AFREE,AFREE         DO WE HAVE A FREE ONE                        
         BNZ   FIND115                                                          
         ST    R5,AFREE            NO SO SAVE THIS                              
*                                                                               
FIND115  BXLE  R5,R6,FIND110       NEXT                                         
         SR    R5,R5                                                            
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        DISPLAY A TSTTAB ENTRY                             *                   
*************************************************************                   
         SPACE 1                                                                
DISDATA  NTR1                                                                   
         MVC   SRVHDR2,HEADER3                                                  
         MVC   SRVHDR3,HEADER4                                                  
         USING TSTTABD,R5                                                       
         BAS   RE,GETUTL           LOCATE UTL                                   
         USING UTLD,R6                                                          
*                                                                               
         MVC   SRVP1(4),ID         DISPLAY ID                                   
*                                                                               
         MVC   SRVUPD,NO           LOG UPDATES Y/N                              
         TM    TSTFLAGS,TSTFUPDX                                                
         BNZ   *+10                                                             
         MVC   SRVUPD,YES                                                       
*                                                                               
         MVC   SRVIOS,NO           LOG IOS Y/N                                  
         TM    TSTAT5,TST5IOTR                                                  
         BZ    *+10                                                             
         MVC   SRVIOS,YES                                                       
*                                                                               
         OC    TSTTRCIO,TSTTRCIO   IO COUNT Y/N                                 
         BZ    DISP090                                                          
         EDIT  (2,TSTTRCIO),(5,SRVIOS),ALIGN=LEFT                               
*                                                                               
DISP090  GOTO1 AHEXOUT,DMCB,TSTLAST,SRVLDA,4                                    
*                                                                               
         XC    INTVL,INTVL         SHOW INTERVAL                                
         BAS   RE,GETINT                                                        
         EDIT  (2,INTVL+2),(5,SRVIOI),ALIGN=LEFT                                
*                                                                               
         MVC   SRVSCR,NO           TRACE SCREENS Y/N                            
         TM    TSTFLAGS,TSTFSCRQ                                                
         BZ    *+10                                                             
         MVC   SRVSCR,YES                                                       
*                                                                               
         MVC   SRVLIM,TSTTACCS     SHOW LIMIT ACCESS                            
*                                                                               
         LH    R0,NUMPATS          SET MAX NUM OF PATCHES                       
         LA    R4,SRVPAT1H         R4=A(SCR PATCH FIELD HDR)                    
         USING FLDHDRD,R4                                                       
         LA    R6,TSTPTCH1         R6=A(TAB PATCH FIELD)                        
*                                                                               
SDISA    SR    RF,RF               CLEAR SCR PATCH FIELD                        
         IC    RF,FLDLEN                                                        
         SH    RF,=H'9'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         XC    FLDDATA(0),FLDDATA                                               
         OI    FLDOIND,X'80'                                                    
         OC    0(3,R6),0(R6)       IS PATCH DEFINED IN TABLE                    
         BZ    SDISB               NO                                           
*                                                                               
         GOTO1 AHEXOUT,DMCB,(R6),FLDDATA,3,=C'MIX'                              
         MVC   FLDDATA+6(1),FLDDATA                                             
         MVI   FLDDATA,C'T'                                                     
         LA    R7,FLDDATA+6                                                     
         CLI   0(R7),C'0'                                                       
         BE    *+12                                                             
         NI    0(R7),X'CF'         SET TO LEVEL A,B, OR C                       
         LA    R7,1(R7)                                                         
         MVI   0(R7),C'+'                                                       
         LA    R7,1(R7)                                                         
         GOTO1 (RF),(R1),3(R6),(R7),2                                           
         MVI   4(R7),C'='                                                       
         LA    R7,5(R7)                                                         
         SR    R8,R8                                                            
         IC    R8,5(R6)                                                         
         GOTO1 (RF),(R1),6(R6),(R7),(R8)                                        
*                                                                               
SDISB    SR    RF,RF               BUMP TO NEXT SCR PATCH FIELD                 
         IC    RF,FLDLEN                                                        
         AR    R4,RF                                                            
         TM    FLDATB,X'20'                                                     
         BO    SDISB                                                            
         LA    R6,L'TSTPTCH1(R6)                                                
         BCT   R0,SDISA                                                         
*                                                                               
         CLI   INFO,0              DO WE HAVE A MESSAGE                         
         BNE   *+8                                                              
         MVI   INFO,4              NO SET TO TSTTAB ENTRY DISPLAYED             
*                                                                               
         DROP  R5,R6                                                            
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        FIND THE UTL ENTRY FOR THE TERMINAL IN TSTTAB      *                   
*************************************************************                   
         SPACE 1                                                                
         USING TSTTABD,R5                                                       
GETUTL   L     R6,AUTLTAB                                                       
         SR    R1,R1                                                            
         ICM   R1,3,TSTNUM                                                      
         BCTR  R1,0                                                             
         MH    R1,0(R6)                                                         
         LA    R6,6(R6,R1)                                                      
         CLC   TSTNUM,0(R6)        CONFIRM UTL FOUND                            
         BER   RE                                                               
         DC    H'0'                                                             
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        VALIDATE TSTTAB ENTRY SCREEN                       *                   
*************************************************************                   
         SPACE 1                                                                
VALDATA  NTR1                                                                   
         USING FLDHDRD,R4                                                       
         USING TSTTABD,R5                                                       
         BAS   RE,GETUTL                                                        
         USING UTLD,R6                                                          
*                                                                               
VALD2    LA    R4,SRVUPDH          LOG UPDATES = Y OR N                         
         CLI   FLDILEN,0                                                        
         BE    VALD2X              ASSUME UNCHANGED IF NOT INPUT                
         CLI   FLDDATA,C'N'                                                     
         BNE   *+12                                                             
         OI    TSTFLAGS,TSTFUPDX   SET INHIBIT FLAG                             
         B     VALD2X                                                           
         CLI   FLDDATA,C'Y'                                                     
         BNE   ERR1                                                             
         NI    TSTFLAGS,255-TSTFUPDX                                            
VALD2X   DS    0H                                                               
         SPACE 2                                                                
VALD4    LA    R4,SRVLDAH          LAST REC = TTTT                              
         CLI   FLDILEN,0                                                        
         BE    VALD4X              ASSUME UNCHANGED IF NOT INPUT                
         MVC   DUB1(4),TSTLAST     SAVE OLD VALUE                               
         MVC   DUB(8),FLDDATA                                                   
         CLI   FLDILEN,8                                                        
         BNE   ERR1                                                             
VALD4A   GOTO1 AHEXIN,DMCB,DUB,TSTLAST,8                                        
         OC    12(4,R1),12(R1)                                                  
         BZ    ERR1                                                             
*                                                                               
         CLC   TSTLAST(2),LTRK                                                  
         BL    ERR6                                                             
         CLC   TSTLAST(2),HTRK                                                  
         BH    ERR6                                                             
         CLC   DUB1(4),TSTLAST     CAN ONLY SET RECORD NUMBER TO ZERO           
         BE    VALD4X                                                           
         CLI   TSTLAST+3,0                                                      
         BNE   ERR6                                                             
         CLI   TSTLAST+2,0                                                      
         BNE   ERR6                                                             
*                                                                               
VALD4X   DS    0H                                                               
         SPACE 2                                                                
*                                                                               
VALDIO   BAS   RE,GETINT           GET PREVIOUS INTERVAL                        
         CLI   ACTN,2              IF THIS IS AN ADD, INIT INVL=0               
         BNE   *+10                                                             
         XC    INTVL,INTVL         INTERVAL BETWN I/O'S SAVED                   
*                                                                               
         LA    R4,SRVIOSH          IO TRACE HEADER                              
         CLI   SRVIOSH+5,0         ANY INPUT IN IO TRACE PARM?                  
         BE    VALDIO5             NO, SEE IF ANYTHING IN INTERVAL              
         OI    TSTAT5,TST5IOTR     TRACE ON                                     
         CLI   SRVIOS,C'Y'         WAS A YES ENTERED?                           
         BE    VALDIO5             YES, DEFAULT=YES                             
         CLI   SRVIOS,C'N'         IO TRACE OFF?                                
         BNE   VALDIO2             NO, SEE IF NUMERIC INPUT                     
         NI    TSTAT5,X'FF'-TST5IOTR TURN OFF BIT                               
         XC    INTVL,INTVL           CLEAR INTERVAL                             
         XC    TSTTRCIO,TSTTRCIO     DEFAULT TRACE # =0                         
         B     VALDIOX               GO DEAL W/NEXT INPUT PARM                  
*                                                                               
VALDIO2  TM    SRVIOSH+4,X'08'       VALID NUMBERIC?                            
         BNO   ERR10                 NO, ERROR                                  
         CLI   SRVIOSH+5,5           MAX 5 DIGITS ALLOWED                       
         BH    ERR11                 MORE THAN 4 ENTERED                        
         ZIC   RF,SRVIOSH+5          GET LENGTH OF INPUT                        
         BCTR  RF,0                  REDUCE BY 1 FOR EX                         
         EX    RF,*+8                MOVE DIGITS INTO WORK                      
         B     *+10                  BRANCH AROUND THE EX                       
         PACK  DUB,SRVIOS(0)         PACK TO CONVERT TO HEX                     
         CVB   RF,DUB                RF=NUMBER IN HEX                           
         C     RF,=F'32767'          MAX TO FIT IN HALF WORD STRG               
         BH    ERR11                 INVALID INPUT                              
         STH   RF,TSTTRCIO           SAVE IN TRACE TABLE                        
*                                                                               
VALDIO5  LA    R4,SRVIOIH                                                       
         CLI   SRVIOIH+5,0           ANY INPUT                                  
         BE    VALDIOX                                                          
         TM    SRVIOIH+4,X'08'       VALID NUMBERIC?                            
         BNO   ERR10                 NO, ERROR                                  
         ZIC   RF,SRVIOIH+5          GET LENGTH OF INPUT                        
         BCTR  RF,0                  REDUCE BY 1 FOR EX                         
         EX    RF,*+8                MOVE DIGITS INTO WORK                      
         B     *+10                  BRANCH AROUND THE EX                       
         PACK  DUB,SRVIOI(0)         PACK TO CONVERT TO HEX                     
         CVB   RF,DUB                RF=NUMBER IN HEX                           
         ST    RF,INTVL              SAVE INTERVAL                              
*                                                                               
VALDIOX  NI    TSTFLAGS,255-TSTFSCRQ                                            
         CLI   SRVSCR,C'Y'                                                      
         BNE   *+8                                                              
         OI    TSTFLAGS,TSTFSCRQ                                                
*                                                                               
         BAS   RE,SETCNT           RESET COUNTER IN TRACE BUFFER                
         BAS   RE,SETSCT           RESET SCRIPT TRACE BUFFER                    
         LR    RE,RE                                                            
*                                                                               
VALD5    LA    R4,SRVLIMH          ACCESS LIMIT = XXXX                          
         CLI   FLDILEN,0                                                        
         BE    VALD5X              ASSUME UNCHANGED IF NOT INPUT                
         MVC   TSTTACCS,=CL4' '                                                 
         SR    RF,RF                                                            
         IC    RF,FLDILEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TSTTACCS(0),FLDDATA                                              
*                                                                               
VALD5X   DS    0H                                                               
         DROP  R6                                                               
*                                                                               
VALD6    LA    R4,SRVPAT1H         EACH PATCH IS TSPPOOL+DDDD=VVVVVVVV          
         LA    R6,TSTPTCH1                                                      
         LH    R0,NUMPATS                                                       
*                                                                               
VALD6A   XC    0(9,R6),0(R6)       DELETE PATCH                                 
         CLI   FLDILEN,0                                                        
         BE    VALD6F                                                           
*                                                                               
VALD6B   LA    R7,WORK1                                                         
         GOTO1 ASCANNER,DMCB,(R4),(2,(R7)),C',==+'                              
         CLI   4(R1),2                                                          
         BNE   ERR1                                                             
*                                                                               
VALD6C   CLI   0(R7),6             PHASE NAME IS TSPPOO OR TSPPOOX              
         BL    ERR7                                                             
         CLI   0(R7),7                                                          
         BH    ERR7                                                             
         CLI   12(R7),C'T'                                                      
         BNE   ERR7                                                             
         MVC   12(1,R7),18(R7)                                                  
         CLI   12(R7),C' '                                                      
         BNE   *+8                                                              
         MVI   12(R7),X'C0'        SET LEVEL ZERO IF NOT INPUT                  
         CLI   12(R7),X'C0'                                                     
         BL    ERR7                                                             
         CLI   12(R7),X'C3'        LEVEL MUST BE A,B, OR C                      
         BH    ERR7                                                             
         OI    12(R7),X'F0'                                                     
         GOTO1 AHEXIN,DMCB,12(R7),0(R6),6                                       
         OC    12(4,R1),12(R1)                                                  
         BZ    ERR7                                                             
*                                                                               
VALD6D   MVC   DUB,=4C'0'          DISPLACEMENT IS 1 THRU 4 HEX CHRS            
         CLI   1(R7),1                                                          
         BL    ERR8                                                             
         CLI   1(R7),4                                                          
         BH    ERR8                                                             
         SR    R1,R1               RIGHT JUSTIFY DISPLACEMENT                   
         IC    R1,1(R7)                                                         
         LA    RF,4                                                             
         SR    RF,R1                                                            
         LA    RF,DUB(RF)                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),22(R7)                                                   
         GOTO1 AHEXIN,DMCB,DUB,3(R6),4                                          
         OC    12(4,R1),12(R1)                                                  
         BZ    ERR8                                                             
*                                                                               
VALD6E   LA    R7,32(R7)           PATCH VALUE IS 2 THRU 32 HEX CHRS            
         CLI   1(R7),0                                                          
         BNE   ERR9                                                             
         CLI   0(R7),32                                                         
         BH    ERR9                                                             
         SR    R8,R8                                                            
         IC    R8,0(R7)                                                         
         GOTO1 AHEXIN,DMCB,12(R7),6(R6),(R8)                                    
         OC    12(4,R1),12(R1)                                                  
         BZ    ERR9                                                             
         MVC   5(1,R6),15(R1)      SET L'PATCH                                  
*                                                                               
VALD6F   SR    RF,RF               BUMP TO NEXT SCR PATCH FIELD                 
         IC    RF,FLDLEN                                                        
         AR    R4,RF                                                            
         TM    FLDATB,X'20'                                                     
         BO    VALD6F                                                           
         LA    R6,L'TSTPTCH1(R6)                                                
         BCT   R0,VALD6A                                                        
*                                                                               
VALDX    B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE SUBACTION FIELDS                          *                   
*************************************************************                   
         SPACE 1                                                                
VALSUB   NTR1                                                                   
         MVI   FLAG,0                                                           
         L     R5,ATSTTAB          SET UP BXLE                                  
         LH    R6,0(R5)                                                         
         STH   R6,ENTLEN                                                        
         L     R7,2(R5)                                                         
         LA    R5,6(R5,R6)                                                      
*                                                                               
         LA    R4,SRVSELH          LOOP ROUND TSTTAB                            
VALS110  CLI   8(R4),C'?'                                                       
         BNE   *+8                                                              
         ST    R4,AHELP                                                         
         OC    0(2,R5),0(R5)                                                    
         BZ    VALS115                                                          
         CLI   5(R4),0             1 LINE PER ENTRY                             
         BE    VALS112                                                          
         B     VALS200             GO SEE WHAT THE INPUT IS                     
*                                                                               
VALS112  LA    R4,94(R4)                                                        
VALS115  BXLE  R5,R6,VALS110                                                    
         B     VALSXXX                                                          
*                                                                               
VALS200  LA    RF,SELTAB                                                        
         SR    R1,R1                                                            
         IC    R1,5(R4)                                                         
         MVI   5(R4),0             ONCE ONLY PLEASE                             
         BCTR  R1,0                                                             
VALS210  EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),8(R4)                                                    
         BE    VALS250                                                          
         LA    RF,8(RF)                                                         
         CLI   0(RF),0                                                          
         BNE   VALS210                                                          
         B     ERR12                                                            
*                                                                               
VALS250  ST    R4,CURSOR                                                        
         MVC   8(3,R4),SPACES      CLEAR INPUT FIELD                            
         MVI   FLAG,FLGSEL                                                      
         ICM   RF,15,4(RF)                                                      
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     VALS112                                                          
*                                                                               
VALSXXX  B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE P1 TEST ID                                *                   
*************************************************************                   
         SPACE 1                                                                
VALP1    LA    R4,SRVP1H                                                        
         ST    R4,CURSOR                                                        
         CLI   8(R4),C'?'                                                       
         BNE   *+8                                                              
         ST    R4,AHELP                                                         
         CLI   SRVP1H+5,0          ZERO MEANS ALL                               
         BE    VALP1A                                                           
         BNE   VALP1X                                                           
VALP1A   MVC   ID,ALL                                                           
         MVC   SRVP1(4),ALL                                                     
         BR    RE                                                               
VALP1X   MVC   ID,SRVP1            TAKE ID FROM INPUT FIELD                     
         OC    ID,SPACES                                                        
         BR    RE                                                               
         SPACE 1                                                                
*************************************************************                   
*        VALIDATE P2 ACTION                                 *                   
*************************************************************                   
         SPACE 1                                                                
VALP2    LA    R4,SRVP2H                                                        
         CLI   8(R4),C'?'                                                       
         BNE   *+8                                                              
         ST    R4,AHELP                                                         
         LA    RF,ACTNTBL          RF=A(TABLE)                                  
         SR    R1,R1                                                            
         ICM   R1,1,SRVP2H+5       R1=INPUT LEN                                 
         BZ    VALP2X                                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
VALP2A   CLC   SRVP2(0),0(RF)      LOOK FOR A MATCH                             
         BE    VALP2X                                                           
         LA    RF,8(RF)            TRY NEXT ENTRY                               
         CLI   0(RF),X'FF'                                                      
         BNE   VALP2A                                                           
         B     ERR2                END OF TABLE                                 
*                                                                               
VALP2X   MVC   ACTN,7(RF)          SAVE ACTION VALUE AND EXIT                   
         MVC   SRVP2(7),0(RF)                                                   
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        LOAD SCREEN BYTE=SCREEN                            *                   
*************************************************************                   
         SPACE 1                                                                
LOADSCRN NTR1                                                                   
         L     RF,AUTL             CHECK FOR CURRENT                            
         SR    R1,R1                                                            
         IC    R1,TSVCREQ-UTLD(RF)                                              
         SRL   R1,4                                                             
         STC   R1,BYTE1                                                         
         OI    BYTE1,X'F0'                                                      
         CLC   BYTE1,BYTE          EXIT IF SAME                                 
         BE    EXIT                                                             
*                                                                               
         MVI   DMCB+4,C'R'         LOAD NEW ROOT SCREEN                         
         MVC   DMCB+5(2),=X'0136'                                               
         MVC   DMCB+7(1),BYTE                                                   
         GOTO1 ACALLOV,DMCB,SCREEN,,0                                           
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                BAD RETURN FROM CALLOV                       
         IC    R1,BYTE                                                          
         SLL   R1,4                                                             
         STC   R1,BYTE1            SET TSVCREQ TO NEW ROOT                      
         L     R1,AUTL                                                          
         NI    TSVCREQ-UTLD(R1),X'0F'                                           
         OC    TSVCREQ-UTLD(1,R1),BYTE1                                         
*                                                                               
         MVC   SRVID(5),=C'=TEST'  RESET FIELDS                                 
         OI    SRVIDH+6,X'80'                                                   
         MVC   SRVP1(4),ID                                                      
         OI    SRVP1H+6,X'80'                                                   
*                                                                               
         BAS   RE,REACT            RESTORE ACTION                               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        RE DISPLAY THE ACTION FIELD                        *                   
*************************************************************                   
         SPACE 1                                                                
REACT    LA    RF,ACTNTBL          RESET ACTION FIELD                           
RESACT0  CLC   ACTN,7(RF)                                                       
         BE    RESACT1                                                          
         LA    RF,8(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   RESACT0                                                          
         LA    RF,ACTNTBL                                                       
RESACT1  MVC   SRVP2(7),0(RF)                                                   
         OI    SRVP2H+6,X'80'                                                   
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        DISPLAY TWA LOG FROM TSTRCVR                       *                   
*************************************************************                   
         SPACE 1                                                                
TWADISP  NTR1                                                                   
         USING TSTTABD,R5                                                       
TWAD10   MVC   DA(2),TSTLOW        START FROM LOW+0100                          
         MVC   DA+2(2),=X'0100'                                                 
         MVI   READ,C'T'                                                        
*                                                                               
         CLI   PFKEY,8             WAS DOWN PFKEY PRESSED                       
         BNE   TWAD20                                                           
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
         MVI   FIRST,C'Y'          READ THIS TWA                                
         BAS   RE,RDRCV                                                         
         BE    TWAD20                                                           
         BNE   INFO5               SHOULD NOT HAPPEN                            
*                                                                               
TWAD20   CLI   PFKEY,7             WAS UP PFKEY PRESSED                         
         BNE   TWAD50                                                           
*                                                                               
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
TWAD21   OI    FLAG,FLGBACK                                                     
         MVI   FIRST,C'Y'          BACK ONE RECORD                              
         CLI   DA+2,1                                                           
         BE    TWAD22              BACK ONE TRACK                               
*                                                                               
TWAD21A  IC    R1,DA+2             JUST SUB ONE FROM RECORD                     
         BCTR  R1,0                                                             
         STC   R1,DA+2                                                          
         B     TWAD30                                                           
*                                                                               
TWAD22   CLC   DA(2),TSTLOW        ARE WE GOING OFF THE TOP                     
         BE    TWAD50                                                           
*                                                                               
         GOTO1 ADATAMGR,DMCB,DADDS,DABACK,IOAREA,0,ADTF,DA                      
         MVI   FIRST,C'Y'                                                       
         B     TWAD21A             BACK ONE TRACK                               
*                                                                               
TWAD30   BAS   RE,RDRCV                                                         
         BNE   TWAD21                                                           
         NI    FLAG,255-FLGBACK                                                 
         B     TWAD55                                                           
*                                                                               
TWAD50   MVI   READ,C'T'           READ FIRST/NEXT TWA                          
         BAS   RE,RDRCV                                                         
         BNE   INFO5                                                            
*                                                                               
TWAD55   GOTO1 AHEXOUT,DMCB,DA,FOOTER+28,4                                      
         LA    RE,IOAREA+64                                                     
         LA    R0,SRVMSGH                                                       
         LH    R1,TSTLEN                                                        
         CH    R1,=H'4096'                                                      
         BL    *+6                                                              
         DC    H'0'                MAX TWA LEN 4K                               
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         LR    R1,R0                                                            
         XC    0(3,R1),0(R1)                                                    
         MVC   SRVID(17),=CL17'=TEST'  RESET ID FIELD                           
         MVI   SRVIDH+5,5                                                       
         OI    SRVIDH+6,X'81'                                                   
         LA    R1,SRVIDH                                                        
         ST    R1,CURSOR                                                        
*                                                                               
         LA    RE,SRVMSGH                                                       
TWAD60   OI    6(RE),X'80'         SET ALL THE XMIT BITS ON                     
         CLC   2(2,RE),FOOTER+2                                                 
         BNL   TWADX                                                            
         SR    R0,R0                                                            
         ICM   R0,1,0(RE)                                                       
         BZ    TWADX                                                            
         AR    RE,R0                                                            
         B     TWAD60                                                           
*                                                                               
         B     TWADX                                                            
*                                                                               
TWAD90   MVI   PFKEY,0                                                          
         B     TWAD10                                                           
*                                                                               
TWADX    MVC   0(87,RE),FOOTER                                                  
         XC    87(3,RE),87(RE)                                                  
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY UPDATE LOG FROM TSTRCVR                    *                   
*************************************************************                   
         SPACE 1                                                                
DUPD     NTR1                                                                   
         USING TSTTABD,R5                                                       
DUPD10   MVC   DA(2),TSTLOW        START FROM LOW+0100                          
         MVC   DA+2(2),=X'0100'                                                 
         OC    SVRECDA,SVRECDA     TEST FIRST TIME                              
         BZ    DUPD40                                                           
         OC    SVTOPDA,SVTOPDA                                                  
         BNZ   *+10                                                             
         MVC   SVTOPDA,DA                                                       
*                                                                               
         CLI   PFKEY,4             RF4=TFM                                      
         BE    GOTFM                                                            
*                                                                               
         CLI   PFKEY,0             WAS ENTER PRESSED                            
         BNE   DUPD15                                                           
         XC    SVRECDS,SVRECDS     RESET TO ZERO DISP                           
         MVC   SVRECDA,SVTOPDA                                                  
         MVC   DA,SVTOPDA          TOP RECORD                                   
*                                                                               
DUPD15   CLI   PFKEY,8             WAS DOWN PFKEY PRESSED                       
         BNE   *+10                                                             
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
*                                                                               
         CLI   PFKEY,7             WAS UP PFKEY PRESSED                         
         BNE   DUPD40                                                           
*                                                                               
         OI    FLAG,FLGBACK                                                     
         MVC   SVRECDA,SVTOPDA     RESTORE TOP DISP                             
         MVC   SVRECDS,SVTOPDS                                                  
         MVC   DA,SVRECDA          RESTORE PREVIOUS DA                          
*                                                                               
         LA    R2,19               19 SCREEN LINES                              
DUPD20   SR    R1,R1                                                            
         ICM   R1,3,SVRECDS        TRACK BACKWARDS                              
         BZ    DUPD21                                                           
         SH    R1,=H'20'           20 BYTES PER LINE                            
         BM    DUPD21                                                           
         STH   R1,SVRECDS                                                       
         BCT   R2,DUPD20           NEXT LINE                                    
         B     DUPD40                                                           
*                                                                               
DUPD21   MVI   FIRST,C'Y'          BACK ONE RECORD                              
         CLI   DA+2,1                                                           
         BE    DUPD22              BACK ONE TRACK                               
*                                                                               
DUPD21A  IC    R1,DA+2             JUST SUB ONE FROM RECORD                     
         BCTR  R1,0                                                             
         STC   R1,DA+2                                                          
         B     DUPD23                                                           
*                                                                               
DUPD22   CLC   DA(2),TSTLOW        ARE WE GOING OFF THE TOP                     
         BNE   DUPD22A                                                          
         MVI   PFKEY,0             RESET TO START                               
         NI    FLAG,255-FLGBACK                                                 
         B     DUPD10                                                           
*                                                                               
DUPD22A  GOTO1 ADATAMGR,DMCB,DADDS,DABACK,IOAREA,0,ADTF,DA                      
         MVI   FIRST,C'Y'                                                       
         B     DUPD21A             BACK ONE TRACK                               
*                                                                               
DUPD23   BAS   RE,RDRCV            READ THE RECORD                              
         BNE   DUPD21                                                           
         SR    R0,R0                                                            
         NI    FLAG,255-FLGBACK                                                 
         LH    R1,TSTLEN                                                        
         LA    R1,20(R1)           ADD 20 FOR BLANK LINE                        
         D     R0,=F'20'           CALCULATE END DISP                           
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)            ADD ONE IF ANY REMAINDER                     
         MH    R1,=H'20'                                                        
         STH   R1,SVRECDS          SAVE END DISP                                
         B     DUPD20                                                           
*                                                                               
DUPD40   MVC   SVTOPDA,SVRECDA     SAVE NEW TOP OF SCREEN                       
         MVC   SVTOPDS,SVRECDS                                                  
         LA    R4,SRVLI1                                                        
         MVI   READ,C'R'           SET RECOVERY                                 
         MVI   FIRST,C'Y'          SET READ DA                                  
*                                                                               
DUPD50   BAS   RE,RDRCV            READ FIRST/NEXT RECORD                       
         BNE   INFO6                                                            
*                                                                               
         BAS   RE,BLDREC           BUILD A SCREEN                               
         ICM   R4,15,FULL                                                       
         BZ    DUPDX                                                            
         B     DUPD50              IF MORE SCREEN LEFT GET NEXT                 
*                                                                               
DUPDX    B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        READ FIRST/NEXT TWA OR RECOVERY REC                *                   
*************************************************************                   
         SPACE 1                                                                
RDRCV    NTR1                                                                   
*                                                                               
RDRCV1   LA    R1,DMRSEQ           SET READ TYPE                                
         CLI   FIRST,C'N'                                                       
         BE    *+12                                                             
         LA    R1,DMRDIR           IF FIRST=Y DMRDIR                            
         B     *+10                                                             
         XC    SVRECDS,SVRECDS     SET DISP TO 0                                
         ST    R1,DMCB                                                          
         MVI   FIRST,C'N'                                                       
*                                                                               
         OC    DA(2),DA                                                         
         BZ    EXITNEQ                                                          
*                                                                               
         GOTO1 ADATAMGR,DMCB,,TSTRCVR,DA,IOAREA,0                               
         TM    8(R1),X'80'                                                      
         BO    RDRCV9              EOF                                          
         TM    8(R1),X'10'                                                      
         BO    RDRCV9              NOT FOUND RETURN EOF                         
         CLI   8(R1),0                                                          
         BNE   RDRCV9                                                           
         MVC   TSTLEN,18(R1)       EXTRACT RECORD LEN                           
*                                                                               
         CLC   DA,TSTLAST          EOF                                          
         BH    RDRCV9                                                           
*                                                                               
         CLI   READ,C'T'           ARE WE READING TWAS                          
         BNE   RDRCV2                                                           
         CLI   IOAREA,0            NOT A TWA SO RSEQ                            
         BNE   RDRCVN                                                           
         MVC   SVRECDA,DA          SAVE DA                                      
         LH    R1,TSTLEN                                                        
         SH    R1,=H'64'           SUBTRACT HEADER LEN                          
         STH   R1,TSTLEN                                                        
         B     EXITEQU                                                          
*                                                                               
RDRCV2   CLI   IOAREA,0            IS A TWA SO RSEQ                             
         BE    RDRCVN                                                           
         MVC   SVRECDA,DA          SAVE DA                                      
         LH    R1,TSTLEN                                                        
         SH    R1,=H'24'           SUBTRACT HEADER LEN                          
         STH   R1,TSTLEN                                                        
         B     EXITEQU                                                          
*                                                                               
RDRCV9   B     EXITNEQ                                                          
*                                                                               
RDRCVN   TM    FLAG,FLGBACK        READ NEXT UNLESS BACKWARDS                   
         BO    EXITNEQ                                                          
         B     RDRCV1                                                           
         EJECT                                                                  
*************************************************************                   
*        BUILD RECOVERY RECORD                              *                   
*************************************************************                   
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         LA    R6,FILTAB                                                        
         USING FILTABD,R6                                                       
         MVI   BYTE,1              SET FIRST LINE OF RECORD                     
         LA    R7,IOAREA                                                        
         SR    R1,R1                                                            
         IC    R1,0(R7)            LOCATE FILE NEAME IT TABLE                   
         SLL   R1,5                                                             
         AR    R6,R1                                                            
         MVC   0(7,R4),DMFLNAME    DISPLAY FILE NAME                            
*                                                                               
         CLI   1(R7),1             IS THIS COPY/CHANGE OR ADD                   
         BNE   *+10                                                             
         MVC   8(4,R4),=C'COPY'                                                 
         CLI   1(R7),2                                                          
         BNE   *+10                                                             
         MVC   8(6,R4),=C'CHANGE'                                               
         CLI   1(R7),3                                                          
         BNE   *+10                                                             
         MVC   8(3,R4),=C'ADD'                                                  
*                                                                               
         SR    R2,R2               R2=RECORD LEN + 20 FOR BLANK LINE            
         ICM   R2,3,TSTLEN                                                      
         LA    R2,20(R2)                                                        
*                                                                               
BLDR010  CLI   BYTE,2              SECOND LINE GETS DISK ADDR                   
         BNE   BLDR019                                                          
*                                                                               
         MVC   0(10,R4),=C'(........)'                                          
         GOTO1 AHEXOUT,DMCB,DA,1(R4),4                                          
*                                                                               
BLDR019  STH   R2,HALF             SAVE LENGTH IN HALF                          
         CH    R2,=H'20'                                                        
         BL    *+8                                                              
         LA    R2,20               IF REMAINING LEN > 20 DISP 20                
*                                                                               
         CLC   HALF,=H'40'         IF REMAINING LEN < 40 DISP HALF-20           
         BNL   BLDR020                                                          
         LH    R2,HALF                                                          
         SH    R2,=H'20'                                                        
         BNP   BLDR030             IF THIS IS < 1 ITS A BLANK LINE              
*                                                                               
BLDR020  GOTO1 AHEXOUT,DMCB,24(R7),16(R4),(R2)                                  
         LR    R1,R2                                                            
         BCTR  R1,0                                                             
         EX    R1,*+12             SHOW CHARS AND TRANSLATE OUT FUNNYS          
         EX    R1,*+14                                                          
         B     *+16                                                             
         MVC   58(0,R4),24(R7)                                                  
         TR    58(0,R4),TTABLE                                                  
*                                                                               
         CLI   BYTE,3              THIRD + LINE GETS DISPLACEMENTS              
         BL    BLDR030                                                          
         EDIT  (B2,SVRECDS),(4,10(R4)),FILL=0                                   
*                                                                               
BLDR030  LA    R7,20(R7)           BUMP DOWN RECORD                             
         LR    R1,R7                                                            
         LA    R0,IOAREA                                                        
         SR    R1,R0               R1=CURRENT DISPLACEMENT                      
         CH    R1,SVRECDS                                                       
         BNH   BLDR050             SKIP THIS LINE IF NOT UP TO SVDISP           
         STCM  R1,3,SVRECDS                                                     
*                                                                               
BLDR040  LA    R4,78+8(R4)         NEXT LINE                                    
         LA    R1,SRVPFK                                                        
         CR    R4,R1               TEST END OF SCREEN                           
         BL    *+10                                                             
         SR    R4,R4               SET R4 TO ZERO IF END                        
         B     BLDRECX                                                          
*                                                                               
BLDR050  MVC   0(78,R4),SPACES     CLEAR NEXT LINE                              
         IC    R1,BYTE                                                          
         LA    R1,1(R1)            BUMP LINE IN REC COUNT                       
         STC   R1,BYTE                                                          
*                                                                               
         LH    R1,HALF             SUB 20 FROM REMAINING                        
         SH    R1,=H'20'                                                        
         LR    R2,R1                                                            
         BP    BLDR010             STILL MORE TO DISPLAY                        
*                                                                               
         DROP  R6                                                               
BLDRECX  ST    R4,FULL             EXIT R4 SHOWS MORE SCREEN AVAILABLE          
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*        READ IN SAVED STORAGE                              *                   
*************************************************************                   
         SPACE 1                                                                
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
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),TSTID   TEST FOR MY ID                               
         BNE   READX                                                            
         LA    R1,SRCOMWRK                                                      
         MVC   SAVEDSTR(SAVEDL),4(R1)                                           
READX    B     EXIT                                                             
         SPACE 1                                                                
*************************************************************                   
*        WRITE OUT SAVED STORAGE                            *                   
*************************************************************                   
         SPACE 1                                                                
WRITESTR NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R1,SRCOMWRK                                                      
         MVC   0(4,R1),TSTID                                                    
         MVC   4(SAVEDL,R1),SAVEDSTR                                            
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 ADATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        ERROR EXITS                                        *                   
*************************************************************                   
         SPACE 1                                                                
ERR0     MVC   MSG(17),=C'Missing parameter'                                    
         B     ERRX                                                             
ERR1     MVC   MSG(17),=C'Invalid parameter'                                    
         B     ERRX                                                             
ERR2     MVC   MSG(14),=C'Invalid action'                                       
         B     ERRX                                                             
ERR3     MVC   MSG(15),=C'Duplicate entry'                                      
         B     ERRX                                                             
ERR4     MVC   MSG(18),=C'No space in TSTTAB'                                   
         B     ERRX                                                             
ERR5     MVC   MSG(15),=C'Entry not found'                                      
         B     ERRX                                                             
ERR6     MVC   MSG(20),=C'Invalid disk address'                                 
         B     ERRX                                                             
ERR7     MVC   MSG(18),=C'Invalid phase name'                                   
         B     ERRX                                                             
ERR8     MVC   MSG(20),=C'Invalid displacenent'                                 
         B     ERRX                                                             
ERR9     MVC   MSG(18),=C'Invalid patch data'                                   
         B     ERRX                                                             
ERR10    MVC   MSG(28),=C'Invalid input. Y/N/DEC# ONLY'                         
         B     ERRX                                                             
ERR11    MVC   MSG(25),=C'Invalid number. MAX=32767'                            
         B     ERRX                                                             
ERR12    MVC   MSG(18),=C'Invalid sub action'                                   
         B     ERRX                                                             
*                                                                               
ERRX     MVC   SRVMSG(13),=C'***ERR (....)'                                     
         MVC   SRVMSG+8(4),FACNA                                                
         MVC   SRVMSG+13(47),MSG                                                
         OI    SRVMSGH+6,X'80'                                                  
         OI    6(R4),X'40'         POSN CURSOR ON INVALID FIELD                 
*                                                                               
         B     XMOD1                                                            
         EJECT                                                                  
*************************************************************                   
*        INFO EXITS                                         *                   
*************************************************************                   
         SPACE 1                                                                
INFOX    CLI   INFO,0                                                           
         BE    XMOD1                                                            
*                                                                               
         CLI   INFO,1                                                           
         BNE   *+10                                                             
         MVC   MSG(18),=C'TSTTAB Entry added'                                   
         CLI   INFO,2                                                           
         BNE   *+10                                                             
         MVC   MSG(20),=C'TSTTAB Entry changed'                                 
         CLI   INFO,3                                                           
         BNE   *+10                                                             
         MVC   MSG(20),=C'TSTTAB Entry deleted'                                 
         CLI   INFO,4                                                           
         BNE   *+10                                                             
         MVC   MSG(22),=C'TSTTAB Entry displayed'                               
         CLI   INFO,7                                                           
         BNE   *+10                                                             
         MVC   MSG(24),=C'I/O Count status changed'                             
         B     INFOXX                                                           
*                                                                               
INFO5    MVC   MSG(18),=C'End of twa log    '                                   
         XC    SRVP1,SRVP1                                                      
         MVC   SRVP1(4),ID                                                      
         MVI   ACTN,1                                                           
         BAS   RE,REACT                                                         
         LA    R1,SRVP1H                                                        
         ST    R1,CURSOR                                                        
         B     INFOXX                                                           
*                                                                               
INFO6    MVC   MSG(18),=C'End of update log '                                   
         B     INFOXX                                                           
*                                                                               
INFOXX   MVC   SRVMSG(8),=C'(....)  '                                           
         MVC   SRVMSG+1(4),FACNA                                                
         MVC   SRVMSG+8(52),MSG                                                 
         OI    SRVMSGH+6,X'80'                                                  
*                                                                               
         B     XMOD1                                                            
         EJECT                                                                  
*************************************************************                   
*        CALCULATE ENTRY/LTRK/HTRK FROM R5=A(ENTRY)         *                   
*************************************************************                   
         SPACE 1                                                                
GETXTNT  SR    R0,R0               GET ENTRY DATA FROM TSTTAB AT R5             
         LR    R1,R5                                                            
         L     RF,ATSTTAB                                                       
         SR    R1,RF               R1=DISPLACEMENT                              
         LH    RF,0(RF)            RF=WIDTH                                     
         DR    R0,RF                                                            
         STH   R1,ENTRY            R1=ENTRY NUMBER                              
         L     RF,ATSTTAB                                                       
         MH    R1,6(RF)            MULT BY TRACKS PER CI                        
         LA    R1,1(R1)                                                         
         STH   R1,LTRK             R1=LOW TRACK NUMBER                          
         AH    R1,6(RF)                                                         
         BCTR  R1,0                                                             
         STH   R1,HTRK             R1=HIGH TRACK NUMBER                         
         BR    RE                                                               
         EJECT                                                                  
*********************************************************************           
* SETCNT- INNITIALIZE COUNTER OF # RECORDS READ TO 0 IN TRACE BUFFER            
*********************************************************************           
         SPACE 1                                                                
SETCNT   NTR1                                                                   
         L     R1,ASSB                                                          
         USING SSBD,R1                                                          
         TM    SSBSYSFL,X'80'        TST/MEL SYSTEM?                            
         BZ    EXIT                                                             
         DROP  R1                                                               
*                                    NO->NO TRACE BUFFER                        
         USING TSTTABD,R5                                                       
         ICM   R6,15,TSTTRC          GET ADDRESS OF TRACE TABLE                 
         L     R5,INTVL                                                         
*                                                                               
         LA    RE,SET10              GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
SET10    DS    0H                                                               
*                                                                               
         XC    36(4,R6),36(R6)       ZERO THE TRACE COUNT                       
         STCM  R5,15,28(R6)                                                     
*                                                                               
SET20    LA    RE,SET30              GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
SET30    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* SETSCT - RESET VALUES IN SCRIPT TRACE BUFFER                      *           
*********************************************************************           
         SPACE 1                                                                
SETSCT   NTR1                                                                   
*                                                                               
         USING TSTTABD,R5                                                       
         ICM   R6,15,TSTSCT          GET ADDRESS OF SCRIPT TRACE TABLE          
         BZ    SSCT100                                                          
         USING SCTTABD,R6                                                       
*                                                                               
         LA    RE,SSCT010            GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
SSCT010  DS    0H                                                               
*                                                                               
         XC    SCTCALL,SCTCALL                                                  
         XC    SCTCOUNT,SCTCOUNT                                                
         XC    SCTSIN,SCTSIN                                                    
         XC    SCTNEXT,SCTNEXT                                                  
         XC    SCTSERR,SCTSERR                                                  
         XC    SCTSDSP,SCTSDSP                                                  
         XC    SCTSOFF,SCTSOFF                                                  
         XC    SCTSOUTL,SCTSOUTL                                                
         XC    SCTSTA1,SCTSTA1                                                  
         XC    SCTRD,SCTRD                                                      
*                                                                               
SSCT020  LA    RE,SSCT100            GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
SSCT100  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**************************************************************                  
*        GETINT- READ OFF INTERVAL FROM TRACE BUFFER         *                  
**************************************************************                  
         SPACE 1                                                                
GETINT   NTR1                                                                   
         L     R1,ASSB                                                          
         USING SSBD,R1                                                          
         TM    SSBSYSFL,X'80'        TST/MEL SYSTEM?                            
         BZ    EXIT                                                             
         DROP  R1                                                               
         ICM   R6,15,TSTTRC          GET ADDRESS OF TRACE TABLE                 
*                                                                               
         LA    RE,GET10              GET INTO 31-BIT MODE                       
         O     RE,=X'80000000'       TURN ON HIGH ORDER BIT                     
         BSM   0,RE                  SET THE MODE                               
GET10    DS    0H                                                               
*                                                                               
         MVC   INTVL,28(R6)          GET INTERVAL                               
*                                                                               
GET20    LA    RE,GET30              GET OUT OF 31-BIT MODE                     
         BSM   0,RE                  TURN OFF 31-BIT MODE                       
GET30    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
******************************************************                          
*        CALL GETHELP AND EXIT TO MONITOR            *                          
******************************************************                          
         SPACE 1                                                                
HELPOUT  L     R1,AHELP                                                         
         OI    6(R1),X'40'         SET CURSOR                                   
         LR    R0,R1                                                            
         MVC   HELPKEY,HELPID                                                   
*                                                                               
         LA    RE,HELPTAB          FIND WHICH PANEL                             
         SR    RF,RF                                                            
         LA    RF,1                                                             
HELP010  EX    0,0(RE)             BY TESTING AHELP                             
         CR    R1,R0                                                            
         BE    HELP020                                                          
         LA    RE,4(RE)                                                         
         CLI   0(RE),0                                                          
         BE    HELP020                                                          
         LA    RF,1(RF)                                                         
         B     HELP010                                                          
*                                                                               
HELP020  STC   RF,HELPNUM                                                       
         XC    DMCB(24),DMCB                                                    
         GOTO1 AGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0)                         
         DC    H'0'                                                             
HELPTAB  LA    R1,SRVP1H           POSSIBLE HELP FIELDS                         
         LA    R1,SRVP2H                                                        
         LA    R1,SRVP3H                                                        
         LA    R1,SRVP4H                                                        
         LA    R1,SRVSELH          <--- OR GREATER                              
         DC    H'0'                                                             
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS HEADERS TABLES & LTORG                   *                   
*************************************************************                   
*                                                                               
*              C'....+....1....+....2....+....3....+....4'                      
*              C'....+....5....+....6....+....7....+....8'                      
*                                                                               
HEADER0  DC    C'                     - RCVR Disk Addr - '                      
         DC    C'Trk  --- Traces ---                     '                      
HEADER1  DC    C'Sel No Terminal User Low  High Last     '                      
         DC    C'Left Updt I/Os Twas Patch               '                      
HEADER2  DC    C'--- -- -------- ---- ---- ---- -------- '                      
         DC    C'---- ---- ---- ---- -----               '                      
HEADER3  DC    C'PATCHES  PHASE+OFFS=DATA                '                      
         DC    C'                                        '                      
HEADER4  DC    C'----------------------------------------'                      
         DC    C'--------                                '                      
PFKLINE  DC    C'PF1=Help PF3=Return PF4=Trace PF7=Up PF8'                      
         DC    C'=Down PF12=Exit                         '                      
PFKLIN2  DC    C'PF1=Help PF3=Return PF4=TFM PF7=Up PF8=D'                      
         DC    C'own Enter=Align PF12=Exit               '                      
*                                                                               
TSTID    DC    C'$TST'                                                          
ALL      DC    C'ALL  '                                                         
YES      DC    C'Yes  '                                                         
NO       DC    C'No   '                                                         
NONE     DC    C'None '                                                         
*                                                                               
NUMPATS  DC    H'5'                NUMBER OF PATCHES IN TSTTAB ENTRY            
*                                                                               
HELPID   DC    XL10'0136FF00010000000000'                                       
*                                                                               
DMREAD   DC    CL8'DMREAD'                                                      
DMRDIR   DC    CL8'DMRDIR'                                                      
DMRSEQ   DC    CL8'DMRSEQ'                                                      
DMWRT    DC    CL8'DMWRT'                                                       
TEMPSTR  DC    CL8'TEMPSTR'                                                     
TSTRCVR  DC    CL8'TSTRCVR'                                                     
DADDS    DC    CL8'DADDS'                                                       
SPACES   DC    80C' '                                                           
*                                                                               
FOOTER   DC    X'5728073000008000'                                              
         DC    C'* Screen trace. DA=(00010100) PF8 Next S'                      
         DC    C'creen / PF7=Previous / Enter to return '                       
*                                                                               
DABACK   EQU   9                                                                
*                                                                               
ACTNTBL  DS    0CL8                                                             
         DC    C'DISPLAY',X'01'                                                 
         DC    C'ADD    ',X'02'                                                 
         DC    C'CHANGE ',X'03'                                                 
         DC    C'DELETE ',X'04'                                                 
         DC    C'UPDATES',X'05'                                                 
         DC    C'TWAS   ',X'06'                                                 
         DC    C'RESET  ',X'07'                                                 
         DC    X'FF'                                                            
*                                                                               
*        DC    C'ACT',X'FF',AL4(ROUTINE)                                        
*                                                                               
SELTAB   DC    CL3'SEL',X'00',AL4(SELECT)                                       
         DC    CL3'CHA',X'00',AL4(CHANGE)                                       
         DC    CL3'DEL',X'80',AL4(DELETE)                                       
         DC    CL3'UP ',X'80',AL4(UPDT)                                         
         DC    CL3'UU ',X'00',AL4(UUPDT)                                        
         DC    CL3'IO ',X'80',AL4(TRACE)                                        
         DC    CL3'TWA',X'00',AL4(TWAS)                                         
         DC    CL3'TT ',X'00',AL4(TTWAS)                                        
         DC    CL3'RES',X'80',AL4(RESET)                                        
         DC    XL4'00'                                                          
*                                                                               
         LTORG                                                                  
TTABLE   DC    C'................................'                              
         DC    C'................................'                              
         DC    X'404B4B4B4B4B4B4B4B4B4A4B4C4D4E4F'                              
         DC    X'504B4B4B4B4B4B4B4B4B5A5B5C5D5E5F'                              
         DC    X'60614B4B4B4B4B4B4B4B6A6B6C6D6E6F'                              
         DC    X'4B4B4B4B4B4B4B4B4B797A7B7C7D7E7F'                              
         DC    C'.abcdefghi.......jklmnopqr......'                              
         DC    C'..stuvwxyz......................'                              
         DC    C'{ABCDEFGHI......}JKLMNOPQR......'                              
         DC    C'\.STUVWXYZ......0123456789|.....'                              
*                                                                               
         EJECT                                                                  
       ++INCLUDE DMFILTAB                                                       
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE                                    *                   
*************************************************************                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
RELO     DS    A                                                                
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
FULL1    DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE1    DS    X                                                                
DMCB     DS    6F                                                               
*                                                                               
ASAVE    DS    A                                                                
ADTF     DS    A                                                                
AUTL     DS    A                                                                
AUTLTAB  DS    A                                                                
AHEXOUT  DS    A                                                                
AHEXIN   DS    A                                                                
ASCANNER DS    A                                                                
ACALLOV  DS    A                                                                
ADATAMGR DS    A                                                                
ASSB     DS    A                                                                
ATSTTAB  DS    A                                                                
AGETTXT  DS    A                                                                
AGETHELP DS    A                                                                
AHELP    DS    A                                                                
CURSOR   DS    A                                                                
*                                                                               
SAVETST  DS    A                                                                
IDADDR   DS    A                                                                
AVADDR   DS    A                                                                
*                                                                               
IDCOUNT  DS    PL2                                                              
AVCOUNT  DS    PL2                                                              
*                                                                               
AFREE    DS    A                                                                
LTRK     DS    H                                                                
HTRK     DS    H                                                                
ENTRY    DS    H                                                                
ENTLEN   DS    H                                                                
*                                                                               
DA       DS    F                                                                
TSTLEN   DS    H                                                                
FIRST    DS    X                                                                
READ     DS    X                                                                
*                                                                               
INFO     DS    X                                                                
PFKEY    DS    X                                                                
INTVL    DS    F                                                                
ID       DS    CL4                                                              
IDSTART  DS    H                                                                
RECLEN   DS    H                                                                
TRM      DS    H                                                                
FACNA    DS    CL4                                                              
FACID    DS    CL3                                                              
ACTN     DS    C                                                                
FLAG     DS    C                                                                
FLGSEL   EQU   X'80'                                                            
FLGDIS   EQU   X'40'                                                            
FLGBACK  EQU   X'20'                                                            
MSG      DS    CL60                                                             
WORK     DS    CL200                                                            
WORK1    DS    CL200                                                            
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
SAVEDSTR EQU   *                   SAVED PART OF W/S                            
SVDATA   DS    0CL12                                                            
SVRECDA  DS    F                   SAVED DA FOR UPDATES                         
SVTOPDA  DS    F                   SAVED DA FOR UPDATES                         
SVRECDS  DS    H                   DISPLACEMENT INTO REC                        
SVTOPDS  DS    H                   DISPLACEMENT INTO REC                        
*                                                                               
SVACTN   DS    X                   SAVED ACTION                                 
SVTSTID  DS    CL4                 SAVED TEST ID                                
*                                                                               
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
IOAREA   DS    4096C                                                            
*                                                                               
WORKX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*        TSTTAB LINE DSECT                                  *                   
*************************************************************                   
         SPACE 1                                                                
TSTLINED DSECT                                                                  
TSTSELH  DS    XL8                                                              
TSTSEL   DS    CL3                                                              
TSTLINH  DS    XL8                                                              
TSTLIN   DS    0CL75                                                            
TSTNO    DS    CL2                                                              
         DS    CL1                                                              
TSTTERM  DS    CL8                                                              
         DS    CL1                                                              
TSTUSER  DS    CL4                                                              
         DS    CL1                                                              
TSTLOWT  DS    CL4                                                              
         DS    CL1                                                              
TSTHIGT  DS    CL4                                                              
         DS    CL1                                                              
TSTLAT   DS    CL8                                                              
         DS    CL1                                                              
TSTCAPT  DS    CL4                                                              
         DS    CL1                                                              
TSTLOG   DS    CL4                                                              
         DS    CL1                                                              
TSTTRAC  DS    CL4                                                              
         DS    CL1                                                              
TSTSCR   DS    CL4                                                              
         DS    CL1                                                              
TSTPAT   DS    CL5                                                              
         DS    CL1                                                              
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 1                                                                
* DDFLDHDR                                                                      
       ++INCLUDE DDFLDHDR                                                       
         EJECT                                                                  
* DMFILTABD                                                                     
       ++INCLUDE DMFILTABD                                                      
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
         SPACE 1                                                                
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
SRTSTFFD DSECT                                                                  
         DS    CL64                                                             
SCREEN   EQU   *                                                                
* SRTSTFFD                                                                      
       ++INCLUDE SRTSTFFD                                                       
         ORG   SCREEN                                                           
* SRTSTFED                                                                      
       ++INCLUDE SRTSTFED                                                       
         ORG   SCREEN                                                           
* SRTSTFDD                                                                      
       ++INCLUDE SRTSTFDD                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SRTST00S  10/23/96'                                      
         END                                                                    
