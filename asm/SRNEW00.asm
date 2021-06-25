*          DATA SET SRNEW00    AT LEVEL 023 AS OF 08/22/00                      
*PHASE T12C00A                                                                  
         TITLE '$NEW - DISPLAY CHANGE CONTROL MESSAGES '                        
         PRINT NOGEN                                                            
SRNEW    CSECT                                                                  
         NMOD1 WRKX-WRKD,*$NEW**,RA,CLEAR=YES,RR=R4                             
         USING WRKD,RC                                                          
         ST    RD,BASERD                                                        
         ST    R4,RELO                                                          
         USING SRPARMD,R1          R1=A(S/R PARAM LIST)                         
         L     R9,SRQASYSF                                                      
         USING SYSFACD,R9          R9=A(SYS FAC LIST)                           
         L     R3,SRQATWA                                                       
         USING SRNEWFFD,R3         R3=A(TWA)                                    
         L     RF,SRQATIA                                                       
         ST    RF,ASAVE            USE TIA AS SAVE AREA                         
         LA    R4,SRVSEL1H                                                      
         ST    R4,CURSOR           SET INITIAL CURSOR                           
*                                                                               
         L     RF,SRQAUTL          EXTRACT UTL DATA                             
         USING UTLD,RF                                                          
         MVC   ATBUFF,TBUFF                                                     
         MVC   TRM,TNUM                                                         
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BZ    *+8                                                              
         OI    TSVCREQ,X'02'                                                    
*                                                                               
         MVI   TYPE,C'F'                                                        
         MVC   MYTSYS,TOVSYS                                                    
         MVC   MYTPRG,TPRG                                                      
         MVC   MYTLANG,TLANG                                                    
         XI    MYTLANG,X'FF'                                                    
         MVC   MYTCTRY,TCTRY                                                    
*                                                                               
INIT010  L     RF,SRQATIOB         EXTRACT TIOB DATA                            
         USING TIOBD,RF                                                         
         MVC   PFKEY,TIOBAID                                                    
*                                                                               
INIT015  SR    RE,RE               PF1 HELP                                     
         ICM   RE,3,TIOBCURD                                                    
         AR    RE,R3                                                            
*                                                                               
INIT020  L     RF,SRQACOMF         EXTRACT COMFACS ADDRESSES                    
         USING COMFACSD,RF         RF=A(COM FAC LIST)                           
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETHELP,CGETHELP                                                
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VCOLLY,CCALLOV                                                   
         DROP  R1,RF                                                            
*                                                                               
         L     RF,VSSB             EXTRACT SSB DATA                             
         MVC   RECLEN,SSBTWAL-SSBD(RF)                                          
         MVC   FACID(4),SSBSYSN4-SSBD(RF)                                       
         MVC   FACNA(3),SSBSYSNA-SSBD(RF)                                       
*                                                                               
         GOTO1 VGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         ST    R1,ASYSLST                                                       
*                                                                               
         GOTO1 VDATCON,DMCB,(X'05',0),(X'02',TODAY)                             
         MVC   DSTR,=X'0000'                                                    
         MVC   DEND,=X'FFFF'                                                    
*                                                                               
         BAS   RE,MAIN                                                          
*                                                                               
XMOD1    L     RD,BASERD                                                        
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'                                                      
         XMOD1                                                                  
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        MAIN CONTROL                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     NTR1                                                                   
         BAS   RE,READSTR          READ SAVED STORAGE                           
*                                                                               
         CLI   PFKEY,1                                                          
         BE    MAIN060                                                          
         CLI   PFKEY,2                                                          
         BE    MAIN070                                                          
*                                                                               
         SR    R1,R1               PF 7 UP                                      
         IC    R1,SVTOP                                                         
         CLI   PFKEY,7                                                          
         BNE   *+8                                                              
         SH    R1,=H'17'                                                        
         CLI   PFKEY,8             PF 8 DOWN                                    
         BNE   *+8                                                              
         AH    R1,=H'17'                                                        
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         SR    R1,R1                                                            
*                                                                               
         STC   R1,SVTOP                                                         
*                                                                               
         CLI   SVFLAG,C'S'         IF LAST WAS SELECT                           
         BNE   MAIN010                                                          
*                                                                               
         MVC   SRVSYS,SVSYS        RESTORE OLD VALUES                           
         MVI   SRVSYSH+5,3                                                      
         MVC   SRVPRG,SVPRG                                                     
         MVI   SRVPRGH+5,3                                                      
         MVI   SVFLAG,C' '                                                      
*                                                                               
MAIN010  CLI   SRVID+4,C','                                                     
         BNE   MAIN020                                                          
         CLC   SRVID+4(2),=C',,'   TEST STEREO PGM =NEW,,PROGRAM                
         BE    MAIN015                                                          
         MVI   SRVSYSH+5,3                                                      
         MVC   SRVSYS(3),SRVID+5                                                
         CLI   SRVID+8,C','                                                     
         BNE   MAIN020                                                          
         MVI   SRVPRGH+5,3                                                      
         MVC   SRVPRG(3),SRVID+9                                                
         B     MAIN020                                                          
*                                                                               
MAIN015  MVI   TYPE,C'S'           SET STEREO NAME                              
         MVI   SRVSYSH+5,6                                                      
         MVC   SRVSYS(6),=C'STEREO'                                             
         MVI   SRVPRGH+5,8                                                      
         MVC   SRVPRG(8),SRVID+6                                                
*                                                                               
MAIN020  LA    R4,SRVSYSH                                                       
         BAS   RE,VALSYS           VALIDATE SYS/PRG/DATE                        
         CLI   SYSTEM,0                                                         
         BNE   MAIN021                                                          
         MVC   SRVSYS,=C'ALL     ' IF SYS ALL SET SYS/PRG ALL                   
         MVC   SRVPRG,=C'ALL     '                                              
         MVI   PROGRAM,0                                                        
         B     MAIN022                                                          
*                                                                               
MAIN021  LA    R4,SRVPRGH          ELSE VALIDATE PROGRAM                        
         BAS   RE,VALPGM                                                        
*                                                                               
MAIN022  EQU   *                                                                
*                                                                               
         CLI   PROGRAM,0           TEST FOR SPECIFIC                            
         BNE   MAIN050                                                          
         CLI   SPROGRAM,X'40'      TEST FOR SPECIFIC                            
         BH    MAIN050                                                          
*                                                                               
MAIN040  BAS   RE,CHKSEL           LOOK FOR SELECT FIELDS                       
         BE    MAIN049                                                          
         BAS   RE,BLDLST           NONE SO BUILD LIST                           
         LA    R4,SRVSEL1H                                                      
         B     INF1                                                             
*                                                                               
MAIN049  MVI   SVFLAG,C'S'         FLAG SELECT USED                             
*                                                                               
MAIN050  BAS   RE,DISPMSG                                                       
         B     INF0                                                             
*                                                                               
MAIN060  OC    SVREP1,SVREP1                                                    
         BZ    MAIN010                                                          
         MVC   DUB1,SVREP1                                                      
         B     MAIN080                                                          
*                                                                               
MAIN070  MVI   DATEFLAG,C'X'                                                    
         OC    SVREP2,SVREP2                                                    
         BZ    MAIN010                                                          
         MVC   DUB1,SVREP2                                                      
         B     MAIN080                                                          
*                                                                               
MAIN080  LA    RF,WORK                                                          
         MVC   0(3,RF),DUB1                                                     
         MVI   3(RF),C','                                                       
         EDIT  (B2,DUB1+3),(5,4(RF)),ALIGN=LEFT,WRK=WORK1                       
         LR    R1,R0                                                            
         LA    RF,4(R1,RF)                                                      
         LA    RE,WORK                                                          
         SR    RF,RE                                                            
*                                                                               
         L     R1,ATBUFF           LOCATE TERMINAL BUFFER                       
         LA    RF,3(RF)                                                         
         MVI   0(R1),X'08'         LENGTH FOR =DQU                              
         STC   RF,8(R1)            LENGTH FOR REPID                             
         MVC   1(2,R1),SRVIDH+2    INSERT SCREEN ADDRESSES                      
         MVC   9(2,R1),SRVSYSH+2                                                
         MVC   3(5,R1),DC5DQU      =DQUP IN SRVID FIELD                         
         MVC   11(L'WORK,R1),WORK                                               
*                                                                               
         LA    RF,8(R1,RF)                                                      
         MVI   0(RF),0             END MARKER                                   
         MVC   SRVID(8),=C'=GOBACK '                                            
         B     XMOD1                                                            
*                                                                               
MAIN990  BAS   RE,WRITESTR         WRITE STORAGE AND EXIT                       
         LA    R4,SRVSEL1H                                                      
         ST    R4,CURSOR                                                        
MAINX    B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        BUILD LIST OF PROVER RECORDS                       *                   
*************************************************************                   
         SPACE 1                                                                
BLDLST   NTR1                                                                   
         LA    R5,IOAREA           SET UP R5 FOR RECORDS                        
         USING CTPRREC,R5                                                       
         XC    CTPRKEY,CTPRKEY                                                  
         MVI   CTPRKTYP,CTPRKTYQ                                                
         MVC   CTPRKPTY,TYPE                                                    
         MVC   CTPRKLAN,MYTLANG                                                 
*                                                                               
         CLI   TYPE,C'S'           SET STEREO KEY                               
         BNE   BLDL002                                                          
*                                                                               
         MVC   CTPRKPNM,SPROGRAM   OR SYS/PGM KEY                               
         B     BLDL005                                                          
*                                                                               
BLDL002  MVC   CTPRKSYS,SYSTEM                                                  
         MVC   CTPRKPRG,PROGRAM                                                 
*                                                                               
BLDL005  GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOAREA,IOAREA                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SRVHDR2,HEADER2     SET HEADER                                   
         XC    SVNDX(9*21),SVNDX   CLEAR SEL INDEX                              
         XC    SVNDX+189(9*21),SVNDX+189                                        
         LA    R7,SVNDX                                                         
*                                                                               
         LA    R4,SRVSEL1H         SET UP R4 FOR SCREEN LINES                   
         USING SLINED,R4                                                        
*                                                                               
BLDL010  CLI   CTPRKTYP,CTPRKTYQ   CHECK RECORD STILL GOOD                      
         BNE   BLDLSTX                                                          
         CLC   CTPRKPTY,TYPE       CHECK TYPE                                   
         BNE   BLDLSTX                                                          
         CLC   CTPRKLAN,MYTLANG    CHECK LANG                                   
         BNE   BLDLHI                                                           
*                                                                               
         CLI   TYPE,C'S'                                                        
         BE    BLDL040                                                          
*                                                                               
         CLI   SYSTEM,0            ALL SYSTEMS                                  
         BE    *+14                                                             
         CLC   CTPRKSYS,SYSTEM     CHECK SYSTEM                                 
         BNE   BLDLSTX                                                          
         CLI   PROGRAM,0           ALL PROGRAMS                                 
         BE    *+14                                                             
         CLC   CTPRKPRG,PROGRAM    CHECK PROGRAM                                
         BNE   BLDLSTX                                                          
*                                                                               
         SR    R1,R1               BUMP TOP COUNTER                             
         IC    R1,TOP                                                           
         LA    R1,1(R1)                                                         
         STC   R1,TOP                                                           
         CLC   TOP,SVTOP           TEST SAVED TOP                               
         BL    BLDLHI                                                           
*                                                                               
         LA    R6,CTPRDATA         DATE LEVEL ELEMENT                           
         USING CTPRVD,R6                                                        
BLDL015  CLI   CTPRVEL,CTPRVELQ                                                 
         BE    BLDL017                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BZ    BLDLHI                                                           
         AR    R6,R0                                                            
         B     BLDL015                                                          
*                                                                               
BLDL017  CLC   DEND,CTPRVDFR                                                    
         BL    BLDLHI                                                           
         CLC   DSTR,CTPRVDTO                                                    
         BH    BLDLHI                                                           
*                                                                               
BLDL040  MVC   0(1,R7),CTPRKPTY    SAVE FOR SELECT                              
         MVC   1(1,R7),CTPRKSYS                                                 
         MVC   2(1,R7),CTPRKPRG                                                 
         MVC   3(8,R7),CTPRKPNM                                                 
*                                                                               
         XC    SELSEL,SELSEL                                                    
         MVC   LNTYPE,=C'FACPAK'   SET TYPE                                     
         CLI   CTPRKPTY,C'F'                                                    
         BE    *+10                                                             
         MVC   LNTYPE,=C'STEREO '                                               
*                                                                               
         MVC   LNSUB,=C'   '       SUB IS BLANK FOR NOW                         
*                                                                               
         CLI   CTPRKPTY,C'S'                                                    
         BE    BLDL050                                                          
*                                                                               
         MVC   GTSYS,CTPRKSYS      SYSTEM PROGRAM                               
         BAS   RE,GETOVS                                                        
         MVC   LNSYS,GTSYSN                                                     
         MVC   GTSYSSE,GTSYS                                                    
         BAS   RE,GETSYS                                                        
         MVC   GTPROG,CTPRKPRG                                                  
         BAS   RE,GETPROG                                                       
         MVC   LNPROG,GTPROGN                                                   
         B     BLDL100                                                          
*                                                                               
BLDL050  MVC   LNPROG,CTPRKPNM                                                  
*                                                                               
BLDL100  LA    R6,CTPRDATA         DATE LEVEL ELEMENT                           
         USING CTPRVD,R6                                                        
BLDL115  CLI   CTPRVEL,CTPRVELQ                                                 
         BE    BLDL120                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BZ    BLDLHI                                                           
         AR    R6,R0                                                            
         B     BLDL115                                                          
*                                                                               
BLDL120  MVI   DATEFLAG,C'N'                                                    
         OC    CTPRVDFR,CTPRVDFR   ANY DATE                                     
         BZ    BLDL122                                                          
*                                                                               
         CLC   TODAY,CTPRVDFR      IS TODAY<FROM                                
         BL    BLDL121                                                          
         CLC   TODAY,CTPRVDTO      IS TODAY>TO                                  
         BH    BLDL121                                                          
*                                                                               
         MVI   DATEFLAG,C'Y'                                                    
BLDL121  GOTO1 VDATCON,DMCB,(X'22',CTPRVDFR),(X'51',LNDATE),CTPRVDTO            
*                                                                               
BLDL122  EDIT  (B1,CTPRVVER),(3,LNVERS),ALIGN=LEFT,ZERO=NOBLANK                 
         LA    R1,LNVERS                                                        
         AR    R1,R0                                                            
         MVI   0(R1),C'.'                                                       
         EDIT  (B1,CTPRVLEV),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
         MVC   LNCHANG+0(3),CTPRVPQ1                                            
         MVI   LNCHANG+3,C','                                                   
         EDIT  (B2,CTPRVPQ1+3),(5,LNCHANG+4),ALIGN=LEFT                         
*                                                                               
         MVC   LNAUDIT(3),=C'n/a'                                               
         CLI   DATEFLAG,C'N'                                                    
         BE    BLDL130                                                          
         OC    CTPRVPQ2,CTPRVPQ2                                                
         BZ    BLDL130                                                          
*                                                                               
         MVC   LNAUDIT+0(3),CTPRVPQ2                                            
         MVI   LNAUDIT+3,C','                                                   
         EDIT  (B2,CTPRVPQ2+3),(5,LNAUDIT+4),ALIGN=LEFT                         
*                                                                               
BLDL130  MVC   11(5,R7),CTPRVPQ1   SAVE FOR SELECT                              
         CLI   DATEFLAG,C'N'                                                    
         BE    *+10                                                             
         MVC   16(5,R7),CTPRVPQ2                                                
         LA    R7,21(R7)                                                        
*                                                                               
BLDLNXT  LA    R4,8+3+8+75(R4)     BUMP TO NEXT FIELD                           
         LA    R1,SRVPFKH                                                       
         CR    R4,R1                                                            
         BNL   BLDLSTX                                                          
*                                                                               
BLDLHI   GOTO1 VDATAMGR,DMCB,DMRSEQ,CTFILE,IOAREA,IOAREA                        
         CLI   8(R1),0                                                          
         BE    BLDL010             NEXT RECORD                                  
         DC    H'0'                                                             
*                                                                               
BLDLSTX  B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY FULL MESSAGE                               *                   
*************************************************************                   
         SPACE 1                                                                
DISPMSG  NTR1                                                                   
         LA    R5,IOAREA           SET UP R5 FOR RECORDS                        
         USING CTPRREC,R5                                                       
*                                                                               
         MVI   DMCB+4,C'R'         LOAD NEW ROOT SCREEN                         
         MVC   DMCB+5(2),=X'012C'                                               
         MVI   DMCB+7,X'FE'                                                     
         GOTO1 VCOLLY,DMCB,SCREEN,,0                                            
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                BAD RETURN FROM CALLOV                       
*                                                                               
         LA    R4,SRVSYSH                                                       
         XC    CTPRKEY,CTPRKEY                                                  
         MVI   CTPRKTYP,CTPRKTYQ                                                
         MVC   CTPRKPTY,TYPE                                                    
         MVC   CTPRKLAN,MYTLANG                                                 
         CLI   TYPE,C'S'                                                        
         BNE   DISP001                                                          
         MVC   CTPRKPNM,SPROGRAM                                                
         B     DISP002                                                          
DISP001  MVC   CTPRKSYS,SYSTEM                                                  
         MVC   CTPRKPRG,PROGRAM                                                 
         MVC   GTSYS,CTPRKSYS      SYSTEM PROGRAM                               
         BAS   RE,GETOVS                                                        
         MVC   SRVSYS,GTSYSN                                                    
         MVC   GTSYSSE,GTSYS                                                    
         BAS   RE,GETSYS                                                        
         MVC   GTPROG,CTPRKPRG                                                  
         BAS   RE,GETPROG                                                       
         MVC   SRVPRG(8),GTPROGN                                                
         B     DISP003                                                          
*                                                                               
DISP002  MVC   SRVSYS,=C'STEREO '                                               
         MVC   SRVPRG(8),SPROGRAM                                               
*                                                                               
DISP003  GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOAREA,IOAREA                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TYPE,C'S'                                                        
         BE    DISP004                                                          
         CLC   CTPRKSYS,SYSTEM     CHECK SYSTEM                                 
         BNE   ERR6                                                             
         CLC   CTPRKPRG,PROGRAM    CHECK PROGRAM                                
         BNE   ERR6                                                             
         B     DISP010                                                          
*                                                                               
DISP004  CLC   CTPRKPNM,SPROGRAM   CHECK PROGRAM                                
         BNE   ERR6                                                             
*                                                                               
DISP010  LA    R6,CTPRDATA         DATE LEVEL ELEMENT                           
         USING CTPRVD,R6                                                        
DISP015  CLI   CTPRVEL,CTPRVELQ                                                 
         BE    DISP016                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BZ    DISPMSX                                                          
         AR    R6,R0                                                            
         B     DISP015                                                          
*                                                                               
DISP016  CLI   TYPE,C'S'           TEST STEREO                                  
         BNE   DISP020                                                          
         MVC   SRVPER(33),=C'DATE=                      VER= '                  
         GOTO1 VDATCON,DMCB,(X'22',CTPRVDFR),(X'51',SRVPER+5),CTPRVDTO          
*                                                                               
         EDIT  (B1,CTPRVVER),(3,SRVPER+31),ALIGN=LEFT,ZERO=NOBLANK              
         LA    R1,SRVPER+31                                                     
         AR    R1,R0                                                            
         MVI   0(R1),C'.'                                                       
         EDIT  (B1,CTPRVLEV),(3,1(R1)),ALIGN=LEFT,ZERO=NOBLANK                  
*                                                                               
DISP020  MVC   SVREP1,CTPRVPQ1                                                  
         XC    SVREP2,SVREP2                                                    
*                                                                               
         CLI   DATEFLAG,C'X'                                                    
         BE    DISP060                                                          
*                                                                               
         CLC   TODAY,CTPRVDFR      IS TODAY<FROM                                
         BL    DISP041                                                          
         CLC   TODAY,CTPRVDTO      IS TODAY>TO                                  
         BH    DISP041                                                          
*                                                                               
         MVC   SVREP2,CTPRVPQ2     CANCEL REPORT 2 IF OUT OF DATE               
*                                                                               
DISP030  LA    R6,CTPRDATA         MESSAGE ELEMENT                              
         USING GMSGEL,R6                                                        
DISP035  CLI   GMSGEL,GMSGELC                                                   
         BE    DISP040                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BZ    DISP041                                                          
         AR    R6,R0                                                            
         B     DISP035                                                          
*                                                                               
DISP040  SR    R1,R1               EXTRACT MESSAGE                              
         IC    R1,GMSGELL                                                       
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SRVHDR1(0),GMSGTXT                                               
         NI    SRVHDR1H+1,X'F3'    SET NORMAL INTENSITY                         
*                                                                               
DISP041  LA    R4,SRVML1                                                        
         LA    R6,CTPRDATA         MESSAGE ELEMENT                              
         USING GMTXTD,R6                                                        
DISP045  CLI   GMTXTEL,GMTXTELC                                                 
         BE    DISP050                                                          
DISP046  SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BZ    DISPMSX                                                          
         AR    R6,R0                                                            
         B     DISP045                                                          
*                                                                               
DISP050  LA    R4,SRVML1                                                        
         SR    R0,R0                                                            
         IC    R0,GMTXTLNO                                                      
         BCTR  R0,0                                                             
         MH    R0,=H'96'                                                        
         AR    R4,R0                                                            
         SR    R1,R1               EXTRACT MESSAGE                              
         IC    R1,GMTXTELL                                                      
         SH    R1,=H'4'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),GMTXTLIN                                                 
         B     DISP046                                                          
*                                                                               
DISP060  LA    R4,SRVML1+96                                                     
*                                                                               
         MVC   10(50,R4),NOUP01                                                 
         LA    4,96(R4)                                                         
         MVC   10(50,R4),NOUP02                                                 
*                                                                               
DISPMSX  LA    R4,SRVSYSH                                                       
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CHECK FOR SELECT FIELDS                            *                   
*************************************************************                   
         SPACE 1                                                                
CHKSEL   NTR1                                                                   
         LA    R4,SRVSEL1H                                                      
         LA    R7,SVNDX                                                         
CHKSEL1  CLI   5(R4),0                                                          
         BE    CHKNXT                                                           
         CLI   8(R4),C'S'                                                       
         BE    CHKSEL2                                                          
*                                                                               
         CLI   8(R4),C'1'                                                       
         BE    CHKSEL3                                                          
         CLI   8(R4),C'2'                                                       
         BE    CHKSEL4                                                          
*                                                                               
CHKSEL2  MVC   TYPE,0(R7)                                                       
         MVC   SYSTEM,1(R7)                                                     
         MVC   PROGRAM,2(R7)                                                    
         MVC   SPROGRAM,3(R7)                                                   
         B     GOODXIT                                                          
*                                                                               
CHKSEL3  LA    RF,WORK                                                          
         MVC   0(3,RF),11(R7)                                                   
         MVI   3(RF),C','                                                       
         EDIT  (B2,14(R7)),(5,4(RF)),ALIGN=LEFT,WRK=WORK1                       
         LR    R1,R0                                                            
         LA    RF,4(R1,RF)                                                      
         LA    RE,WORK                                                          
         SR    RF,RE                                                            
         B     CHKSEL5                                                          
*                                                                               
CHKSEL4  MVI   DATEFLAG,C'X'                                                    
         OC    16(3,R7),16(R7)                                                  
         BZ    CHKSEL2                                                          
         LA    RF,WORK                                                          
         MVC   0(3,RF),16(R7)                                                   
         MVI   3(RF),C','                                                       
         EDIT  (B2,19(R7)),(5,4(RF)),ALIGN=LEFT,WRK=WORK1                       
         LR    R1,R0                                                            
         LA    RF,4(R1,RF)                                                      
         LA    RE,WORK                                                          
         SR    RF,RE                                                            
         B     CHKSEL5                                                          
*                                                                               
CHKSEL5  L     R1,ATBUFF           LOCATE TERMINAL BUFFER                       
         LA    RF,3(RF)                                                         
         MVI   0(R1),X'08'         LENGTH FOR =DQU                              
         STC   RF,8(R1)            LENGTH FOR REPID                             
         MVC   1(2,R1),SRVIDH+2    INSERT SCREEN ADDRESSES                      
         MVC   9(2,R1),SRVSYSH+2                                                
         MVC   3(5,R1),DC5DQU      =DQUP IN SRVID FIELD                         
         MVC   11(L'WORK,R1),WORK                                               
*                                                                               
         LA    RF,8(R1,RF)                                                      
         MVI   0(RF),0             END MARKER                                   
         MVC   SRVID(8),=C'=GOBACK '                                            
         B     XMOD1                                                            
*                                                                               
CHKNXT   LA    R7,21(R7)                                                        
         LA    R4,8+3+8+75(R4)                                                  
         LA    R1,SRVPFKH                                                       
         CR    R4,R1                                                            
         BL    CHKSEL1                                                          
         B     BADXIT                                                           
         EJECT                                                                  
*************************************************************                   
*        VALIDATE SYSTEM FIELD                              *                   
*************************************************************                   
         SPACE 1                                                                
VALSYS   ST    RE,SAVERE                                                        
         CLI   5(R4),0                                                          
         BE    VALSYSN                                                          
         MVC   SVSYS,8(R4)                                                      
*                                                                               
                                                                                
         CLC   8(3,R4),=C'ALL'                                                  
         BNE   *+12                                                             
         MVI   SYSTEM,0                                                         
         B     VALSYSX                                                          
         CLC   8(6,R4),=C'STEREO'                                               
         BNE   *+16                                                             
         MVI   TYPE,C'S'                                                        
         MVI   SYSTEM,X'FF'                                                     
         B     VALSYSX                                                          
*                                                                               
         L     R1,ASYSLST                                                       
         LA    R1,6(R1)                                                         
         USING SYSLSTD,R1                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R4)                                                         
         BCTR  RF,0                                                             
VALSYS1  EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),SYSLNAME                                                 
         BE    VALSYS2                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),SYSLSHRT                                                 
         BE    VALSYS2                                                          
         LA    R1,SYSLLEN(R1)                                                   
         CLI   0(R1),0                                                          
         BNE   VALSYS1                                                          
         B     ERR3                                                             
*                                                                               
VALSYS2  MVC   8(7,R4),SYSLNAME                                                 
         MVC   SYSTEM,SYSLNUM                                                   
*                                                                               
         L     R5,VSELIST                                                       
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
         CLC   SEOVSYS,SYSTEM                                                   
         BE    *+10                                                             
         BXLE  R5,R6,*-10                                                       
         DC    H'0'                                                             
         L     R1,SEPGMS           SET-UP FOR PGMLIST BXLE                      
         ST    R1,APRGMS                                                        
*                                                                               
VALSYSX  L     RE,SAVERE                                                        
         BR    RE                                                               
VALSYSN  MVC   SYSTEM,MYTSYS       IF NO SYSTEM SET FROM UTL                    
         B     VALSYSX                                                          
         EJECT                                                                  
*************************************************************                   
*        VALIDATE PROGRAM FIELD                             *                   
*************************************************************                   
         SPACE 1                                                                
VALPGM   ST    RE,SAVERE                                                        
         CLI   5(R4),0                                                          
         BE    VALPGMN                                                          
         MVC   SVPRG,8(R4)                                                      
*                                                                               
         MVI   PROGRAM,0                                                        
         CLC   8(3,R4),=C'ALL'                                                  
         BE    VALPGMX                                                          
*                                                                               
         CLI   TYPE,C'S'           TEST FOR STEREO                              
         BNE   VALPGM0                                                          
*                                                                               
         MVC   SPROGRAM,SPACES     SET STEREO PROGRAM                           
         OC    SPROGRAM,8(R4)                                                   
         B     VALPGMX                                                          
*                                                                               
VALPGM0  L     R5,APRGMS           FIND PROGRAM IN PGMS LIST                    
         BAS   RE,SETBXLE                                                       
         USING PGMLSTD,R5                                                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R4)                                                         
         BCTR  RF,0                                                             
VALPGM1  CLI   PGMCTRY,0           DEFAULT MATCHES ALL                          
         BE    *+14                                                             
         CLC   PGMCTRY,MYTCTRY     TEST PROG COUNTRY                            
         BNE   VALPGM1A                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R4),PGMNAME                                                  
         BE    VALPGM2                                                          
VALPGM1A BXLE  R5,R6,VALPGM1                                                    
         B     ERR4                                                             
*                                                                               
VALPGM2  MVC   8(7,R4),PGMNAME                                                  
         MVC   PROGRAM,PGMNUM                                                   
*                                                                               
VALPGMX  L     RE,SAVERE                                                        
         BR    RE                                                               
VALPGMN  MVC   PROGRAM,MYTPRG                                                   
         B     VALPGMX                                                          
         EJECT                                                                  
*************************************************************                   
*        GET GTSYS & GTSYSN FROM GTSYSSE                    *                   
*************************************************************                   
         SPACE 1                                                                
GETSYS   NTR1                                                                   
         L     R5,VSELIST          MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R5                                                       
GETSYS0  CLI   GTSYSSE,0           IF SYSSE IS ZERO                             
         BNE   GETSYS1                                                          
         CLC   GTSYS,SEOVSYS       TEST GTSYS WITH OVSYS                        
         BE    GETSYS2                                                          
GETSYS1  CLC   GTSYSSE,SESYS       TEST SE NUMBER                               
         BE    GETSYS2                                                          
         BXLE  R5,R6,GETSYS0       NEXT                                         
         MVI   GTSYS,0                                                          
         XC    GTSYSN,GTSYSN                                                    
         B     BADXIT              ERROR EXIT NOT FOUND                         
*                                                                               
GETSYS2  MVC   GTSYS,SEOVSYS       FOUND                                        
         MVC   GTSYSN,SENAME       SET NAME                                     
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         B     GOODXIT                                                          
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTPROGN FROM GTPROG                            *                   
*************************************************************                   
         SPACE 1                                                                
GETPROG  NTR1                                                                   
         ICM   R5,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         BZ    GETPRG2                                                          
         BAS   RE,SETBXLE          SET BXLE                                     
         USING PGMLSTD,R5                                                       
GETPRG0  CLC   GTPROG,PGMNUM       TEST PROG NUMBER                             
         BNE   GETPRG0A                                                         
         CLI   PGMCTRY,0                                                        
         BE    GETPRG1                                                          
         CLC   PGMCTRY,MYTCTRY     TEST PROG COUNTRY                            
         BE    GETPRG1                                                          
GETPRG0A BXLE  R5,R6,GETPRG0       NEXT                                         
GETPRG2  XC    GTPROGN,GTPROGN     NOT FOUND SO HEXOUT NUMBER                   
         OC    GTPROG,GTPROG                                                    
         BZ    GOODXIT                                                          
         GOTO1 VHEXOUT,DMCB,GTPROG,GTPROGN,1                                    
         B     GOODXIT                                                          
GETPRG1  MVC   GTPROGN(7),PGMNAME  SET NAME                                     
         B     GOODXIT                                                          
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        GET SYSTEM NAME FROM GTSYS                         *                   
*************************************************************                   
         SPACE 1                                                                
GETOVS   NTR1                                                                   
         L     RE,ASYSLST          RE=SYSLST                                    
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
*                                                                               
GETS010  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    GETS090                                                          
         CLC   GTSYS,SYSLNUM       TEST SYSTEM NUMBER                           
         BE    GETS990                                                          
         LA    RE,SYSLLEN(RE)      NEXT                                         
         B     GETS010                                                          
*                                                                               
GETS090  B     BADXIT              SET CC NEQ NOT FOUND                         
*                                                                               
GETS990  MVC   GTSYSN,SYSLNAME                                                  
         LR    R1,RE               R1=A(SYSLST ENTRY)                           
         B     GOODXIT             SET CC EQU                                   
         DROP  RE                                                               
         EJECT                                                                  
GOODXIT  CR    RB,RB                                                            
         B     XIT1                                                             
BADXIT   LTR   RB,RB                                                            
         B     XIT1                                                             
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
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R2),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),NEWD    TEST FOR MY ID                               
         BNE   READX                                                            
         LA    R1,SRCOMWRK                                                      
         MVC   SAVEDSTR(255),4(R1)                                              
         MVC   SAVEDSTR+255(SAVEDL-255),255+4(R1)                               
READX    B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        WRITE OUT SAVED STORAGE                            *                   
*************************************************************                   
         SPACE 1                                                                
WRITESTR NTR1                                                                   
         L     R5,ASAVE                                                         
         USING SRSD,R5                                                          
         LA    R1,SRCOMWRK                                                      
         MVC   0(4,R1),NEWD                                                     
         MVC   4(255,R1),SAVEDSTR                                               
         MVC   4+255(SAVEDL-255,R1),SAVEDSTR+255                                
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT1                                                             
         DROP  R5                                                               
         EJECT                                                                  
*************************************************************                   
*        SUB ROUTINES                                       *                   
*************************************************************                   
         SPACE 1                                                                
TWAXC    NTR1                                                                   
         TWAXC (R1),SRVSEL1H,PROT=Y                                             
         B     XIT1                                                             
*                                                                               
SETBXLE  LH    R6,0(R5)                                                         
         L     R7,2(R5)                                                         
         LA    R5,6(R5)                                                         
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        ERROR EXITS                                        *                   
*************************************************************                   
         SPACE 1                                                                
ERR0     LA    R0,2                INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR1     LA    R0,1                MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R0,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR3     LA    R0,32               INVALID SYSTEM                               
         B     ERRX                                                             
ERR4     LA    R0,33               INVALID PROGRAM                              
         B     ERRX                                                             
ERR5     LA    R0,28               INVALID DATE                                 
         B     ERRX                                                             
ERR6     LA    R0,200              NO MESSAGES FOUND                            
         B     ERRX                                                             
ERR7     LA    R0,68               NO CHANGE REPORT                             
         B     ERRX                                                             
ERR8     LA    R0,69               NO HISTORY REPORT                            
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     L     RD,BASERD                                                        
         ST    R4,CURSOR                                                        
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),(4,FACID)                           
         B     XMOD1                                                            
         SPACE 2                                                                
*************************************************************                   
*        INFORMATION EXITS                                  *                   
*************************************************************                   
         SPACE 2                                                                
INF0     LA    R0,192              MESSAGE DISPLAYED. ENTER TO RETURN           
         B     INFX                                                             
INF1     LA    R0,193              MESSAGES DISPLAYED. SELECT OR PF12           
         B     INFX                                                             
INF2     LA    R0,10               HIT PF1 OR PF2                               
         B     INFX                                                             
*                                                                               
INFX     BAS   RE,WRITESTR                                                      
         L     RD,BASERD                                                        
         ST    R4,CURSOR                                                        
         XC    DMCB(24),DMCB                                                    
         GOTO1 VGETTXT,DMCB,(R0),0,(C'I',0),(4,FACID)                           
         B     XMOD1                                                            
         EJECT                                                                  
*************************************************************                   
*        CALL GETHELP AND EXIT TO MONITOR                   *                   
*************************************************************                   
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
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0)                         
         DC    H'0'                                                             
HELPTAB  LA    R1,SRVSYSH           POSSIBLE HELP FIELDS                        
         LA    R1,SRVPRGH                                                       
         LA    R1,SRVPRGH                                                       
         LA    R1,SRVSEL1H         <--- OR GREATER                              
         DC    H'0'                                                             
         EJECT                                                                  
*************************************************************                   
*        CONSTANTS AND LITERALS                             *                   
*************************************************************                   
         SPACE 1                                                                
TEMPSTR  DC    C'TEMPSTR'                                                       
DMREAD   DC    C'DMREAD '                                                       
DMRDHI   DC    C'DMRDHI '                                                       
DMRSEQ   DC    C'DMRSEQ '                                                       
CTFILE   DC    C'CTFILE '                                                       
DMWRT    DC    C'DMWRT  '                                                       
NEWD     DC    C'T12C'                                                          
DC5DQU   DC    C'=DQUP'                                                         
SPACES   DC    80C' '                                                           
HELPID   DC    XL10'012CFF00010000000000'                                       
*                                                                               
HEADER2  DC    C'--- ------ --- ------- ------- ----------------- ---'          
         DC    C'---- ----------------------'                                   
*                                                                               
NOUP01   DC    Cl50'  No published update today                       '         
NOUP02   DC    Cl50'  for current features listing, hit PF1           '         
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*        WORKING STORAGE DSECT                              *                   
*************************************************************                   
         SPACE 2                                                                
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
DMCB     DS    6F                                                               
*                                                                               
WORK     DS    CL80                                                             
WORK1    DS    CL32                                                             
*                                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
VGETHELP DS    A                                                                
VPERVAL  DS    A                                                                
VGETFACT DS    A                                                                
VCOLLY   DS    A                                                                
ASYSLST  DS    A                                                                
ASAVE    DS    A                                                                
APRGMS   DS    A                                                                
BASERD   DS    A                                                                
RELO     DS    A                                                                
*                                                                               
GTSYS    DS    X                   OV SYSTEM                                    
GTSYSSE  DS    X                   SE SYSTEM                                    
GTSYSL   DS    X                   LEN FOR OVSYS NAME SEARCH                    
GTSYSN   DS    CL7                 SYS NAME                                     
GTPROGL  DS    X                   LEN FOR PROG NAME SEARCH                     
GTPROGN  DS    CL8                 PROG NAME                                    
GTPROG   DS    X                   PROG NUMBER                                  
GTLANGN  DS    CL3                 LANGUAGE NAME                                
GTLANG   DS    X                   LANGUAGE CODE                                
GTAPGMS  DS    A                   A(PRGMS LIST FOR SYSTEM)                     
*                                                                               
CURSOR   DS    A                                                                
SAVERE   DS    A                                                                
AHELP    DS    A                                                                
ATBUFF   DS    A                                                                
PFKEY    DS    X                                                                
COUNT    DS    X                                                                
TRM      DS    H                                                                
RECLEN   DS    H                                                                
FACID    DS    CL4                                                              
FACNA    DS    CL3                                                              
TODAY    DS    CL2                                                              
*                                                                               
DATEFLAG DS    X                                                                
TOP      DS    X                                                                
TYPE     DS    X                                                                
SYSTEM   DS    X                                                                
PROGRAM  DS    X                                                                
DSTR     DS    XL2                                                              
DEND     DS    XL2                                                              
MYTSYS   DS    X                                                                
MYTPRG   DS    X                                                                
MYTLANG  DS    X                                                                
MYTCTRY  DS    X                                                                
SPROGRAM DS    CL8                                                              
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
SAVEDSTR EQU   *                   SAVED PART OF W/S                            
SVTOP    DS    X                                                                
SVFLAG   DS    X                                                                
SVNDX    DS    18XL21              SAVE FOR SELECT                              
SVSYS    DS    CL8                                                              
SVPRG    DS    CL8                                                              
SVDAT    DS    CL17                                                             
SVREP1   DS    CL5                                                              
SVREP2   DS    CL5                                                              
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
IOAREA   DS    600D                4K WORK AREA                                 
WRKX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        SCREEN DSECT                                       *                   
*************************************************************                   
         SPACE 2                                                                
SRNEWFFD DSECT                                                                  
         DS    CL64                                                             
SCREEN   EQU   *                                                                
*SRNEWFFD                                                                       
       ++INCLUDE SRNEWFFD                                                       
         ORG   SCREEN                                                           
*SRNEWFED                                                                       
       ++INCLUDE SRNEWFED                                                       
         EJECT                                                                  
*************************************************************                   
*        OTHER DSECTS                                       *                   
*************************************************************                   
         SPACE 1                                                                
SLINED   DSECT                                                                  
SELSELH  DS    CL8                                                              
SELSEL   DS    CL3                                                              
LINEH    DS    CL8                                                              
LINE     DS    0CL75                                                            
LNTYPE   DS    CL6                                                              
         DS    CL1                                                              
LNSUB    DS    CL3                                                              
         DS    CL1                                                              
LNSYS    DS    CL7                                                              
         DS    CL1                                                              
LNPROG   DS    CL8                                                              
         DS    CL1                                                              
LNDATE   DS    CL17                                                             
         DS    CL1                                                              
LNVERS   DS    CL7                                                              
         DS    CL1                                                              
LNCHANG  DS    CL9                                                              
         DS    CL2                                                              
LNAUDIT  DS    CL9                                                              
         DS    CL1                                                              
         EJECT                                                                  
         SPACE 1                                                                
*DDCOMFACS                                                                      
*FADSECTS                                                                       
*FAFACTS                                                                        
*FASYSLSTD                                                                      
*DDPERVALD                                                                      
*CTGENFILE                                                                      
*GEGENMSG                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEGENMSG                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SRNEW00   08/22/00'                                      
         END                                                                    
