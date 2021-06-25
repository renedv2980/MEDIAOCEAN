*          DATA SET SRVER00X   AT LEVEL 006 AS OF 08/22/00                      
*PHASE T14500A                                                                  
         TITLE '$VERSION - DISPLAY CHANGE VERSION TABLE'                        
         PRINT NOGEN                                                            
VERSION  CSECT                                                                  
         NMOD1 WRKX-WRKD,*$VER**,RA,CLEAR=YES,RR=R4                             
         USING WRKD,RC                                                          
         ST    RD,BASERD                                                        
         ST    R4,RELO                                                          
         USING SRPARMD,R1          R1=A(S/R PARAM LIST)                         
         L     R9,SRQASYSF                                                      
         USING SYSFACD,R9          R9=A(SYS FAC LIST)                           
         L     R3,SRQATWA                                                       
         USING SRVERFFD,R3         R3=A(TWA)                                    
         L     RF,SRQATIA                                                       
         ST    RF,ASAVE            USE TIA AS SAVE AREA                         
*                                                                               
         L     RF,SRQAUTL          EXTRACT UTL DATA                             
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+12                                                             
         OI    TSVCREQ,X'02'                                                    
         OI    DDS,DDSNEW                                                       
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
INIT020  L     RF,SRQACOMF         EXTRACT COMFACS ADDRESSES                    
         USING COMFACSD,RF         RF=A(COM FAC LIST)                           
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VGETHELP,CGETHELP                                                
         DROP  R1,RF                                                            
*                                                                               
         L     RF,VSSB             EXTRACT SSB DATA                             
         MVC   RECLEN,SSBTWAL-SSBD(RF)                                          
         MVC   FACID(4),SSBSYSN4-SSBD(RF)                                       
         MVC   FACNA(3),SSBSYSNA-SSBD(RF)                                       
         MVC   ALANG,SSBALANG-SSBD(RF)                                          
         MVC   ALET,SSBTBLET-SSBD(RF)                                           
*                                                                               
         MVC   SRVHDR,FLHEADER                                                  
         MVC   SRVHDR1,FLUNDER                                                  
         EJECT                                                                  
*************************************************************                   
*        MAIN CONTROL                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     BAS   RE,READSTR          READ SAVED VALUES                            
         BAS   RE,PARMVAL          VALIDATE PARMS                               
         LA    R7,SRVP1H                                                        
         ST    R7,CURSOR                                                        
*                                                                               
         TM    DDS,DDSNEW          RESET PAGE IF NEW                            
         BO    MAIN02                                                           
         XR    R1,R1               TEST FOR UP OR DOWN                          
         IC    R1,PAGE                                                          
         CLI   PFKEY,7             PF7 UP                                       
         BNE   *+8                                                              
         SH    R1,=H'1'                                                         
         CLI   PFKEY,8             PF8 DOWN                                     
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         LTR   R1,R1                                                            
         BNM   *+6                                                              
*                                                                               
MAIN02   XR    R1,R1               XC R1 IF IT GOES MINUS                       
         STC   R1,PAGE                                                          
         CLC   PAGE,SVPAGE         NEW SCREEN IF PAGE CHANGED                   
         BE    *+8                                                              
         OI    DDS,DDSNEW                                                       
         MVC   SVPAGE,PAGE         SAVE THIS PAGE VALUE                         
*                                                                               
         MVI   PASS,C'I'           INPUT PASS FIRST                             
         NI    DDS,255-DDSEOT                                                   
*                                                                               
         STAR  CLEAR=YES,ARS=OFF                                                
*                                                                               
MAIN04   L     R6,VVRSNTAB                                                      
         LAM   R6,R6,ALET                                                       
         SAC   512                                                              
         USING DMSPACED,R6                                                      
         ICM   R1,15,DSPTEND                                                    
         STCM  R1,15,AVRSNMAX                                                   
         ICM   R6,15,DSPTFRST                                                   
         B     MAIN08                                                           
         DROP  R6                                                               
*                                                                               
MAIN08   LA    R5,SRVSELH          R5=START OF SCREEN                           
         XR    R1,R1               INDEX INTO TABLE                             
         IC    R1,PAGE                                                          
         MH    R1,=H'32'           NUMBER OF ENTRIES ON A PAGE                  
*                                                                               
         LTR   R1,R1               FIRST PAGE?                                  
         BZ    MAIN10              YES                                          
         LR    RF,R1                                                            
         MH    RF,=Y(VRSNNEXT-VRSNTABD)                                         
         AR    R6,RF               INDEX INTO TABLE                             
         C     R6,AVRSNMAX                                                      
         BL    MAIN10                                                           
         REAR  ARS=OFF                                                          
         B     ERR0                EXIT - INVALID INPUT FIELD                   
*                                                                               
MAIN10   MVC   VIRGIN,0(R6)                                                     
         OC    VIRGIN,VIRGIN       TEST FOR EMPTY ENTRY                         
         BZ    MAIN12                                                           
         GOTO1 FILTERS             APPLY FILTER TO VERSION TABLE ENTRY          
         SAFE                                                                   
         BNE   MAIN22                                                           
*                                                                               
MAIN12   TM    DDS,DDSNEW          TEST FOR NEW SCREEN                          
         BO    MAIN14              YES                                          
         CLI   PASS,C'I'           IS THIS INPUT PASS                           
         BNE   MAIN14                                                           
*                                                                               
         GOTO1 VIRVAL              VALIDATE SCREEN ENTRY                        
         SAFE                                                                   
*                                                                               
         CLC   VSAVE,VIRGIN        TEST FOR SOMETHING CHANGED                   
         BE    *+8                                                              
         OI    DDS,DDSUPD          FLAG TABLE UPDATED                           
         B     MAIN16                                                           
*                                                                               
MAIN14   OC    VIRGIN,VIRGIN       TEST FOR EMPTY ENTRY                         
         BZ    MAIN16                                                           
         GOTO1 VIRDISP             DISPLAY ENTRY                                
         SAFE                                                                   
*                                                                               
MAIN16   MVC   0(L'VIRGIN,R6),VIRGIN                                            
         LA    R6,VRSNNEXT-VRSNTABD(R6)                                         
         C     R6,AVRSNMAX                                                      
         BL    MAIN18              NOT REACHED END OF TABLE YET                 
         OI    DDS,DDSEOT                                                       
         B     MAIN24                                                           
*                                                                               
MAIN18   LA    R1,SRVSELLH         BUMP SCREEN POS                              
         CR    R5,R1                                                            
         BE    MAIN20              END OF LEFT HAND SIDE                        
         LA    R1,SRVSELRH                                                      
         CR    R5,R1                                                            
         BE    MAIN24              END OF RIGHT HAND SIDE                       
         LA    R5,VLLINEL(R5)                                                   
         LA    R5,VLLINEL(R5)                                                   
         B     MAIN10              PROCESS NEXT VERSION TAB ENTRY               
*                                                                               
MAIN20   LA    R5,SRVSEL2H         DO RIGHT HAND SINE                           
         B     MAIN10                                                           
*                                                                               
MAIN22   LA    R6,VRSNNEXT-VRSNTABD(R6)                                         
         C     R6,AVRSNMAX                                                      
         BNL   MAIN24              END OF TABLE                                 
         B     MAIN10                                                           
*                                                                               
MAIN24   CLI   PASS,C'D'           WAS THIS A DISPLAY PASS                      
         BE    MAINX                                                            
*                                                                               
         GOTO1 SQUASH              CLOSE UP VERSION TABLE ENTRIES               
         SAFE                                                                   
*                                                                               
         MVI   PASS,C'D'           NO SO GO BACK AND DO IT                      
         LA    R1,SRVSELH                                                       
         BAS   RE,TWAXC            CLEAR SCREEN FIRST                           
         B     MAIN04                                                           
*                                                                               
MAINX    REAR  ARS=OFF             RESET ACCESS REGISTERS                       
*                                                                               
         LA    R1,SRVSELH          START OF SCREEN                              
         ST    R1,CURSOR                                                        
         TM    DDS,DDSEOT                                                       
         BO    INF0                                                             
*                                                                               
         TM    DDS,DDSUPD          TEST FOR UPDATE                              
         BNO   EXIT                                                             
         L     R1,VSSB             SET CHECKPOINT FLAG                          
         OI    SSBSTAT1-SSBD(R1),SSBSCHK1                                       
         B     INF1                                                             
*                                                                               
EXIT     XR    RF,RF               DISPLAY PAGE+1                               
         IC    RF,PAGE                                                          
         LA    RF,1(RF)                                                         
         EDIT  (RF),(3,SRVP1),ALIGN=LEFT                                        
         OC    AHELP,AHELP         USE HELPOUT IF AHELP NOT ZERO                
         BNZ   HELPOUT                                                          
         BAS   RE,WRITESTR         WRITE SAVED STR                              
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         AND SET CURSOR                               
*                                                                               
EXIT1    XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
*        VALIDATE PARMS                                     *                   
*************************************************************                   
         SPACE 1                                                                
PARMVAL  NTR1                                                                   
*                                                                               
P1VAL    MVI   PAGE,0              TOP OF SCREEN                                
         LA    R4,SRVP1H                                                        
         ST    R4,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R4)          TEST FOR NO INPUT                            
         BZ    P1VALX                                                           
         CLI   8(R4),C'?'          TEST FOR HELP                                
         BNE   *+8                                                              
         ST    R4,AHELP                                                         
         LA    R4,8(R4)                                                         
         ST    R3,FULL             SAVE A(TWA)                                  
         LR    R3,R1                                                            
         BAS   RE,VALNUM           VALIDATE PAGE NUMBER                         
         L     R3,FULL             RESTORE A(TWA)                               
         CLI   DUB,X'FF'                                                        
         BE    ERR0                                                             
         CVB   R1,DUB                                                           
         BCTR  R1,0                                                             
         STC   R1,PAGE             SAVE IN PAGE                                 
P1VALX   EQU   *                                                                
*                                                                               
P2VAL    MVI   FLSYS,0             SYSTEM FILTER                                
         LA    R7,SRVP2H                                                        
         ST    R7,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R7)          TEST FOR NO INPUT                            
         BZ    P2VALX                                                           
         CLI   8(R7),C'?'          TEST FOR HELP                                
         BNE   *+8                                                              
         ST    R7,AHELP                                                         
         MVC   GTSYSL,5(R7)        VALIDATE SYSTEM NAME                         
         MVC   GTSYSN,8(R7)                                                     
         BAS   RE,VALOVS                                                        
         BNE   ERR0                                                             
         MVC   FLSYS,GTSYS         SET SYSTEM FILTER                            
P2VALX   CLC   OFLSYS,FLSYS                                                     
         BE    P2VALXX                                                          
         OI    DDS,DDSNEW          SET NEW SCREEN IF FLSYS CHANGES              
         MVC   OFLSYS,FLSYS                                                     
P2VALXX  EQU   *                                                                
         SPACE 1                                                                
PARMVALX B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        READ IN SAVED STORAGE                              *                   
*************************************************************                   
         SPACE 1                                                                
READSTR  NTR1                                                                   
         GOTO1 VHEXIN,DMCB,SRVSAV,SAVED,78                                      
READX    B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        WRITE OUT SAVED STORAGE                            *                   
*************************************************************                   
         SPACE 1                                                                
WRITESTR NTR1                                                                   
         GOTO1 VHEXOUT,DMCB,SAVED,SRVSAV,39                                     
         B     XIT1                                                             
         SPACE 2                                                                
*************************************************************                   
*        TWAXC CLEAR FROM R1 TO END OF SCREEN               *                   
*************************************************************                   
         SPACE 1                                                                
TWAXC    NTR1                                                                   
         TWAXC (R1),SRVFTRH,PROT=N                                              
XIT1     XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*        DISPLAY VERSION TAB ENTRY IN VIRGIN @ R5           *                   
*************************************************************                   
         SPACE 1                                                                
VIRDISP  NTR1                                                                   
         USING VERLINED,R5         OUTPUT LINE ON SCREEN                        
         LA    R4,VIRGIN                                                        
         USING VRSNTABD,R4         COPY OF VERSION TAB ENTRY                    
*                                                                               
VVD000   GOTO1 VHEXOUT,DMCB,VRSNPGM,VLROOT,2                                    
         MVI   VLROOT,C'T'         ROOT PHASE TSPP                              
*                                                                               
         MVC   GTSYS,VRSNPGM                                                    
         BAS   RE,GETOVS           GET SYSTEM NAME                              
         BNE   VVD020                                                           
         MVC   VLSYSP(3),GTSYSN                                                 
         MVI   VLSYSP+3,C'/'                                                    
*                                                                               
VVD010   MVI   GTSYSSE,0                                                        
         BAS   RE,GETSYS           GET A(PGMLST)                                
         BNE   VVD020                                                           
         MVC   GTPROG,VRSNPGM+1                                                 
         BAS   RE,GETPROG          GET PROGRAM NAME                             
         MVC   VLSYSP+4(3),GTPROGN MAKE IT SYS/PRG                              
*                                                                               
VVD020   MVC   GTLANG,VRSNFLAG     EXTRACT LANG FROM FLAGS                      
         NI    GTLANG,X'0F'                                                     
         BAS   RE,GETLANG          GET LANGUAGE                                 
         MVC   VLLANG,GTLANGN                                                   
*                                                                               
         TM    VRSNFLAG,VRSNDATE   TEST PAST EXPIRY DATE                        
         BNO   *+8                                                              
         MVI   VLFLAGS,C'*'        SET * IN FLAGS                               
*                                                                               
VVD030   MVC   VLADV,SPACES                                                     
         CLI   VRSNADV,0                                                        
         BE    VVD040                                                           
         MVC   GTADV,VRSNADV                                                    
         BAS   RE,GETADV           GET ADV NAME                                 
         MVC   VLADV,GTADVN                                                     
*                                                                               
VVD040   MVC   VLSENUM,SPACES                                                   
         CLI   VRSNSEN,0                                                        
         BE    VVD050                                                           
         MVC   GTSYSSE,VRSNSEN                                                  
         BAS   RE,GETSYS           GET SE SYSTEM NAME                           
         MVC   VLSENUM,GTSYSN                                                   
*                                                                               
VVD050   MVC   VLAGY,SPACES        2 CHR AGENCY                                 
         CLI   VLAGY,0                                                          
         BE    VVD060                                                           
         MVC   VLAGY,VRSNAGY                                                    
*                                                                               
VVD060   MVC   VLLEVEL(1),VRSNABC  LEVEL 1/2/3                                  
         OI    VLLEVEL,X'C0'       SET TO A/B/C                                 
         B     XIT1                                                             
VIRDERR  MVC   VLROOT,=C'*ERR'                                                  
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE VERSION TAB ENTRY @ R5                    *                   
*************************************************************                   
         SPACE 1                                                                
VIRVAL   NTR1                                                                   
         USING VERLINED,R5         OUTPUT LINE ON SCREEN                        
         CLI   VLSEL,C'*'          TEST EOT INDICATOR                           
         BE    XIT1                                                             
         MVC   VSAVE,VIRGIN        SAVE OLD VIRGIN                              
         MVC   TSPP1,VIRGIN        SAVE OLD TSPP                                
         XC    TSPP2,TSPP2                                                      
         XC    TSPP3,TSPP3                                                      
         XC    VIRGIN,VIRGIN       XC FOR A VIRGIN VIRGIN                       
         LA    R4,VIRGIN                                                        
         USING VRSNTABD,R4         COPY OF VERSION TAB ENTRY                    
*                                                                               
         CLI   VLSEL,C'D'          TEST FOR DELETE                              
         BE    XIT1                                                             
         LA    R1,VLROOTH          SET CURSOR                                   
         ST    R1,CURSOR                                                        
         CLI   5(R1),0             TEST FOR NO INPUT                            
         BE    VVL010                                                           
*                                                                               
         CLI   VLROOT,C'T'         ROOT PHASE TSPP                              
         BNE   ERR0                                                             
         MVC   FULL,VLROOT                                                      
         MVI   FULL,C'0'                                                        
         GOTO1 VHEXIN,DMCB,FULL,TSPP2,4                                         
         OC    12(4,R1),12(R1)                                                  
         BZ    ERR0                INVALID INPUT                                
*                                                                               
VVL010   LA    R1,VLSYSPH          SET CURSOR                                   
         ST    R1,CURSOR                                                        
         CLI   5(R1),0             TEST FOR NO INPUT                            
         BE    VVL015                                                           
         MVC   GTSYSN(3),VLSYSP                                                 
         MVI   GTSYSL,3                                                         
         BAS   RE,VALOVS           VALIDATE SYSTEM NAME                         
         BNE   ERR0                                                             
         MVC   TSPP3(1),GTSYS                                                   
         CLI   VLSYSP+3,C'/'                                                    
         BNE   ERR0                                                             
*                                                                               
         MVI   GTSYSSE,0                                                        
         BAS   RE,GETSYS           GET A(PGMLST)                                
         BNE   ERR0                                                             
         MVC   GTPROGN(3),VLSYSP+4                                              
         MVI   GTPROGL,3                                                        
         BAS   RE,VALPROG          VALIDATE PROGRAM NAME                        
         BNE   ERR0                                                             
         MVC   TSPP3+1(1),GTPROG                                                
*                                                                               
VVL015   CLC   TSPP2,TSPP3         DO WE AGREE                                  
         BE    VVL018                                                           
         OC    TSPP3,TSPP3         IF 3 IS ZERO 2 WINS                          
         BZ    VVL017                                                           
         OC    TSPP2,TSPP2         IF 2 IS ZERO 3 WINS                          
         BZ    VVL018                                                           
         CLC   TSPP2,TSPP1         IF 2 IS SAME AS OLD 3 WINS                   
         BE    VVL018                                                           
         CLC   TSPP3,TSPP1         IF 3 IS SAME AS OLD 2 WINS                   
         BE    VVL017                                                           
*                                                                               
VVL016   MVC   VRSNPGM,TSPP1       IF ALL ELSE FAILS OLD WINS                   
         B     VVL020                                                           
VVL017   MVC   VRSNPGM,TSPP2                                                    
         B     VVL020                                                           
VVL018   MVC   VRSNPGM,TSPP3                                                    
*                                                                               
VVL020   MVC   VRSNFLAG,VSAVE+VRSNFLAG-VRSNPGM                                  
         NI    VRSNFLAG,X'F0'      COPY FLAGS FROM OLD ENTRY                    
         LA    R1,VLLANGH          SET CURSOR                                   
         ST    R1,CURSOR                                                        
         CLI   5(R1),0             TEST FOR NO INPUT                            
         BE    VVL030                                                           
         MVC   GTLANGN,VLLANG                                                   
         BAS   RE,VALLANG          VALIDATE LANGUAGE                            
         BNE   ERR0                                                             
         OC    VRSNFLAG,GTLANG                                                  
*                                                                               
VVL030   LA    R1,VLADVH           SET CURSOR                                   
         ST    R1,CURSOR                                                        
         CLI   5(R1),0             TEST FOR NO INPUT                            
         BE    VVL040                                                           
         MVC   GTADVN,VLADV                                                     
         BAS   RE,VALADV           VALIDATE ADV NAME                            
         BNE   ERR0                                                             
         MVC   VRSNADV,GTADV                                                    
*                                                                               
VVL040   LA    R1,VLSENUMH         SET CURSOR                                   
         ST    R1,CURSOR                                                        
         CLI   5(R1),0             TEST FOR NO INPUT                            
         BE    VVL050                                                           
         MVC   GTSYSN,VLSENUM                                                   
         OC    GTSYSN,SPACES                                                    
         BAS   RE,VALSYS           VALIDATE SYSTEM NAME                         
         BNE   ERR0                                                             
         MVC   VRSNSEN,GTSYSSE                                                  
*                                                                               
VVL050   LA    R1,VLAGYH           SET CURSOR                                   
         ST    R1,CURSOR                                                        
         CLI   5(R1),0             TEST FOR NO INPUT                            
         BE    VVL060                                                           
         MVC   VRSNAGY,VLAGY                                                    
*                                                                               
VVL060   LA    R1,VLLEVELH         SET CURSOR                                   
         ST    R1,CURSOR                                                        
         CLI   5(R1),0             TEST FOR NO INPUT                            
         BE    VVL070              MISSING INPUT                                
         CLI   VLLEVEL,C'A'                                                     
         BL    ERR0                                                             
         CLI   VLLEVEL,C'C'                                                     
         BH    ERR0                                                             
         MVC   VRSNABC,VLLEVEL     LEVEL A/B/C                                  
         NI    VRSNABC,255-X'C0'   CHANGE TO 1/2/3                              
*                                                                               
         LA    R1,VLSYSPH          SET CURSOR                                   
         ST    R1,CURSOR                                                        
         OC    VRSNPGM,VRSNPGM     MUST HAVE PRGM                               
         BZ    ERR1                                                             
         CLC   VIRGIN,VSAVE        IF SOMETHING CHANGED                         
         BE    XIT1                                                             
         BAS   RE,DUPES            CHECK FOR DUPLICATES                         
         BNE   ERR3                                                             
         B     XIT1                                                             
*                                                                               
VVL070   OC    VIRGIN,VIRGIN       TEST FOR NO INPUT                            
         BZ    XIT1                                                             
         B     ERR1                ELSE MISSING INPUT                           
         EJECT                                                                  
*************************************************************                   
*        TEST FOR FILTERS  EXIT CC=NEQ TO FILTER OUT        *                   
*************************************************************                   
         SPACE 1                                                                
FILTERS  NTR1                                                                   
         LA    R4,VIRGIN                                                        
         USING VRSNTABD,R4         COPY OF VERSION TAB ENTRY                    
         CLI   FLSYS,0                                                          
         BE    GOODXIT             NO FILTER                                    
         CLI   VRSNPGM,0                                                        
         BE    GOODXIT             NULL ENTRY                                   
*                                                                               
         CLC   FLSYS,VRSNPGM       TEST SYSTEM FILTER                           
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        GET GTSYS & GTSYSN FROM GTSYSSE                    *                   
*************************************************************                   
         SPACE 1                                                                
GETSYS   NTR1                                                                   
         L     R4,VSELIST          MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE                                                       
         USING SELISTD,R4                                                       
GETSYS0  CLI   GTSYSSE,0           IF SYSSE IS ZERO                             
         BNE   GETSYS1                                                          
         CLC   GTSYS,SEOVSYS       TEST GTSYS WITH OVSYS                        
         BE    GETSYS2                                                          
GETSYS1  CLC   GTSYSSE,SESYS       TEST SE NUMBER                               
         BE    GETSYS2                                                          
         BXLE  R4,R0,GETSYS0       NEXT                                         
         MVI   GTSYS,0                                                          
         XC    GTSYSN,GTSYSN                                                    
         B     BADXIT              ERROR EXIT NOT FOUND                         
*                                                                               
GETSYS2  MVC   GTSYS,SEOVSYS       FOUND                                        
         MVC   GTSYSN,SENAME       SET NAME                                     
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         B     GOODXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTSYSSE FROM GTSYSN                            *                   
*************************************************************                   
         SPACE 1                                                                
VALSYS   NTR1                                                                   
         L     R4,VSELIST          MUST HAVE SYSFACS                            
         BAS   RE,SETBXLE          SET BXLE                                     
         USING SELISTD,R4                                                       
VALSYS0  CLC   GTSYSN,SENAME       TEST NAME                                    
         BE    VALSYS1                                                          
         BXLE  R4,R0,VALSYS0       NEXT                                         
         MVI   GTSYS,0                                                          
         XC    GTSYSN,GTSYSN                                                    
         B     BADXIT              ERROR EXIT NOT FOUND                         
*                                                                               
VALSYS1  MVC   GTSYS,SEOVSYS       FOUND                                        
         MVC   GTSYSSE,SESYS       SET NUMBER                                   
         MVC   GTAPGMS,SEPGMS      SAVE A(PGMS)                                 
         B     GOODXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTPROGN FROM GTPROG                            *                   
*************************************************************                   
         SPACE 1                                                                
GETPROG  NTR1                                                                   
         ICM   R4,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         BZ    GETPRG2                                                          
         BAS   RE,SETBXLE          SET BXLE                                     
         USING PGMLSTD,R4                                                       
GETPRG0  CLC   GTPROG,PGMNUM       TEST PROG NUMBER                             
         BE    GETPRG1                                                          
         BXLE  R4,R0,GETPRG0       NEXT                                         
GETPRG2  XC    GTPROGN,GTPROGN     NOT FOUND SO HEXOUT NUMBER                   
         GOTO1 VHEXOUT,DMCB,GTPROG,GTPROGN,1                                    
         B     GOODXIT                                                          
GETPRG1  MVC   GTPROGN,PGMNAME     SET NAME                                     
         B     GOODXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTPROG FROM GTPROGN                            *                   
*************************************************************                   
         SPACE 1                                                                
VALPROG  NTR1                                                                   
         CLI   GTPROGN+2,0         FIX FOR 2CHR PROG NAMES                      
         BNE   *+8                                                              
         MVI   GTPROGN+2,C' '                                                   
         ICM   R4,15,GTAPGMS       MUST HAVE A(PRGMS)                           
         BZ    VALPRG2                                                          
         BAS   RE,SETBXLE          SET BXLE                                     
         SR    RE,RE                                                            
         IC    RE,GTPROGL                                                       
         BCTR  RE,0                                                             
         USING PGMLSTD,R4                                                       
VALPRG0  EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   GTPROGN(0),PGMNAME  TEST NAME                                    
         BE    VALPRG1                                                          
         BXLE  R4,R0,VALPRG0       NEXT                                         
*                                                                               
         GOTO1 VHEXIN,DMCB,GTPROGN,GTPROG,2                                     
         OC    12(4,R1),12(R1)     TRY FOR HEX LAST                             
         BNZ   GOODXIT                                                          
*                                                                               
VALPRG2  XC    GTPROGN,GTPROGN     NOT FOUND                                    
         B     BADXIT                                                           
VALPRG1  MVC   GTPROG,PGMNUM       FOUND                                        
         B     GOODXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTLANGN FROM GTLANG                            *                   
*************************************************************                   
         SPACE 1                                                                
GETLANG  NTR1                                                                   
         L     R4,ALANG            MUST HAVE A(LANGTAB)                         
         BAS   RE,SETBXLE          SET BXLE                                     
         USING LANGTABD,R4                                                      
GETLAN0  CLC   GTLANG,LANGCODE     TEST LANGUAGE CODE                           
         BE    GETLAN1                                                          
         BXLE  R4,R0,GETLAN0       NEXT                                         
         MVC   GTLANGN,SPACES                                                   
         B     BADXIT              ERROR EXIT NOT FOUND                         
GETLAN1  MVC   GTLANGN,LANGSHR     SET NAME                                     
         B     GOODXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTLANG FROM GTLANGN                            *                   
*************************************************************                   
         SPACE 1                                                                
VALLANG  NTR1                                                                   
         L     R4,ALANG            MUST HAVE A(LANGTAB)                         
         BAS   RE,SETBXLE          SET BXLE                                     
         USING LANGTABD,R4                                                      
VALLAN0  CLC   GTLANGN,LANGSHR     TEST NAME                                    
         BE    VALLAN1                                                          
         BXLE  R4,R0,VALLAN0       NEXT                                         
         MVI   GTLANG,0                                                         
         B     BADXIT              ERROR EXIT NOT FOUND                         
VALLAN1  MVC   GTLANG,LANGCODE     FOUND                                        
         B     GOODXIT                                                          
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*        GET GTSYS FROM SYSTEM NAME                         *                   
*************************************************************                   
         SPACE 1                                                                
VALOVS   NTR1                                                                   
         LA    RE,SYSLST           RE=SYSLST                                    
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
         SR    R1,R1               R1=LEN-1 FOR COMPARE                         
         IC    R1,GTSYSL                                                        
         BCTR  R1,0                                                             
         CLI   GTSYSL,3            DO WE HAVE >3 CHRS                           
         BH    VALS050                                                          
*                                                                               
VALS010  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    VALS050                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLSHRT(0),GTSYSN  TEST SHORT NAME                              
         BE    VALS990                                                          
VALS012  LA    RE,SYSLLEN(RE)      NEXT                                         
         B     VALS010                                                          
*                                                                               
VALS050  LA    RE,SYSLST           RE=SYSLST                                    
         LA    RE,6(RE)                                                         
VALS051  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    VALS090                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),GTSYSN  TEST LONG NAME                               
         BE    VALS990                                                          
         LA    RE,SYSLLEN(RE)      NEXT                                         
         B     VALS051                                                          
*                                                                               
VALS090  B     BADXIT              SET CC NEQ NOT FOUND                         
VALS990  MVC   GTSYS,SYSLNUM                                                    
         LR    R1,RE               R1=A(SYSLST ENTRY)                           
         B     GOODXIT             SET CC EQU                                   
         DROP  RE                                                               
         EJECT                                                                  
*************************************************************                   
*        GET SYSTEM NAME FROM GTSYS                         *                   
*************************************************************                   
         SPACE 1                                                                
GETOVS   NTR1                                                                   
         LA    RE,SYSLST           RE=SYSLST                                    
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
GETS990  MVC   GTSYSN,SYSLSHRT                                                  
         LR    R1,RE               R1=A(SYSLST ENTRY)                           
         B     GOODXIT             SET CC EQU                                   
         DROP  RE                                                               
         EJECT                                                                  
*************************************************************                   
*        GET SYSTEM NAME FROM GTADV                         *                   
*************************************************************                   
         SPACE 1                                                                
GETADV   NTR1                                                                   
         LA    RE,FACIDTAB         RE=FACIDTAB                                  
GETAD010 CLC   GTADV,4(RE)         TEST ADV NUMBER                              
         BE    GETAD020                                                         
         CLI   4(RE),8             IF WE ARE ON SYS8 THEN NOT FOUND             
         BE    BADXIT                                                           
         LA    RE,8(RE)                                                         
         B     GETAD010                                                         
*                                                                               
GETAD020 MVC   GTADVN,0(RE)                                                     
         B     GOODXIT             SET CC EQU                                   
         EJECT                                                                  
*************************************************************                   
*        GET SYSTEM GTADV FROM SYSTEM NAME                  *                   
*************************************************************                   
         SPACE 1                                                                
VALADV   NTR1                                                                   
         LA    RE,FACIDTAB         RE=FACIDTAB                                  
         CLI   GTADVN+3,0          FIX FOR 3 CHR ADV NAME                       
         BNE   *+8                                                              
         MVI   GTADVN+3,C' '                                                    
VALAD010 CLC   GTADVN,0(RE)        TEST 4CHR ADV NAME                           
         BE    VALAD020                                                         
         CLI   4(RE),8             IF WE ARE ON SYS8 THEN NOT FOUND             
         BE    BADXIT                                                           
         LA    RE,8(RE)                                                         
         B     VALAD010                                                         
*                                                                               
VALAD020 MVC   GTADV,4(RE)         FOUND IT                                     
         B     GOODXIT             SET CC EQU                                   
         EJECT                                                                  
*************************************************************                   
*        SQUASH OUT ZERO ENTRIES FROM VRSNTAB               *                   
*************************************************************                   
         SPACE 1                                                                
SQUASH   NTR1  ,                                                                
         XC    ALAST,ALAST                                                      
         LAM   R1,R1,ALET                                                       
         L     R1,VVRSNTAB                                                      
         USING DMSPACED,R1                                                      
         SAC   512                                                              
         XR    R6,R6                                                            
         ICM   R6,3,DSPTWIDE       SET BXLE ON R6/7                             
         ICM   R7,15,DSPTEND                                                    
         ICM   R1,15,DSPTFRST      SET CURRENT POINT IN R6                      
         LA    R1,0(R1)            AND CLEAR HOB                                
         USING VRSNTABD,R1                                                      
*                                                                               
SQUASH04 OC    VRSNPGM,VRSNPGM     TEST FOR EMPTY SLOT                          
         BNZ   SQUASH06            NOT EMPTY                                    
         OC    ALAST,ALAST         ALREADY HAVE EMPTY SLOT?                     
         BNZ   SQUASH08            YES                                          
*                                                                               
         ST    R1,ALAST            NO - SET THIS AS FIRST EMPTY SLOT            
         B     SQUASH08                                                         
*                                                                               
SQUASH06 OC    ALAST,ALAST         DO WE HAVE AN EMPTY SLOT TO FILL?            
         BZ    SQUASH08            NO                                           
*                                                                               
         CPYA  R2,R1               SET UP MVCL TO R2/3 FROM R4/5                
         L     R2,ALAST            R2=TO ADDRESS                                
         LA    R3,1(R7)            R3=A(EOT)                                    
         SR    R3,R2               R3=DISTANCE(EOT-FIRST EMPTY)                 
*                                                                               
         CPYA  R4,R1                                                            
         LR    R4,R1               R4=A(CURRENT)                                
         LA    R5,1(R7)            R5=A(EOT)                                    
         SR    R5,R4               R5=DISTANCE(EOT-CURRENT)                     
*                                                                               
         MVCL  R2,R4               MOVE OVER EMPTY SLOTS                        
*                                                                               
         L     R1,ALAST            MAKE SURE ALL IS WELL HERE                   
         XC    ALAST,ALAST         CLEAR A(LAST SPACE)                          
         LAM   R2,R2,=F'0'                                                      
         LAM   R4,R4,=F'0'         MAKE SURE YOU CLEAR THE ARS                  
*                                                                               
SQUASH08 BXLE  R1,R6,SQUASH04      NEXT                                         
         B     XIT1                                                             
         DROP  R1                                                               
         EJECT                                                                  
*************************************************************                   
*        CHECK FOR DUPLICATES CC=NEQ DUPLICATE FOUND        *                   
*************************************************************                   
         SPACE 1                                                                
DUPES    NTR1  ,                                                                
         STAR  CLEAR=Y,ARS=OFF                                                  
         LAM   R4,R4,ALET                                                       
         L     R4,VVRSNTAB                                                      
         USING DMSPACED,R4                                                      
         SAC   512                                                              
         XR    R0,R0                                                            
         ICM   R0,3,DSPTWIDE       SET BXLE ON R6/7                             
         ICM   R1,15,DSPTEND                                                    
         ICM   R4,15,DSPTFRST      SET CURRENT POINT IN R6                      
         LA    R4,0(R4)            AND CLEAR HOB                                
         USING VRSNTABD,R4                                                      
*                                                                               
DUPE02   CLC   VIRGIN,VRSNPGM      TEST FOR DUPLICATES                          
         BE    DUPEYES                                                          
         BXLE  R4,R0,DUPE02        NEXT                                         
*                                                                               
DUPENO   REAR  ARS=OFF             NO DUPLICATES                                
         B     GOODXIT                                                          
*                                                                               
DUPEYES  REAR  ARS=OFF             DUPLICATE FOUND                              
         B     BADXIT                                                           
         EJECT                                                                  
       ++INCLUDE DDVALNUM                                                       
         EJECT                                                                  
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
GOODXIT  CR    RB,RB                                                            
         B     XIT1                                                             
BADXIT   LTR   RB,RB                                                            
         B     XIT1                                                             
ERR0     LA    R0,2                INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR1     LA    R0,1                MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R0,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR3     LA    R0,192              DUPLICATE TABLE ENTRY                        
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     L     RD,BASERD                                                        
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),(4,FACID)                           
         B     EXIT                                                             
*                                                                               
INF0     LA    R0,177              END OF TABLE                                 
         B     INFX                                                             
INF1     LA    R0,185              VERSION TABLE UPDATED                        
         B     INFX                                                             
INFX     XC    DMCB(24),DMCB                                                    
         GOTO1 VGETTXT,DMCB,(R0),0,(C'I',0),(4,FACID)                           
         B     EXIT                                                             
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
         GOTO1 VGETHELP,DMCB,(X'50',HELPKEY),0,(C'B',0)                         
         DC    H'0'                                                             
HELPTAB  LA    R1,SRVP1H           POSSIBLE HELP FIELDS                         
         LA    R1,SRVP2H                                                        
         LA    R1,SRVP3H                                                        
         LA    R1,SRVP4H                                                        
         LA    R1,SRVSELH          <--- OR GREATER                              
         DC    H'0'                                                             
         EJECT                                                                  
******************************************************                          
*        CONSTANTS                                   *                          
******************************************************                          
         SPACE 1                                                                
DOTS     DC    40C'.'                                                           
SPACES   DC    80C' '                                                           
VERD     DC    C'$VER'                                                          
DMREAD   DC    CL7'DMREAD'                                                      
DMWRT    DC    CL7'DMWRT'                                                       
TEMPSTR  DC    CL7'TEMPSTR'                                                     
HELPID   DC    XL10'0145FF00010000000000'                                       
DUMMY    BR    RE                                                               
*             ***....+....1....+....2....+....3....+....***                     
FLHEADER DC    C'S Root Sys/Prg Lng Adv  SE-sys  Ag Lv  '                       
         DC    C' S Root Sys/Prg Lng Adv  SE-sys  Ag Lv  '                      
FLUNDER  DC    C'- ---- ------- --- ---- ------- -- --- '                       
         DC    C' - ---- ------- --- ---- ------- -- --- '                      
*                                                                               
       ++INCLUDE FASYSLST                                                       
       ++INCLUDE FACIDTAB                                                       
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
DDS      DS    X                                                                
DDSNEW   EQU   X'80'               NEW SESSION                                  
DDSEOT   EQU   X'40'               END OF TABLE ENCOUNTERED                     
DDSUPD   EQU   X'20'               TABLE UPDATED                                
DMCB     DS    6F                                                               
WORK     DS    CL80                                                             
*                                                                               
VIRGIN   DS    CL8                 VERSION TAB ENTRY                            
VSAVE    DS    CL8                 SAVED VERSION TAB ENTRY                      
TSPP1    DS    CL2                 SYS/PRG ARGUMENTS                            
TSPP2    DS    CL2                                                              
TSPP3    DS    CL2                                                              
PASS     DS    CL1                                                              
*                                                                               
FLSYS    DS    X                   SYSTEM FILTER                                
*                                                                               
GTSYS    DS    X                   OV SYSTEM                                    
GTSYSSE  DS    X                   SE SYSTEM                                    
GTSYSL   DS    X                   LEN FOR OVSYS NAME SEARCH                    
GTSYSN   DS    CL7                 SYS NAME                                     
GTPROGL  DS    X                   LEN FOR PROG NAME SEARCH                     
GTPROGN  DS    CL7                 PROG NAME                                    
GTPROG   DS    X                   PROG NUMBER                                  
GTLANGN  DS    CL3                 LANGUAGE NAME                                
GTLANG   DS    X                   LANGUAGE CODE                                
GTAPGMS  DS    A                   A(PRGMS LIST FOR SYSTEM)                     
GTADV    DS    X                   ADV NUMBER                                   
GTADVN   DS    CL4                 ADV 4CHR SYSTEM NAME                         
*                                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
VGETHELP DS    A                                                                
ASAVE    DS    A                                                                
ALANG    DS    A                                                                
ALAST    DS    A                                                                
AVRSNMAX DS    A                                                                
BASERD   DS    A                                                                
RELO     DS    A                                                                
*                                                                               
CURSOR   DS    A                                                                
SAVERE   DS    A                                                                
AHELP    DS    A                                                                
ALET     DS    A                                                                
PFKEY    DS    X                                                                
COUNT    DS    X                                                                
TRM      DS    H                                                                
RECLEN   DS    H                                                                
FACID    DS    CL4                                                              
FACNA    DS    CL3                                                              
PAGE     DS    X                                                                
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
SAVED    DS    0CL39               SAVE PART OF TWA                             
OFLSYS   DS    CL1                 OLD SYSTEM FILTER                            
SVPAGE   DS    CL1                                                              
         DS    CL37                                                             
*                                                                               
IOAREA   DS    600D                4K WORK AREA                                 
WRKX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
VERLINED DSECT                                                                  
VLSELH   DS    CL8                 SELECT                                       
VLSEL    DS    CL1                                                              
VLROOTH  DS    CL8                 ROOT PHASE TSPP                              
VLROOT   DS    CL4                                                              
VLSYSPH  DS    CL8                 SYS/PROG                                     
VLSYSP   DS    CL7                                                              
VLLANGH  DS    CL8                 LANG                                         
VLLANG   DS    CL3                                                              
VLADVH   DS    CL8                 ADV                                          
VLADV    DS    CL4                                                              
VLSENUMH DS    CL8                 SE SYSTEM                                    
VLSENUM  DS    CL7                                                              
VLAGYH   DS    CL8                 AGENCY                                       
VLAGY    DS    CL2                                                              
VLLEVELH DS    CL8                 LEVEL                                        
VLLEVEL  DS    CL2                                                              
VLFLAGS  DS    CL2                 FLAGS                                        
VLLINEL  EQU   *-VERLINED                                                       
         EJECT                                                                  
*DDFLDHDR                                                                       
*DDCOMFACS                                                                      
*FADSECTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
         EJECT                                                                  
SRVERFFD DSECT                                                                  
         DS    CL64                                                             
*SRVERFFD                                                                       
*FASYSLSTD                                                                      
*DMSPACED                                                                       
       ++INCLUDE SRVERFFD                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DMSPACED                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRVER00X  08/22/00'                                      
         END                                                                    
