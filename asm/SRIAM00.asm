*          DATA SET SRIAM00    AT LEVEL 019 AS OF 03/06/08                      
*PHASE T12600A                                                                  
         TITLE '$IAM - TRANSFER TERMINAL DATA'                                  
         PRINT NOGEN                                                            
IAM      CSECT                                                                  
         NMODL WORKL,**$IAM**,RA                                                
         USING WORKD,RC                                                         
         USING SRPARMD,R1                                                       
         ST    RD,SAVERD                                                        
         ST    R1,APARM                                                         
         MVC   SRPARS,SRPARM1      MOVE CALLING PARAMS                          
         DROP  R1                                                               
         L     R3,SRPAR6           R3=A(TWA)                                    
         USING TWAD,R3                                                          
         L     R9,SRPAR1           R9=A(SYSFAC)                                 
         USING SYSFACD,R9                                                       
         L     R4,SRPAR4                                                        
         USING COMFACSD,R4                                                      
         MVC   VTERMVAL,CTERMVAL                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VGETFACT,CGETFACT                                                
         DROP  R4                                                               
         SPACE 2                                                                
         L     RE,SRPAR8                                                        
         MVC   PFKEY,TIOBAID-TIOBD(RE)                                          
         SPACE 2                                                                
INIT     L     RE,VSSB             EXTRACT SSB DATA                             
         MVC   SYSNA,SSBSYSN4-SSBD(RE)                                          
         MVC   RECLEN,SSBTWAL-SSBD(RE)                                          
         MVC   TMSLEN,SSBTMSL-SSBD(RE)                                          
INIT1    CLC   RECLEN,=H'18432'    18K TEMPSTR DISPLACEMENTS                    
         BNE   INIT2                                                            
         MVC   GLODSP,=Y(CHKPTGLD)                                              
         MVC   CHKDSP,=Y(CHKPTDSP)                                              
         B     INIT3                                                            
INIT2    CLC   RECLEN,=H'14336'    14K TEMPSTR DISPLACEMENTS                    
         BNE   INIT3                                                            
         MVC   GLODSP,=H'12288'                                                 
         MVC   CHKDSP,=H'12800'                                                 
*                                                                               
INIT3    BRAS  RE,ON31                                                          
         L     R5,SRPAR3           R5=UTL SET CURSOR FLAG                       
         USING UTLD,R5                                                          
         OI    TSVCREQ,X'02'                                                    
         MVI   DDS,0               INITIALIZE TERMINAL FLAG                     
         TM    TSTAT,X'60'                                                      
         BZ    *+8                                                              
         MVI   DDS,X'01'           SET DDS TERMINAL                             
         TM    TSTAT7,TST7PQPU                                                  
         BZ    *+8                                                              
         OI    DDS,X'02'           SET PRIVILEGED USER                          
         DROP  R5                                                               
*                                                                               
         MVC   SRVHD1,HEADER1      SET UP HEADERS                               
         MVC   SRVHD2,HEADER2                                                   
         MVC   SRVPFK,PFKEYS                                                    
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM                                                        *         
***********************************************************************         
         SPACE 1                                                                
MAIN     CLI   PFKEY,4             PF4 SWAP TO ADVN                             
         BNE   MAIN000                                                          
         BAS   RE,SWAPADV                                                       
         B     MAINX                                                            
MAIN000  MVI   ACTN,1                                                           
         LA    R2,SRVP1H                                                        
         BAS   RE,VALSR            VALIDATE SR FIELD                            
         BAS   RE,VALP1            VALIDATE P1 FIELD                            
         CLI   ACTN,1                                                           
         BNE   MAIN001                                                          
         BAS   RE,VALSEL           CHECK SELECT ACTIONS                         
         CLI   ACTN,1                                                           
         BNE   MAIN001                                                          
         BAS   RE,TLIST            GOTO LIST CODE                               
         B     INF01                                                            
*                                                                               
MAIN001  BAS   RE,VLSOURC          CHECK SOURCE TERMINAL                        
         CLI   ACTN,3              TEST FOR =IAM / COPY                         
         BNE   MAIN01                                                           
         BAS   RE,IM1              DO IAM1                                      
         B     MAINX                                                            
*                                                                               
MAIN01   CLI   ACTN,2              TEST FOR =PEEK                               
         BNE   MAIN02                                                           
         BAS   RE,PEEK             DO PEEK                                      
         B     MAINX                                                            
*                                                                               
MAIN02   CLI   ACTN,4              TEST FOR LINK                                
         BNE   MAIN03                                                           
         MVI   FLAG,X'20'          SET FULL PEEK                                
         BAS   RE,PEEK             DO PEEK FIRST                                
         BAS   RE,LINK             THEN SET UP LINK                             
         B     MAINX                                                            
*                                                                               
MAIN03   DC    H'0'                                                             
*                                                                               
MAINX    B     XMOD1                                                            
*                                                                               
XIT1     XIT1                                                                   
*                                                                               
XMOD1    BRAS  RE,OFF31                                                         
         L     RD,SAVERD           BACK TO BASE RD                              
         XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LIST TERMINALS THAT ARE IN =WHOAMI =TERM OR HAVE TST6IAMS           *         
***********************************************************************         
         SPACE 1                                                                
TLIST    NTR1                                                                   
         LA    R2,SRVSL1H                                                       
         USING LINED,R2                                                         
         L     R1,VUTL             SET UTL BXLE                                 
         LH    R4,0(R1)                                                         
         L     R5,2(R1)                                                         
         LA    R6,6(R1)                                                         
         USING UTLD,R6                                                          
*                                                                               
TLIST010 CLI   TSVCREQ+1,X'28'     TEST WHOAMI                                  
         BE    TLIST020                                                         
         TM    TSTAT6,TST6IAMS     OR IAM SOURCE                                
         BO    TLIST020                                                         
         B     TLIST800                                                         
*                                                                               
TLIST020 EDIT  (B2,TNUM),(5,LINNUM),FILL=0                                      
         MVC   LINLUID(8),TSYM     DISPLAY TERMINAL DATA                        
         MVC   GIUSER,TUSER                                                     
         BAS   RE,GETUSER                                                       
         MVC   LINUSER,GIUSERID                                                 
         MVC   GTSYSSE,TSYS                                                     
         BAS   RE,GETSYS                                                        
         MVC   LINSYS,GTSYSN                                                    
         MVC   GTPROG,TPRG                                                      
         BAS   RE,GETPROG                                                       
         MVC   LINPRG,GTPROGN                                                   
*                                                                               
         LA    R2,LINELEN(R2)                                                   
         LA    R1,SRVPFK           TEST FOR END                                 
         CR    R1,R2                                                            
         BNH   TLISTX                                                           
*                                                                               
TLIST800 BXLE  R6,R4,TLIST010      NEXT TERMINAL                                
*                                                                               
TLISTX   B     XIT1                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* TEST SELECT FIELDS                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   NTR1                                                                   
         LA    R2,SRVSL1H                                                       
         USING LINED,R2                                                         
VALS010  CLI   SELHDR+5,0                                                       
         BNE   VALS020                                                          
         LA    R2,LINELEN(R2)                                                   
         LA    R1,SRVPFK           TEST FOR END                                 
         CR    R1,R2                                                            
         BH    VALS010                                                          
         B     XIT1                                                             
*                                                                               
VALS020  GOTO1 VTERMVAL,DMCB,NUMHDR,0                                           
         ICM   R5,15,DMCB+4        R5=A(UTL ENTRY OF NAMED TERM)                
         BZ    ERROR1                                                           
         ST    R5,SVUTL            SVUTL=A(UTL ENTRY OF NAMED TERM)             
         USING UTLD,R5                                                          
         MVC   LUID,TSYM           EXTRACT LUID OF NAMED TERMINAL               
*                                                                               
         CLI   SELDATA,C'S'        SELECT ACTION 4                              
         BNE   *+12                                                             
         MVI   ACTN,4                                                           
         B     XIT1                                                             
         CLI   SELDATA,C'C'        COPY ACTION 3                                
         BNE   *+12                                                             
*NOP*    MVI   ACTN,3                                                           
         MVI   ACTN,2                                                           
         B     XIT1                                                             
         B     ERROR7                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE LUID IN S/R FIELD                                          *         
* SET FLAGS DEPENDING ON WHY WE ARE HERE - EITHER $IAM OR $PEEK       *         
***********************************************************************         
         SPACE 1                                                                
VALSR    NTR1                                                                   
         XC    LUID,LUID           CLEAR LUID,SVUTL & FLAG                      
         XC    SVUTL,SVUTL                                                      
         MVI   FLAG,0                                                           
         LA    R2,SRVIDH           TEST WHICH S/R INPUT                         
*                                                                               
VALSRA   CLC   SRVID+1(4),=C'PEEK' =PEEK                                        
         BNE   VALSRB                                                           
         MVI   ACTN,2                                                           
         LA    R1,4                                                             
         MVI   FLAG,0              SET SHORT VERSION                            
         B     VALSRD                                                           
*                                                                               
VALSRB   CLC   SRVID+1(3),=C'IAM'  =IAM/=WATCH                                  
         BE    *+14                                                             
         CLC   SRVID+1(5),=C'WATCH'                                             
         BNE   VALSRC                                                           
*NOP*    LA    R1,3                =IAM SOXED OUT - SET TO =PEEK                
         LA    R1,2                                                             
         MVI   FLAG,1              SET FULL VERSION REQUIRED                    
         B     VALSRD                                                           
*                                                                               
VALSRC   DC    H'0'                                                             
*                                                                               
VALSRD   LA    RF,SRVID+1(R1)      TEST IF $...,LUID INPUT                      
         CLI   0(RF),C','                                                       
         BNE   XIT1                                                             
         LA    RE,LUID                                                          
         LA    RF,1(RF)                                                         
         LA    R0,9                                                             
VALSRE   CLI   0(RF),C','          SCAN FOR END OF LUID                         
         BE    VALSRF                                                           
         CLI   0(RF),C' '                                                       
         BNH   VALSRG                                                           
         MVC   0(1,RE),0(RF)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VALSRE                                                        
         B     ERROR1              LUID TOO LONG                                
*                                                                               
VALSRF   CLI   1(RF),C'C'                                                       
         BNE   *+12                                                             
         OI    FLAG,X'80'          $...,LUID,C TO SEE S/R CONNECT DATA          
         B     VALSRG                                                           
*                                                                               
         CLI   1(RF),C'S'                                                       
         BNE   *+12                                                             
         OI    FLAG,X'40'          $...,LUID,S TO SEE S/R ACTUAL DATA           
         B     VALSRG                                                           
*                                                                               
VALSRG   OC    LUID,LUID           TEST IF LUID INPUT                           
         BZ    ERROR1                                                           
         GOTO1 VTERMVAL,DMCB,(X'80',LUID),0                                     
         ICM   R5,15,DMCB+4        R5=A(UTL ENTRY OF NAMED TERM)                
         BZ    ERROR1                                                           
         ST    R5,SVUTL            SVUTL=A(UTL ENTRY OF NAMED TERM)             
         USING UTLD,R5                                                          
         MVC   LUID,TSYM           EXTRACT LUID OF NAMED TERMINAL               
*                                                                               
VALSRH   MVI   ACTN,2              SET ACTION TO PEEK                           
         TM    FLAG,1                                                           
         BZ    *+8                                                              
*NOP*    MVI   ACTN,3              SET ACTION TO COPY                           
         MVI   ACTN,2                                                           
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE P1 - CAN BE NUMBER OR A LUID                               *         
***********************************************************************         
         SPACE 1                                                                
VALP1    NTR1                                                                   
         LA    R2,SRVP1H           CAN ALTERNATIVELY INPUT IT IN P1             
         CLI   5(R2),0                                                          
         BE    XIT1                                                             
*                                                                               
         GOTO1 VTERMVAL,DMCB,(R2),0                                             
*                                                                               
VALPA1   ICM   R5,15,DMCB+4        R5=A(UTL ENTRY OF NAMED TERM)                
         BZ    ERROR1                                                           
         ST    R5,SVUTL            SVUTL=A(UTL ENTRY OF NAMED TERM)             
         USING UTLD,R5                                                          
         MVC   LUID,TSYM           EXTRACT LUID OF NAMED TERMINAL               
         CLI   ACTN,2                                                           
         BE    *+8                                                              
*NOP*    MVI   ACTN,3              SET ACTION COPY                              
         MVI   ACTN,2                                                           
*                                                                               
VALP2    CLC   SRVID+1(5),=C'WATCH'                                             
         BE    VALP2W                                                           
         LA    R2,SRVP2H           TEST P2 FOR PARMS                            
         CLI   5(R2),0                                                          
         BE    XIT1                                                             
*                                                                               
         CLC   SRVP2(5),=C'WATCH'                                               
         BNE   XIT1                                                             
VALP2W   MVI   ACTN,4                                                           
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE SOURCE TERMINAL                                            *         
***********************************************************************         
         SPACE 1                                                                
VLSOURC  NTR1                                                                   
         ICM   R5,15,SVUTL                                                      
         BZ    ERROR1                                                           
         OC    TPRNT,TPRNT         ERROR IF PRINTER                             
         BNZ   ERROR1                                                           
         CLI   ACTN,4              NOT CONNECTED IS OK TO WATCH                 
         BE    *+14                                                             
         OC    TUSER,TUSER         ERROR IF NOT CONNECTED                       
         BZ    ERROR2                                                           
*&&UK                                                                           
         CLC   TAGY,=C'$D'         BANNED AGENCIES FOR UK                       
         BE    ERROR1                                                           
         CLC   TAGY,=C'XD'                                                      
         BE    ERROR1                                                           
         CLC   TAGY,=C'XA'                                                      
         BE    ERROR1                                                           
         CLC   TAGY,=C'XG'                                                      
         BE    ERROR1                                                           
*&&                                                                             
*&&US                                                                           
         CLC   TAGY,=C'**'         BANNED AGENCIES FOR US                       
         BE    ERROR1                                                           
*&&                                                                             
         L     R5,SRPAR3           GET CALLING TERMINAL UTL                     
         TM    DDS,X'01'           TEST DDS TERMINAL                            
         BO    XIT1                YES                                          
*                                                                               
         L     R6,SVUTL            NO MUST BE SAME USER/SYS/PGM                 
         CLC   TUSER,TUSER-UTLD(R6)                                             
         BNE   ERROR3                                                           
*                                                                               
         CLI   ACTN,4              =WATCH OK FOR SAME USERID                    
         BE    XIT1                                                             
*                                                                               
         CLC   TSYS(2),TSYS-UTLD(R6)                                            
         BNE   ERROR3                                                           
*                                                                               
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* PEEK VERSION. JUST WANT TO SHOW THE SCREEN THAT IS ON THE NAMED TRM *         
* ON OUR TERMINAL. WE WILL SET ALL UNPROT FIELDS TO PROTECTED AND SET *         
* =PEEK,LUID IN THE S/R FIELD SO THAT WE CAN COME BACK AGAIN.         *         
***********************************************************************         
         SPACE 1                                                                
PEEK     NTR1                                                                   
         L     RE,SVUTL            READ NAMED TERM'S TWA INTO OUR TWA           
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+10(2),0(RE)                                                 
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,(R3),TO24=Y                        
         BRAS  RE,ON31                                                          
         CLI   8(R1),0                                                          
         BNE   ERROR5                                                           
         LR    RE,R3               EXRACT S/R CONNECT DATA FIELD                
         AH    RE,CHKDSP                                                        
         MVC   SVCTDATA,CHSRMSG-CHKPTD(RE)                                      
*                                                                               
PEEK1    LA    R2,64(R3)           R2=A(NEXT FIELD IN TWA)                      
         USING FHD,R2                                                           
*                                                                               
         LA    RF,8(R2)                                                         
         ST    RF,AMFDATA          SAVE A(MESSAGE FIELD DATA)                   
         SR    R1,R1                                                            
         OI    FHAT,FHATPR         SET MESSAGE FIELD ATTRIBUTES                 
         MVI   FHOI,FHOITR                                                      
         IC    R1,0(R2)                                                         
         CHI   R1,8                                                             
         BL    ERROR6                                                           
         AR    R2,R1                                                            
         LA    RF,8(R2)                                                         
         ST    RF,ASRDATA          SAVE A(SERVICE REQUEST DATA)                 
         TM    FLAG,X'20'                                                       
         BO    PEEK2                                                            
         MVC   SVSRDATA,0(RF)      SAVE SERVICE REQUEST FIELD                   
         XC    0(17,RF),0(RF)                                                   
         MVC   0(6,RF),=C'=PEEK,'  SET IT TO =PEEK,LUID                         
         MVC   6(8,RF),LUID                                                     
         NI    FHAT,255-FHATPR     SET S/R FIELD ATTRIBUTES                     
         MVC   DUB(1),FHOI                                                      
         MVI   FHOI,FHOITR+FHOIMO                                               
         NI    FHOI,255-FHOICU                                                  
*                                                                               
PEEK2    ICM   R1,1,FHLN            BUMP TO NEXT SCREEN FIELD                   
         BZ    PEEK3                                                            
         CHI   R1,1                                                             
         BE    ERROR6                                                           
         AR    R2,R1                                                            
         OI    FHAT,FHATPR         SET FIELD ATTRIBUTES                         
         TM    FHOI,FHOICU                                                      
         BZ    *+8                                                              
         OI    FHAT,FHATHI                                                      
         MVI   FHOI,FHOITR                                                      
         B     PEEK2                                                            
*                                                                               
PEEK3    MVC   0(3,R2),=X'001000'  SET END-OF-TWA/CLEAR BEFORE                  
         CLI   FLAG,0                                                           
         BE    PEEKX                                                            
         TM    FLAG,X'20'                                                       
         BO    PEEKX                                                            
*                                                                               
PEEK4    TM    FLAG,X'80'          TEST IF WANT S/R CONNECT DATA                
         BZ    PEEK5                                                            
         L     RE,AMFDATA                                                       
         MVI   41(RE),C'<'                                                      
         MVC   42(17,RE),SVCTDATA                                               
         MVI   59(RE),C'>'                                                      
         B     PEEKX                                                            
*                                                                               
PEEK5    TM    FLAG,X'40'          TEST IF WANT ACTUAL S/R DATA                 
         BZ    PEEKX                                                            
         L     RE,AMFDATA                                                       
         MVI   41(RE),C'<'                                                      
         MVC   42(17,RE),SVSRDATA                                               
         MVI   59(RE),C'>'                                                      
         B     PEEKX                                                            
*                                                                               
PEEKX    B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* FULL VERSION. TAKE OVER COMPLETELY THE TRANSACTION SO THAT THIS     *         
* TERMINAL CAN INPUT TO THE REQUESTED SCREEN AND CONTINUE.            *         
* THIS MEANS THAT TEMPSTR/TEMPEST MUST BE RECREATED.                  *         
***********************************************************************         
         SPACE 1                                                                
IM1      NTR1                                                                   
         L     R4,SVUTL                                                         
SAVE     USING UTLD,R4                                                          
         TM    SAVE.TSTAT2,TSTATTIP  TERMINAL IN PROCESS?                       
         BZ    IIS02                 NO                                         
         CLC   =C'OVERRIDE',SRVP2    TEST TO IGNORE TRM IN PROCESS              
         BNE   ERROR4                                                           
*                                                                               
* RELEASE ANY TEMPEST CONTROL INTERVALS OWNED BY ME                             
*                                                                               
IIS02    GOTO1 VTICTOC,DUB,C'1SET',0 SUSPEND SYSTEM TIMER                       
*                                                                               
         TM    TFLAG,TFLAGRTS      HAVE I RESERVED TEMPEST CIS                  
         BZ    IIS04               NO                                           
*                                                                               
         L     R7,SRPAR2           YES READ MY TWA#0 INTO TIA                   
         XC    DMCB+08(2),DMCB+08                                               
         MVC   DMCB+10(2),TNUM                                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,(R7),TO24=Y                        
         BRAS  RE,ON31                                                          
*                                                                               
         CLI   8(R1),0                                                          
         BNE   IIS04                                                            
         AH    R7,CHKDSP                                                        
         USING CHKPTD,R7                                                        
         OC    CHXTNDX,CHXTNDX     ANY RESERVED TEMPEST                         
         BZ    IIS04               NO                                           
         TM    CHXTFLG,X'80'                                                    
         BZ    IIS03                                                            
*                                                                               
IIS03    L     R8,VSSB             YES SET TEMPEST DATA IN TCB                  
         L     R8,SSBTKADR-SSBD(R8)                                             
         USING TCBD,R8                                                          
         MVC   TCBXTINF,CHXTINF                                                 
         GOTO1 VDATAMGR,DMCB,DMRLSE,TEMPEST,(255,0),0,TO24=Y                    
         BRAS  RE,ON31                                                          
*                                                                               
* MOVE UTL DATA AND TWA'S AND RESERVE/COPY TEMPEST PAGES                        
*                                                                               
IIS04    XC    DMCB(24),DMCB                                                    
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
*                                                                               
         MVI   SSBMTIND,0          DISABLE MULTI-TASKING WAITS <=======         
*                                                                               
         LHI   R0,4                SET TO COPY PAGES 1-4                        
IM4      STC   R0,DMCB+8                                                        
         BRAS  RE,RDTWA                                                         
         STC   R0,DMCB+8           RESTORE PAGE NUMBER                          
         BRAS  RE,WRTWA                                                         
         BCT   R0,IM4                                                           
         STC   R0,DMCB+8                                                        
*                                                                               
         MVI   DMCB+8,0                                                         
         BRAS  RE,RDTWA            NOW READ PAGE ZERO                           
*                                                                               
         MVC   TWATRM,TNUM                                                      
*                                                                               
* MOVE CONNECT DATA TO UTL IF DDS TERMINAL                                      
* USER TERMINALS KEEP OLD CONNECT AND AUTH STATUS                               
*                                                                               
IM6      TM    TSTAT1,TSTATDDS     TEST DDS TERMINAL                            
         BZ    IM7                 IF NOT, DON'T COPY CONNECT DATA              
*                                                                               
         MVC   TCTDATA,SAVE.TCTDATA MOVE TO CALLING TRM                         
         MVC   TPERSON,SAVE.TPERSON                                             
         MVC   TAGCTRY,SAVE.TAGCTRY                                             
         MVC   HALF(1),TSTAT6                                                   
         MVC   TSVDATA2,SAVE.TSVDATA2                                           
         MVC   TSTAT6,HALF                                                      
*                                                                               
IM7      MVC   TSVCREQ,=X'0126'    DO NOT CLEAR                                 
*                                                                               
IM8      MVC   DUB(L'TTRCNT),TTRCNT                                             
         MVC   HALF+0(1),TFLAG     KEEP MY TFLAG                                
         MVC   HALF+1(1),TSESSION  KEEP MY SESSION                              
         MVC   TSVDATA1,SAVE.TSVDATA1                                           
         MVC   TARSYS,SAVE.TARSYS                                               
*                                                                               
         NI    HALF,TFLAGSVS                                                    
         NI    TFLAG,255-TFLAGSVS                                               
         OC    TFLAG,HALF                                                       
         MVC   TSESSION,HALF+1     RESTORE MY SESSION                           
         MVC   TTRCNT,DUB                                                       
*                                                                               
IM10     L     R7,SRPAR6           RESTORE TCB SYSTEM SWITCH DATA               
         AH    R7,CHKDSP                                                        
         USING CHKPTD,R7           R7=A(TWA0 CHECKPOINT AREA)                   
         MVC   TMSAHIS,CHXTINF                                                  
         XC    CHXTINF,CHXTINF                                                  
*                                                                               
IM10B    L     R8,VSSB                                                          
         L     R8,SSBTKADR-SSBD(R8)                                             
         USING TCBD,R8             R8=A(TCB)                                    
         MVC   TCBSRMSG,CHSRMSG    COPY SRV MESSAGE                             
         MVC   TCBLNSYS,CHLNSYS                                                 
         MVC   TCBLNPRG,CHLNPRG                                                 
*                                  RESTORE UTL DATA                             
IM11     SR    R0,R0                                                            
         ICM   R0,1,CHTCBNUM       R0=N'SYSTEM SWITCH ENTRIES                   
         BZ    IM11X                                                            
         STC   R0,TCBSWNUM         RESTORE TCB DATA FOR CHECKPOINT              
         LA    RE,TCBSWTAB                                                      
         USING TCBSWTAB,RE         RE=A(TCB SYSTEM TABLE)                       
         LA    RF,CHTCBTAB                                                      
         USING CHTCBTAB,RF         RF=A(TWA CHKPT TABLE)                        
*                                                                               
IM11A    MVC   TCBSWSYS,CHTCBSYS                                                
         MVC   TCBSWSOV,CHTCBSOV                                                
         MVC   TCBSWAGB,CHTCBAGB                                                
         MVC   TCBSWACS,CHTCBACS                                                
         LA    RE,TCBSWLEN(RE)                                                  
         LA    RF,CHTCBLEN(RF)                                                  
         BCT   R0,IM11A                                                         
*                                                                               
IM11X    EQU   *                                                                
         DROP  RE,RF                                                            
*                                                                               
IM14     OC    TMSAHIS+1(2),TMSAHIS+1  TEST IF HE HAD TEMPEST CIS               
         BZ    IM14X                   NO                                       
         SR    RF,RF                   YES I WILL GET THE SAME                  
         ICM   RF,3,TMSAHIS+1                                                   
         GOTO1 VDATAMGR,DMCB,DMRSRV,TEMPEST,((RF),0),0,TO24=Y                   
         BRAS  RE,ON31                                                          
         CLI   DMCB+8,0                                                         
         BNE   IM14X                                                            
         LH    R0,DMCB+10          R0=RECORDS PER TRACK                         
         L     R8,VSSB                                                          
         L     R8,SSBTKADR-SSBD(R8)                                             
         USING TCBD,R8             R8=A(TCB)                                    
         MVC   TMSAMINE,TCBXTINF                                                
         MH    R0,TCBXTNUM                                                      
         STH   R0,HALF             HALF=NUMBER OF PAGES TO COPY                 
*                                                                               
IM14A    LA    RE,TEMPEST          SET DMCB TO COPY TEMPEST PAGES               
         ST    RE,DMCB+4                                                        
         XC    DMCB+8(4),DMCB+8                                                 
         MVC   DMCB+12(4),SRPAR2   USE TIA AS I/O AREA                          
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),TMSLEN                                                
         L     RF,VDATAMGR                                                      
         LA    R1,DMCB                                                          
         LA    R7,1                R7=TEMPEST PAGE NUMBER                       
*                                                                               
IM14B    STC   R7,8(R1)            SET PAGE NUMBER                              
         LA    RE,DMREAD                                                        
         ST    RE,0(R1)                                                         
         MVC   TCBXTINF,TMSAHIS                                                 
         BASSM RE,RF               READ HIS PAGE                                
         CLI   8(R1),0                                                          
         BNE   IM14X                                                            
         STC   R7,8(R1)            RESET PAGE NUMBER                            
         LA    RE,=C'DMWRT'                                                     
         ST    RE,0(R1)                                                         
         MVC   TCBXTINF,TMSAMINE                                                
         BASSM RE,RF               WRITE MY PAGE                                
         CLI   8(R1),0                                                          
         BNE   IM14X                                                            
         LA    R7,1(R7)            BUMP TO NEXT PAGE                            
         CH    R7,HALF                                                          
         BNH   IM14B                                                            
*                                                                               
IM14X    EQU   *                                                                
*                                                                               
IM15     OC    GLODSP,GLODSP       TEST IF GLOBALS IN TWA#0                     
         BZ    IM15X                                                            
         L     R0,SRPAR6           POINT TO GLOBALS IN TWA#0                    
         AH    R0,GLODSP                                                        
         LA    R1,CHKPTGLL                                                      
         L     RE,VSSB             POINT TO GLOBALS IN TASK                     
         L     RE,SSBTKADR-SSBD(RE)                                             
         L     RE,TCBWRKA-TCBD(RE)                                              
         L     RE,104(RE)                                                       
         LR    RF,R1                                                            
         LR    R7,RE                                                            
         MVCL  RE,R0                                                            
         MVI   11(R7),X'C0'        SET GLOBALS READ AND UPDATED                 
IM15X    EQU   *                                                                
*                                                                               
IMX      L     R1,APARM                                                         
         MVI   0(R1),X'FF'         TELL MONITOR TO WRITE-BACK TWA 0             
*                                                                               
         L     RE,VSSB                                                          
         USING SSBD,RE                                                          
         MVI   SSBMTIND,C'M'       ENABLE MULTI-TASKING WAITS<=========         
*                                                                               
EXIT     B     XIT1                                                             
         DROP  SAVE                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP IAM LINK                                                     *         
***********************************************************************         
         SPACE 1                                                                
LINK     NTR1                                                                   
         GOTO1 VGETFACT,DMCB,(X'80',0),F#VIAMT                                  
         L     R1,0(R1)                                                         
         ICM   R1,15,0(R1)                                                      
         BZ    LINKX                                                            
         ST    R1,FULL             SAVE THIS ADDR                               
*                                                                               
         L     RF,SVUTL                                                         
         XR    R0,R0                                                            
         ICM   R0,3,TNUM-UTLD(RF)                                               
*                                                                               
         LH    RE,0(R1)            SET BXLE                                     
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
LINK010  CLM   R0,15,0(R1)         CHECK TABLE FOR CONTENTION                   
         BE    ERROR8                                                           
         CLM   R0,15,4(R1)                                                      
         BE    ERROR8                                                           
         BXLE  R1,RE,LINK010                                                    
*                                                                               
LINK020  L     RF,SRPAR3                                                        
         XR    R0,R0                                                            
         ICM   R0,3,TNUM-UTLD(RF)                                               
*                                                                               
         L     R1,FULL                                                          
         LH    RE,0(R1)            SET BXLE                                     
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
LINK021  CLM   R0,15,4(R1)         CHECK TABLE FOR MYSELF                       
         BE    LINK022                                                          
         CLM   R0,15,0(R1)         CHECK MYSELF FOR SOURCE                      
         BE    ERROR8                                                           
         BXLE  R1,RE,LINK021                                                    
         B     LINK030                                                          
*                                                                               
LINK022  L     RF,0(R1)            CLEAR UTL VALUES                             
         BCTR  RF,0                                                             
         L     RE,VUTL                                                          
         MH    RF,0(RE)                                                         
         LA    RF,6(RE,RF)                                                      
         NI    TSTAT6-UTLD(RF),255-(TST6IAMS+TST6IAMD)                          
         L     RF,SRPAR3                                                        
         NI    TSTAT6-UTLD(RF),255-(TST6IAMS+TST6IAMD)                          
*                                                                               
         XC    0(8,R1),0(R1)       CLEAR ENTRY                                  
         B     LINK020             AND RETRY                                    
*                                                                               
LINK030  L     R1,FULL                                                          
         LH    RE,0(R1)            SET BXLE                                     
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
LINK031  OC    0(8,R1),0(R1)       FIND CLEAN ENTRY                             
         BE    LINK040                                                          
         BXLE  R1,RE,LINK031                                                    
         B     ERROR9                                                           
*                                                                               
LINK040  L     R5,SRPAR3           AT LAST.. SET LINK                           
         XR    R0,R0                                                            
         ICM   R0,3,TNUM-UTLD(R5)                                               
         STCM  R0,15,4(R1)                                                      
         OI    TSTAT6-UTLD(R5),TST6IAMD                                         
         L     R5,SVUTL                                                         
         ICM   R0,3,TNUM-UTLD(R5)                                               
         STCM  R0,15,0(R1)                                                      
         OI    TSTAT6-UTLD(R5),TST6IAMS                                         
*                                                                               
LINKX    B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* GET USERID FROM NUMBER IN GIUSER                                    *         
***********************************************************************         
         SPACE 1                                                                
GETUSER  NTR1                                                                   
         CLC   GIUSER,GIPREV                                                    
         BE    GETUSRX                                                          
         MVC   GIPREV,GIUSER                                                    
         LA    R7,IOAREA           BUILD ID RECORD KEY                          
         USING CTIREC,R7                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),GIUSER                                               
         NI    CTIKID+8,X'7F'      UNSET GENERIC FLAG                           
         GOTO1 VDATAMGR,DMCB,DMREAD,CTFILE,(R7),(R7),TO24=Y                     
         BRAS  RE,ON31                                                          
         CLI   8(R1),0                                                          
         BNE   GETUSR12            NNNNNN IF REC NOT FOUND                      
         LA    R7,CTIDATA                                                       
         SR    RE,RE                                                            
GETUSR10 AR    R7,RE                                                            
         CLI   0(R7),X'02'         TEST ID ELEMENT                              
         BNE   *+14                                                             
         MVC   GIUSERID,2(R7)      GET ID NAME                                  
         B     GETUSR20                                                         
*                                                                               
         IC    RE,1(R7)            BUMP TO NEXT ELEMENT                         
         CLI   0(R7),0                                                          
         BNE   GETUSR10            BUT DROP THROUGH IF LAST                     
*                                                                               
GETUSR12 EDIT  (B2,GIUSER),(6,GIUSERID),FILL=0                                  
*                                                                               
GETUSR20 LA    RF,0                                                             
         LA    RE,GIUSERID                                                      
GETUSR21 CLI   0(RE),X'40'                                                      
         BE    GETUSR30                                                         
         CLI   0(RE),0                                                          
         BE    GETUSR30                                                         
         LA    RF,1(RF)                                                         
         LA    RE,1(RE)                                                         
         CH    RF,=H'8'                                                         
         BL    GETUSR21                                                         
GETUSR30 STC   RF,GIULEN                                                        
GETUSRX  B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* GET GTSYS & GTSYSN FROM GTSYSSE                                     *         
***********************************************************************         
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
***********************************************************************         
* GET GTSYSSE FROM GTSYSN                                             *         
***********************************************************************         
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
***********************************************************************         
* GOBACKS - GENERATE SERVICE REQUESTS TO SWAP ADV SYS                 *         
***********************************************************************         
         SPACE 1                                                                
SWAPADV  NTR1                                                                   
         ICM   R1,15,TBUFF                                                      
         MVI   0(R1),10            SET LEN                                      
         MVC   1(2,R1),SRVIDH+2    SET ADDR                                     
         MVC   3(7,R1),=C'=SWAP,N' SET =SWAP                                    
         MVI   10(R1),7            SET LEN                                      
         MVC   11(2,R1),SRVIDH+2   SET ADDR                                     
*NOP*    MVC   13(4,R1),=C'=IAM'   SET =IAM                                     
*NOP*    MVI   17(R1),0            SET END                                      
         MVC   13(5,R1),=C'=PEEK'  SET =PEEK                                    
         MVI   18(R1),0            SET END                                      
         MVC   SRVID(8),=C'=GOBACK '                                            
         SR    RF,RF                                                            
         IC    RF,SYSNA+3                                                       
         LA    RF,1(RF)                                                         
*&&UK*&& CLM   RF,1,=C'5'                                                       
*&&US*&& CLM   RF,1,=C'9'                                                       
         BNE   *+8                                                              
         LA    RF,C'1'                                                          
         STC   RF,9(R1)                                                         
         B     GOODXIT                                                          
         EJECT                                                                  
***********************************************************************         
* GET GTPROGN FROM GTPROG                                             *         
***********************************************************************         
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
***********************************************************************         
* GET GTPROG FROM GTPROGN                                             *         
***********************************************************************         
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
***********************************************************************         
* GET GTSYS FROM SYSTEM NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
VALOVS   NTR1                                                                   
         LA    RE,SYSLST           RE=SYSLST                                    
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE                                                       
         SR    R1,R1               R1=LEN-1 FOR COMPARE                         
         IC    R1,GTSYSL                                                        
         BCTR  R1,0                                                             
         CLI   GTSYSL,3            DO WE HAVE >3 CHRS                           
         BH    VALO050                                                          
*                                                                               
VALO010  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    VALO050                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLSHRT(0),GTSYSN  TEST SHORT NAME                              
         BE    VALO990                                                          
VALO012  LA    RE,SYSLLEN(RE)      NEXT                                         
         B     VALO010                                                          
*                                                                               
VALO050  LA    RE,SYSLST           RE=SYSLST                                    
         LA    RE,6(RE)                                                         
VALO051  CLI   SYSLNUM,0           TEST FOR EOT                                 
         BE    VALO090                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SYSLNAME(0),GTSYSN  TEST LONG NAME                               
         BE    VALO990                                                          
         LA    RE,SYSLLEN(RE)      NEXT                                         
         B     VALO051                                                          
*                                                                               
VALO090  B     BADXIT              SET CC NEQ NOT FOUND                         
VALO990  MVC   GTSYS,SYSLNUM                                                    
         LR    R1,RE               R1=A(SYSLST ENTRY)                           
         B     GOODXIT             SET CC EQU                                   
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET SYSTEM NAME FROM GTSYS                                          *         
***********************************************************************         
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
***********************************************************************         
* SUBROUTINES FOR READING/WRITING TEMPSTR                             *         
***********************************************************************         
         SPACE 1                                                                
RDTWA    NTR1  ,                                                                
         L     RE,SVUTL                                                         
         MVC   DMCB+10(2),0(RE)    SET TRM NUM                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,(R3),TO24=Y                        
         BRAS  RE,ON31                                                          
         TM    8(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
WRTWA    NTR1  ,                                                                
         MVC   DMCB+10(2),TNUM     SET CALLING TRM NUM                          
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,,(R3),TO24=Y                         
         BRAS  RE,ON31                                                          
         TM    8(R1),X'FF'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
*                                                                               
ON31     O     RE,=X'80000000'                                                  
         BSM   0,RE                                                             
*                                                                               
OFF31    N     RE,=X'7FFFFFFF'                                                  
         BSM   0,RE                                                             
*                                                                               
GOODXIT  CR    RB,RB                                                            
         B     XIT1                                                             
BADXIT   LTR   RB,RB                                                            
         B     XIT1                                                             
SETBXLE  LH    R0,0(R4)            SET BXLE                                     
         L     R1,2(R4)                                                         
         LA    R4,6(R4)                                                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
         SPACE 1                                                                
INF01    MVC   SRVMSG(L'INFO1),INFO1                                            
         MVC   SRVMSG+1(4),SYSNA                                                
         LA    R2,SRVSL1H                                                       
         B     ERRXT                                                            
*                                                                               
ERROR1   MVC   SRVMSG(L'ERR1),ERR1                                              
         B     ERRXT                                                            
*                                                                               
ERROR2   MVC   SRVMSG(L'ERR2),ERR2                                              
         B     ERRXT                                                            
*                                                                               
ERROR3   MVC   SRVMSG(L'ERR3),ERR3                                              
         B     ERRXT                                                            
*                                                                               
ERROR4   MVC   SRVMSG(L'ERR4),ERR4                                              
         B     ERRXT                                                            
*                                                                               
ERROR5   MVC   64(BADTWAL,R3),BADTWA                                            
         MVC   SRVMSG(L'ERR5),ERR5                                              
         B     ERRXT                                                            
*                                                                               
ERROR6   MVC   64(BADTWAL,R3),BADTWA                                            
         MVC   SRVMSG(L'ERR6),ERR6                                              
         B     ERRXT                                                            
*                                                                               
ERROR7   MVC   SRVMSG(L'ERR7),ERR7                                              
         B     ERRXT                                                            
*                                                                               
ERROR8   MVC   SRVMSG(L'ERR8),ERR8                                              
         B     ERRXT                                                            
*                                                                               
ERROR9   MVC   SRVMSG(L'ERR9),ERR9                                              
         B     ERRXT                                                            
*                                                                               
ERRXT    NI    SRVIDH+6,X'BF'      UNSET CURSOR                                 
         OI    6(R2),X'40'                                                      
*                                                                               
* CLEAR SVC REQ FIELD IN UTL SO THIS SCREEN WILL BE SENT                        
*                                                                               
         L     R5,SRPAR3                                                        
         USING UTLD,R5                                                          
*        MVC   TSVCREQ,=X'01EE'    $BYE                                         
*                                                                               
         B     XMOD1                                                            
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         SPACE 1                                                                
ERR1     DC    C'ED/0000 Terminal id not valid'                                 
ERR2     DC    C'ED/0000 Terminal is not connected'                             
ERR3     DC    C'ED/0000 Terminal is not the same ID/SYS/PRG'                   
ERR4     DC    C'ED/0000 Terminal is in process - REQUEST AGAIN'                
ERR5     DC    C'ED/0000 DISK ERROR READING TERMINAL TWA'                       
ERR6     DC    C'ED/0000 Terminal has invalid screen'                           
ERR7     DC    C'ED/0000 Invalid select code. Not S or C'                       
ERR8     DC    C'ED/0000 Link already active '                                  
ERR9     DC    C'ED/0000 Too many active links'                                 
*                                                                               
INFO1    DC    C'(SYSN) =TERM List displayed. Select or Copy'                   
*                                                                               
BADTWA   DC    XL08'4420000100008000'                                           
         DC    XL60'00'                                                         
         DC    XL08'1900003E0000C000'                                           
         DC    CL17'=PEEK'                                                      
         DC    XL03'000100'                                                     
BADTWAL  EQU   *-BADTWA                                                         
*                                                                               
DMREAD   DC    CL8'DMREAD  '                                                    
DMRLSE   DC    CL8'DMRLSE  '                                                    
DMRSRV   DC    CL8'DMRSRV  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
TEMPEST  DC    CL8'TEMPEST '                                                    
*                                                                               
*               '....+....1....+....2....+....3....+....4'                      
*               '....+....5....+....6....+....7....+...'                        
HEADER1  DC    C'Sel  Number   Luid   Userid   Sys Prg   '                      
         DC    C'                                      '                        
HEADER2  DC    C'---- ------ -------- -------- --- ---   '                      
         DC    C'                                      '                        
PFKEYS   DC    C'PF4=Swap ADVs                           '                      
         DC    C'                                      '                        
*                                                                               
       ++INCLUDE FASYSLST                                                       
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
SAVERD   DS    A                   SAVE BASE RD                                 
SRPARS   DS    0CL32                                                            
SRPAR1   DS    A                   SYSFAC                                       
SRPAR2   DS    A                   TIA                                          
SRPAR3   DS    A                   UTL                                          
SRPAR4   DS    A                   COMFACS                                      
SRPAR5   DS    A                   SELIST                                       
SRPAR6   DS    A                   TWA                                          
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
VTERMVAL DS    A                                                                
VHEXIN   DS    A                                                                
VHEXOUT  DS    A                                                                
VGETFACT DS    A                                                                
         DS    A                                                                
*                                                                               
DUB      DS    D                                                                
DMCB     DS    6F                                                               
FULL     DS    F                                                                
HALF     DS    H                                                                
WORK     DS    CL10                                                             
LUID     DS    CL8                                                              
FLAG     DS    X                                                                
ACTN     DS    X                                                                
PFKEY    DS    X                                                                
DDS      DS    X                                                                
         DS    XL2                                                              
SVSRDATA DS    XL17                ACTUAL DATA IN S/R FIELD                     
SVCTDATA DS    XL17                CONNECT DATA IN S/R FIELD                    
*                                                                               
AMFDATA  DS    A                   A(MESSAGE HEADER FIELD DATA)                 
ASRDATA  DS    A                   A(SERVICE REQUEST FIELD DATA)                
SVUTL    DS    A                                                                
APARM    DS    A                                                                
TMSLEN   DS    H                   TEMPEST RECORD LENGTH                        
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
CHKDSP   DS    H                   TEMPSTR DISPLACEMENT TO CHKPNT               
GLODSP   DS    H                   TEMPSTR DISPLACEMENT TO GLOBALS              
SYSNA    DS    CL4                 ADV APPLICATION NAME                         
*                                                                               
GIUSER   DS    CL2                 WORK AREA FOR GETUSER                        
GIPREV   DS    CL2                                                              
GIULEN   DS    CL1                                                              
GIUSERID DS    CL8                                                              
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
*                                                                               
TMSAHIS  DS    XL8                 HIS TEMPEST ALLOC FLG/NUM/NDX                
TMSAMINE DS    XL8                 MY  TEMPEST ALLOC FLG/NUM/NDX                
*                                                                               
IOAREA   DS    1024C                                                            
*                                                                               
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
LINED    DSECT                                                                  
SELHDR   DS    XL8                                                              
SELDATA  DS    CL4                                                              
NUMHDR   DS    XL8                                                              
LINNUM   DS    CL6                                                              
LINHDR   DS    XL8                                                              
LINDATA  DS    CL66                                                             
         ORG   LINDATA                                                          
LINLUID  DS    CL8                                                              
         DS    CL1                                                              
LINUSER  DS    CL8                                                              
         DS    CL1                                                              
LINSYS   DS    CL3                                                              
         DS    CL1                                                              
LINPRG   DS    CL3                                                              
         DS    CL1                                                              
         ORG                                                                    
LINELEN  EQU   *-LINED                                                          
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FAPGMLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
* FASRS                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASRS                                                          
         PRINT ON                                                               
* FASRPARM                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASRPARM                                                       
         PRINT ON                                                               
* FAUTL                                                                         
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* FATCB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATCB                                                          
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FASYSLST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
* DDFH                                                                          
       ++INCLUDE DDFH                                                           
         EJECT                                                                  
* FACHKPT                                                                       
       ++INCLUDE FACHKPT                                                        
         EJECT                                                                  
FATWA    DSECT                                                                  
       ++INCLUDE FATWA                                                          
* SRIAMFFD                                                                      
       ++INCLUDE SRIAMFFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SRIAM00   03/06/08'                                      
         END                                                                    
