*          DATA SET SRDQU00S   AT LEVEL 006 AS OF 05/01/02                      
*PHASE T15900A,*                                                                
*INCLUDE GETIDS                                                                 
*INCLUDE SIXPACK                                                                
         TITLE 'SRDQU00 - DISPLAY PRTQUE REPORTS'                               
DQU      CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DQUWRKX-DQUWRKD,**$DQU**,R6,CLEAR=YES,RR=R3                      
*                                                                               
         USING DQUWRKD,RC          RC=A(W/S)                                    
         ST    RD,SAVEDRD          SAVE RD IN WORK AREA                         
         ST    R3,RELO                                                          
*                                                                               
         MVC   SRPARAS(SRPARAL),0(R1)                                           
         L     RA,SRPASYS                                                       
         L     RA,VSSB-SYSFACD(RA)                                              
         MVC   RECLEN,SSBTWAL-SSBD(RA)                                          
         L     RA,SRPATWA                                                       
         USING T159FFD,RA          RA=A(TWA)                                    
         L     R9,SRPACOM                                                       
         USING COMFACSD,R9         R9=A(COMFACS)                                
         L     R8,SRPAUTL                                                       
         USING UTLD,R8             R8=A(UTL)                                    
         L     R7,SRPATIA                                                       
         USING SRSD,R7             R7=A(SPECIAL S/R SAVE PAGE)                  
         MVC   DATADISP,=H'28'                                                  
*                                                                               
         L     RF,=A(VALOCHRS)                                                  
         AR    RF,R3                                                            
         ST    RF,AVALCHRS                                                      
         L     RF,=A(HELPSCR)                                                   
         AR    RF,R3                                                            
         ST    RF,AHELPSCR                                                      
         L     RF,=A(DQUPROF)                                                   
         AR    RF,R3                                                            
         ST    RF,ADQUPROF                                                      
         L     RF,=A(WRITBLE)                                                   
         AR    RF,R3                                                            
         ST    RF,AWRITBLE                                                      
*                                                                               
         GOTO1 CDICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
         BAS   RE,MAIN                                                          
         CLI   HELPNUM,0           WAS HELP REQUESTED                           
         BNE   HELPOUT                                                          
*                                                                               
DQXMOD   XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
*                    MAIN PROGRAM                           *                   
*************************************************************                   
MAIN     NTR1                                                                   
*                                                                               
         MVC   HELPKEY,HELPID      INITIALISE HELP KEY                          
*                                                                               
         L     R5,SRPATIOB         TRANSLATOR I/O BLOCK                         
         USING TIOBD,R5                                                         
         MVC   PFKEY,TIOBAID       PFKEY NUMBER OR ZERO (ENTER)                 
         DROP  R5                                                               
*                                                                               
         CLI   PFKEY,12            HIGHEST PF KEY FOR NOW                       
         BNH   M10                                                              
         ZIC   R1,PFKEY                                                         
         SH    R1,=H'12'           EQUATE HIGH PF KEYS TO LOWER VALUES          
         STC   R1,PFKEY                                                         
*                                                                               
M10      CLI   PFKEY,PFHELP        CHECK HELP PFKEY PRESSED                     
         BNE   *+12                                                             
         BAS   RE,DISPLAY          SKIP TO DISPLAY ROUTINE                      
         B     EXIT                LEAVE PROGRAM                                
         CLI   PFKEY,PFPQR                                                      
         BE    PQRET                                                            
*                                                                               
         L     RF,=A(READSTR)      READ S/R SAVE STORAGE PAGE INTO TWA          
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         CLC   OIDENT,=C'$DQU'     TEST IF ITS STILL MINE                       
         BNE   M11                                                              
         MVC   FSTRING,OFSTRING    PASS ALONG OLD SEARCH INFO                   
         MVC   FLEN,OFLEN                                                       
         MVC   FSCOL,OFSCOL                                                     
         MVC   VARSCRL,OVARSCRL                                                 
*                                                                               
M11      BAS   RE,VALIDATE                                                      
         BAS   RE,PROCESS                                                       
         BAS   RE,DISPLAY                                                       
         MVC   NIDENT,=C'$DQU'                                                  
         L     RF,=A(WRITESTR)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         TM    ATTRIBS,QLATERR     EXIT WITH ERROR STATUS MESSAGE               
         BNO   DQXIT                                                            
         BAS   RE,INFO4                                                         
*                                                                               
DQXIT    XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*                VALIDATE SCREEN AND TERMINAL               *                   
*************************************************************                   
VALIDATE NTR1                                                                   
         MVI   DDS,0               INITIALIZE TERMINAL FLAG                     
         TM    TSTAT,X'60'                                                      
         BZ    *+8                                                              
         MVI   DDS,X'01'           SET DDS TERMINAL                             
         TM    TSTAT7,TST7PQPU                                                  
         BZ    *+8                                                              
         OI    DDS,X'02'           SET PRIVILEGED USER                          
*                                                                               
         MVC   PUBLICID,PUBENG     DEFAULT ENGLISH                              
         CLI   TAGCTRY,3                                                        
         BNE   *+10                                                             
         MVC   PUBLICID,PUBGER     GERMAN PUBLIC                                
*                                                                               
V2       MVC   REPUSER,TUSER       SET USER ID FROM $CT VALUE                   
         LA    R2,DQUSRVH                                                       
         CLC   DQUSRV+1(4),=C'DQUP' TEST PUBLIC DQU                             
         BNE   V3                                                               
         MVC   REPUSER,PUBLICID    SET USER ID TO PUBLIC VALUE                  
         B     V4                                                               
V3       OC    TUSER,TUSER         TEST IF LOGGED ON                            
         BNZ   *+12                YES                                          
         TM    DDS,X'01'           NO ONLY FOR DDS TERMINALS                    
         BZ    ERR0                                                             
         CLC   DQUSRV+1(4),=C'DQUG' TEST ID GROUP DQU                           
         BNE   V4                                                               
         OC    TPQGRPID,TPQGRPID   IGNORE IF NO GROUP USER ID                   
         BZ    V4                                                               
         MVC   REPUSER,TPQGRPID    SET USER ID TO GROUP VALUE                   
V4       OC    REPUSER,REPUSER                                                  
         BNZ   *+12                                                             
         TM    DDS,X'01'                                                        
         BZ    ERR0                ERROR IF NO USER ID AVAILABLE                
*                                                                               
         XC    KEY,KEY             GET PRTQ FILE FOR THIS USER ID               
         MVC   KEY+UKSRCID-UKINDEX(2),REPUSER                                   
         GOTO1 CDATAMGR,DMCB,(X'00',GFILE),PRTQUE,KEY,PLINE,CIREC               
         MVC   PRTQID,KEY+UKUSRINF-UKINDEX                                      
*                                                                               
         BAS   RE,VALSPID          VALIDATE SCREEN FIELDS                       
         BAS   RE,VALPAGE                                                       
         BAS   RE,VALLINE                                                       
         BAS   RE,VALCOL                                                        
         L     RF,=A(VALOPT)                                                    
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LTR   RE,R0                                                            
         BNZ   ERRS                                                             
         BAS   RE,VALSCR                                                        
*                                                                               
         CLI   NEWREP,C'Y'         CHANGED IF NEW REPORT                        
         BE    V20                                                              
         CLC   SPAGE,OPAGE         CHECK FOR CHANGE OF FIELDS                   
         BNE   V20                                                              
         CLC   SLINE,OLINE                                                      
         BNE   V20                                                              
         CLC   SCOL,OCOL                                                        
         BNE   V20                                                              
         MVI   CHANGED,C'N'        NO FIELD HAS BEEN CHANGED                    
         B     VX                                                               
V20      MVI   CHANGED,C'Y'        A FIELD HAS BEEN CHANGED                     
VX       B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                   VALIDATE SPOOL ID                       *                   
*************************************************************                   
VALSPID  NTR1                                                                   
*                                                                               
         LA    R2,DQUSPIDH         FIELD SHOULD CONTAIN 'SUBID,SEQNO'           
         CLI   5(R2),0                                                          
         BNE   VSP2                                                             
*                                                                               
VSP1     GOTO1 CGLOBBER,DMCB,=C'GETF',(R2),,GLVSPID                             
         CLI   5(R2),0                                                          
         BE    INFO3                                                            
*                                                                               
         LA    RF,9                COMPUTE REAL INPUT LENGTH                    
         LA    RE,8+8(R2)                                                       
         CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,*-10                                                          
         STC   RF,5(R2)                                                         
*                                                                               
VSP2     CLI   8(R2),C'?'          ? FOR HELP                                   
         BNE   VSP3                                                             
         MVI   HELPNUM,1                                                        
         ST    R2,QHDR                                                          
         B     HELPOUT                                                          
VSP3     GOTO1 CSCANNER,DMCB,(R2),(4,SCANOUT)                                   
         LA    R3,SCANOUT                                                       
         CLI   4(R1),2             ONLY TWO ? FIRST MUST BE SUB ID              
         BE    VSP7                                                             
         CLC   12(1,R3),SR@PSWD    FIRST MAY BE PASSWORD                        
         BE    VSP7                                                             
*                                                                               
         TM    DDS,X'03'           DDS/PRIVILEGED CAN HAVE U=... FIRST          
         BZ    ERR2                                                             
         CLC   SCANOUT+12(2),=C'U '                                             
         BNE   ERR2                                                             
         MVC   REPCUSR,SCANOUT+22                                               
         CLC   REPCUSR,OREPCUSR                                                 
         BNE   VSP3A                                                            
         MVC   REPUSER,OREPUSER                                                 
         MVC   PRTQID,OPRTQID                                                   
         LA    R3,32(R3)                                                        
         B     VSP7                                                             
VSP3A    TM    DDS,X'01'           ALL USERIDS ARE VALID FOR DDS                
         BO    VSP4                                                             
         LA    RE,INVGRPID                                                      
         SR    R1,R1                                                            
VSP3C    ICM   R1,1,0(RE)          SEARCH TABLE OF INVALID USER IDS             
         BZ    VSP4                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SCANOUT+22(0),1(RE)                                              
         BE    ERR11               DDS IDS INVALID FOR PRIVILEGED               
         LA    RE,L'INVGRPID(RE)                                                
         B     VSP3C                                                            
*                                                                               
VSP4     LA    R4,CTIOAREA         VALIDATE INPUT U=USERID                      
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,REPCUSR                                                   
         OC    CTIKID,=CL10' '                                                  
         GOTO1 CDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',CTIREC,CTIREC                
         CLI   8(R1),X'00'                                                      
         BNE   ERR11                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   ERR11                                                            
         MVC   REPUSER,2(R4)                                                    
*                                                                               
VSP5     TM    DDS,X'01'           DDS USER CAN INPUT ANY VALID ID              
         BO    VSP6                                                             
         OC    TUSER,TUSER         PRIVILEGED USER MUST BE LOGGED ON            
         BZ    ERR0                                                             
         CLC   REPUSER,TUSER       TEST IF U=LOGONID                            
         BE    VSP6                                                             
         LA    R4,CTIOAREA                                                      
         XC    CTIKEY,CTIKEY       READ LOGON ID RECORD                         
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TUSER                                                
         GOTO1 CDATAMGR,DMCB,=C'DMREAD',=C'CTFILE',CTIREC,CTIREC                
         CLI   8(R1),X'00'                                                      
         BNE   ERR0                                                             
         MVI   DMCB,C'C'           SET COMPATIBLE ID MATCH                      
         STCM  R4,7,DMCB+1                                                      
         XC    DMCB+4(4),DMCB+4                                                 
         MVI   DMCB+8,C'A'         SET PASSING ID ALPHA IN P4                   
         MVC   DMCB+9(3),CDATAMGR+1                                             
         LA    RE,REPCUSR                                                       
         ST    RE,DMCB+12                                                       
         GOTO1 =V(GETIDS),DMCB,RR=RELO                                          
         TM    DMCB+12,X'01'                                                    
         BZ    ERR11               U=XXXXX MUST BE COMPATIBLE ID                
*                                                                               
VSP6     XC    KEY,KEY             GET PRTQ FILE FOR THIS USER ID               
         MVC   KEY+UKSRCID-UKINDEX(2),REPUSER                                   
         GOTO1 CDATAMGR,DMCB,(X'00',GFILE),PRTQUE,KEY,PLINE,CIREC               
         MVC   PRTQID,KEY+UKUSRINF-UKINDEX                                      
         LA    R3,32(R3)                                                        
*                                                                               
VSP7     CLI   1(R3),0             R3 POINTS TO SUB ID BLOCK                    
         BE    VSP7A                                                            
         CLC   12(1,R3),SR@PSWD                                                 
         BNE   ERR2                                                             
         CLI   1(R3),1             PASSWORD IS 1 THRU 6 CHRS                    
         BL    ERR2                                                             
         CLI   1(R3),6                                                          
         BH    ERR2                                                             
         MVC   REPPSW,22(R3)                                                    
         LA    R3,32(R3)                                                        
*                                                                               
         TM    DDS,X'01'           IF DDS TERMINAL                              
         BNO   VSP7A                                                            
         CLC   REPPSW,DDSPSWD      DDSPSWD MATCHES ALL                          
         BNE   VSP7A                                                            
         MVC   REPPSW,FFS                                                       
*                                                                               
VSP7A    CLI   0(R3),3                                                          
         BH    ERR2                                                             
         CLI   0(R3),2                                                          
         BL    ERR2                                                             
         MVC   REPSUBID,12(R3)                                                  
*                                                                               
         LA    RE,REPSUBID         MASSAGE SUBID                                
         LA    RF,3                                                             
         CLI   0(RE),C'A'                                                       
         BNL   *+8                                                              
         MVI   0(RE),C'.'                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
         LA    R3,32(R3)           SECOND HALF IS SEQUENCE NUMBER               
         CLI   1(R3),0                                                          
         BNE   ERR2                                                             
         TM    2(R3),X'80'                                                      
         BZ    ERR2                                                             
         OC    4(4,R3),4(R3)                                                    
         BZ    ERR2                                                             
         CLC   4(4,R3),=F'65000'                                                
         BH    ERR2                                                             
         MVC   REPSEQNO,6(R3)                                                   
*                                                                               
         CLC   OIDENT,=C'$DQU'     CHECK S/R SAVE FOR DQU                       
         BNE   VSP10                                                            
         CLC   REPUSER,OREPUSER    CHECK REPORT ID AGAINST PREVIOUS             
         BNE   VSP10                                                            
         CLC   REPSUBID,OSUBID                                                  
         BNE   VSP10                                                            
         CLC   REPSEQNO,OSEQNO                                                  
         BNE   VSP10                                                            
         MVC   SCRFLAG,OSCRFLAG    ASSUME SAME FLAGS & PROFILES                 
         MVC   PROFILE,OPROFILE                                                 
         MVI   NEWREP,C'N'         SAME AS PREVIOUS                             
         B     VSP20                                                            
*                                                                               
VSP10    MVI   NEWREP,C'Y'         NEW REPORT                                   
         BAS   RE,VALREP                                                        
         BAS   RE,RDPROF           GET NEW REPORT PROFILE                       
         B     DQXIT                                                            
*                                                                               
VSP20    BAS   RE,VALREP           SAME REPORT AS LAST TIME                     
         B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                     VALIDATE REPORT                       *                   
*************************************************************                   
VALREP   NTR1                      INITIALIZE BUFFER                            
         GOTO1 CDATAMGR,DMCB,(X'00',BUFFER),PRTQUE,KEY,PLINE,CIREC,0            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE ON INIT BUFFER DISK ERROR                
*                                                                               
         LA    R5,KEY              SEARCH PRTQUE INDEX FOR REPORT               
         USING UKRECD,R5                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKSRCID,REPUSER                                                  
         MVC   UKSUBID,REPSUBID                                                 
         MVC   UKREPNO,REPSEQNO                                                 
         GOTO1 CDATAMGR,DMCB,(X'08',INDEX),PRTQID,KEY,PLINE,CIREC,0             
         TM    8(R1),X'80'                                                      
         BO    ERR3                EOF - REPORT NOT FOUND                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE ON INDEX READ DISK ERROR                 
*                                                                               
         MVC   PLINE(4),=F'0'      READ REPORT HEADER RECORD                    
         MVC   PLINE+4(4),=C'PAGE'                                              
         GOTO1 CDATAMGR,DMCB,(X'00',RANDOM),PRTQID,KEY,PLINE,CIREC,0            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T READ REPORT HEADER              
*                                                                               
         LA    R5,PLINE            CHECK REPORT ATTRIBUTES                      
         USING PQPLD,R5                                                         
         MVC   SYSTEM,QLSYS                                                     
         MVC   PROGRAM,QLPRG                                                    
         MVC   ATTRIBS,QLATTB                                                   
*                                                                               
         TM    QLATTB,QLATPW       TEST PASSOWRD PROTECT                        
         BNO   VR05                                                             
         CLC   REPPSW,FFS          TEST DDS GLOBAL PASSWORD                     
         BE    VR05                                                             
         CLC   REPPSW,QLPSWD       TEST USER PASSWORD                           
         BNE   ERR3                                                             
VR05     TM    QLATTB,QLATNP       IGNORE NON PRINTABLE REPORTS                 
         BO    VR10                                                             
         TM    QLSTAT,QLSTIN       TEST INVISIBLE REPORT                        
         BO    ERR3                                                             
         TM    QLATTB,QLATJOBI     IGNORE REPORTS WITH JCL IN THEM              
         BO    VR10                                                             
         B     VR20                                                             
VR10     TM    DDS,X'01'           DDS TERMINALS ONLY ALLOWED                   
         BZ    ERR3                                                             
*                                                                               
VR20     EQU   *                                                                
VR30     ZICM  RF,QLPAGES,2        STORE NUMBER OF FINAL PAGE                   
         ST    RF,LASTPAGE                                                      
VRX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                  VALIDATE PAGE NUMBER                     *                   
*************************************************************                   
VALPAGE  NTR1                                                                   
VP1      LA    R2,DQUPAGEH                                                      
         CLI   5(R2),0             CHECK FOR BLANK FIELD                        
         BNE   VP10                                                             
         CLI   NEWREP,C'Y'         CHECK NEW REPORT                             
         BNE   VP5                                                              
         MVC   SPAGE,=F'1'         DEFAULT TO FIRST PAGE                        
         B     VPX                                                              
VP5      MVC   SPAGE,OPAGE         DEFAULT TO PREVIOUS                          
         B     VPX                                                              
VP10     CLI   8(R2),C'?'          ? FOR HELP                                   
         BNE   VP11                                                             
         MVI   HELPNUM,2                                                        
         ST    R2,QHDR                                                          
         MVI   5(R2),0                                                          
         B     VP1                                                              
VP11     TM    4(R2),X'08'         CHECK FOR VALID NUMERIC INPUT                
         BZ    ERR2                                                             
         ZIC   R1,5(R2)            EXTRACT NUMBER FROM FIELD                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         ST    R1,SPAGE                                                         
         LTR   R1,R1               PAGE NUMBER MUST BE GREATER THAN 0           
         BZ    ERR4                                                             
         C     R1,LASTPAGE         AND LESS THAN OR EQUAL TO LASTPAGE           
         BH    ERR4                                                             
VPX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                VALIDATE LINE NUMBER                       *                   
*************************************************************                   
VALLINE  NTR1                                                                   
VL1      LA    R2,DQULINEH                                                      
         CLI   5(R2),0                                                          
         BNE   VL10                                                             
         CLI   NEWREP,C'Y'         CHECK NEW REPORT                             
         BNE   VL5                                                              
         MVC   SLINE,=F'1'         DEFAULT TO FIRST LINE                        
         B     VLX                                                              
VL5      MVC   SLINE,OLINE         DEFAULT TO PREVIOUS                          
         B     VLX                                                              
VL10     CLI   8(R2),C'?'          ? FOR HELP                                   
         BNE   VL11                                                             
         MVI   HELPNUM,3                                                        
         ST    R2,QHDR                                                          
         MVI   5(R2),0                                                          
         B     VL1                                                              
VL11     TM    4(R2),X'08'         CHECK FOR VALID NUMERIC INPUT                
         BZ    ERR2                                                             
         ZIC   R1,5(R2)            EXTRACT NUMBER FROM FIELD                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         ST    R1,SLINE                                                         
         LTR   R1,R1               LINE NUMBER MUST BE GREATER THAN 0           
         BZ    ERR5                                                             
         C     R1,=F'99999'        AND LESS THAN OR EQUAL TO 99999              
         BH    ERR5                                                             
VLX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                VALIDATE COLUMN NUMBER                     *                   
*************************************************************                   
VALCOL   NTR1                                                                   
VC1      LA    R2,DQUCOLH                                                       
         CLI   5(R2),0                                                          
         BNE   VC10                                                             
         CLI   NEWREP,C'Y'         CHECK NEW REPORT                             
         BNE   VC5                                                              
         MVC   SCOL,=F'1'          DEFAULT TO FIRST COLUMN                      
         B     VCX                                                              
VC5      MVC   SCOL,OCOL           DEFAULT TO PREVIOUS                          
         B     VCX                                                              
VC10     CLI   8(R2),C'?'          ? FOR HELP                                   
         BNE   VC11                                                             
         MVI   HELPNUM,4                                                        
         ST    R2,QHDR                                                          
         MVI   5(R2),0                                                          
         B     VC1                                                              
VC11     TM    4(R2),X'08'         CHECK FOR VALID NUMERIC INPUT                
         BZ    ERR2                                                             
         ZIC   R1,5(R2)            EXTRACT NUMBER FROM FIELD                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         ST    R1,SCOL                                                          
         LTR   R1,R1               COL NUMBER MUST BE GREATER THAN ZERO         
         BZ    ERR6                                                             
         C     R1,=F'170'          AND LESS THAN OR EQUAL TO 170                
         BH    ERR6                                                             
VCX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                VALIDATE SCROLL FIELD                      *                   
*************************************************************                   
VALSCR   NTR1                                                                   
VS0      LA    R2,DQUSCRH                                                       
         CLI   5(R2),0                                                          
         BNE   VS10                                                             
         CLI   NEWREP,C'Y'         CHECK NEW REPORT                             
         BNE   VS5                                                              
         MVC   SCROLL,=F'19'       DEFAULT TO SCROLL OF 19                      
         B     VSX                                                              
VS5      MVC   SCROLL,OSCROLL      DEFAULT TO PREVIOUS                          
         B     VSX                                                              
VS10     CLI   8(R2),C'?'          ? FOR HELP                                   
         BNE   VS11                                                             
         MVI   HELPNUM,6                                                        
         ST    R2,QHDR                                                          
         MVI   5(R2),0                                                          
         B     VS0                                                              
VS11     TM    4(R2),X'08'         CHECK FOR VALID NUMERIC INPUT                
         BZ    ERR2                                                             
         ZIC   R1,5(R2)            EXTRACT NUMBER FROM FIELD                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         ST    R1,SCROLL                                                        
VSX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                  PROCESS USER REQUESTS                    *                   
*************************************************************                   
PROCESS  NTR1                                                                   
         CLI   CHANGED,C'Y'        OVERRIDE ALL OTHER REQUESTS IF PAGE,         
         BE    PX                  LINE, OR COLUMN HAS BEEN CHANGED             
         CLC   OPTYPE(1),SR@FIND   OVERRIDE PFKEY IF FIND REQUESTED             
         BNE   P10                                                              
         BAS   RE,DOFIND                                                        
         B     PX                                                               
P10      CLI   PFKEY,0             CHECK FOR PFKEY PRESSED                      
         BE    P20                                                              
         BAS   RE,DOPFKEY                                                       
         B     PX                                                               
P20      CLI   OPTYPE,C' '         PRESS OF ENTER MEANS DOWN 19 LINES           
         BNE   PX                                                               
         CLI   HELPNUM,0           UNLESS HELP THIS TIME                        
         BNE   PX                                                               
         TM    TFLAG,TFLAGHLP                                                   
         BO    PX                  OR HELP LAST TIME                            
*                                                                               
* WHEN USING PROFILES, NEXT SCREENFUL OF DATA IS (19-#HDLINES) UNLESS           
* ON LINE 1 AND SHOWINGF WHOLE PAGE AT TOP                                      
         TM    SCRFLAG,SCROVR      OVERRIDE PROFILE?                            
         BNZ   P30                                                              
         CLC   SPAGE+3(1),PROFILE+(PROFIGNP-PROFD) IGNORE PAGE?                 
         BNH   P30                  YES - TREAT SAME AS OVERRIDE                
         TM    SCRFLAG,KEEPHDS     ARE THERE ANY HDLINES TO KEEP?               
         BZ    P30                                                              
         TM    SCRFLAG,SHOWPAGE    IF SET AND 1ST LINE, SHOW PAGE               
         BZ    *+14                                                             
         CLC   =F'1',SLINE                                                      
         BE    P30                                                              
         LA    RF,19                                                            
         ZIC   RE,PROFILE+(PROFNHDS-PROFD)                                      
         SR    RF,RE                                                            
         ST    RF,OPVAL                                                         
         B     *+10                                                             
P30      MVC   OPVAL,=F'19'                                                     
         BAS   RE,DOWNL                                                         
PX       B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                  PROCESS PFKEY REQUESTS                   *                   
*************************************************************                   
DOPFKEY  NTR1                                                                   
         CLI   PFKEY,PFFIND        CHECK SPECIAL REPEAT FIND REQUEST            
         BNE   DOPF6                                                            
         CLC   FLEN,=F'0'          CHECK NO PREVIOUS FIND                       
         BNE   DOPF5                                                            
         BAS   RE,DISPLAY          DIPLAY ERROR MESSAGE                         
         B     INFO1               ENTER FIND COMND                             
*                                                                               
DOPF5    BAS   RE,DOFIND                                                        
         B     DOPFX                                                            
*                                                                               
DOPF6    CLI   OPTYPE,C' '         CHECK FOR BLANK OPTION FIELD                 
         BNE   DOPF20                                                           
         CLI   PFKEY,PFUPP         CHECK IF SCROLL PAGE                         
         BE    DOPF10                                                           
         CLI   PFKEY,PFDOWNP                                                    
         BE    DOPF10                                                           
         MVC   OPVAL,SCROLL        DEFAULT TO SCROLL FIELD AMOUNT               
         B     DOPF20                                                           
DOPF10   MVC   OPVAL,=F'1'         DEFAULT TO ONE PAGE                          
*                                                                               
DOPF20   CLI   PFKEY,PFUPP                                                      
         BNE   *+12                                                             
         BAS   RE,UPP                                                           
         B     DOPFX                                                            
         CLI   PFKEY,PFDOWNP                                                    
         BNE   *+12                                                             
         BAS   RE,DOWNP                                                         
         B     DOPFX                                                            
         CLI   PFKEY,PFUPL                                                      
         BNE   *+12                                                             
         BAS   RE,UPL                                                           
         B     DOPFX                                                            
         CLI   PFKEY,PFDOWNL                                                    
         BNE   *+12                                                             
         BAS   RE,DOWNL                                                         
         B     DOPFX                                                            
         CLI   PFKEY,PFLEFTC                                                    
         BNE   *+12                                                             
         BAS   RE,LEFTC                                                         
         B     DOPFX                                                            
         CLI   PFKEY,PFRIGHTC                                                   
         BNE   *+12                                                             
         BAS   RE,RIGHTC                                                        
         B     DOPFX                                                            
DOPFX    B     DQXIT               IF SILLY PFKEY PRESSED DO NOTHING            
         EJECT                                                                  
*************************************************************                   
*                  PROCESS FIND REQUEST                     *                   
*************************************************************                   
DOFIND   NTR1                                                                   
         CLC   OPTYPE(1),SR@FIND   IF FIND OPTION SELECTED                      
         BE    DOF10                                                            
         CLI   OWASFIND,C'Y'       OR IF LAST COMMAND WAS NOT A FIND            
         BNE   DOF10                                                            
         CLI   NEWREP,C'Y'         OR IF REPORT IS NEW                          
         BE    DOF10                                                            
         MVC   FPAGE,OFPAGE        START SEARCH WHERE LAST FOUND                
         MVC   FLINE,OFLINE                                                     
         MVC   FCOL,OFCOL                                                       
         B     DOF20                                                            
DOF10    MVC   FPAGE,SPAGE         START SEARCH WHERE SCREEN IS - 1             
         MVC   FLINE,SLINE                                                      
         L     R1,SCOL                                                          
         BCTR  R1,0                                                             
         ST    R1,FCOL                                                          
*                                                                               
DOF20    BAS   RE,FINDIO           SCAN LINES FOR MATCH                         
         BAS   RE,FIGSCR           FIGURE OUT SCREEN CORNER                     
         BAS   RE,FIGMESS          FIGURE OUT MESSAGE                           
*                                                                               
         MVI   WASFIND,C'Y'        SET LAST COMMAND FIND FLAG                   
         B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*               READ LINES UNTIL STRING FOUND               *                   
*************************************************************                   
FINDIO   NTR1                                                                   
         XC    TRTAB,TRTAB         SET UP SCAN TABLE FOR SCAN ROUTINE           
         ZIC   R1,FSTRING                                                       
         STC   R1,TRTAB(R1)                                                     
         L     R5,FLEN             R5 HOLDS LENGTH FOR SCAN ROUTINE             
         BCTR  R5,0                                                             
*                                                                               
         LA    R3,1000             SCAN A MAXIMUM OF 1000 LINES                 
         MVC   BPAGE,FPAGE         SET UP ARGUMENTS FOR FIRSTLIN                
         MVC   BLINE,FLINE                                                      
         BAS   RE,FIRSTLIN         FIND FIRST LINE                              
*                                                                               
FILOOP   CLC   QPAGE,=F'0'         CHECK EOF                                    
         BE    FI30                                                             
         BAS   RE,SCANLINE         SCAN PRINT LINE FOR MATCH                    
         CLC   FCOL,=F'0'                                                       
         BNE   FI20                MATCH FOUND                                  
         BCT   R3,FI10                                                          
         B     FI40                1000 LINES READ                              
FI10     BAS   RE,NEXTLINE                                                      
         B     FILOOP                                                           
*                                                                               
FI20     MVI   FINDRES,FOUNDIT     SET FLAG FOR SUCCESSFUL SEARCH               
         B     FIX                                                              
FI30     MVI   FINDRES,FOUNDEOF    SET FLAG FOR BOTTOM OF DATA REACHED          
         B     FIX                                                              
FI40     MVI   FINDRES,TOOFAR      SET FLAG FOR 1000 LINES SEARCHED             
FIX      MVC   FPAGE,QPAGE         SAVE PRINT QUE LOCATION FOR NOW              
         MVC   FLINE,QLINE                                                      
         B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*             SEARCH FOR MATCH ON CURRENT LINE              *                   
*************************************************************                   
SCANLINE NTR1                                                                   
         OC    PLINE,SPACES        RAISE PLINE TO UPPERCASE                     
         MVI   PLINE+171,0         PUT TAG AT END OF PLINE                      
         LA    R1,PLINE+1          R1 HOLDS CURRENT START OF SCAN               
         A     R1,FCOL             START ONE PAST OLD COLUMN                    
*                                                                               
SLLOOP   LA    R2,PLINE+171        R2 HOLDS LENGTH OF SCAN                      
         SR    R2,R1                                                            
         EX    R2,EXTRT            EXECUTE SCAN                                 
         BZ    SL10                FIRST CHAR OF FSTRING NOT FOUND              
         EX    R5,EXCLC            EXECUTE COMPARE                              
         BE    SL20                MATCH FOR FSTRING FOUND                      
         LA    R1,1(R1)            ADVANCE R1 TO NEXT CHAR                      
         B     SLLOOP                                                           
*                                                                               
SL10     MVC   FCOL,=F'0'          ZERO MEANS MATCH NOT FOUND                   
         B     SLX                                                              
SL20     LA    R0,PLINE            COMPUTE COL NUMBER WHERE MATCH FOUND         
         SR    R1,R0                                                            
         ST    R1,FCOL                                                          
SLX      B     DQXIT                                                            
*                                                                               
EXTRT    TRT   0(0,R1),TRTAB                                                    
EXCLC    CLC   0(0,R1),FSTRING                                                  
         EJECT                                                                  
*************************************************************                   
*       FIGURE OUT SCREEN CORNER FROM RESULT OF SEARCH      *                   
*************************************************************                   
FIGSCR   NTR1                                                                   
         CLI   FINDRES,FOUNDIT     CHECK SEARCH STRING WAS FOUND                
         BNE   FGS100                                                           
         CLC   FPAGE,SPAGE         CHECK WITHIN SCREEN BOUNDRIES                
         BNE   FGS5                                                             
         L     R0,FLINE            SLINE <= FLINE < SLINE+19 ?                  
         C     R0,SLINE                                                         
         BL    FGS5                                                             
         S     R0,=F'19'                                                        
         C     R0,SLINE                                                         
         BNL   FGS5                                                             
         L     R0,FCOL             SCOL <= FCOL < SCOL+80-FLEN ?                
         C     R0,SCOL                                                          
         BL    FGS5                                                             
         S     R0,=F'80'                                                        
         A     R0,FLEN                                                          
         C     R0,SCOL                                                          
         BNL   FGS5                                                             
         B     FGSX                                                             
*                                                                               
FGS5     L     R1,FLINE            CALCULATE NEW SCREEN CORNER                  
         C     R1,=F'2'                                                         
         BL    FGS10                                                            
         BCTR  R1,0                                                             
         ST    R1,SLINE            SLINE = FLINE-1                              
         B     FGS20                                                            
FGS10    MVC   SLINE,=F'1'         SLINE = 1                                    
FGS20    L     R1,FCOL                                                          
         C     R1,=F'11'                                                        
         BL    FGS30                                                            
         S     R1,=F'10'                                                        
         ST    R1,SCOL             SCOL = FCOL-10                               
         B     FGS40                                                            
FGS30    MVC   SCOL,=F'1'          SCOL = 1                                     
FGS40    MVC   SPAGE,FPAGE         SPAGE = FPAGE                                
         B     FGSX                                                             
*                                                                               
FGS100   CLI   FINDRES,FOUNDEOF    CHECK BOTTOM OF DATA REACHED                 
         BNE   FGS200                                                           
         MVC   FPAGE,=F'1'         SET FIND LOCATION TO BEGIN OF REPORT         
         MVC   FLINE,=F'1'                                                      
         MVC   FCOL,=F'0'                                                       
         B     FGSX                LEAVE SCREEN LOCATION UNCHANGED              
*                                                                               
FGS200   MVC   SPAGE,FPAGE         STRING NOT FOUND IN 1000 LINES               
         MVC   SLINE,FLINE                                                      
         MVC   SCOL,=F'1'                                                       
FGSX     B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*          FIGURE MESSAGE ABOUT RESULT OF SEARCH            *                   
*************************************************************                   
FIGMESS  NTR1                                                                   
         CLI   FINDRES,FOUNDIT     USE MESSAGE THAT CORRESPONDS WITH            
         BNE   DSF10               GIVEN RESULT OF SEARCH                       
         LA    R4,153              CHARS  FOUND                                 
         B     DSF30                                                            
DSF10    CLI   FINDRES,FOUNDEOF                                                 
         BNE   DSF20                                                            
         LA    R4,151              NOT FOUND BEFORE END                         
         B     DSF30                                                            
DSF20    LA    R4,152              NOT FOUND AFTER 1000 LINES                   
*                                                                               
DSF30    XC    DMCB(24),DMCB                                                    
         GOTO1 CGETTXT,DMCB,(R4),0,(C'I',0),(FLEN+3,FSTRING)                    
         B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*               PROCESS UP PAGE PFKEY REQUEST               *                   
*************************************************************                   
UPP      NTR1                                                                   
         CLC   SPAGE,OPVAL         CHECK FOR MAXIMUM PAGE UP                    
         BNH   UPPM                                                             
         L     R1,SPAGE            COMPUTE NEW PAGE                             
         S     R1,OPVAL                                                         
         ST    R1,SPAGE                                                         
         B     UPPX                                                             
UPPM     MVC   SPAGE,=F'1'         SET TO TOP PAGE                              
UPPX     B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*              PROCESS DOWN PAGE PFKEY REQUEST              *                   
*************************************************************                   
DOWNP    NTR1                                                                   
         L     R1,SPAGE            COMPUTE NEW PAGE                             
         A     R1,OPVAL                                                         
         ST    R1,SPAGE                                                         
         CLC   SPAGE,LASTPAGE      CHECK NOT TOO LARGE                          
         BNH   DOWNPX                                                           
         MVC   SPAGE,LASTPAGE      SET TO BOTTOM PAGE                           
DOWNPX   B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*              PROCESS UP LINE PFKEY REQUEST                *                   
*************************************************************                   
UPL      NTR1                                                                   
         CLC   SLINE,=F'1'         CHECK ON PAGE BORDER                         
         BNE   UPL50                                                            
         CLC   SPAGE,=F'1'         ARE WE ALREADY AT THE TOP                    
         BE    UPLX                                                             
         L     R1,SPAGE            GO BACK ONE PAGE                             
         BCTR  R1,0                                                             
         ST    R1,SPAGE                                                         
         ST    R1,BPAGE            COUNT NUMBER OF LINES ON THIS PAGE           
         LA    R5,1                                                             
         ST    R5,BLINE                                                         
         BAS   RE,FIRSTLIN                                                      
UPL10    CLC   QPAGE,BPAGE         ARE WE STILL ON THIS PAGE                    
         BNE   UPL20                                                            
         LA    R5,1(R5)                                                         
         BAS   RE,NEXTLINE                                                      
         B     UPL10                                                            
UPL20    ST    R5,SLINE            SLINE NOW EQUALS LAST LINE + 1               
         TM    SCRFLAG,SCROVR      OVERRIDE PROFILE?                            
         BNZ   UPL50                                                            
         CLC   SPAGE+3(1),PROFILE+(PROFIGNP-PROFD) IGNORE PAGE?                 
         BNH   UPL50                YES - TREAT SAME AS OVERRIDE                
         TM    SCRFLAG,SHOWPAGE    IF NOT SHOWING PAGE, NO PROBLEM              
         BZ    UPL50                                                            
*                                                                               
* SET ACTUAL NUMBER OF LINES TO SCROLL BACK, SINCE NO LONGER AT TOP             
         LA    RF,19                                                            
         ZIC   RE,PROFILE+(PROFNHDS-PROFD)                                      
         SR    RF,RE                                                            
         ST    RF,OPVAL                                                         
*                                                                               
UPL50    CLC   SLINE,OPVAL         CHECK FOR MAXIMUM LINES UP                   
         BNH   UPLM                                                             
         L     R1,SLINE            COMPUTE NEW LINE NUMBER                      
         S     R1,OPVAL                                                         
         ST    R1,SLINE                                                         
         B     UPLX                                                             
UPLM     MVC   SLINE,=F'1'                                                      
UPLX     B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*             PROCESS DOWN LINE PFKEY REQUEST               *                   
*************************************************************                   
DOWNL    NTR1                                                                   
         MVC   BPAGE,SPAGE         SET UP AGUMENTS TO FIRSTLIN                  
         MVC   BLINE,SLINE                                                      
         BAS   RE,FIRSTLIN         READ OPVAL NUMBER OF LINES                   
         L     R1,OPVAL                                                         
DOWNL10  CLC   QPAGE,BPAGE         OR UNTIL NEW PAGE IS ENCOUNTERED             
         BNE   DOWNL20                                                          
         BAS   RE,NEXTLINE                                                      
         BCT   R1,DOWNL10                                                       
DOWNL20  CLC   QPAGE,=F'0'         CHECK END OF REPORT                          
         BE    DOWNLS                                                           
         MVC   SPAGE,QPAGE         SET SPAGE AND SLINE                          
         MVC   SLINE,QLINE                                                      
         B     DOWNLX                                                           
DOWNLS   BAS   RE,INFO2            END OF REPORT                                
DOWNLX   B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*            PROCESS LEFT COLUMN PFKEY REQUEST              *                   
*************************************************************                   
LEFTC    NTR1                                                                   
         L     R1,SCOL             COMPUTE NEW COLUMN NUMBER                    
         TM    SCRFLAG,SCROVR                                                   
         BNZ   LC10                                                             
         CLC   SPAGE+3(1),PROFILE+(PROFIGNP-PROFD) IGNORE PAGE?                 
         BNH   LC10                 YES - TREAT SAME AS OVERRIDE                
         TM    SCRFLAG,KEEPSDS     USING CHUNKS?                                
         BZ    LC10                                                             
         ZIC   RF,VARSCRL                                                       
         CR    R1,RF               CHECK FOR MAX SCROLL LEFT                    
*         BNH   LC20                                                            
         BNH   LEFTCM                                                           
         SR    R1,RF                                                            
         B     LC20                                                             
LC10     CLC   SCOL,OPVAL          CHECK MAXIMUM SCROLL LEFT                    
         BNH   LEFTCM                                                           
         S     R1,OPVAL                                                         
LC20     ST    R1,SCOL                                                          
         B     LEFTCX                                                           
LEFTCM   MVC   SCOL,=F'1'          SET LEFT-MOST COLUMN NUMBER                  
LEFTCX   B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*            PROCESS RIGHT COLUMN PFKEY REQUEST             *                   
*************************************************************                   
RIGHTC   NTR1                                                                   
         L     R1,SCOL             COMPUTE NEW COLUMN NUMBER                    
         TM    SCRFLAG,SCROVR                                                   
         BNZ   RC10                                                             
         CLC   SPAGE+3(1),PROFILE+(PROFIGNP-PROFD) IGNORE PAGE?                 
         BNH   RC10                 YES - TREAT SAME AS OVERRIDE                
         TM    SCRFLAG,KEEPSDS     USING CHUNKS?                                
         BZ    RC10                                                             
         ZIC   RF,VARSCRL                                                       
         AR    R1,RF                                                            
         B     *+8                                                              
*                                                                               
RC10     A     R1,OPVAL                                                         
         ST    R1,SCOL                                                          
         CLC   SCOL,=F'170'        CHECK MAXIMUM SCROLL RIGHT                   
         BNH   RIGHTCX                                                          
         MVC   SCOL,=F'170'        SET RIGHT-MOST COLUMN NUMBER                 
RIGHTCX  B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*              DISPLAY RESULTS OF PROCESSING                *                   
*************************************************************                   
DISPLAY  NTR1                                                                   
         BAS   RE,DISPTOP          DISPLAY TOP FIELDS                           
*                                                                               
         CLI   PFKEY,PFHELP        CHECK FOR HELP KEY PRESSED                   
         BNE   *+12                                                             
         BAS   RE,DISPHELP         DISPLAY HELP SCREEN                          
         B     DSX                 EXIT                                         
*                                                                               
         LA    R2,DQUL1H           INITIALIZE R2 TO FIRST DISPLAY FIELD         
         LA    R1,19               INITIALIZE LINE COUNTER TO 19                
*                                                                               
         TM    SCRFLAG,SCROVR      OVERRIDE PROFILE?                            
         BNZ   DS5                                                              
         CLC   SPAGE+3(1),PROFILE+(PROFIGNP-PROFD) IGNORE PAGE?                 
         BNH   DS5                  YES - TREAT SAME AS OVERRIDE                
         TM    SCRFLAG,KEEPHDS     ARE THERE ANY HDLINES TO KEEP?               
         BZ    DS5                                                              
         TM    SCRFLAG,SHOWPAGE    IF SET AND 1ST LINE, SHOW PAGE               
         BZ    DS3                                                              
         CLC   =F'1',SLINE                                                      
         BNE   DS3                                                              
         MVC   SCROLL,=F'19'                                                    
         EDIT  SCROLL,(4,DQUSCR),ALIGN=LEFT                                     
         B     DS5                                                              
*                                                                               
DS3      BAS   RE,DISPHDS                                                       
*                                                                               
* SET NUMBER OF LINES TO SCROLL FOR NEXT SCREEN                                 
         LA    RF,19                                                            
         ZIC   RE,PROFILE+(PROFNHDS-PROFD)                                      
         SR    RF,RE                                                            
         ST    RF,SCROLL                                                        
         EDIT  (RF),(4,DQUSCR),ALIGN=LEFT                                       
*                                                                               
* MAKE SURE (PROFHDST + PROFNHDS) <= SLINE                                      
         LA    RF,PROFILE                                                       
         USING PROFD,RF                                                         
         ZIC   RE,PROFHDST                                                      
         ZIC   R0,PROFNHDS                                                      
         DROP  RF                                                               
         AR    RE,R0               R0=PROFHDST + PROFNHDS                       
         C     RE,SLINE                                                         
         BNH   DS5                                                              
         ST    RE,BLINE                                                         
         B     *+10                                                             
*                                                                               
DS5      MVC   BLINE,SLINE                                                      
         MVI   EOPFLAG,0           INITIALIZE END OF PAGE FLAG                  
         MVC   BPAGE,SPAGE         SET UP ARGUMENTS TO FIRSTLIN                 
         BAS   RE,FIRSTLIN         READ UP TO FIRST LINE                        
*                                                                               
DSLOOP   CLC   QPAGE,BPAGE         HAS NEW PAGE BEEN ENCOUNTERED                
         BE    DS10                                                             
         MVI   EOPFLAG,1           SET END OF PAGE FLAG                         
         XC    PLINE,PLINE         DISPLAY BLANK LINES FROM NOW ON              
         XC    QLINE,QLINE         CLEARED FOR MARK CURSOR ALGORITH             
DS10     DS    0H                                                               
         LA    R3,PLINE                                                         
         TM    SCRFLAG,SCROVR      OVERRIDE PROFILE?                            
         BNZ   DS30                                                             
         CLC   SPAGE+3(1),PROFILE+(PROFIGNP-PROFD) IGNORE PAGE?                 
         BNH   DS30                 YES - TREAT SAME AS OVERRIDE                
*                                                                               
* IF AT TOP OF PAGE AND SHOWING ALL LINES, SHIFT 'FLOATING' HEADLINES           
* RIGHT TO START AT 1ST DISPLAY COL                                             
         CLC   =F'1',SLINE         AT TOP OF PAGE?                              
         BNE   DS20                 NO                                          
         TM    SCRFLAG,SHOWPAGE    ARE WE SHOWING ALL LINES?                    
         BZ    DS20                 NO                                          
         LA    RF,20               NUMBER OF LINES PER PAGE+1 (1 BASED)         
         SR    RF,R1               R1 = COUNTER OF LINES TO PRINT (BCT)         
         ZIC   RE,PROFILE+(PROFHDST-PROFD)   WHERE 'FLOATING' HEADS END         
         CR    RF,RE                                                            
         BNL   DS20                                                             
         IC    RE,PROFILE+(PROFEDCL-PROFD)                                      
         SR    R3,RE               PRINT REAL HEADS FROM LEFTMOST COL           
         B     DS30                                                             
*                                                                               
DS20     TM    SCRFLAG,KEEPSDS                                                  
         BZ    DS30                                                             
         BAS   RE,VARCOLS                                                       
         B     DS40                                                             
*                                                                               
DS30     A     R3,SCOL             R3 POINTS TO CORRECT COLUMN                  
         LA    RF,PLINE+1                                                       
         CR    R3,RF               STARTING BEFORE PLINE?                       
         BNL   *+6                  NO                                          
         LR    R3,RF               DON'T ALLOW IT TO HAPPEN...                  
         MVC   8(79,R2),0(R3)      MOVE DATA INTO SCREEN FIELD                  
DS40     OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         CLI   FINDRES,FOUNDIT     HAS A STRING BEEN FOUND                      
         BNE   DS50                                                             
         CLC   QLINE,FLINE         IS THIS THE LINE WHERE IT WAS FOUND          
         BNE   DS50                                                             
         L     R5,SRPATIOB         PUT CURSOR AT STRING FOUND                   
         USING TIOBD,R5                                                         
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STCM  R0,3,TIOBCURD       DISPLACEMENT TO FIELD HEADER                 
         L     R0,FCOL                                                          
         S     R0,SCOL                                                          
         STC   R0,TIOBCURI         DIPLACEMENT WITHIN FIELD                     
         OI    TIOBINDS,TIOBSETC                                                
         NI    DQUOPTH+6,X'BF'     UNDO CURSOR MARKER FROM DISPTOP              
         DROP  R5                                                               
*                                                                               
DS50     ZIC   R0,0(R2)            BUMP R2 TO NEXT FIELD                        
         AR    R2,R0                                                            
         BCT   R1,DS60             DISPLAY ONLY 19 LINES                        
         B     DSX                 19 FIELDS DISPLAYED                          
*                                                                               
DS60     CLI   EOPFLAG,1                                                        
         BE    DS10                                                             
         BAS   RE,NEXTLINE         GET NEW DISPLAY LINE                         
         B     DSLOOP                                                           
*                                                                               
DSX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*                    DISPLAY TOP FIELDS                     *                   
*************************************************************                   
DISPTOP  NTR1                                                                   
         XC    DQUPAGE,DQUPAGE                                                  
         XC    DQULINE,DQULINE                                                  
         XC    DQUCOL,DQUCOL                                                    
         XC    DQUOPT,DQUOPT                                                    
         XC    DQUSCR,DQUSCR                                                    
         OI    DQUPAGEH+6,X'80'                                                 
         OI    DQULINEH+6,X'80'                                                 
         OI    DQUCOLH+6,X'80'                                                  
         OI    DQUOPTH+6,X'80'                                                  
         OI    DQUSCRH+6,X'80'                                                  
         EDIT  SPAGE,(4,DQUPAGE),ALIGN=LEFT                                     
         EDIT  SLINE,(5,DQULINE),ALIGN=LEFT                                     
         EDIT  SCOL,(4,DQUCOL),ALIGN=LEFT                                       
         EDIT  SCROLL,(4,DQUSCR),ALIGN=LEFT                                     
         CLI   HELPNUM,0                                                        
         BNE   DISX                                                             
         OI    DQUOPTH+6,X'40'     CURS TO OPT UNLESS HELP                      
DISX     B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*              DISPLAY HELP SCREEN DATA                     *                   
*************************************************************                   
DISPHELP NTR1                                                                   
         MVC   DQUMSG(27),=C'PRINT QUEUE DISPLAY PROGRAM'      ??               
         OI    DQUMSGH+6,X'80'                                                  
         LA    R2,DQUL1H           FIRST DISPLAY LINE                           
         L     R3,AHELPSCR         FIRST HELP LINE                              
         LA    R4,L'HELPSCR        BXLE ARGUMENTS                               
         LA    R5,HELPSCRL(R3)                                                  
         BCTR  R5,0                                                             
DHLOOP   MVC   8(79,R2),0(R3)      MOVE LINE IN                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)            BUMP TO NEXT DISPLAY LINE                    
         AR    R2,R0                                                            
         BXLE  R3,R4,DHLOOP        BUMP TO NEXT HELP LINE                       
         B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*       READ UNTIL FIRST LINE THAT IS TO BE DISPLAYED       *                   
*************************************************************                   
FIRSTLIN NTR1                                                                   
         MVC   QPAGE,BPAGE         INITIALIZE PAGE-LINE FLAGS                   
         MVC   QLINE,=F'0'                                                      
         XC    PLINE,PLINE         READ UNTIL BEGINNING OF PAGE                 
         MVC   PLINE+0(4),BPAGE                                                 
         MVC   PLINE+4(4),=C'PAGE'                                              
         GOTO1 CDATAMGR,DMCB,(X'01',RANDOM),PRTQID,KEY,PLINE,CIREC,0            
         CLI   8(R1),0                                                          
         BNE   ERR12               DISK ERROR                                   
*                                                                               
FL10     BAS   RE,NEXTLINE         READ LINES UNTIL                             
         CLC   QPAGE,BPAGE                                                      
         BNE   FLX                 END OF PAGE OR REPORT IS FOUND               
         CLC   QLINE,BLINE                                                      
         BE    FLX                 OR CORRECT LINE IS FOUND                     
         B     FL10                                                             
FLX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*               READ NEXT LINE OF REPORT                    *                   
*************************************************************                   
NEXTLINE NTR1                                                                   
NL5      CLI   PAGEWAIT,1          CHECK FOR SKIP TO CHANNEL 1                  
         BNE   NL10                                                             
         MVI   PAGEWAIT,0                                                       
         AF    QPAGE,=F'1'         SET NEW PAGE                                 
         MVC   QLINE,=F'0'                                                      
*                                                                               
NL10     GOTO1 CDATAMGR,DMCB,(X'01',READ),PRTQID,KEY,PLINE,CIREC,0              
         CLI   8(R1),0                                                          
         BE    NL20                                                             
         TM    8(R1),X'80'         TEST EOF                                     
         BO    NLEOF                                                            
         DC    H'0'                DIE ON DISK ERROR                            
*                                  TRANLATE TO SCREEN DISPLAYABLE               
NL20     DS    0H                                                               
         L     RF,AVALCHRS                                                      
         TR    PLINE+1(L'PLINE-1),0(RF)                                         
         CLI   PLINE,X'89'         TEST FOR DATA THEN EJECT                     
         BE    NL25                                                             
         CLI   PLINE,X'8B'         TEST FOR EJECT THAN DATA                     
         BNE   NL30                                                             
         AF    QPAGE,=F'1'         SET NEW PAGE                                 
         MVC   QLINE,=F'0'                                                      
         B     NL30                                                             
*                                                                               
NL25     MVI   PAGEWAIT,1                                                       
*                                  SKIP BLANK LINES                             
NL30     CLC   PLINE+1(L'PLINE-1),SPACES                                        
         BE    NL5                                                              
         AF    QLINE,=F'1'         SET NEXT LINE                                
         B     NLX                                                              
*                                  SKIP BLANK LINES                             
NLEOF    MVC   QPAGE,=F'0'         PAGE ZERO MEANS EOF                          
NLX      B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*  DISPLAY VARIABLE LENGTH & FIXED COLS                     *                   
*                                                           *                   
*   EXPECTS:                                                *                   
*    R2 = DISPLAY LINE                                      *                   
*    R3 = PLINE                                             *                   
*                                                           *                   
*************************************************************                   
VARCOLS  NTR1                                                                   
         LA    R4,PROFILE                                                       
         USING PROFD,R4                                                         
         ZIC   RF,PROFSTCL         GET KEEP START COL                           
         ZIC   RE,PROFEDCL         GET KEEP END COL                             
         DROP  R4                                                               
         AR    R3,RF               R3 POINTS AT FIRST CHAR ON SCREEN            
         BCTR  RF,0                                                             
         SR    RE,RF               NUMBER OF COLS TO KEEP                       
         BCTR  RE,0                FOR EX                                       
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)       DISPLAY FIXED COL                            
*                                                                               
* DISPLAY REST OF DATA                                                          
         LA    R2,8+1(RE,R2)       NXT AVAIL SPC (+1 SINCE RE BCTR'ED)          
         LA    R3,PLINE                                                         
*                                                                               
* IF DISPLAYING FLOATING CHUNKS, FIGURE OUT WHICH TO DISPLAY                    
         LA    RF,PROFILE+(PROFFLT1-PROFD)    RF=A(1ST FLT COL)                 
         LA    R0,4                                                             
*                                                                               
VCL10    DS    0H                                                               
         CLI   0(RF),0                                                          
         BE    VCL100              NO MORE FLOAT COLS DEFINED                   
         CLC   0(1,RF),SCOL+3      FLOAT COL < SCOL?                            
         BNL   VCL20                                                            
         LA    RF,1(RF)                                                         
         BCT   R0,VCL10                                                         
         B     VCL100                                                           
*                                                                               
VCL20    DS    0H                                                               
         ZIC   R5,0(RF)            R5 = FLOAT END COL                           
         CH    R0,=H'4'            FIRST CHUNK?                                 
         BNE   VCL30                                                            
         ZIC   R1,PROFILE+(PROFEDCL-PROFD)   R1=PROFEDCL                        
         B     VCL40                                                            
VCL30    BCTR  RF,0                RF TO PREV FLOAT ED COL                      
         ZIC   R1,0(RF)                                                         
VCL40    DS    0H                                                               
         LA    R1,1(R1)            R1 = SCOL                                    
         CR    R5,R1               CHUNK-END > CHUNK-ST?                        
         BH    *+10                 YES, OK                                     
         LR    R5,R1               ELSE MAKE IT CHUNK-ST + 1                    
         LA    R5,1(R5)                                                         
*                                                                               
         AR    R3,R1                                                            
         SR    R5,R1               R5 = OUTPUT LENGTH                           
         LR    RF,R5               FOR EX AT VCL110                             
         ST    R1,SCOL                                                          
*                                                                               
         LA    R0,79                                                            
         LA    R1,1(RE,RF)         L'TOTAL OUTPUT - RE BCTR'ED                  
         CR    R1,R0               TOTAL OUTPUT > 79?                           
         BL    *+16                 NO                                          
         SR    R0,RE                                                            
         BCTR  R0,0                                                             
         LTR   RF,R0                                                            
         BZ    VCLX                SOMETHINGS FUBAR, DO NOTHING!                
         BCTR  RF,0                                                             
*                                                                               
         LA    R5,1(R5)                                                         
         STC   R5,VARSCRL                                                       
         EDIT  SCOL,(4,DQUCOL),ALIGN=LEFT                                       
         OI    DQUCOLH+6,X'80'                                                  
         B     VCL110                                                           
*                                                                               
* MAKE SURE PROFEDCL < SCOL                                                     
VCL100   ZIC   R5,PROFILE+(PROFEDCL-PROFD)   R5=PROFEDCL                        
         C     R5,SCOL                                                          
         BL    *+14                                                             
         AR    R3,R5               MIN START COL OF DATA WANTED                 
         LA    R3,1(R3)                                                         
         B     *+8                                                              
         A     R3,SCOL             START COL OF DATA WANTED                     
*                                                                               
         SR    RF,RF                                                            
         LA    RF,79                                                            
         SR    RF,RE               NUMBER OF CHARS LEFT ON SCREEN               
         BCTR  RF,0                RE WAS BCTR'ED                               
         BCTR  RF,0                FOR EX                                       
VCL110   EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
*                                                                               
VCLX     B     DQXIT                                                            
         EJECT                                                                  
*************************************************************                   
*  KEEP HEADLINES ON SCREEN                                 *                   
*                                                           *                   
*   EXPECTS:                                                *                   
*    R1 = LINE COUNTER                                      *                   
*    R2 = FIRST DISPLAY FIELD                               *                   
*                                                           *                   
*   RETURNS:                                                *                   
*    R1 = UPDATED LINE COUNTER                              *                   
*    R2 = NEXT AVAIL DISPLAY FIELD                          *                   
*                                                           *                   
*************************************************************                   
DISPHDS  NTR1                                                                   
         LA    R4,PROFILE                                                       
         USING PROFD,R4                                                         
*                                                                               
         MVC   BPAGE,SPAGE         SET ARGS TO FIRSTLIN                         
         XC    BLINE,BLINE                                                      
         MVC   BLINE+3(1),PROFHDST                                              
         BAS   RE,FIRSTLIN                                                      
*                                                                               
         ZIC   R5,PROFNHDS         GET NUMBER OF HEADLINES                      
         SR    R0,R0                                                            
DH10     DS    0H                                                               
         LA    R3,PLINE                                                         
         TM    SCRFLAG,KEEPSDS                                                  
         BZ    *+12                                                             
         BAS   RE,VARCOLS                                                       
         B     *+14                                                             
*                                                                               
         A     R3,SCOL             R3 TO CORRECT COL                            
         MVC   8(79,R2),0(R3)      MOVE DATA INTO SCREEN FLD                    
         OI    6(R2),X'80'         XMIT FLD                                     
*                                                                               
         IC    R0,0(R2)            NEXT FIELD                                   
         AR    R2,R0                                                            
         BCTR  R1,0                1 LESS OUTPUT LINE                           
         BAS   RE,NEXTLINE         GET NEXT HDLINE                              
         BCT   R5,DH10                                                          
*                                                                               
DHX      XIT1  REGS=(R1,R2)                                                     
         DROP  R4                                                               
         EJECT                                                                  
*************************************************************                   
*  READ PROFILE REC AND SET SCROLL FLAGS                    *                   
*************************************************************                   
RDPROF   NTR1                                                                   
         NI    SCRFLAG,X'00'+USERPROF    RESET ALL BUT USERPROF FLAG            
         XC    PROFILE,PROFILE                                                  
         XC    WORK,WORK                                                        
         BAS   RE,GETSYS                                                        
         CLI   WORK,0                                                           
         BE    RDX                 N/A FOR SYSTEM                               
*                                                                               
* IF THIS IS A WRITER REPORT, LOOK FOR A DQU WRITER PROFILE                     
         L     R4,AWRITBLE                                                      
         USING WRITBLED,R4                                                      
*                                                                               
         CLC   WRSYS(3),SYSTEM     MATCH ON SPP                                 
         BE    *+20                 YES                                         
RD3      LA    R4,21(R4)           NEXT ENTRY                                   
         CLI   0(R4),X'FF'         EOT?                                         
         BNE   *-18                 NO                                          
         B     RD5                  YES - NOT A WRITER TYPE PGM                 
*                                                                               
         TM    SCRFLAG,USERPROF    USER SUPPLIED NAME?                          
         BZ    *+12                                                             
         LA    R2,SCANOUT+22       R2 = A(WRITER NAME)                          
         B     RD4                                                              
*                                                                               
         MVC   BPAGE,WRPAGE                                                     
         MVC   BLINE,WRLINE                                                     
         BAS   RE,FIRSTLIN         FIND WRITER NAME                             
         LA    RF,PLINE                                                         
         ZIC   R0,WRKWCOL                                                       
         AR    RF,R0                                                            
         CLC   0(L'WRKEYWRD,RF),WRKEYWRD      IS THE NAME FLD HERE?             
         BNE   RD3                 NO - SEE IF ANOTHER ENTRY                    
         LA    R2,PLINE                                                         
         IC    R0,WRNAMCOL                                                      
         AR    R2,R0               R2 = A(WRITER NAME)                          
         DROP  R4                                                               
*                                                                               
RD4      XC    KEY,KEY             READ FOR DQU WRITER TYPE PROFILE             
         LA    R4,KEY                                                           
         USING CTUREC,R4                                                        
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVC   CTUKSYS,WORK            SYS AND PROG SET IN WORK...              
         MVC   CTUKPROG+1(2),WORK+2    ...BY GETSYS CALL                        
         MVC   CTUKAGY,TAGY                                                     
         GOTO1 =V(SIXPACK),DMCB,(R2),CTUKNAM,8,RR=RELO                          
         DROP  R4                                                               
         GOTO1 CDATAMGR,DMCB,(X'00',=C'DMREAD'),=C'CTFILE',KEY,CTIOAREA         
         CLI   DMCB+8,0                                                         
         BE    *+16                                                             
         TM    SCRFLAG,USERPROF    DID USER ASK FOR PROFILE?                    
         BNZ   RDX                  YES - NO DEFAULT PROFILE                    
         B     RD5                  NO - TRY FOR A DEFAULT                      
*                                                                               
         LA    R4,CTIOAREA                                                      
         MVI   ELCODE,X'72'                                                     
         BAS   RE,GETEL                                                         
         BNE   RD5                 SHOULD BE=, BUT IF NOT, USE GETPROF          
         MVC   PROFILE,CTPVALUE-CTPVEL(R4)     SAVE PROFILE                     
         B     RD20                                                             
*                                                                               
RD5      MVC   WORK+4(2),TAGY                                                   
         TM    SCRFLAG,USERPROF    DID USER ASK FOR PROFILE?                    
         BZ    *+10                 NO                                          
         MVC   WORK+2(2),SCANOUT+22                                             
         GOTO1 CGETPROF,DMCB,WORK,PROFILE,CDATAMGR                              
         OC    PROFILE,PROFILE     PROFILE FOUND?                               
         BNZ   RD20                                                             
         TM    SCRFLAG,USERPROF    DID USER ASK FOR PROFILE?                    
         BNZ   RDX                  YES - NO DEFAULT PROFILE                    
*                                                                               
* NO PROFILE REC WAS FOUND.  USE DEFAULT OF FIELD REC IF IT EXISTS              
         XC    KEY,KEY                                                          
         LA    RF,KEY                                                           
         USING CTUREC,RF                                                        
         MVI   CTUKTYP,CTUKTYPQ                                                 
         MVC   CTUKSYS,WORK            SYS AND PROG SET IN WORK...              
         MVC   CTUKPROG+1(2),WORK+2    ...BY GETSYS CALL                        
         DROP  RF                                                               
         GOTO1 CDATAMGR,DMCB,(X'00',=C'DMREAD'),=C'CTFILE',KEY,CTIOAREA         
         CLI   DMCB+8,0            ANY ERROR?                                   
         BNE   RDX                 ASSUME NOT FOUND                             
*                                                                               
* GET DEFAULT VALUES FROM X'70' ELEMS AND SAVE IN PROFILE                       
         LA    R1,PROFILE                                                       
         LA    R4,CTIOAREA                                                      
         MVI   ELCODE,X'70'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDX                                                              
         USING CTFDD,R4                                                         
RD10     DS    0H                                                               
         CLI   CTFDNUM,16          LIVE FIELD (NOT USED ON PROFILES)            
         BNE   RD15                                                             
         CLI   CTFDDEF,C'Y'        IS THIS LIVE?                                
         BE    RD20                 YES (DON'T NEED TO SAVE)                    
         XC    PROFILE,PROFILE     SET NO PROFILE AND EXIT                      
         B     RDX                                                              
*                                                                               
RD15     MVC   0(1,R1),CTFDDEF     MOVE DEFAULT VALUE TO PROFILE                
         LA    R1,1(R1)                                                         
         BAS   RE,NEXTEL                                                        
         BE    RD10                                                             
         DROP  R4                                                               
*                                                                               
* SET SCRFLAG ACCORDING TO PROFILE                                              
RD20     DS    0H                                                               
         LA    RF,PROFILE                                                       
         USING PROFD,RF                                                         
         OI    SCRFLAG,PROFND      SET PROFILE REC FOUND                        
*                                                                               
         CLI   PROFNHDS,0          KEEP NO HEADLINES?                           
         BE    *+16                                                             
         CLI   PROFHDST,0          NO HEADLINE START?                           
         BE    *+8                                                              
         OI    SCRFLAG,KEEPHDS                                                  
*                                                                               
         CLI   PROFSTCL,0          NO START KEEP COL                            
         BE    *+18                                                             
         CLC   PROFEDCL,PROFSTCL   END COL <= START COL                         
         BNH   *+8                                                              
         OI    SCRFLAG,KEEPSDS                                                  
*                                                                               
         CLI   PROFPAGE,C'Y'                                                    
         BNE   *+8                                                              
         OI    SCRFLAG,SHOWPAGE                                                 
*                                                                               
         B     RDX                                                              
         DROP  RF                                                               
RDX      NI    SCRFLAG,X'FF'-USERPROF   RESET USER PROFILE FLAG                 
         XIT1                           DNCTFITABTX                             
* (DONOTCHANGETHISFUCKINGINSTRUCTIONTOABRANCHTOXIT !)                           
         EJECT                                                                  
*************************************************************                   
*       GET SYSTEM AND PROGRAM FOR PROFILE REQUEST          *                   
*                                                           *                   
*    SYSTEM CODE X'FS' (S = 0 -> F) S = SYSTEM              *                   
*    PROGRAM COPIED FROM PQPRG                              *                   
*     RETURNED IN WORK AS C'S0NN' FOR GETPROF CALL          *                   
*                                                           *                   
*************************************************************                   
GETSYS   NTR1                                                                   
         L     RF,ADQUPROF                                                      
*                                                                               
         CLC   1(1,RF),SYSTEM      MATCH ON SYSTEM?                             
         BE    *+20                                                             
         LA    RF,3(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         BNE   *-18                                                             
         B     GSX                                                              
*                                                                               
         MVC   WORK(1),2(RF)       SET SYSTEM                                   
         MVI   WORK+1,C'0'                                                      
         MVC   WORK+2(2),PROGRAM   SET PROGRAM                                  
GSX      XIT1                                                                   
         EJECT                                                                  
*************************************************************                   
*               PROCESS PF4 REQUEST                         *                   
*************************************************************                   
         SPACE 1                                                                
PQRET    L     R1,TBUFF            LOCATE TERMINAL BUFFER                       
         MVI   0(R1),X'09'         LENGTH FOR XX=PQ,#R                          
         MVC   1(2,R1),DQUSRVH+2   INSERT SCREEN ADDRESSES                      
*&&UK*&& MVC   3(6,R1),=C'=PQ,#R'                                               
*&&US*&& MVC   3(6,R1),=C'=QU,#R'                                               
         MVI   10(R1),0                                                         
*                                                                               
PQRET1   CLC   DQUSRV+1(4),=C'DQUP' TEST PUBLIC DQU                             
         BNE   PQRET2                                                           
         MVI   0(R1),X'0A'         LENGTH FOR XX=PQP,#R                         
         MVC   1(2,R1),DQUSRVH+2   INSERT SCREEN ADDRESSES                      
*&&UK*&& MVC   3(7,R1),=C'=PQP,#R'                                              
*&&US*&& MVC   3(7,R1),=C'=QUP,#R'                                              
         MVI   11(R1),0                                                         
*                                                                               
PQRET2   CLC   DQUSRV+1(4),=C'DQUG' TEST GROUP DQU                              
         BNE   PQRETX                                                           
         MVI   0(R1),X'0A'         LENGTH FOR XX=PQG,#R                         
         MVC   1(2,R1),DQUSRVH+2   INSERT SCREEN ADDRESSES                      
*&&UK*&& MVC   3(7,R1),=C'=PQG,#R'                                              
*&&US*&& MVC   3(7,R1),=C'=QUG,#R'                                              
         MVI   11(R1),0                                                         
*                                                                               
PQRETX   MVC   DQUSRV(8),=C'=GOBACK '                                           
         B     EXIT                                                             
         EJECT                                                                  
*************************************************************                   
*                CALL GETHELP AND EXIT                      *                   
*************************************************************                   
HELPOUT  L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         GOTO1 CGETHELP,DMCB,(X'50',HELPKEY),QHDR,0,0,0                         
         DC    H'0'                GETHELP EXITS TO MONITOR                     
*************************************************************                   
*                       INFO MESSAGES                       *                   
*************************************************************                   
INFO1    LA    R0,150              ENTER A FIND COMMAND                         
         B     INFOX                                                            
INFO2    LA    R0,154              END OF REPORT                                
         B     INFOR                                                            
INFO3    LA    R0,155              ENTER SPOOL ID                               
         OI    6(R2),X'40'         CURSOR POS TO SPOOL ID                       
         XC    REPSUBID,REPSUBID   FORCE NEW REPORT                             
         L     RF,=A(WRITESTR)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         B     INFOX                                                            
INFO4    LA    R0,187              REPORT IS IN ERROR                           
         B     INFOR                                                            
INFOX    LA    RE,EXIT                                                          
INFOR    XC    DMCB(24),DMCB                                                    
         ST    RE,FULL             SAVE RETURN                                  
         GOTO1 CGETTXT,DMCB,(R0),0,(C'I',0)                                     
         L     RE,FULL                                                          
         BR    RE                                                               
*************************************************************                   
*                       ERROR MESSAGES                      *                   
*************************************************************                   
ERR0     LA    RE,SREMBC           MUST BE CONNECTED                            
         B     ERRS                                                             
ERR1     LA    RE,SREMIF           MISSING INPUT FIELD                          
         B     ERRS                                                             
ERR2     LA    RE,SREIIF           INVALID INPUT FIELD                          
         B     ERRS                                                             
ERR3     LA    RE,SRERNF           REPORT NOT FOUND                             
         B     ERRS                                                             
ERR4     LA    RE,SREIPN           INVALID PAGE NUMBER                          
         B     ERRX                                                             
ERR5     LA    RE,SREILN           INVALID LINE NUMBER                          
         B     ERRX                                                             
ERR6     LA    RE,SREICN           INVALID COLUMN NUMBER                        
         B     ERRX                                                             
ERR10    LA    RE,SREISA           INVALID SCROLL AMOUNT                        
         B     ERRX                                                             
ERR11    LA    RE,SREUID           INVALID USER ID                              
         B     ERRS                                                             
ERR12    LA    RE,SREDSK           DISK ERROR - CONTACT DDS                     
         LA    R2,DQUSPIDH                                                      
         B     ERRX                                                             
ERRS     STH   RE,ERRNUM                                                        
         XC    REPSUBID,REPSUBID   FORCE NEW REPORT                             
         L     RF,=A(WRITESTR)                                                  
         A     RF,RELO                                                          
         BASR  RE,RF                                                            
         LH    RE,ERRNUM                                                        
*************************************************************                   
*         OUTPUT ERROR MESSAGES   RE=MSG NUMBER             *                   
*************************************************************                   
ERRX     OI    6(R2),X'40'         CURSOR POS                                   
         XC    DMCB(24),DMCB                                                    
         GOTO1 CGETTXT,DMCB,(RE),0,(C'E',0)                                     
         SPACE 1                                                                
EXIT     L     RD,SAVEDRD          POPS SAVE AREA BACK TO BEGINNING             
         CLI   HELPNUM,0           WAS HELP REQUESTED                           
         BNE   HELPOUT                                                          
         B     DQXMOD              EXITS FROM PROGRAM                           
         EJECT                                                                  
*************************************************************                   
*                       CONSTANTS                           *                   
*************************************************************                   
DDDCLST  DS    0C                                                               
         DCDDL SR#FIND,3,L                                                      
         DCDDL SR#MAX,3,L                                                       
         DCDDL SR#PSWD,3,L                                                      
*                                                                               
BUFFER   DC    CL8'BUFFER'                                                      
GFILE    DC    CL8'GFILE'                                                       
INDEX    DC    CL8'INDEX'                                                       
RANDOM   DC    CL8'RANDOM'                                                      
READ     DC    CL8'READ'                                                        
PRTQUE   DC    CL8'PRTQUE'                                                      
DDSPSWD  DC    CL6'DDSPQ '                                                      
FFS      DC    X'FFFFFFFFFFFFFFFF'                                              
*                                                                               
INVGRPID DS    0CL8                INVALID GROUP IDS - EXECUTED CLC             
         DC    AL1(3),CL7'DDS'                                                  
*&&UK*&& DC    AL1(3),CL7'TST'                                                  
*&&US*&& DC    AL1(2),CL7'SJ'                                                   
*&&US*&& DC    AL1(3),CL7'TCH'                                                  
INVGRPIX DC    AL1(0),CL7' '                                                    
*                                                                               
PUBENG   DC    AL2(32000)                                                       
PUBGER   DC    AL2(32000)                                                       
*                                                                               
SPACES   DC    CL250' '                                                         
*                                                                               
HELPID   DC    XL10'0159FF00000000000000'  SYS/PRG/SCRN                         
*                                                                               
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*************************************************************                   
*                       LITERALS                            *                   
*************************************************************                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*                VALIDATE OPTION FIELD                      *                   
*************************************************************                   
VALOPT   NTR1  BASE=*,LABEL=*                                                   
VO1      CLC   DQUOPT(1),SR@FIND   CHECK FOR SPECIAL FIND OPTION                
         BNE   VO10                                                             
         CLI   DQUOPT+1,C'?'       CHECK FOR FIND HELP                          
         BNE   VO5                                                              
         LA    R2,DQUOPTH                                                       
         MVC   9(2,R2),=C'  '      REMOVE F?                                    
         ST    R2,QHDR                                                          
         MVI   HELPNUM,7           SET HELP PAGE                                
         B     VOX                                                              
VO5      BAS   RE,VALFIND          VALIDATE FIND OPTION                         
         B     VOXX                                                             
VO10     LA    R2,DQUOPTH          PROCEED WITH STANDARD SCROLL OPTIONS         
         CLI   5(R2),0                                                          
         BNE   VO20                                                             
         MVI   OPTYPE,C' '         BLANK FOR NO OPTION                          
         B     VOX                                                              
VO20     CLI   8(R2),C'?'          ? FOR HELP                                   
         BNE   VO21                                                             
         MVI   HELPNUM,5                                                        
         ST    R2,QHDR                                                          
         MVI   5(R2),0                                                          
         B     VO1                                                              
VO21     TM    4(R2),X'08'         TEST FOR NUMERIC INPUT                       
         BZ    VO30                                                             
         MVI   OPTYPE,C'N'         N FOR NUMERIC SCROLL AMOUNT                  
         LA    R0,SREISA           INVALID SCROLL AMOUNT                        
         CLI   5(R2),8                                                          
         BH    VOXX                                                             
*                                                                               
         ZIC   R1,5(R2)            EXTRACT NUMBER FROM FIELD                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         ST    R1,OPVAL            OPVAL HOLDS SCROLL AMOUNT                    
         B     VOX                                                              
VO30     CLI   5(R2),1                                                          
         BNE   VO40                                                             
         CLC   DQUOPT(1),SR@MAX    M FOR MAXIMUM SCROLL AMOUNT                  
         BNE   VO40                                                             
         MVI   OPTYPE,C'N'         SET TO INFINITY                              
         MVC   OPVAL,=F'9999'                                                   
         B     VOX                                                              
*                                                                               
VO40     CLC   =C'NF',8(R2)        OVERRIDE SCROLL PROFILE?                     
         BNE   VO50                                                             
         OI    SCRFLAG,SCROVR                                                   
         B     VOX                                                              
*                                                                               
VO50     CLC   =C'UF',8(R2)        USE SCROLL PROFILE?                          
         BNE   VO60                                                             
         NI    SCRFLAG,X'FF'-SCROVR                                             
         B     VOX                                                              
*                                                                               
VO60     GOTO1 CSCANNER,DMCB,(R2),(1,SCANOUT)  USE USER-RQSTD PROFILE?          
         ZIC   R1,SCANOUT                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SCANOUT+12(0),=C'PROFILE'                                        
         BNE   VO70                                                             
         OI    SCRFLAG,USERPROF                                                 
         BAS   RE,RDPROF                                                        
         B     VOX                                                              
*                                                                               
VO70     LA    R0,SREOPT           INVALID OPTION                               
         B     VOXX                                                             
VOX      SR    R0,R0                                                            
VOXX     LTR   R0,R0               EXIT WITH CC NEQ IF ERROR                    
         XIT1  REGS=(R0)           EXIT WITH R0=ERROR NUMBER                    
         SPACE 1                                                                
VALFIND  NTR1                                                                   
         LA    R2,DQUOPTH                                                       
         OC    DQUOPT(48),SPACES   BLANK OUT NULLS                              
         CLI   DQUOPT+1,C' '                                                    
         BNE   VFERR7                                                           
         CLI   5(R2),2                                                          
         BNH   VFERR8              MISSING FIND STRING                          
         MVC   OPTYPE(1),SR@FIND   F FOR FIND STRING                            
         MVI   FSCOL,X'00'         ZERO MEANS NO SPECIFIC COLUMN NUMBER         
         LA    R3,DQUOPT+2                                                      
VF10     CLI   0(R3),C' '          FIND BEGINNING OF SEARCH ARGUMENT            
         BNE   VF20                                                             
         LA    R3,1(R3)                                                         
         B     VF10                                                             
VF20     ZIC   R4,5(R2)                                                         
         LA    R4,8(R2,R4)         R4 POINTS PAST LAST CHAR IN FIELD            
         LR    R5,R4                                                            
         SR    R5,R3               R5 HOLDS LENGTH OF SEARCH ARGUMENT           
         BCTR  R5,0                SUBTRACT ONE FOR EXECUTED MOVE               
         BCTR  R4,0                R4 POINTS TO LAST CHAR IN FIELD              
         CLI   0(R3),X'7D'         X'7D' IS EBCDIC FOR SINGLE QUOTE             
         BNE   VF30                                                             
         CLI   0(R4),X'7D'         ARGUMENT MUST END WITH SINGLE QUOTE          
         BNE   VFERR9                                                           
         LA    R3,1(R3)            BUMP R3 PAST STARTING QUOTE                  
         BCTR  R5,0                                                             
         BCTR  R5,0                SUBTRACT QUOTES FROM MOVE                    
VF30     EX    R5,*+8              MOVE ARGUMENT TO FSTRING                     
         B     *+10                                                             
         MVC   FSTRING(0),0(R3)                                                 
         LA    R5,1(R5)                                                         
         ST    R5,FLEN             FLEN HOLDS LENGTH OF ARGUMENT                
         B     VFX                                                              
VFERR7   LA    R0,SREOPT           INVALID OPTION                               
         B     VFXX                                                             
VFERR8   LA    R0,SREMFS           MISSING FIND STRING                          
         B     VFXX                                                             
VFERR9   LA    R0,SREMEQ           MISSING END QUOTE                            
         B     VFXX                                                             
VFX      SR    R0,R0                                                            
VFXX     LTR   R0,R0               EXIT WITH CC NEQ IF ERROR                    
         XIT1  REGS=(R0)           EXIT WITH R0=ERROR NUMBER                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*                 READ IN SAVED STORAGE                     *                   
*************************************************************                   
READSTR  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,SRPAGENO         READ IN TWA11                                
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 CDATAMGR,DMCB,(X'80',=C'DMREAD'),=C'TEMPSTR',(R2),SRSD           
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEDSTR(SAVEDL),SR$DQU                                          
READX    XIT1                                                                   
         LTORG                                                                  
         SPACE 1                                                                
*************************************************************                   
*               WRITE OUT SAVED STORAGE                     *                   
*************************************************************                   
WRITESTR NTR1  BASE=*,LABEL=*                                                   
         MVC   SR$DQU(SAVEDL),NEWSTR                                            
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TNUM                                                        
         GOTO1 CDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',(R2),SRSD                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
*          SRDQUPROF   (DQUPROF & WRITBLE)                  *                   
*************************************************************                   
       ++INCLUDE SRDQUPROF                                                      
         EJECT                                                                  
*************************************************************                   
*                  MORE CONSTANTS                           *                   
*************************************************************                   
VALOCHRS DC    XL16'404E40404E40404E40404E40404E4040'  00-0F **TEMP**           
         DC    XL16'4E40404E40404E40404E40406040407A'  10-1F **TEMP**           
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'404040404040404040404A4B4C4D4E4F'  40-4F                    
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'4081828384858687888940404040404E'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404E4E404040'  A0-AF                    
         DC    XL16'40404040404040404040404E4E404060'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404E4E404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-D1                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404E4E404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F97A4040404040'  F0-FF                    
*                                                                               
* 4A=A UMLAUT=  5A=U UMLAUT=!  6A=o UMLAUT=º  A1=SS SIGN=~                     
* C0=a UMLAUT={  D0=u UMLAUT=}  E0=O UMLAUT=\  4F=EXCLAIM MARK=|                
         SPACE 1                                                                
HELPSCR  DC    CL79'     PF1  -  Help screen                    OPTIONSx        
                 -  F (find command)'                                           
         DC    CL79'     PF2  -  Scroll up a page                      x        
                    n (scroll value)'                                           
         DC    CL79'     PF3  -  Scroll down a page'                            
         DC    CL79'     PF5  -  Scroll up lines'                               
         DC    CL79'     PF6  -  Scroll down lines'                             
         DC    CL79'     PF8  -  Scroll left'                                   
         DC    CL79'     PF9  -  Scroll right'                                  
         DC    CL79'     PF10 -  Repeat find for same string'                   
HELPSCRL EQU   (*-HELPSCR)                                                      
         EJECT                                                                  
*************************************************************                   
*                       WORK AREA                           *                   
*************************************************************                   
DQUWRKD  DSECT                                                                  
DMCB     DS    6F                                                               
GTB      DS    6F                                                               
ERRNUM   DS    H                                                                
PUBLICID DS    AL2                                                              
WORK     DS    CL24                                                             
DUB      DS    D                                                                
FULL     DS    F                                                                
SAVEDRD  DS    F                   VALUE OF RD AFTER FIRST NMOD1                
RELO     DS    F                                                                
REPPSW   DS    CL6                                                              
MSG      DS    CL60                MESSAGE TO GO INTO MESSAGE FIELD             
* DO NOT SEPERATE SYSTEM AND PROGRAM!                                           
SYSTEM   DS    C                   SYSTEM WHERE REPORT CAME FROM                
PROGRAM  DS    CL2                 PROGRAM WHERE REPORT CAME FROM               
ATTRIBS  DS    X                   ATTRIBUTES                                   
AVALCHRS DS    A                   A(VALOCHRS)                                  
AHELPSCR DS    A                   A(HELPSCR)                                   
ADQUPROF DS    A                   A(DQUPROF)                                   
AWRITBLE DS    A                   A(WRITBLE)                                   
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
*                                                                               
QHDR     DS    A                                                                
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
         DS    XL6                                                              
*                                                                               
*HSAVE    DS    XL32                                                            
*                                                                               
SRPARAS  DS    0F                  SERVICE REQUEST PARAMETER LIST               
SRPASYS  DS    A                   A(SYSFACS)                                   
SRPATIA  DS    A                   A(TIA)                                       
SRPAUTL  DS    A                   A(UTL ENTRY)                                 
SRPACOM  DS    A                   A(COMFACS)                                   
SRPASEL  DS    A                   A(SELIST ENTRY)                              
SRPATWA  DS    A                   A(TWA)                                       
SRPAMAP  DS    A                   A(PHASE MAP)                                 
SRPATIOB DS    A                   A(TIOB)                                      
SRPARAL  EQU   *-SRPARAS                                                        
*                                                                               
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
         DS    XL1                 N/D                                          
DDS      DS    XL1                 FLAG FOR TERMINAL TYPE                       
*                                                                               
SAVEDSTR DS    0F                  SAVED STORAGE VARIABLES                      
OIDENT   DS    CL4                 DQU INDENTIFIER                              
OREPUSER DS    CL2                 OLD USER ID - FOUND IN UTL                   
OREPCUSR DS    CL10                OLD USER ID - EXPANDED                       
OSUBID   DS    CL3                 OLD SUBID                                    
OSEQNO   DS    XL2                 OLD SEQUENCE NUMBER                          
OPRTQID  DS    CL8                 PRTQ FILE ID                                 
OPAGE    DS    F                   OLD PAGE                                     
OLINE    DS    F                   OLD LINE                                     
OCOL     DS    F                   OLD COLUMN                                   
OSCROLL  DS    F                   OLD SCROLL AMOUNT                            
OVARSCRL DS    X                   #-COLS TO SCROLL WHEN USING CHUNKS           
OWASFIND DS    X                   WAS LAST COMMAND A FIND?                     
OFPAGE   DS    F                   PAGE LAST STRING FOUND                       
OFLINE   DS    F                   LINE LAST STRING FOUND                       
OFCOL    DS    F                   COLUMN LAST STRING FOUND                     
OFSTRING DS    CL60                LAST SEARCH STRING                           
OFLEN    DS    F                   LAST STRING LENGTH                           
OFSCOL   DS    CL1                 LAST OPTIONAL SEARCH COLUMN                  
OPROFILE DS    XL16                LAST PROFILE DATA                            
OSCRFLAG DS    X                   VARIOUS SCROLL FLAGS (SEE SCRFLAG)           
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
NEWSTR   DS    0F                  NEW PROGRAM VARIABLES                        
NIDENT   DS    CL4                                                              
REPUSER  DS    CL2                 USER ID - FOUND IN UTL                       
REPCUSR  DS    CL10                USER ID - EXPANDED                           
REPSUBID DS    CL3                 USER SUBID                                   
REPSEQNO DS    XL2                 REPORT SEQUENCE NUMBER                       
PRTQID   DS    CL8                 PRTQ FILE ID                                 
SPAGE    DS    F                   SCREEN PAGE                                  
SLINE    DS    F                   SCREEN LINE                                  
SCOL     DS    F                   SCREEN COLUMN                                
SCROLL   DS    F                   SCROLL AMOUNT                                
VARSCRL  DS    X                   #-COLS TO SCROLL WHEN USING CHUNKS           
WASFIND  DS    X                   TIME WAS A FIND COMMAND                      
FPAGE    DS    F                   PAGE FOUND                                   
FLINE    DS    F                   LINE FOUND                                   
FCOL     DS    F                   COLUMN FOUND                                 
FSTRING  DS    CL60                SEARCH STRING                                
FLEN     DS    F                   STRING LENGTH                                
FSCOL    DS    CL1                 OPTIONAL SEARCH COLUMN                       
PROFILE  DS    XL16                PROFILE DATA                                 
SCRFLAG  DS    X                   VARIOUS SCROLL FLAGS                         
* SCRFLAG VALUES                                                                
PROFND   EQU   X'80'               PROFILE RECORD EXISTS                        
KEEPHDS  EQU   X'40'               KEEP HEADLINES                               
KEEPSDS  EQU   X'20'               KEEP SIDELINES                               
SCROVR   EQU   X'10'               SCROLL OVERRIDE (IGNORE PROFILE)             
SHOWPAGE EQU   X'08'               SHOW ENTIRE PAGE WHEN LINE=1                 
USERPROF EQU   X'04'               USE USER-RQSTD PROFILE                       
*UNUSED* EQU   X'02'                                                            
*UNUSED* EQU   X'01'                                                            
*                                                                               
*                                                                               
CTIOAREA DS    CL2100              FOR USER ID VALIDATION                       
*                                                                               
OPTYPE   DS    CL1                 OPTION TYPE                                  
OPVAL    DS    F                   OPTION VALUE - FOR NUMERIC OPTION            
*                                                                               
PFKEY    DS    XL1                 VALUE OF LAST PFKEY PRESSED                  
PFHELP   EQU   1                   HELP                                         
PFFIND   EQU   10                  REPEAT FIND                                  
PFUPP    EQU   2                   UP PAGES                                     
PFDOWNP  EQU   3                   DOWN PAGES                                   
PFPQR    EQU   4                   PRINT QUEUE RETURN                           
PFUPL    EQU   5                   UP LINES                                     
PFDOWNL  EQU   6                   DOWN LINES                                   
PFLEFTC  EQU   8                   LEFT COLUMNS                                 
PFRIGHTC EQU   9                   RIGHT COLUMNS                                
*                                                                               
FINDRES  DS    XL1                 FLAG FOR RESULT OF FIND ATTEMPT              
FOUNDIT  EQU   1                   REQUESTED STRING WAS FOUND                   
FOUNDEOF EQU   2                   BOTTOM OF DATA REACHED                       
TOOFAR   EQU   3                   COULDN'T FIND IN 1000 LINES                  
*                                                                               
SCANOUT  DS    CL256               USED TO VALIDATE SPOOL ID                    
TRTAB    DS    CL256               TABLE FOR TRT INSTRUCTION                    
LASTPAGE DS    F                   NUMBER OF PAGES IN THIS REPORT               
*                                                                               
CHANGED  DS    CL1                 FLAG FOR CHANGE OF PAGE,LINE,OR COL          
NEWREP   DS    XL1                 FLAG FOR NEW REPORT                          
*                                                                               
DATADISP DS    H                   CODES FOR GETEL                              
ELCODE   DS    X                                                                
*                                                                               
QPAGE    DS    F                   PRINT QUE PAGE                               
QLINE    DS    F                   PRINT QUE LINE                               
EOPFLAG  DS    XL1                 END OF PAGE ENCOUNTERED ON PRINT QUE         
PAGEWAIT DS    XL1                 SKIP TO CHANNEL 1 ENCOUNTERED                
*                                                                               
BPAGE    DS    F                   USED AS ARGUMENTS TO FIRSTLIN                
BLINE    DS    F                                                                
*                                                                               
KEY      DS    CL40                REPORT KEY                                   
PLINE    DS    CL250               REPORT LINE                                  
CIREC    DS    CL14336             CONTROL INTERVAL RECORD                      
*                                                                               
DQUWRKX  EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*                     PROFILE DSECT                         *                   
*************************************************************                   
PROFD    DSECT                                                                  
PROFHDST DS    X                   HEADLINE START LINE                          
PROFNHDS DS    X                   NUMBER OF HEADLINES                          
PROFSTCL DS    X                   FIXED CHUNK START COL                        
PROFEDCL DS    X                   FIXED CHUNK END COL                          
PROFFLT1 DS    X                   FLOATING 1                                   
PROFFLT2 DS    X                   FLOATING 2                                   
PROFFLT3 DS    X                   FLOATING 3                                   
PROFFLT4 DS    X                   FLOATING 4                                   
PROFPAGE DS    X                   OPTION TO SHOW PAGE FROM TOP                 
PROFIGNP DS    X                   IGNORE PROFILE FOR PAGES 0 == > N            
         DS    XL6                 SPARE                                        
*                                                                               
PROFX    EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*                     WRITER DSECT                          *                   
*************************************************************                   
WRITBLED DSECT                                                                  
WRSYS    DS    C                                                                
WRPGM    DS    CL2                                                              
WRKEYWRD DS    CL8                                                              
WRPAGE   DS    XL4                                                              
WRLINE   DS    XL4                                                              
WRKWCOL  DS    X                                                                
WRNAMCOL DS    X                                                                
*                                                                               
WRITDX   EQU   *                                                                
         EJECT                                                                  
*************************************************************                   
*                     INCLUDE FILES                         *                   
*************************************************************                   
* SRERREQUS                                                                     
       ++INCLUDE SRERREQUS                                                      
         EJECT                                                                  
* FAGETTXTD                                                                     
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
* SRDQUFFD                                                                      
       ++INCLUDE SRDQUFFD                                                       
         EJECT                                                                  
* DMPRTQK                                                                       
       ++INCLUDE DMPRTQK                                                        
         EJECT                                                                  
* DMPRTQL                                                                       
       ++INCLUDE DMPRTQL                                                        
         EJECT                                                                  
* FADSECTS                                                                      
       ++INCLUDE FADSECTS                                                       
         EJECT                                                                  
* DDCOMFACS                                                                     
       ++INCLUDE DDCOMFACS                                                      
* DDGLOBEQUS                                                                    
       ++INCLUDE DDGLOBEQUS                                                     
* SRDDEQUS                                                                      
       ++INCLUDE SRDDEQUS                                                       
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SRDQU00S  05/01/02'                                      
         END                                                                    
