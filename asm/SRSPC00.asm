*          DATA SET SRSPC00    AT LEVEL 038 AS OF 02/05/09                      
*PHASE T13E00A,+0                                                               
         TITLE '$QUIESCE - DISPLAY CHANGE SYSTEM FILE STATUS'                   
         PRINT NOGEN                                                            
QUI      CSECT                                                                  
         NMOD1 WRKX-WRKD,*$QUI**,R9,RA,CLEAR=YES,RR=R4                          
         USING WRKD,RC                                                          
         ST    RD,BASERD                                                        
         ST    R4,RELO                                                          
         USING SRPARMD,R1          R1=A(S/R PARAM LIST)                         
         L     R8,SRQASYSF                                                      
         USING SYSFACD,R8          R8=A(SYS FAC LIST)                           
         L     R3,SRQATWA                                                       
         USING SRQUIFFD,R3         R3=A(TWA)                                    
         L     RF,SRQATIA                                                       
         ST    RF,ASAVE            USE TIA AS SAVE AREA                         
*                                                                               
         L     RF,SRQAUTL          EXTRACT UTL DATA                             
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BZ    INIT010                                                          
         CLI   SRVP1H+5,0                                                       
         BNE   INIT010                                                          
         CLI   SRVP2H+5,0                                                       
         BNE   INIT010                                                          
         MVC   SRVP1(3),=C'ALL'                                                 
         MVC   SRVP2(6),=C'ACTIVE'                                              
         MVI   SRVP1H+5,3                                                       
         MVI   SRVP2H+5,6                                                       
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
         MVC   VGETFACT,CGETFACT                                                
         DROP  R1,RF                                                            
*                                                                               
         L     RF,VSSB             EXTRACT SSB DATA                             
         MVC   RECLEN,SSBTWAL-SSBD(RF)                                          
         MVC   FACID(4),SSBSYSN4-SSBD(RF)                                       
         MVC   FACNA(3),SSBSYSNA-SSBD(RF)                                       
         MVC   AFID(4),SSBAFID-SSBD(RF)                                         
         MVC   OPMSG,SPACES                                                     
         MVC   OPFACID,=C'+FACPAK+'                                             
         MVC   OPFACID+4(3),FACNA                                               
*                                                                               
         TIME  BIN                                                              
         ST    R0,TIMENOW                                                       
         EJECT                                                                  
*************************************************************                   
*        MAIN CONTROL                                       *                   
*************************************************************                   
         SPACE 1                                                                
         BAS   RE,READSTR                                                       
*                                                                               
         SR    R1,R1               TEST FOR UP OR DOWN                          
         IC    R1,SVTOP                                                         
         CLI   PFKEY,7             PF7 UP                                       
         BNE   *+8                                                              
         SH    R1,=H'16'                                                        
         CLI   PFKEY,8             PF8 DOWN                                     
         BNE   *+8                                                              
         LA    R1,16(R1)                                                        
         LTR   R1,R1                                                            
         BNM   *+6                                                              
         SR    R1,R1               XC R1 IF IT GOES MINUS                       
         STC   R1,SVTOP                                                         
*                                                                               
         CLI   PFKEY,9             PF9 SWAP WITH SAVEPARMS                      
         BNE   QUI010                                                           
         BAS   RE,SAVEPARM                                                      
         XC    WORK(80),SWPPARMS                                                
         XC    SWPPARMS(80),WORK                                                
         XC    WORK(80),SWPPARMS                                                
         BAS   RE,LOADPARM                                                      
*                                                                               
QUI010   BAS   RE,PARMVAL          VALIDATE P1 - P4                             
*                                                                               
         CLI   PARMFLG,C'D'                                                     
         BE    QUIDS                                                            
         OC    SELDATA,SELDATA                                                  
         BZ    QUISE                                                            
*                                                                               
QUIFL    BAS   RE,DISPFIL          DISPLAY SELECTED SE FILES                    
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF1                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,DISPFIL          REDISPLAY AS STATUS CHANGED                  
         B     INF0                                                             
*                                                                               
QUISE    BAS   RE,DISPSE           DISPLAY LIST OF SE SYSTEMS                   
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF2                                                             
         BAS   RE,ACTSUB                                                        
         TM    SFLAG,SFLGSLCT      TEST SELECTED SE SYS                         
         BO    QUIFL                                                            
         B     QUISE               REDISPLAY SYSTEMS                            
*                                                                               
QUIDS    BAS   RE,DATADISP         DISPLAY DATASPACE LIST                       
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,DATADISP         REDISPLAY AS STATUS CHANGED                  
         B     INF3                                                             
*                                                                               
EXIT     OC    AHELP,AHELP         USE HELPOUT IF AHELP NOT ZERO                
         BNZ   HELPOUT                                                          
         BAS   RE,WRITESTR         WRITE SAVED STR                              
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         AND SET CURSOR                               
EXIT1    XMOD1 1                                                                
*                                                                               
M24SET   SLL   RE,1                SET AMODE 24                                 
         SRL   RE,1                                                             
         BSM   0,RE                                                             
*                                                                               
M31SET   ICM   RE,8,=X'80'         SET AMODE 31                                 
         BSM   0,RE                                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE PARMS                                     *                   
*************************************************************                   
         SPACE 1                                                                
PARMVAL  NTR1                                                                   
*                                                                               
P1VAL    MVI   PARMFLG,C' '        CLEAR FLAG                                   
         LA    R7,SRVP1H                                                        
         ST    R7,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R7)                                                       
         BZ    P1VALX                                                           
         CLI   8(R7),C'?'                                                       
         BNE   *+8                                                              
         ST    R7,AHELP                                                         
         CLC   8(6,R7),=C'DSPACE'                                               
         BNE   *+12                                                             
         MVI   PARMFLG,C'D'        FLAG DSPACE                                  
         B     P1VALX                                                           
         CLI   5(R7),3                                                          
         BNE   P1V010                                                           
         CLC   8(3,R7),=C'ALL'                                                  
         BE    P1VALX                                                           
P1V010   CLI   5(R7),4                                                          
         BL    ERR4                LESS THAN MIN                                
         CLI   5(R7),7                                                          
         BH    ERR5                GREATER THAN MAX                             
         BCTR  R1,0                                                             
         STC   R1,SELDATA          SAVE EXECUTE LEN                             
         MVC   SELDATA+1(7),8(R7)  SAVE FIELD                                   
P1VALX   EQU   *                                                                
         SPACE 1                                                                
P2VAL    OI    DFLAG,DFLGACT       THIS IS DEFAULT                              
         LA    R7,SRVP2H                                                        
         ST    R7,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R7)                                                       
         BZ    P2VALX                                                           
         CLI   8(R7),C'?'                                                       
         BNE   *+8                                                              
         ST    R7,AHELP                                                         
         BCTR  R1,0                                                             
         EX    R1,P2ACTV           DISP ACTIVE SYSTEMS ONLY                     
         BE    P2VALX                                                           
         EX    R1,P2ACTV1          DISP ACTIVE SYSTEMS ONLY                     
         BE    P2VALX                                                           
         EX    R1,P2INACT          DISP ACTIVE SYSTEMS ONLY                     
         BNE   *+8                                                              
         NI    DFLAG,255-DFLGACT                                                
P2VALX   EQU   *                                                                
         SPACE 1                                                                
P4VAL    LA    R7,SRVP4H           CHECK FOR OVERRIDE                           
         ST    R7,CURSOR                                                        
         SR    R1,R1                                                            
         CLI   5(R7),0                                                          
         BE    P4VALX                                                           
         CLI   5(R7),8                                                          
         BNE   P4VALX                                                           
         CLC   8(8,R7),=C'OVERRIDE'                                             
         BNE   P4VALX                                                           
         OI    DFLAG,DFLGOVR       SET OVERRIDE FLAG                            
P4VALX   EQU   *                                                                
         SPACE 1                                                                
PARMVALX B     XIT1                                                             
         SPACE 1                                                                
P2ACTV   CLC   8(0,R7),=C'ACTIVE'                                               
P2ACTV1  CLC   8(0,R7),=C'ACTV'                                                 
P2INACT  CLC   8(0,R7),=C'INACTIVE'                                             
         EJECT                                                                  
*************************************************************                   
*        VALIDATE SUBACTION FIELDS                          *                   
*************************************************************                   
         SPACE 1                                                                
VALSUB   NTR1                                                                   
         TM    SFLAG,SFLGSEL       ONLY SELECT ONCE                             
         BZ    *+12                                                             
         NI    SFLAG,255-SFLGSEL                                                
         B     XIT1                                                             
         LA    R1,16               16 SUBACTION LINES                           
         LA    R7,SRVSELH                                                       
VALS010  STC   R1,COUNT                                                         
         CLI   5(R7),0                                                          
         BNE   VALS050                                                          
VALS011  LA    R7,SELINEL(R7)                                                   
         SR    R1,R1                                                            
         IC    R1,COUNT                                                         
         BCT   R1,VALS010                                                       
         B     XIT1                                                             
*                                                                               
VALS050  CLI   8(R7),C'?'                                                       
         BNE   *+12                                                             
         ST    R7,AHELP                                                         
         B     VALS011                                                          
         TM    DFLAG,DFLGSAME      SCREEN MUST BE THE SAME                      
         BZ    ERR3                                                             
         SR    R1,R1                                                            
         IC    R1,5(R7)                                                         
         BCTR  R1,0                                                             
         LA    RE,SELTAB                                                        
VALS051  CLC   4(1,RE),SVDISP      TEST LIST TYPE                               
         BNE   VALS052                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),8(R7)                                                    
         BE    VALS060                                                          
VALS052  LA    RE,12(RE)                                                        
         CLI   0(RE),0                                                          
         BNE   VALS051                                                          
         ST    R7,CURSOR                                                        
         B     ERR2                INVALID SUB ACTION                           
*                                                                               
VALS060  OI    SFLAG,SFLGSEL                                                    
         SR    RF,RF                                                            
         IC    RF,COUNT                                                         
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         LA    RF,SVSEL(RF)                                                     
         MVC   0(1,RF),3(RE)       SAVE ACTION NUMBER                           
         B     VALS011                                                          
         EJECT                                                                  
******************************************************                          
*        EXECUTE SUB ACTIONS                         *                          
******************************************************                          
         SPACE 1                                                                
ACTSUB   NTR1                                                                   
         LA    R7,SRVSELH          POINT TO TOP OF SCREEN                       
         USING SELINED,R7                                                       
         LA    R4,16               COUNT 16 TIMES                               
ACT010   STC   R4,COUNT                                                         
         BCTR  R4,0                                                             
         SLL   R4,3                                                             
         LA    R5,SVSEL(R4)        CHECK SVSEL ENTRY                            
         CLI   0(R5),0                                                          
         BE    ACT050              NO SELECT SO GET NEXT                        
*                                                                               
         LA    RE,SELTAB           SCAN SELTAB FOR ACTION                       
ACT020   CLC   0(1,R5),3(RE)                                                    
         BE    ACT030                                                           
         LA    RE,12(RE)           NEXT                                         
         CLI   0(RE),0                                                          
         BNE   ACT020                                                           
         DC    H'0'                NOT FOUND IN TABLE                           
*                                                                               
ACT030   ST    R7,CURSOR           SET CURSOR TO THIS                           
         L     RF,8(RE)                                                         
         A     RF,RELO                                                          
         LR    R1,R5               PARSE SVSEL ENTRY                            
         BASR  RE,RF               EXECUTE SUB ACTION                           
         OI    SACTH+1,X'08'       TURN ON HI INTENSITY                         
*                                                                               
ACT050   LA    R7,SELINEL(R7)      NEXT LINE                                    
         SR    R4,R4                                                            
         IC    R4,COUNT                                                         
         BCT   R4,ACT010           BACK FOR NEXT                                
         LA    R7,SRVSELH                                                       
         ST    R7,CURSOR                                                        
         B     XIT1                                                             
         EJECT                                                                  
******************************************************                          
*        DISPLAY SELIST DATA                         *                          
******************************************************                          
         SPACE 1                                                                
DISPSE   NTR1                                                                   
         MVI   SVDISP,C'S'         SET SYSTEM LIST                              
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
DISPSE1  L     R6,VSELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         OC    SVTOP,SVTOP         TEST FOR SAVED VALUE                         
         BZ    DSE050                                                           
         SR    R1,R1                                                            
         IC    R1,SVTOP                                                         
DSE010   TM    DFLAG,DFLGACT       TEST ACTIVE OVERRIDE                         
         BNO   *+12                                                             
         TM    SEIND,SEIACTV+SEISTRT    IGNORE NEVER ACTIVE                     
         BZ    *+6                                                              
         BCTR  R1,0                LOCATE POSITION IN LIST                      
         LTR   R1,R1                                                            
         BZ    DSE050                                                           
         BXLE  R6,R4,DSE010                                                     
         MVI   SVTOP,0             WRAPPED ROUND                                
         B     DISPSE1                                                          
*                                                                               
DSE050   MVC   SRVHDR0,SEHEADR0                                                 
         MVC   SRVHDR,SEHEADER                                                  
         MVC   SRVHDR1,SEUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING SELINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    DSE055                                                           
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
DSE055   ST    R7,CURSOR                                                        
         LA    R1,16               SET COUNT TO 16 LINES                        
*                                                                               
DSE060   LTR   R1,R1               TEST AND SAVE COUNT                          
         BZ    DSEX                                                             
         STC   R1,COUNT                                                         
*                                                                               
         NI    SLINEH+1,255-X'08'                                               
         TM    DFLAG,DFLGACT       TEST ACTIVE OVERRIDE                         
         BNO   *+12                                                             
         TM    SEIND,SEIACTV+SEISTRT    IGNORE NEVER ACTIVE                     
         BZ    DSENXT                                                           
*                                                                               
         TM    SEIND,SEIACTV       IF PREV ACTIVE                               
         BZ    *+16                                                             
         TM    SEIND,SEISTRT       BUT NOT STARTED                              
         BNZ   *+8                                                              
         OI    SLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         MVC   SLINE,SEDFAULT      SET DEFAULT DATA                             
         MVC   SNAME,SENAME                                                     
         GOTO1 VHEXOUT,DMCB,SESYS,SNUM,1                                        
         TM    SEIND,SEISTRT                                                    
         BZ    *+10                                                             
         MVC   SSTRTD,YES                                                       
         TM    SEIND,SEIRCVP                                                    
         BZ    *+10                                                             
         MVC   SRCVR,YES                                                        
         TM    SEIND,SEIACTV                                                    
         BZ    *+10                                                             
         MVC   SACTV,YES                                                        
         TM    SEIND,SEIRONLY                                                   
         BZ    *+10                                                             
         MVC   SRWSTA,RDONLY                                                    
         TM    SEIND,SEISETRO                                                   
         BZ    *+14                                                             
         OI    SLINEH+1,X'08'      HI INTENSITY                                 
         MVC   SRWSTA,SETRO                                                     
         TM    SEIND,SEIQUIES                                                   
         BZ    *+14                                                             
         OI    SLINEH+1,X'08'      HI INTENSITY                                 
         MVC   SQUIESC,YES                                                      
         TM    SEIND,SEIRESA                                                    
         BZ    *+14                                                             
         OI    SLINEH+1,X'08'      HI INTENSITY                                 
         MVC   SRSTRCT,YES                                                      
         TM    SEIND,SEINOP                                                     
         BZ    *+10                                                             
         MVC   SNOP,YES                                                         
         EDIT  (B2,SEQLEN),(4,SQLEN),ZERO=NOBLANK,ALIGN=LEFT                    
         EDIT  (B1,SETASKMX),(4,SMAXTSK),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT  (B1,SETASK),(4,SACTTSK),ZERO=NOBLANK,ALIGN=LEFT                  
         L     R1,SEFILES                                                       
         L     R1,0(R1)                                                         
         EDIT  (B2,2(R1)),(10,SRFILES),ZERO=NOBLANK,ALIGN=LEFT                  
*                                                                               
         LA    R7,SELINEL(R7)      NEXT SCREEN LINE                             
         SR    RF,RF                                                            
         IC    RF,COUNT                                                         
         BCTR  RF,0                                                             
         LR    R1,RF               SAVE COUNT IN R1                             
*                                                                               
         SLL   RF,3                SAVE DATA FOR SELECT                         
         LA    RF,SVSEL(RF)                                                     
         MVI   0(RF),0             SPACE FOR SUB ACTION                         
         MVC   1(7,RF),SENAME                                                   
*                                                                               
DSENXT   BXLE  R6,R4,DSE060                                                     
*                                                                               
DSEX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET DATA ON SCREEN                           
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY FILE DTFS                                  *                   
*************************************************************                   
         SPACE 1                                                                
DISPFIL  NTR1                                                                   
         MVI   SVDISP,C'F'         SET FILE LIST                                
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
         BAS   RE,FINDSYS                                                       
         BE    DFIL020                                                          
         LA    R1,SRVP1H           ELSE ERROR                                   
         ST    R1,CURSOR                                                        
         B     ERR6                                                             
*                                                                               
DFIL020  L     R6,ASENTRY                                                       
         MVC   AFILES,SEFILES                                                   
         MVC   AFILEX,SEFILEX                                                   
         MVC   SRVHDR0,FLHEADR0                                                 
         MVC   SRVHDR,FLHEADER                                                  
         MVC   SRVHDR1,FLUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING FILINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    DFIL025                                                          
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
         MVI   SVTOP,0             AND ZERO SCREEN POSITION                     
                                                                                
         USING SYSFLSTD,R4                                                      
DFIL025  ST    R7,CURSOR                                                        
         L     R4,AFILES           A(Entry point to DTFs)                       
         L     R4,0(,R4)           A(SYSFLST)                                   
         MVC   HALF,SYSF#FLS       Save number of files in HALF                 
         LA    R4,SYSFLIST         A(List of files)                             
*                                                                               
         LH    RF,HALF             FIND POS IN FILES                            
         SR    R1,R1                                                            
         ICM   R1,1,SVTOP                                                       
         BZ    DFIL030                                                          
         LA    R4,SYSFLNQ(,R4)     BUMP FILE                                    
         BCTR  RF,0                DEC N'FILES                                  
         BCT   R1,*-6              DEC SVTOP                                    
         STH   RF,HALF                                                          
         LTR   RF,RF                                                            
         BH    DFIL030                                                          
         MVI   SVTOP,0             WRAP ROUND                                   
         B     DFIL025                                                          
*                                                                               
DFIL030  LA    R1,16               SET COUNT TO 16 LINES                        
DFIL031  LTR   R1,R1               TEST & SAVE COUNT                            
         BZ    DFIX                                                             
         STC   R1,COUNT                                                         
*                                                                               
         USING ISDTF,R6                                                         
         SR    R6,R6                                                            
         ICM   R6,7,SYSFADTF       A(DTF)                                       
         MVC   FLINE,FLDFAULT      SET DEFAULT DATA                             
         NI    FLINEH+1,255-X'08'                                               
*                                                                               
         MVC   FNAME,ISFFID                                                     
         LA    RF,3(R4)                                                         
         GOTO1 VHEXOUT,DMCB,(RF),FXNUM,1                                        
*                                                                               
         TM    ISFOPEN,ISFOQUIE+ISFOOPN                                         
         BZ    DFIL040             BOTH OFF CLOSED                              
         MVC   FOPEN(8),QUIESCED                                                
         BO    DFIL040             BOTH ON QOPEN                                
         MVC   FOPEN(8),QCLOSED                                                 
         TM    ISFOPEN,ISFOOPN     IF OPEN BIT NOT ON QCLOSED                   
         BZ    DFIL040                                                          
         MVC   FOPEN(8),OPEN       ELSE MUST BE OPEN                            
*                                                                               
DFIL040  TM    ISFOPEN,ISFOQUIE                                                 
         BZ    *+8                                                              
         OI    FLINEH+1,X'08'      HI INTENSITY                                 
         TM    ISFOPEN,ISFONOP                                                  
         BZ    *+10                                                             
         MVC   FNOP,YES                                                         
         TM    ISFOPEN,ISFODYN                                                  
         BZ    *+10                                                             
         MVC   FDYNAM,YES                                                       
         TM    ISFOPEN,ISFORO                                                   
         BZ    *+10                                                             
         MVC   FRWSTA,RDONLY                                                    
         TM    ISFOPEN,ISFOQUIE                                                 
         BZ    *+10                                                             
         MVC   FRWSTA,QUIRO                                                     
         TM    ISFOPEN,ISFOCPU                                                  
         BZ    *+10                                                             
         MVC   FXCPU,YES                                                        
         TM    ISFOPEN,ISFOPROP                                                 
         BZ    *+10                                                             
         MVC   FPREV,YES                                                        
         TM    ISFOPEN,ISFOENQ                                                  
         BZ    *+10                                                             
         MVC   FQDQ,YES                                                         
         DROP  R6                  ISDTF                                        
                                                                                
         TM    SYSFIND1,SFRCV                                                   
         BZ    *+10                                                             
         MVC   FRCV,YES                                                         
         TM    SYSFIND1,SFREQ                                                   
         BZ    *+10                                                             
         MVC   FREQ,YES                                                         
         TM    SYSFIND1,SFPRTQ                                                  
         BZ    *+10                                                             
         MVC   FPRTQ,YES                                                        
         TM    SYSFIND1,SFHDR                                                   
         BZ    *+10                                                             
         MVC   FHDR,YES                                                         
         TM    SYSFIND1,SFISF                                                   
         BZ    *+10                                                             
         MVC   FISF,YES                                                         
*                                                                               
         LA    R7,FILINEL(R7)      NEXT LINE                                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,COUNT                                                         
         BCTR  RF,0                                                             
         LR    R1,RF                                                            
         SLL   RF,3                SAVE DATA FOR SELECT                         
         LA    RF,SVSEL(RF)                                                     
         MVI   0(RF),0             SPACE FOR SUB ACTION                         
         STCM  R6,15,4(RF)         SAVE A(DTF)                                  
         MVC   1(1,RF),SYSFIND1    SAVE FLAGS                                   
*                                                                               
         LA    R4,SYSFLNQ(,R4)     NEXT FILE                                    
         LH    RF,HALF             DEC FILE COUNT                               
         BCT   RF,*+8                                                           
         B     DFIX                                                             
         STH   RF,HALF                                                          
         B     DFIL031                                                          
         DROP  R4                  SYSFLSTD                                     
*                                                                               
DFIX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY DATASPACE HEADERS                          *                   
*************************************************************                   
         SPACE 1                                                                
DATADISP NTR1                                                                   
         L     RF,VSSB                                                          
         BAS   RE,M31SET                                                        
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,SSBALET-SSBD(RF)                                         
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
*                                                                               
         MVI   SVDISP,C'D'         SET DATASPACE LIST                           
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
DISPDS1  OC    SVTOP,SVTOP         TEST FOR SAVED VALUE                         
         BZ    DDS050                                                           
         SR    R1,R1               INDEX INTO HEADERS                           
         IC    R1,SVTOP                                                         
         MH    R1,=Y(L'DSPHDR)                                                  
         AR    R2,R1                                                            
*                                                                               
DDS050   MVC   SRVHDR0,DSHEADR0                                                 
         MVC   SRVHDR,DSHEADER                                                  
         MVC   SRVHDR1,DSUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING DSLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    DDS055                                                           
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
DDS055   ST    R7,CURSOR                                                        
         LA    R1,16               SET COUNT TO 16 LINES                        
*                                                                               
DDS060   LTR   R1,R1               TEST AND SAVE COUNT                          
         BZ    DDSX                                                             
         STC   R1,COUNT                                                         
*                                                                               
         NI    DLINEH+1,255-X'08'  NORMAL OR                                    
*                                                                               
*        OI    DLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         MVC   DLINE,DSDFAULT      SET DEFAULT DATA                             
*                                                                               
         MVC   DRSRC,DSPNAME                                                    
         LR    R1,R2                                                            
         S     R1,ADSPACE                                                       
         SRL   R1,6                DIV BY 64                                    
         ST    R1,FULL                                                          
         SAC   0                                                                
         GOTO1 VHEXOUT,DMCB,FULL+3,DRNUM,1                                      
         SAC   512                                                              
         MVC   DRTYPE+1(1),DSPTYPE                                              
*                                                                               
         OC    DSPLOCK,DSPLOCK     ANY LOCK WORD                                
         BZ    DDS090                                                           
*                                                                               
         MVC   FULL,DSPLOCK+2                                                   
         SAC   0                                                                
         GOTO1 VHEXOUT,DMCB,FULL,DLWORD+4,2                                     
         SAC   512                                                              
         CLI   DSPLOCK+1,C' '                                                   
         BE    *+20                                                             
         MVC   DLWORD(4),=C'S../'                                               
         MVC   DLWORD+1(2),DSPLOCK                                              
         B     *+16                                                             
         MVC   DLWORD(4),=C'SY./'                                               
         MVC   DLWORD+2(1),DSPLOCK                                              
*                                                                               
DDS070   MVC   DLNAME,DSPMVS       OWNER NAME                                   
*                                                                               
         MVI   DLFLG,C' '          FLAGS                                        
         TM    DSPFLAG,DSPLONG                                                  
         BZ    *+8                                                              
         MVI   DLFLG,C'L'                                                       
         TM    DSPFLAG,DSPIOW                                                   
         BZ    *+8                                                              
         MVI   DLFLG,C'I'                                                       
*                                                                               
DDS080   TM    DSPJOB,X'80'        TEST OFFLINE JOB                             
         BO    DDS085                                                           
         L     R1,AFID                                                          
DDS081   CLC   DSPJOB(1),4(R1)     FIND ADV SYSTEM                              
         BE    DDS082                                                           
         LA    R1,8(R1)                                                         
         CLI   5(R1),X'FF'         CHECK EOT                                    
         BNE   DDS081                                                           
         DC    H'0'                                                             
DDS082   MVC   DLJOB(4),0(R1)                                                   
         MVC   DLJOB+4(2),=C'/#'                                                
         MVC   DLJOB+6(1),DSPJOB+1                                              
         B     DDS090                                                           
*                                                                               
DDS085   MVI   DLJOB,C'J'          JOB NO                                       
         SR    R1,R1                                                            
         ICM   R1,3,DSPJOB                                                      
         N     R1,=X'00007FFF'                                                  
         EDIT  (R1),(6,DLJOB+1),FILL=0                                          
*                                                                               
DDS090   EDIT  (B4,DSPLCNT),(5,DLLOCKS)                                         
         EDIT  (B4,DSPWCNT),(5,DLWAITS)                                         
         MVC   FULL,DSPTIME                                                     
         BAS   RE,TIMEOUT                                                       
         MVC   DLTIME(11),WORK1   00:00:00.00.....                              
*                                                                               
         SR    R0,R0               SHOW DELTA T                                 
         L     R1,TIMENOW                                                       
         S     R1,DSPTIME                                                       
         C     R1,=F'100'          LESS THAN 1 SEC                              
         BL    DDS091                                                           
         D     R0,=F'100'          CONVERT TO SECS                              
         SR    R0,R0                                                            
*                                                                               
         C     R1,=F'60'           LESS THAN 1 MIN                              
         BL    DDS092                                                           
         D     R0,=F'60'           CONVERT TO MINS                              
         SR    R0,R0                                                            
         CH    R1,=H'99'           LESS THAN 99 MINS                            
         BL    DDS093                                                           
         MVC   DLTIME+12(4),=C'+>>>'                                            
         B     DDS100                                                           
*                                                                               
DDS091   MVC   DLTIME+12(4),=C'+00.'                                            
         B     DDS095                                                           
DDS092   MVC   DLTIME+12(4),=C'+00s'                                            
         B     DDS095                                                           
DDS093   MVC   DLTIME+12(4),=C'+00m'                                            
*                                                                               
DDS095   EDIT  (R1),(2,DLTIME+13)                                               
*                                                                               
DDS100   LA    R7,DLLINEL(R7)      NEXT SCREEN LINE                             
         SR    RF,RF                                                            
         IC    RF,COUNT                                                         
         BCTR  RF,0                                                             
         LR    R1,RF               SAVE COUNT IN R1                             
*                                                                               
         SLL   RF,3                SAVE DATA FOR SELECT                         
         LA    RF,SVSEL(RF)                                                     
         MVI   0(RF),0             SPACE FOR SUB ACTION                         
         LR    RE,R2                                                            
         S     RE,ADSPACE                                                       
         SRL   RE,6                DIV BY 64                                    
         STCM  RE,15,1(RF)                                                      
*                                                                               
DDSNXT   LA    R2,64(R2)                                                        
         B     DDS060                                                           
*                                                                               
DDSX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         BAS   RE,M24SET                                                        
         SAC   0                                                                
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
         CLC   SRCOMWRK(4),QUID    TEST FOR MY ID                               
         BNE   READX                                                            
         LA    R1,SRCOMWRK                                                      
         MVC   SAVEDSTR(SAVEDL),4(R1)                                           
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
         MVC   0(4,R1),QUID                                                     
         MVC   4(SAVEDL,R1),SAVEDSTR                                            
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT1                                                             
         DROP  R5                                                               
         SPACE 2                                                                
*************************************************************                   
*        TWAXC                                              *                   
*************************************************************                   
         SPACE 1                                                                
TWAXC    NTR1                                                                   
         TWAXC (R1),SRVXLINH,PROT=Y                                             
XIT1     XIT1                                                                   
         EJECT                                                                  
***************************************************                             
*        EDIT TUS IN FULL TO WORK HH:MM:SS.SS     *                             
***************************************************                             
         SPACE 1                                                                
TIMEOUT  ST    RE,SAVERE                                                        
         MVC   WORK1(11),=C'00:00:00.00'                                        
         SR    RE,RE                                                            
         L     RF,FULL                                                          
         D     RE,=F'360000'                                                    
         EDIT  (RF),(2,WORK1),FILL=0      HRS                                   
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'6000'                                                      
         EDIT  (RF),(2,WORK1+3),FILL=0    MINS                                  
         LR    RF,RE                                                            
         SR    RE,RE                                                            
         D     RE,=F'100'                                                       
         EDIT  (RF),(2,WORK1+6),FILL=0    SECS                                  
         EDIT  (RE),(2,WORK1+9),FILL=0    100/SEC                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        FIND ASENTRY FROM SELDATA                          *                   
*************************************************************                   
         SPACE 1                                                                
FINDSYS  NTR1                                                                   
         L     R6,VSELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         SR    R1,R1                                                            
         IC    R1,SELDATA                                                       
FINDS10  EX    R1,*+8              FIND SELECTED SYS                            
         B     *+10                                                             
         CLC   SENAME(0),SELDATA+1                                              
         BE    FINDSX                                                           
         BXLE  R6,R4,FINDS10                                                    
         LTR   RB,RB               SET CC NEQ (NOT FOUND)                       
         B     XIT1                                                             
FINDSX   ST    R6,ASENTRY          SAVE ASENTRY                                 
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SELECT AN ENTRY                                    *                   
*************************************************************                   
         SPACE 1                                                                
SELECT   TM    SFLAG,SFLGSLCT                                                   
         BOR   RE                  ALREADY GOT A SELECT                         
         ST    RE,SAVERE           SAVE PARAMS                                  
         BAS   RE,SAVEPARM                                                      
         MVC   SWPPARMS,WORK                                                    
         L     RE,SAVERE                                                        
         MVC   SELDATA,0(R1)                                                    
         MVI   SELDATA,6           FIX EXEC LEN TO 7                            
         OI    SFLAG,SFLGSLCT                                                   
         MVC   SRVP1(7),1(R1)      PUT INTO P1 AS WELL                          
         XC    SRVP2,SRVP2         CLEAR P2                                     
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        QUIESCE A FILE     (SET QUIESCE BIT ON FOR R/O)    *                   
*************************************************************                   
         SPACE 1                                                                
QUIES    NTR1                                                                   
         ICM   R6,15,4(R1)         GET A(DTF)                                   
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN     IGNORE IF CLOSED                             
         BZ    QUIX                                                             
*                                                                               
         BAS   RE,SYSWAIT          WAIT FOR SYSTEM                              
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         TM    ISFOPEN,ISFOQUIE    Q IS A TOGGLE                                
         BNZ   QUIES010                                                         
         OI    ISFOPEN,ISFOQUIE                                                 
         MVC   OPCMND,QUIESCED                                                  
         BAS   RE,WTO                                                           
         B     QUIX                                                             
QUIES010 NI    ISFOPEN,255-ISFOQUIE                                             
         MVC   OPCMND,UNQUI                                                     
         BAS   RE,WTO                                                           
*                                                                               
QUIX     BAS   RE,SECHECK                                                       
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        LOCK A DATASPACE RESOURCE                          *                   
*************************************************************                   
         SPACE 1                                                                
DLOCK    NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         MVC   FULL,1(R1)          RESOURCE                                     
         XC    DMCB(12),DMCB                                                    
*                                                                               
         CLI   BYTE,X'10'          NORMAL LOCK                                  
         BNE   *+8                                                              
         MVI   DMCB,X'04'                                                       
         CLI   BYTE,X'11'          I/O WAIT LOCK                                
         BNE   *+8                                                              
         MVI   DMCB,X'44'                                                       
         CLI   BYTE,X'12'          LONG WAIT LOCK                               
         BNE   *+8                                                              
         MVI   DMCB,X'84'                                                       
         CLI   BYTE,X'13'          FREE RESOURCE                                
         BNE   *+8                                                              
         MVI   DMCB,X'10'                                                       
         CLI   BYTE,X'14'          FORCE FREE RESOURCE                          
         BNE   *+8                                                              
         MVI   DMCB,X'11'                                                       
*                                                                               
         MVC   DMCB+2,FULL+2                                                    
         GOTO1 VLOCKSPC,DMCB       LOCK OR FREE RESOURCE                        
         TM    DMCB+4,0                                                         
         BE    XIT1                                                             
         B     ERR16               REPORT ERRORS                                
         EJECT                                                                  
*************************************************************                   
*        OPEN A FILE                                        *                   
*************************************************************                   
         SPACE 1                                                                
QOPEN    NTR1                                                                   
         MVC   BYTE1,0(R1)                                                      
         MVC   BYTE,1(R1)                                                       
         ICM   R6,15,4(R1)         QUIESCE OPEN A FILE                          
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN                                                  
         BNZ   ERR8                FILE ALREADY OPEN DUMMY                      
         TM    ISFOPEN,ISFOQUIE                                                 
         BNZ   *+8                                                              
         BAS   RE,RESTRICT                                                      
         BAS   RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         CLI   BYTE1,X'04'         TEST OPEN OR QOPEN                           
         BNE   *+14                                                             
         OI    ISFOPEN,ISFOQUIE                                                 
         MVC   OPCMND,QOPENED                                                   
         CLI   BYTE1,X'0A'                                                      
         BNE   *+14                                                             
         NI    ISFOPEN,255-ISFOQUIE                                             
         MVC   OPCMND,OPENED                                                    
         BAS   RE,SINGLE           SINGLE THREAD                                
         TM    ISFTYPE,ISFTIS                                                   
         BZ    QO1                                                              
         GOTO1 VDATAMGR,DMCB,ISDDS,8,IOAREA,0,(R6),0,0                          
         B     QOX                                                              
*                                                                               
         DROP  R6                                                               
         USING DTFPHD,R6                                                        
QO1      MVC   FULL,FIRSTDA                                                     
         GOTO1 VDATAMGR,DMCB,DADDS,DAOPEN,IOAREA,0,(R6),FULL,0                  
*                                                                               
         MVC   FULL,FIRSTDA        SET TO 00010100                              
         TM    DTFTYPE,DTFTBIG                                                  
         BZ    *+10                                                             
         MVC   FULL,=X'00004101'   OR 00004101 FOR BIG FILES                    
         TM    BYTE,SFHDR                                                       
         BZ    QO1A                IF NO HEADER GO READ FOR EOF                 
*                                                                               
         GOTO1 (RF),(R1),DADDS,RDID,IOAREA,0,(R6),FULL,0                        
         MVC   FULL,IOAREA+92                                                   
         TM    DTFTYPE,DTFTBIG     RE TEST AFTER HEADER READ                    
         BZ    QO1A                                                             
         SR    R0,R0               ADJUST DA IF BIG FILE                        
         ICM   R0,3,IOAREA+92                                                   
         CLI   IOAREA+94,X'FF'                                                  
         BE    *+8                                                              
         ICM   R0,4,IOAREA+94                                                   
         SLL   R0,32-18                                                         
         ST    R0,FULL                                                          
*                                                                               
QO1A     GOTO1 (RF),(R1),DADDS,ADDADR,IOAREA,0,(R6),FULL,0                      
QOX      BAS   RE,MULTI            OK TO MULTI TASK AGAIN                       
         BAS   RE,WTO                                                           
         BAS   RE,SECHECK                                                       
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLOSE A FILE                                       *                   
*************************************************************                   
         SPACE 1                                                                
QCLOSE   NTR1                                                                   
         MVC   BYTE1,0(R1)                                                      
         CLI   BYTE1,X'0B'                                                      
         BNE   *+8                                                              
         BAS   RE,RESTRICT                                                      
         ICM   R6,15,4(R1)         QUIESCE CLOSE A FILE                         
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN                                                  
         BZ    ERR9                FILE ALREADY CLOSED DUMMY                    
         BAS   RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         CLI   BYTE1,X'03'         TEST FOR QCLOSE                              
         BNE   *+8                                                              
         OI    ISFOPEN,ISFOQUIE    LET THE WORLD KNOW I DID IT                  
         CLI   BYTE1,X'0B'                                                      
         BNE   *+8                                                              
         NI    ISFOPEN,255-ISFOQUIE                                             
*                                                                               
         TM    ISFTYPE,ISFTIS                                                   
         BZ    QC1                                                              
         GOTO1 VDATAMGR,DMCB,ISDDS,9,IOAREA,0,(R6),0,0                          
         B     QCX                                                              
QC1      GOTO1 VDATAMGR,DMCB,DADDS,DACLOSE,IOAREA,0,(R6),X'00010101',0          
QCX      MVC   OPCMND,CLOSED                                                    
         BAS   RE,WTO                                                           
         BAS   RE,SECHECK                                                       
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        LOOP UNTIL SYSTEM AT ASENTRY IS INACTIVE           *                   
*************************************************************                   
         SPACE 1                                                                
SYSWAIT  NTR1                                                                   
         TM    DFLAG,DFLGOVR       DON'T WAIT IF OVERRIDE ENTERED               
         BO    SWAIT4                                                           
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         LA    R3,60               I/O LOOP COUNT                               
*                                                                               
SWAIT1   L     R6,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING TCBD,R6                                                          
*                                                                               
         CLC   TCBSYS,SESYS                                                     
         BE    *+12                                                             
         BXLE  R6,R4,*-10                                                       
         B     SWAIT4                                                           
*                                  IF IT IS DO AN I/O AND WAIT                  
         GOTO1 VGETFACT,DMCB,(X'80',=F'38400'),F#WAIT                           
         BCT   R3,SWAIT1                                                        
         B     ERR7                                                             
*                                                                               
SWAIT4   B     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
*************************************************************                   
*        CHECK SELIST ENTRYS QUIESCE BIT IS CORRECT         *                   
*************************************************************                   
         SPACE 1                                                                
SECHECK  NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   AFILES,SEFILES                                                   
         MVC   AFILEX,SEFILEX                                                   
         NI    SEIND,255-SEIQUIES                                               
                                                                                
         USING SYSFLSTD,R4                                                      
         L     R4,AFILES           A(Entry point to DTFs)                       
         L     R4,0(,R4)           A(SYSFLIST)                                  
         MVC   HALF,SYSF#FLS       Save number of files in HALF                 
         LA    R4,SYSFLIST         A(List of files)                             
*                                                                               
         USING ISDTF,R1                                                         
SECHK01  SR    R1,R1                                                            
         ICM   R1,7,SYSFADTF       A(DTF)                                       
         TM    ISFOPEN,ISFOQUIE                                                 
         BZ    SECHK02                                                          
         OI    SEIND,SEIQUIES                                                   
         B     XIT1                                                             
         DROP  R1                                                               
*                                                                               
SECHK02  LA    R4,SYSFLNQ(,R4)     Next file                                    
         LH    RF,HALF             DEC FILE COUNT                               
         BCT   RF,*+8                                                           
         B     XIT1                                                             
         STH   RF,HALF                                                          
         B     SECHK01                                                          
         DROP  R4,R6                                                            
         EJECT                                                                  
*************************************************************                   
*        START A SYSTEM                                     *                   
*************************************************************                   
         SPACE 1                                                                
UPSYS    NTR1                                                                   
         MVC   SELDATA,0(R1)                                                    
         MVC   BYTE1,0(R1)                                                      
         MVI   SELDATA,6           FIX EXEC LEN TO 7                            
         BAS   RE,FINDSYS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         BZ    UPOK                NO - EXIT                                    
         TM    SEIND,SEISTRT       MAKE SURE SYSTEM IS DOWN                     
         BZ    UPS0                                                             
         B     ERR14               IF NOT ERROR                                 
*                                                                               
UPS0     BAS   RE,SETSYS           FRIG TCB ENTRY FOR SYSCHA                    
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VFINDSYS,(SESYS,0)                                 
                                                                                
         USING SYSFLSTD,R1                                                      
         L     R1,4(,R1)           R1=A(SYSFILES LIST)                          
         LH    R0,SYSF#FLS         # of files                                   
         LA    R1,SYSFLIST         List of files                                
         XC    AREQUEST,AREQUEST                                                
*                                  CLEAR ISCILAST FOR ALL I/S FILES             
         USING ISDTF,RE                                                         
UPS1     L     RE,SYSFADTF-1       A(DTF)                                       
         TM    SYSFIND1,SFISF      I/S FILE ?                                   
         BZ    UPS1A                                                            
         TM    SYSFIND2,SFALIAS    Non native file?                             
         BO    UPS1A               Yes, so don't clear                          
         XC    ISCILAST,ISCILAST                                                
         DROP  RE                                                               
                                                                                
UPS1A    TM    SYSFIND1,SFREQ      TEST REQUEST FILE                            
         BZ    *+8                                                              
         ST    RE,AREQUEST                                                      
         LA    R1,SYSFLNQ(,R1)                                                  
         BCT   R0,UPS1                                                          
         DROP  R1                                                               
*                                  OPEN SEFILES                                 
         BAS   RE,SINGLE                                                        
         GOTO1 VDMOD000,DMCB,VOPENSYS,(SESYS,IOAREA)                            
         BAS   RE,MULTI                                                         
         OC    DMCB+8(2),DMCB+8                                                 
         BZ    UPS2                                                             
         MVC   DUB(2),DMCB+8       SAVE ERROR BYTES                             
         MVC   DMCB(4),VCLSESYS                                                 
         GOTO1 VDMOD000,DMCB       CLOSE SYSTEM FILES                           
         MVC   DMCB+8(2),DUB                                                    
         BAS   RE,RELSYS           RESET TCB ENTRY                              
         TM    DMCB+9,X'80'        INVALID CPU ID                               
         BO    ERR11                                                            
         TM    DMCB+9,X'40'        MISSING CPU ID                               
         BO    ERR12                                                            
         TM    DMCB+9,X'20'        ACTIVE IN ANOTHER PARTITION                  
         BO    ERR13                                                            
         DC    H'0'                                                             
*                                  LOAD & GO TO RECOVERY RESTORE                
UPS2     CLI   BYTE1,7             RECOVERY=NO  (+RN)                           
         BE    UPS3                NO MUCKING ABOUT                             
         GOTO1 VCALLOV,DMCB,IOAREA,X'D9010100'                                  
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                DIE IF CAN'T LOAD                            
         LA    RF,IOAREA                                                        
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),(R1),,(SESYS,(R8)),0                                        
         CLI   8(R1),0                                                          
         BE    UPS3                                                             
         XC    SRVMSG,SRVMSG       BUILD ERROR MSG                              
         MVC   SRVMSG(12),=C'*RCVR ERROR*'                                      
         L     RE,8(R1)                                                         
         MVC   SRVMSG+13(40),0(RE)                                              
         OI    DFLAG,DFLGMSG       TELL INF & ERR EXITS ITS THERE               
*                                                                               
UPS3     ICM   R1,15,AREQUEST      TEST SYSTEM REQUEST FILE ERASED              
         BZ    UPS4                                                             
         CLC   DNEXT-DTFPHD(L'DNEXT,R1),=X'00010000'                            
         BNE   UPS4                                                             
         SH    R1,=H'4'                                                         
         ICM   R1,15,0(R1)         YES - POINT TO REQUEST ADDRESS LIST          
         BZ    UPS4                                                             
*                                                                               
         LH    RE,0(R1)            SET UP FOR REQUEST LIST BXLE                 
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         XC    2(6,R1),2(R1)       AND CLEAR REQUEST POINTERS                   
         BXLE  R1,RE,*-6                                                        
*                                                                               
UPS4     BAS   RE,RELSYS           RESET TCB ENTRY                              
*                                                                               
UPOK     NI    SEIND,X'FC'                                                      
         OI    SEIND,X'80'         SET SE UP                                    
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STARTED                                                   
         CLI   BYTE1,8                                                          
         BNE   UPX                                                              
         OI    SEIND,SEIQUIES      QUIESCE ALL FILES IF '+Q'                    
         MVC   OPCMND,QOPENED                                                   
         BAS   RE,QONALL                                                        
*                                                                               
UPX      BAS   RE,WTO                                                           
         B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        QUIESCE OR UNQUIESCE A WHOLE SYSTEM                *                   
*************************************************************                   
         SPACE 1                                                                
SQUIES   NTR1                                                                   
         MVC   SELDATA,0(R1)                                                    
         MVI   SELDATA,6           FIX EXEC LEN TO 7                            
         BAS   RE,FINDSYS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    SQUOKX              NO - EXIT                                    
         BAS   RE,SYSWAIT                                                       
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STARTED                                                   
         TM    SEIND,SEIQUIES                                                   
         BZ    SQU010                                                           
         BAS   RE,QOFFALL                                                       
         NI    SEIND,255-SEIQUIES                                               
         MVC   OPCMND,UNQUI                                                     
         B     SQUOK                                                            
*                                                                               
SQU010   BAS   RE,QONALL                                                        
         OI    SEIND,SEIQUIES                                                   
         MVC   OPCMND,QUIESCED                                                  
         B     SQUOK                                                            
*                                                                               
SQUOK    BAS   RE,WTO                                                           
SQUOKX   B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        STOP A SYSTEM                                      *                   
*************************************************************                   
         SPACE 1                                                                
DNSYS    NTR1                                                                   
         MVC   BYTE1,0(R1)                                                      
         MVC   SELDATA,0(R1)                                                    
         MVI   SELDATA,6           FIX EXEC LEN TO 7                            
         BAS   RE,FINDSYS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   OPSYSID(7),0(R6)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STOPPED                                                   
         OC    SEFILES,SEFILES     ANY FILES                                    
         BZ    DNOK                NO - EXIT                                    
         BAS   RE,SYSWAIT                                                       
*                                                                               
         OI    SEIND,SEINOP        SET SE NO-OP                                 
         NI    SEIND,X'65'         TURN-OFF OP,SRO,QUI & STOP                   
*                                                                               
         BAS   RE,QOFFALL                                                       
*                                  CLOSE SE FILES                               
DN4      BAS   RE,SETSYS                                                        
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VCLSESYS,(SESYS,IOAREA)                            
         BAS   RE,RELSYS                                                        
*                                                                               
DNOK     BAS   RE,WTO                                                           
         B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        ADJUST SYSTEM MAX TASKS                            *                   
*************************************************************                   
         SPACE 1                                                                
MXSYS    NTR1                                                                   
         MVC   SELDATA,0(R1)                                                    
         MVC   BYTE1,0(R1)                                                      
         MVI   SELDATA,6           FIX EXEC LEN TO 7                            
         BAS   RE,FINDSYS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         CLI   BYTE1,X'0C'         TEST MX+                                     
         BE    MXSYS1                                                           
         CLI   BYTE1,X'0D'         TEST MX-                                     
         BE    MXSYS2                                                           
         B     MXSYSOK             NO - EXIT                                    
*                                                                               
MXSYS1   SR    R1,R1               BUMP ACTIVE TASKS ALLOWED                    
         IC    R1,SETASKMX                                                      
         LA    R1,1(R1)                                                         
         STC   R1,SETASKMX                                                      
         B     MXSYSOK             NO - EXIT                                    
*                                                                               
MXSYS2   CLI   SETASKMX,1          CHECK MORE THAN 1 CURRENTLY                  
         BNH   MXSYSOK                                                          
         SR    R1,R1               DECREMENT  ACTIVE TASKS ALLOWED              
         IC    R1,SETASKMX                                                      
         BCTR  R1,0                                                             
         STC   R1,SETASKMX                                                      
*                                                                               
MXSYSOK  B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
*************************************************************                   
*        TURN ON/OFF ALL QUIESCE BITS FOR A SYSTEM          *                   
*************************************************************                   
         SPACE 1                                                                
QOFFALL  MVI   BYTE,0                                                           
         B     QALL                                                             
QONALL   MVI   BYTE,1                                                           
QALL     NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R4,SEFILES          A(Entry point of DTFs)                       
                                                                                
         USING SYSFLSTD,R4                                                      
         L     R4,0(R4)            A(Sysflist)                                  
         MVC   HALF,SYSF#FLS       SAVE N'FILES IN HALF                         
         LA    R4,SYSFLIST         A(List of files)                             
*                                                                               
         USING ISDTF,R6                                                         
QALL010  SR    R6,R6                                                            
         ICM   R6,7,SYSFADTF                                                    
         CLI   BYTE,1              TURN OFF ALL QUIESCE BITS                    
         BE    *+8                                                              
         NI    ISFOPEN,255-ISFOQUIE                                             
         CLI   BYTE,0              TURN ON ALL QUIESCE BITS                     
         BE    *+8                                                              
         OI    ISFOPEN,ISFOQUIE                                                 
         LA    R4,SYSFLNQ(,R4)     NEXT FILE                                    
         LH    RF,HALF             DEC FILE COUNT                               
         BCT   RF,*+8                                                           
         B     DFIX                                                             
         STH   RF,HALF                                                          
         B     QALL010                                                          
         DROP  R4,R6                                                            
*                                                                               
QOFFX    B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        SET & RESET TCB ENTRY                              *                   
*************************************************************                   
         SPACE 1                                                                
SETSYS   NTR1  ,                   SET TCB ENTRY FOR SYSCHA                     
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R7,VSSB                                                          
         USING SSBD,R7                                                          
         L     R7,SSBTKADR                                                      
         USING TCBD,R7                                                          
         MVC   SAVETCB(8),TCBDTFS                                               
         MVC   SAVETCB+8(1),TCBSYS                                              
         MVC   TCBDTFS(8),SEFILES                                               
         MVC   TCBSYS,SESYS                                                     
         GOTO1 VDTFIOA,DMCB,(C'I',(R7))                                         
         B     XIT1                                                             
         SPACE 1                                                                
RELSYS   NTR1  ,                   RESET TCB ENTRY                              
         L     R7,VSSB                                                          
         USING SSBD,R7                                                          
         L     R7,SSBTKADR                                                      
         USING TCBD,R7                                                          
         MVC   TCBDTFS(8),SAVETCB                                               
         MVC   TCBSYS,SAVETCB+8                                                 
         GOTO1 VDTFIOA,DMCB,(C'I',(R7))                                         
         B     XIT1                                                             
         DROP  R6                                                               
         SPACE 2                                                                
*************************************************************                   
*        SET & RESET SINGLE THREAD MODE                     *                   
*************************************************************                   
         SPACE 1                                                                
SINGLE   ST    RE,SAVERE                                                        
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),0      DISABLE MULTI-TASKING WAITS             
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MULTI    ST    RE,SAVERE                                                        
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),C'M'   ENABLE MULTI-TASKING WAITS              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
SAVEPARM MVC   WORK+00(1),SRVP1H+5                                              
         MVC   WORK+01(16),SRVP1                                                
         MVC   WORK+17(1),SRVP2H+5                                              
         MVC   WORK+18(16),SRVP2                                                
         MVC   WORK+34(1),SRVP3H+5                                              
         MVC   WORK+35(16),SRVP3                                                
         MVC   WORK+51(1),SRVP4H+5                                              
         MVC   WORK+52(16),SRVP4                                                
         MVC   WORK+69(1),SVTOP                                                 
         BR    RE                                                               
LOADPARM MVC   SRVP1H+5(1),WORK+00                                              
         MVC   SRVP1(16),WORK+01                                                
         MVC   SRVP2H+5(1),WORK+17                                              
         MVC   SRVP2(16),WORK+18                                                
         MVC   SRVP3H+5(1),WORK+34                                              
         MVC   SRVP3(16),WORK+35                                                
         MVC   SRVP4H+5(1),WORK+51                                              
         MVC   SRVP4(16),WORK+52                                                
         MVC   SVTOP,WORK+69                                                    
         BR    RE                                                               
         SPACE 2                                                                
DUMMY    BR    RE                                                               
         SPACE 2                                                                
RESTRICT B     ERR10                                                            
         EJECT                                                                  
ERR0     LA    R0,2                INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR1     LA    R0,1                MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R0,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR3     LA    R0,262              SCREEN HAS CHANGED                           
         B     ERRX                                                             
ERR4     LA    R0,6                LENGTH LESS THAN MIN                         
         B     ERRX                                                             
ERR5     LA    R0,7                GREATER THAN MAX                             
         B     ERRX                                                             
ERR6     LA    R0,32               INVALID SYSTEM                               
         B     ERRX                                                             
ERR7     LA    R0,263              SYSTEM IS BUSY                               
         B     ERRX                                                             
ERR8     LA    R0,264              FILE ALREADY OPEN                            
         B     ERRX                                                             
ERR9     LA    R0,265              FILE ALREADY CLOSED                          
         B     ERRX                                                             
ERR10    LA    R0,2                INVALID INPUT (RESTRICTED)                   
         B     ERRX                                                             
ERR11    LA    R0,266              INVALID CPU ID                               
         B     ERRX                                                             
ERR12    LA    R0,267              MISSING CPU ID                               
         B     ERRX                                                             
ERR13    LA    R0,268              SYSTEM ACTV IN ANOTHER PARTITION             
         B     ERRX                                                             
ERR14    LA    R0,269              SYSTEM IS ALREADY STARTED                    
         B     ERRX                                                             
ERR15    LA    R0,270              SYSTEM IS ALREADY STOPPED                    
         B     ERRX                                                             
ERR16    LA    R0,307              UNABLE TO ALLOCATE                           
         B     ERRX                                                             
         SPACE 2                                                                
ERRX     L     RD,BASERD                                                        
         TM    DFLAG,DFLGMSG       DON'T FUCK WITH MSG FIELD                    
         BO    EXIT                                                             
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),(4,FACID)                           
         B     EXIT                                                             
         SPACE 2                                                                
INF0     LA    R0,300              FILE STATUS CHANGED                          
         B     INFX                                                             
INF1     LA    R0,301              SYSTEM FILES DISPLAYED                       
         B     INFX                                                             
INF2     LA    R0,302              SYSTEM STATUS DISPLAYED                      
         B     INFX                                                             
INF3     LA    R0,308              DATASPACE RESOURCES DISPLAYED                
         B     INFX                                                             
INFX     TM    DFLAG,DFLGMSG       DON'T FUCK WITH MSG FIELD                    
         BO    EXIT                                                             
         XC    DMCB(24),DMCB                                                    
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
*        WRITE TO OPERATOR                           *                          
******************************************************                          
         SPACE 1                                                                
WTO      NTR1                                                                   
         OC    OPMSG,SPACES                                                     
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,DMCB,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                       
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     XIT1                                                             
         EJECT                                                                  
******************************************************                          
*        CONSTANTS                                   *                          
******************************************************                          
         SPACE 1                                                                
DOTS     DC    40C'.'                                                           
SPACES   DC    80C' '                                                           
QUID     DC    C'$QUI'                                                          
YES      DC    CL7'Yes'                                                         
RDONLY   DC    CL7' R/O'                                                        
RDWRT    DC    CL7' R/W'                                                        
SETRO    DC    CL7'SR/O'                                                        
QUIRO    DC    CL7'QR/O'                                                        
QCLOSED  DC    CL8'QClosed'                                                     
OPEN     DC    CL10'Open'                                                       
OPENED   DC    CL10'Opened'                                                     
CLOSED   DC    CL10'Closed'                                                     
QUIESCED DC    CL10'Quiesced'                                                   
UNQUI    DC    CL10'Unquiesced'                                                 
QOPENED  DC    CL10'Qopened'                                                    
STARTED  DC    CL10'Started'                                                    
STOPPED  DC    CL10'Stopped'                                                    
QSTARTED DC    CL10'Qstarted'                                                   
DMREAD   DC    CL7'DMREAD'                                                      
DMWRT    DC    CL7'DMWRT'                                                       
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DADDS    DC    CL7'DADDS'                                                       
ISDDS    DC    CL7'ISDDS'                                                       
FIRSTDA  DC    X'00010101'                                                      
HELPID   DC    XL10'0125FF00010000000000'                                       
*             ***....+....1....+....2....+....3....+....4***                    
SEHEADR0 DC    C'                      Rcvr Prev        Q'                      
         DC    C'ui- Rrtc                               '                       
SEHEADER DC    C'Sel Sename  Num  Strt Pend Actv Access e'                      
         DC    C'sc  acc  Nop  Qlen Act  Max  No Files  '                       
SEUNDER  DC    C'--- ------- ---- ---- ---- ---- ------ -'                      
         DC    C'--- ---- ---- ---- ---- ---- ----------'                       
SEDFAULT DC        C'             No   .... ....  R/W   .'                      
         DC    C'... .... ....                          '                       
*                                                                               
FLHEADR0 DC    C'            Ext                Dyna     '                      
         DC    C'             DDS                       '                       
FLHEADER DC    C'Sel File-Id Num  Status   Nop  DD   Acce'                      
         DC    C'ss Xcpu Open QDQ  Rcv Req PQ  Hdr IS   '                       
FLUNDER  DC    C'--- ------- ---- -------- ---- ---- ----'                      
         DC    C'-- ---- ---- ---- --- --- --- --- ---- '                       
FLDFAULT DC        C'             Closed   ...  ....  R/W'                      
         DC    C'   .... .... .... ... ... ... ... .... '                       
*                                                                               
DSHEADR0 DC    C'    Resource         Lock     Owner     '                      
         DC    C'           Num   Num                   '                       
DSHEADER DC    C'Sel Name     Num Typ Word     Name     J'                      
         DC    C'ob     Flg Locks Waits Lock time       '                       
DSUNDER  DC    C'--- -------- --- --- -------- -------- -'                      
         DC    C'------ --- ----- ----- ----------------'                       
DSDFAULT DC        C'                                    '                      
         DC    C'                                       '                       
*                                                                               
*        DC    C'ACT',X'NN',C'?',XL3'00',AL4(ROUTINE)                           
*                                                                               
SELTAB   DC    CL3'SEL',X'01',C'S',XL3'00',AL4(SELECT)                          
         DC    CL3'-  ',X'05',C'S',XL3'00',AL4(DNSYS)                           
         DC    CL3'DN ',X'05',C'S',XL3'00',AL4(DNSYS)                           
         DC    CL3'+  ',X'06',C'S',XL3'00',AL4(UPSYS)                           
         DC    CL3'UP ',X'06',C'S',XL3'00',AL4(UPSYS)                           
         DC    CL3'+NR',X'07',C'S',XL3'00',AL4(UPSYS)                           
         DC    CL3'+QU',X'08',C'S',XL3'00',AL4(UPSYS)                           
         DC    CL3'QUI',X'09',C'S',XL3'00',AL4(SQUIES)                          
         DC    CL3'MX+',X'0C',C'S',XL3'00',AL4(MXSYS)                           
         DC    CL3'MX-',X'0D',C'S',XL3'00',AL4(MXSYS)                           
         DC    CL3'QUI',X'02',C'F',XL3'00',AL4(QUIES)                           
         DC    CL3'CLO',X'03',C'F',XL3'00',AL4(QCLOSE)                          
         DC    CL3'QOP',X'04',C'F',XL3'00',AL4(QOPEN)                           
         DC    CL3'OP ',X'0A',C'F',XL3'00',AL4(QOPEN)                           
         DC    CL3'CLS',X'0B',C'F',XL3'00',AL4(QCLOSE)                          
         DC    CL3'LOC',X'10',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'LIO',X'11',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'LLO',X'12',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'FRE',X'13',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'FFR',X'14',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'X  ',X'7F',C'S',XL3'00',AL4(DUMMY)                           
         DC    CL3'X  ',X'7F',C'F',XL3'00',AL4(DUMMY)                           
         DC    CL3'X  ',X'7F',C'D',XL3'00',AL4(DUMMY)                           
         DC    XL4'00'                                                          
*                                                                               
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
DMCB     DS    6F                                                               
WORK     DS    CL80                                                             
WORK1    DS    CL32                                                             
DFLAG    DS    X                                                                
DFLGACT  EQU   X'80'               DIPLAY ACTIVE SYSTEMS ONLY                   
DFLGSAME EQU   X'40'               SAME SCREEN AS LAST TIME                     
DFLGDAT  EQU   X'20'               DATA ON SCREEN                               
DFLGMSG  EQU   X'10'               MESSAGE FIELD ALREADY SET                    
DFLGOVR  EQU   X'01'               OVERRIDE ENTERED IN P4                       
SFLAG    DS    X                                                                
SFLGSEL  EQU   X'80'               SELECT FIELD ENTERED                         
SFLGSLCT EQU   X'40'               SELECT ENTERED                               
*                                                                               
CHKSEL   DS    16XL8               PREV SCREEN SAVE                             
SELDATA  DS    CL8                 SELECTED FIELD                               
PARMFLG  DS    CL1                 D FOR DSPACE                                 
*                                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
VGETHELP DS    A                                                                
VGETFACT DS    A                                                                
ASAVE    DS    A                                                                
BASERD   DS    A                                                                
RELO     DS    A                                                                
*                                                                               
ASENTRY  DS    A                   A(CURRENTLY DISPLAYED SELIST)                
AFILES   DS    A                                                                
AFILEX   DS    A                                                                
ADTF     DS    A                                                                
AREQUEST DS    A                                                                
CURSOR   DS    A                                                                
SAVERE   DS    A                                                                
AHELP    DS    A                                                                
ADSPACE  DS    A                                                                
AFID     DS    A                                                                
PFKEY    DS    X                                                                
COUNT    DS    X                                                                
TRM      DS    H                                                                
RECLEN   DS    H                                                                
FACID    DS    CL4                                                              
FACNA    DS    CL3                                                              
TIMENOW  DS    F                                                                
*                                                                               
SAVETCB  DS    XL10                                                             
*                                                                               
OPMSG    DS    0CL40  '+FACPAK+ SSSSSSSS FFFFFFFF CCCCCCCCCC                    
OPFACID  DS    CL8                                                              
         DS    CL1                                                              
OPSYSID  DS    CL8                                                              
         DS    CL1                                                              
OPFILID  DS    CL8                                                              
         DS    CL1                                                              
OPCMND   DS    CL10                                                             
         DS    CL3                                                              
OPMSGL   EQU   *-OPMSG                                                          
*                                                                               
HELPKEY  DS    0CL10                                                            
         DS    XL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
         DS    XL5                                                              
*                                                                               
SAVEDSTR EQU   *                   SAVED PART OF W/S                            
SVTOP    DS    X                   TOP OF SCREEN INDEX                          
SVDISP   DS    X                   DISPLAY "S" OR "F"                           
SVSEL    DS    16XL8               SAVED IDS FOR SELECT                         
SVSELL   EQU   *-SVSEL                                                          
SWPPARMS DS    CL80                SWAP DATA                                    
         DS    X                                                                
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
IOAREA   DS    600D                4K WORK AREA                                 
WRKX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
SELINED  DSECT                                                                  
SACTH    DS    CL8                                                              
SACT     DS    CL3                                                              
SLINEH   DS    CL8                                                              
SLINE    DS    CL75                                                             
         ORG   SLINE                                                            
SNAME    DS    CL7                                                              
         DS    CL1                                                              
SNUM     DS    CL4                                                              
         DS    CL1                                                              
SSTRTD   DS    CL4                                                              
         DS    CL1                                                              
SRCVR    DS    CL4                                                              
         DS    CL1                                                              
SACTV    DS    CL4                                                              
         DS    CL1                                                              
SRWSTA   DS    CL6                                                              
         DS    CL1                                                              
SQUIESC  DS    CL4                                                              
         DS    CL1                                                              
SRSTRCT  DS    CL4                                                              
         DS    CL1                                                              
SNOP     DS    CL4                                                              
         DS    CL1                                                              
SQLEN    DS    CL4                                                              
         DS    CL1                                                              
SACTTSK  DS    CL4                                                              
         DS    CL1                                                              
SMAXTSK  DS    CL4                                                              
         DS    CL1                                                              
SRFILES  DS    CL10                                                             
         ORG                                                                    
SELINEL  EQU   *-SELINED                                                        
         EJECT                                                                  
FILINED  DSECT                                                                  
FACTH    DS    CL8                                                              
FACT     DS    CL3                                                              
FLINEH   DS    CL8                                                              
FLINE    DS    CL75                                                             
         ORG   FLINE                                                            
FNAME    DS    CL7                                                              
         DS    CL1                                                              
FXNUM    DS    CL4                                                              
         DS    CL1                                                              
FOPEN    DS    CL8                                                              
         DS    CL1                                                              
FNOP     DS    CL4                                                              
         DS    CL1                                                              
FDYNAM   DS    CL4                                                              
         DS    CL1                                                              
FRWSTA   DS    CL6                                                              
         DS    CL1                                                              
FXCPU    DS    CL4                                                              
         DS    CL1                                                              
FPREV    DS    CL4                                                              
         DS    CL1                                                              
FQDQ     DS    CL4                                                              
         DS    CL1                                                              
FRCV     DS    CL3                                                              
         DS    CL1                                                              
FREQ     DS    CL3                                                              
         DS    CL1                                                              
FPRTQ    DS    CL3                                                              
         DS    CL1                                                              
FHDR     DS    CL3                                                              
         DS    CL1                                                              
FISF     DS    CL3                                                              
         ORG                                                                    
FILINEL  EQU   *-FILINED                                                        
         EJECT                                                                  
DSLINED  DSECT                                                                  
DACTH    DS    CL8                                                              
DACT     DS    CL3                                                              
DLINEH   DS    CL8                                                              
DLINE    DS    CL75                                                             
         ORG   DLINE                                                            
DRSRC    DS    CL8                                                              
         DS    CL1                                                              
DRNUM    DS    CL3                                                              
         DS    CL1                                                              
DRTYPE   DS    CL3                                                              
         DS    CL1                                                              
DLWORD   DS    CL8                                                              
         DS    CL1                                                              
DLNAME   DS    CL8                                                              
         DS    CL1                                                              
DLJOB    DS    CL7                                                              
         DS    CL1                                                              
DLFLG    DS    CL3                                                              
         DS    CL1                                                              
DLLOCKS  DS    CL5                                                              
         DS    CL1                                                              
DLWAITS  DS    CL5                                                              
         DS    CL1                                                              
DLTIME   DS    CL15                                                             
         ORG                                                                    
DLLINEL  EQU   *-DSLINED                                                        
         EJECT                                                                  
DAOPEN   EQU   14                                                               
RDID     EQU   01                                                               
ADDADR   EQU   11                                                               
DACLOSE  EQU   15                                                               
         SPACE 2                                                                
         EJECT                                                                  
*DDFLDHDR                                                                       
*DDCOMFACS                                                                      
*FADSECTS                                                                       
*FAFACTS                                                                        
*DMDTFIS                                                                        
*DMDTFPH                                                                        
*DMSYSFD                                                                        
*DMSPACED                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMSYSFD                                                        
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
         EJECT                                                                  
SRQUIFFD DSECT                                                                  
         DS    CL64                                                             
*SRQUIFFD                                                                       
       ++INCLUDE SRQUIFFD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038SRSPC00   02/05/09'                                      
         END                                                                    
