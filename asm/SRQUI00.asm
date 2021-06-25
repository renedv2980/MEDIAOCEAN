*          DATA SET SRQUI00    AT LEVEL 004 AS OF 04/11/16                      
*PROCESS USING(WARN(15))                                                        
*PHASE T12500A                                                                  
         TITLE '$QUIESCE - DISPLAY CHANGE SYSTEM FILE STATUS'                   
         PRINT NOGEN                                                            
QUIESCE  CSECT                                                                  
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
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'UPDID'                                          
         L     R1,12(R1)                                                        
         MVC   MYUPDID,0(R1)       GET UPDID FOR SYSTEM                         
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL                                                        *         
***********************************************************************         
         BAS   RE,READSTR                                                       
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
EXIT     OC    AHELP,AHELP         USE HELPOUT IF AHELP NOT ZERO                
         BNZ   HELPOUT                                                          
         BAS   RE,WRITESTR         WRITE SAVED STR                              
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         AND SET CURSOR                               
EXIT1    XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE PARAMETERS                                                 *         
***********************************************************************         
PARMVAL  NTR1                                                                   
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
         CLC   8(6,R7),=C'DLOCKS'                                               
         BNE   *+12                                                             
         MVI   PARMFLG,C'L'        FLAG DLOCKS                                  
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
         BE    P2VAL10                                                          
         EX    R1,P2ACTV1          DISP ACTIVE SYSTEMS ONLY                     
         BE    P2VAL10                                                          
         EX    R1,P2INACT          DISP ACTIVE SYSTEMS ONLY                     
         BNE   *+8                                                              
         NI    DFLAG,255-DFLGACT                                                
         EX    R1,P2ALL            DISP ACTIVE SYSTEMS ONLY                     
         BNE   *+8                                                              
         NI    DFLAG,255-DFLGACT                                                
*                                                                               
P2VAL10  EX    R1,DSNAMES                                                       
         BNE   P2VALX                                                           
         MVI   DSNFLAG,C'Y'                                                     
*                                                                               
P2VALX   EQU   *                                                                
                                                                                
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
                                                                                
PARMVALX B     XIT1                                                             
                                                                                
P2ACTV   CLC   8(0,R7),=C'ACTIVE'                                               
P2ACTV1  CLC   8(0,R7),=C'ACTV'                                                 
P2INACT  CLC   8(0,R7),=C'INACTIVE'                                             
P2ALL    CLC   8(0,R7),=C'ALL'                                                  
DSNAMES  CLC   8(0,R7),=C'DSN'                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUB ACTION FIELDS                                          *         
***********************************************************************         
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
***********************************************************************         
* EXECUTE SUB ACTIONS                                                 *         
***********************************************************************         
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
***********************************************************************         
* DISPLAY SELIST DATA                                                 *         
***********************************************************************         
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
         TM    SEIND,SEIACTV+SEISTRT IGNORE NEVER ACTIVE                        
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
         GOTO1 VHEXOUT,DMCB,SESYS,SNUM,1,0,0                                    
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
*                                                                               
         TM    SEIND,SEIQUIES                                                   
         BZ    *+14                                                             
         OI    SLINEH+1,X'08'      HI INTENSITY                                 
         MVC   SQUIESC,YES                                                      
*                                                                               
         SR    RE,RE                                                            
         IC    RE,SESYS                                                         
         CVD   RE,DUB              NO BUILD ENQUEUE ID                          
         UNPK  DUB(4),DUB+6(2)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   UPDMIN+4(4),DUB                                                  
         XC    DUB(12),DUB                                                      
         LA    R2,UPDMAJ                                                        
         LA    RE,UPDMIN                                                        
         ENQ   ((R2),(RE),E,8,SYSTEM),RET=TEST                                  
         LTR   RF,RF                                                            
         BZ    DSE100                                                           
         TM    3(RF),X'08'         TEST IF RESOURCE IS OWNED BY ME              
         BZ    *+10                                                             
         MVC   SRENQ,YES1                                                       
         TM    3(RF),X'04'         TEST IF RESOURCE NOT AVAIL                   
         BZ    *+10                                                             
         MVC   SRENQ,YES2                                                       
*                                                                               
DSE100   TM    SEIND,SEIRESA                                                    
         BZ    *+10                                                             
         MVC   SNOP,DDS                                                         
         TM    SEIND,SEINOP                                                     
         BZ    *+10                                                             
         MVC   SNOP,NOP                                                         
         EDIT  (B2,SEQLEN),(4,SQLEN),ZERO=NOBLANK,ALIGN=LEFT                    
         EDIT  (B1,SETASKMX),(4,SMAXTSK),ZERO=NOBLANK,ALIGN=LEFT                
         EDIT  (B1,SETASK),(4,SACTTSK),ZERO=NOBLANK,ALIGN=LEFT                  
         L     R1,SEFILES                                                       
         L     R1,0(R1)                                                         
         EDIT  (B2,2(R1)),(4,SRFILES),ZERO=NOBLANK,ALIGN=LEFT                   
*                                                                               
         TM    SEIND,SEIRONLY+SEISETRO                                          
         BO    DSE102                                                           
*                                                                               
         ICM   RE,15,SEFILES       A(START OF FILES FOR SYSTEM)                 
         BZ    DSE102                                                           
         L     RE,0(RE)            A(SYSLIST)                                   
         USING SYSFLSTD,RE                                                      
         SR    RF,RF                                                            
         ICM   RF,3,SYSF#FLS       RF=NUM OF FILES                              
         BZ    DSE102                                                           
         LA    RE,SYSFLIST                                                      
*                                                                               
DSE101   SR    R1,R1               R1=A(DTF)                                    
         ICM   R1,7,SYSFADTF                                                    
         USING ISDTF,R1                                                         
         TM    ISFOPEN,ISFORO      TEST FOR READ ONLY                           
         BZ    DSE102                                                           
         LA    RE,SYSFLNQ(RE)      BUMP TO NEXT FILE                            
         BCT   RF,DSE101                                                        
         DROP  RE                                                               
*                                                                               
         MVC   SRWSTA,RDONLY       INDICATE READ ONLY IF ALL FILES ARE          
         DROP  R1                                                               
*                                                                               
DSE102   GOTO1 VDMOD000,DMCB,VFINDSYS,(1,0)                                     
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         AHI   R1,-4                                                            
         L     R1,0(R1)            EXTRACT SYSSTAB                              
         LTR   R1,R1                                                            
         BZ    DSE110              NO TABLE                                     
         SR    RF,RF                                                            
         IC    RF,SESYS                                                         
         AR    RF,R1                                                            
*                                                                               
         MVC   SRUPDADV,=C'ALL '                                                
         CLI   0(RF),X'FF'                                                      
         BE    DSE110                                                           
         MVC   SRUPDADV,=C'????'                                                
         CLI   0(RF),X'00'                                                      
         BE    DSE110                                                           
*                                                                               
         SR    RE,RE                                                            
         IC    RE,0(RF)                                                         
         L     R1,VSSB                                                          
         L     R1,SSBAFID-SSBD(R1)                                              
         SLL   RE,3                                                             
         AR    RE,R1                                                            
         MVC   SRUPDADV,0(RE)                                                   
*                                                                               
DSE110   LA    R7,SELINEL(R7)      NEXT SCREEN LINE                             
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
***********************************************************************         
* DISPLAY FILE DTFS                                                   *         
***********************************************************************         
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
                                                                                
DFIL025  ST    R7,CURSOR                                                        
         L     R4,AFILES                                                        
         L     R4,0(R4)            R4=A(SYSLIST FILE LIST)                      
         USING SYSFLSTD,R4                                                      
         MVC   HALF,SYSF#FLS       SAVE NUMBER OF FILES                         
         LA    R4,SYSFLIST                                                      
*                                                                               
         LH    RF,HALF             FIND POS IN FILES                            
         SR    R1,R1                                                            
         ICM   R1,1,SVTOP                                                       
         BZ    DFIL030                                                          
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         BCTR  RF,0                                                             
         BCT   R1,*-6                                                           
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
         SR    R6,R6               R6=A(DTF)                                    
         ICM   R6,7,SYSFADTF                                                    
         USING ISDTF,R6                                                         
         MVC   FLINE,FLDFAULT      SET DEFAULT DATA                             
         NI    FLINEH+1,255-X'08'                                               
*                                                                               
         MVC   FNAME,ISFFID                                                     
         LA    RF,3(R4)                                                         
         GOTO1 VHEXOUT,DMCB,(RF),FXNUM,1,0,0                                    
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
*                                                                               
DFIL050  TM    ISFFLAG,ISFGLOB                                                  
         BZ    *+14                                                             
         MVC   FPROT,=C'Global'                                                 
         B     DFIL060                                                          
         TM    ISFOPEN,ISFOENQ                                                  
         BZ    *+14                                                             
         MVC   FPROT,=C'DDSENQ'                                                 
         B     DFIL060                                                          
         TM    ISFOPEN,ISFOCPU                                                  
         BZ    *+10                                                             
         MVC   FPROT,=C'MVSENQ'                                                 
*                                                                               
DFIL060  CLI   DSNFLAG,C'Y'        DISPLAY DSN                                  
         BNE   DFIL061                                                          
         LA    R1,DYNBLK2                                                       
         ST    R1,DYNBLK1                                                       
         OI    DYNBLK1,X'80'                                                    
         LA    R1,DYNBLK4                                                       
         ST    R1,DYNBLK3                                                       
         LA    R1,DYNBLK5                                                       
         ST    R1,DYNBLK3+4                                                     
         OI    DYNBLK3+4,X'80'                                                  
         MVC   DYNBLK2,=X'1407000000000000000000000000000018000000'             
         LA    R1,DYNBLK3                                                       
         ST    R1,DYNBLK2+8                                                     
         MVC   DYNBLK4(6),=X'000100010008'                                      
         MVC   DYNBLK4+6(8),FNAME                                               
         MVC   DYNBLK5,SPACES                                                   
         MVC   DYNBLK5(6),=X'000500010020'                                      
         LA    R1,DYNBLK1                                                       
         DYNALLOC                                                               
         MVC   FDSNAME,SPACES                                                   
         MVC   FDSNAME(4),=C'DSN='                                              
         MVC   FDSNAME+4(26),DYNBLK5+6                                          
         B     DFIL900                                                          
*                                                                               
DFIL061  MVC   FORG,=C'DA'                                                      
         TM    0(R4),SFISF                                                      
         BZ    DFIL065                                                          
         MVC   FORG,=C'IS'                                                      
*&&UK                                                                           
         TM    ISFTYPE,ISFTPDOV                                                 
         BZ    DFIL065                                                          
         MVC   FORG,=C'IX'                                                      
*&&                                                                             
DFIL065  TM    SYSFIND1,SFRCV                                                   
         BZ    *+10                                                             
         MVC   FTYPE(6),=C'Rcv   '                                              
         TM    SYSFIND1,SFREQ                                                   
         BZ    *+10                                                             
         MVC   FTYPE(6),=C'Req    '                                             
         TM    SYSFIND1,SFPRTQ                                                  
         BZ    *+10                                                             
         MVC   FTYPE(6),=C'Prtq  '                                              
*                                                                               
         TM    SYSFIND1,SFHDR                                                   
         BZ    *+10                                                             
         MVC   FHDR,YES                                                         
         TM    SYSFIND1,SFISF      TEST FOR INDEX SEQ                           
         BO    DFIL080                                                          
*                                                                               
         USING DTFPHD,R6                                                        
DFIL070  MVC   FUSED,=C'    '      SET TO NOT AVAIL FIRST                       
         TM    DTFOPEN,ISFOOPN                                                  
         BZ    DFIL079                                                          
         LA    RE,DMTX                                                          
         USING EXTENTD,RE                                                       
         CLC   EXTENTD(EXTLNQ),=14X'FF'                                         
         BNE   *+8                                                              
         LA    RE,DNDXMTX                                                       
         TM    DIND,DINDXAM        HIGH CORE EXTENT MATRIX                      
         BZ    *+8                                                              
         ICM   RE,15,DMTX                                                       
         SAM31                                                                  
         SR    R1,R1               NUMBER OF TRACKS                             
         SR    R2,R2               NUMBER OF EXTENTS                            
         SR    RF,RF                                                            
DFIL071  CLI   0(RE),X'FF'         ANY MORE EXTENTS                             
         BE    DFIL072                                                          
         ICM   RF,3,EXT#TRKS       PICK UP HIGH TRACK                           
         AR    R1,RF                                                            
         LA    R2,1(R2)                                                         
         LA    RE,EXTLNQ(RE)                                                    
         B     DFIL071                                                          
         DROP  RE                                                               
*                                                                               
DFIL072  SAM24                                                                  
         ST    R1,FULL             SAVE TOTAL NUMBER OF TRACKS                  
         EDIT  (R1),(7,FTRKS)      SHOW TRACKS AND EXTENTS                      
         EDIT  (R2),(2,FXTN+1),ALIGN=LEFT                                       
*                                                                               
         SR    RF,RF               RF=TRACKS USED                               
         ICM   RF,3,DNEXT          16-BIT                                       
         TM    DTFTYPE,DTFTBIGF+DTFTBIG                                         
         BZ    DFL076                                                           
         BO    DFL074                                                           
         TM    DTFTYPE,DTFTBIGF                                                 
         BO    DFL073                                                           
         ICM   RF,7,DNEXT          18-BIT                                       
         SRL   RF,6                                                             
         B     DFL076                                                           
DFL073   ICM   RF,7,DNEXT          20-BIT                                       
         SRL   RF,4                                                             
         B     DFL076                                                           
DFL074   ICM   RF,7,DNEXT          22-BIT                                       
         SRL   RF,2                                                             
*                                                                               
DFL076   MHI   RF,100              COMPUTE PERCENTAGE USED                      
         SR    RE,RE                                                            
         ICM   R1,15,FULL          R1=TOTAL NUMBER OF TRACKS                    
         BZ    DFIL079                                                          
         DR    RE,R1                                                            
         EDIT  (RF),(4,FUSED),TRAIL=C'%',ZERO=NOBLANK                           
                                                                                
DFIL079  B     DFIL900                                                          
*                                                                               
         USING ISDTF,R6                                                         
DFIL080  MVC   FUSED,=C'    '      SET TO NOT AVAIL FIRST                       
         TM    ISFOPEN,ISFOOPN                                                  
         BZ    DFIL089                                                          
         LA    RE,ISXTNTIX                                                      
         USING EXTENTD,RE                                                       
         SR    R1,R1               NUMBER OF TRACKS                             
         SR    R2,R2               NUMBER OF EXTENTS                            
DFIL081  LA    R2,1(R2)                                                         
         SR    RF,RF                                                            
         ICM   RF,3,EXT#TRKS       PICK UP HIGH TRACK                           
         TM    ISFTYPE,ISFTBIGF                                                 
         BO    *+6                 BIG FILE HAS TRACKS PER EXTENT               
         SR    R1,R1               ELSE LAST EXTENT HAS TOTAL TRACKS            
         AR    R1,RF                                                            
         LA    RE,EXTLNQ(RE)                                                    
         CLI   0(RE),X'FF'         ANY MORE EXTENTS                             
         BNE   DFIL081                                                          
         DROP  RE                                                               
*                                                                               
         ST    R1,FULL             SAVE TOTAL TRACKS                            
         MVC   FTRKS(2),=C'p+'                                                  
         EDIT  (R1),(5,FTRKS+2)    SHOW TRACKS AND EXTENTS                      
         MVC   FXTN(2),=C'p+'                                                   
         EDIT  (R2),(1,FXTN+2),ALIGN=LEFT                                       
*                                                                               
         SR    RF,RF               RF=TRACKS USED                               
         ICM   RF,3,ISOVLAST       16-BIT                                       
         TM    ISFTYPE,ISFTBIGF+ISFTBIG                                         
         BZ    DFIL088                                                          
         ICM   RF,7,ISOVLAST       20-BIT                                       
         SRL   RF,4                                                             
*                                                                               
DFIL088  MHI   RF,100              COMPUTE PRECENTAGE USED                      
         SR    RE,RE                                                            
         ICM   R1,15,FULL                                                       
         BZ    DFIL089                                                          
         DR    RE,R1                                                            
         EDIT  (RF),(4,FUSED),TRAIL=C'%',ZERO=NOBLANK                           
DFIL089  B     DFIL900                                                          
*                                                                               
DFIL900  LA    R7,FILINEL(R7)      NEXT LINE                                    
         SR    RF,RF                                                            
         IC    RF,COUNT                                                         
         BCTR  RF,0                                                             
         LR    R1,RF                                                            
         SLL   RF,3                SAVE DATA FOR SELECT                         
         LA    RF,SVSEL(RF)                                                     
         MVI   0(RF),0             SPACE FOR SUB ACTION                         
         STCM  R6,15,4(RF)         Save off DTF for later                       
         MVC   1(1,RF),SYSFIND1    SAVE FLAGS                                   
*                                                                               
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         LH    RF,HALF                                                          
         BCT   RF,*+8                                                           
         B     DFIX                                                             
         STH   RF,HALF                                                          
         B     DFIL031                                                          
         DROP  R4                                                               
*                                                                               
DFIX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY DATASPACE HEADERS                                           *         
***********************************************************************         
DATADISP NTR1                                                                   
         L     RF,VSSB                                                          
         SAM31                                                                  
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,SSBALET-SSBD(RF)                                         
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         MVC   AECBS,0(R2)                                                      
*                                                                               
         MVI   SVDISP,C'D'         SET DATASPACE LIST                           
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
DISPDS1  OC    SVTOP,SVTOP         TEST FOR SAVED VALUE                         
         BZ    DDS050                                                           
         SR    R1,R1               INDEX INTO HEADERS                           
         IC    R1,SVTOP                                                         
         MHI   R1,L'DSPHDR                                                      
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
         C     R2,AECBS                                                         
         BNL   DDSX                                                             
         OC    DSPNAME,DSPNAME                                                  
         BZ    DDSNXT                                                           
*                                                                               
         NI    DLINEH+1,255-X'08'  NORMAL INTENSITY                             
         MVC   DLINE,DSDFAULT      SET DEFAULT DATA                             
*                                                                               
         MVC   DRSRC,DSPNAME                                                    
         LR    R1,R2                                                            
         S     R1,ADSPACE                                                       
         SRL   R1,6                DIV BY 64                                    
         ST    R1,FULL                                                          
         SAC   0                                                                
         GOTO1 VHEXOUT,DMCB,FULL+3,DRNUM,1,0,0                                  
         SAC   512                                                              
         MVC   DRTYPE+1(1),DSPTYPE                                              
*                                                                               
         CLC   DSPLOCK,=F'0'       ANY LOCK WORD                                
         BE    DDS090                                                           
*                                                                               
         MVC   FULL,DSPLOCK+2                                                   
         SAC   0                                                                
         GOTO1 VHEXOUT,DMCB,FULL,DLWORD+4,2,0,0                                 
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
DDS090   EDIT  (B2,DSPLCNT),(5,DLLOCKS),ZERO=NOBLANK                            
         EDIT  (B2,DSPWCNT),(5,DLWAITS),ZERO=NOBLANK                            
         MVC   FULL,DSPTIME                                                     
         OC    FULL,FULL                                                        
         BZ    DDS100                                                           
         BAS   RE,TIMEOUT                                                       
         MVC   DLTIME(11),WORK1    00:00:00.00.....                             
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
         SAM24                                                                  
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* READ IN TEMPSTR SAVED STORAGE                                       *         
***********************************************************************         
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
                                                                                
***********************************************************************         
* WRITE OUT TEMPSTR SAVED STORAGE                                     *         
***********************************************************************         
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
                                                                                
***********************************************************************         
* CLEAR TWA WITH TWAXC                                                *         
***********************************************************************         
TWAXC    NTR1                                                                   
         TWAXC (R1),SRVXLINH,PROT=Y                                             
XIT1     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* EDIT TUS IN FULL TO WORK HH:MM::SS.SS                               *         
***********************************************************************         
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
***********************************************************************         
* FIND ASENTRY FROM SELDATA                                           *         
***********************************************************************         
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
***********************************************************************         
* SELECT AN ENTRY                                                     *         
***********************************************************************         
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
***********************************************************************         
* QUIESCE A FILE. SET QUIESCE BIT ON FOR READ-ONLY                    *         
***********************************************************************         
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
         MVC   OPFILID,ISFFID                                                   
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
***********************************************************************         
* LOCK A DATASPACE RESOURCE                                           *         
***********************************************************************         
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
         MVC   DMCB+2(2),FULL+2                                                 
         GOTO1 VLOCKSPC,DMCB       LOCK OR FREE RESOURCE                        
         TM    DMCB+4,0                                                         
         BE    XIT1                                                             
         B     ERR16               REPORT ERRORS                                
         EJECT                                                                  
***********************************************************************         
* OPEN A FILE                                                         *         
***********************************************************************         
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
         BAS   RE,SETSYS                                                        
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID,ISFFID                                                   
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
QO1      GOTO1 VDATAMGR,DMCB,DADDS,DAOPEN,IOAREA,0,(R6),FULL,0                  
*                                                                               
         USING DTFPHD,R6                                                        
         MVC   FULL,=X'00010101'   16-BIT FORMAT HEADER RECORD DSKADR           
         TM    DTFTYPE,DTFTBIGF+DTFTBIG                                         
         BZ    QO2                                                              
         BO    QO1B                                                             
         TM    DTFTYPE,DTFTBIGF                                                 
         BO    QO1A                                                             
         MVC   FULL,=X'00004101'   18-BIT                                       
         B     QO2                                                              
QO1A     MVC   FULL,=X'00001101'   20-BIT                                       
         B     QO2                                                              
QO1B     MVC   FULL,=X'00000501'   22-BIT                                       
*                                                                               
QO2      TM    BYTE,SFHDR          READ HEADER RECORD IF DEFINED                
         BZ    QO3                                                              
         GOTO1 (RF),(R1),DADDS,RDID,IOAREA,0,(R6),FULL,0                        
*                                                                               
         SR    R0,R0               R0=TRACKS USED AT LOAD TIME                  
         ICM   R0,3,IOAREA+92                                                   
         CLI   IOAREA+94,X'FF'                                                  
         BE    *+8                                                              
         ICM   R0,4,IOAREA+94                                                   
*                                                                               
         TM    DTFTYPE,DTFTBIGF+DTFTBIG                                         
         BZ    QO2C                                                             
         BO    QO2B                                                             
         TM    DTFTYPE,DTFTBIGF                                                 
         BO    QO2A                                                             
         SLL   R0,32-18            18-BIT                                       
         ST    R0,FULL                                                          
         B     QO3                                                              
QO2A     SLL   R0,32-20            20-BIT                                       
         ST    R0,FULL                                                          
         B     QO3                                                              
QO2B     SLL   R0,32-22            22-BIT                                       
         ST    R0,FULL                                                          
         B     QO3                                                              
QO2C     SLL   R0,32-16            16-BIT                                       
         ST    R0,FULL                                                          
*                                                                               
QO3      GOTO1 (RF),(R1),DADDS,ADDADR,IOAREA,0,(R6),FULL,0                      
*                                                                               
QOX      BAS   RE,RELSYS                                                        
         BAS   RE,MULTI            OK TO MULTI TASK AGAIN                       
         BAS   RE,WTO                                                           
         BAS   RE,SECHECK                                                       
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* CLOSE A FILE                                                        *         
***********************************************************************         
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
         MVC   OPFILID,ISFFID                                                   
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
***********************************************************************         
* LOOP UNTIL SYSTEM AT ASENTRY IS INACTIVE                            *         
***********************************************************************         
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
         CLC   TCBSEN,SESYS        IF IT IS DO AN I/O AND WAIT                  
         BE    *+12                                                             
         BXLE  R6,R4,*-10                                                       
         B     SWAIT4                                                           
*                                                                               
         GOTO1 VGETFACT,DMCB,(X'80',=F'38400'),F#WAIT                           
         BCT   R3,SWAIT1                                                        
         B     ERR7                                                             
*                                                                               
SWAIT4   B     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK SELIST ENTRYS QUIESCE BIT IS CORRECT                          *         
***********************************************************************         
SECHECK  NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   AFILES,SEFILES                                                   
         MVC   AFILEX,SEFILEX                                                   
         NI    SEIND,255-SEIQUIES                                               
                                                                                
         L     R4,AFILES                                                        
         L     R4,0(R4)                                                         
         USING SYSFLSTD,R4                                                      
         MVC   HALF,SYSF#FLS       SAVE N'FILES IN HALF                         
         LA    R4,SYSFLIST                                                      
*                                                                               
SECHK01  SR    R1,R1                                                            
         ICM   R1,7,SYSFADTF                                                    
         USING ISDTF,R1                                                         
         TM    ISFOPEN,ISFOQUIE                                                 
         BZ    *+12                                                             
         OI    SEIND,SEIQUIES                                                   
         B     XIT1                                                             
         DROP  R1                                                               
*                                                                               
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         LH    RF,HALF                                                          
         BCT   RF,*+8                                                           
         B     XIT1                                                             
         STH   RF,HALF                                                          
         B     SECHK01                                                          
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* START A SYSTEM                                                      *         
***********************************************************************         
UPSYS    NTR1                                                                   
         MVC   SELDATA,0(R1)                                                    
         MVC   BYTE1,0(R1)                                                      
         MVI   SELDATA,6           FIX EXEC LEN TO 7                            
         BAS   RE,FINDSYS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     EXIT IF NO FILES IN SYSTEM                   
         BZ    UPOK                                                             
         TM    SEIND,SEISTRT       MAKE SURE SYSTEM IS DOWN                     
         BZ    UPS0                                                             
         B     ERR14               IF NOT ERROR                                 
*                                                                               
UPS0     BAS   RE,SETSYS           FRIG TCB ENTRY FOR SYSCHA                    
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VFINDSYS,(SESYS,0)                                 
                                                                                
         L     R1,4(R1)            R1=A(SYSFILES LIST)                          
         USING SYSFLSTD,R1                                                      
         LH    R0,SYSF#FLS         NUMBER OF FILES                              
         LA    R1,SYSFLIST         A(LIST OF FILES)                             
         XC    AREQUEST,AREQUEST                                                
*                                                                               
UPS1     L     RE,SYSFADTF-1       A(DTF)                                       
         USING ISDTF,RE                                                         
         TM    SYSFIND1,SFISF      TEST I/S FILE                                
         BZ    UPS1A                                                            
         TM    SYSFIND2,SFALIAS    TEST NON NATIVE FILE                         
         BO    UPS1A                                                            
         XC    ISCILAST,ISCILAST   CLEAR ISCILAST FOR I/D FILE                  
         DROP  RE                                                               
*                                                                               
UPS1A    TM    SYSFIND1,SFREQ      TEST REQUEST FILE                            
         BZ    *+8                                                              
         ST    RE,AREQUEST                                                      
         LA    R1,SYSFLNQ(R1)      BUMP TO NEXT FILE                            
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
         BE    UPS3                                                             
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
         MVC   OPFILID,SPACES                                                   
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
SQUIES   NTR1                                                                   
         MVC   SELDATA,0(R1)                                                    
         MVI   SELDATA,6           FIX EXEC LEN TO 7                            
         BAS   RE,FINDSYS                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     EXIT IF NO FILES IN SYSTEM                   
         BZ    SQUOKX                                                           
         BAS   RE,SYSWAIT                                                       
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID,SPACES                                                   
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
***********************************************************************         
* STOP A SYSTEM                                                       *         
***********************************************************************         
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
         MVC   OPFILID,SPACES                                                   
         MVC   OPCMND,STOPPED                                                   
         OC    SEFILES,SEFILES     EXIT IF NO FILES IN SYSTEM                   
         BZ    DNOK                                                             
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
***********************************************************************         
* ADJUST SYSTEM MAX TASKS                                             *         
***********************************************************************         
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
***********************************************************************         
* TURN ON/OFF ALL QUIESCE BITS FOR A SYSTEM                           *         
***********************************************************************         
QOFFALL  MVI   BYTE,0                                                           
         B     QALL                                                             
QONALL   MVI   BYTE,1                                                           
QALL     NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R4,SEFILES          A(START OF FILES FOR SYSTEM)                 
         L     R4,0(R4)            A(SYSLIST FILES LIST)                        
         USING SYSFLSTD,R4                                                      
         MVC   HALF,SYSF#FLS       SAVE NUMBER OF FILES IN LIST                 
         LA    R4,SYSFLIST                                                      
*                                                                               
QALL010  SR    R6,R6               R6=A(DTF)                                    
         ICM   R6,7,SYSFADTF                                                    
         USING ISDTF,R6                                                         
         CLI   BYTE,1              TURN OFF ALL QUIESCE BITS                    
         BE    *+8                                                              
         NI    ISFOPEN,255-ISFOQUIE                                             
         CLI   BYTE,0              TURN ON ALL QUIESCE BITS                     
         BE    *+8                                                              
         OI    ISFOPEN,ISFOQUIE                                                 
*                                                                               
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         LH    RF,HALF                                                          
         BCT   RF,*+8                                                           
         B     DFIX                                                             
         STH   RF,HALF                                                          
         B     QALL010                                                          
*                                                                               
QOFFX    B     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SET AND RESET TCB ENTRY                                             *         
***********************************************************************         
SETSYS   NTR1                      SET TCB ENTRY FOR SYSCHA                     
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R7,VSSB                                                          
         USING SSBD,R7                                                          
         L     R7,SSBTKADR                                                      
         USING TCBD,R7                                                          
         MVC   SAVETCB(8),TCBDTFS                                               
         MVC   SAVETCB+8(1),TCBSEN                                              
         MVC   TCBDTFS(8),SEFILES                                               
         MVC   TCBSEN,SESYS                                                     
         GOTO1 VDTFIOA,DMCB,(C'I',(R7))                                         
         B     XIT1                                                             
                                                                                
RELSYS   NTR1                      RESET TCB ENTRY                              
         L     R7,VSSB                                                          
         USING SSBD,R7                                                          
         L     R7,SSBTKADR                                                      
         USING TCBD,R7                                                          
         MVC   TCBDTFS(8),SAVETCB                                               
         MVC   TCBSEN,SAVETCB+8                                                 
         GOTO1 VDTFIOA,DMCB,(C'I',(R7))                                         
         B     XIT1                                                             
         DROP  R6                                                               
                                                                                
***********************************************************************         
* SET AND RESET SINGLE THREAD MODE                                    *         
***********************************************************************         
SINGLE   ST    RE,SAVERE                                                        
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),0    DISABLE MULTI-TASKING WAITS               
         GOTO1 VTICTOC,DUB,C'SSET'    SUSPEND TIMERS                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MULTI    ST    RE,SAVERE                                                        
         GOTO1 VTICTOC,DUB,C'RSET'    RESET TIMERS                              
         L     RE,VSSB                                                          
         MVI   SSBMTIND-SSBD(RE),C'M' ENABLE MULTI-TASKING WAITS                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* SAVE AND LOAD PARAMETERS                                            *         
***********************************************************************         
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
                                                                                
DUMMY    BR    RE                                                               
                                                                                
RESTRICT B     ERR10                                                            
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
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
*                                                                               
ERRX     L     RD,BASERD                                                        
         TM    DFLAG,DFLGMSG       DON'T FUCK WITH MSG FIELD                    
         BO    EXIT                                                             
         XC    DMCB(24),DMCB       R0 HAS SERVICE SYSTEM ERR NUM                
         GOTO1 VGETTXT,DMCB,(R0),0,(C'E',0),(4,FACID)                           
         B     EXIT                                                             
***********************************************************************         
* ERROR EXITS                                                         *         
***********************************************************************         
INF0     LA    R0,300              FILE STATUS CHANGED                          
         B     INFX                                                             
INF1     LA    R0,301              SYSTEM FILES DISPLAYED                       
         B     INFX                                                             
INF2     LA    R0,302              SYSTEM STATUS DISPLAYED                      
         B     INFX                                                             
INF3     LA    R0,308              DATASPACE RESOURCES DISPLAYED                
         B     INFX                                                             
INFX     TM    DFLAG,DFLGMSG       DONT FUCK WITH MSG FIELD                     
         BO    EXIT                                                             
         XC    DMCB(24),DMCB                                                    
         GOTO1 VGETTXT,DMCB,(R0),0,(C'I',0),(4,FACID)                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CALL GETHELP AND EXIT TO MONITOR                                    *         
***********************************************************************         
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
         LA    R1,SRVSELH          OR GREATER                                   
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
***********************************************************************         
WTO      NTR1                                                                   
         OC    OPMSG,SPACES                                                     
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,DMCB,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                       
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         CNOP  0,8                                                              
MYUPDID  DS    0CL16                                                            
UPDMAJ   DC    C'DMGR    '                                                      
UPDMIN   DC    C'SNUM0000'                                                      
DOTS     DC    40C'.'                                                           
SPACES   DC    80C' '                                                           
QUID     DC    C'$QUI'                                                          
YES      DC    CL7'Yes'                                                         
YES1     DC    CL7'Yes+'                                                        
YES2     DC    CL7'Yes-'                                                        
DDS      DC    CL7'DDS'                                                         
NOP      DC    CL7'NOP'                                                         
RDONLY   DC    CL4' R/O'                                                        
RDWRT    DC    CL4' R/W'                                                        
SETRO    DC    CL4'SR/O'                                                        
QUIRO    DC    CL4'QR/O'                                                        
*                                                                               
QCLOSED  DC    CL10'Qclosed'                                                    
OPEN     DC    CL10'Open'                                                       
OPENED   DC    CL10'Opened'                                                     
CLOSED   DC    CL10'Closed'                                                     
QUIESCED DC    CL10'Quiesced'                                                   
UNQUI    DC    CL10'Unquiesced'                                                 
QOPENED  DC    CL10'Qopened'                                                    
STARTED  DC    CL10'Started'                                                    
STOPPED  DC    CL10'Stopped'                                                    
QSTARTED DC    CL10'Qstarted'                                                   
*                                                                               
DMREAD   DC    CL7'DMREAD'                                                      
DMWRT    DC    CL7'DMWRT'                                                       
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DADDS    DC    CL7'DADDS'                                                       
ISDDS    DC    CL7'ISDDS'                                                       
HELPID   DC    XL10'0125FF00010000000000'                                       
                                                                                
*               *....+....1....+....2....+....3....+....4                       
SEHEADR0 DC    C'                      Rcvr Prev        Q'                      
         DC    C'ui- MVS  DDS/                Num  Updt '                       
SEHEADER DC    C'Sel Sename  Num  Strt Pend Actv Access e'                      
         DC    C'sc  ENQ  Nop  Qlen Act  Max  File ADV  '                       
SEUNDER  DC    C'--- ------- ---- ---- ---- ---- ------ -'                      
         DC    C'--- ---- ---- ---- ---- ---- ---- ---- '                       
SEDFAULT DC        C'             No   .... ....  R/W   .'                      
         DC    C'... .... ....                          '                       
*                                                                               
FLHEADR0 DC    C'             Ext               Dyna     '                      
         DC    C'            Usable                     '                       
FLHEADER DC    C'Sel File-Id  Num Status   Nop  DD   Mode'                      
         DC    C' Protn  Xtn Tracks  Used Or Type   Hdr '                       
FLUNDER  DC    C'--- -------- --- -------- ---- ---- ----'                      
         DC    C' ------ --- ------- ---- -- ------ --- '                       
FLDFAULT DC    C'             Closed   ...  ....  R/W    '                      
         DC    C'                                       '                       
*                                                                               
DSHEADR0 DC    C'    Resource         Lock     Owner     '                      
         DC    C'           Num   Num                   '                       
DSHEADER DC    C'Sel Name     Num Typ Word     Name     J'                      
         DC    C'ob     Flg Locks Waits Lock time       '                       
DSUNDER  DC    C'--- -------- --- --- -------- -------- -'                      
         DC    C'------ --- ----- ----- ----------------'                       
DSDFAULT DC    C'                                        '                      
         DC    C'                                       '                       
*                                                                               
DCHEADR0 DC    C'    Resource          Online -----------'                      
         DC    C'--------    Offline ------------------ '                       
DCHEADER DC    C'Sel Name     Num Sta  Locks  Waits  Ltim'                      
         DC    C'e  Wtime    Locks  Waits  Ltime  Wtime '                       
DCUNDER  DC    C'--- -------- --- ---  ------ ------ ----'                      
         DC    C'-- ------   ------ ------ ------ ------'                       
DCDFAULT DC    C'                                        '                      
         DC    C'                                       '                       
*                                                                               
LSHEADR0 DC    C'    System   Max       Actv      High   '                      
         DC    C'   Total     Total                     '                       
LSHEADER DC    C'             Locks     Locks     water  '                      
         DC    C'   Locks     Waits                     '                       
LSUNDER  DC    C'--- -------- -------   -------   -------'                      
         DC    C'   -------   -------                   '                       
LSDFAULT DC    C'                                        '                      
         DC    C'                                       '                       
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
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE AND SCREEN DSECTS                                   *         
***********************************************************************         
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
DSNFLAG  DS    CL1                 Y FOR DSNAMES                                
*                                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
VGETFACT DS    A                                                                
VGETHELP DS    A                                                                
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
SAVER2   DS    A                                                                
AHELP    DS    A                                                                
ADSPACE  DS    A                                                                
AECBS    DS    A                                                                
AFID     DS    A                                                                
PFKEY    DS    X                                                                
COUNT    DS    X                                                                
TRM      DS    H                                                                
RECLEN   DS    H                                                                
FACID    DS    CL4                                                              
FACNA    DS    CL3                                                              
TIMENOW  DS    F                                                                
*                                                                               
DYNBLK1  DS    F                   DYNALLOC BLOCK                               
DYNBLK2  DS    XL20                                                             
DYNBLK3  DS    XL8                                                              
DYNBLK4  DS    XL14                                                             
DYNBLK5  DS    XL38                                                             
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
SRENQ    DS    CL4                                                              
         DS    CL1                                                              
SNOP     DS    CL4                                                              
         DS    CL1                                                              
SQLEN    DS    CL4                                                              
         DS    CL1                                                              
SACTTSK  DS    CL4                                                              
         DS    CL1                                                              
SMAXTSK  DS    CL4                                                              
         DS    CL1                                                              
SRFILES  DS    CL4                                                              
         DS    CL1                                                              
SRUPDADV DS    CL4                                                              
         ORG                                                                    
SELINEL  EQU   *-SELINED                                                        
         EJECT                                                                  
FILINED  DSECT                                                                  
FACTH    DS    CL8                                                              
FACT     DS    CL3                                                              
FLINEH   DS    CL8                                                              
FLINE    DS    CL75                                                             
         ORG   FLINE                                                            
FNAME    DS    CL8                                                              
         DS    CL1                                                              
FXNUM    DS    CL3                                                              
         DS    CL1                                                              
FOPEN    DS    CL8                                                              
         DS    CL1                                                              
FNOP     DS    CL4                                                              
         DS    CL1                                                              
FDYNAM   DS    CL4                                                              
         DS    CL1                                                              
FRWSTA   DS    CL4                                                              
         DS    CL1                                                              
FPROT    DS    CL6                                                              
         DS    CL1                                                              
FDSNAME  DS    0CL31                                                            
FXTN     DS    CL3                                                              
         DS    CL1                                                              
FTRKS    DS    CL7                                                              
         DS    CL1                                                              
FUSED    DS    CL4                                                              
         DS    CL1                                                              
FORG     DS    CL2                                                              
         DS    CL1                                                              
FTYPE    DS    CL6                                                              
         DS    CL1                                                              
FHDR     DS    CL3                                                              
         DS    CL1                                                              
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
CLLINED  DSECT                                                                  
CACTH    DS    CL8                                                              
CACT     DS    CL3                                                              
CLINEH   DS    CL8                                                              
CLINE    DS    CL75                                                             
         ORG   CLINE                                                            
CRSRC    DS    CL8                                                              
         DS    CL1                                                              
CRNUM    DS    CL3                                                              
         DS    CL1                                                              
CRSTA    DS    CL3                                                              
         DS    CL2                                                              
CRLOCK   DS    CL6                                                              
         DS    CL1                                                              
CRWAIT   DS    CL6                                                              
         DS    CL1                                                              
CRLT     DS    CL6                                                              
         DS    CL1                                                              
CRWT     DS    CL6                                                              
         DS    CL3                                                              
CRLOCKO  DS    CL6                                                              
         DS    CL1                                                              
CRWAITO  DS    CL6                                                              
         DS    CL1                                                              
CRLTO    DS    CL6                                                              
         DS    CL1                                                              
CRWTO    DS    CL6                                                              
         ORG                                                                    
CLLINEL  EQU   *-CLLINED                                                        
         EJECT                                                                  
LSLINED  DSECT                                                                  
LACTH    DS    CL8                                                              
LACT     DS    CL3                                                              
LLINEH   DS    CL8                                                              
LLINE    DS    CL75                                                             
         ORG   LLINE                                                            
LRSRC    DS    CL8                                                              
         DS    CL1                                                              
LRMAX    DS    CL7                                                              
         DS    CL3                                                              
LRACTV   DS    CL7                                                              
         DS    CL3                                                              
LRHIGH   DS    CL7                                                              
         DS    CL3                                                              
LRLOCK   DS    CL7                                                              
         DS    CL3                                                              
LRWAIT   DS    CL7                                                              
         DS    CL3                                                              
         ORG                                                                    
LLLINEL  EQU   *-LSLINED                                                        
         EJECT                                                                  
***********************************************************************         
* EQUATES FROM DMFILES AND DSECTS                                     *         
***********************************************************************         
DAOPEN   EQU   14                                                               
RDID     EQU   01                                                               
ADDADR   EQU   11                                                               
DACLOSE  EQU   15                                                               
                                                                                
*DDFLDHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*DDCOMFACS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*FADSECTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
*FAFACTS                                                                        
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*DMDTFIS                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*DMDTFPH                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMDTFPH                                                        
         PRINT ON                                                               
*DMSYSFD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMSYSFD                                                        
         PRINT ON                                                               
*DMXTNTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE DMXTNTD                                                        
         PRINT ON                                                               
*DMSPACED                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMSPACED                                                       
         PRINT ON                                                               
*DMDSYSHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMDSYSHDR                                                      
         PRINT ON                                                               
         EJECT                                                                  
SRQUIFFD DSECT                                                                  
         DS    CL64                                                             
*SRQUIFFD                                                                       
       ++INCLUDE SRQUIFFD                                                       
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SRQUI00   04/11/16'                                      
         END                                                                    
