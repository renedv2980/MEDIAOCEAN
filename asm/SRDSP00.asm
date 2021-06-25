*          DATA SET SRDSP00    AT LEVEL 087 AS OF 09/17/20                      
*PHASE T12E00A                                                                  
         TITLE '$DSPACE - DISPLAY DATASPACE TABLES '                            
         PRINT NOGEN                                                            
DSPACE   CSECT                                                                  
         NMOD1 WRKX-WRKD,*$DSP**,RA,R9,R8,CLEAR=YES,RR=R4                       
         USING WRKD,RC                                                          
         ST    RD,BASERD                                                        
         ST    R4,RELO                                                          
         USING SRPARMD,R1          R1=A(S/R PARAM LIST)                         
         L     R3,SRQASYSF                                                      
         USING SYSFACD,R3          R8=A(SYS FAC LIST)                           
         MVC   MYSSB,VSSB                                                       
         MVC   ASELIST,VSELIST                                                  
         MVC   ADATAMGR,VDATAMGR                                                
         MVC   ALOCKSPC,VLOCKSPC                                                
         MVC   ATICTOC,VTICTOC                                                  
         MVC   AWCTYPE,VWCTYPE                                                  
         MVC   ADMOD000,VDMOD000                                                
         MVC   ADDSTATE,VDDSTATE                                                
         DROP  R3                                                               
         L     R3,SRQATWA                                                       
         USING SRDSPFFD,R3         R3=A(TWA)                                    
         L     RF,SRQATIA                                                       
         ST    RF,ASAVE            USE TIA AS SAVE AREA                         
         L     RF,SRQACOMF                                                      
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
*                                                                               
         L     RF,SRQAUTL          EXTRACT UTL DATA                             
         ST    RF,MYUTL                                                         
         USING UTLD,RF                                                          
         MVC   TRM,TNUM                                                         
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
         MVC   VADDAY,CADDAY                                                    
         MVC   VGETHELP,CGETHELP                                                
         DROP  R1,RF                                                            
*                                                                               
         L     RF,MYSSB            EXTRACT SSB DATA                             
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL                                                   
         MVC   FACID(4),SSBSYSN4                                                
         MVC   FACNA(3),SSBSYSNA                                                
         MVC   AFID(4),SSBAFID                                                  
         MVC   OPMSG,SPACES                                                     
         MVC   OPFACID,=C'+FACPAK+'                                             
         MVC   OPFACID+4(3),FACNA                                               
*                                                                               
         MVC   MYALET,SSBALET      DMGR ALET                                    
         CLC   SRVSRV+1(5),=C'DTABS'                                            
         BNE   *+14                                                             
         MVC   MYALET,SSBTBLET     TABS ALET                                    
         MVI   TABS,C'Y'                                                        
*                                                                               
         TIME  BIN                                                              
         ST    R0,TIMENOW                                                       
*                                                                               
         TIME  TU                                                               
         ST    R1,TODAY            SAVE DATE                                    
*                                                                               
         GOTO1 VDATCON,DMCB,(6,TODAY),(0,DUB)                                   
         GOTO1 VADDAY,DMCB,(C'D',DUB),DUB1,F'1'                                 
         GOTO1 VDATCON,DMCB,(0,DUB1),(15,TOMORROW)                              
         GOTO1 VADDAY,DMCB,(C'D',DUB),DUB1,F'-1'                                
         GOTO1 VDATCON,DMCB,(0,DUB1),(15,YESTERDY)                              
         GOTO1 VDATCON,DMCB,(0,DUB),(15,TODAY)                                  
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        MAIN CONTROL                                       *                   
*************************************************************                   
         SPACE 1                                                                
MAIN     BAS   RE,READSTR                                                       
*                                                                               
MAIN010  SR    R1,R1               TEST FOR UP OR DOWN                          
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
         CLI   PFKEY,0             ANY PFKEY HIT                                
         BE    QUI020                                                           
*                                                                               
         LA    R1,ACTTAB                                                        
QUI010   CLC   PFKEY,8(R1)         SCAN FOR PFKEY HIT                           
         BE    QUI015                                                           
         LA    R1,12(R1)                                                        
         CLI   0(R1),C'X'          TEST EOT                                     
         BNE   QUI010                                                           
         B     QUI020              NOT FOUND                                    
*                                                                               
QUI015   MVC   ACTION,7(R1)        SET ACTION                                   
         CLC   SVACT,ACTION                                                     
         BE    *+8                                                              
         MVI   SVTOP,0                                                          
         MVC   SVACT,ACTION        SET ACTION                                   
         B     QUI021                                                           
*                                                                               
QUI020   BAS   RE,P1VAL            VALIDATE P1                                  
QUI021   BAS   RE,P2VAL            VALIDATE P2                                  
         BAS   RE,P4VAL            VALIDATE P4                                  
                                                                                
         LA    R1,ACTTAB                                                        
QUI030   CLC   ACTION,7(R1)                                                     
         BNE   QUI031                                                           
         MVC   SRVP1(7),0(R1)      ECHO ACTION                                  
         SR    RF,RF                                                            
         ICM   RF,7,9(R1)                                                       
         A     RF,RELO                                                          
         BR    RF                                                               
*                                                                               
QUI031   LA    R1,12(R1)           NEXT TABLE ENTRY                             
         CLI   0(R1),C'X'                                                       
         BNE   QUI030                                                           
         B     ERR0                                                             
*                                                                               
QUIDS    CLI   SRVP2,C'A'          DISPLAY ACTIVE LOCKS                         
         BE    QUIDSA                                                           
*                                                                               
         BAS   RE,COUNTS           DISPLAY DATASPACE LIST                       
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         TM    SFLAG,SFLGSLCT      TEST IF SELECT ENTERED                       
         BO    MAIN010                                                          
         BAS   RE,COUNTS           REDISPLAY AS STATUS CHANGED                  
         B     INF3                                                             
*                                                                               
QUIDSA   BAS   RE,DATADISP         DISPLAY DATASPACE LIST                       
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,DATADISP         REDISPLAY AS STATUS CHANGED                  
         B     INF3                                                             
*                                                                               
QUILS    BAS   RE,LOCKDISP         DISPLAY LOCKTAB SUMMARY                      
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,LOCKDISP         REDISPLAY AS STATUS CHANGED                  
         B     INF3                                                             
*                                                                               
WAITS    BAS   RE,WAITDISP         DISPLAY WAIT ECBS                            
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,WAITDISP         REDISPLAY                                    
         B     INF3                                                             
*                                                                               
FILES    BAS   RE,FILDISP          DISPLAY FILES                                
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,FILDISP          REDISPLAY FILES                              
         B     INF3                                                             
*                                                                               
RECOVS   BAS   RE,RECODISP         DISPLAY RECOVERY DATA                        
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,RECODISP         REDISPLAY                                    
         B     INF3                                                             
*                                                                               
JOBS     BAS   RE,JOBDISP          DISPLAY JOBTAB SUMMARY                       
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,JOBDISP          REDISPLAY                                    
         B     INF3                                                             
*                                                                               
COMMS    BAS   RE,COMDISP          DISPLAY COMTAB                               
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,COMDISP          REDISPLAY                                    
         B     INF3                                                             
*                                                                               
STATE    BAS   RE,COMDISP          DISPLAY STATES                               
         BAS   RE,VALSUB                                                        
         TM    SFLAG,SFLGSEL       TEST ANY SUB ACTIONS                         
         BNO   INF3                                                             
         BAS   RE,ACTSUB                                                        
         BAS   RE,COMDISP          REDISPLAY                                    
         B     INF3                                                             
*                                                                               
EXIT     OC    AHELP,AHELP         USE HELPOUT IF AHELP NOT ZERO                
         BNZ   HELPOUT                                                          
         BAS   RE,WRITESTR         WRITE SAVED STR                              
         L     R1,CURSOR                                                        
         OI    6(R1),X'40'         AND SET CURSOR                               
EXIT1    XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
*************************************************************                   
*        VALIDATE PARMS                                     *                   
*************************************************************                   
         SPACE 1                                                                
P1VAL    NTR1                                                                   
         MVI   ACTION,C'D'         DEFAULT IS DSPACE                            
         LA    R7,SRVP1H                                                        
         ST    R7,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R7)                                                       
         BZ    P1VALX                                                           
*                                                                               
         CLI   8(R7),C'?'                                                       
         BNE   *+8                                                              
         ST    R7,AHELP                                                         
*                                                                               
         BCTR  R1,0                                                             
         LA    RF,ACTTAB           CHECK TABLE FOR ACTIONS                      
P1V010   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R7),0(RF)                                                    
         BNE   *+14                                                             
         MVC   ACTION,7(RF)        SAVE LETTER IN ACTION                        
         B     P1VALX                                                           
*                                                                               
         LA    RF,12(RF)                                                        
         CLI   0(RF),C'X'          TEST FOR EOT                                 
         BNE   P1V010                                                           
         B     ERR0                                                             
*                                                                               
P1VALX   CLC   SVACT,ACTION                                                     
         BE    *+8                                                              
         MVI   SVTOP,0                                                          
         MVC   SVACT,ACTION        SET ACTION                                   
         B     XIT1                                                             
*                                                                               
P2VAL    NTR1                                                                   
         OI    DFLAG,DFLGACT       THIS IS DEFAULT                              
         LA    R7,SRVP2H                                                        
         ST    R7,CURSOR                                                        
         SR    R1,R1                                                            
         ICM   R1,1,5(R7)                                                       
         BNZ   P2VAL01                                                          
         CLI   ACTION,C'F'         FILES MUST HAVE SYSTEM                       
         BE    ERR1                                                             
         CLI   ACTION,C'J'         SO MUST JOBS                                 
         BE    ERR1                                                             
*                                                                               
P2VAL01  CLI   8(R7),C'?'                                                       
         BNE   *+8                                                              
         ST    R7,AHELP                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,P2ACTV           DISP ACTIVE SYSTEMS ONLY                     
         BE    P2VALX                                                           
         EX    R1,P2ACTV1          DISP ACTIVE SYSTEMS ONLY                     
         BE    P2VALX                                                           
         EX    R1,P2INACT          DISP INACTIVE SYSTEMS ONLY                   
         BNE   *+8                                                              
         NI    DFLAG,255-DFLGACT                                                
*                                                                               
         CLI   ACTION,C'D'         THESE DO NOT NEED SYSTEM NAME                
         BE    P2VALX                                                           
         CLI   ACTION,C'W'                                                      
         BE    P2VALX                                                           
         CLI   ACTION,C'C'                                                      
         BE    P2VALX                                                           
*                                                                               
         CLI   5(R7),0             LOCKS / SE IS OPTIONAL                       
         BNE   *+12                                                             
         CLI   ACTION,C'L'                                                      
         BE    P2VALX                                                           
*                                                                               
         CLI   5(R7),4                                                          
         BL    ERR4                LESS THAN MIN                                
         CLI   5(R7),7                                                          
         BH    ERR5                GREATER THAN MAX                             
         STC   R1,SELDATA          SAVE EXECUTE LEN                             
         MVC   SELDATA+1(7),8(R7)  SAVE FIELD                                   
*                                                                               
P2VALX   B     XIT1                                                             
         SPACE 1                                                                
P4VAL    NTR1                                                                   
         LA    R7,SRVP4H           CHECK FOR OVERRIDE                           
         ST    R7,CURSOR                                                        
         SR    R1,R1                                                            
         CLI   5(R7),0                                                          
         BE    P4VALX                                                           
         CLI   5(R7),8                                                          
         BNE   P4VALX                                                           
         CLC   8(8,R7),=C'OVERRIDE'                                             
         BNE   P4VALX                                                           
         OI    DFLAG,DFLGOVR       SET OVERRIDE FLAG                            
P4VALX   B     XIT1                                                             
         SPACE 1                                                                
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
VALS011  LA    R7,DLLINEL(R7)                                                   
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
*                                                                               
         LA    RE,SELDMGR                                                       
         CLI   TABS,C'Y'                                                        
         BNE   *+8                                                              
         LA    RE,SELTABS                                                       
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
         USING DSLINED,R7                                                       
         LA    R4,16               COUNT 16 TIMES                               
ACT010   STC   R4,COUNT                                                         
         BCTR  R4,0                                                             
         SLL   R4,3                                                             
         LA    R5,SVSEL(R4)        CHECK SVSEL ENTRY                            
         CLI   0(R5),0                                                          
         BE    ACT050              NO SELECT SO GET NEXT                        
*                                                                               
         LA    RE,SELDMGR          SCAN SELDMGR FOR ACTION                      
         CLI   TABS,C'Y'                                                        
         BNE   *+8                                                              
         LA    RE,SELTABS          SCAN SELDMGR FOR ACTION                      
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
         OI    DACTH+1,X'08'       TURN ON HI INTENSITY                         
*                                                                               
ACT050   LA    R7,DLLINEL(R7)      NEXT LINE                                    
         SR    R4,R4                                                            
         IC    R4,COUNT                                                         
         BCT   R4,ACT010           BACK FOR NEXT                                
         LA    R7,SRVSELH                                                       
         ST    R7,CURSOR                                                        
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY DATASPACE HEADERS                          *                   
*************************************************************                   
         SPACE 1                                                                
DATADISP NTR1                                                                   
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
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
         MH    R1,=Y(L'DSPHDR)                                                  
         AR    R2,R1                                                            
*                                                                               
         CHI   R2,(255*64)         UP TO 255 SYSTEMS                            
         JL    DDS050                                                           
         XR    R2,R2                                                            
         MVI   SVTOP,0                                                          
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
         LTR   R2,R2               1ST ENTRY IS HEADERS                         
         BZ    DDSNXT                                                           
         OC    DSPNAME,DSPNAME                                                  
         BZ    DDSNXT                                                           
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
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CS    RE,RF,DSPLOCK       Any lock word?                               
         BE    DDS090              Nothing there                                
         MVC   FULL(2),DSPLOCK                                                  
         MVC   FULL+2(2),DSPJOB                                                 
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
DDS080   XC    FULL,FULL                                                        
         MVC   FULL(3),DSPJOB      DISPLAY ADV OR JOB INFO                      
         BAS   RE,DISJOB                                                        
         MVC   DLJOB,DUB1                                                       
*                                                                               
DDS090   MVC   FULL,DSPTIME                                                     
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
         CHI   R2,(255*64)         UP TO 255 SYSTEMS                            
         JL    DDS060                                                           
*                                                                               
DDSX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY DATASPACE COUNTERS                         *                   
*************************************************************                   
         SPACE 1                                                                
COUNTS   NTR1                                                                   
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         MVC   AECBS,0(R2)                                                      
*                                                                               
         MVI   SVDISP,C'D'         SET DATASPACE LIST                           
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
         OC    SVTOP,SVTOP         TEST FOR SAVED VALUE                         
         BZ    CTD050                                                           
         SR    R1,R1               INDEX INTO HEADERS                           
         IC    R1,SVTOP                                                         
         MH    R1,=Y(L'DSPHDR)                                                  
         AR    R2,R1                                                            
         CHI   R2,(255*64)         UP TO 255 SYSTEMS                            
         JL    CTD050                                                           
         XR    R2,R2                                                            
         MVI   SVTOP,0                                                          
*                                                                               
CTD050   MVC   SRVHDR0,DCHEADR0                                                 
         MVC   SRVHDR,DCHEADER                                                  
         MVC   SRVHDR1,DCUNDER                                                  
*                                                                               
         CLI   TABS,C'Y'           TABS HAVE LENGTH NOT STATUS                  
         BNE   *+10                                                             
         MVC   SRVHDR+17(6),=C'Length'                                          
*                                                                               
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING CLLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    CTD055                                                           
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
CTD055   ST    R7,CURSOR                                                        
         LA    R1,16               SET COUNT TO 16 LINES                        
*                                                                               
CTD060   LTR   R1,R1               TEST AND SAVE COUNT                          
         BZ    CTDX                                                             
         STC   R1,COUNT                                                         
         C     R2,AECBS                                                         
         BNL   CTDX                                                             
         LTR   R2,R2               FIRST ENTRY IS HEADERS                       
         BZ    CTDNXT                                                           
         OC    DSPNAME,DSPNAME                                                  
         BZ    CTDNXT                                                           
*                                                                               
         NI    CLINEH+1,255-X'08'  NORMAL OR                                    
*                                                                               
*        OI    CLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         MVC   CLINE,DSDFAULT      SET DEFAULT DATA                             
*                                                                               
         MVC   CRSRC,DSPNAME                                                    
         LR    R1,R2                                                            
         S     R1,ADSPACE                                                       
         SRL   R1,6                DIV BY 64                                    
         ST    R1,FULL                                                          
         SAC   0                                                                
         GOTO1 VHEXOUT,DMCB,FULL+3,CRNUM,1                                      
         SAC   512                                                              
*                                                                               
         ST    R2,SAVER2           SAVE A HEADER                                
         ICM   R2,15,60(R2)                                                     
         SLL   R2,2                                                             
         SRL   R2,2                                                             
         USING DMSYSHDR,R2                                                      
         MVC   DUB,DSYBDATE        EXTRACT BILLDATE                             
         CLI   TABS,C'Y'           TABS HAVE NO DATE                            
         BNE   *+10                                                             
         XC    DUB,DUB                                                          
         L     R2,SAVER2                                                        
         USING DMSPACED,R2         RESTORE HEADER                               
*                                                                               
         OC    DUB(4),DUB          NULL DATE                                    
         BNZ   *+14                                                             
         MVC   CRBDATE,=C'-n/a- '                                               
         B     CTD066                                                           
*                                                                               
         CLC   DUB(4),TODAY        TODAYS DATE                                  
         BNE   *+12                                                             
         MVI   CRBDATE+5,C' '                                                   
         B     CTD065                                                           
*                                                                               
         CLC   DUB(4),TOMORROW     TOMORROWS DATE                               
         BNE   *+12                                                             
         MVI   CRBDATE+5,C'T'                                                   
         B     CTD065                                                           
*                                                                               
         CLC   DUB(4),YESTERDY     YESTERDAY DATE                               
         BNE   *+12                                                             
         MVI   CRBDATE+5,C'Y'                                                   
         B     CTD065                                                           
*                                                                               
         MVI   CRBDATE+5,C'O'      Out of range                                 
*                                                                               
CTD065   SAC   0                                                                
         GOTO1 VDATCON,DMCB,(6,DUB),(16,CRBDATE)                                
         SAC   512                                                              
*                                                                               
CTD066   SAC   0                                                                
         SAM24                                                                  
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),FULL                                                   
         GOTO1 ADDSTATE,DMCB,=C'ENQUIRE'                                        
         SAM31                                                                  
         SAC   512                                                              
         LAM   AR2,AR2,MYALET                                                   
         LA    R0,4                                                             
         LA    R1,CRSTATE                                                       
         LA    RE,DMCB+8                                                        
CTDS010  LARL  RF,STATETAB                                                      
CTDS011  CLC   0(1,RE),0(RF)                                                    
         JE    CTDS015                                                          
         LA    RF,3(RF)                                                         
         CLI   0(RF),X'FF'                                                      
         JNE   CTDS011                                                          
         DC    H'0'                                                             
*                                                                               
CTDS015  MVC   0(2,R1),1(RF)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,1(RE)                                                         
         JCT   R0,CTDS010                                                       
*                                                                               
CTD067   OC    DSPLOCK,DSPLOCK     TEST ACTIVE                                  
         BZ    *+14                                                             
         MVC   CRSTA,=C'Active'    ACTIVE OVERRIDES OTHER STATUS                
         B     CTD090                                                           
*                                                                               
         CLI   TABS,C'Y'           TABS DSP SHOWS LENGTH                        
         BNE   CTD080                                                           
*                                                                               
CTD070   ICM   R1,15,DSPTEND       CALCULATE LENGTH                             
         LA    R1,1(R1)                                                         
         ICM   RF,15,DSPTFRST                                                   
         SLL   RF,2                                                             
         SRL   RF,2                                                             
         SR    R1,RF                                                            
         C     R1,=F'1023'                                                      
         BH    CTD071                                                           
         EDIT  (R1),(6,CRSTA),TRAIL=C'b'                                        
         B     CTD090                                                           
*                                                                               
CTD071   SRL   R1,10               CONVERT TO K                                 
         C     R1,=F'1023'                                                      
         BH    CTD072                                                           
         EDIT  (R1),(6,CRSTA),TRAIL=C'k'                                        
         B     CTD090                                                           
*                                                                               
CTD072   SR    R0,R0               CONVERT TO MEG                               
         D     R0,=F'102'          DIV BY 102.4 ISH                             
         EDIT  (R1),(6,CRSTA),1,TRAIL=C'M'                                      
         B     CTD090                                                           
*                                                                               
CTD080   XC    SELDATA,SELDATA                                                  
         MVC   SELDATA(1),FULL+3                                                
         BAS   RE,FINDNUM          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     RF,SEFILES                                                       
         L     RF,0(RF)                                                         
         L     R1,8(RF)                                                         
         TM    DTFFLAG-DTF(R1),DTFGLOB                                          
         BNZ   *+14                                                             
         MVC   CRSTA,=C'Local '    TEST FOR LOCAL SYSTEM                        
         B     CTD090                                                           
*                                                                               
         ST    R2,FULL             SAVE R2                                      
         ICM   R2,7,DSPECB+1                                                    
         USING DMSYSHDR,R2                                                      
         TM    DSYSSTAT,DSYQOPEN   STATUS OPEN                                  
         BNO   *+10                                                             
         MVC   CRSTA,=C'Open  '                                                 
         TM    DSYSSTAT,DSYQOPEN+DSYQEXCL                                       
         BNO   *+10                                                             
         MVC   CRSTA,=C'Maint '                                                 
         TM    DSYSSTAT,DSYQSTOP   STATUS CLOSING                               
         BNO   *+10                                                             
         MVC   CRSTA,=C'Closin'                                                 
         TM    DSYSSTAT,DSYQSTOP+DSYQOPEN                                       
         BNZ   *+10                                                             
         MVC   CRSTA,=C'Closed'                                                 
         L     R2,FULL             RESTORE R2                                   
         USING DMSPACED,R2                                                      
*                                                                               
CTD090   EDIT  (B4,DSPLCNT),(10,CRLOCK),ZERO=NOBLANK                            
         EDIT  (B4,DSPWCNT),(7,CRWAIT),ZERO=NOBLANK                             
         EDIT  (B4,DSPLCNTO),(10,CRLOCKO),ZERO=NOBLANK                          
         EDIT  (B4,DSPWCNTO),(7,CRWAITO),ZERO=NOBLANK                           
*                                                                               
CTD140   LA    R7,DLLINEL(R7)      NEXT SCREEN LINE                             
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
CTDNXT   LA    R2,64(R2)                                                        
         CHI   R2,(255*64)         UP TO 255 SYSTEMS                            
         JL    CTD060                                                           
*                                                                               
CTDX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY DATASPACE LOCKTAB SUMMARY                  *                   
*************************************************************                   
         SPACE 1                                                                
LOCKDISP NTR1                                                                   
*                                                                               
         CLI   SELDATA,0           IS THIS FOR SPECIFIC SE                      
         BNE   LOCKDET             YES DO DETAILED LOCK SCREEN                  
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         MVC   AECBS,0(R2)                                                      
*                                                                               
         MVI   SVDISP,C'L'         SET LOCKTAB LIST                             
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
LOCKDS1  OC    SVTOP,SVTOP         TEST FOR SAVED VALUE                         
         BZ    DLS050                                                           
         SR    R1,R1               INDEX INTO HEADERS                           
         IC    R1,SVTOP                                                         
         MH    R1,=Y(L'DSPHDR)                                                  
         AR    R2,R1                                                            
         CHI   R2,(255*64)         UP TO 255 SYSTEMS                            
         JL    DLS050                                                           
         XR    R2,R2                                                            
         MVI   SVTOP,0                                                          
*                                                                               
DLS050   MVC   SRVHDR0,LSHEADR0                                                 
         MVC   SRVHDR,LSHEADER                                                  
         MVC   SRVHDR1,LSUNDER                                                  
*                                                                               
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING LSLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    DLS055                                                           
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
DLS055   ST    R7,CURSOR                                                        
         LA    R1,16               SET COUNT TO 16 LINES                        
*                                                                               
DLS060   LTR   R1,R1               TEST AND SAVE COUNT                          
         BZ    DLSX                                                             
         STC   R1,COUNT                                                         
         C     R2,AECBS                                                         
         BNL   DLSX                                                             
         LTR   R2,R2               FISRT ENTRY IS HEADERS                       
         BZ    DLSNXT                                                           
         OC    DSPNAME,DSPNAME                                                  
         BZ    DLSNXT                                                           
*                                                                               
         NI    LLINEH+1,255-X'08'  NORMAL OR                                    
*                                                                               
*        OI    LLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         MVC   LLINE,LSDFAULT      SET DEFAULT DATA                             
*                                                                               
         MVC   LRSRC,DSPNAME                                                    
         ST    R2,SAVER2           SAVE THIS THEN GET LOCKTAB                   
         SR    R1,R1                                                            
         ICM   R1,7,DSPECB+1                                                    
         LR    R2,R1                                                            
         ST    R2,ASYSHDR                                                       
         L     R2,DSYALOCK-DSYHDR(,R2)                                          
*                                                                               
         EDIT  (B2,00(R2)),(7,LRMAX),ZERO=NOBLANK                               
         EDIT  (B2,02(R2)),(7,LRACTV),ZERO=NOBLANK                              
         EDIT  (B2,04(R2)),(7,LRHIGH),ZERO=NOBLANK                              
         EDIT  (B4,12(R2)),(7,LRLOCK),ZERO=NOBLANK                              
         EDIT  (B4,08(R2)),(7,LRWAIT),ZERO=NOBLANK                              
*                                                                               
         L     R2,SAVER2           RESTORE R2                                   
*                                                                               
DLS100   LA    R7,DLLINEL(R7)      NEXT SCREEN LINE                             
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
DLSNXT   LA    R2,64(,R2)                                                       
         CHI   R2,(255*64)         UP TO 255 SYSTEMS                            
         JL    DLS060                                                           
*                                                                               
DLSX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY LOCKTAB DETAILED ENTRY                     *                   
*************************************************************                   
         SPACE 1                                                                
LOCKDET  BAS   RE,FINDSYS          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   SENUM,SESYS                                                      
         DROP  R6                                                               
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         SR    RF,RF                                                            
         IC    RF,SENUM                                                         
         SLL   RF,6                                                             
         LR    R2,RF                                                            
         ICM   RF,7,DSPECB+1                                                    
         LR    R2,RF                                                            
         ST    R2,ASYSHDR                                                       
         USING DMSYSHDR,R2                                                      
         MVC   AJOBTAB,DSYAJOBS    GET A(LOCKTAB)                               
         MVC   FULL,DSYABUFF                                                    
         L     R2,DSYALOCK                                                      
         DROP  R2                                                               
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,2(R2)          LOAD R0 WITH NUM LOCKS                       
         BZ    DTL022                                                           
         LA    R2,16(R2)           POINT TO FIRST ENTRY                         
*                                                                               
DTL010   OC    0(8,R2),0(R2)       IGNORE ZERO ENTRIES                          
         BZ    DTL020                                                           
*                                                                               
         LA    R1,16               LOCKINFO HOLDS 16                            
         LA    R7,LOCKINFO         BUILD INFO TABLE HERE                        
DTL011   OC    0(3,R7),0(R7)                                                    
         BZ    DTL015              FREE SPACE                                   
         CLC   1(3,R2),0(R7)                                                    
         BE    DTL015              MY ENTRY                                     
         LA    R7,18(R7)                                                        
         BCT   R1,DTL011           TRY ALL 16                                   
         B     DTL020                                                           
*                                                                               
DTL015   MVC   0(3,R7),1(R2)       SAVE JOB OR ADV/T# INFO                      
         LA    R1,5                                                             
         LR    RF,R7                                                            
DTL016   LA    RF,3(RF)            BUMP THROUGH FILE TABLE                      
         CLI   0(RF),0                                                          
         BE    DTL017              FREE SPACE                                   
         CLC   0(1,R2),0(RF)                                                    
         BE    DTL017              MY ENTRY                                     
         BCT   R1,DTL016                                                        
         B     DTL020                                                           
*                                                                               
DTL017   SR    R1,R1               BUMP LOCK COUNT FOR THIS FILE                
         MVC   0(1,RF),0(R2)       SAVE FILENUM                                 
         ICM   R1,3,1(RF)                                                       
         LA    R1,1(R1)            ADD 1 TO COUNTER                             
         STCM  R1,3,1(RF)                                                       
         B     DTL021                                                           
*                                                                               
DTL020   LA    R2,8(,R2)           NEXT LOCKTAB ENTRY                           
         C     R2,FULL             DON'T RUN OFF END                            
         BNL   DTL022                                                           
         B     DTL010                                                           
*                                                                               
DTL021   LA    R2,8(,R2)           NEXT LOCKTAB ENTRY                           
         C     R2,FULL             DON'T RUN OFF END                            
         BNL   DTL022                                                           
         BCT   R0,DTL010           UNTIL ALL CHECKED                            
*                                                                               
DTL022   SAM24 ,                                                                
         SAC   0                                                                
*                                                                               
         LA    R2,LOCKINFO                                                      
         MVI   SVDISP,C'I'         SET DETAILED LOCKS (INDIVIDUAL)              
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
DTL050   MVC   SRVHDR0,LDHEADR0                                                 
         MVC   SRVHDR,LDHEADER                                                  
         MVC   SRVHDR1,LDUNDER                                                  
*                                                                               
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING LSLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    DTL055                                                           
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
DTL055   ST    R7,CURSOR                                                        
         LA    R0,16               SET COUNT TO 16 LINES                        
*                                                                               
DTL060   LTR   R0,R0               TEST AND SAVE COUNT                          
         BZ    DTLX                                                             
         STC   R0,COUNT                                                         
*                                                                               
         NI    LLINEH+1,255-X'08'  NORMAL INTENSITY                             
         MVC   LLINE,LSDFAULT      SET DEFAULT DATA                             
*                                                                               
         OC    0(4,R2),0(R2)       IGNORE NULL ENTRIES                          
         BZ    DTL100                                                           
         MVC   FULL,0(R2)          JOB DETAIL                                   
         MVI   FULL+3,0                                                         
         ST    R0,SAVER0                                                        
         BAS   RE,DISJOB           SHOW JOB NUM OR ADV/TSK                      
         L     R0,SAVER0                                                        
         MVC   LRSRC,DUB1                                                       
         LA    R6,LRSRC+9          FIRST SCREEN ENTRY                           
*                                                                               
         ST    R0,SAVER0                                                        
         ST    R2,SAVER2                                                        
         LA    R2,3(R2)            FIRST FILE ENTRY                             
         LA    R3,5                PRINT F# NNNNN                               
DTL070   CLI   0(R2),0                                                          
         BE    DTL071                                                           
         GOTO1 VHEXOUT,DMCB,0(R2),0(R6),1                                       
         EDIT  (B2,1(R2)),(4,3(R6))                                             
DTL071   LA    R6,10(R6)                                                        
         LA    R2,3(R2)                                                         
         BCT   R3,DTL070                                                        
         L     R0,SAVER0                                                        
         L     R2,SAVER2                                                        
*                                                                               
DTL100   LA    R7,DLLINEL(R7)      NEXT SCREEN LINE                             
         SR    RF,RF                                                            
         IC    RF,COUNT                                                         
         BCTR  RF,0                                                             
         LR    R1,RF               SAVE COUNT IN R1                             
*                                                                               
         SLL   RF,3                SAVE DATA FOR SELECT                         
         LA    RF,SVSEL(RF)                                                     
         MVI   0(RF),0             SPACE FOR SUB ACTION                         
         MVC   1(4,RF),FULL                                                     
*                                                                               
DTLNXT   LA    R2,18(R2)           NEXT LOCKINFO                                
         BCT   R0,DTL060           UNTIL ALL CHECKED                            
*                                                                               
DTLX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        DISPLAY DATASPACE WAITTAB SUMMARY                  *                   
*************************************************************                   
         SPACE 1                                                                
WAITDISP NTR1                                                                   
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         MVC   AECBS,0(R2)                                                      
         L     R2,AECBS                                                         
*                                                                               
         MVI   SVDISP,C'W'         SET WAITTAB LIST                             
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
WAITDS1  OC    SVTOP,SVTOP         TEST FOR SAVED VALUE                         
         BZ    WAI050                                                           
         SR    R1,R1               INDEX INTO HEADERS                           
         IC    R1,SVTOP                                                         
         SLL   R1,3                                                             
         AR    R2,R1                                                            
*                                                                               
WAI050   MVC   SRVHDR0,WAHEADR0                                                 
         MVC   SRVHDR,WAHEADER                                                  
         MVC   SRVHDR1,WAUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING WALINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    WAI055                                                           
*                                                                               
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
WAI055   ST    R7,CURSOR                                                        
         ICM   R5,15,0(R2)         TEST AND SAVE COUNT                          
         BZ    WAIX                                                             
         LA    R4,16               SET COUNT TO 16 LINES                        
         LA    R2,16(,R2)                                                       
*                                                                               
WAI060   OC    0(16,R2),0(R2)      IGNORE ZERO ENTRIES                          
         BNZ   *+12                                                             
         LA    R2,16(,R2)                                                       
         B     WAI060                                                           
*                                                                               
         NI    WLINEH+1,255-X'08'  NORMAL OR                                    
*                                                                               
*        OI    WLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         MVC   WLINE,WADFAULT      SET DEFAULT DATA                             
*                                                                               
         MVC   FULL,0(R2)          OWNER DETAIL                                 
         MVI   FULL+3,0                                                         
         BAS   RE,DISJOB                                                        
         MVC   WAOWN,DUB1                                                       
*                                                                               
         MVC   FULL,4(R2)          REQUESTOR DETAIL                             
         MVI   FULL+3,0                                                         
         BAS   RE,DISJOB                                                        
         MVC   WAREQ,DUB1                                                       
*                                                                               
         MVC   WASTAT,=C'Waiting for'                                           
         TM    12(R2),X'40'                                                     
         BZ    *+10                                                             
         MVC   WASTAT,=C'Posted by  '                                           
*                                                                               
WAI100   LA    R7,DLLINEL(R7)      NEXT SCREEN LINE                             
*                                                                               
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         ST    R2,SVSEL(RF)        SAVE ADDR OF ECB                             
*                                                                               
         BCTR  R4,0                COUNT DOWN LINES                             
         LTR   R4,R4                                                            
         BZ    WAIX                                                             
*                                                                               
WAINXT   LA    R2,16(,R2)          NEXT ECB                                     
         BCT   R5,WAI060                                                        
*                                                                               
WAIX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        DISPLAY JOB TABLE FOR A SYSTEM                    *                    
************************************************************                    
         SPACE 1                                                                
JOBDISP  NTR1                                                                   
*                                                                               
         BAS   RE,FINDSYS          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   SENUM,SESYS                                                      
         DROP  R6                                                               
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         SR    RF,RF                                                            
         IC    RF,SENUM                                                         
         SLL   RF,6                                                             
         LR    R2,RF                                                            
         ICM   RF,7,DSPECB+1                                                    
         LR    R2,RF                                                            
         ST    R2,ASYSHDR                                                       
         USING DMSYSHDR,R2                                                      
         L     R2,DSYAJOBS                                                      
         USING DSJOBHDR,R2                                                      
*                                                                               
         MVI   SVDISP,C'J'         SET JOBTAB LIST                              
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
         OC    SVTOP,SVTOP         TEST FOR SAVED VALUE                         
         BZ    JOB050                                                           
         XR    R1,R1                                                            
         ICM   R1,1,SVTOP                                                       
         CHI   R1,DSJBHMXQ-16      MAX POS FOR SCROLLING                        
         JNH   *+8                                                              
         LA    R1,DSJBHMXQ-16                                                   
         STC   R1,SVTOP                                                         
         SLL   R1,4                                                             
         AR    R2,R1                                                            
*                                                                               
JOB050   MVC   SRVHDR0,SYHEADR0                                                 
         MVC   SRVHDR,SYHEADER                                                  
         MVC   SRVHDR1,SYUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING SYLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    JOB055                                                           
*                                                                               
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
JOB055   ST    R7,CURSOR                                                        
         LA    R4,16               SET COUNT TO 16 LINES                        
*                                                                               
JOB060   MVC   SLINE,SYDFAULT      SET DEFAULT DATA                             
*                                                                               
         NI    SLINEH+1,255-X'08'  NORMAL OR                                    
*                                                                               
*        OI    SLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         LA    R1,17               SET SLOT NUMBER                              
         SR    RF,RF                                                            
         IC    RF,SVTOP                                                         
         SR    R1,R4                                                            
         AR    R1,RF                                                            
         EDIT  (R1),(3,SYJSLOT+1)                                               
*                                                                               
         OC    0(16,R2),0(R2)      IGNORE ZERO ENTRIES                          
         BZ    JOB100                                                           
*                                                                               
         MVC   SYJNAME,DSJOBNAM    JOB NAME                                     
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSJOBNUM    JOB NUMBER                                   
         MVI   FULL+3,0                                                         
         MVI   FULL+2,X'80'                                                     
         BAS   RE,DISJOB                                                        
         MVC   SYJNUM,DUB1                                                      
*                                                                               
         CLI   DSJOBADV,0                                                       
         BE    JOBS070                                                          
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSJOBNUM    JOB NUMBER                                   
         MVC   FULL+3(1),DSJOBADV                                               
         BAS   RE,DISJOB                                                        
         MVC   SYJADV,DUB1                                                      
*                                                                               
JOBS070  SR    R1,R1                                                            
         ICM   R1,3,DSJTIME                                                     
         SLL   R1,8                                                             
         ST    R1,FULL                                                          
         BAS   RE,TIMEOUT                                                       
         MVC   SYJTIME(8),WORK1    HH:MM:SS                                     
*                                                                               
         GOTO1 VHEXOUT,DMCB,DSJOASID,SYJASID,2                                  
*                                                                               
         TM    DSJOBFLG,DSJOBUPQ                                                
         BZ    *+10                                                             
         MVC   SYJSTAT,=CL10'UPDATING'                                          
*                                                                               
         TM    DSJOBFLG,DSJOBROQ                                                
         BZ    *+10                                                             
         MVC   SYJSTAT,=CL10'READ/ONLY'                                         
*                                                                               
         TM    DSJOBFLG,DSJOBSHQ                                                
         BZ    *+10                                                             
         MVC   SYJSTAT,=CL10'SHARED   '                                         
*                                                                               
         TM    DSJOBFLG,DSJOBMAQ                                                
         BZ    *+10                                                             
         MVC   SYJSTAT,=CL10'MAINT '                                            
*                                                                               
         MVC   SYJRCV,=C'YES'                                                   
         TM    DSJOBFLG,DSJOBRCV                                                
         BO    *+10                                                             
         MVC   SYJRCV,=C'NO '                                                   
*                                                                               
JOB100   LA    R7,SYLINEL(R7)      NEXT SCREEN LINE                             
*                                                                               
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         ST    R2,SVSEL(RF)        SAVE ADDR OF ECB                             
*                                                                               
JOBNXT   LA    R2,16(,R2)          NEXT JOB                                     
         BCT   R4,JOB060                                                        
*                                                                               
JOBX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        DISPLAY COMM TABLE                                *                    
************************************************************                    
         SPACE 1                                                                
COMDISP  NTR1                                                                   
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         L     R2,DHACOMM                                                       
         USING DSCOMM,R2                                                        
*                                                                               
         MVI   SVDISP,C'C'         SET COMTAB LIST                              
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,SVTOP          TEST FOR SAVED VALUE                         
         BZ    COM050                                                           
         SLL   R1,5                                                             
         AR    R2,R1                                                            
*                                                                               
COM050   MVC   SRVHDR0,COHEADR0                                                 
         MVC   SRVHDR,COHEADER                                                  
         MVC   SRVHDR1,COUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING COLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    COM055                                                           
*                                                                               
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
COM055   ST    R7,CURSOR                                                        
         LA    R4,16               SET COUNT TO 16 LINES                        
*                                                                               
COM060   MVC   COLINE,CODFAULT     SET DEFAULT DATA                             
*                                                                               
         NI    COLINEH+1,255-X'08' NORMAL OR                                    
*                                                                               
*        OI    COLINEH+1,X'08'     HI INTENSITY                                 
*                                                                               
         OC    0(32,R2),0(R2)      IGNORE ZERO ENTRIES                          
         BZ    COM100                                                           
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSCSORC     DISPLAY ADV SOURCE                           
         BAS   RE,DISJOB                                                        
         MVC   COORG,DUB1                                                       
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSCDEST     DISPLAY ADV DEST                             
         BAS   RE,DISJOB                                                        
         MVC   CODEST,DUB1                                                      
*                                                                               
         MVC   FULL,DSCTIME                                                     
         BAS   RE,TIMEOUT                                                       
         MVC   COTIME(8),WORK1     HH:MM:SS                                     
         MVC   COCOMM,SPACES                                                    
*                                                                               
         LA    R1,COMTAB           SCAN COMTAB FOR ACTION                       
COM070   CLC   DSCCOMM,8(R1)                                                    
         BE    COM075                                                           
         LA    R1,16(R1)                                                        
         CLI   8(R1),X'FF'                                                      
         BNE   COM070                                                           
COM075   MVC   COCOMM(8),0(R1)                                                  
*                                                                               
         MVC   SELDATA(1),DSCDATA+1                                             
         BAS   RE,FINDNUM          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   COCOMM+8(7),SENAME  SHOW SYSTEM NAME                             
*                                                                               
         MVC   FILNUM,DSCDATA+2                                                 
         CLI   FILNUM,0                                                         
         BE    COM100                                                           
         BAS   RE,FINDFIL                                                       
         L     R6,ADTF                                                          
         MVC   COCOMM+16(8),22(R6)                                              
*                                                                               
COM100   LA    R7,SYLINEL(R7)      NEXT SCREEN LINE                             
*                                                                               
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         ST    R2,SVSEL(RF)        SAVE ADDR OF ECB                             
*                                                                               
COMNXT   LA    R2,32(,R2)          NEXT COM                                     
         BCT   R4,COM060                                                        
*                                                                               
COMX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        DISPLAY VERMONT RECOVERY HEADER                   *                    
************************************************************                    
         SPACE 1                                                                
RECODISP NTR1                                                                   
*                                                                               
         BAS   RE,FINDSYS          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   SENUM,SESYS                                                      
         DROP  R6                                                               
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         SR    RF,RF                                                            
         IC    RF,SENUM                                                         
         SLL   RF,6                                                             
         LR    R2,RF                                                            
         ICM   RF,7,DSPECB+1                                                    
         LR    R2,RF                                                            
         ST    R2,ASYSHDR                                                       
         USING DMSYSHDR,R2                                                      
         MVC   AJOBTAB,DSYAJOBS                                                 
         L     R2,DSYABUFF                                                      
         DROP  R2                                                               
*                                                                               
         MVI   SVDISP,C'R'         SET RECOVERY LIST                            
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
RECO050  MVC   SRVHDR0,RVHEADR0                                                 
         MVC   SRVHDR,RVHEADER                                                  
         MVC   SRVHDR1,RVUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING RCLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    RECO055                                                          
*                                                                               
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
RECO055  ST    R7,CURSOR                                                        
         LA    R4,16               SET COUNT TO 10 LINES                        
         LA    R2,16(,R2)                                                       
*                                                                               
RECO060  MVC   RLINE,RVDFAULT      SET DEFAULT DATA                             
*                                                                               
         NI    RLINEH+1,255-X'08'  NORMAL OR                                    
*                                                                               
*        OI    RLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         OC    0(8,R2),0(R2)       IGNORE ZERO ENTRIES                          
         BZ    RECO100                                                          
*                                                                               
RECO070  LA    R0,DSJBHMXQ         MAX JOB TABLE ENTRIES                        
         MVC   DUB,0(R2)           SAVE ENTRY                                   
         ST    R2,FULL             SAVE A(ENTRY)                                
         L     R2,AJOBTAB                                                       
RECO071  CLI   DUB,X'FF'                                                        
         BNE   *+18                                                             
         CLC   8(2,R2),DUB+2       TEST JOBNUM MATCH                            
         BE    RECO075                                                          
         BNE   *+14                                                             
         CLC   10(1,R2),DUB        TEST ADV NUM MATCH                           
         BE    RECO075                                                          
         LA    R2,16(,R2)                                                       
         BCT   R0,RECO071                                                       
         MVC   RCJOB,=C'UNKNOWN '                                               
         B     *+10                                                             
*                                                                               
RECO075  MVC   RCJOB,0(R2)                                                      
*                                                                               
         CLI   DUB,X'FF'                                                        
         BE    RECO076                                                          
         EDIT  (B3,DUB+1),(8,RCSIN),DUB=DUB1                                    
         B     RECO077                                                          
*                                                                               
RECO076  MVC   RCSIN,=C'Job     '   JOB NO                                      
         SR    R1,R1                                                            
         ICM   R1,3,DUB+2                                                       
         N     R1,=X'0000FFFF'                                                  
         EDIT  (R1),(5,RCSIN+3),FILL=0,DUB=DUB1                                 
*                                                                               
RECO077  SAC   0                                                                
         GOTO1 VHEXOUT,DMCB,DUB+4,RCDA,4                                        
         SAC   512                                                              
         L     R2,FULL                                                          
*                                                                               
RECO100  LA    R7,RCLINEL(R7)      NEXT SCREEN LINE                             
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         ST    R2,SVSEL(RF)        SAVE ADDR OF ECB                             
*                                                                               
RECONXT  LA    R2,8(,R2)           NEXT RECO                                    
         BCTR  R4,0                                                             
         C     R4,=F'6'                                                         
         BH    RECO060                                                          
*                                                                               
RECOX    CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        DISPLAY FILE TABLE FOR A SYSTEM                   *                    
************************************************************                    
         SPACE 1                                                                
FILDISP  NTR1                                                                   
*                                                                               
         BAS   RE,FINDSYS          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   SENUM,SESYS                                                      
         L     R6,SEFILES          POINT R6 TO FILES                            
         L     R6,0(R6)                                                         
         DROP  R6                                                               
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         SR    R2,R2                                                            
         USING DMSPACED,R2                                                      
         ST    R2,ADSPACE                                                       
         SR    RF,RF                                                            
         IC    RF,SENUM                                                         
         SLL   RF,6                                                             
         LR    R2,RF                                                            
         ICM   RF,7,DSPECB+1                                                    
         LR    R2,RF                                                            
         ST    R2,ASYSHDR                                                       
         USING DMSYSHDR,R2                                                      
         LH    R5,DSYFILES                                                      
         CH    R5,=H'16'                                                        
         BL    *+8                                                              
         LA    R5,16                                                            
         LA    R4,16                                                            
         LA    R2,DSYDATA                                                       
         USING DSFILHDR,R2                                                      
*                                                                               
         MVI   SVDISP,C'F'         SET FILTAB LIST                              
         MVC   CHKSEL(SVSELL),SVSEL                                             
         XC    SVSEL(SVSELL),SVSEL                                              
*                                                                               
FIL050   MVC   SRVHDR0,FLHEADR0                                                 
         MVC   SRVHDR,FLHEADER                                                  
         MVC   SRVHDR1,FLUNDER                                                  
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING FLLINED,R7                                                       
         TM    DFLAG,DFLGDAT       TEST DATA ON SCREEN                          
         BZ    FIL055                                                           
*                                                                               
         GOTO1 TWAXC,(R7)          TWAXC IT OFF                                 
FIL055   ST    R7,CURSOR                                                        
*                                                                               
FIL060   LA    RE,4(R6)            RE=START OF FILE LIST                        
*                                                                               
FIL061   CLC   DSFILNUM,3(RE)                                                   
         BE    FIL070                                                           
         LA    RE,8(RE)                                                         
         B     FIL061                                                           
*                                                                               
FIL070   L     RE,4(RE)            SET RE TO DTF                                
*                                                                               
         NI    FLINEH+1,255-X'08'  NORMAL OR                                    
*                                                                               
*        OI    FLINEH+1,X'08'      HI INTENSITY                                 
*                                                                               
         MVC   FLINE,FLDFAULT      SET DEFAULT DATA                             
*                                                                               
         MVC   FLNAME,22(RE)       FILE NAME                                    
         MVC   BYTE1,20(RE)        SAVE FLAG                                    
*                                                                               
FIL080   CLI   DSFILACT,0          ANY ACTIONS BEING PERFORMED                  
         BE    FIL090                                                           
         LA    R1,FACTION                                                       
FIL081   CLC   DSFILACT,0(R1)                                                   
         BE    FIL082                                                           
         LA    R1,9(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   FIL081                                                           
FIL082   MVC   FLMAINT,1(R1)                                                    
*                                                                               
FIL090   MVC   BYTE,DSFILNUM       FILE NUMBER                                  
         MVC   DUB+0(4),DSEOF1                                                  
         MVC   DUB+4(4),DSEOF2                                                  
         SAC   0                                                                
         GOTO1 VHEXOUT,DMCB,BYTE,FLNUM,1                                        
         TM    BYTE1,X'10'                                                      
         BO    FIL095                                                           
         GOTO1 (RF),(R1),DUB+0,FLDNEXT,4                                        
         GOTO1 (RF),(R1),DUB+4,FLDCOUNT,2                                       
         B     FIL099                                                           
*                                                                               
FIL095   GOTO1 (RF),(R1),DUB+0,FLOVLAST,4                                       
         GOTO1 (RF),(R1),DUB+4,FLPDLAST,4                                       
FIL099   SAC   512                                                              
*                                                                               
         SR    R1,R1               ANY DSNAME INDEX                             
         ICM   R1,3,DSDSNX                                                      
         BZ    FIL100                                                           
*                                                                               
         ST    R2,FULL             SAVE CURRENT POSITION                        
         SR    R2,R2                                                            
         ICM   R2,15,DHADSN-DMDSHDR(R2)                                         
         BNZ   *+12                                                             
         L     R2,FULL                                                          
         B     FIL100                                                           
         BCTR  R1,0                                                             
         SLL   R1,5                                                             
         AR    R2,R1                                                            
         MVC   FLDSNAME,0(R2)      SHOW FILENAME                                
         L     R2,FULL                                                          
*                                                                               
FIL100   LA    R7,FLLINEL(R7)      NEXT SCREEN LINE                             
*                                                                               
         LR    RF,R4                                                            
         BCTR  RF,0                                                             
         SLL   RF,3                                                             
         ST    R2,SVSEL(RF)        SAVE ADDR OF file                            
*                                                                               
FILNXT   LA    R2,32(,R2)          NEXT FILE                                    
         BCTR  R4,0                                                             
         BCT   R5,FIL060                                                        
*                                                                               
FILX     CLC   CHKSEL(SVSELL),SVSEL                                             
         BNE   *+8                                                              
         OI    DFLAG,DFLGSAME      SAME SCREEN AS LAST TIME                     
         OI    DFLAG,DFLGDAT       SET  DATA ON SCREEN                          
*                                                                               
         DROP  R2                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
************************************************************                    
*        DISPLAY ADV/TASK OR JOB NUMBER INFO               *                    
*        FULL=AL2(NN),JOBFLG,ADVN                          *                    
************************************************************                    
         SPACE 1                                                                
DISJOB   TM    FULL+2,X'80'        TEST OFFLINE JOB                             
         BO    DJOB085                                                          
         CLI   FULL+3,0                                                         
         BE    *+14                                                             
         MVC   FULL+0(1),FULL+3                                                 
         MVI   FULL+1,0                                                         
         MVC   BYTE,FULL+0                                                      
         L     R1,AFID                                                          
         NI    BYTE,X'0F'                                                       
DJOB081  CLC   BYTE,4(R1)          FIND ADV SYSTEM                              
         BE    DJOB082                                                          
         LA    R1,8(R1)                                                         
         CLI   5(R1),X'FF'         CHECK EOT                                    
         BNE   DJOB081                                                          
         DC    H'0'                                                             
DJOB082  MVC   DUB1(4),0(R1)                                                    
         CLI   DUB1+3,C' '                                                      
         BE    *+14                                                             
         MVC   DUB1+2(1),DUB1+3                                                 
         MVI   DUB1+3,C' '                                                      
*                                                                               
         MVC   DUB1+4(2),=C'/#'                                                 
         MVC   DUB1+6(1),FULL+1                                                 
*                                                                               
         MVC   FULL+3(1),FULL+0                                                 
*                                                                               
         TM    FULL+3,X'F0'                                                     
         BZR   RE                                                               
         SR    R1,R1                                                            
         IC    R1,FULL+3                                                        
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,DUB1+3                                                        
         BR    RE                                                               
*                                                                               
DJOB085  MVC   DUB1,=C'J       '   JOB NO                                       
         SR    R1,R1                                                            
         ICM   R1,3,FULL                                                        
         N     R1,=X'0000FFFF'                                                  
         EDIT  (R1),(5,DUB1+1),FILL=0                                           
         BR    RE                                                               
*************************************************************                   
*        FIND ASENTRY FROM SELDATA=NAME                     *                   
*************************************************************                   
         SPACE 1                                                                
FINDSYS  NTR1                                                                   
         L     R6,ASELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
         SR    R1,R1                                                            
         IC    R1,SELDATA                                                       
FINDS10  EX    R1,*+8              FIND SELECTED SYS                            
         B     *+10                                                             
         CLC   SENAME(0),SELDATA+1                                              
         BE    FINDSX                                                           
*                                                                               
         BXLE  R6,R4,FINDS10                                                    
         LTR   RB,RB               SET CC NEQ (NOT FOUND)                       
         B     XIT1                                                             
FINDSX   ST    R6,ASENTRY          SAVE ASENTRY                                 
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        FIND ASENTRY FROM SELDATA=NUMBER                   *                   
*************************************************************                   
         SPACE 1                                                                
FINDNUM  NTR1                                                                   
         L     R6,ASELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
FINDN10  CLC   SESYS,SELDATA       FIND SELECTED SYS                            
         BE    FINDNX                                                           
*                                                                               
         BXLE  R6,R4,FINDN10                                                    
         LTR   RB,RB               SET CC NEQ (NOT FOUND)                       
         B     XIT1                                                             
FINDNX   ST    R6,ASENTRY          SAVE ASENTRY                                 
         B     XIT1                                                             
         EJECT                                                                  
*********************************************************************           
*        GET A(DTF) FOR RECOVERY FILE OF SENUM                      *           
*********************************************************************           
         SPACE 1                                                                
GETDTF   LR    RF,R1                                                            
*                                                                               
GETD020  SR    R1,R1               FIND RECOVERY FILE                           
         ICM   R1,3,2(RF)                                                       
         LA    RF,4(RF)                                                         
*                                                                               
GETD030  TM    0(RF),X'40'         TEST RECOVERY                                
         BO    GETD040                                                          
         LA    RF,8(RF)                                                         
         BCT   R1,GETD030                                                       
         DC    H'0'                                                             
*                                                                               
GETD040  SR    R1,R1               RETURN A(DTF) IN R1                          
         ICM   R1,7,5(RF)                                                       
         BR    RE                                                               
         EJECT                                                                  
*************************************************************                   
*        FIND ADTF FOR FILE IN FILNUM                       *                   
*************************************************************                   
         SPACE 1                                                                
FINDFIL  NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R4,SEFILES                                                       
         L     R4,0(R4)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,2(R4)                                                       
         LA    R4,4(R4)                                                         
*                                                                               
FINDF01  CLC   FILNUM,3(R4)                                                     
         BE    FINDF02                                                          
         LA    R4,8(R4)                                                         
         BCT   R0,FINDF01                                                       
         LA    R6,SPACES                                                        
         B     XIT1                                                             
*                                                                               
FINDF02  SR    R6,R6                                                            
         ICM   R6,7,5(R4)                                                       
         ST    R6,ADTF                                                          
*                                                                               
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
         GOTO1 ADATAMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R2),SRSD                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   SRCOMWRK(4),DISP    TEST FOR MY ID                               
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
         MVC   0(4,R1),DISP                                                     
         MVC   4(SAVEDL,R1),SAVEDSTR                                            
         LA    R2,SRPAGENO                                                      
         SLL   R2,32-8                                                          
         ICM   R2,3,TRM                                                         
         GOTO1 ADATAMGR,DMCB,DMWRT,TEMPSTR,(R2),SRSD                            
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
*        SELECT AN ENTRY                                    *                   
*************************************************************                   
         SPACE 1                                                                
SELECT   NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
         TM    SFLAG,SFLGSLCT                                                   
         BO    XIT1                ALREADY GOT A SELECT                         
         XC    SELDATA,SELDATA                                                  
         MVC   SELDATA(1),4(R1)                                                 
         BAS   RE,FINDNUM          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   SRVP2(7),SENAME                                                  
         MVI   SRVP2H+5,5                                                       
         OI    SFLAG,SFLGSLCT                                                   
         CLI   0(R1),1                                                          
         BNE   *+10                                                             
         MVC   SRVP1(8),=C'JOBS    '                                            
         CLI   0(R1),2                                                          
         BNE   *+10                                                             
         MVC   SRVP1(8),=C'FILES   '                                            
         CLI   0(R1),3                                                          
         BNE   *+10                                                             
         MVC   SRVP1(8),=C'RCVR    '                                            
         CLI   0(R1),4                                                          
         BNE   *+10                                                             
         MVC   SRVP1(8),=C'LOCKS   '                                            
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        ADJUST A BILLDATE                                  *                   
*************************************************************                   
         SPACE 1                                                                
BILLDATE NTR1                                                                   
         MVC   BYTE,0(R1)                                                       
         LLC   R2,4(R1)                                                         
         LAM   AR2,AR2,MYALET                                                   
         SLL   R2,6                                                             
         SAC   512                                                              
         ICM   R2,15,60(R2)                                                     
         BZ    BILLDATX                                                         
         SLL   R2,2                                                             
         SRL   R2,2                                                             
         USING DMSYSHDR,R2                                                      
         MVC   DUB(4),DSYBDATE                                                  
         SAC   0                                                                
         GOTO1 VDATCON,DMCB,(6,DUB),(0,DUB1)                                    
*                                                                               
BILLD010 CLI   BYTE,5                                                           
         BNE   BILLD020                                                         
         GOTO1 VADDAY,DMCB,(C'D',DUB1),DUB1,F'1'                                
*                                                                               
BILLD020 CLI   BYTE,6                                                           
         BNE   BILLD030                                                         
         GOTO1 VADDAY,DMCB,(C'D',DUB1),DUB1,F'-1'                               
*                                                                               
BILLD030 GOTO1 VDATCON,DMCB,(0,DUB1),(15,DUB)                                   
         SAC   512                                                              
         MVC   DSYBDATE,DUB                                                     
BILLDATX SAC   0                                                                
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
         MVC   DMCB+2(2),FULL+2                                                 
         GOTO1 ALOCKSPC,DMCB       LOCK OR FREE RESOURCE                        
         TM    DMCB+4,0                                                         
         BE    XIT1                                                             
         B     ERR16               REPORT ERRORS                                
         EJECT                                                                  
*************************************************************                   
*        CLEAR THE LOCKTAB FOR A SYSTEM                     *                   
*************************************************************                   
         SPACE 1                                                                
LCLEAR   NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         MVC   FULL,1(R1)          RESOURCE                                     
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVI   DMCB,4              GIVE UP ON LONG LOCKS                        
         MVC   DMCB+2(2),FULL+2                                                 
         GOTO1 ALOCKSPC,DMCB       LOCK RESOURCE                                
         TM    DMCB+4,0                                                         
         BNE   ERR16               REPORT ERRORS                                
         L     R1,DMCB+4                                                        
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         L     R2,DSPECB-DMSPACED(R1)                                           
         USING DMSYSHDR,R2                                                      
         L     R2,DSYALOCK         PICK UP LOCKTAB                              
*                                                                               
         XC    2(2,R2),2(R2)       ZERO NUMBER OF LOCKS                         
         SR    R3,R3                                                            
         ICM   R3,3,0(R2)          R3=MAX LOCKS                                 
         SLL   R3,3                R3=MAX LOCKS * 8                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R2,16(,R2)                                                       
         MVCL  R2,RE               ZAP ALL LOCKS TO ZERO                        
         SAM24 ,                                                                
         SAC   0                                                                
*                                                                               
         MVI   DMCB,X'10'          FREE SYSTEM                                  
         MVC   DMCB+2(2),FULL+2                                                 
         GOTO1 ALOCKSPC,DMCB       UNLOCK RESOURCE                              
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLEAR THE LOCKTAB OF A JOB / TASK                  *                   
*************************************************************                   
         SPACE 1                                                                
ICLEAR   NTR1                                                                   
         BAS   RE,FINDSYS          FIND SE ENTRY                                
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),SESYS                                                  
         DROP  R6                                                               
         MVC   BYTE,0(R1)          ACTION                                       
         MVC   FULL,1(R1)          RESOURCE (LOCK ID)                           
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVI   DMCB,4              GIVE UP ON LONG LOCKS                        
         MVC   DMCB+2(2),HALF                                                   
         GOTO1 ALOCKSPC,DMCB       LOCK RESOURCE                                
         TM    DMCB+4,0                                                         
         BNE   ERR16               REPORT ERRORS                                
         L     R1,DMCB+4                                                        
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
         L     R2,DSPECB-DMSPACED(R1)                                           
         USING DMSYSHDR,R2                                                      
         L     R2,DSYALOCK         PICK UP LOCKTAB                              
         ST    R2,SAVER2                                                        
*                                                                               
         SR    R1,R1               LOCK COUNTER                                 
         SR    R0,R0               TOTAL LOCKS                                  
         ICM   R0,3,0(R2)                                                       
         LA    R2,16(R2)                                                        
*                                                                               
ICL010   OC    0(8,R2),0(R2)       ANYTHING HERE                                
         BZ    ICL030                                                           
         CLC   1(3,R2),FULL        DO WE WANT TO CLEAR THESE                    
         BNE   *+14                                                             
         XC    0(8,R2),0(R2)       YES CLEAR IT THEN                            
         B     ICL030                                                           
*                                                                               
         LA    R1,1(R1)            ELSE BUMP LOCK COUNTER                       
*                                                                               
ICL030   LA    R2,8(R2)                                                         
         BCT   R0,ICL010                                                        
         L     R2,SAVER2                                                        
         STCM  R1,3,2(R2)          SAVE NEW COUNT                               
         SAM24 ,                                                                
         SAC   0                                                                
*                                                                               
         MVI   DMCB,X'10'          FREE SYSTEM                                  
         MVC   DMCB+2(2),HALF                                                   
         GOTO1 ALOCKSPC,DMCB       UNLOCK RESOURCE                              
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        POST A WAIT TABLE ENTRY                            *                   
*************************************************************                   
         SPACE 1                                                                
WPOST    NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),1(R1)     ECB                                          
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
*                                                                               
         L     R2,FULL                                                          
         OI    12(R2),X'40'        POST THE ECB                                 
*                                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        POST A FACPAK OPER ECB                             *                   
*************************************************************                   
         SPACE 1                                                                
JPOST    NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),1(R1)     A(JOB)                                       
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
*                                                                               
         L     R2,FULL                                                          
         USING DSJOBHDR,R2                                                      
*                                                                               
         CLI   BYTE,X'41'          POST JOB                                     
         BE    JPOST005                                                         
         CLI   BYTE,X'42'          CLEAR JOB                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    0(16,R2),0(R2)      CLEAR IT                                     
*                                                                               
         PUSH  USING                                                            
         L     R2,ASYSHDR          CLEAR THE EXCLUSIVE FLAG TOO                 
         USING DMSYSHDR,R2                                                      
         NI    DSYSSTAT,255-DSYQEXCL                                            
         B     JPOSTX                                                           
         POP   USING                                                            
*                                                                               
JPOST005 OC    DSJOBADV,DSJOBADV   IS THIS AN ADV                               
         BZ    JPOSTX                                                           
*                                                                               
         MVC   BYTE,DSJOBADV                                                    
         SR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         L     R2,DHAADVS                                                       
*                                                                               
         USING DSJOBD,R2                                                        
         LA    R1,DSJOBMXQ                                                      
JPOST010 CLC   BYTE,DSJADV                                                      
         BE    JPOST020                                                         
         LA    R2,DSJOBLNQ(,R2)                                                 
         BCT   R1,JPOST010                                                      
         B     JPOSTX                                                           
         DROP  R2                                                               
*                                                                               
JPOST020 MVC   WORK1,0(R2)                                                      
         SAC   0                                                                
         BAS   RE,POSTIT                                                        
         SAC   512                                                              
*                                                                               
JPOSTX   SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLEAR A COMMAND TABLE ENTRY                        *                   
*************************************************************                   
         SPACE 1                                                                
CCLEAR   NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),1(R1)     A(COMMAND ENTRY)                             
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
*                                                                               
         L     R2,FULL                                                          
*                                                                               
         XC    0(32,R2),0(R2)      CLEAR IT                                     
*                                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLEAR A RECOVERY ENTRY                             *                   
*************************************************************                   
         SPACE 1                                                                
RCLEAR   NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),1(R1)     A(COMMAND ENTRY)                             
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
*                                                                               
         L     R2,FULL                                                          
*                                                                               
         XC    0(8,R2),0(R2)       CLEAR IT                                     
*                                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        FORCE RECOVERY ON OLD ENTRY                        *                   
*************************************************************                   
         SPACE 1                                                                
RRECOV   NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),1(R1)     A(COMMAND ENTRY)                             
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
*                                                                               
         L     R2,FULL                                                          
         MVC   DUB,0(R2)           SAVE RECOVERY ENTRY                          
*                                                                               
RREC070  LA    R0,DSJBHMXQ         MAX JOB TABLE ENTRIES                        
         XC    FULL,FULL           SAVE A(ENTRY)                                
         L     R2,AJOBTAB                                                       
RREC071  OC    0(8,R2),0(R2)       FREE ENTRY?                                  
         BNZ   RREC072                                                          
         OC    FULL,FULL                                                        
         BNZ   RREC072                                                          
         ST    R2,FULL             SAVE ADDR IN FULL                            
RREC072  CLI   DUB,X'FF'                                                        
         BNE   *+18                                                             
         CLC   8(2,R2),DUB+2       TEST JOBNUM MATCH                            
         BE    RREC075                                                          
         BNE   *+14                                                             
         CLC   10(1,R2),DUB        TEST ADV NUM MATCH                           
         BE    RREC075                                                          
         LA    R2,16(,R2)                                                       
         BCT   R0,RREC071                                                       
*                                                                               
         ICM   R2,15,FULL                                                       
         BZ    RREC075                                                          
         USING DSJOBHDR,R2         FAKE UP NEW JOB ENTRY FOR RECOVERY           
         MVC   DSJOBNAM,=C'DUMMY   '                                            
         MVC   DSJOBNUM,DUB+2                                                   
         MVI   DSJOBFLG,DSJOBRCV                                                
         DROP  R2                                                               
*                                                                               
RREC075  EQU   *                   DO NOTHING                                   
*                                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
         EJECT                                                                  
*************************************************************                   
*        CLEAR FILE DETAIL                                  *                   
*************************************************************                   
         SPACE 1                                                                
FCLEAR   NTR1                                                                   
         MVC   BYTE,0(R1)          ACTION                                       
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),1(R1)     A(COMMAND ENTRY)                             
*                                                                               
         L     RF,MYSSB                                                         
         SAM31 ,                                                                
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,MYALET                                                   
*                                                                               
         L     R2,FULL                                                          
         USING DSFILHDR,R2                                                      
*                                                                               
         XC    DSEOF1,DSEOF1       CLEAR EOFS DETAIL                            
         XC    DSEOF2,DSEOF2                                                    
         XC    DSFILACT,DSFILACT                                                
*                                                                               
         SAM24 ,                                                                
         SAC   0                                                                
         B     XIT1                                                             
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
         BNZ   ERR17                                                            
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         BNE   ERR17                                                            
         L     R4,ASCBASSB                                                      
*                                                                               
         SAM31 ,                   SWITCH TO 31-BIT MODE                        
         L     R4,ASSBJSAB-ASSB(R4) R4 = A(JSAB)                                
         USING JSAB,R4                                                          
         CLC   WORK1(8),JSABJBNM                                                
         SAM24 ,                   SWITCH BACK TO 24 BT MODE                    
         BNE   ERR17                                                            
*                                                                               
         LA    R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         B     XIT1                                                             
*                                                                               
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4                                                            
*************************************************************                   
*        SET & RESET SINGLE THREAD MODE                     *                   
*************************************************************                   
         SPACE 1                                                                
SINGLE   ST    RE,SAVERE                                                        
         L     RE,MYSSB                                                         
         MVI   SSBMTIND-SSBD(RE),0      DISABLE MULTI-TASKING WAITS             
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
MULTI    ST    RE,SAVERE                                                        
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         L     RE,MYSSB                                                         
         MVI   SSBMTIND-SSBD(RE),C'M'   ENABLE MULTI-TASKING WAITS              
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
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
ERR17    LA    R0,196              SYSTEM NOT POSTED                            
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
         USING SRDSPFFD,R3         R3=A(TWA)                                    
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
         GOTO1 ATICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 ADMOD000,DMCB,AWCTYPE,OPMSG,OPMSGL,C'LVL1'                       
         GOTO1 ATICTOC,DUB,C'RSET' RESET TIMERS                                 
         B     XIT1                                                             
         EJECT                                                                  
******************************************************                          
*        CONSTANTS                                   *                          
******************************************************                          
         SPACE 1                                                                
         LTORG                                                                  
DOTS     DC    40C'.'                                                           
SPACES   DC    80C' '                                                           
DISP     DC    C'$DSP'                                                          
YES      DC    CL7'Yes'                                                         
OPEN     DC    CL10'Open'                                                       
DMREAD   DC    CL7'DMREAD'                                                      
DMWRT    DC    CL7'DMWRT'                                                       
TEMPSTR  DC    CL7'TEMPSTR'                                                     
DADDS    DC    CL7'DADDS'                                                       
ISDDS    DC    CL7'ISDDS'                                                       
FIRSTDA  DC    X'00010101'                                                      
HELPID   DC    XL10'012EFF00010000000000'                                       
*                                                                               
*             ***....+....1....+....2....+....3....+....4***                    
DSHEADR0 DC    C'    Resource         Lock     Owner     '                      
         DC    C'           Num   Num                   '                       
DSHEADER DC    C'Sel Name     Num Typ Word     Name     J'                      
         DC    C'ob     Flg Locks Waits Lock time       '                       
DSUNDER  DC    C'--- -------- --- --- -------- -------- -'                      
         DC    C'------ --- ----- ----- ----------------'                       
DSDFAULT DC        C'                                    '                      
         DC    C'                                       '                       
*                                                                               
DCHEADR0 DC    C'    Resource                   ---State-'                      
         DC    C'-- Online ---------- Offline --------- '                       
DCHEADER DC    C'Sel Name     Num Status BDate  Globl Loc'                      
         DC    C'al Locks      Waits  Locks      Waits  '                       
DCUNDER  DC    C'--- -------- --- ------ ------ ---------'                      
         DC    C'-- ---------- ------ ---------- ------ '                       
DCDFAULT DC        C'                                    '                      
         DC    C'                                       '                       
*                                                                               
LSHEADR0 DC    C'    System   Max       Actv      High   '                      
         DC    C'   Total     Total                     '                       
LSHEADER DC    C'             Locks     Locks     water  '                      
         DC    C'   Locks     Waits                     '                       
LSUNDER  DC    C'--- -------- -------   -------   -------'                      
         DC    C'   -------   -------                   '                       
LSDFAULT DC        C'                                    '                      
         DC    C'                                       '                       
*                                                                               
LDHEADR0 DC    C'    Adv/Task File#     File#     File#  '                      
         DC    C'   File#     File#                     '                       
LDHEADER DC    C'Sel Job#     Locks     Locks     Locks  '                      
         DC    C'   Locks     Locks                     '                       
LDUNDER  DC    C'--- -------- -------   -------   -------'                      
         DC    C'   -------   -------                   '                       
LDDFAULT DC        C'                                    '                      
         DC    C'                                       '                       
*                                                                               
*                                                                               
WAHEADR0 DC    C'                                        '                      
         DC    C'                                       '                       
WAHEADER DC    C'Sel Job/Sys Status      Job/Sys         '                      
         DC    C'                                       '                       
WAUNDER  DC    C'--- ------- ----------- -------         '                      
         DC    C'                                       '                       
WADFAULT DC        C'                                    '                      
         DC    C'                                       '                       
*                                                                               
*                                                                               
RCHEADR0 DC    C'    Time     T# Sin    Term#     High   '                      
         DC    C'   Total     Total                     '                       
RCHEADER DC    C'             Locks     Locks     Water  '                      
         DC    C'   Locks     Waits                     '                       
RCUNDER  DC    C'--- -------- -------   -------   -------'                      
         DC    C'   -------   -------                   '                       
RCDFAULT DC        C'                                    '                      
         DC    C'                                       '                       
*                                                                               
*                                                                               
SYHEADR0 DC    C'                                        '                      
         DC    C'                                       '                       
SYHEADER DC    C'Sel Slot# Jobname  Number Adv# Start tim'                      
         DC    C'e ASID Status     Rcv                  '                       
SYUNDER  DC    C'--- ----- -------- ------ ---- ---------'                      
         DC    C'- ---- ---------- ---                  '                       
SYDFAULT DC        C'      .......  ...... ....          '                      
         DC    C'                                       '                       
*                                                                               
*                                                                               
FLHEADR0 DC    C'                                        '                      
         DC    C'                                       '                       
FLHEADER DC    C'Sel Filename Ex  Dnext   Dcnt ISovlast I'                      
         DC    C'Spdlast Data set name        Maint''ce  '                      
FLUNDER  DC    C'--- -------- -- -------- ---- -------- -'                      
         DC    C'------- -------------------- --------- '                       
FLDFAULT DC        C'........ .. ........ .... ........ .'                      
         DC    C'.......                                '                       
*                                                                               
*                                                                               
RVHEADR0 DC    C'                                        '                      
         DC    C'                                       '                       
RVHEADER DC    C'Sel Jobname     Sin     Diskaddr        '                      
         DC    C'                                       '                       
RVUNDER  DC    C'--- --------  --------  --------        '                      
         DC    C'                                       '                       
RVDFAULT DC        C'........  ........  ........        '                      
         DC    C'                                       '                       
*                                                                               
*                                                                               
COHEADR0 DC    C'                                        '                      
         DC    C'                                       '                       
COHEADER DC    C'Sel Time     Orig Dest Command          '                      
         DC    C'                                       '                       
COUNDER  DC    C'--- -------- ---- ---- -----------------'                      
         DC    C'-------------                          '                       
CODFAULT DC        C'........ .... .... .................'                      
         DC    C'                                       '                       
*                                                                               
*        DC    C'ACTION ',C'CHR',X'PFK',AL3(ROUTINE)                            
*                                                                               
ACTTAB   DC    C'DSPACE ',C'D',X'02',AL3(QUIDS)                                 
         DC    C'LOCKS  ',C'L',X'03',AL3(QUILS)                                 
         DC    C'WAITS  ',C'W',X'04',AL3(WAITS)                                 
         DC    C'FILES  ',C'F',X'00',AL3(FILES)                                 
         DC    C'RCVR   ',C'R',X'05',AL3(RECOVS)                                
         DC    C'RECOV  ',C'R',X'05',AL3(RECOVS)                                
         DC    C'JOBS   ',C'J',X'06',AL3(JOBS)                                  
         DC    C'COMMS  ',C'C',X'0B',AL3(COMMS)                                 
         DC    C'STATE  ',C'C',X'0B',AL3(STATE)                                 
         DC    C'X'                                                             
*                                                                               
*        DC    C'ACT',X'NN',C'CHR',XL3'00',AL4(ROUTINE)                         
*                                                                               
FACTION  DC    X'01',CL8'ERASING '                                              
         DC    X'02',CL8'LOADING '                                              
         DC    X'03',CL8'DUMPING '                                              
         DC    X'FF',CL8'UNKNOWN '                                              
*                                                                               
         DS    0F                                                               
*                                                                               
SELTABS  DC    XL4'00'                                                          
*                                                                               
SELDMGR  DC    CL3'JOB',X'01',C'D',XL3'00',AL4(SELECT)                          
         DC    CL3'FIL',X'02',C'D',XL3'00',AL4(SELECT)                          
         DC    CL3'RCV',X'03',C'D',XL3'00',AL4(SELECT)                          
         DC    CL3'LOC',X'04',C'D',XL3'00',AL4(SELECT)                          
         DC    CL3'SEL',X'01',C'D',XL3'00',AL4(SELECT)                          
*                                                                               
         DC    CL3'LLL',X'10',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'LIO',X'11',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'LOF',X'12',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'FRE',X'13',C'D',XL3'00',AL4(DLOCK)                           
         DC    CL3'FFR',X'14',C'D',XL3'00',AL4(DLOCK)                           
*                                                                               
         DC    CL3'B+ ',X'05',C'D',XL3'00',AL4(BILLDATE)                        
         DC    CL3'B- ',X'06',C'D',XL3'00',AL4(BILLDATE)                        
*                                                                               
         DC    CL3'CLR',X'21',C'L',XL3'00',AL4(LCLEAR)                          
         DC    CL3'SEL',X'22',C'L',XL3'00',AL4(SELECT)                          
*                                                                               
         DC    CL3'CLR',X'23',C'I',XL3'00',AL4(ICLEAR)                          
*                                                                               
         DC    CL3'POS',X'31',C'W',XL3'00',AL4(WPOST)                           
*                                                                               
         DC    CL3'POS',X'41',C'J',XL3'00',AL4(JPOST)                           
         DC    CL3'CLR',X'42',C'J',XL3'00',AL4(JPOST)                           
*                                                                               
         DC    CL3'CLR',X'51',C'C',XL3'00',AL4(CCLEAR)                          
*                                                                               
         DC    CL3'CLR',X'61',C'R',XL3'00',AL4(RCLEAR)                          
         DC    CL3'RCV',X'62',C'R',XL3'00',AL4(RRECOV)                          
*                                                                               
         DC    CL3'CLR',X'71',C'F',XL3'00',AL4(FCLEAR)                          
*                                                                               
*NOP     DC    CL3'X  ',X'7F',C'S',XL3'00',AL4(DUMMY)                           
*NOP     DC    CL3'X  ',X'7F',C'F',XL3'00',AL4(DUMMY)                           
*NOP     DC    CL3'X  ',X'7F',C'D',XL3'00',AL4(DUMMY)                           
         DC    XL4'00'                                                          
*                                                                               
STATETAB DC    C'O',C'OP'                                                       
         DC    C'C',C'CL'                                                       
         DC    C'U',C'US'                                                       
         DC    C'R',C'RO'                                                       
         DC    C'W',C'RW'                                                       
         DC    X'00',C'--'                                                      
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
       ++INCLUDE FACOMTAB                                                       
         EJECT                                                                  
WRKD     DSECT                                                                  
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
BYTE1    DS    C                                                                
MYALET   DS    F                                                                
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
TABS     DS    X                   TABS FLAG                                    
*                                                                               
CHKSEL   DS    16XL8               PREV SCREEN SAVE                             
SELDATA  DS    CL8                 SELECTED FIELD                               
ACTION   DS    CL1                 D FOR DSPACE                                 
FILNUM   DS    CL1                                                              
*                                                                               
Q1       DS    F                                                                
Q2       DS    F                                                                
Q3       DS    F                                                                
Q4       DS    F                                                                
Q5       DS    F                                                                
Q6       DS    F                                                                
*                                                                               
DA       DS    F                                                                
*                                                                               
VHEXOUT  DS    A                                                                
VHEXIN   DS    A                                                                
VGETTXT  DS    A                                                                
VDATCON  DS    A                                                                
VADDAY   DS    A                                                                
VGETHELP DS    A                                                                
ASAVE    DS    A                                                                
BASERD   DS    A                                                                
RELO     DS    A                                                                
AJOBTAB  DS    A                                                                
*                                                                               
MYUTL    DS    A                                                                
MYSSB    DS    A                                                                
ASELIST  DS    A                                                                
ADATAMGR DS    A                                                                
ALOCKSPC DS    A                                                                
ATICTOC  DS    A                                                                
AWCTYPE  DS    A                                                                
ADMOD000 DS    A                                                                
ADDSTATE DS    A                                                                
*                                                                               
ASYSHDR  DS    A                   A(CURRENTLY DISPLAYED SYSTEM)                
ASENTRY  DS    A                   A(CURRENTLY DISPLAYED SELIST                 
AFILES   DS    A                                                                
AFILEX   DS    A                                                                
ADTF     DS    A                                                                
AREQUEST DS    A                                                                
CURSOR   DS    A                                                                
SAVERE   DS    A                                                                
SAVER0   DS    A                                                                
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
SENUM    DS    X                                                                
TIMENOW  DS    F                                                                
*                                                                               
TODAY    DS    F                   WORKING AREAS FOR BILLDATE                   
TOMORROW DS    F                                                                
YESTERDY DS    F                                                                
*                                                                               
SAVETCB  DS    XL10                                                             
*                                                                               
LOCKINFO DS    16CL18                                                           
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
SVACT    DS    X                   SAVE PREVIOUS ACTION                         
         DS    0F                                                               
SVSEL    DS    16XL8               SAVED IDS FOR SELECT                         
SVSELL   EQU   *-SVSEL                                                          
         DS    X                                                                
SAVEDL   EQU   *-SAVEDSTR                                                       
*                                                                               
IOAREA   DS    1200D               8K WORK AREA                                 
WRKX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
DSLINED  DSECT                                                                  
DACTH    DS    CL8                                                              
DACT     DS    CL3                                                              
DLINEH   DS    CL8                                                              
DLINE    DS    CL75                                                             
         ORG   DLINE                                                            
DRSRC    DS    CL8                                                              
         DS    CL1                                                              
DRNUM    DS    CL2                                                              
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
CRSTA    DS    CL6                                                              
         DS    CL1                                                              
CRBDATE  DS    CL6                                                              
         DS    CL1                                                              
CRSTATE  DS    CL11                                                             
         DS    CL1                                                              
CRLOCK   DS    CL10                                                             
CRWAIT   DS    CL7                                                              
         DS    CL1                                                              
CRLOCKO  DS    CL10                                                             
CRWAITO  DS    CL7                                                              
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
WALINED  DSECT                                                                  
WACTH    DS    CL8                                                              
WACT     DS    CL3                                                              
WLINEH   DS    CL8                                                              
WLINE    DS    CL75                                                             
         ORG   WLINE                                                            
WAREQ    DS    CL7                                                              
         DS    CL1                                                              
WASTAT   DS    CL11                                                             
         DS    CL1                                                              
WAOWN    DS    CL7                                                              
         ORG                                                                    
WALINEL  EQU   *-WALINED                                                        
         EJECT                                                                  
SYLINED  DSECT                                                                  
SACTH    DS    CL8                                                              
SACT     DS    CL3                                                              
SLINEH   DS    CL8                                                              
SLINE    DS    CL75                                                             
         ORG   SLINE                                                            
SYJSLOT  DS    CL5                                                              
         DS    CL1                                                              
SYJNAME  DS    CL8                                                              
         DS    CL1                                                              
SYJNUM   DS    CL6                                                              
         DS    CL1                                                              
SYJADV   DS    CL4                                                              
         DS    CL1                                                              
SYJTIME  DS    CL10                                                             
         DS    CL1                                                              
SYJASID  DS    CL4                                                              
         DS    CL1                                                              
SYJSTAT  DS    CL10                                                             
         DS    CL1                                                              
SYJRCV   DS    CL3                                                              
         ORG                                                                    
SYLINEL  EQU   *-SYLINED                                                        
         EJECT                                                                  
FLLINED  DSECT                                                                  
FACTH    DS    CL8                                                              
FACT     DS    CL3                                                              
FLINEH   DS    CL8                                                              
FLINE    DS    CL75                                                             
         ORG   FLINE                                                            
FLNAME   DS    CL8                                                              
         DS    CL1                                                              
FLNUM    DS    CL2                                                              
         DS    CL1                                                              
FLDNEXT  DS    CL8                                                              
         DS    CL1                                                              
FLDCOUNT DS    CL4                                                              
         DS    CL1                                                              
FLOVLAST DS    CL8                                                              
         DS    CL1                                                              
FLPDLAST DS    CL8                                                              
         DS    CL1                                                              
FLDSNAME DS    CL20                                                             
         DS    CL1                                                              
FLMAINT  DS    CL8                                                              
         ORG                                                                    
FLLINEL  EQU   *-FLLINED                                                        
         EJECT                                                                  
RCLINED  DSECT                                                                  
RACTH    DS    CL8                                                              
RACT     DS    CL3                                                              
RLINEH   DS    CL8                                                              
RLINE    DS    CL75                                                             
         ORG   RLINE                                                            
RCJOB    DS    CL8                                                              
         DS    CL2                                                              
RCSIN    DS    CL8                                                              
         DS    CL2                                                              
RCDA     DS    CL8                                                              
         ORG                                                                    
RCLINEL  EQU   *-RCLINED                                                        
         EJECT                                                                  
COLINED  DSECT                                                                  
COACTH   DS    CL8                                                              
COACT    DS    CL3                                                              
COLINEH  DS    CL8                                                              
COLINE   DS    CL75                                                             
         ORG   COLINE                                                           
COTIME   DS    CL8                                                              
         DS    CL1                                                              
COORG    DS    CL4                                                              
         DS    CL1                                                              
CODEST   DS    CL4                                                              
         DS    CL1                                                              
COCOMM   DS    CL32                                                             
         ORG                                                                    
COLINEL  EQU   *-COLINED                                                        
         EJECT                                                                  
*DDFLDHDR                                                                       
*DDCOMFACS                                                                      
*FADSECTS                                                                       
*DMDTFIS                                                                        
*DMDTFPH                                                                        
*DMSPACED                                                                       
*DMDSYSHDR                                                                      
*DMDSHDR                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMSPACED                                                       
       ++INCLUDE DMDSYSHDR                                                      
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
         EJECT                                                                  
SRDSPFFD DSECT                                                                  
         DS    CL64                                                             
*SRDSPFFD                                                                       
       ++INCLUDE SRDSPFFD                                                       
         SPACE 2                                                                
         IHAASCB                                                                
         IHAASSB                                                                
         IAZJSAB                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087SRDSP00   09/17/20'                                      
         END                                                                    
