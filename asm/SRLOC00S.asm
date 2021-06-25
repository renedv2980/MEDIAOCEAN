*          DATA SET SRLOC00S   AT LEVEL 009 AS OF 02/15/01                      
*PHASE T15C00A                                                                  
         PRINT NOGEN                                                            
         TITLE '$LOCK - LOCK RECORDS FOR UPDATIVE SOON REQS'                    
LOCK     CSECT                                                                  
         NMODL WORKL,**$LOC**,R9,CLEAR=YES,RR=RE                                
         USING WORKD,RC             RC=A(W/S)                                   
         ST    RE,RELO                                                          
         ST    R1,APARMS                                                        
         MVC   IPARMS,0(R1)         SAVE INCOMING PARAMETERS                    
*                                                                               
         LHI   R8,LITERALS-LOCK                                                 
         AR    R8,RB                                                            
         USING LITERALS,R8                                                      
*                                                                               
         ST    RD,SAVERD           SAVE FOR ERROR EXIT                          
         ST    RB,BASERB           SAVE FOR COMMON SUB ROUTINES                 
         USING SRPARMD,R1                                                       
         L     RA,ATWA                                                          
         USING SRLOCFFD,RA         RA=A(TWA)                                    
         BRAS  RE,INIT             DO INITIALISATION                            
         DROP  R1                                                               
*                                                                               
         L     R7,ASAVE                                                         
         AHI   R7,SR$LOCK-SRSD                                                  
         USING SAVEDSTR,R7                                                      
*                                                                               
         BRAS  RE,READSTR          READ SAVE STORAGE                            
         BRAS  RE,SETPRTQ          SET UP PRTQ FILE                             
*                                                                               
         LA    R1,SRVTABH          TEST CURSOR POSN                             
         C     R1,CURSOR                                                        
         BL    LOCK02                                                           
         XC    SVCTKEY,SVCTKEY                                                  
         XC    SVSEQ,SVSEQ                                                      
*                                                                               
LOCK02   BRAS  RE,HELPSCAN                                                      
         BRAS  RE,VALACT                                                        
         BRAS  RE,VALTYPE                                                       
         BRAS  RE,VALOPT                                                        
*                                                                               
         OC    PQUSER,PQUSER       IF USER CHANGED PRTQ MAY BE CHANGED          
         BZ    LOCK04                                                           
         CLC   PQUSER,TRMUSER                                                   
         BE    LOCK04                                                           
         BRAS  RE,SETPRTQ          RESET PRTQ                                   
*                                                                               
LOCK04   CLI   ACTION,ACTDIS       DISPLAY                                      
         BE    LOCK12                                                           
         CLI   ACTION,ACTFREE      FREE                                         
         BNE   LOCK06                                                           
         GOTO1 ALOCKET,DMCB,(C'F',KEY),ACOMFACS                                 
         BRAS  RE,TABFREE                                                       
         B     LOCK16                                                           
*                                                                               
LOCK06   CLI   ACTION,ACTSTOP      STOP                                         
         BNE   LOCK08                                                           
         GOTO1 ALOCKET,DMCB,(C'S',KEY),ACOMFACS                                 
         B     LOCK16                                                           
*                                                                               
LOCK08   CLI   ACTION,ACTADD       ADD                                          
         BNE   LOCK10                                                           
         OC    KEY,KEY                                                          
         BZ    ERR13                                                            
         GOTO1 ALOCKET,DMCB,(C'L',KEY),ACOMFACS                                 
         BRAS  RE,LOCKETR                                                       
         B     LOCK16                                                           
*                                                                               
LOCK10   CLI   ACTION,ACTBLD       BUILD                                        
         BNE   LOCK16                                                           
         GOTO1 ALOCKET,DMCB,(C'B',KEY),ACOMFACS                                 
         B     LOCK16                                                           
*                                                                               
LOCK12   BRAS  RE,VALSUB                                                        
         TM    DDSFLAG,DDSACT      TEST ANY SUB ACTIONS                         
         BZ    LOCK16                                                           
*                                                                               
         SR    R1,R1               EXECUTE SUBACTIONS                           
         IC    R1,LOCTYPE                                                       
         SLL   R1,2                                                             
         EX    0,*+4(R1)                                                        
         B     LOCK14                                                           
         BAS   RE,FACACT                                                        
         BAS   RE,REPACT                                                        
         BAS   RE,LOCACT                                                        
         BAS   RE,DSKACT                                                        
*                                                                               
LOCK14   MVC   SVSEQ,PREVSEQ       USE PREVIOUS SAVE VALUES                     
         MVC   SVCTKEY,PREVCTK                                                  
*                                                                               
LOCK16   MVC   PREVSEQ,SVSEQ       SAVE THESE VALUES                            
         MVC   PREVCTK,SVCTKEY                                                  
         SR    R1,R1               EXECUTE DISPLAY MODULES                      
         IC    R1,LOCTYPE                                                       
         SLL   R1,2                                                             
         EX    0,*+4(R1)                                                        
         B     LOCK18                                                           
         BAS   RE,FACDISP                                                       
         BAS   RE,REPDISP                                                       
         BAS   RE,LOCDISP                                                       
         BAS   RE,DSKDISP                                                       
*                                                                               
LOCK18   LA    R3,SRVSACTH                                                      
         ST    R3,CURSOR                                                        
         CLI   ACTION,1                                                         
         BE    INFO                                                             
         LA    R3,SRVACTH                                                       
         ST    R3,CURSOR                                                        
         B     INFO                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY FAUPDTAB FOR SOON ENTRIES                                   *         
***********************************************************************         
         SPACE 1                                                                
FACDISP  NTR1  ,                                                                
         LAM   R0,RF,ARZERO                                                     
         LA    R3,SRVSACTH                                                      
         USING FACLINE,R3                                                       
         MVC   SVTYPE,LOCTYPE                                                   
         MVCDD SRVHD1,SR#LOC04     REPLACE HEADERS                              
         MVCDD SRVHD2,SR#LOC05                                                  
         MVI   BYTE,0                                                           
*                                                                               
         LH    R4,AUPDLEN                                                       
         L     R5,AUPDTABX                                                      
         L     R6,AUPDTAB                                                       
         USING UPDTABD,R6                                                       
         LA    R2,UPDTABS                                                       
LCL      USING UPDTABD,R2                                                       
*                                                                               
DFC02    LAM   R0,RF,ARZERO        OVERKILL - BETTER SAFE THAN SORRY            
         LAM   R6,R6,ALET                                                       
         SAC   512                                                              
         CLI   UPDTABT,UPDTSQ      SEE IF SOON ENTRY                            
         BNE   DFC06                                                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BYTE                                                          
         AHI   R1,1                                                             
         STC   R1,BYTE                                                          
         CLC   BYTE,SVSEQ                                                       
         BL    DFC06                                                            
*                                                                               
         LA    R1,SRVXXXH                                                       
         CR    R3,R1                                                            
         BE    DFC08               NO MORE ROOM ON SCREEN                       
*                                                                               
         MVC   LCL.UPDTABT(UPDTABL),UPDTABT                                     
         SAC   0                                                                
         LAM   R6,R6,ARZERO        FROM NOW ON USE LOCAL COPY                   
*                                                                               
         EDIT  (B1,BYTE),(3,FLNUM),ALIGN=LEFT                                   
*                                                                               
         CLC   LCL.UPDUSR,SAVUSER                                               
         BE    DFC04               SAME USER AS LAST TIME                       
         MVC   SAVUSER,LCL.UPDUSR                                               
         BRAS  RE,GETUSR                                                        
         MVC   FLUSERID,USERID DISPLAY USER ID                                  
*                                                                               
DFC04    MVC   FLREPID(3),LCL.UPDRID   DISPLAY REPORT ID                        
         MVI   FLREPID+3,C','                                                   
         LA    RF,FLREPID+4                                                     
         EDIT  (B2,LCL.UPDSEQ),(5,0(RF)),ALIGN=LEFT,ZERO=NOBLANK                
         EDIT  (B1,LCL.UPDCHAIN),(2,FLQUEUE),FILL=0                             
         EDIT  (B2,LCL.UPDRULE),(5,FLRULE),FILL=0                               
         GOTO1 AHEXOUT,DMCB,LCL.UPDFLAGS,FLFLAG+1,1                             
         GOTO1 (RF),(R1),LCL.UPDSENUM,FLSENUM,1                                 
         GOTO1 (RF),(R1),LCL.UPDEXT1,FLFILES+0,1                                
         GOTO1 (RF),(R1),LCL.UPDEXT2,FLFILES+3,1                                
         GOTO1 (RF),(R1),LCL.UPDKEY,FLDATA,10                                   
*                                                                               
         AHI   R3,L'RLINE          NEXT LINE                                    
         AHI   R2,UPDTABL                                                       
*                                                                               
DFC06    BXLE  R6,R4,DFC02                                                      
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         CLI   BYTE,0              TEST FOR EMPTY TABLE                         
         BE    ERR8                                                             
         MVI   BYTE,0                                                           
*                                                                               
DFC08    SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         MVC   SVSEQ,BYTE                                                       
         MVC   INFONUM,INFO1       SET INFO MSG                                 
         B     EXITOK                                                           
         DROP  LCL,R3,R6                                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY LOCKET ENTRIES                                              *         
***********************************************************************         
         SPACE 1                                                                
LOCDISP  NTR1  ,                                                                
         LAM   R0,RF,ARZERO                                                     
         LA    R3,SRVSACTH                                                      
         USING USRLINE,R3                                                       
         MVC   SVTYPE,LOCTYPE                                                   
         MVCDD SRVHD1,SR#LOC06     REPLACE HEADERS                              
         MVCDD SRVHD2,SR#LOC07                                                  
         MVI   BYTE,0              CLEAR FLAG                                   
*                                                                               
         LH    R4,AUPDLEN                                                       
         L     R5,AUPDTABX                                                      
         L     R6,AUPDTAB                                                       
         USING UPDTABD,R6                                                       
         LA    R2,UPDTABS                                                       
LCL      USING UPDTABD,R2                                                       
*                                                                               
LKT02    LAM   R0,RF,ARZERO        OVERKILL - BETTER SAFE THAN SORRY            
         LAM   R6,R6,ALET                                                       
         SAC   512                                                              
         CLI   UPDTABT,UPDTLQ      SEE IF LOCKET ENTRY                          
         BNE   LKT08                                                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STC   R1,BYTE                                                          
         CLC   BYTE,SVSEQ                                                       
         BL    LKT08                                                            
*                                                                               
         LA    R1,SRVXXXH                                                       
         CR    R3,R1                                                            
         BE    LKT10               NO MORE ROOM                                 
*                                                                               
         MVC   LCL.UPDTABT(UPDTABL),UPDTABT                                     
         SAC   0                                                                
         LAM   R6,R6,ARZERO        FROM NOW ON USE LOCAL COPY                   
*                                                                               
         CLI   SYSFILT,0           SYSTEM=?                                     
         BE    *+14                                                             
         CLC   LCL.UPDLTSE,SYSFILT                                              
         BNE   LKT08                                                            
*                                                                               
         LA    R0,LCL.UPDLDATE                                                  
         GOTO1 ADATCON,DMCB,(8,(R0)),(16,ULDATE)                                
         MVC   LKSYSSE,LCL.UPDLTSE                                              
         BRAS  RE,GETSYS           GET SYSTEM NAME                              
*                                                                               
         MVC   ULSYS,LKSYSN                                                     
         MVC   ULAGY,LCL.UPDLTAG                                                
         MVC   ULTYP,LCL.UPDLTYP                                                
         MVC   ULKEY,LCL.UPDLKEY                                                
         MVC   ULSTAT,DC@LOCKD                                                  
         MVC   ULLUID,LCL.UPDLUID                                               
         MVC   FULL(3),LCL.UPDLTIME                                             
         BRAS  RE,TIMCONV                                                       
         MVC   ULTIME1,WORK                                                     
         MVC   ULFAC,=CL4'????'                                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,LCL.UPDLFAC                                                   
         CHI   RF,FACIDMAX                                                      
         BH    LKT06                                                            
*                                                                               
         LA    RE,FACIDTAB                                                      
         USING FACITABD,RE                                                      
LKT04    CLI   0(RE),X'FF'                                                      
         BE    LKT06                                                            
         CLM   RF,1,FACIID                                                      
         BE    *+12                                                             
         AHI   RE,L'FACITAB                                                     
         B     LKT04                                                            
         MVC   ULFAC,FACISN4                                                    
         DROP  RE                                                               
*                                                                               
LKT06    AHI   R3,L'RLINE          NEXT LINE                                    
         AHI   R2,UPDTABL                                                       
*                                                                               
LKT08    BXLE  R6,R4,LKT02                                                      
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         MVI   SVSEQ,0                                                          
         CLI   BYTE,0                                                           
         BE    ERR8                NOWT TO DISPLAY                              
         MVC   INFONUM,INFO2       LOCK ENTRIES DISPLAYED                       
         B     EXITOK                                                           
*                                                                               
LKT10    MVC   SVSEQ,BYTE                                                       
         MVC   INFONUM,INFO4       SET ENTER FOR NEXT MESSAGE                   
         B     EXITOK                                                           
         DROP  LCL,R3,R6                                                        
         EJECT                                                                  
***********************************************************************         
* DISPLAY DISK RECORDS FROM CTFILE                                    *         
***********************************************************************         
         SPACE 1                                                                
DSKDISP  NTR1  ,                                                                
         LA    R3,SRVSACTH                                                      
         USING USRLINE,R3                                                       
         L     R6,AIOAREA                                                       
         USING CT8REC,R6                                                        
*                                                                               
         LA    R2,UPDTABS                                                       
         MVC   SVTYPE,LOCTYPE                                                   
         MVCDD SRVHD1,SR#LOC06     REPLACE HEADERS                              
         MVCDD SRVHD2,SR#LOC07                                                  
         MVC   INFONUM,INFO3                                                    
         MVI   BYTE,0                                                           
         CLI   STAFILT,8           IF STATUS=DEL THEN READ DELETES              
         BNE   *+8                                                              
         MVI   BYTE,X'08'                                                       
*                                                                               
         MVC   CT8KEY,SVCTKEY      SET LAST KEY FOR READHI                      
         OC    CT8KEY,CT8KEY       TEST FOR FIRST TIME                          
         BNZ   *+14                                                             
         MVI   CT8KTYP,CT8KTYPQ                                                 
         MVC   CT8KDATE,DATFILT                                                 
         GOTO1 ADMGR,DMCB,(BYTE,DMRDHI),CTFILE,CT8KEY,CT8REC                    
*                                                                               
DSK02    OC    DATFILT,DATFILT                                                  
         BZ    *+14                                                             
         CLC   CT8KDATE,DATFILT                                                 
         BNE   DSK06                                                            
         CLI   CT8KTYP,CT8KTYPQ                                                 
         BNE   DSK06                                                            
*                                                                               
         CLI   STAFILT,9           STAT=LOCKED                                  
         BNE   *+12                                                             
         TM    CT8STAT,X'40'                                                    
         BO    DSK04                                                            
         CLI   STAFILT,10          STAT=UNLOCKED                                
         BNE   *+12                                                             
         TM    CT8STAT,X'40'                                                    
         BNO   DSK04                                                            
         CLI   SYSFILT,0           SYS=?                                        
         BE    *+14                                                             
         CLC   CT8KSE,SYSFILT                                                   
         BNE   DSK04                                                            
*                                                                               
         MVC   1(L'CT8KEY,R2),CT8KEY  SAVE KEY AT +1                            
         AHI   R2,UPDTABL                                                       
*                                                                               
         GOTO1 ADATCON,DMCB,(8,CT8KDATE),(16,ULDATE)                            
         MVC   LKSYSSE,CT8KSE                                                   
         BRAS  RE,GETSYS                                                        
         MVC   ULSYS,LKSYSN                                                     
         MVC   ULAGY,CT8KAG                                                     
         MVC   ULTYP,CT8KTYPE                                                   
         MVC   ULKEY,CT8KKEY                                                    
         MVC   ULSTAT,DC@LOCK                                                   
         TM    CT8STAT,X'40'                                                    
         BNO   *+10                                                             
         MVC   ULSTAT,DC@UNLK                                                   
         TM    CT8STAT,X'80'                                                    
         BNO   *+10                                                             
         MVC   ULSTAT,SR@DEL                                                    
*                                                                               
         LA    R4,CT8DATA                                                       
         USING CTLKLD,R4                                                        
         MVC   ULLUID,CTLKLUID                                                  
         MVC   FULL(3),CTLKLT1     DISPLAY TIMES                                
         BRAS  RE,TIMCONV                                                       
         MVC   ULTIME1,WORK                                                     
         MVC   FULL(3),CTLKLT2                                                  
         BRAS  RE,TIMCONV                                                       
         MVC   ULTIME2,WORK                                                     
*                                                                               
         AHI   R3,L'RLINE          NEXT LINE                                    
*                                                                               
DSK04    GOTO1 ADMGR,DMCB,(BYTE,DMRSEQ),CTFILE,CT8KEY,CT8REC                    
         CLI   CT8KTYP,CT8KTYPQ                                                 
         BE    *+14                                                             
         XC    SVCTKEY,SVCTKEY                                                  
         B     DSK06                                                            
         LA    R1,SRVXXXH                                                       
         CR    R3,R1                                                            
         BNE   DSK02                                                            
         MVC   SVCTKEY,CT8KEY                                                   
         MVC   INFONUM,INFO5       SET ENTER FOR NEXT MESSAGE                   
*                                                                               
DSK06    LA    R1,SRVSACTH                                                      
         CR    R3,R1                                                            
         BE    ERR8                NOTHING TO DISPLAY                           
         B     EXITOK                                                           
         DROP  R3,R6,R4                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY REPORTS                                                     *         
***********************************************************************         
         SPACE 1                                                                
REPDISP  NTR1  ,                                                                
         TM    DDSFLAG,DDSACT      DON'T SCAN IF SUB ACTION ENTERED             
         BNZ   *+8                                                              
         BRAS  RE,SCAN00           SCAN PQ FOR UPDATIVE SOONS                   
         B     LISTREP             DISPLAY SOON JOBS                            
         EJECT                                                                  
***********************************************************************         
* BUILD REPORT STATUS LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
LISTREP  XC    MSG,MSG                                                          
         LA    R3,SRVSACTH                                                      
         USING REPLINE,R3                                                       
         LA    R6,UPDTABS                                                       
         USING USAVED,R6                                                        
         OC    COUNT,COUNT                                                      
         BZ    EXITOK                                                           
LRP02    CLC   USDUSR,SAVUSER                                                   
         BE    LRP04               SAME USER AS LAST TIME                       
         MVC   SAVUSER,USDUSR                                                   
         BRAS  RE,GETUSR                                                        
*                                                                               
LRP04    MVC   RLUSERID,USERID     DISPLAY USER ID                              
         MVC   RLREPID(3),USDRID   DISPLAY REPORT ID                            
         MVI   RLREPID+3,C','                                                   
         LA    RF,RLREPID+4                                                     
         EDIT  (B2,USDSEQ),(5,0(RF)),ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
         MVI   BYTE,1                                                           
         LAM   R0,RF,ARZERO                                                     
         L     R4,AUPDTAB          SCAN FAUPDTAB                                
         LH    RE,AUPDLEN                                                       
         L     RF,AUPDTABX                                                      
         LAM   R4,R4,ALET                                                       
         SAC   512                                                              
T        USING UPDTABD,R4                                                       
*                                                                               
LRP06    CLC   USDUSR(7),T.UPDUSR                                               
         BNE   *+14                                                             
         MVC   USDSENUM(15),T.UPDSENUM                                          
         B     LRP08                                                            
         BXLE  R4,RE,LRP06                                                      
         MVI   BYTE,0              SET REPORT NOT IN TABLE                      
*                                                                               
LRP08    SAC   0                   DISPLAY REPORT STATUS                        
         LAM   R0,RF,ARZERO                                                     
         LA    R2,STATTAB          DISPLAY REPORT STATUS                        
         USING STATABD,R2                                                       
         XR    R0,R0                                                            
         CLI   USSTAT,0            TEST PURGED                                  
         BE    LRP14                                                            
         TM    USSTAT,PQSTHO                                                    
         BZ    LRP10                                                            
         LA    R0,1                HOLD                                         
         CLI   BYTE,0                                                           
         BE    LRP14                                                            
         LA    R0,2                QUEUED                                       
         B     LRP14                                                            
*                                                                               
LRP10    CLI   BYTE,0                                                           
         BNE   LRP12                                                            
         LA    R0,7                ERROR                                        
         TM    USATTB,PQATERR                                                   
         BNZ   LRP14                                                            
         LA    R0,6                READY                                        
         B     LRP14                                                            
*                                                                               
LRP12    LA    R0,4                RUNNING                                      
         TM    USATTB,PQATJOBO+PQATJOBI                                         
         BO    LRP14                                                            
         LA    R0,5                UPDATING                                     
         TM    USATTB,PQATJOBO                                                  
         BO    LRP14                                                            
         LA    R0,3                RELEASED                                     
         TM    USATTB,PQATJOBI                                                  
         BO    LRP14                                                            
         DC    H'0'                DON'T KNOW                                   
*                                                                               
LRP14    CLM   R0,1,STAVERB                                                     
         BE    LRP16                                                            
         AHI   R2,STATABLQ                                                      
         CLI   0(R2),0                                                          
         BNE   LRP14                                                            
         DC    H'0'                                                             
*                                                                               
LRP16    EX    0,STALARF                                                        
         MVC   RLREPSTA,0(RF)                                                   
*                                                                               
         CLI   USSTAT,0                                                         
         BE    LRP18                                                            
         TM    USATTB,PQATJOBI     TEST REPORT IS STILL JCL                     
         BZ    LRP18                                                            
         BRAS  RE,GETLOCK          GETLOCK PUT DETAILS INTO MSG                 
         BRAS  RE,GETSYS                                                        
         B     LRP26                                                            
*                                                                               
LRP18    CLI   BYTE,0              MUST BE A TABLE ENTRY                        
         BNE   *+20                                                             
         XC    MSG,MSG                                                          
         MVCDD MSG(9),SR#NONE EOF                                               
         B     LRP26                                                            
         MVC   LKSYSSE,USDSENUM                                                 
         BRAS  RE,GETSYS                                                        
         BE    LRP20                                                            
         XC    MSG,MSG                                                          
         MVCDD MSG(9),SR#NONE EOF                                               
         B     LRP26                                                            
*                                                                               
LRP20    LA    R2,PHSTAB           FIND PHASE FOR SYSTEM                        
         USING PHSTABD,R2                                                       
*                                                                               
LRP22    CLC   LKSYS,PHSSYS        TEST SYSTEM NAME                             
         BE    LRP24                                                            
         AHI   R2,PHSTABLQ                                                      
         CLI   0(R2),0                                                          
         BNE   LRP22                                                            
         DC    H'0'                INVALID SYSTEM                               
*                                                                               
LRP24    CLC   PHASE,PHSPHS        SAME PHASE AS BEFORE ?                       
         BE    *+10                                                             
         XC    APHASE,APHASE       NO CLEAR PHASE ADDR                          
         MVC   PHASE,PHSPHS                                                     
*                                                                               
         ST    R6,UPDENTRY                                                      
         BRAS  RE,GETPHASE                                                      
*                                                                               
         L     RF,APHASE                                                        
         LR    R1,RC                                                            
         MVI   CALL,C'D'           CALL DISPLAY PHASE                           
         BASR  RE,RF                                                            
*                                                                               
LRP26    MVC   RLREPSYS,LKSYSN                                                  
         TM    DDSFLAG,DDSTRM      3 CHRS FOR NON DDS TERMINALS                 
         BNZ   *+8                                                              
         MVI   RLREPSYS+3,C' '                                                  
         TM    DDSFLAG,DDSHEX      TEST FOR DISP=HEX MODE                       
         BZ    LRP28                                                            
         XC    MSG,MSG                                                          
         GOTO1 AHEXOUT,DMCB,USDFLAGS,MSG,17                                     
*                                                                               
LRP28    MVC   RLREPFIL,MSG                                                     
         AHI   R3,L'RLINE                                                       
         LA    R1,SRVXXXH                                                       
         CR    R3,R1                                                            
         BNL   EXITOK              END OF SCREEN                                
         AHI   R6,UPDTABL                                                       
         OC    0(7,R6),0(R6)       END OF RECORDS                               
         BNZ   LRP02                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS FAUPDTAB SUB ACTIONS                                        *         
***********************************************************************         
         SPACE 1                                                                
FACACT   NTR1  ,                                                                
         BRAS  RE,TABLOCK                                                       
         BNE   ERR11                                                            
*                                                                               
         LA    R4,UPDTABS                                                       
         USING UPDTABD,R4                                                       
         LA    R0,18                                                            
FCT02    CLI   UPDTABT,0           ANY ACTION                                   
         BNE   FCT04                                                            
         AHI   R4,UPDTABL          NO TRY NEXT                                  
         BCT   R0,FCT02                                                         
         B     FCT12                                                            
*                                                                               
FCT04    CLI   UPDTABT,SUBDEL      DELETE IS ONLY VALID ACTION                  
         BE    FCT06                                                            
         DC    H'0'                                                             
*                                                                               
FCT06    MVI   UPDTABT,0                                                        
         LAM   R0,RF,ARZERO                                                     
         LAM   R2,R2,ALET                                                       
         L     R2,AUPDTAB          R2=A(UPDATE TABLE)                           
         LH    RE,AUPDLEN                                                       
         L     RF,AUPDTABX                                                      
         SAC   512                                                              
DS       USING UPDTABD,R2                                                       
*                                                                               
FCT08    CLC   DS.UPDUSR(7),UPDUSR MATCH ENTRY BY REPORT KEY                    
         BE    FCT10                                                            
         BXLE  R2,RE,FCT08                                                      
         B     ERR7                                                             
*                                                                               
FCT10    L     R3,AUPDTABX                                                      
         AHI   R3,1                                                             
         SR    R3,R2               R3=L'DEST                                    
         LR    RE,R2                                                            
         AH    RE,AUPDLEN          RE=SOURCE                                    
         LR    RF,R3                                                            
         SH    RF,AUPDLEN          RF=L'SOURCE                                  
         CPYA  RE,R2                                                            
         MVCL  R2,RE               MOVE & PAD LAST ENTRY WITH ZERO              
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         B     FCT02                                                            
*                                                                               
FCT12    BRAS  RE,TABFREE                                                       
         B     EXITOK                                                           
         DROP  DS,R4                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS DISK SUB ACTIONS                                            *         
***********************************************************************         
         SPACE 1                                                                
DSKACT   NTR1  ,                                                                
         BRAS  RE,CTSWIT                                                        
         L     R6,AIOAREA                                                       
         USING CT8REC,R6                                                        
         LA    R2,UPDTABS          R6=SAVE UPDATE TABLE                         
         USING UPDTABD,R2                                                       
         LA    R0,18                                                            
DACT02   CLI   UPDTABT,0           ANY ACTION                                   
         BNE   DACT04                                                           
         AHI   R2,UPDTABL          NO TRY NEXT                                  
         BCT   R0,DACT02                                                        
         B     EXITOK                                                           
*                                                                               
DACT04   CLI   UPDTABT,SUBDEL      DELETE                                       
         BE    DACT06                                                           
         CLI   UPDTABT,SUBUNLK     UNLOCK                                       
         BE    DACT06                                                           
         DC    H'0'                                                             
*                                                                               
DACT06   MVC   CT8KEY,UPDTSOON     RESTORE KEY FROM +1                          
         GOTO1 ADMGR,DMCB,(X'80',DMREAD),CTFILE,CT8KEY,CT8REC                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   UPDTABT,SUBUNLK     TEST FOR UNLOCK                              
         BE    *+12                                                             
         OI    CT8STAT,X'80'       SET TO DELETED                               
         B     DACT08                                                           
*                                                                               
         LA    R1,CT8DATA          SET COMPLETION TIME                          
         USING CTLKLD,R1                                                        
         MVC   CTLKLT2,LKTIME                                                   
         OI    CT8STAT,X'40'       SET TO UNLOCKED                              
*                                                                               
DACT08   GOTO1 ADMGR,DMCB,DMWRT,CTFILE,CT8KEY,CT8REC                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   UPDTABT,0           RESET THIS FIELD                             
         B     DACT02                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS LOCKET SUB ACTIONS                                          *         
***********************************************************************         
         SPACE 1                                                                
LOCACT   NTR1  ,                                                                
         LA    R2,UPDTABS                                                       
         USING UPDTABD,R2                                                       
         LA    R0,18                                                            
LCA02    CLI   UPDTABT,0           ANY ACTION                                   
         BNE   LCA04                                                            
         AHI   R2,UPDTABL          NO TRY NEXT                                  
         BCT   R0,LCA02                                                         
         B     EXITOK                                                           
*                                                                               
LCA04    CLI   UPDTABT,SUBDEL      DELETE                                       
         BE    LCA06                                                            
         CLI   UPDTABT,SUBUNLK     UNLOCK                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   UPDTABT,0                                                        
         MVC   KEY,UPDLTSE                                                      
         GOTO1 ALOCKET,DMCB,(C'U',KEY),ACOMFACS                                 
         BRAS  RE,LOCKETR                                                       
         B     LCA02                                                            
*                                                                               
LCA06    MVI   UPDTABT,0                                                        
         BRAS  RE,TABLOCK          GET LOCK TABLE                               
         BNE   ERR11                                                            
*                                                                               
         LAM   R0,RF,ARZERO                                                     
         L     R4,AUPDTAB          R6=FAC UPDATE TABLE                          
         LH    RE,AUPDLEN                                                       
         L     RF,AUPDTABX                                                      
         LAM   R4,R4,ALET                                                       
         SAC   512                                                              
DS       USING UPDTABD,R4                                                       
*                                                                               
LCA08    CLC   DS.UPDTLOCK,UPDTLOCK                                             
         BE    LCA10                                                            
         BXLE  R4,RE,LCA08                                                      
         B     ERR7                                                             
*                                                                               
LCA10    L     R5,AUPDTABX                                                      
         AHI   R5,1                                                             
         SR    R5,R4               R5=L'DEST                                    
         LR    RE,R4                                                            
         AH    RE,AUPDLEN          RE=SOURCE                                    
         LR    RF,R5                                                            
         SH    RF,AUPDLEN          RF=L'SOURCE                                  
         CPYA  RE,R4                                                            
         MVCL  R4,RE               MOVE & PAD LAST ENTRY WITH ZERO              
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         BRAS  RE,TABFREE                                                       
         B     LCA02                                                            
         DROP  DS,R2                                                            
         EJECT                                                                  
***********************************************************************         
* PROCESS REPORT ACTIONS                                              *         
***********************************************************************         
         SPACE 1                                                                
REPACT   NTR1                                                                   
         LA    R6,UPDTABS          R6=SAVE UPDATE TABLE                         
         USING USAVED,R6                                                        
         LA    R0,18                                                            
RCA02    CLI   0(R6),0             ANY ACTION                                   
         BNE   RCA04                                                            
         AHI   R6,UPDTABL          NO TRY NEXT                                  
         BCT   R0,RCA02                                                         
         B     EXITOK                                                           
*                                                                               
RCA04    CLI   0(R6),SUBRLSE       RELEASE                                      
         BE    RCA06                                                            
         CLI   0(R6),SUBPRGE       PURGE                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DUB,PURGE           SET PURGE ACTION                             
         MVI   USSTAT,0            SET PURGE STATUS                             
         B     RCA14                                                            
*                                                                               
RCA06    BRAS  RE,LKQBIT           TEST FOR INHIBIT BIT                         
         BO    ERR12                                                            
         BRAS  RE,TABLOCK          GET LOCK TABLE                               
         BNE   ERR11                                                            
         MVI   0(R6),0                                                          
*                                                                               
         LAM   R0,RF,ARZERO                                                     
         L     R4,AUPDTAB          R4=FAC UPDATE TABLE                          
         LH    RE,AUPDLEN                                                       
         L     RF,AUPDTABX                                                      
         LAM   R4,R4,ALET                                                       
         SAC   512                                                              
         USING UPDTABD,R4                                                       
*                                                                               
RCA08    CLC   UPDUSR(7),USDUSR    TEST REPORT ALREADY EXISTS                   
         BE    RCA12                                                            
         CLI   UPDTABT,0           TEST FOR FREE ENTRY                          
         BE    RCA10                                                            
         BXLE  R4,RE,RCA08                                                      
         SAC   0                                                                
         LAM   R4,R4,ARZERO                                                     
         B     ERR9                TABLE FULL                                   
*                                                                               
RCA10    MVI   UPDTABT,UPDTSQ      ADD ENTRY TO TABLE                           
         MVC   UPDUSR(7),USDUSR                                                 
         MVC   UPDSENUM,USDSENUM                                                
         MVC   UPDEXT1,USDEXT1                                                  
         MVC   UPDEXT2,USDEXT2                                                  
         MVC   UPDRULE,USDRULE                                                  
         MVC   UPDKEY(10),USDKEY                                                
*                                                                               
RCA12    OI    UPDFLAGS,X'01'      SET LOCK                                     
         SAC   0                                                                
         LAM   R4,R4,ARZERO                                                     
         MVC   DUB,ACTI                                                         
*                                                                               
RCA14    MVI   0(R6),0                                                          
         XC    NDX,NDX                                                          
         XC    SAVE(12),SAVE                                                    
         MVC   SAVE+4(4),=C'LINE'                                               
         MVC   CIRSN,USDSEQ                                                     
         BRAS  RE,RSNXPE                                                        
         BRAS  RE,GETCAD                                                        
         L     R5,ACIREC            INITIALISE CIREC                            
         USING PQRECD,R5                                                        
         GOTO1 ADMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,8(R5)                                                         
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
         GOTO1 ADMGR,DMCB,(X'00',RANDOM),PRTQID,NDX,SAVE,(R5)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   PQKEY,USDUSR        TEST REPORT CHANGED                          
         BNE   ERR7                                                             
         GOTO1 ADMGR,DMCB,(X'00',DUB),PRTQID,NDX,SAVE,(R5)                      
         CLI   8(R1),0                                                          
         BE    RCA02                                                            
         DC    H'0'                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* TEST IF LOCKING HAS BEEN STOPPED                                    *         
***********************************************************************         
         SPACE 1                                                                
LKQBIT   NTR1  ,                                                                
         LAM   R0,RF,ARZERO                                                     
         LAM   R2,R2,ALET                                                       
         L     R2,AUPDTABH                                                      
         SAC   512                                                              
         USING LOCTABD,R2                                                       
         TM    4(R2),X'80'                                                      
         IPM   R0                  PRESERVE CC                                  
         SAC   0                                                                
         LAM   R2,R2,ARZERO                                                     
         SPM   R0                  RESTORE CC                                   
         BO    EXITL                                                            
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOCK TABLE IF POSSIBLE. EXIT CC=EQU IF OK                           *         
***********************************************************************         
         SPACE 1                                                                
TABLOCK  NTR1  BASE=*,LABEL=*                                                   
         LHI   R3,200              TRY 200 TIMES                                
*                                                                               
TLK02    LAM   R0,RF,ARZERO        I DON'T TRUST TICTOC HERE                    
         LAM   R2,R2,ALET                                                       
         L     R2,AUPDTABH                                                      
         SAC   512                                                              
         XR    R0,R0                                                            
         ICM   R1,15,=A(LOCTOKN)                                                
         CS    R0,R1,0(R2)         LOCK TABLE IF FREE                           
         BE    TLK04                                                            
*                                                                               
         SAC   0                                                                
         LAM   R2,R2,ARZERO                                                     
*                                                                               
         LHI   RF,384              WAIT 1 MILLISECOND THEN TRY AGAIN            
         GOTO1 ATICTOC,DMCB,C'WAIT',(RF)                                        
         BCT   R3,TLK02                                                         
         B     EXITL                                                            
*                                                                               
TLK04    OI    DDSFLAG,DDSLOCK     FLAG THAT I LOCKED IT                        
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FREE TABLE                                                          *         
***********************************************************************         
         SPACE 1                                                                
TABFREE  NTR1  BASE=*,LABEL=*                                                   
         LAM   R0,RF,ARZERO                                                     
         LAM   R2,R2,ALET                                                       
         L     R2,AUPDTABH                                                      
         SAC   512                                                              
         XC    0(4,R2),0(R2)                                                    
         SAC   0                                                                
         LAM   R2,R2,ARZERO                                                     
         MVI   LKFLAG,C'N'                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
*        SCAN SCREEN INPUT FIELDS FOR HELP REQUEST          *                   
***********************************************************************         
         SPACE 1                                                                
HELPSCAN NTR1  BASE=*,LABEL=*                                                   
         LA    R4,64(RA)           R4=A(FIRST FIELD)                            
         SR    R2,R2               CLEAR FIELD COUNT                            
HSCAN1   SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         BZ    HSCANX                                                           
         TM    1(R4),X'20'         TEST PROT                                    
         BNO   HSCAN3                                                           
         AR    R4,R0               NEXT FIELD                                   
         B     HSCAN1                                                           
HSCAN2   LTR   R0,R0                                                            
         BZ    HSCAN2A                                                          
         TM    HLPFLG,X'80'                                                     
         BNO   *+6                                                              
         BCTR  R1,0                                                             
         LR    R5,R0                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    0(0,R1),0(R1)                                                    
HSCAN2A  SR    R0,R0                                                            
         ICM   R0,1,0(R4)                                                       
         AR    R4,R0                                                            
         B     HSCAN1                                                           
*                                                                               
HSCAN3   EQU   *                   UNPROT FOUND                                 
         LA    R2,1(R2)            INC FIELD COUNT                              
         LA    R1,8(R4)                                                         
         SH    R0,=H'8'                                                         
         SR    R5,R5               POS COUNT ZERO                               
HSCAN4   CLI   0(R1),C'?'                                                       
         BE    HSCAN5              HELP REQUIRED                                
         CLI   PFKEY,1             CHECK FOR PFKEY 1                            
         BNE   HSCAN4A                                                          
         L     RF,CURSOR                                                        
         CR    RF,R4                                                            
         BE    HSCAN5                                                           
HSCAN4A  LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,HSCAN4                                                        
         B     HSCAN2              NEXT FIELD                                   
*                                                                               
HSCAN5   XC    HELP,HELP           CLEAR HELP                                   
         ST    R4,QHDR             SAVE ADDR                                    
         STC   R2,HLPFLD           SET FIELD NUM                                
         STC   R5,HLPPOS           POSITION                                     
         STC   R5,5(R4)            AND NEW FIELD LENGTH                         
         TM    DDSFLAG,DDSTRM                                                   
         BNO   HSCAN6                                                           
         CLI   1(R1),C'*'          DDS CAN ENTER ?*                             
         BNE   HSCAN6                                                           
         OI    HLPFLG,X'80'        AND GET DDS HELP                             
         LA    R1,1(R1)                                                         
HSCAN6   SR    R3,R3               CHECK FOR PAGE NO                            
         TM    1(R1),X'F0'                                                      
         BNO   HSCAN2                                                           
         LA    R3,1(R3)                                                         
         TM    2(R1),X'F0'                                                      
         BNO   HSCAN7                                                           
         LA    R3,1(R3)                                                         
         TM    3(R1),X'F0'                                                      
         BNO   HSCAN7                                                           
         LA    R3,1(R3)                                                         
*                                                                               
HSCAN7   BCTR  R3,0                CONVERT PAGE NO                              
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R1)                                                      
         CVB   R3,DUB                                                           
         STC   R3,HLPPAG                                                        
         B     HSCAN2                                                           
*                                                                               
HSCANX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACTION FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALACT   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,SRVACTH                                                       
         ST    R4,CURSOR                                                        
         USING FHD,R4                                                           
         XR    R1,R1                                                            
         ICM   R1,1,FHIL           INPUT?                                       
         BNZ   VACT02              YES                                          
         MVI   ACTION,ACTDIS       DEFAULT ACTION IS DISPLAY                    
         MVC   SRVACT,SR@DSP                                                    
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
*                                                                               
VACT02   BCTR  R1,0                                                             
         LA    R2,ACTTAB                                                        
         USING ACTTABD,R2                                                       
*                                                                               
VACT04   CLI   ACTLARF,0           EOT?                                         
         BE    ERR2                INVALID INPUT                                
         EX    0,ACTLARF           FIND KEYWORD                                 
         EX    R1,*+8                                                           
         BE    VACT06                                                           
         CLC   SRVACT(0),0(RF)     COMPARE KEYWORD                              
         AHI   R2,ACTTABLQ                                                      
         B     VACT04                                                           
*                                                                               
VACT06   MVC   ACTION,ACTVERB                                                   
         MVC   SRVACT,0(RF)                                                     
         OI    FHOI,FHOITR                                                      
         B     EXITOK                                                           
         DROP  R2,R4                                                            
*                                                                               
ACTTAB   DC    X'41F0',S(SR@DSP),AL1(ACTDIS),X'00'                              
         DC    X'41F0',S(DC@FREE),AL1(ACTFREE),X'00'                            
         DC    X'41F0',S(DC@STOP),AL1(ACTSTOP),X'00'                            
         DC    X'41F0',S(DC@ADD),AL1(ACTADD),X'00'                              
         DC    X'41F0',S(DC@BUILD),AL1(ACTBLD),X'00'                            
         DC    X'00'                                                            
*                                                                               
ACTTABD  DSECT                                                                  
ACTLARF  DS    XL2                                                              
ACTDD    DS    S                                                                
*                                                                               
ACTVERB  DS    X                                                                
ACTDIS   EQU   1                                                                
ACTFREE  EQU   2                                                                
ACTSTOP  EQU   3                                                                
ACTADD   EQU   4                                                                
ACTBLD   EQU   5                                                                
*                                                                               
ACTFLG   DS    X                                                                
ACTTABLQ EQU   *-ACTTABD                                                        
*                                                                               
LOCK     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE TYPE FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALTYPE  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,SRVTYPH                                                       
         ST    R4,CURSOR                                                        
         USING FHD,R4                                                           
         XR    R1,R1                                                            
         ICM   R1,1,FHIL                                                        
         BNZ   VTYP02                                                           
*&&US*&& MVI   LOCTYPE,TYPLOCK     US - DEFAULT TYPE IS LOCK                    
*&&US*&& MVC   SRVTYP,DC@LOCK                                                   
*&&UK*&& MVI   LOCTYPE,TYPSOON     UK - DEFAULT TYPE IS SOON                    
*&&UK*&& MVC   SRVTYP,DC@SOON                                                   
         B     VTYP08                                                           
*                                                                               
VTYP02   BCTR  R1,0                                                             
         LA    R2,TYPTAB                                                        
         USING TYPTABD,R2                                                       
*                                                                               
VTYP04   CLI   TYPLARF,0                                                        
         BE    ERR2                                                             
         EX    0,TYPLARF           FIND KEYWORD                                 
         EX    R1,*+8                                                           
         BE    VTYP06                                                           
         CLC   SRVTYP(0),0(RF)     COMPARE KEYWORD                              
         AHI   R2,TYPTABLQ                                                      
         B     VTYP04                                                           
*                                                                               
VTYP06   MVC   LOCTYPE,TYPVERB                                                  
         MVC   SRVTYP,0(RF)                                                     
*                                                                               
VTYP08   OI    FHOI,FHOITR                                                      
         MVC   BYTE,ACTION         CHECK COMPATIBLE WITH ACTION                 
         LA    R1,VALACTS                                                       
         BRAS  RE,VALIDATE                                                      
         BNE   ERR2                                                             
         B     EXITOK                                                           
         DROP  R2,R4                                                            
*                                                                               
TYPTAB   DC    X'41F0',S(DC@FAC),AL1(TYPFAC),X'00'                              
         DC    X'41F0',S(DC@SOON),AL1(TYPSOON),X'00'                            
         DC    X'41F0',S(DC@LOCK),AL1(TYPLOCK),X'00'                            
         DC    X'41F0',S(DC@DISK),AL1(TYPDISK),X'00'                            
         DC    X'00'                                                            
*                                                                               
TYPTABD  DSECT                                                                  
TYPLARF  DS    XL2                                                              
TYPDD    DS    S                                                                
*                                                                               
TYPVERB  DS    X                                                                
TYPFAC   EQU   1                                                                
TYPSOON  EQU   2                                                                
TYPLOCK  EQU   3                                                                
TYPDISK  EQU   4                                                                
*                                                                               
TYPFLG   DS    X                                                                
TYPTABLQ EQU   *-TYPTABD                                                        
*                                                                               
LOCK     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUB-ACTION FIELDS                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSUB   NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,18               18 SUBACTION LINES                           
         LA    R4,SRVSACTH                                                      
         USING FHD,R4                                                           
         CLC   SVTYPE,LOCTYPE      TEST TYPE IS THE SAME                        
         BNE   EXITOK                                                           
*                                                                               
         LA    R6,UPDTABS                                                       
         USING UPDTABD,R6                                                       
VSUB02   MVI   UPDTABT,0           OVERRIDE THIS WITH SUB-ACTION                
         CLI   FHIL,0                                                           
         BNE   VSUB04                                                           
         CLI   FHDA,C' '                                                        
         BNE   VSUB06                                                           
*                                                                               
VSUB04   AHI   R6,UPDTABL          BUMP TABLE ENTRY                             
         AHI   R4,L'RLINE          BUMP SCREEN ENTRY                            
         BCT   R0,VSUB02                                                        
         B     EXITOK                                                           
*                                                                               
VSUB06   XR    R1,R1               SUB ACTION FOUND                             
         IC    R1,FHIL                                                          
         BCTR  R1,0                                                             
*                                                                               
         LA    R2,SUBTAB                                                        
         USING SUBTABD,R2                                                       
VSUB08   CLI   SUBLARF,0           TEST END OF TABLE                            
         BE    VSUB12                                                           
         CLM   R1,1,SUBMINL        TEST MIN LENGTH FOR COMPARE                  
         BL    VSUB10                                                           
         EX    0,SUBLARF           FIND KEYWORD                                 
         EX    R1,*+8                                                           
         BE    VSUB14                                                           
         CLC   FHDA(0),0(RF)       COMPARE KEYWORD                              
*                                                                               
VSUB10   AHI   R2,SUBTABLQ                                                      
         B     VSUB08                                                           
*                                                                               
VSUB12   MVC   FHDA(3),SR3ERR      INVALID SUB ACTION                           
         ST    R4,CURSOR                                                        
         MVC   ERRNUM,ERROR10                                                   
         B     EXITL                                                            
*                                                                               
VSUB14   MVC   FHDA(3),SPACES      REMOVE SUBACTION                             
         OI    DDSFLAG,DDSACT                                                   
         MVC   UPDTABT,SUBVERB     STORE ACTION IN UPDFLAGS                     
         MVC   BYTE,SUBVERB        CHECK COMPATIBLE WITH TYPE                   
         LA    R1,VALSUBS                                                       
         BRAS  RE,VALIDATE                                                      
         BE    VSUB04              NEXT SUB ACTION                              
*                                                                               
         MVI   UPDTABT,0           SET ERROR CONDITION                          
         ST    R4,CURSOR                                                        
         MVC   ERRNUM,ERROR10                                                   
         B     VSUB04              NEXT                                         
         DROP  R4                                                               
*                                                                               
SUBTAB   DC    X'41F0',S(SR@RLEAS),AL1(SUBRLSE),X'00'                           
         DC    X'41F0',S(SR@PURGE),AL1(SUBPRGE),X'00'                           
         DC    X'41F0',S(SR@DEL),AL1(SUBDEL),X'00'                              
         DC    X'41F0',S(DC@UNLK),AL1(SUBUNLK),X'00'                            
         DC    X'00'                                                            
*                                                                               
SUBTABD  DSECT                                                                  
SUBLARF  DS    XL2                                                              
SUBDD    DS    S                                                                
*                                                                               
SUBVERB  DS    X                                                                
SUBRLSE  EQU   1                                                                
SUBPRGE  EQU   2                                                                
SUBDEL   EQU   3                                                                
SUBUNLK  EQU   4                                                                
*                                                                               
SUBMINL  DS    X                                                                
SUBTABLQ EQU   *-SUBTABD                                                        
*                                                                               
LOCK     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTION FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,SRVOPTH                                                       
         ST    R4,CURSOR                                                        
         USING FHD,R4                                                           
         CLI   FHIL,0              TEST NO OPTIONS                              
         BE    EXITOK                                                           
*                                                                               
         GOTO1 ASCANNER,DMCB,(R4),(8,SCANBLK)                                   
         XR    R3,R3                                                            
         ICM   R3,1,4(R1)                                                       
         BZ    ERR2                                                             
*                                                                               
         LA    R4,SCANBLK                                                       
         USING SCANBLKD,R4                                                      
*                                                                               
VOPT02   LA    R2,OPTTAB           SCAN OPTTAB FOR A MATCH                      
         USING OPTTABD,R2                                                       
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,1,SC1STLEN                                                    
         BZ    ERR2                                                             
         BCTR  R1,0                                                             
*                                                                               
VOPT04   CLI   OPTLARF,0                                                        
         BE    ERR2                                                             
         EX    0,OPTLARF           GET KEYWORD                                  
         EX    R1,*+8                                                           
         BE    VOPT06              KEYWORD FOUND                                
         CLC   SC1STFLD(0),0(RF)                                                
         AHI   R2,OPTTABLQ                                                      
         B     VOPT04                                                           
*                                                                               
VOPT06   MVC   WORK(9),0(RF)       SAVE KEYWORD                                 
         MVC   BYTE,OPTVERB        BYTE=OPTION NUMBER                           
         LA    R1,VALOPTS                                                       
         BRAS  RE,VALIDATE         TEST OPTION WITH TYPE                        
         BNE   ERR2                                                             
         L     RF,OPTRTN           RF=OPTION ROUTINE                            
         A     RF,RELO                                                          
         BASR  RE,RF               IF OK THEN EXECUTE OPTION                    
*                                                                               
         AHI   R4,SCBLKLQ          NEXT SCANNER ENTRY                           
         BCT   R3,VOPT02                                                        
         B     EXITOK                                                           
*                                                                               
         DS    0F                                                               
OPTTAB   DC    X'41F0',S(SR@DSP),A(OPDISP),X'01',X'000000'                      
         DC    X'41F0',S(SR@STAT),A(OPSTAT),X'02',X'000000'                     
         DC    X'41F0',S(DC@DDS),A(OPDDS),X'03',X'000000'                       
         DC    X'41F0',S(DC@USR),A(OPUSR),X'04',X'000000'                       
         DC    X'41F0',S(DC@TEST),A(OPTEST),X'05',X'000000'                     
         DC    X'41F0',S(DC@KEY),A(OPKEY),X'06',X'000000'                       
         DC    X'41F0',S(SR@SYS),A(OPSYS),X'07',X'000000'                       
         DC    X'41F0',S(SR@DATE),A(OPDATE),X'08',X'000000'                     
         DC    X'0000'                                                          
*                                                                               
OPTTABD  DSECT                                                                  
OPTLARF  DS    XL2                                                              
OPTDD    DS    S                                                                
OPTRTN   DS    A                                                                
*                                                                               
OPTVERB  DS    X                                                                
OPTFAC   EQU   1                                                                
OPTSOON  EQU   2                                                                
OPTLOCK  EQU   3                                                                
OPTDISK  EQU   4                                                                
*                                                                               
OPTFLG   DS    X                                                                
OPTTABLQ EQU   *-OPTTABD                                                        
*                                                                               
LOCK     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* OPTIONS ROUTINES                                                    *         
* NTRY: R4     = A(SCANBLK ENTRY)                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R4                                                      
OPDISP   NTR1  ,               *** DISP=HEX                                     
         CLI   SC2NDLEN,3                                                       
         BNE   ERR2                                                             
         CLC   =C'HEX',SC2NDFLD                                                 
         BNE   ERR2                                                             
         OI    DDSFLAG,DDSHEX                                                   
         B     EXITOK                                                           
*                                                                               
OPSTAT   NTR1  ,               *** STAT=                                        
         LA    R2,STATTAB                                                       
         USING STATABD,R2                                                       
         XR    R1,R1                                                            
         ICM   R1,1,SC2NDLEN                                                    
         BZ    ERR6                                                             
         BCTR  R1,0                                                             
*                                                                               
OPST02   CLI   STALARF,0                                                        
         BE    ERR6                                                             
         EX    0,0(RE)             SCAN STATTAB FOR STATUS                      
         EX    R1,*+8                                                           
         BE    OPST04                                                           
         CLC   SC2NDFLD(0),0(RF)                                                
         AHI   R2,STATABLQ                                                      
         B     OPST02                                                           
*                                                                               
OPST04   MVC   STAFILT,STAVERB                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
OPDDS    B     ERR2            *** NO OPTIONS YET                               
*                                                                               
OPTEST   MVC   DUB,SC2NDFLD    *** TEST=                                        
         BR    RE                                                               
*                                                                               
OPSYS    NTR1  ,               *** OPTION SYSTEM=!                              
         MVC   LKSYSN,SC2NDFLD     PUT NAME IN LKSYSN                           
         MVI   LKSYS,0                                                          
         BRAS  RE,GETSYS           MATCH SENAME TO GET SESYS                    
         BNE   ERR16               INVALID SYSTEM                               
         MVC   SYSFILT,LKSYSSE                                                  
         B     EXITOK                                                           
*                                                                               
OPDATE   NTR1  ,               *** OPTION DATE=                                 
         CLC   SC2NDFLD,SR@TODAY                                                
         BNE   OPDT02                                                           
         MVC   DATFILT,TODAY                                                    
         B     EXITOK                                                           
*                                                                               
OPDT02   GOTO1 APERVAL,DMCB,(SC2NDLEN,SC2NDFLD),(X'60',MSG)                     
         CLI   4(R1),4                                                          
         BNE   ERR17                                                            
         GOTO1 ADATCON,DMCB,(2,MSG+34),(19,DATFILT)                             
         B     EXITOK                                                           
*                                                                               
OPUSR    NTR1  ,               *** U=ALL                                        
         XC    PQUSER,PQUSER       TEST FOR U=ALL                               
         CLI   SC2NDLEN,3                                                       
         BNE   *+14                                                             
         CLC   =C'ALL',SC2NDFLD                                                 
         BE    EXITOK                                                           
*                                                                               
         L     R3,ACIREC                                                        
         USING CTIREC,R3           GET USERID RECORD                            
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SC2NDFLD                                                  
         GOTO1 ADMGR,DMCB,DMREAD,CTFILE,(R3),(R3)                               
         TM    8(R1),X'92'                                                      
         BNZ   ERR5                INVALID USER                                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,CTIDATA                                                       
         XR    RF,RF                                                            
OPUS02   CLI   0(R3),0             SCAN FOR DESCRIPTION ELEMENT                 
         BE    ERR5                                                             
         CLI   0(RF),X'02'                                                      
         BE    OPUSR02                                                          
         ICM   R0,1,1(RF)                                                       
         BZ    ERR5                IF NO USER NUMBER 'INVALID USERID'           
         BXH   R3,RF,OPUS02                                                     
*                                                                               
OPUSR02  MVC   PQUSER,2(R3)        SET NEW USERID                               
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
OPKEY    NTR1  ,               *** KEY=SENAME,AG,TY,KEYDATA                     
         LA    RF,KEY                                                           
         USING LKKEYD,RF                                                        
         XC    LOCKKEY,LOCKKEY                                                  
         MVC   LKSYSN(7),SC2NDFLD  PUT NAME IN LKSYSN                           
         MVI   LKSYS,0                                                          
         BRAS  RE,GETSYS           MATCH SENAME TO GET SESYS                    
         BNE   ERR15               INVALID KEY                                  
*                                                                               
         MVC   LOCKSE,LKSYSSE                                                   
         BRAS  RE,OPKEYNXT                                                      
         CLI   SC1STLEN,0          ZERO=ALL AGENCYS                             
         BE    OPK02                                                            
         CLI   SC1STLEN,2          ELSE MUST BE 2 BYTES                         
         BNE   ERR15                                                            
         MVC   LOCKAGY,SC1STFLD                                                 
*                                                                               
OPK02    BRAS  RE,OPKEYNXT                                                      
         CLI   0(R4),0             ZERO=ALL TYPES                               
         BE    OPKEY2                                                           
         CLI   0(R4),2             ELSE MUST BE 2                               
         BNE   ERR15                                                            
         MVC   LOCKRTY,12(R4)                                                   
*                                                                               
OPKEY2   BRAS  RE,OPKEYNXT                                                      
         CLI   SC1STLEN,10                                                      
         BH    ERR15                                                            
         CLI   SC1STLEN,0                                                       
         BE    OPKEYX                                                           
         XR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LOCKKEY(0),SC1STFLD                                              
         B     OPKEYX                                                           
*                                                                               
OPKEYNXT AHI   R4,SCBLKLQ          NEXT SCANNER ENTRY                           
         AHI   R3,-1                                                            
         BNP   ERR15               COUNT NUMBER OF ENTRYS                       
         BR    RE                                                               
         DROP  R4,RF                                                            
*                                                                               
OPKEYX   XIT1  REGS=(R3,R4)                                                     
         EJECT                                                                  
***********************************************************************         
* READ IN SAVED STORAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
READSTR  NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,SRPAGENO         READ IN TWA11                                
         SLL   R0,32-8                                                          
         ICM   R0,3,TRM                                                         
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),RECLEN                                                
         GOTO1 ADMGR,DMCB,(X'80',DMREAD),TEMPSTR,(R0),ASAVE                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(24),DMCB       REMOVE THE SPECIAL BITS                      
         CLC   IDENT,$LOC                                                       
         BE    EXITOK                                                           
         LA    R0,SAVEDSTR         XC SAVE STORAGE                              
         LHI   R1,SAVEDL                                                        
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* WRITE OUT SAVED STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
WRITESTR NTR1  BASE=*,LABEL=*                                                   
         MVC   IDENT,$LOC                                                       
         LHI   R0,SRPAGENO                                                      
         SLL   R0,32-8                                                          
         ICM   R0,3,TRM                                                         
         GOTO1 ADMGR,DMCB,DMWRT,TEMPSTR,(R0),ASAVE                              
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CHECK TYPE VS ACTION/SUBACTION/OPTION                               *         
* NTRY: R1     = A(TABLE)                                             *         
*       BYTE   = VALUE TO VALIDATE                                    *         
* EXIT: CC EQ  = TYPE IS VALID                                        *         
***********************************************************************         
         SPACE 1                                                                
VALIDATE NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VDT02    CLI   0(R1),X'FF'         EOT?                                         
         BE    EXITL                                                            
         CLC   LOCTYPE,0(R1)       FIND ENTRY FOR TYPE                          
         BE    VDT04                                                            
         LA    R1,1(R1)                                                         
         CLI   0(R1),0                                                          
         BNE   *-8                                                              
         LA    R1,1(R1)                                                         
         B     VDT02                                                            
*                                                                               
VDT04    LA    R1,1(R1)            TEST VALID ENTRIES WITH BYTE                 
         CLI   0(R1),0                                                          
         BE    EXITL               NOT FOUND                                    
         CLC   BYTE,0(R1)                                                       
         BNE   VDT04                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCAN PRINT QUEUE INDEX FOR UPDATIVE SOON REPORTS                    *         
***********************************************************************         
         SPACE 1                                                                
SCAN00   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,UPDTABS                                                       
         USING USAVED,R6                                                        
         LA    R4,NDX                                                           
         USING UKRECD,R4                                                        
         LR    R0,R6               XC SAVE TABLE                                
         LHI   R1,L'UPDTABS                                                     
         MH    R1,=H'18'                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    R5,CXREC                                                         
         XC    COUNT,COUNT         CLEAR COUNT                                  
         MVC   SVTYPE,LOCTYPE      INDICATE JOBS DISPLAY                        
         XC    UKINDEX,UKINDEX                                                  
*                                                                               
SCAN     GOTO1 ADMGR,DMCB,(X'00',INDEX),PRTQID,UKINDEX,SAVE,(R5)                
         CLI   8(R1),X'90'         EOF                                          
         BE    SCANX                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
SCAN1    OC    UKKEY,UKKEY         TEST IF PURGED                               
         BZ    SCAN                                                             
         OC    PQUSER,PQUSER       TEST U=ALL                                   
         BZ    *+14                                                             
         CLC   UKSRCID,PQUSER      ELSE USERIDS MUST MATCH                      
         BNE   SCAN                                                             
         TM    UKTYPE,PQTYUPDT     TEST UPDATIVE SOON                           
         BZ    SCAN                                                             
*                                                                               
         MVC   USDUSR,UKSRCID      SAVE REPORT DETAILS                          
         MVC   USDRID,UKSUBID                                                   
         MVC   USDSEQ,UKREPNO                                                   
         MVC   USSTAT,UKSTAT                                                    
         MVC   USATTB,UKATTB                                                    
         MVC   USSORT+0(2),UKAGELD                                              
         MVC   USSORT+2(2),UKAGELT                                              
         LA    R6,UPDTABL(R6)      NEXT                                         
*                                                                               
         LH    R1,COUNT                                                         
         LA    R1,1(R1)            BUMP COUNT                                   
         STH   R1,COUNT                                                         
         B     SCAN                                                             
SCANX    XC    0(UPDTABL,R6),0(R6)                                              
         LA    R6,UPDTABS                                                       
         LH    R1,COUNT                                                         
         ST    R1,DMCB+4                                                        
         GOTO1 ASORT,DMCB,(R6),,UPDTABL,5,USSORT-USAVED                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* GET LOCK= CARD                                                      *         
***********************************************************************         
         SPACE 1                                                                
GETLOCK  NTR1  ,                                                                
         XC    LKDTAIL,LKDTAIL                                                  
         XC    NDX,NDX                                                          
         MVC   CIRSN,USDSEQ                                                     
         BRAS  RE,RSNXPE                                                        
         BRAS  RE,GETCAD                                                        
         L     R5,ACIREC            INITIALISE CIREC                            
         USING PQRECD,R5                                                        
         GOTO1 ADMGR,DMCB,(X'00',BUFFER),PRTQID,NDX,SAVE,(R5)                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,8(R5)                                                         
         AR    RE,R5                                                            
         MVC   SKFSTCI-SKBUFFD(4,RE),CIADDR                                     
         MVC   SKINTNO-SKBUFFD(1,RE),CFPQINUM                                   
         MVC   SKEXTNO-SKBUFFD(1,RE),BUPQFILE                                   
GLOCK1   GOTO1 ADMGR,DMCB,(X'01',READ),PRTQID,NDX,SAVE,(R5)                     
         MVC   PQRLEN,22(R1)                                                    
         CLI   8(R1),0                                                          
         BE    GLOCK2                                                           
         TM    8(R1),X'80'         TEST EOF                                     
         BO    *+6                                                              
         DC    H'0'                SOME OTHER ERROR                             
         MVCDD MSG(9),SR#NONE EOF                                               
         B     GLOCKX                                                           
GLOCK2   TM    LKFLAG,FLGCARD+FLGLOCK                                           
         BO    GLOCK3              MUST HAVE LOCK= & REQ CARD                   
         CLC   SAVE+1(2),=C'/*'                                                 
         BNE   *+8                                                              
         OI    LKFLAG,FLGCARD                                                   
         CLC   SAVE+1(5),=C'LOCK='                                              
         BNE   GLOCK1                                                           
*                                                                               
         MVI   MSG,C' '            CREATE SCANNER CARD IN MSG                   
         MVC   MSG+1(L'MSG-1),MSG                                               
         MVC   MSG(80),SAVE+6                                                   
         OI    LKFLAG,FLGLOCK                                                   
         B     GLOCK1              GO BACK FOR REQ CARD                         
*                                                                               
GLOCK3   MVC   REQUEST,SAVE+1      SAVE REQUEST CARD                            
         GOTO1 ASCANNER,DMCB,(C'C',MSG),SCANBLK                                 
         MVI   MSG,C' '            CLEAR MSG                                    
         MVC   MSG+1(L'MSG-1),MSG                                               
         MVC   BYTE,4(R1)                                                       
         LA    RF,SCANBLK                                                       
         LA    R2,PHSTAB           FIND PHASE FOR SYSTEM                        
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
GLOCK3A  CLC   12(0,RF),0(R2)      TEST SYSTEM NAME                             
         BE    GLOCK4                                                           
         LA    R2,12(,R2)                                                       
         CLI   0(R2),0                                                          
         BNE   GLOCK3A                                                          
         DC    H'0'                INVALID SYSTEM                               
*                                                                               
GLOCK4   CLC   PHASE,7(R2)         SAME PHASE AS BEFORE ?                       
         BE    *+10                                                             
         XC    APHASE,APHASE       NO CLEAR PHASE ADDR                          
         MVC   PHASE,7(R2)         SAVE PHASE                                   
         MVC   USDEXT1,8(R2)       SET DEFAULT FILES                            
         MVC   USDEXT2,9(R2)                                                    
         MVC   LKSYS,10(R2)        SET OV SYSTEM                                
         LA    RF,32(RF)                                                        
         LA    R2,FILTAB                                                        
         USING FILTABD,R2                                                       
GLOCK5   CLI   1(RF),0             GET FILES OR RULE                            
         BNE   GLOCK10                                                          
         SR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         BCTR  R1,0                                                             
GLOCK7   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   12(0,RF),DMFLNAME   TEST FILE NAME                               
         BE    GLOCK8                                                           
         LA    R2,DMFLLEN(,R2)                                                  
         CLI   DMFLNUM,0                                                        
         BNE   GLOCK7                                                           
         DC    H'0'                INVALID FILENAME IN JCL                      
GLOCK8   LA    RF,32(RF)                                                        
         OC    LKFILE1,LKFILE1                                                  
         BNZ   *+12                                                             
         ST    R2,LKFILE1          SAVE FILE POINTERS                           
         B     GLOCK5                                                           
         ST    R2,LKFILE2                                                       
         B     GLOCK5              GET NEXT FILE OR RULE                        
*                                                                               
GLOCK10  OC    LKFILE1,LKFILE1     IF ZERO USE DEFAULTS                         
         BZ    GLOCK11                                                          
         L     R1,LKFILE1          ELSE GET LOCK=VALUES                         
         MVC   USDEXT1,0(R1)                                                    
         L     R1,LKFILE2                                                       
         MVC   USDEXT2,0(R1)                                                    
*                                                                               
GLOCK11  ST    RF,ASCANBLK                                                      
         ST    R6,UPDENTRY                                                      
         BRAS  RE,GETPHASE                                                      
         L     RF,APHASE                                                        
         LR    R1,RC                                                            
         MVI   CALL,C'L'           GET LOCK DETAILS                             
         BASR  RE,RF                                                            
         MVI   CALL,C'D'           THEN DISPLAY THEM                            
         BASR  RE,RF                                                            
         MVC   LKSYSSE,USDSENUM                                                 
GLOCKX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET LKSYSSE AND LKAGB FROM LKAGY                                    *         
* NTRY: R3     = A(REPLINE)                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING REPLINE,R3                                                       
GETSE    NTR1  BASE=*,LABEL=*                                                   
         CLC   LKAGY,SAVAGY        TRY SAVE DATA FIRST                          
         BNE   GSE02                                                            
         MVC   LKSYSSE,SAVSE                                                    
         MVC   LKAGB,SAVAGB                                                     
         B     EXITOK                                                           
*                                                                               
GSE02    L     R4,ACIREC                                                        
*        L     R4,SRQATIA-SRPARMD(R1)                                           
         USING CT5REC,R4                                                        
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,LKAGY                                                   
         GOTO1 ADMGR,DMCB,DMREAD,CTFILE,(R4),(R4)                               
         CLI   8(R1),0                                                          
         BNE   GETSX                                                            
         MVC   SAVAGY,LKAGY                                                     
*                                                                               
         LA    R4,CT5DATA                                                       
         USING CTSYSD,R4                                                        
         SR    R0,R0                                                            
GSE04    CLI   0(R4),X'21'         SCAN FOR SYSTEM ELEMENT                      
         BE    GSE08                                                            
GSE06    ICM   R0,1,1(R4)                                                       
         BZ    GETSX                                                            
         AR    R4,R0                                                            
         B     GSE04                                                            
*                                                                               
GSE08    CLC   CTSYSNUM,LKSYS      TEST FOR OV SYSTEM                           
         BNE   GSE06                                                            
         MVC   LKSYSSE,CTSYSSE     SAVE SENUM                                   
         MVC   SAVSE,CTSYSSE                                                    
         MVC   LKAGB,CTSYSAGB      SAVE AGENCY/COMPANY BINARY                   
         MVC   SAVAGB,CTSYSAGB                                                  
*                                                                               
GETSX    L     RE,SAVERE                                                        
         CLI   LKSYSSE,0                                                        
         BNE   GETSXX              OK EXIT                                      
         XC    RLREPFIL,RLREPFIL                                                
         MVCDD RLREPFIL(9),SR#ERROR                                             
GETSXX   B     EXITOK              ERROR EXIT                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* CONVERT TIME TO DISPLAY FORMAT IN WORK(8)                           *         
***********************************************************************         
         SPACE 1                                                                
TIMCONV  NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(8),SPACES                                                   
         OC    FULL(3),FULL                                                     
         BZ    EXITOK                                                           
         MVI   FULL+3,X'0C'                                                     
         UNPK  DUB,FULL                                                         
         MVC   WORK+0(2),DUB+1                                                  
         MVC   WORK+3(2),DUB+3                                                  
         MVC   WORK+6(2),DUB+5                                                  
         MVI   WORK+2,C':'                                                      
         MVI   WORK+5,C':'                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET USERID FROM SAVUSER                                             *         
***********************************************************************         
         SPACE 1                                                                
GETUSR   NTR1  BASE=*,LABEL=*                                                   
         MVC   USERID,SR@UNKNW     PREPARE FOR UNKNOWN ID                       
         L     R4,ACIREC                                                        
         USING CTIREC,R4                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),SAVUSER                                              
         GOTO1 ADMGR,DMCB,DMREAD,CTFILE,(R4),(R4)                               
         CLI   8(R1),0                                                          
         BNE   EXIT                                                             
*                                                                               
         LA    R4,CTIDATA                                                       
         SR    R0,R0                                                            
GETUSR1  CLI   0(R4),X'02'         SCAN FOR ID ELEMENT                          
         BE    GETUSRX                                                          
         ICM   R0,1,1(R4)                                                       
         BZ    EXIT                                                             
         AR    R4,R0                                                            
         B     GETUSR1                                                          
*                                                                               
GETUSRX  MVC   USERID,2(R4)        SAVE USERID                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* LOAD OV PHASE                                                       *         
***********************************************************************         
         SPACE 1                                                                
GETPHASE NTR1  BASE=*,LABEL=*                                                   
         OC    APHASE,APHASE                                                    
         BNZ   EXITOK                                                           
         GOTO1 ACALLOV,DMCB,(PHASE,0),0                                         
         MVC   APHASE,0(R1)                                                     
         CLI   4(R1),X'FF'                                                      
         BNE   EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* GET LKSYS & LKSYSN FROM LKSYSSE OR VICE VERSA                       *         
***********************************************************************         
         SPACE 1                                                                
GETSYS   NTR1  BASE=*,LABEL=*                                                   
         L     R1,ASYSFACS                                                      
         L     R4,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R4)                                                         
         L     RF,2(R4)                                                         
         LA    R4,6(R4)                                                         
         USING SELISTD,R4                                                       
*                                                                               
GETSYS0  CLI   LKSYSSE,0           IF LKSYSSE=ZERO MATCH ON NAME                
         BNE   GETSYS1                                                          
         CLC   LKSYSN,SENAME                                                    
         BE    GETSYS4                                                          
         BNE   GETSYS2                                                          
GETSYS1  CLC   LKSYSSE,SESYS                                                    
         BE    GETSYS3                                                          
GETSYS2  BXLE  R4,RE,GETSYS0                                                    
         MVI   LKSYS,0                                                          
         XC    LKSYSN,LKSYSN                                                    
         B     EXITL                                                            
*                                                                               
GETSYS3  MVC   LKSYS,SEOVSYS                                                    
         MVC   LKSYSN,SENAME                                                    
         B     EXITOK                                                           
*                                                                               
GETSYS4  MVC   LKSYS,SEOVSYS                                                    
         MVC   LKSYSSE,SESYS                                                    
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SWITCH TO CONTROL FOR CTFILE UPDATES                                *         
***********************************************************************         
         SPACE 1                                                                
CTSWIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ASWITCH,DMCB,X'0AFFFFFF',0  SWITCH TO CON                        
         OI    DDSFLAG,DDSSWIT                                                  
         CLI   4(R1),0             SET CC=EQU IF GOOD SWITCH                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SWITCH BACK TO SERVICE SYSTEM                                       *         
***********************************************************************         
         SPACE 1                                                                
SESWIT   NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ASWITCH,DMCB,X'01FFFFFF',0  SWITCH BACK                          
         NI    DDSFLAG,255-DDSSWIT                                              
         CLI   4(R1),0             SET CC=EQU IF GOOD SWITCH                    
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP PRTQFILE                                                     *         
***********************************************************************         
         SPACE 1                                                                
SETPRTQ  NTR1  BASE=*,LABEL=*                                                   
         MVC   PRTQID,=CL8'PRTQUE'                                              
         LA    R1,L'PQINDEX                                                     
         STH   R1,CINDXLN                                                       
         L     R1,AENQDEQ                                                       
         ST    R1,CIENQDEQ                                                      
         LA    R4,NDX                                                           
         USING UKRECD,R4                                                        
         LA    R5,CXREC                                                         
         GOTO1 ADMGR,DMCB,(X'00',GLIST),PRTQID,NDX,SAVE,(R5)                    
         L     RE,UKUSRINF                                                      
         CLI   0(RE),0             NUMBER OF PRTQ FILES                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),16            CAN BE FROM 1 - 16                           
         BNH   *+6                                                              
         DC    H'0'                                                             
         SR    R1,R1               R1=NUM OF FILES IN LIST                      
         IC    R1,0(RE)                                                         
         LA    R1,2(R1)            ADD TWO FOR HDR AND TRL                      
         SLL   R1,3                                                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PRTQLST(0),0(RE)    COPY PRTQ LIST TO MY OWN AREA                
         XC    PRTQLSTX,PRTQLSTX   SET END OF MAXIMUM LIST                      
*                                                                               
         MVC   UKSRCID,PQUSER      GET PRTQ ID FOR THIS USER                    
         GOTO1 ADMGR,DMCB,(X'00',GFILE)                                         
         MVC   PRTQID,UKUSRINF                                                  
         GOTO1 ADMGR,DMCB,(X'00',BUFFER)                                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,CXREC                                                         
         MVC   BUFFDATA,0(R1)                                                   
         MVC   CIDATA,12(R1)       SAVE FILE DATA FOR THIS PRTQ                 
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISATION                                                      *         
* NTRY: R1     = A(SRPARMS)                                           *         
***********************************************************************         
         SPACE 1                                                                
INIT     NTR1  BASE=*,LABEL=*                                                   
         L     RF,ASYSFACS                                                      
         USING SYSFACD,RF          R2=A(SYSFACS)                                
         MVC   ADMGR,VDATAMGR      SAVE SYSFACS ADDRS                           
         MVC   AENQDEQ,VENQDEQ                                                  
         MVC   ACALLOV,VCALLOV                                                  
         MVC   ATICTOC,VTICTOC                                                  
         MVC   ASSB,VSSB                                                        
         MVC   AUPD,VUPDTAB                                                     
         DROP  RF                                                               
*                                                                               
         L     RF,ACOMFACS         SAVE COMFACS ENTRYS                          
         USING COMFACSD,RF                                                      
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   ADICTATE,CDICTATE                                                
         MVC   AGETTXT,CGETTXT                                                  
         MVC   ASCANNER,CSCANNER                                                
         MVC   ADATCON,CDATCON                                                  
         MVC   ASWITCH,CSWITCH                                                  
         MVC   ALOCKET,CLOCKET                                                  
         MVC   APERVAL,CPERVAL                                                  
         DROP  RF                                                               
*                                                                               
         L     RF,ASSB             SAVE SSB DETAILS                             
         USING SSBD,RF                                                          
         MVC   RECLEN,SSBTWAL                                                   
         XC    ALET,ALET                                                        
         TM    SSBSTAT4,SSBDSLCK                                                
         BZ    *+10                                                             
         MVC   ALET,SSBALET                                                     
         DROP  RF                                                               
*                                                                               
         ICM   RF,15,ALET          LOCAL UPDATE TABLE?                          
         BNZ   INIT02              NO                                           
*                                                                               
         L     RF,AUPD                                                          
         AHI   RF,-6                                                            
         ST    RF,AUPDTABH         LOCAL UPDATE TABLE LOCK TOKEN                
         AHI   RF,6                                                             
         MVC   AUPDLEN,0(RF)                                                    
         MVC   AUPDTABX,2(RF)                                                   
         AHI   RF,6                                                             
         ST    RF,AUPDTAB                                                       
         B     INIT04                                                           
         DROP  R2                                                               
*                                                                               
INIT02   LAM   R0,RF,ARZERO        DSPACE UPDATE TABLE                          
         LAM   R2,R2,ALET                                                       
         XR    R2,R2                                                            
         SAC   512                                                              
         ICM   R2,15,DHALOCT-DMDSHDR(R2)                                        
         ST    R2,AUPDTABH                                                      
         USING LOCTABD,R2                                                       
         MVC   AUPDLEN,LOCLEN                                                   
         MVC   AUPDTABX,LOCEND                                                  
         LA    RF,LOCFST                                                        
         ST    RF,AUPDTAB                                                       
         SAC   0                                                                
         LAM   R2,R2,ARZERO                                                     
         DROP  R2                                                               
*                                                                               
INIT04   L     RF,AUTL             SAVE UTL DETAILS                             
         USING UTLD,RF             R2=A(UTL)                                    
         MVC   PQUSER,TUSER                                                     
         MVC   TRMUSER,TUSER                                                    
         MVC   TRM,TNUM                                                         
         TM    TSTAT1,TSTATDDS                                                  
         BZ    *+8                                                              
         OI    DDSFLAG,DDSTRM                                                   
         TM    TSVCREQ,X'01'       TEST FIRST TIME                              
         BNO   *+8                                                              
         OI    TSVCREQ,X'02'       SET CURSOR FLAG                              
         DROP  RF                                                               
*                                                                               
         L     RF,ATIOB            SAVE TIOB DETAILS                            
         MVC   PFKEY,TIOBAID-TIOBD(RF)                                          
         MVC   HALF,TIOBCURD-TIOBD(RF)                                          
         L     RF,ATWA                                                          
         AH    RF,HALF                                                          
         ST    RF,CURSOR                                                        
*                                                                               
         LHI   RF,CIREC-WORKD      GET OUT OF RANGE WORK                        
         AR    RF,RC                                                            
         ST    RF,ACIREC                                                        
         LHI   RF,CXREC-WORKD                                                   
         AR    RF,RC                                                            
         ST    RF,ACXREC                                                        
         LHI   RF,IOAREA-WORKD                                                  
         AR    RF,RC                                                            
         ST    RF,AIOAREA                                                       
*                                                                               
         LHI   RF,GETSE-LOCK       SAVE A(COMMON ROUTINES)                      
         AR    RF,RB                                                            
         ST    RF,AGETSE                                                        
*                                                                               
         L     RF,ASELIST                                                       
         USING SELISTD,RF          SAVE SEFILES ADDR                            
         L     RF,SEFILES                                                       
         MVC   AFILETAB,0(RF)                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 ACALLOV,DMCB,0,X'D9A00A0D'                                       
         MVC   ASQUASH,0(R1)       GET A(SQUASHER)                              
         GOTO1 (RF),(R1),0,X'D9A00A12'                                          
         MVC   ASORT,0(R1)         GET A(XSORT)                                 
*                                                                               
         GOTO1 ADICTATE,DMCB,C'LU  ',DDDCLST,DDDSLST                            
         MVC   HELPKEY,HELPID                                                   
*                                                                               
         TIME                                                                   
         STCM  R0,14,LKTIME        GET TIME HH:MM:SS                            
         SRL   R1,4                                                             
         STCM  R1,7,TODAY                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS CONSTANTS AND HANDY ROUTINES ALL ADDRESSED FROM R8         *         
***********************************************************************         
         SPACE 1                                                                
LITERALS DS    0D                                                               
         EJECT                                                                  
***********************************************************************         
* INFO MESSAGES                                                       *         
***********************************************************************         
         SPACE 1                                                                
INFO1    DC    H'203'              UPDATE TABLE ENTRIES DISPLAYED               
INFO2    DC    H'253'              LOCK TABLE ENTRIES DISPLAYED                 
INFO3    DC    H'254'              LOCKET RECORDS DISPLAYED                     
INFO4    DC    H'255'              LOCK TABLE ENTRIES DISPLAYED - NEXT          
INFO5    DC    H'256'              LOCKET RECORDS DISPLAYED - NEXT              
*                                                                               
ERROR10  DC    H'57'               INVALID SUB ACTION                           
*                                                                               
INFO     TM    DDSFLAG,DDSLOCK                                                  
         BNO   *+8                                                              
         BRAS  RE,TABFREE                                                       
         TM    DDSFLAG,DDSSWIT     SWITCH BACK IF SWITCHED                      
         BNO   *+8                                                              
         BRAS  RE,SESWIT                                                        
         SAC   0                                                                
         LAM   R0,RF,ARZERO                                                     
*                                                                               
         CLI   HLPFLD,0            CHECK FOR HELP EXIT                          
         BNE   HELPOUT                                                          
         XR    R0,R0                                                            
         ICM   R0,3,ERRNUM         TEST FOR INTERNAL ERROR                      
         BNZ   ERRX                                                             
         XR    R0,R0                                                            
         ICM   R0,3,INFONUM        ELSE DISPLAY INFO MESSAGE                    
         BZ    XMOD                                                             
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 AGETTXT,DMCB,(R0),0,(C'I',0),,,X'00010000'                       
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* ERROR MESSAGES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ERR0     LA    R0,SREMBC           MUST BE CONNECTED                            
         B     ERRX                                                             
ERR1     LA    R0,SREMIF           MISSING INPUT FIELD                          
         B     ERRX                                                             
ERR2     LA    R0,SREIIF           INVALID INPUT FIELD                          
         B     ERRX                                                             
ERR3     LA    R0,SRERNF           REPORT NOT FOUND                             
         B     ERRX                                                             
ERR4     LA    R0,SREIPN           INVALID PAGE NUMBER                          
         B     ERRX                                                             
ERR5     LA    R0,SREUID           INVALID USERID                               
         B     ERRX                                                             
ERR6     LA    R0,51               INVALID STATUS                               
         B     ERRX                                                             
ERR7     LA    R0,162              QUEUE HAS CHANGED                            
         B     ERRX                                                             
ERR8     LA    R0,66               NOTHING TO DISPLAY                           
         B     ERRX                                                             
ERR9     LA    R0,261              TABLE FULL                                   
         B     ERRX                                                             
ERR10    LA    R0,57               INVALID SUB ACTION                           
         B     ERRX                                                             
ERR11    LA    R0,114              LOCKTAB BUSY                                 
         B     ERRX                                                             
ERR12    LA    R0,132              LOCKING IS INHIBITED                         
         B     ERRX                                                             
ERR13    LA    R0,273              KEY REQUIRED                                 
         B     ERRX                                                             
ERR14    LA    R0,274              KEY IS ALREADY LOCKED                        
         B     ERRX                                                             
ERR15    LA    R0,275              INVALID KEY                                  
         B     ERRX                                                             
ERR16    LA    R0,32               INVALID SYSTEM                               
         B     ERRX                                                             
ERR17    LA    R0,54               INVALID VALUE FOR OPTION KEYWORD=            
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB+12,9                                                        
         LA    R1,WORK             OPTION KEYWORD SHOULD BE IN WORK             
         STCM  R1,7,DMCB+13                                                     
         B     ERRXX                                                            
*                                                                               
ERRX     XC    DMCB(24),DMCB                                                    
*                                                                               
ERRXX    TM    DDSFLAG,DDSSWIT     SWITCH BACK IF SWITCHED                      
         BNO   *+8                                                              
         BRAS  RE,SESWIT                                                        
         TM    DDSFLAG,DDSLOCK     UNLOCK IF LOCKED                             
         BNO   *+8                                                              
         BRAS  RE,TABFREE                                                       
         SAC   0                                                                
         LAM   R0,RF,ARZERO        PROTECTION                                   
         CLI   HLPFLD,0            CHECK FOR HELP EXIT                          
         BNE   HELPOUT                                                          
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 AGETTXT,DMCB,(R0),0,(C'E',0),,,X'00010000'                       
         B     XMOD                                                             
         EJECT                                                                  
***********************************************************************         
* EXIT POINTS                                                         *         
***********************************************************************         
         SPACE 1                                                                
EXITOK   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,255                                                            
         B     EXIT                                                             
*                                                                               
XMOD     MVCDD SRVPFK,SR#LOC09     LOCKING IS ENABLED                           
         BRAS  RE,LKQBIT                                                        
         BNO   *+10                                                             
         MVCDD SRVPFK,SR#LOC08     LOCKING IS INHIBITED                         
         BRAS  RE,WRITESTR         WRITE SAVED STORAGE                          
         L     R1,CURSOR           SET CURSOR                                   
         OI    6(R1),X'40'                                                      
         L     RD,SAVERD                                                        
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*************************************************************                   
*        CHECK LOCKET RETURN CODES                          *                   
*************************************************************                   
         SPACE 1                                                                
LOCKETR  CLI   4(R1),0             GOOD RETURN                                  
         BER   RE                                                               
         CLI   4(R1),1             KEY IS ALREADY LOCKED                        
         BE    ERR14                                                            
         CLI   4(R1),2             LOCKTAB IS BUSY                              
         BE    ERR11                                                            
         CLI   4(R1),3             LOCKTAB IS FULL                              
         BE    ERR9                                                             
         CLI   4(R1),4             LOCKING IS INHIBITED                         
         BE    ERR12                                                            
         CLI   4(R1),5             KEY NOT FOUND FOR UNLOCK                     
         BE    ERR15                                                            
         CLI   4(R1),6             CONTROL NOT STARTED                          
         BE    ERR15               (MESSAGE CAN'T BE READ ANYWAY)               
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* CALL GETHELP AND EXIT                                               *         
***********************************************************************         
         SPACE 1                                                                
HELPOUT  LA    R1,HLPIND6                                                       
         CLI   HLPFLD,6            CHECK FOR SEL FIELDS                         
         BNL   HELP010                                                          
         SR    RF,RF                                                            
         IC    RF,HLPFLD           READ HELP FIELD                              
         SLL   RF,2                                                             
         EX    0,HLPIND0(RF)       SELECT WHICH TABLE                           
         B     HELP010                                                          
HLPIND0  DC    XL4'00'             NO FIELD NUMBER                              
         LA    R1,HLPIND1                                                       
         LA    R1,HLPIND2                                                       
         LA    R1,HLPIND3                                                       
         LA    R1,HLPIND4                                                       
         LA    R1,HLPIND5                                                       
*                                                                               
HELP010  CLC   0(1,R1),ACTION      MATCH ACTION                                 
         BE    HELP020                                                          
         CLI   0(R1),X'FF'         FF MATCHES ANY                               
         BNE   HELP040                                                          
HELP020  CLC   1(1,R1),LOCTYPE     MATCH TYPE                                   
         BE    HELP030                                                          
         CLI   1(R1),X'FF'         FF MATCHES ANY                               
         BNE   HELP040                                                          
*                                                                               
HELP030  TM    3(R1),X'80'         DDS PANEL ?                                  
         BNO   HELP050                                                          
         TM    HLPFLG,X'80'                                                     
         BO    HELP050                                                          
HELP040  LA    R1,4(R1)            NEXT ENTRY                                   
         B     HELP010                                                          
*                                                                               
HELP050  MVC   HELPNUM,3(R1)                                                    
         MVC   HELPPAG,HLPPAG                                                   
         L     R1,QHDR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
         XC    DMCB(24),DMCB                                                    
         L     RF,ACOMFACS                                                      
         L     RF,CGETHELP-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(X'50',HELPKEY),QHDR,(C'B',0)                          
         DC    H'0'                GETHELP EXITS TO MONITOR                     
         EJECT                                                                  
***********************************************************************         
* PRINT QUEUE ROUTINES                                                *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE DMPRTQR                                                        
         EJECT                                                                  
***********************************************************************         
* LITERAL POOL                                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF PHASES FOR LOCKOUTS                                        *         
***********************************************************************         
         SPACE 1                                                                
PHSTAB   DS    0L                                                               
         DC    CL7'MEDIA  ',X'01',X'4142',X'04',X'00',A(0)                      
         DC    CL7'CONTROL',X'02',X'0000',X'0A',X'00',A(0)                      
         DC    X'00'                                                            
*                                                                               
PHSTABD  DSECT                                                                  
PHSNAME  DS    CL7                 KEYWORD                                      
PHSPHS   DS    XL1                 PHASE                                        
PHSFL1   DS    XL1                 FILE1                                        
PHSFL2   DS    XL1                 FILE2                                        
PHSSYS   DS    XL1                 SYSTEM                                       
         DS    XL1                 N/D                                          
         DS    A                   N/D                                          
PHSTABLQ EQU   *-PHSTABD                                                        
*                                                                               
LOCK     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0H                                                               
         DC    CL16'*STATTAB*STATTAB'                                           
STATTAB  DS    0CL6                                                             
         DC    X'41F0',S(SR@PRGED),AL1(0),X'00'                                 
         DC    X'41F0',S(SR@HOLD),AL1(1),X'00'                                  
         DC    X'41F0',S(SR@QUED),AL1(2),X'00'                                  
         DC    X'41F0',S(SR@RLESD),AL1(3),X'00'                                 
         DC    X'41F0',S(SR@RNING),AL1(4),X'00'                                 
         DC    X'41F0',S(SR@UPD),AL1(5),X'00'                                   
         DC    X'41F0',S(SR@READY),AL1(6),X'00'                                 
         DC    X'41F0',S(SR@ERROR),AL1(7),X'00'                                 
         DC    X'41F0',S(SR@DEL),AL1(8),X'00'                                   
         DC    X'41F0',S(DC@LOCKD),AL1(9),X'00'                                 
         DC    X'41F0',S(DC@UNLK),AL1(10),X'00'                                 
         DC    X'00'                                                            
*                                                                               
STATABD  DSECT                                                                  
STALARF  DS    XL2                 LA RF                                        
STADD    DS    S                   S(TEXT)                                      
*                                                                               
STAVERB  DS    X                   STATUS VERB                                  
STAFAC   EQU   1                                                                
STASOON  EQU   2                                                                
STALOCK  EQU   3                                                                
STADISK  EQU   4                                                                
*                                                                               
STAFLG   DS    X                   FLAGS                                        
STATABLQ EQU   *-STATABD                                                        
*                                                                               
LOCK     CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALID TYPE/ACTION MATRIX                                            *         
***********************************************************************         
         SPACE 1                                                                
*        DC   A(TYP),AL1(VAL1,VAL2...VALN),X'00'                                
*                                                                               
         DS    0F                                                               
VALACTS  DC    AL1(TYPFAC),AL1(ACTDIS,0)                                        
         DC    AL1(TYPSOON),AL1(ACTDIS,0)                                       
         DC    AL1(TYPLOCK),AL1(ACTDIS,ACTFREE,ACTSTOP,ACTADD,ACTBLD,0)         
         DC    AL1(TYPDISK),AL1(ACTDIS,ACTFREE,ACTSTOP,0)                       
         DC    X'FF'                                                            
VALSUBS  DC    AL1(TYPFAC),AL1(3),X'00'                                         
         DC    AL1(TYPSOON),AL1(1,2),X'00'                                      
         DC    AL1(TYPLOCK),AL1(3,4),X'00'                                      
         DC    AL1(TYPDISK),AL1(3,4),X'00'                                      
         DC    X'FF'                                                            
VALOPTS  DC    AL1(TYPFAC),AL1(2,3,4),X'00'                                     
         DC    AL1(TYPSOON),AL1(1,2,3,4),X'00'                                  
         DC    AL1(TYPLOCK),AL1(3,6,7,8),X'00'                                  
         DC    AL1(TYPDISK),AL1(2,3,7,8),X'00'                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
*                                                                               
*        DC    X'ACT,TYP,FLG',AL1(PANEL)                                        
*                                                                               
HLPIND1  DC    X'FFFF00',AL1(255)  =LOCK FIELD                                  
HLPIND2  DC    X'FFFF00',AL1(1)    ACTION FIELD                                 
HLPIND3  DC    X'FFFF00',AL1(2)    TYPE FIELD                                   
HLPIND4  DC    X'010100',AL1(3)    OPTION FIELD                                 
         DC    X'010200',AL1(4)                                                 
         DC    X'010300',AL1(5)                                                 
         DC    X'010400',AL1(6)                                                 
         DC    X'02FF00',AL1(7)                                                 
         DC    X'03FF00',AL1(7)                                                 
         DC    X'04FF00',AL1(8)                                                 
         DC    X'05FF00',AL1(7)                                                 
         DC    X'FFFF00',AL1(255)                                               
HLPIND5  DC    X'FFFF00',AL1(255)  TAB FIELD                                    
HLPIND6  DC    X'010100',AL1(9)    ACT FIELDS                                   
         DC    X'010200',AL1(10)                                                
         DC    X'010300',AL1(11)                                                
         DC    X'010400',AL1(12)                                                
         DC    X'FFFF00',AL1(13)                                                
         EJECT                                                                  
DDDCLST  DS    0C                                                               
         DCDDL SR#HELD,9,L                                                      
         DCDDL SR#HOLD,9,L                                                      
         DCDDL SR#RLESD,9,L                                                     
         DCDDL SR#QUED,9,L                                                      
         DCDDL SR#RNING,9,L                                                     
         DCDDL SR#UPD,9,L                                                       
         DCDDL SR#READY,9,L                                                     
         DCDDL SR#ERROR,9,L                                                     
         DCDDL SR#ERROR,3,L,LABEL=SR3ERR                                        
         DCDDL SR#NONE,9,L                                                      
         DCDDL SR#STAT,9,L                                                      
         DCDDL SR#SYS,9,L                                                       
         DCDDL SR#DATE,9,L                                                      
         DCDDL SR#TODAY,9,L                                                     
*                                                                               
         DCDDL SR#DSP,9,L                                                       
         DCDDL SR#RLEAS,9,L                                                     
         DCDDL SR#UNKNW,9,L                                                     
         DCDDL SR#PURGE,9,L                                                     
         DCDDL SR#PRGED,9,L                                                     
         DCDDL SR#DEL,9,L                                                       
*                                                                               
DC@FAC   DC    CL8'FACPAK'                                                      
DC@DDS   DC    CL8'DDS'                                                         
DC@USR   DC    CL8'USER'                                                        
DC@QUEUE DC    CL8'QUEUE'                                                       
DC@FLAG  DC    CL8'FLAG'                                                        
DC@TEST  DC    CL8'TEST'                                                        
DC@FREE  DC    CL8'FREE'                                                        
DC@STOP  DC    CL8'STOP'                                                        
DC@ADD   DC    CL8'ADD'                                                         
DC@BUILD DC    CL8'BUILD'                                                       
*                                                                               
DC@LOCK  DC    CL8'LOCK'                                                        
DC@LOCKD DC    CL8'LOCKED'                                                      
DC@SOON  DC    CL8'SOON'                                                        
DC@DISK  DC    CL8'DISK'                                                        
DC@UNLK  DC    CL8'UNLOCK'                                                      
DC@KEY   DC    CL8'KEY'                                                         
*                                                                               
BUFFER   DC    CL8'BUFFER  '                                                    
GFILE    DC    CL8'GFILE   '                                                    
RANDOM   DC    CL8'RANDOM  '                                                    
PRTQUE   DC    CL8'PRTQUE  '                                                    
READ     DC    CL8'READ    '                                                    
ACTI     DC    CL8'ACTI    '                                                    
PURGE    DC    CL8'PURGE   '                                                    
INDEX    DC    CL8'INDEX   '                                                    
DMREAD   DC    CL8'DMREAD  '                                                    
DMRDHI   DC    CL8'DMRDHI  '                                                    
DMRSEQ   DC    CL8'DMRSEQ  '                                                    
DMWRT    DC    CL8'DMWRT   '                                                    
GLIST    DC    CL8'GLIST   '                                                    
CTFILE   DC    CL8'CTFILE  '                                                    
TEMPSTR  DC    CL8'TEMPSTR '                                                    
$LOC     DC    CL4'$LOC'                                                        
*                                                                               
SPACES   DC    CL80' '                                                          
EFFS     DC    F'-1'                                                            
HELPID   DC    XL10'015CFF00000000000000'                                       
ARZERO   DC    16F'0'                                                           
         EJECT                                                                  
***********************************************************************         
* FACIDTAB                                                            *         
***********************************************************************         
         SPACE 1                                                                
       ++INCLUDE FACIDTAB                                                       
         EJECT                                                                  
***********************************************************************         
* DATAMGR FILE TABLE (DMFILTAB)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE DMFILTAB                                                       
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE DSECT                                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
APARMS   DS    A                                                                
*                                                                               
IPARMS   DS    0XL32               INCOMING PARAMETER LIST                      
ASYSFACS DS    A                                                                
ASAVE    DS    A                                                                
AUTL     DS    A                                                                
ACOMFACS DS    A                                                                
ASELIST  DS    A                                                                
ATWA     DS    A                                                                
AMAP     DS    A                                                                
ATIOB    DS    A                                                                
*                                                                               
SAVERD   DS    F                                                                
SAVERE   DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
DDSFLAG  DS    X                                                                
DDSTRM   EQU   X'80'               DDS TERMINAL                                 
DDSACT   EQU   X'40'               VALID SUB ACTION ENTERED                     
DDSHEX   EQU   X'20'               DISP=HEX OPTION                              
DDSSWIT  EQU   X'10'               SWITCHED TO CONTROL                          
DDSLOCK  EQU   X'08'               FAUPDTAB LOCKED                              
*                                                                               
BASERB   DS    F                                                                
DMCB     DS    6F                                                               
*                                                                               
RELO     DS    A                   ADDRESSES                                    
ALET     DS    A                                                                
*                                  ADDRESSES FROM SYSFACS                       
ADMGR    DS    A                                                                
AENQDEQ  DS    A                                                                
ACALLOV  DS    A                                                                
ATICTOC  DS    A                                                                
ASSB     DS    A                                                                
AUPD     DS    A                                                                
*                                  ADDRESSES FROM COMFACS                       
AHEXOUT  DS    A                                                                
ADICTATE DS    A                                                                
AGETTXT  DS    A                                                                
ASCANNER DS    A                                                                
ADATCON  DS    A                                                                
ASWITCH  DS    A                                                                
ALOCKET  DS    A                                                                
APERVAL  DS    A                                                                
*                                  UPDATE TABLE INFORMATION                     
AUPDTABH DS    A                                                                
AUPDTAB  DS    A                                                                
AUPDTABX DS    A                                                                
AUPDLEN  DS    H                                                                
*                                  PHASES FROM CALLOV                           
ASQUASH  DS    A                                                                
ASORT    DS    A                                                                
*                                                                               
AFILETAB DS    A                                                                
ABLDMED  DS    A                                                                
AGETSE   DS    A                                                                
ASCANBLK DS    A                                                                
UPDENTRY DS    A                                                                
APHASE   DS    A                                                                
CURSOR   DS    A                                                                
*                                  NON ADDRESSIBLE AREAS WITHIN WORKD           
ACIREC   DS    A                                                                
ACXREC   DS    A                                                                
AIOAREA  DS    A                                                                
*                                  NON ADDRESSIBLE AREAS WITHIN WORKD           
QHDR     DS    A                                                                
BUFFDATA DS    0XL12               PRTQUE DATA GIVEN BY BUFFER ACTION           
BUPQTAB  DS    V                                                                
BUPQFILE DS    V                                                                
BUSAVE   DS    XL4                                                              
*                                                                               
PRTQLST  DS    0X                                                               
PRTQMAX  DS    X                   NUMBER OF PRTQ FILES                         
         DS    X                                                                
PRTQFLG  DS    X                                                                
         DS    XL5                                                              
PRTQNTRY DS    16XL8               MAXIMUM OF 16 PRTQ FILES                     
PRTQLSTX DS    XL8                                                              
*                                                                               
NDX      DS    CL40                                                             
SAVE     DS    360C                                                             
*                                                                               
INFONUM  DS    H                   PRESET INFO MESSAGE                          
ERRNUM   DS    H                   PRESET ERROR MESSAGE                         
PHASE    DS    X                   CURRENT PHASE LOADED                         
CALL     DS    X                   PHASE ACTION (D OR L)                        
RECLEN   DS    H                   TEMPSTR RECORD LENGTH                        
         DS    H                                                                
PQRLEN   DS    X                   LEN OF PQ RECORD                             
*                                                                               
ACTION   DS    X                   ACTION VALUE                                 
LOCTYPE  DS    X                   TYPE VALUE                                   
STAFILT  DS    X                   STATUS FILTER VALUE                          
SYSFILT  DS    X                   SYSTEM FILTER VALUE                          
DATFILT  DS    XL3                 DATE FILTER CYYDDD                           
PFKEY    DS    X                   PFKEY USED                                   
LKTIME   DS    XL3                 TIME NOW HH:MM:SS                            
KEY      DS    XL25                                                             
*                                                                               
HELPKEY  DS    0CL10               GETHELP KEY                                  
         DS    CL3                                                              
HELPNUM  DS    XL1                                                              
HELPPAG  DS    XL1                                                              
HELPLANG DS    X                                                                
         DS    XL4                                                              
HELP     DS    0CL4                HELP DATA                                    
HLPFLD   DS    X                                                                
HLPPOS   DS    X                                                                
HLPPAG   DS    X                                                                
HLPFLG   DS    X                                                                
*                                                                               
PRTQID   DS    CL8                 NAME OF PRTQ FOR DMGR                        
SAVUSER  DS    CL2                 SAVED USERID FOR LIST                        
USERID   DS    CL8                                                              
SAVAGY   DS    CL2                 SAVED 2CHR AGY FOR GETSE                     
SAVAGB   DS    C                   SAVED AGB FOR GETSE                          
SAVSE    DS    C                   SAVED SE FOR GETSE                           
PQUSER   DS    XL2                                                              
TRMUSER  DS    XL2                                                              
TRM      DS    XL2                                                              
TODAY    DS    XL3                 TODAY CYYDDD                                 
*                                                                               
WORK     DS    CL32                                                             
*                                                                               
MSG      DS    CL80                AREA FOR (FILES LOCKED) TEXT                 
EXT      DS    CL132               EXTRA TEXT FOR GETTXT CALLS                  
REQUEST  DS    CL80                AREA FOR REQUEST CARD                        
*                                  LOCK=CARD DETAILS                            
LKDTAIL  DS    0CL21                                                            
LKFILE1  DS    A                   A(FILTAB ENTRY)                              
LKFILE2  DS    A                   A(FILTAB ENTRY)                              
LKFLAG   DS    X                   LOCK FLAG                                    
FLGLOCK  EQU   X'80'                                                            
FLGCARD  EQU   X'40'                                                            
FLGSOFT  EQU   X'20'                                                            
LKAGY    DS    CL2                 2CHR AGENCY                                  
LKSYS    DS    X                   OV SYSTEM                                    
MED      EQU   X'04'                                                            
ACC      EQU   X'06'                                                            
LKSYSSE  DS    X                   SE SYSTEM                                    
LKAGB    DS    X                                                                
LKSYSN   DS    CL7                 SE SYSTEM NAME                               
*                                                                               
SCANBLK  DS    256C                                                             
*                                                                               
         DS    0H                                                               
       ++INCLUDE DMPRTQW                                                        
*                                                                               
DDDSLST  DS    0C                                                               
         DSDDL                                                                  
*                                                                               
CXREC    DS     14336C                                                          
CIREC    DS     14336C                                                          
IOAREA   DS     1024C                                                           
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* SAVED STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
SAVEDSTR DSECT                     SAVED STORAGE VARIABLES (COPY)               
IDENT    DS    CL4                 $LOCK INDENTIFIER                            
SVTYPE   DS    C                   FAC SOON LOCK DISK                           
COUNT    DS    H                   NUMBER OF ENTRYS IN UPDTABS                  
UPDTABS  DS    18XL32              SAVED UPDTAB DATA                            
SVCTKEY  DS    CL25                LAST DISK KEY DISPLAYED                      
SVSEQ    DS    X                   SEQ NUM FOR SCROLLING                        
PREVCTK  DS    CL25                PREVIOUS LAST KEY DISPLAYED                  
PREVSEQ  DS    X                   PREVIOUS SEQ NUM FOR SCROLLING               
SAVEDL   EQU   *-SAVEDSTR                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER REQUIRED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
USAVED   DSECT                     SAVE STORAGE ENTRY                           
USDTYP   DS    XL1                                                              
USDUSR   DS    XL2                 USER ID                                      
USDRID   DS    CL3                 REPORT ID                                    
USDSEQ   DS    XL2                 REPORT SEQ NO                                
*                                                                               
USDFLAGS DS    XL1                 FLAGS                                        
USDCHAIN DS    XL1                 NEXT UPDATE IN CHAIN                         
*                                                                               
USDSENUM DS    XL1                 SE NUMBER                                    
USDEXT1  DS    XL1                 EXTERNAL FILE NUMBER 1                       
USDEXT2  DS    XL1                 EXTERNAL FILE NUMBER 2                       
*                                                                               
USDRULE  DS    XL2                 LOCKOUT RULE                                 
USDKEY   DS    CL10                KEY FOR LOCKOUT RULE                         
*                                                                               
USSTAT   DS    X                                                                
USATTB   DS    X                                                                
USSORT   DS    XL5                                                              
USAVEDL  EQU   *-USAVED                                                         
         EJECT                                                                  
***********************************************************************         
* REPORT LINE DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
REPLINE  DSECT                     REPORT LINE DSECT                            
RLINE    DS    0CL93                                                            
RLSAHDR  DS    CL8                                                              
RLSACT   DS    CL3                                                              
RLLNHDR  DS    CL8                                                              
RLLINE   DS    0CL74                                                            
RLUSERID DS    CL8                                                              
         DS    CL1                                                              
RLREPID  DS    CL9                                                              
         DS    CL1                                                              
RLREPSTA DS    CL9                                                              
         DS    CL1                                                              
RLREPSYS DS    CL4                                                              
         DS    CL1                                                              
RLREPFIL DS    CL40                                                             
         EJECT                                                                  
***********************************************************************         
* FAUPDTAB LINE DSECT                                                 *         
***********************************************************************         
         SPACE 1                                                                
FACLINE  DSECT                                                                  
FLINE    DS    0CL93                                                            
FLSAHDR  DS    CL8                                                              
FLSACT   DS    CL3                                                              
FLLNHDR  DS    CL8                                                              
FLLINE   DS    0CL74                                                            
FLNUM    DS    CL3                                                              
         DS    CL1                                                              
FLUSERID DS    CL8                                                              
         DS    CL1                                                              
FLREPID  DS    CL9                                                              
         DS    CL1                                                              
FLFLAG   DS    CL4                                                              
         DS    CL2                                                              
FLQUEUE  DS    CL2                                                              
         DS    CL3                                                              
FLSENUM  DS    CL2                                                              
         DS    CL2                                                              
FLFILES  DS    CL5                                                              
         DS    CL1                                                              
FLRULE   DS    CL5                                                              
         DS    CL1                                                              
FLDATA   DS    CL24                                                             
         EJECT                                                                  
***********************************************************************         
* LOCKET LINE DSECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
USRLINE  DSECT                     LOCKET LINE DSECT                            
ULINE    DS    CL93                                                             
         ORG   ULINE                                                            
ULSAHDR  DS    CL8                                                              
ULSACT   DS    CL3                                                              
ULLNHDR  DS    CL8                                                              
ULLINE   DS    0CL74                                                            
ULFAC    DS    CL4                                                              
         DS    CL1                                                              
ULDATE   DS    CL6                                                              
         DS    CL1                                                              
ULSYS    DS    CL7                                                              
         DS    CL1                                                              
ULAGY    DS    CL2                                                              
         DS    CL1                                                              
ULTYP    DS    CL2                                                              
         DS    CL1                                                              
ULKEY    DS    CL10                                                             
         DS    CL1                                                              
ULSTAT   DS    CL6                                                              
         DS    CL1                                                              
ULLUID   DS    CL8                                                              
         DS    CL1                                                              
ULTIME1  DS    CL8                                                              
         DS    CL1                                                              
ULTIME2  DS    CL8                                                              
         ORG                                                                    
         EJECT                                                                  
***********************************************************************         
* SCREEN MAP                                                          *         
***********************************************************************         
         SPACE 1                                                                
SRLOCFFD DSECT                                                                  
         DS    CL64                                                             
       ++INCLUDE SRLOCFFD                                                       
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* FADSECTS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FADSECTS                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FALOCKETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
* SRDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE SRDDEQUS                                                       
         PRINT ON                                                               
* DMPRTQD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQD                                                        
         PRINT ON                                                               
* DMPRTQK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQK                                                        
         PRINT ON                                                               
* DMPRTQS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMPRTQS                                                        
         PRINT ON                                                               
* SRERREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE SRERREQUS                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DMFILTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DMFILTABD                                                      
         PRINT ON                                                               
* DMDSHDR                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDSHDR                                                        
         PRINT ON                                                               
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
* FACIDTABD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FACIDTABD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SRLOC00S  02/15/01'                                      
         END                                                                    
