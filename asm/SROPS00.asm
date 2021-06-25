*          DATA SET SROPS00    AT LEVEL 008 AS OF 03/15/19                      
*PHASE T10F00A                                                                  
*INCLUDE GETBOOK                                                                
*INCLUDE TIMEOUT                                                                
*&&      SET   NOP=N                                                            
         IEABRCX DEFINE                                                         
         IEABRCX DISABLE                                                        
         TITLE '$OPER - HANDLE OPERATOR COMMANDS'                               
OPER     CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL WORKX-WORKD,**$OPS**,CLEAR=YES,RR=RE                             
         USING WORKD,RC            RC=A(W/S)                                    
         J     START                                                            
*                                                                               
$$DATA   LOCTR ,                   DATA LOCATION CLOSE TO RB                    
*                                                                               
***********************************************************************         
* RB = BASE1 LITERALS AND CONSTANTS                                   *         
* RA = RESERVED                                                       *         
* R9 = LOCAL BASE - USE WHEN NEEDED                                   *         
* CONSTANTS & LITERALS DEFINE LATER USING $$DATA LOCTR                *         
***********************************************************************         
*                                                                               
$$CODE   LOCTR ,                   CODE STARTS AFTER DATA                       
*                                                                               
START    ST    RD,SAVERD                                                        
         ST    RE,RELO                                                          
         MVC   SRPARS,0(R1)        SAVE S/R PARAMETER LIST                      
         L     R7,SRPAR1                                                        
         USING SYSFACD,R7          R7=A(SYSFACS)                                
         L     R8,VSSB                                                          
         USING SSBD,R8             R8=A(SSB)                                    
         L     R5,SRPAR6                                                        
         USING SROPSFFD,R5                                                      
*                                                                               
         OI    SSBOPECB,X'80'      SET SROPS IN PROGRESS                        
*                                                                               
         MVC   SYSNAME(3),SSBSYSNA                                              
         MVI   SYSNAME+3,C'+'                                                   
         TM    SSBSTAT4,SSBSAOR    AM I AN AOR                                  
         JZ    OPS010                                                           
*                                                                               
         LLC   R1,SSBSYSIX         SET AOR CHR IN MSG                           
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,SYSNAME+3                                                     
*                                                                               
OPS010   MVC   ATCB,SSBTKADR                                                    
         MVC   ATIA,SRPAR2                                                      
         MVC   AUTL,SRPAR3                                                      
         L     R1,AUTL                                                          
         USING UTLD,R1                                                          
         MVC   MYLUID,TSYM                                                      
         OI    TPRGIND,X'14'       SET CONVERTED MAX IOS                        
         LA    RF,MYPGMLST                                                      
         STCM  RF,7,TASVC          SET DUMMY PGMLST ENTRY ALSO                  
         DROP  R1                                                               
*                                                                               
         MVC   OPFACID,=C'+FACPAK+'                                             
         MVC   OPFACID+4(4),SYSNAME                                             
*                                                                               
         L     R1,SRPAR4                                                        
         USING COMFACSD,R1                                                      
         MVC   ASCANNER,CSCANNER                                                
         MVC   AHEXOUT,CHEXOUT                                                  
         MVC   AMQIO,CMQIO                                                      
         MVC   AGETFACT,CGETFACT                                                
         MVC   ADATCON,CDATCON                                                  
         MVC   AGETDAY,CGETDAY                                                  
         MVC   AADDAY,CADDAY                                                    
         DROP  R1                                                               
*                                                                               
         L     R1,=A(IOAREA-WORKD)                                              
         AR    R1,RC                                                            
         ST    R1,AIOAREA                                                       
         L     R1,=A(BUFF1-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF1                                                        
         L     R1,=A(BUFF2-WORKD)                                               
         AR    R1,RC                                                            
         ST    R1,ABUFF2                                                        
         L     R1,=A(COMTAB)                                                    
         A     R1,RELO                                                          
         ST    R1,ACOMTAB                                                       
*                                                                               
         GOTO1 AGETFACT,DMCB,0                                                  
         L     R1,0(R1)                                                         
         L     R1,FASYSLST-FACTSD(R1)                                           
         ST    R1,ASYSLST                                                       
*                                                                               
         GOTO1 VDATAMGR,DMCB,=C'UPDID'                                          
         L     R1,12(R1)                                                        
         MVC   MYUPDID,0(R1)       GET UPDID FOR SYSTEM                         
*                                                                               
         L     R1,AUTL                                                          
         CLC   TSYM-UTLD(8,R1),=C'NONENONE'                                     
         JE    OPS030                                                           
         CLC   TSYM-UTLD(8,R1),=C'FREENTRY'                                     
         JE    OPS030                                                           
         CLC   TSYM-UTLD(8,R1),=C'FREEFREE'                                     
         JE    OPS030                                                           
         BRAS  RE,SRMAIN           IF NOT AUTO THEN =OPER                       
         J     XMOD2                                                            
*                                                                               
OPS030   L     R1,SSBOPECB         TEST FO OPER ECB POSTED                      
         TM    0(R1),X'40'                                                      
         JNZ   OPS050                                                           
*                                                                               
OPS040   BRAS  RE,DMAIN            MAIN DSPACE COMMS                            
         J     XMOD2                                                            
*                                                                               
OPS050   BRAS  RE,MAIN             MAIN OPER COMMS                              
*                                                                               
XMOD1    L     RD,SAVERD                                                        
         BRAS  RE,CLEANUP                                                       
XMOD2    L     RD,SAVERD                                                        
         NI    SSBOPECB,255-X'C0'  RESET SROPS IN PROGRESS                      
         SAC   0                                                                
*                                                                               
         XMOD1                                                                  
*                                                                               
XITEQU   CR    RB,RB                                                            
         J     XIT1                                                             
XITNEQ   LTR   RB,RB                                                            
*                                                                               
XIT1     XIT1                                                                   
*                                                                               
CLEANUP  NTR1                      CLEAR OPS COMMAND                            
         SAC   0                                                                
         L     RF,SSBACOMM                                                      
         USING COMLIST,RF                                                       
         ICM   R2,15,COMCIBPT      A(CIB)                                       
         JZ    XIT1                                                             
         LA    R3,COMCIBPT         A(A(CIB))                                    
         QEDIT ORIGIN=(R3),BLOCK=(R2)  FREE THE CIB                             
         DROP  RF                                                               
         ICM   R2,15,ACOMM         IS THERE AN ACTIVE COMMAND                   
         JZ    XIT1                                                             
         SAC   512                                                              
         MVC   WORK,0(R2)          SAVE IT IN WORK                              
         XC    0(32,R2),0(R2)      THEN CLEAR IT                                
         SAC   0                                                                
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM FOR =OPER                                              *         
***********************************************************************         
SRMAIN   NTR1                                                                   
         MVI   ONLINE,C'Y'         FLAG WE ARE FROM =OPER                       
*                                                                               
*&&UK*&& CLC   MYLUID(4),=C'D1LH'                                               
*&&UK*&& BE    SRMA010                                                          
*&&US*&& CLC   MYLUID(3),=C'SY1'                                                
*&&US*&& BE    SRMA010                                                          
*&&US*&& CLC   MYLUID(6),=C'OPSSY1'                                             
*&&US*&& BE    SRMA010                                                          
*&&US*&& CLC   SYSNAME(2),=C'RE'   ONLY OPS CAN USE THIS ON REP                 
*&&US*&& BE    COMNO                                                            
         CLC   SYSNAME(2),=C'AD'   ONLY OPS CAN USE THIS ON ADV                 
         JE    COMNO                                                            
*                                                                               
SRMA010  LA    RF,SRVP1H           GET COMMAND DATA FROM FIELD                  
         OI    6(RF),X'40'         SET CURSOR                                   
         L     R7,SRPAR1                                                        
         USING SYSFACD,R7          R7=A(SYSFACS)                                
*                                                                               
         MVC   CARD,SPACES                                                      
         SR    R1,R1                                                            
         ICM   R1,1,5(RF)                                                       
         JZ    MAINX                                                            
         BCTR  R1,0                                                             
         MVC   CARD(0),8(RF)                                                    
         EXRL  R1,*-6                                                           
*                                                                               
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         JNE   MAIN070             NEQ SO TRY GROUP COMMAND                     
*                                                                               
         MVC   SRVP1,SPACES                                                     
*                                                                               
         TM    FLAG,X'80'          TEST FOR THIS FACPAK ONLY                    
         JZ    SMAIN050                                                         
         SR    R1,R1                                                            
         ICM   R1,3,ACTNUM         EXECUTE ACTION NOW                           
         SLL   R1,2                                                             
         LARL  RF,MAIN100                                                       
         EX    0,0(RF,R1)                                                       
         J     MAINX                                                            
*                                                                               
SMAIN050 BRAS  RE,SETCOM           SEND COMMAND TO DSPACE                       
*                                                                               
         XC    DISPLOOP,DISPLOOP   CLEAR DISPLOOP                               
*                                                                               
COM010   MVI   DISPFLAG,C'N'                                                    
         SAM31                                                                  
         SAC   512                                                              
         XC    WORK,WORK           CLEAR ACCESS REGS                            
         LAM   ARE,ARD,WORK                                                     
         LAM   AR2,AR2,SSBALET                                                  
         SR    R2,R2                                                            
         USING DMDSHDR,R2                                                       
         L     R2,DHACOMM                                                       
         AHI   R2,4096             SKIP HEADERS                                 
         USING DSCOMM,R2                                                        
*                                                                               
COM050   LARL  R1,HDR1                                                          
         MVC   SRVHDR1,0(R1)                                                    
         LARL  R1,HDR2                                                          
         MVC   SRVHDR2,0(R1)                                                    
*                                                                               
         OC    DISPLOOP,DISPLOOP   XMIT FIRST TIME                              
         JNZ   *+12                                                             
         OI    SRVHDR1H+6,X'80'                                                 
         OI    SRVHDR2H+6,X'80'                                                 
*                                                                               
         LA    R7,SRVSELH          POINT TO 1ST LINE                            
         USING COLINED,R7                                                       
*                                                                               
COM055   LA    R4,16               SET COUNT TO 16 LINES                        
*                                                                               
COM060   OC    DISPLOOP,DISPLOOP   FIRST TIME                                   
         JZ    COM065                                                           
*                                                                               
         CLI   COCOMM,C' '         NO COMMAND SO IGNORE                         
         JNH   COM100                                                           
         OC    0(32,R2),0(R2)      PROCESSED?                                   
         JZ    *+12                                                             
         MVI   DISPFLAG,C'Y'       FLAG MORE TO COME                            
         J     COM100                                                           
*                                                                               
         MVC   COCOMM+28(9),=C'COMPLETED'                                       
         OI    COLINEH+6,X'80'     XMIT                                         
         J     COM100                                                           
*                                                                               
COM065   NI    COLINEH+1,255-X'08' NORMAL                                       
         OI    COLINEH+6,X'80'     XMIT                                         
         MVI   DISPFLAG,C'Y'       FLAG MORE TO COME                            
         MVC   COLINE,SPACES                                                    
         OC    0(32,R2),0(R2)      IGNORE ZERO ENTRIES                          
         JZ    COM100                                                           
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSCSORC     DISPLAY ADV SOURCE                           
         BRAS  RE,DISJOB                                                        
         MVC   COORG,DUB1                                                       
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSCDEST     DISPLAY ADV OR JOB DEST                      
         MVC   FULL+2(1),DSCFLAG                                                
         BRAS  RE,DISJOB                                                        
         MVC   CODEST,DUB1                                                      
*                                                                               
         MVC   FULL,DSCTIME                                                     
         BRAS  RE,TIMEOUT                                                       
         MVC   COTIME(8),WORK1     HH:MM:SS                                     
         MVC   COCOMM,SPACES                                                    
*                                                                               
         L     R1,ACOMTAB          SCAN COMTAB FOR ACTION                       
COM070   CLC   DSCCOMM,8(R1)                                                    
         JE    COM075                                                           
         LA    R1,16(R1)                                                        
         CLI   8(R1),X'FF'                                                      
         JNE   COM070                                                           
COM075   MVC   COCOMM(8),0(R1)                                                  
*                                                                               
         TM    11(R1),X'10'        IF SET NO SE INFORMATION                     
         JO    COMNXT                                                           
*                                                                               
         MVC   BYTE,DSCDATA+1                                                   
         BRAS  RE,FINDNUM          FIND SE ENTRY                                
*                                                                               
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         MVC   COCOMM+9(7),SENAME  SHOW SYSTEM NAME                             
         MVC   APGMS,SEPGMS                                                     
         MVC   FILNUM,DSCDATA+2                                                 
         CLI   FILNUM,0                                                         
         JE    COM100                                                           
*                                                                               
         TM    11(R1),FACOSYPG     SYS PRG                                      
         JO    COM090                                                           
*                                                                               
         BRAS  RE,FINDFIL                                                       
         L     R6,ADTF                                                          
         MVC   COCOMM+16(8),DTFDD-DTF(R6)                                       
         J     COM100                                                           
*                                                                               
COM090   BRAS  RE,GETPGMS                                                       
         MVC   COCOMM+16(8),WORK                                                
*                                                                               
COM100   LA    R7,COLINEL(R7)      NEXT SCREEN LINE                             
*                                                                               
COMNXT   LA    R2,32(,R2)          NEXT COM                                     
         JCT   R4,COM060                                                        
*                                                                               
         SAC   0                                                                
         SAM24                                                                  
         GOTO1 AGETFACT,DMCB,(X'80',WORK),F#WRITE                               
         GOTO1 AGETFACT,DMCB,(X'80',=F'7680'),F#WAIT 1/5 SECOND                 
*                                                                               
         OC    DISPLOOP,DISPLOOP   FIRST TIME                                   
         JNZ   *+12                                                             
         MVI   ONLINE,C'N'         NOW SEND MESSAGES TO THE CONSOLE             
         BRAS  RE,DMAIN            DO I HAVE ANYTHING TO DO                     
*                                                                               
         CLI   DISPFLAG,C'N'       EXIT WHEN ALL DONE                           
         JE    COMX                                                             
*                                                                               
         L     R1,DISPLOOP                                                      
         LA    R1,1(R1)                                                         
         ST    R1,DISPLOOP         LOOP FOR 2 SECONDS AT 1/5                    
         CLC   DISPLOOP,=F'10'                                                  
         JL    COM010                                                           
         J     COMX                                                             
*                                                                               
COMNO    LA    R1,17               NOT AUTHORIZED                               
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
*                                                                               
         DROP  R2                                                               
COMX     SAM24                                                                  
         SAC   0                                                                
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY JOB DETAILS                                                 *         
***********************************************************************         
DISJOB   TM    FULL+2,X'01'        TEST OFFLINE JOB                             
         JO    DJOB085                                                          
         CLI   FULL+3,0                                                         
         JE    *+14                                                             
         MVC   FULL+0(1),FULL+3                                                 
         MVI   FULL+1,0                                                         
         MVC   BYTE,FULL+0                                                      
         L     R1,SSBAFID                                                       
         NI    BYTE,X'0F'                                                       
DJOB081  CLC   BYTE,4(R1)          FIND ADV SYSTEM                              
         JE    DJOB082                                                          
         LA    R1,8(R1)                                                         
         CLI   5(R1),X'FF'         CHECK EOT                                    
         JNE   DJOB081                                                          
         DC    H'0'                                                             
DJOB082  MVC   DUB1(4),0(R1)                                                    
         CLI   DUB1+3,C' '                                                      
         JE    *+14                                                             
         MVC   DUB1+2(1),DUB1+3                                                 
         MVI   DUB1+3,C' '                                                      
*                                                                               
         MVC   DUB1+4(2),=C'/#'                                                 
         MVC   DUB1+6(1),FULL+1                                                 
         TM    FULL+3,X'F0'                                                     
         BZR   RE                                                               
         LLC   R1,FULL+3                                                        
         SRL   R1,4                                                             
         LA    R1,X'C0'(R1)                                                     
         STC   R1,DUB1+3                                                        
         BR    RE                                                               
*                                                                               
DJOB085  SR    R3,R3               SET TO DATASPACE                             
         USING DMDSHDR,R3                                                       
         LAM   AR3,AR3,SSBALET                                                  
         L     R3,DHAJOBS          SET R3 TO ADV SYS BLOCK                      
         LA    R0,DSJOBMXQ                                                      
DJOB090  CLI   0(R3),0             EMPTY ADV ENTRY                              
         JE    DJOB100                                                          
*                                                                               
         CLC   FULL(2),12(R3)      MATCH ON ASID                                
         JNE   DJOB100                                                          
         MVC   DUB1,0(R3)          MOVE IN JOBNAME                              
*                                                                               
DJOB100  LA    R3,DSJOBLNQ(,R3)                                                 
         JCT   R0,DJOB090          ALL JOBS DONE                                
*                                                                               
         BR    RE                                                               
         DROP  R3                                                               
                                                                                
***********************************************************************         
* EDIT TUS IN FULL TO WORK HH:MM:SS.SS                                *         
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
* FIND ASENTRY FROM BYTE=NUMBER                                       *         
***********************************************************************         
FINDNUM  NTR1                                                                   
         L     R7,SRPAR1                                                        
         USING SYSFACD,R7          R7=A(SYSFACS)                                
         L     R6,VSELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
FINDN10  CLC   SESYS,BYTE          FIND SELECTED SYS                            
         JE    FINDNX                                                           
*                                                                               
         JXLE  R6,R4,FINDN10                                                    
         LTR   RB,RB               SET CC NEQ (NOT FOUND)                       
         J     XIT1                                                             
FINDNX   ST    R6,ASENTRY          SAVE ASENTRY                                 
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* MAIN PROGRAM                                                        *         
***********************************************************************         
MAIN     NTR1                                                                   
         USING SYSFACD,R7                                                       
         L     RF,SSBACOMM         SET RF TO COMM BLOCK                         
         USING COMLIST,RF                                                       
         L     R2,COMCIBPT         A(CIB)                                       
         USING CIBNEXT,R2                                                       
         DROP  RF                                                               
*                                                                               
         CLI   CIBVERB,CIBSTOP     DID OPERATOR ENTER 'STOP'?                   
         JNE   MAIN010                                                          
         OI    SSBSTAT1,SSBSEOJ    SET EOJ                                      
*                                                                               
         LA    R1,3                FORCED END OF JOB                            
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     MAINX                                                            
*                                                                               
MAIN010  CLI   CIBVERB,CIBMODFY    DID OPERATOR ENTER 'MODIFY'?                 
         JE    *+10                YES                                          
         BRAS  RE,CLEANUP          WHAT DID THE OPERATOR DO?                    
         DC    H'0'                                                             
*                                                                               
         SR    R1,R1               GET DATA LEN IN R1                           
         ICM   R1,3,CIBDATLN                                                    
         BCTR  R1,0                                                             
         MVC   CARD,SPACES         MOVE DATA TO WORK AND SCAN IT                
         MVC   CARD(0),CIBDATA                                                  
         EXRL  R1,*-6                                                           
*                                                                               
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         JNE   MAIN070             NEQ SO TRY GROUP COMMAND                     
*                                                                               
         TM    FLAG,X'80'          TEST FOR THIS FACPAK ONLY                    
         JZ    MAIN050                                                          
         SR    R1,R1                                                            
         ICM   R1,3,ACTNUM         EXECUTE ACTION NOW                           
         SLL   R1,2                                                             
         LARL  RF,MAIN100                                                       
         EX    0,0(RF,R1)                                                       
         J     MAINX                                                            
*                                                                               
MAIN050  BRAS  RE,SETCOM           SEND COMMAND TO DSPACE                       
         J     MAINX                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK FOR GROUP COMMAND IN COMMAND RECORD                           *         
***********************************************************************         
MAIN070  LA    R4,IOAREA           BUILD BOOK KEY IN WORK                       
         USING CTJREC,R4                                                        
         XC    CTJKEY,CTJKEY                                                    
         MVI   CTJKTYP,CTCOTYPQ    SET COMMAND TYPE                             
         MVC   CTJKID,COMMAND                                                   
MAIN080  L     RF,=V(GETBOOK)                                                   
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,IOAREA,CARD,VDATAMGR                                   
         TM    8(R1),X'80'                                                      
         JO    MAINX               EOF                                          
         CLI   8(R1),0                                                          
         JNE   ERRX                UNKNOWN COMMAND                              
         MVC   CARD+70(10),SPACES                                               
*                                                                               
         CLI   HEADER,0            SET UP HEADER IF NOT ALREADY DONE            
         JNE   *+8                                                              
         BRAS  RE,SETHEAD                                                       
*                                                                               
         CLI   CARD,C' '           IGNORE BLANKS AND STARS                      
         JE    MAIN080                                                          
         CLI   CARD,C'*'                                                        
         JE    MAIN080                                                          
*                                                                               
         BRAS  RE,VALCARD          VALIDATE CARD INPUT                          
         JE    *+6                                                              
         DC    H'0'                                                             
         JNE   ERRX                                                             
*                                                                               
         TM    FLAG,X'80'          TEST FOR THIS FACPAK ONLY                    
         JO    ERRX                                                             
*                                                                               
         BRAS  RE,SETCOM           SEND COMMAND TO DSPACE                       
         J     MAIN080                                                          
*                                                                               
MAINX    J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* EXECUTE OPERATOR COMMANDS                                           *         
***********************************************************************         
MAIN100  DC    F'0'                ACTION TABLE                                 
         DC    XL4'00'                                                          
         BRAS  RE,SETCAN                                                        
         BRAS  RE,SETDUMP                                                       
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         BRAS  RE,WAKEUP                                                        
         BRAS  RE,SHOWRPL                                                       
         BRAS  RE,FIXECB                                                        
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         BRAS  RE,CHECK                                                         
         BRAS  RE,STARTTTS                                                      
         BRAS  RE,RECYCLE                                                       
         DC    XL4'00'                                                          
         DC    XL4'00'                                                          
         BRAS  RE,NEWDAY           24  NEWDAY                                   
         BRAS  RE,TASK             25  TASK                                     
         BRAS  RE,RESETSIN         26  RESET SIN                                
         BRAS  RE,SSBSTOP          27  SSB STOP                                 
         BRAS  RE,SSBGO            28  SSB GO                                   
         DC    F'0'                29  QUIESCE USS RCVR                         
         DC    F'0'                30  UNQUIESCE USS RCVR                       
         DC    F'0'                31  STATUS USS RCVR                          
         DC    F'0'                32  ENDRUN                                   
         DC    F'0'                33  DEQ                                      
         BRAS  RE,REBUILD          34  REBUILD                                  
         DC    F'0'                35  STATE                                    
         DC    F'0'                36  PGMSTOP                                  
         DC    F'0'                37  PGMSTART                                 
         DC    F'0'                38  PGMREST                                  
         DC    F'0'                39  PGMUNRE                                  
         EJECT                                                                  
***********************************************************************         
* MAIN DSPACE COMMAND PROGRAM                                         *         
***********************************************************************         
DMAIN    NTR1                                                                   
         L     R7,SRPAR1                                                        
         USING SYSFACD,R7                                                       
         LAM   AR2,AR2,SSBALET     PICK UP ALET                                 
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHACOMM          SET R2 TO COMMS BLOCK                        
         AHI   R2,4096             SKIP HEADERS                                 
         LA    R0,512                                                           
*                                                                               
         USING DSCOMM,R2                                                        
DMAIN010 ST    R0,SAVER0                                                        
         TM    DSCFLAG,DSCDONEQ    IGNORE PROCESSED                             
         JO    DMAIN090                                                         
         TM    DSCFLAG,DSCACTVQ    IGNORE ACTIVE                                
         JO    DMAIN090                                                         
         CLC   SSBSYSIX,DSCDEST    IS IT FOR ME                                 
         JE    DMAIN020                                                         
         J     DMAIN090                                                         
*                                                                               
DMAIN020 ST    R2,ACOMM            SAVE OUR ADDRESS                             
         MVC   OPEXTRA(9),=C'(J000000)'                                         
         CLC   DSCSORC,=X'FFFE'                                                 
         JE    DMAIN030                                                         
         XC    FULL,FULL                                                        
         MVC   FULL(2),DSCSORC     DISPLAY ADV SOURCE                           
         BRAS  RE,DISJOB                                                        
         MVC   OPEXTRA+1(7),DUB1                                                
         J     DMAIN040                                                         
*                                                                               
DMAIN030 EDIT  (B2,DSCJNUM),(6,OPEXTRA+2),FILL=0                                
*                                                                               
DMAIN040 MVC   DATA(16),DSCDATA    COPY DATA TO DATA                            
         SR    R1,R1                                                            
         ICM   R1,3,DSCCOMM        R1=ACTION NUMBER                             
         JZ    ERRX                                                             
         CHI   R1,MAXACT                                                        
         JNH   DMAIN050                                                         
*                                                                               
         XC    DSCOMMS,DSCOMMS     REMOVE COMMAND TO STOP LOOPING               
         BRAS  RE,CLEANUP          ACTION NUMBER EXCEEDED                       
         DC    H'0'                                                             
*                                                                               
DMAIN050 OI    DSCFLAG,DSCACTVQ    FLAG AS ACTIVE                               
         STH   R1,ACTNUM           SAVE ACTION NUMBER                           
         SLL   R1,2                * 4 FOR EXECUTE                              
         SAC   0                                                                
         LARL  RF,ACTION                                                        
         EX    0,0(RF,R1)          GO DO IT                                     
         SAC   512                                                              
         LAM   AR2,AR2,SSBALET                                                  
         OI    DSCFLAG,DSCDONEQ    FLAG IT AS DONE                              
*                                                                               
         OC    DSCHEADR,DSCHEADR   ANY HEADERS TO DEAL WITH                     
         JZ    DMAIN080                                                         
         MVC   HEADER,DSCHEADR                                                  
         XC    0(32,R2),0(R2)      CLEAR THIS ONE                               
         BRAS  RE,CHKHEAD          CHECK FOR OTHERS                             
*                                                                               
DMAIN080 CLC   DSCSORC,=X'FFFE'    WAS IS DDOPER                                
         JE    *+10                                                             
         XC    0(32,R2),0(R2)      CLEAR THE COMMAND UNLESS DDOPER              
         XC    ACOMM,ACOMM                                                      
*                                                                               
         CLC   ACTNUM,=AL2(04)     IF OPEN AND SYS IS READ ONLY                 
         JNE   *+18                                                             
         TM    SVSEIND,SEIRONLY                                                 
         JNO   *+10                                                             
         MVC   ACTNUM,=AL2(10)     CHANGE TO READ FOR DDSTATE CALL              
*                                                                               
         LARL  R1,STATES           CALL DDSTATE FOR LOCAL STATE CHANGE          
DMAIN085 CLC   ACTNUM,0(R1)                                                     
         JE    DMAIN086                                                         
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'         EOT SO NO CALL REQUIRED                      
         JNE   DMAIN085                                                         
         J     DMAIN090                                                         
*                                                                               
DMAIN086 XC    DMCB(12),DMCB       CALL DDSTATE                                 
         LA    R1,2(R1)                                                         
         ST    R1,DMCB                                                          
         MVC   DMCB+6(2),SYNUM                                                  
         SAC   0                                                                
         GOTO1 VDDSTATE,DMCB                                                    
         LAM   AR2,AR2,SSBALET                                                  
         SAC   512                                                              
*                                                                               
DMAIN090 LA    R2,32(,R2)          POINT TO NEXT COMM ENTRY                     
         L     R0,SAVER0                                                        
         JCT   R0,DMAIN010         NEXT COMMAND                                 
*                                                                               
DMAINX   J     XIT1                                                             
         DROP  R2                                                               
STATES   DC    AL2(04),CL6'LOPEN ' LOCAL STATE CHANGE COMMANDS                  
         DC    AL2(05),CL6'LCLOSE'                                              
         DC    AL2(10),CL6'LREAD '                                              
         DC    AL2(11),CL6'LOPEN '                                              
         DC    XL2'FFFF'                                                        
         EJECT                                                                  
***********************************************************************         
* EXECUTE COMMANDS                                                    *         
***********************************************************************         
ACTION   DC    F'0'                                                             
         BRAS  RE,SETEOJ           ADV COMMANDS                                 
         DC    F'0'                NOT DATASPACED                               
         DC    F'0'                ..                                           
         BRAS  RE,DSSTRT           4                                            
         BRAS  RE,DSSTOP           5                                            
         BRAS  RE,DSOPEN           6                                            
         BRAS  RE,DSCLOS           7                                            
         BRAS  RE,DSQUI            8                                            
         BRAS  RE,DSUNQU           9                                            
         BRAS  RE,DSREAD           10                                           
         BRAS  RE,DSWRIT           11                                           
         BRAS  RE,DSBROAD          12                                           
         BRAS  RE,DSOPEN           13  QOPEN                                    
         DC    F'0'                14  WAKEUP                                   
         DC    F'0'                15  STATUS RPLS                              
         DC    F'0'                16  FIX ECB                                  
         BRAS  RE,DSENABL          17  RESOURCE ENABLE                          
         BRAS  RE,DSDISABL         18  RESOURCE DISABLE                         
         DC    F'0'                19  CHECK COMMANDS                           
         DC    F'0'                20  STARTTTS                                 
         DC    F'0'                21  RECYCLE                                  
         BRAS  RE,PATCH            22  PATCH (1ST PASS)                         
         BRAS  RE,PATCHIT          23  PATCH IT                                 
         DC    F'0'                24  NEWDAY                                   
         DC    F'0'                25  TASK                                     
         DC    F'0'                26  RESETSIN                                 
         BRAS  RE,SSBSTOP          27  SSB STOP                                 
         BRAS  RE,SSBGO            28  SSB GO                                   
         BRAS  RE,USSSTOP          29  QUIESCE USS RCVR                         
         BRAS  RE,USSGO            30  UNQUIESCE USS RCVR                       
         BRAS  RE,USSSTAT          31  STATUS USS RCVR                          
         BRAS  RE,NOP              32  ENDRUN FOR RUNNERS                       
         DC    F'0'                33  DEQ                                      
         DC    F'0'                34  REBUILD                                  
         DC    F'0'                35  STATE                                    
         BRAS  RE,DSPGACTN         36  PROGRAM STOP                             
         BRAS  RE,DSPGACTN         37  PROGRAM START                            
         BRAS  RE,DSPGACTN         38  PROGRAM RESTRICT                         
         BRAS  RE,DSPGACTN         39  PROGRAM UNRESTRICT                       
*                                                                               
NOP      BR    RE                                                               
*                                                                               
MAXACT   EQU   39                  DONT FORGET TO SET THIS                      
         EJECT                                                                  
***********************************************************************         
* PRESET WORKING STORAGE                                              *         
***********************************************************************         
DSSTRT   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         L     RE,SAVERE                                                        
         J     UPSYS                                                            
*                                                                               
DSSTOP   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         L     RE,SAVERE                                                        
         J     DNSYS                                                            
*                                                                               
DSOPEN   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         BRAS  RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         J     QOPEN                                                            
*                                                                               
DSCLOS   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         BRAS  RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         J     QCLOSE                                                           
*                                                                               
DSQUI    ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         BRAS  RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         J     QUION                                                            
*                                                                               
DSUNQU   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         BRAS  RE,FINDFIL                                                       
         L     RE,SAVERE                                                        
         J     QUIOFF                                                           
*                                                                               
DSREAD   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         L     RE,SAVERE                                                        
         J     ROSYS                                                            
*                                                                               
DSWRIT   ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         L     RE,SAVERE                                                        
         J     RWSYS                                                            
*                                                                               
DSBROAD  ST    RE,SAVERE                                                        
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   BRDNUM,DATA+2                                                    
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         L     RE,SAVERE                                                        
         J     BCAST                                                            
*                                                                               
DSPGACTN ST    RE,SAVERE           DO PROGRAM ACTION                            
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,DATA                                                       
         MVC   FILNUM,DATA+2                                                    
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JNE   DSNEQ                                                            
         BRAS  RE,SETPGMS          SET PROGRAMS                                 
         BRAS  RE,WTO                                                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
DSNOP    ST    RE,SAVERE           NOT USED ANYMORE                             
         J     QNOP                                                             
*                                                                               
DSNEQ    L     RE,SAVERE           SYSTEM NOT DEFINED                           
         BR    RE                                                               
*                                                                               
DSENABL  ST    RE,SAVERE                                                        
         MVC   RESOURCE,DATA+3                                                  
         J     ENABLE                                                           
*                                                                               
DSDISABL ST    RE,SAVERE                                                        
         MVC   RESOURCE,DATA+3                                                  
         J     DISABLE                                                          
         EJECT                                                                  
***********************************************************************         
* SET CANCEL OR DUMP                                                  *         
***********************************************************************         
SETCAN   OI    SSBSTAT1,SSBSEOJ    SET SSB TO EOJ FOR NODUMP                    
SETDUMP  OI    SSBSTAT3,SSBCANCL   FORCE SYSTEM CANCEL                          
         BRAS  RE,CLEANUP                                                       
         DC    H'0'                THIS WILL ABEND SYSTEM                       
         EJECT                                                                  
***********************************************************************         
* RECYCLE/SEND RESTART MESSAGE AND EOJ                                *         
***********************************************************************         
RECYCLE  NTR1                                                                   
         LA    R1,16               REQUEST RESTART                              
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* SEND STARTTTS REQUEST                                               *         
***********************************************************************         
STARTTTS NTR1                                                                   
         LA    R1,16               REQUEST RESTART TTS                          
         BRAS  RE,GETMSG                                                        
         MVI   ONLINE,C'N'         FORCE TO CONSOLE                             
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* STOP ALL ACTIVE SYSTEMS AND SET EOJ                                 *         
***********************************************************************         
SETEOJ   NTR1                                                                   
         LA    R1,2                EOJ ACCEPTED                                 
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
*                                                                               
         LAM   AR2,AR2,SSBALET                                                  
         ICM   R2,15,SSBATOR                                                    
         LA    R2,TORFACLQ(R2)     INDEX TO FACPAK EXCHANGE GRID                
         USING SBEXCHD,R2                                                       
         SAC   512                                                              
*                                                                               
         LH    R4,0(,R2)           BXLE R2,R4,R5                                
         L     R5,2(,R2)                                                        
         LA    R2,6(,R2)                                                        
         XR    RF,RF               RF=ZERO BASED COUNTER                        
*                                                                               
AOR02    CLC   SBSTOKEN,SSBSTOKN   OUR SLOT?                                    
         JE    AOR04               YES                                          
         LA    RF,1(RF)                                                         
         JXLE  R2,R4,AOR02                                                      
         DC    H'0'                WHERE IS OUR FACPAK IN LIST                  
*                                                                               
AOR04    MVI   SBAVLBL,C'N'        SET NOT AVAILABLE                            
         SAC   0                                                                
         OI    SSBSTAT1,SSBSEOJ    SET EOJ                                      
*                                                                               
         L     R6,VSELIST                                                       
         LH    R4,0(R6)            SET BXLE REGS                                
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
STOP010  ST    R6,ASENTRY          SET A(SENTRY)                                
         USING SELISTD,R6                                                       
         TM    SEIND,SEISTRT                                                    
         JNO   STOP020             IF SYSTEM STARTED                            
         CLI   SESYS,1                                                          
         JE    STOP020             AND NOT SERVICE                              
*                                                                               
         BRAS  RE,DNSYS            CALL DOWN SYS                                
*                                                                               
STOP020  JXLE  R6,R4,STOP010       BXLE TO NEXT SYSTEM                          
*                                                                               
         TM    SSBSTAT4,SSBSAOR    IF THIS IS AN AOR                            
         JO    STOP029             JUST STOP IT                                 
                                                                                
***********************************************************************         
* WAIT FOR ALL AORS TO COMPLETE BEFORE EXITING TOR                    *         
***********************************************************************         
                                                                                
STOP021  ICM   R2,15,SSBATOR                                                    
         LA    R2,TORFACLQ(R2)     INDEX TO FACPAK EXCHANGE GRID                
         USING SBEXCHD,R2                                                       
         SAC   512                                                              
*                                                                               
         LH    R4,0(,R2)           BXLE R2,R4,R5                                
         L     R5,2(,R2)                                                        
         LA    R2,6(,R2)                                                        
         XR    RF,RF               RF=ZERO BASED COUNTER                        
*                                                                               
STOP025  CLC   SBSTOKEN,SSBSTOKN   OUR SLOT?                                    
         JE    STOP026             YES                                          
         CLI   SBSTRTD,SBYES                                                    
         JNE   STOP026                                                          
         SAC   0                                                                
         GOTO1 VTICTOC,DMCB,C'WAIT',F'38400'                                    
         J     STOP021                                                          
                                                                                
STOP026  JXLE  R2,R4,STOP025                                                    
*                                                                               
STOP029  SAC   0                                                                
         MVI   DUB+1,13                                                         
         GOTO1 VLOCKSPC,DUB        CLEAR DHAJOBS ENTRY                          
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* WAKE UP THE SYSTEM IF IT GOES TO SLEEP                              *         
***********************************************************************         
WAKEUP   ST    RE,SAVERE                                                        
         WTO   '** HELLO **'                                                    
         L     RE,SAVERE                                                        
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* ECBLST IS NOT HAPPY - TRY TO FIX IT UP                              *         
* THERE ARE 2 CONTROL ECBS - THE POSTECB AND THE OPERATOR ECB         *         
* WE CAN ONLY FIX UP THE BLOCK IF THERE IS 1 OTHER ECB THERE          *         
* (OTHERWISE HOW DO WE KNOW WHICH ONE TO POST)                        *         
* FOR THIS REASON, THE FIRST THING WE DO IS CHECK HOW MANY ARE IN THE *         
* ECBLST.                                                             *         
* THE FIX IS TO POST THE 'STUCK' TRANSACTION AND JUST HOPE THAT THE   *         
* PROBLEM GOES AWAY...                                                *         
***********************************************************************         
FIXECB   NTR1                                                                   
         XR    R0,R0                                                            
         L     R2,VECBLST                                                       
FEC02    OC    0(4,R2),0(R2)                                                    
         JZ    FEC04                                                            
         AHI   R0,1                                                             
         TM    0(R2),X'80'                                                      
         JZ    FEC02                                                            
*                                                                               
FEC04    CHI   R0,3                TWO CONTROL ECBS + 1 OTHER                   
         JE    FEC06                                                            
         MVC   OPMSGTXT(L'FECNO),FECNO                                          
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
*                                                                               
FEC06    L     R2,VECBLST                                                       
         L     R2,0(R2)            DO DUMMY POST                                
         POST  R2                                                               
         MVC   OPMSGTXT(L'FECFIX),FECFIX                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
*                                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK DSCOMM TABLE FOR UNFINISHED BUSINESS                          *         
***********************************************************************         
CHECK    NTR1                                                                   
         SR    R1,R1                                                            
         LAM   AR2,AR2,SSBALET     PICK UP ALET                                 
         SR    R2,R2                                                            
         USING DMDHDR,R2                                                        
         SAC   512                                                              
         L     R2,DHACOMM          SET R2 TO COMMS BLOCK                        
         AHI   R2,4096             SKIP HEADERS                                 
         LA    R0,512                                                           
*                                                                               
         USING DSCOMM,R2                                                        
CHECK010 OC    0(32,R2),0(R2)      CHECK FOR COMMAND                            
         JZ    *+8                                                              
         LA    R1,1(R1)            COUNT COMMS                                  
*                                                                               
         LA    R2,32(,R2)          POINT TO NEXT COMM ENTRY                     
         JCT   R0,CHECK010         NEXT COMMAND                                 
         SAC   0                                                                
*                                                                               
         LA    R1,14               HOW MANY COMMANDS                            
         BRAS  RE,GETMSG                                                        
         EDIT  (R1),(2,OPMSGOPS)                                                
         LTR   R1,R1                                                            
         JNZ   *+10                                                             
         MVC   OPMSGOPS(2),=C'NO'                                               
         CHI   R1,1                                                             
         JNE   *+8                                                              
         MVI   OPMSGOPS+10,C' '                                                 
         BRAS  RE,WTO                                                           
*                                                                               
         J     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SHOW RPL STATUS FOR THIS SYSTEM                                     *         
***********************************************************************         
SHOWRPL  SAM31                                                                  
         ST    RE,SAVERE                                                        
         MVC   OPMSGOPS(22),=C'** RPL STATUS BLOCK **'                          
         BRAS  RE,WTO                                                           
         GOTO1 VLCWRITE,DMCB,VTGETRPL,0                                         
         MVC   ARPL,4(R1)          GET A(FIRST RPL)                             
         L     R6,ARPL             SCAN RPL LIST                                
         USING FARPLD,R6                                                        
RPL01    TM    FARPLFLG,FARPLBSY   LOOKING FOR A BUSY ONE                       
         JO    RPL02                                                            
RPLNXT   ICM   R6,15,FARPLNXT                                                   
         JZ    RPLXX               LAST RPL SO EXIT                             
         J     RPL01                                                            
RPL02    L     R3,FARPLNIB         GET A(NIB)                                   
         MVC   NSYM,NIBSYM-NIBST(R3)                                            
         L     R3,FARPLRPL                                                      
         L     R3,92(R3)           RPLUSFLD                                     
         USING UTLD,R3                                                          
         MVC   NSYM,TSYM           GET LUID FROM UTL                            
         L     R3,VUTL                                                          
         LH    R4,0(R3)                                                         
         L     R5,2(R3)                                                         
         LA    R3,6(R3)                                                         
RPL03    CLC   TSYM,NSYM           FIND UTL ENTRY                               
         JNE   RPL04                                                            
*                                                                               
         MVC   OPMSGTXT+0(3),FARPLEYE                                           
         MVC   OPMSGTXT+4(8),TSYM                                               
         SR    R0,R0                                                            
         L     R1,TTIME            TERMINAL TIME                                
         D     R0,=F'384'                                                       
         ST    R1,FULL                                                          
         SAM24                                                                  
         GOTO1 =V(TIMEOUT),DMCB,(1,FULL),(X'45',WORK),RR=RELO                   
         MVC   OPMSGTXT+14(8),WORK                                              
         MVC   OPMSGTXT+24(11),=C'TSTAT3/4/5='                                  
         SAM31                                                                  
         MVC   FULL,TSTAT3                                                      
         GOTO1 AHEXOUT,DMCB,FULL,OPMSGTXT+36,3                                  
         BRAS  RE,WTO                                                           
         J     RPLNXT                                                           
*                                                                               
RPL04    JXLE  R3,R4,RPL03                                                      
*                                                                               
RPLXX    SAM24                                                                  
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* SHOW TASK STATUS FOR THIS SYSTEM                                    *         
***********************************************************************         
TASK     L     R6,VTCB             GET LIST OF TCBS                             
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING TCBD,R6                                                          
*                                                                               
TASKNEXT CLI   TCBSEN,0                                                         
         JE    TASK400             NOTHING DOING                                
         MVI   OPTSKID,C'T'                                                     
         MVC   OPTSKID+1(1),TCBID+6                                             
         MVC   OPTSKWT,DOTS                                                     
         XR    R1,R1                                                            
         ICM   R1,7,TCBLOCK+1                                                   
         JZ    TASK010                                                          
         CLC   =C'*TASK',0(R1)     IS THIS ANOTHER TASK?                        
         JNE   TASK010             NO                                           
         MVI   OPTSKWT,C'T'                                                     
         MVC   OPTSKWT+1(1),6(R1)  YES, DISPLAY NUMBER                          
*                                                                               
TASK010  MVC   OPTSKLU,DOTS                                                     
         CLI   TCBSYM,C' '                                                      
         JNH   *+10                                                             
         MVC   OPTSKLU,TCBSYM      LUID                                         
         MVC   OPTSKSP,DOTS        SYSTEM/PROGRAM                               
         GOTOR PGMCHASE,TCBSEN                                                  
         MVC   OPTSKSP,WORK                                                     
         BRAS  RE,WTO                                                           
*                                                                               
TASK400  JXLE  R6,R4,TASKNEXT                                                   
         J     XIT1                                                             
                                                                                
***********************************************************************         
* PASS TCBSEN/TCBPGM IN HALF                                          *         
***********************************************************************         
PGMCHASE NTR1                                                                   
         MVC   HALF,0(R1)                                                       
         GOTO1 AHEXOUT,DMCB,HALF,FULL,2  EXPAND SYS/PROG                        
         MVC   WORK(21),SPACES                                                  
         MVI   WORK+7,C'/'                                                      
*                                                                               
         XC    SYNAME,SYNAME                                                    
         MVC   SYNUM,HALF                                                       
         BRAS  RE,FINDSYS          SET UP SYSTEM                                
         JE    PC02                                                             
         MVC   WORK(2),FULL        MOVE X'SS' TO SSSSSSS AREA                   
         J     PC06                                                             
*                                                                               
PC02     LA    R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         LA    R2,SENAME+L'SENAME-1                                             
         CLI   0(R2),C' '                                                       
         JNE   *+8                 FIND FIRST NON-BLANK                         
         JCT   R2,*-8                                                           
*                                                                               
         CLI   0(R2),C'0'          IS LAST CHAR NUMERIC ?                       
         JL    *+14                YES - SKIP                                   
         MVC   WORK(7),SENAME      MOVE WHOLE SENAME                            
         J     PC04                                                             
*                                                                               
         MVC   WORK(4),SENAME      MOVE ONLY FIRST FOUR CHARS                   
*                                                                               
PC04     L     R4,SEPGMS           PROGRAM NAME LIST FOR THIS SYSTEM            
         USING PGMLSTD,R4                                                       
         LH    R2,0(R4)                                                         
         L     R3,2(R4)                                                         
         LA    R4,6(R4)                                                         
         CLC   PGMNUM,HALF+1       MATCH PROGRAM                                
         JE    PC08                                                             
         JXLE  R4,R2,*-10                                                       
*                                                                               
PC06     MVC   WORK+8(2),FULL+2    MOVE X'PP' TO PPPPPPP AREA                   
         J     PC10                                                             
*                                                                               
PC08     MVC   WORK+8(7),PGMNAME                                                
         DROP  R4,R5                                                            
*                                                                               
PC10     LA    RE,WORK             COMPRESS ALL BLANKS FROM WORK                
         LA    RF,1(RE)                                                         
         LA    R0,WORK+20                                                       
*                                                                               
PC12     CLI   0(RE),C' '          SAVE WORK ADDRESS                            
         JNE   PC14                                                             
         MVC   0(1,RE),0(RF)                                                    
         MVI   0(RF),C' '                                                       
         AHI   RF,1                                                             
         J     PC12                                                             
*                                                                               
PC14     AHI   RE,1                                                             
         CR    RF,RE                                                            
         JH    *+8                                                              
         LA    RF,1(RE)                                                         
PC16     CR    RE,R0                                                            
         JL    PC12                                                             
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* SET UP NEW BILLING DATE FOR SYSTEM                                  *         
***********************************************************************         
NEWDAY   NTR1                                                                   
         STAM  ARE,AR1,DUB         USE DUB AND DUB1 TO SAVE ARS                 
         TIME  TU                                                               
         LAM   ARE,AR1,DUB                                                      
         ST    R1,TODAY            SAVE DATE IN TODAY                           
         ST    R0,FULL             SAVE TIME IN FULL                            
*                                                                               
         MVC   HALF,SYNUM                                                       
         OC    SYNUM,SYNUM                                                      
         JNE   NEWD001             SPECIFIC SYSTEM                              
         LHI   R1,1                                                             
         STH   R1,HALF             OR BEGIN WITH SYSTEM 1                       
*                                                                               
NEWD001  GOTO1 ADATCON,DMCB,(6,TODAY),(0,DUB)                                   
         GOTO1 AADDAY,DMCB,(C'D',DUB),DUB1,F'1'                                 
         GOTO1 ADATCON,DMCB,(0,DUB1),(15,TOMORROW)                              
*                                                                               
NEWD002  LAM   AR2,AR2,SSBALET     PICK UP ALET                                 
         SAC   512                                                              
         SR    R2,R2               SET R2 TO ADVS BLOCK                         
         ICM   R2,3,HALF                                                        
         JZ    NEWDAYXX                                                         
         SLL   R2,6                                                             
*                                                                               
         MVC   SYNAME,16(R2)                                                    
*                                                                               
         ICM   R2,15,60(R2)                                                     
         JZ    NEWDAYXX                                                         
         SLL   R2,2                                                             
         SRL   R2,2                                                             
         USING DMSYSHDR,R2                                                      
         L     R1,TODAY                                                         
         ST    R1,DSYBDATE         UPDATE BILLING DATE TO TODAY                 
*                                                                               
         CLC   FULL,=XL4'8C136000'       AFTER 17:00                            
*NOP     CLC   FULL,=XL4'9450C000'       AFTER 18:00                            
*NOP     CLC   FULL,=AL4(12*60*60*38400) AFTER 12:00 MIDDAY                     
*NOP     CLC   FULL,=XL4'C59DD800'       AFTER 23:59                            
         JL    NEWD003                                                          
*                                                                               
         CLC   DSYBDATE,TODAY      IF TODAY THEN SET TOMORROW                   
         JNL   NEWD004                                                          
*                                                                               
         MVC   WORK(2),=X'002A'    ELSE WARN AND SET TO TODAY                   
         MVC   WORK+2(40),=C'*WARNING* LATE NEWDAY ISSUED FOR        '          
         MVC   WORK+35(7),SYNAME                                                
         WTO   TEXT=WORK                                                        
         LA    R1,22               DATE HAS BEEN SET TO TODAY                   
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
*                                                                               
NEWD003  L     R1,TODAY                                                         
         ST    R1,DSYBDATE         UPDATE BILLING DATE TO TODAY                 
         J     NEWDAYX                                                          
*                                                                               
NEWD004  L     R1,TOMORROW                                                      
         ST    R1,DSYBDATE         UPDATE BILLING DATE TO TOMORROW              
*                                                                               
NEWDAYX  SAC   0                                                                
         ST    R1,DUB                                                           
         GOTO1 ADATCON,DMCB,(6,DUB),(17,DUB1)                                   
         LA    R1,21               NEWDAY MESSAGE                               
         BRAS  RE,GETMSG                                                        
         MVC   OPMSGOPS+7(8),SYNAME                                             
         MVC   OPMSGOPS+23(7),DUB1                                              
         BRAS  RE,WTO                                                           
NEWDAYXX SAC   0                                                                
*                                                                               
         OC    SYNUM,SYNUM         SPECIFIC SYSTEM                              
         JNZ   XIT1                                                             
*                                                                               
         LH    R1,HALF             IF NEWDAY,,ALL                               
         AHI   R1,1                                                             
         STH   R1,HALF             NEXT SYSTEM                                  
         CHI   R1,256                                                           
         JL    NEWD002                                                          
*                                                                               
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* RSET SIN AND SET FACPAK SSB TO NEW DATE                             *         
***********************************************************************         
RESETSIN NTR1                                                                   
*&&DO                                                                           
         GOTO1 ADATCON,DMCB,(5,0),(10,SSBDATE)  C'DD/MM/YY'                     
         GOTO1 (RF),(R1),,(3,SSBDATEB)                                          
         GOTO1 (RF),(R1),(3,SSBDATEB),(0,WORK)                                  
         GOTO1 AGETDAY,DMCB,WORK,WORK+10                                        
         MVC   SSBDAYNO,DMCB       SET DAY NUMBER IN SSB                        
*                                                                               
         MVC   SSBSIN,=X'00000001'                                              
*&&                                                                             
         LA    R1,20               RESET SIN AND DATE ALSO                      
         BRAS  RE,GETMSG                                                        
*&&DO*&& MVC   OPMSGOPS+22(8),SSBDATE                                           
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* SSB STOP - INHIBIT USER INPUT                                       *         
***********************************************************************         
SSBSTOP  NTR1                                                                   
         OI    SSBSTAT1,SSBUII                                                  
         LA    R1,18               SSB STOP                                     
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* SSB GO - RESTORE USER INPUT                                         *         
***********************************************************************         
SSBGO    NTR1                                                                   
         NI    SSBSTAT1,255-SSBUII                                              
         LA    R1,19               SSB GO                                       
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* REBUILD CORERES PHASES                                              *         
***********************************************************************         
REBUILD  NTR1                                                                   
         BRAS  RE,ALLWAIT                                                       
         OI    SSBSTAT4,SSBNOGO    DO REBUILD                                   
         LA    R1,23                                                            
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* USS GO - ENABLE USS WRITES                                          *         
***********************************************************************         
USSGO    NTR1                                                                   
         MVC   SYNUM,DATA                                                       
         LA    R0,128+1            START ACTION IN P1                           
         ZICM  RF,SYNUM+1          SE# TO START IN P2                           
         GOTO1 VRCVRUSS,DMCB,(R0),(RF)                                          
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* USS STOP - INHIBIT USS WRITES                                       *         
***********************************************************************         
USSSTOP  NTR1                                                                   
         MVC   SYNUM,DATA                                                       
         LA    R0,128+2            STOP ACTION IN P1                            
         ZICM  RF,SYNUM+1          SE# TO START IN P2                           
         GOTO1 VRCVRUSS,DMCB,(R0),(RF)                                          
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* USS STATUS - INHIBIT USS WRITES                                     *         
***********************************************************************         
USSSTAT  NTR1                                                                   
         MVC   SYNUM,DATA                                                       
         LA    R0,128+3            STATUS ACTION                                
         GOTO1 VRCVRUSS,DMCB,(R0)                                               
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD BLOCK COMMAND HEADER                                          *         
***********************************************************************         
SETHEAD  NTR1                                                                   
         XC    WORK,WORK           BUILD COMLINE IN WORK                        
         LA    R4,WORK                                                          
         USING DSCOMM,R4                                                        
         MVC   DSCSORC,FFS         SET TO FFS TO INDICATE HEADER                
         MVC   DSCDEST,FFS                                                      
         MVC   DSCCOMM,FFS                                                      
         MVC   DSCDATA(8),COMMAND  PUT COMMAND NAME IN DATA                     
*                                                                               
         TIME  BIN                 SAVE TIME IN DSCTIME                         
         ST    R0,DSCTIME                                                       
*                                                                               
         SR    R2,R2               SET TO DATASPACE                             
         USING DMDSHDR,R2                                                       
         LAM   AR2,AR2,SSBALET                                                  
         LAM   AR3,AR3,SSBALET                                                  
         SAC   512                                                              
*                                                                               
SETHED3  SR    R2,R2               POINT TO DSPACE DHACOMM                      
         LA    RE,128              MUST FIND A FREE ENTRY WITHIN 128            
         L     R2,4(,R2)                                                        
SETHED4  L     RF,=F'-1'           SET TO FFS TO GRAB ENTRY                     
         SR    R1,R1                                                            
         CS    R1,RF,0(R2)                                                      
         JNE   SETHED5                                                          
         MVC   0(32,R2),WORK                                                    
         SAC   0                   GO POST THE SSBOPECB                         
         J     SETHEDX                                                          
*                                                                               
SETHED5  LA    R2,32(,R2)                                                       
         JCT   RE,SETHED4          ALL ENTRIES FULL                             
         BRAS  RE,CLEANUP                                                       
         DC    H'0'                                                             
*                                                                               
SETHEDX  LA    RF,129                                                           
         SR    RF,RE                                                            
         STC   RF,HEADER           SAVE VALUE OF HEADER                         
         LAM   AR3,AR3,=F'0'       MUST CLEAR THIS                              
         SAC   0                                                                
SETHEADX J     XIT1                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK FOR BLOCK COMPLETION                                          *         
***********************************************************************         
CHKHEAD  NTR1                                                                   
         SR    R2,R2               SET TO DATASPACE                             
         USING DMDSHDR,R2                                                       
         SR    R2,R2               POINT TO DSPACE DHACOMM                      
         LA    RE,512                                                           
         L     R2,4(,R2)                                                        
         AHI   R2,4096             SKIP HEADERS                                 
         USING DSCOMM,R2                                                        
*                                                                               
CHKHED4  CLC   DSCHEADR,HEADER     ANY OTHER WITH THIS HEADER                   
         JE    CHKHEDX                                                          
*                                                                               
CHKHED5  LA    R2,32(,R2)                                                       
         JCT   RE,CHKHED4          CHECK ALL ENTRIES                            
*                                                                               
         SR    R2,R2               LOCATE HEADER                                
         L     R2,4(,R2)                                                        
         IC    R1,HEADER                                                        
         BCTR  R1,0                                                             
         SLL   R1,5                                                             
         AR    R2,R1                                                            
         MVC   COMMAND,16(R2)      SAVE COMMAND                                 
         XC    0(32,R2),0(R2)      CLEAR HEADER                                 
         SAC   0                                                                
         LA    R1,15               SEND COMPLETED MESSAGE                       
         BRAS  RE,GETMSG                                                        
         MVC   OPMSGOPS+8(8),COMMAND                                            
         BRAS  RE,WTO                                                           
         SAC   512                                                              
*                                                                               
CHKHEDX  J     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD COMMAND FOR DATASPACE                                         *         
***********************************************************************         
SETCOM   NTR1                                                                   
         XC    WORK,WORK           BUILD COMLINE IN WORK                        
         LA    R4,WORK                                                          
         USING DSCOMM,R4                                                        
         MVC   DSCSORC,SSBSYSIX    FROM ME                                      
         MVI   DSCSORC+1,0                                                      
         MVC   DSCDEST,ADNUM       TO ADNUM (ZERO FOR ALL)                      
         MVI   DSCDEST+1,0                                                      
         MVC   DSCCOMM,ACTNUM      SET ACTION                                   
         MVC   DSCHEADR,HEADER                                                  
*                                                                               
         MVC   DSCDATA(2),SYNUM    SET SYS/FIL INFO IN DATA                     
         MVC   DSCDATA+2(1),FILNUM                                              
         MVC   DSCDATA+3(2),RESOURCE                                            
*                                                                               
         CLC   ACTNUM,=H'33'       IF DEQ USE 16 CHR RESOURCE                   
         JNE   *+10                                                             
         MVC   DSCDATA(16),RESCL16                                              
*                                                                               
         TM    FLAG,X'40'          IS FILENUM REQUIRED                          
         JZ    *+12                                                             
         CLI   FILNUM,0                                                         
         JE    NODATA                                                           
         TM    FLAG,X'10'          ANY DATA REQUIRED                            
         JO    *+14                                                             
         OC    DSCDATA,DSCDATA     MUST BE SOME DATA                            
         JZ    NODATA                                                           
         TIME  BIN                 SAVE TIME IN DSCTIME                         
         ST    R0,DSCTIME                                                       
*                                                                               
         SR    R2,R2               SET TO DATASPACE                             
         USING DMDSHDR,R2                                                       
         LAM   AR2,AR2,SSBALET                                                  
         LAM   AR3,AR3,SSBALET                                                  
         SAC   512                                                              
*                                                                               
SETCOM1  SR    R2,R2                                                            
         ICM   R1,15,SYGROUP       NO GROUP SO JUST DO ONE SYSTEM               
         JZ    SETCOM1A                                                         
         CLI   0(R1),X'FF'         END OF GROUP?                                
         JE    SETCOMX                                                          
         MVC   DSCDATA+1(1),0(R1)  MOVE SYSTEM FROM GROUP                       
*                                                                               
SETCOM1A L     R3,DHAJOBS          SET R3 TO ADV SYS BLOCK                      
         LA    R0,DSJOBMXQ                                                      
SETCOM2  CLI   0(R3),0             EMPTY ADV ENTRY                              
         JE    SETCOM6                                                          
*                                                                               
         OC    16(4,R3),16(R3)     IGNORE IF NO POST ECB                        
         JZ    SETCOM6                                                          
         MVC   BYTE,10(R3)                                                      
         NI    BYTE,X'0F'          MATCH ON AOR AND TOR                         
         CLI   DSCDEST,0           DEST ZERO SEND TO ALL                        
         JE    SETCOM2A                                                         
         CLI   10(R3),0            OTHERWISE IGNORE NON ADVS                    
         JE    SETCOM6                                                          
         CLC   DSCDEST(1),BYTE     DOES DEST MATCH                              
         JNE   SETCOM6                                                          
*                                                                               
SETCOM2A CLC   DSCCOMM,=X'0001'    SPECIAL RULES FOR EOJ                        
         JNE   SETCOM2B                                                         
         CLI   DSCDEST,0           IF EOJ,, TO ALL SYSTEMS                      
         JNE   SETCOM2B                                                         
         CLC   SSBSYSID,BYTE       ALL IS ONLY ALL MY AORS                      
         JNE   SETCOM6                                                          
         TM    SSBSTAT4,SSBSAOR    IF THIS IS AN AOR                            
         JZ    SETCOM2B                                                         
         CLC   SSBSYSIX,10(R3)     THEN IT'S ONLY ME                            
         JNE   SETCOM6                                                          
*                                                                               
SETCOM2B OC    JOBNAME,JOBNAME     ANY JOBNAME SPECIFIED                        
         JZ    SETCOM3                                                          
         LLC   R1,JOBNAMEL                                                      
         BCTR  R1,0                                                             
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   JOBNAME(0),0(R3)    MATCH ON JOBNAME                             
         JNE   SETCOM6                                                          
*                                                                               
SETCOM3  SR    R2,R2               POINT TO DSPACE DHACOMM                      
         LA    RE,512                                                           
         L     R2,4(,R2)                                                        
         AHI   R2,4096             SKIP HEADERS                                 
SETCOM4  L     RF,=F'-1'           SET TO FFS TO GRAB ENTRY                     
         SR    R1,R1                                                            
         CS    R1,RF,0(R2)                                                      
         JNE   SETCOM5                                                          
         MVC   0(32,R2),WORK                                                    
         MVC   6(1,R2),10(R3)      SEND TO THIS ADV                             
*                                                                               
         CLI   10(R3),0            ADV OR JOB                                   
         JNE   SETCOM4A                                                         
         MVC   6(2,R2),12(R3)      JOB THEN USE ASID                            
         OI    10(R2),DSCJNUMQ                                                  
*                                                                               
SETCOM4A MVC   WORK1,0(R3)                                                      
         SAC   0                   GO POST THE SSBOPECB                         
         BRAS  RE,POSTIT                                                        
         SAC   512                 SWITCH BACK TO AR MODE,CC UNCHANGED          
         JNE   *+2                                                              
         JE    *+10                                                             
         XC    0(32,R2),0(R2)      IF POST FAILS THEN REMOVE IT                 
         J     SETCOM6                                                          
*                                                                               
SETCOM5  LA    R2,32(,R2)                                                       
         JCT   RE,SETCOM4          ALL ENTRIES FULL                             
         BRAS  RE,CLEANUP                                                       
         DC    H'0'                                                             
*                                                                               
SETCOM6  LA    R3,DSJOBLNQ(,R3)                                                 
         JCT   R0,SETCOM2          ALL JOBS DONE                                
*                                                                               
         ICM   R1,15,SYGROUP       TEST FOR GROUP                               
         JZ    SETCOMX                                                          
         LA    R1,1(R1)                                                         
         CLI   0(R1),X'FF'         TEST FOR END OF GROUP                        
         JE    SETCOMX                                                          
         ST    R1,SYGROUP                                                       
         J     SETCOM1                                                          
*                                                                               
SETCOMX  LAM   AR3,AR3,=F'0'       MUST CLEAR THIS                              
         SAC   0                                                                
         J     XIT1                                                             
*                                                                               
NODATA   LA    R1,9                MISSING DATA                                 
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* DO CROSS MEMORY POST TO FACPAK (DETAILS IN CARD)                    *         
***********************************************************************         
POSTIT   NTR1                                                                   
         LARL  R9,POSTIT           LOCAL BASE                                   
         USING POSTIT,R9                                                        
         LH    R4,WORK1+12         ASID IS AT +12                               
         L     R2,WORK1+16         SSBOPECB IS AT +16                           
         LOCASCB ASID=(R4)                                                      
         LR    R3,R1                                                            
         LTR   RF,RF                                                            
         JNZ   NOPOST                                                           
         USING ASCB,R3                                                          
         CLC   ASCBASCB,=C'ASCB'                                                
         JNE   NOPOST                                                           
         L     R4,ASCBASSB                                                      
*                                                                               
         SAM31                                                                  
         L     R4,ASSBJSAB-ASSB(R4) R4=A(JSAB)                                  
         USING JSAB,R4                                                          
         CLC   WORK1(8),JSABJBNM                                                
         SAM24                                                                  
         JNE   NOPOST                                                           
*                                                                               
         LA    R5,99               SET COMPLETION CODE                          
         POST  (R2),(R5),ASCB=(R3),LINKAGE=SYSTEM,ECBKEY=8,MF=(E,POSTX)         
         J     XITEQU                                                           
*                                                                               
NOPOST   MVC   WORK1+10(8),WORK1                                                
         MVC   WORK1+2(8),WORK1+10                                              
         MVC   WORK1+10(18),=C' SYSTEM NOT POSTED'                              
         MVC   WORK1+0(2),=H'26'                                                
         WTO   TEXT=WORK1                                                       
         J     XITNEQ                                                           
POSTX    POST  ERRET=DEAD,ECBKEY=YES,MF=L                                       
DEAD     DC    H'0'                                                             
         DROP  R3,R4,R9                                                         
         EJECT                                                                  
***********************************************************************         
* ENABLE A RESOURCE                                                   *         
***********************************************************************         
ENABLE   NTR1                                                                   
         XC    WORK,WORK           ENABLE MEDIA DATASPACE                       
         MVC   WORK(4),=C'GETA'                                                 
         MVC   WORK+8(8),=C'MEDDSPC '                                           
         CLC   RESOURCE,=AL2(1)    ENABLE EXISTING DATASPACE TABLE ID           
         JE    ENABMED                                                          
         CLC   RESOURCE,=AL2(2)                                                 
         JE    ENABMED1                                                         
         CLC   RESOURCE,=AL2(3)                                                 
         JE    ENABMED2                                                         
         CLC   RESOURCE,=AL2(4)                                                 
         JE    ENABMED3                                                         
         CLC   RESOURCE,=AL2(5)                                                 
         JE    ENABMED4                                                         
         CLC   RESOURCE,=AL2(6)                                                 
         JE    ENABMED5                                                         
         J     XIT1                                                             
*                                                                               
ENABMED  MVC   WORK+4(4),SSBMEDTN                                               
         J     ENABM10                                                          
*                                                                               
ENABMED1 MVC   WORK+4(4),=C'MED1'                                               
         J     ENABM10                                                          
*                                                                               
ENABMED2 MVC   WORK+4(4),=C'MED2'                                               
         J     ENABM10                                                          
*                                                                               
ENABMED3 MVC   WORK+4(4),=C'MED3'                                               
         J     ENABM10                                                          
*                                                                               
ENABMED4 MVC   WORK+4(4),=C'MED4'                                               
         J     ENABM10                                                          
*                                                                               
ENABMED5 MVC   WORK+4(4),=C'MED5'                                               
         J     ENABM10                                                          
*                                                                               
ENABMED6 MVC   WORK+4(4),=C'MED6'                                               
         J     ENABM10                                                          
*                                                                               
ENABM10  LA    R0,21                                                            
         LNR   R0,R0                                                            
         LA    R1,WORK                                                          
         SVC   247                                                              
         LTR   RF,RF               TEST OK RETURN                               
         JNZ   ENABLEXX                                                         
         OC    WORK+24(4),WORK+24  TEST ALET FOUND                              
         JZ    ENABLEXX                                                         
         MVC   SSBMEDTN,WORK+4     MOVE IN TABLE NAME                           
         MVC   SSBMEDTB,WORK+20    MOVE IN ORIGIN AND ALET                      
         LA    R1,12                                                            
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
*                                                                               
ENABLEXX LA    R1,11               UNABLE TO OPEN                               
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* DISABLE A RESOURCE                                                  *         
***********************************************************************         
DISABLE  NTR1                                                                   
         CLC   RESOURCE,=AL2(1)                                                 
         JE    DISMED                                                           
         CLC   RESOURCE,=AL2(2)                                                 
         JE    DISMED                                                           
         CLC   RESOURCE,=AL2(3)                                                 
         JE    DISMED                                                           
         CLC   RESOURCE,=AL2(4)                                                 
         JE    DISMED                                                           
         J     XIT1                                                             
*                                                                               
DISMED   XC    SSBMEDTB,SSBMEDTB                                                
         LA    R1,13                                                            
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* NOT USED ANYMORE                                                    *         
***********************************************************************         
QNOP     NTR1                                                                   
         SAC   0                                                                
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* START A SYSTEM                                                      *         
***********************************************************************         
UPSYS    NTR1                                                                   
         CLC   0(8,R1),=C'READONLY'                                             
         JE    *+8                                                              
         BRAS  RE,RWSYS            MAKE SURE WRITES ENABLED                     
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         JZ    UPOK                NO - EXIT                                    
         TM    SEIND,SEISTRT       MAKE SURE SYSTEM IS DOWN                     
         JZ    UPS0                                                             
         BRAS  RE,DNSYS            IF NOT FORGET IT                             
*                                                                               
UPS0     TM    SEIND,SEIACTV       ONLY START IF PREVIOUSLY ACTIVE              
*NOP     JZ    UPX                                                              
         BRAS  RE,SETSYS           FRIG TCB ENTRY FOR SYSCHA                    
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VFINDSYS,(SESYS,0)                                 
*                                                                               
         L     R1,4(R1)            R1=A(SYSFLES)                                
         USING SYSFLSTD,R1                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R1,SYSFLIST         R1=A(FILE LIST ENTRY)                        
         XC    AREQUEST,AREQUEST                                                
*                                                                               
UPS1     SR    RE,RE               RE=A(DTF)                                    
         ICM   RE,7,SYSFADTF                                                    
         USING ISDTF,RE                                                         
         TM    SYSFIND1,SFISF      TEST I/S FILE                                
         JZ    UPS1A                                                            
         TM    SYSFIND2,SFALIAS    TEST NON NATIVE FILE                         
         JO    UPS1A                                                            
         XC    ISCILAST,ISCILAST   CLEAR ISCILAST FOR ALL I/S FILES             
UPS1A    TM    SYSFIND1,SFREQ      TEST REQUEST FILE                            
         JZ    *+8                                                              
         ST    RE,AREQUEST                                                      
*                                                                               
UPS1B    LA    R1,SYSFLNQ(R1)      BUMP TO NEXT FILE                            
         JCT   R0,UPS1                                                          
         DROP  R1,RE                                                            
*                                  OPEN SEFILES                                 
         BRAS  RE,SINGLE                                                        
         GOTO1 VDMOD000,DMCB,VOPENSYS,(SESYS,IOAREA)                            
         BRAS  RE,MULTI                                                         
         OC    DMCB+8(2),DMCB+8                                                 
         JZ    UPS2                                                             
*                                                                               
         MVC   DUB(2),DMCB+8       SAVE ERROR BYTES                             
         MVC   DMCB(4),VCLSESYS                                                 
         GOTO1 VDMOD000,DMCB       CLOSE SYSTEM FILES                           
         MVC   DMCB+8(2),DUB                                                    
         BRAS  RE,RELSYS           RESET TCB ENTRY                              
*                                                                               
         TM    DMCB+9,X'80'        INVALID CPU ID                               
         JNO   *+14                                                             
         MVC   OPCMND,=C'INVLID CPU'                                            
         J     UPS1X                                                            
*                                                                               
         TM    DMCB+9,X'40'        MISSING CPU ID                               
         JNO   *+14                                                             
         MVC   OPCMND,=C'MISSNG CPU'                                            
         J     UPS1X                                                            
*                                                                               
         TM    DMCB+9,X'20'        ACTIVE IN ANOTHER PARTITION                  
         JNO   *+14                                                             
         MVC   OPCMND,=C'ACTIVE IAP'                                            
         J     UPS1X                                                            
*                                                                               
         TM    DMCB+9,X'08'        DID LOCKSPC FIND ANOTHER JOB                 
         JO    UPX                                                              
         BRAS  RE,CLEANUP          CLEANUP AND DIE                              
         DC    H'0'                                                             
*                                                                               
UPS1X    L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         BRAS  RE,WTO                                                           
         J     UPX                                                              
*                                  LOAD & GO TO RECOVERY RESTORE                
UPS2     EQU   *                                                                
         GOTO1 VCALLOV,DMCB,IOAREA,X'D9010100'                                  
         CLI   4(R1),X'FF'                                                      
         JNE   *+10                                                             
         BRAS  RE,CLEANUP                                                       
         DC    H'0'                                                             
         LA    RF,IOAREA                                                        
         XC    DMCB(12),DMCB                                                    
         GOTO1 (RF),(R1),,(SESYS,(R7)),0                                        
         CLI   8(R1),0                                                          
         JE    UPS3                                                             
*NOP     BRAS  RE,CLEANUP                                                       
*NOP     DC    H'0'                                                             
         WTO   '*RCVR ERROR*'                                                   
*NOP     J     UPX                 RUN WITH IT ANYWAY                           
*                                                                               
UPS3     ICM   R1,15,AREQUEST      TEST SYSTEM REQUEST FILE ERASED              
         JZ    UPS4                                                             
         CLC   DNEXT-DTFPHD(L'DNEXT,R1),=X'00010000'                            
         JNE   UPS4                                                             
         AHI   R1,-4                                                            
         ICM   R1,15,0(R1)         YES - POINT TO REQUEST ADDRESS LIST          
         JZ    UPS4                                                             
*                                                                               
         LH    RE,0(R1)            SET UP FOR REQUEST LIST BXLE                 
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         XC    2(6,R1),2(R1)       AND CLEAR REQUEST POINTERS                   
         JXLE  R1,RE,*-6                                                        
*                                                                               
UPS4     BRAS  RE,RELSYS           RESET TCB ENTRY                              
*                                                                               
UPOK     NI    SEIND,X'FC'                                                      
         OI    SEIND,X'80'         SET SE UP                                    
         MVC   SVSEIND,SEIND                                                    
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STARTED                                                   
         BRAS  RE,WTO                                                           
*                                                                               
* IF ITS A TOR AND MQ IS ACTIVE, PERFORM OPENSYS TO COPY ANY WORK Q             
* MESSAGES WAITING FOR A SYSTEM BACK TO THE INPUT Q                             
*                                                                               
         TM    SSBSTAT4,SSBSAOR                                                 
         JO    UPX                                                              
         OC    SSBMQION,SSBMQION                                                
         JZ    UPX                                                              
         ICM   RF,15,AMQIO                                                      
         JZ    UPX                                                              
         GOTO1 (RF),DMCB,=CL8'OPENSYS ',0,0,0                                   
*                                                                               
UPX      J     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* STOP A SYSTEM                                                       *         
***********************************************************************         
DNSYS    NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         TM    SEIND,X'03'         TEST RECOVERY FAILURE                        
         JO    DN01                                                             
         TM    SEIND,X'80'         IGNORE IT IF ALREADY DOWN                    
         JZ    XIT1                                                             
*                                                                               
DN01     MVC   OPSYSID(7),0(R6)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STOPPED                                                   
         OC    SEFILES,SEFILES     ANY FILES                                    
         JZ    DNOK                NO - XIT1                                    
         BRAS  RE,SYSWAIT                                                       
*                                                                               
         OI    SEIND,SEINOP        SET SE NO-OP                                 
         NI    SEIND,X'75'         TURN-OFF OP,QUI & STOP                       
         BRAS  RE,QOFFALL                                                       
*                                                                               
DN4      BRAS  RE,SETSYS           CLOSE SE FILES                               
         BRAS  RE,SINGLE                                                        
         XC    DMCB(24),DMCB                                                    
         LA    R2,WORK             POINT TO DUMMY DMCB                          
         GOTO1 VDMOD000,DMCB,VCLSESYS,(SESYS,IOAREA)                            
         BRAS  RE,MULTI                                                         
         BRAS  RE,RELSYS                                                        
*                                                                               
DNOK     BRAS  RE,WTO                                                           
         J     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LOOP UNTIL SYSTEM AT ASENTRY IS INACTIVE                            *         
***********************************************************************         
SYSWAIT  NTR1                                                                   
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OI    SEIND,X'01'         SET 01 TO SUSPEND ALL INPUT                  
         L     R3,=F'600'          LOOP COUNT                                   
*                                                                               
SWAIT1   L     R6,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING TCBD,R6                                                          
*                                                                               
SWAIT2   CLC   TCBSEN,SESYS        LOOK FOR ACTIVE TASK                         
         JE    SWAIT8                                                           
*                                                                               
         LA    R1,TCBSWTAB         LOOK IN SWITCH TAB                           
         LA    R0,TCBSWMAX                                                      
SWAIT5   CLC   0(1,R1),TCBSEN      FIND THIS SYSTEM                             
         JNE   SWAIT6                                                           
         OC    11(8,R1),11(R1)     NO RECOVERY IS OK                            
         JZ    SWAIT7                                                           
         J     SWAIT8              RECOVERY FOUND SO WAIT                       
SWAIT6   LA    R1,TCBSWLEN(R1)                                                  
         JCT   R0,SWAIT5           NEXT SWTAB ENTRY                             
*                                                                               
SWAIT7   JXLE  R6,R4,SWAIT2                                                     
         NI    SEIND,255-X'01'     RESET INDICATOR                              
         J     SWAIT9                                                           
*                                                                               
* WAIT FOR 1/10 SEC. AFTER 60 SECONDS GIVE UP                                   
*                                                                               
SWAIT8   GOTO1 VTICTOC,DMCB,C'WAIT',F'3840'                                     
*                                                                               
         JCT   R3,SWAIT1                                                        
         NI    SEIND,255-X'01'     RESET INDICATOR                              
         BRAS  RE,CLEANUP          FUCK THIS I'VE HAD ENOUGH                    
         LA    R1,7                SYSTEM IS BUSY                               
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
*                                                                               
SWAIT9   J     XIT1                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LOOP UNTIL ALL SYSTEMS ARE INACTIVE                                 *         
***********************************************************************         
ALLWAIT  NTR1                                                                   
         L     R3,=F'600'          LOOP COUNT                                   
*                                                                               
AWAIT1   L     R6,VTCB             CHECK SE NOT ACTIVE IN ANOTHER TASK          
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING TCBD,R6                                                          
*                                                                               
AWAIT2   CLI   TCBSEN,1            IGNORE SERVICE                               
         JE    AWAIT7                                                           
         CLI   TCBSEN,0            IGNORE INACTIVE                              
         JE    AWAIT7                                                           
*                                                                               
* WAIT FOR 1/10 SEC. AFTER 60 SECONDS GIVE UP                                   
*                                                                               
AWAIT3   GOTO1 VTICTOC,DMCB,C'WAIT',F'3840'                                     
         JCT   R3,AWAIT1                                                        
*                                                                               
         BRAS  RE,CLEANUP          FUCK THIS I'VE HAD ENOUGH                    
         LA    R1,7                SYSTEM IS BUSY                               
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
*                                                                               
AWAIT7   JXLE  R6,R4,AWAIT2                                                     
*                                                                               
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* TURN ON/OFF ALL QUIESCE BITS FOR A SYSTEM                           *         
***********************************************************************         
QOFFALL  MVI   BYTE,0                                                           
         J     QALL                                                             
QONALL   MVI   BYTE,1                                                           
*                                                                               
QALL     NTR1                                                                   
         L     R6,ASENTRY          R6=A(SELIST ENTRY FOR SYSTEM)                
         USING SELISTD,R6                                                       
         L     R4,SEFILES                                                       
         L     R4,0(R4)            R4=A(SYSFLES)                                
         USING SYSFLSTD,R4                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R4,SYSFLIST         R4=A(FILE LIST ENTRY)                        
*                                                                               
QALL010  SR    R6,R6               R6=A(DTF)                                    
         ICM   R6,7,SYSFADTF                                                    
         USING ISDTF,R6                                                         
         CLI   BYTE,1              TURN OFF ALL QUIESCE BITS                    
         JE    *+8                                                              
         NI    ISFOPEN,255-ISFOQUIE                                             
         CLI   BYTE,0              TURN ON ALL QUIESCE BITS                     
         JE    *+8                                                              
         OI    ISFOPEN,ISFOQUIE                                                 
*                                                                               
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         JCT   R0,QALL010                                                       
*                                                                               
QOFFX    J     XIT1                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* SET AND RESET TCB ENTRY                                             *         
***********************************************************************         
SETSYS   NTR1                      SET TCB ENTRY FOR SYSCHA                     
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R5,SSBTKADR                                                      
         USING TCBD,R5                                                          
         MVC   SAVETCB(8),TCBDTFS                                               
         MVC   SAVETCB+8(1),TCBSEN                                              
         MVC   TCBDTFS(8),SEFILES                                               
         MVC   TCBSEN,SESYS                                                     
         GOTO1 VDTFIOA,DMCB,(C'I',(R5))                                         
         DROP  R5                                                               
         J     XIT1                                                             
                                                                                
RELSYS   NTR1                      RESET TCB ENTRY                              
         L     R5,SSBTKADR                                                      
         USING TCBD,R5                                                          
         MVC   TCBDTFS(8),SAVETCB                                               
         MVC   TCBSEN,SAVETCB+8                                                 
         GOTO1 VDTFIOA,DMCB,(C'I',(R5))                                         
         J     XIT1                                                             
         DROP  R6                                                               
                                                                                
***********************************************************************         
* FIND ASENTRY FROM SYS DATA                                          *         
***********************************************************************         
FINDSYS  NTR1                                                                   
         L     R6,VSELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         SR    R1,R1                                                            
         ICM   R1,1,SYNAME         PREPARE FOR NAME SEARCH                      
         BCTR  R1,0                                                             
*                                                                               
         CLC   SYNAME+1(3),=C'CON' UNLESS IT'S CON                              
         JE    FINDS10                                                          
         LA    R1,5                FORCE 6 CHR MATCH ON SYSTEM                  
*                                                                               
FINDS10  OC    SYNUM,SYNUM                                                      
         JNZ   FINDS020                                                         
*                                                                               
         EXRL  R1,*+10             FIND SELECTED SYS                            
         J     *+10                                                             
         CLC   SENAME(0),SYNAME+1                                               
         JE    FINDSX              GOT IT                                       
         JNE   FINDS090                                                         
*                                                                               
FINDS020 CLC   SYNUM+1(1),SESYS    TEST NUMBER                                  
         JE    FINDSX                                                           
*                                                                               
FINDS090 JXLE  R6,R4,FINDS10                                                    
         J     XITNEQ                                                           
*                                                                               
FINDSX   ST    R6,ASENTRY          SAVE ASENTRY                                 
         MVC   SYNAME+1(7),SENAME                                               
         MVI   SYNAME,7                                                         
         MVI   SYNUM,0                                                          
         MVC   SYNUM+1(1),SESYS                                                 
         MVC   APGMS,SEPGMS        SAVE APGMS                                   
*                                                                               
         SAC   512                 CONFIRM DEFINED IN DATASPACE                 
         LAM   AR2,AR2,SSBALET                                                  
         LLC   R2,SESYS                                                         
         SLL   R2,6                                                             
         CLC   SYNAME+1(7),16(R2)  CHECK SYSTEM NAME ON RESOURCE HEADER         
         SAC   0                                                                
         JE    XITEQU                                                           
         J     XITNEQ                                                           
         EJECT                                                                  
***********************************************************************         
* FIND FILE FROM FILENAME ALONE                                       *         
***********************************************************************         
FSYSFIL  NTR1                                                                   
         ICM   R6,15,ASENTRY       IS SYSTEM ALREADY FOUND                      
         JNZ   FSYSF01                                                          
         L     R6,VSELIST          START AT BEGINING OF SELIST                  
         USING SELISTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
FSYSF01  ICM   R3,15,SEFILES       GET SEFILES LIST                             
         JZ    FSYSF090                                                         
         L     R3,0(R3)            R3=A(SYSFLES)                                
         USING SYSFLSTD,R3                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R3,SYSFLIST         R3=A(FILE LIST ENTRY)                        
*                                                                               
FSYSF02  SR    R1,R1               R1=A(DTF)                                    
         ICM   R1,7,SYSFADTF                                                    
         USING DTFPHD,R1                                                        
         CLC   FILNAM,DTFDD                                                     
         JNE   FSYSF03                                                          
*                                                                               
         SR    RE,RE               CHECK THIS IS THE NATIVE SYSTEM              
         ICM   RE,1,DTFXNUM                                                     
         JNZ   *+8                                                              
         IC    RE,SYSFILE#                                                      
         SRL   RE,4                                                             
         CLM   RE,1,=X'0A'         IF CONTROL FILE                              
         JNE   FSYSF10                                                          
         CLM   RE,1,SEOVSYS        AND NOT SEOV CONTROL                         
         JE    FSYSF10                                                          
         CLM   RE,1,=X'0F'         SERVICE FILES ARE OK                         
         JE    FSYSF10                                                          
*                                                                               
FSYSF03  LA    R3,SYSFLNQ(R3)      BUMP TO NEXT FILE                            
         JCT   R0,FSYSF02                                                       
*                                                                               
FSYSF090 OC    ASENTRY,ASENTRY     ARE WE ON AN INDIVIDUAL SYSTEM               
         JNZ   *+8                                                              
         JXLE  R6,R4,FSYSF01       NEXT SYSTEM                                  
*                                                                               
         XC    ADTF,ADTF           NOT FOUND CLEAR DTF                          
         XC    FFLAGS,FFLAGS                                                    
         J     XITNEQ                                                           
*                                                                               
FSYSF10  ST    R6,ASENTRY          SAVE ASENTRY                                 
         MVC   SYNAME+1(7),SENAME                                               
         MVI   SYNAME,7                                                         
         MVI   SYNUM,0                                                          
         MVC   SYNUM+1(1),SESYS                                                 
         MVC   FILNUM,SYSFILE#                                                  
         MVC   FFLAGS,SYSFIND1     SET FLAGS AND DTF ADDR                       
         ST    R1,ADTF                                                          
         J     XITEQU                                                           
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* FIND ADTF FOR FILE IN FILNUM                                        *         
***********************************************************************         
FINDFIL  NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         L     R4,SEFILES                                                       
         L     R4,0(R4)            R4=A(SYSFLES)                                
         USING SYSFLSTD,R4                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R4,SYSFLIST         R4=A(FILE LIST ENTRY)                        
*                                                                               
FINDF01  CLC   FILNUM,SYSFILE#                                                  
         JE    FINDF02                                                          
*                                                                               
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         JCT   R0,FINDF01                                                       
         BRAS  RE,CLEANUP                                                       
         DC    H'0'                                                             
*                                                                               
FINDF02  MVC   FFLAGS,SYSFIND1     SAVE FILE FLAGS                              
         SR    R6,R6                                                            
         ICM   R6,7,SYSFADTF                                                    
         ST    R6,ADTF             SAVE A(DTF)                                  
         J     XIT1                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* FIND PGM ENTRYS AND DO ACTION                                       *         
***********************************************************************         
SETPGMS  NTR1                                                                   
         L     R6,APGMS                                                         
         USING PGMLSTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         MVC   OPSYSID,SYNAME+1                                                 
         MVC   OPFILID,SPACES                                                   
*                                                                               
SETPGM1  CLC   PGMNUM,DATA+2       MATCH PGM NUM                                
         JNE   SETPGM2                                                          
         CLC   OPFILID,SPACES                                                   
         JNE   *+10                                                             
         MVC   OPFILID(7),PGMNAME                                               
*                                                                               
         CLC   ACTNUM,=H'36'       STOP                                         
         JNE   *+14                                                             
         OI    PGMIND,PGMINOP                                                   
         MVC   OPCMND,=C'PGSTOP    '                                            
*                                                                               
         CLC   ACTNUM,=H'37'       START                                        
         JNE   *+14                                                             
         NI    PGMIND,255-PGMINOP                                               
         MVC   OPCMND,=C'PGSTART   '                                            
*                                                                               
         CLC   ACTNUM,=H'38'       RESTRICT                                     
         JNE   *+14                                                             
         OI    PGMIND,PGMIACC                                                   
         MVC   OPCMND,=C'PGREST    '                                            
*                                                                               
         CLC   ACTNUM,=H'39'       UNRESTRICT                                   
         JNE   *+14                                                             
         NI    PGMIND,255-PGMIACC                                               
         MVC   OPCMND,=C'PGUNRES   '                                            
*                                                                               
SETPGM2  JXLE  R6,R4,SETPGM1       NEXT PROGRAM                                 
         J     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* FIND PGM NAME FROM NUMBER                                           *         
***********************************************************************         
GETPGMS  NTR1                                                                   
         L     R6,APGMS                                                         
         USING PGMLSTD,R6                                                       
         LH    R4,0(R6)            SET BXLE                                     
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
*                                                                               
GETPGM1  CLC   PGMNUM,FILNUM       MATCH PGM NUM SAVED IN FILNUM                
         JNE   GETPGM2                                                          
         MVC   WORK(8),PGMNAME                                                  
         J     XIT1                                                             
*                                                                               
GETPGM2  JXLE  R6,R4,GETPGM1       NEXT PROGRAM                                 
         J     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* QUIESCE A FILE - SET QUIESCE BIT ON FOR READ-ONLY                   *         
***********************************************************************         
QUION    MVI   BYTE,0                                                           
         J     QUIES                                                            
QUIOFF   MVI   BYTE,1                                                           
*                                                                               
QUIES    NTR1                                                                   
         L     R6,ADTF             R6=A(DTF)                                    
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN     IGNORE IF CLOSED                             
         JZ    QUIX                                                             
*                                                                               
         BRAS  RE,SYSWAIT          WAIT FOR SYSTEM                              
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         CLI   BYTE,0              IF BYTE=0 QUIESCE                            
         JNE   QUIES010                                                         
         OI    ISFOPEN,ISFOQUIE                                                 
         MVC   OPCMND,QUIESCED     QUIESCE                                      
         BRAS  RE,WTO                                                           
         J     QUIX                                                             
*                                                                               
QUIES010 NI    ISFOPEN,255-ISFOQUIE                                             
         MVC   OPCMND,UNQUI        UNQUIESCE                                    
         BRAS  RE,WTO                                                           
*                                                                               
QUIX     BRAS  RE,SECHECK                                                       
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* OPEN A FILE                                                         *         
***********************************************************************         
QOPEN    NTR1                                                                   
         L     R6,ADTF             QUIESCE OPEN A FILE                          
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN                                                  
         JNZ   XIT1                FILE ALREADY OPEN DUMMY                      
         L     R1,ASENTRY                                                       
         TM    9(R1),X'80'         SYSTEM MUST BE OPEN TOO                      
         JZ    XIT1                                                             
*                                                                               
*NOP*    BRAS  RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
         BRAS  RE,SETSYS                                                        
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         CLC   ACTNUM,=H'13'       TEST OPEN OR QOPEN                           
         JNE   *+14                                                             
         OI    ISFOPEN,ISFOQUIE                                                 
         MVC   OPCMND,QOPENED                                                   
         CLC   ACTNUM,=H'6'                                                     
         JNE   *+14                                                             
         NI    ISFOPEN,255-ISFOQUIE                                             
         MVC   OPCMND,OPENED                                                    
         BRAS  RE,SINGLE           SINGLE THREAD                                
         TM    ISFTYPE,ISFTIS                                                   
         JZ    QO1                                                              
         GOTO1 VDATAMGR,DMCB,ISDDS,8,IOAREA,0,(R6),0,0                          
         J     QOX                                                              
*                                                                               
QO1      GOTO1 VDATAMGR,DMCB,DADDS,DAOPEN,IOAREA,0,(R6),FULL,0                  
*                                                                               
         USING DTFPHD,R6                                                        
         MVC   FULL,=X'00010101'   16-BIT FORMAT HEADER RECORD DSKADR           
         TM    DTFTYPE,DTFTBIGF+DTFTBIG                                         
         JZ    QO2                                                              
         JO    QO1B                                                             
         TM    DTFTYPE,DTFTBIGF                                                 
         JO    QO1A                                                             
         MVC   FULL,=X'00004101'   18-BIT                                       
         J     QO2                                                              
QO1A     MVC   FULL,=X'00001101'   20-BIT                                       
         J     QO2                                                              
QO1B     MVC   FULL,=X'00000501'   22-BIT                                       
*                                                                               
QO2      TM    FFLAGS,SFHDR        IF NO HEADER GO READ FOR EOF                 
         JZ    QO3                                                              
*                                                                               
         GOTO1 (RF),(R1),DADDS,RDID,IOAREA,0,(R6),FULL,0                        
*                                                                               
         SR    R0,R0               R0=TRACKS USED AT LOAD TIME                  
         ICM   R0,3,IOAREA+92                                                   
         CLI   IOAREA+94,X'FF'                                                  
         JE    *+8                                                              
         ICM   R0,4,IOAREA+94                                                   
*                                                                               
         TM    DTFTYPE,DTFTBIGF+DTFTBIG                                         
         JZ    QO2C                                                             
         JO    QO2B                                                             
         TM    DTFTYPE,DTFTBIGF                                                 
         JO    QO2A                                                             
         SLL   R0,32-18            18-BIT                                       
         ST    R0,FULL                                                          
         J     QO3                                                              
QO2A     SLL   R0,32-20            20-BIT                                       
         ST    R0,FULL                                                          
         J     QO3                                                              
QO2B     SLL   R0,32-22            22-BIT                                       
         ST    R0,FULL                                                          
         J     QO3                                                              
QO2C     SLL   R0,32-16            16-BIT                                       
         ST    R0,FULL                                                          
*                                                                               
QO3      GOTO1 (RF),(R1),DADDS,ADDADR,IOAREA,0,(R6),FULL,0                      
QOX      BRAS  RE,RELSYS                                                        
         BRAS  RE,MULTI            OK TO MULTI TASK AGAIN                       
         BRAS  RE,WTO                                                           
         BRAS  RE,SECHECK                                                       
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* CLOSE A FILE                                                        *         
***********************************************************************         
QCLOSE   NTR1                                                                   
         L     R6,ADTF             QUIESCE CLOSE A FILE                         
         USING ISDTF,R6                                                         
         TM    ISFOPEN,ISFOOPN                                                  
         JZ    QCXX                FILE ALREADY CLOSED DUMMY                    
*                                                                               
*NOP*    BRAS  RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),ISFFID                                                
         OI    ISFOPEN,ISFOQUIE    LET THE WORLD KNOW I DID IT                  
         TM    ISFTYPE,ISFTIS                                                   
         JZ    QC1                                                              
*                                                                               
         GOTO1 VDATAMGR,DMCB,ISDDS,9,IOAREA,0,(R6),0,0                          
         J     QCX                                                              
*                                                                               
QC1      GOTO1 VDATAMGR,DMCB,DADDS,DACLOSE,IOAREA,0,(R6),X'00010101',0          
QCX      MVC   OPCMND,CLOSED                                                    
         BRAS  RE,WTO                                                           
         BRAS  RE,SECHECK                                                       
QCXX     J     XIT1                                                             
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
         L     R4,0(R4)            R4=A(SYSFLES)                                
         USING SYSFLSTD,R4                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R4,SYSFLIST         R4=A(FILE LIST ENTRY)                        
*                                                                               
SECHK01  SR    R1,R1               R1=A(DTF)                                    
         ICM   R1,7,SYSFADTF                                                    
         USING ISDTF,R1                                                         
         TM    ISFOPEN,ISFOQUIE                                                 
         JZ    *+12                                                             
         OI    SEIND,SEIQUIES                                                   
         J     XIT1                                                             
*                                                                               
         LA    R4,SYSFLNQ(R4)      BUMP TO NEXT FILE                            
         JCT   R0,SECHK01                                                       
         J     XIT1                                                             
         DROP  R1,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* QUIESCE OR UNQUIESCE A WHOLE SYSTEM                                 *         
***********************************************************************         
SQUIES   NTR1                                                                   
         L     R6,ASENTRY                                                       
         USING SELISTD,R6                                                       
         OC    SEFILES,SEFILES     ANY FILES                                    
         JZ    SQUOKX              NO - EXIT                                    
         BRAS  RE,SYSWAIT                                                       
*                                                                               
         L     R1,ASENTRY                                                       
         MVC   OPSYSID(7),0(R1)                                                 
         MVC   OPFILID(7),SPACES                                                
         MVC   OPCMND,STARTED                                                   
         TM    SEIND,SEIQUIES                                                   
         JZ    SQU010                                                           
*                                                                               
         BRAS  RE,QOFFALL                                                       
         NI    SEIND,255-SEIQUIES                                               
         MVC   OPCMND,UNQUI                                                     
         J     SQUOK                                                            
*                                                                               
SQU010   BRAS  RE,QONALL                                                        
         OI    SEIND,SEIQUIES                                                   
         MVC   OPCMND,QUIESCED                                                  
         J     SQUOK                                                            
*                                                                               
SQUOK    BRAS  RE,WTO                                                           
SQUOKX   J     XIT1                                                             
         DROP  R6                                                               
***********************************************************************         
* SET AND RESET SINGLE THREAD MODE                                    *         
***********************************************************************         
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
***********************************************************************         
* START/READONLY THE SYSTEM IN SENUMB                                 *         
* EXIT WITH CC=NEQ ON ERROR WITH MSGNUM SET                           *         
***********************************************************************         
ROSYS    NTR1                                                                   
         LARL  R9,ROSYS            LOCAL BASE                                   
         USING ROSYS,R9                                                         
         L     R2,ASENTRY                                                       
         USING SELISTD,R2                                                       
         OC    SEFILES,SEFILES     ANY FILES                                    
         JZ    ROOK                NO - EXIT                                    
*                                                                               
         BRAS  RE,SYSWAIT          WAIT FOR SYSTEM TO BE FREE                   
*                                                                               
RO4      NI    SEIND,255-SEISETRO                                               
         TM    SEIND,SEIRONLY      IF NOT DEFAULT TO READ ONLY                  
         JO    *+8                                                              
         OI    SEIND,SEISETRO      SET SE FILES TO SET READ-ONLY                
*                                                                               
         L     R3,SEFILES                                                       
         L     R3,0(R3)            R3=A(SYSFLES)                                
         USING SYSFLSTD,R3                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R3,SYSFLIST         R3=A(FILE LIST ENTRY)                        
         MVC   BYTE,SYSFILE#       EXT NUMBER OF 1ST FILE                       
         NI    BYTE,X'F0'                                                       
*                                                                               
RO4A     MVC   FULL(1),SYSFILE#    EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'                                                       
         CLC   FULL(1),BYTE        ARE THEY FROM SAME SYSTEM                    
         JNE   RO4B                                                             
         SR    R1,R1                                                            
         ICM   R1,7,SYSFADTF                                                    
         OI    DTFOPEN-DTF(R1),DTF_RO   SET READ-ONLY IN DTF                    
*                                                                               
RO4B     LA    R3,SYSFLNQ(R3)      BUMP TO NEXT FILE                            
         JCT   R0,RO4A                                                          
*                                                                               
         GOTO1 VTICTOC,DUB,C'SSET'                                              
         L     RE,ASENTRY                                                       
         MVC   OPSYSID(L'SENAME),SENAME-SELISTD(RE)                             
         MVC   OPCMND(7),=C'RD-ONLY'                                            
         BRAS  RE,WTO                                                           
*                                                                               
         TM    SEIND,SEISTRT       IS SYSTEM UP AND RUNNING                     
         JNZ   RODEQ                                                            
         GOTOR UPSYS,=C'READONLY'  NO SO START IT                               
         J     ROOK                                                             
*                                                                               
RODEQ    LLC   RE,SESYS                                                         
         CVD   RE,DUB              NO BUILD ENQUEUE ID                          
         UNPK  DUB(4),DUB+6(2)                                                  
         OI    DUB+3,X'F0'                                                      
         MVC   UPDMIN+4(4),DUB                                                  
         XC    DUB(12),DUB                                                      
         LA    R2,UPDMAJ                                                        
         LA    RE,UPDMIN                                                        
         DEQ   ((R2),(RE),8,SYSTEM),RET=HAVE                                    
*&&UK                                                                           
         L     R2,ASENTRY                                                       
         XC    DUB,DUB             SET READ ONLY IN DSPACE                      
         MVC   DUB+3(1),SESYS                                                   
         MVI   DUB+1,9                                                          
         GOTO1 VLOCKSPC,DUB                                                     
*&&                                                                             
ROOK     J     XIT1                                                             
         DROP  R2,R3,R9                                                         
         EJECT                                                                  
***********************************************************************         
* SET SYSTEM TO UPDATIVE                                              *         
***********************************************************************         
RWSYS    NTR1                                                                   
         L     R5,ASENTRY                                                       
         USING SELISTD,R5                                                       
         OC    SEFILES,SEFILES     ANY FILES THIS SE                            
         JZ    RWX                 NO - EXIT                                    
         TM    SEIND,SEISETRO      IS IT SET READ-ONLY                          
         JNO   RWX                                                              
*&&UK                                                                           
         XC    DUB,DUB             SET US TO UPDATIVE IN DSPACE                 
         MVC   DUB+3(1),SESYS                                                   
         MVI   DUB+1,8                                                          
         GOTO1 VLOCKSPC,DUB                                                     
         CLI   DUB+4,X'80'         TEST FOR UNAVAILABLE                         
         JO    RWX                                                              
*&&                                                                             
UPENABLE NI    SEIND,255-SEISETRO                                               
         L     R3,SEFILES                                                       
         L     R3,0(R3)            R3=A(SYSFLES)                                
         USING SYSFLSTD,R3                                                      
         LH    R0,SYSF#FLS         R0=NUMBER OF FILES                           
         LA    R3,SYSFLIST         R3=A(FILE LIST ENTRY)                        
         MVC   BYTE,SYSFILE#       EXT NUMBER OF 1ST FILE                       
         NI    BYTE,X'F0'                                                       
*                                                                               
UPEN1    MVC   FULL(1),SYSFILE#    EXT NUMBER OF THIS FILE                      
         NI    FULL,X'F0'                                                       
         CLC   FULL(1),BYTE        ARE THEY FROM SAME SYSTEM                    
         JNE   UPEN2                                                            
         SR    R1,R1                                                            
         ICM   R1,7,SYSFADTF                                                    
         NI    DTFOPEN-DTF(R1),255-X'80'  CLEAR READ-ONLY IN DTF                
*                                                                               
UPEN2    LA    R3,SYSFLNQ(R3)      BUMP TO NEXT FILE                            
         JCT   R0,UPEN1                                                         
         J     RWOK                                                             
*                                                                               
RWOK     L     RE,ASENTRY                                                       
         MVC   OPSYSID(L'SENAME),SENAME-SELISTD(RE)                             
         MVC   OPCMND(7),=C'WRITE  '                                            
         BRAS  RE,WTO                                                           
*                                                                               
RWX      J     XIT1                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* SEND DIRECT BROADCAST TO TERMINALS                                  *         
***********************************************************************         
BCAST    NTR1                                                                   
         SAM31                                                                  
         L     R6,VUTL                                                          
         LH    R4,0(R6)            SET BXLE REGS                                
         L     R5,2(R6)                                                         
         LA    R6,6(R6)                                                         
         USING UTLD,R6                                                          
*                                                                               
BC010    OC    SYNUM,SYNUM         IS THIS FOR ALL SYSTEMS                      
         JZ    *+14                                                             
         CLC   SYNUM+1(1),TSYS     ARE THEY CONNECTED TO THIS SYSTEM            
         JNE   BC050                                                            
         J     BC050               *NOP* BROADCAST                              
         MVC   TBRSYS,BRDNUM       SEND THEM THE BROADCAST                      
         OI    TSTAT2,TSTATBCP                                                  
*                                                                               
BC050    JXLE  R6,R4,BC010         BXLE TO NEXT TERMINAL                        
*                                                                               
BCX      SAM24                                                                  
         J     XIT1                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* PATCH SOMETHING                                                     *         
***********************************************************************         
PATCH    NTR1                                                                   
         GOTO1 VCALLOV,DMCB,IOAREA,X'D9000A00'                                  
         L     R5,0(R1)            A(CORE AREA)                                 
         L     R6,SSBCRADR                                                      
         LR    R7,R5                                                            
         A     R7,SSBCRLEN                                                      
*                                                                               
         LAM   AR2,AR2,SSBPGMTA                                                 
         LAM   AR3,AR3,SSBPGMTA                                                 
         LAM   AR4,AR4,SSBPGMTA                                                 
         SAC   512                                                              
         SAM31                                                                  
*                                                                               
         L     R2,DATA             A PATCH AREA                                 
         USING PATCHWKD,R2                                                      
         LR    R3,R2                                                            
         AHI   R3,-16                                                           
         CLC   0(16,R3),=C'***PATCH AREA***'                                    
         JNE   PATCHX                                                           
*                                                                               
         L     R3,PATCHWRK         A PATCH WORK AREA                            
         L     R4,PATCHPHS         A PHASE                                      
*                                                                               
PATCH010 CLC   0(64,R5),0(R4)      FIND PHASE IN CORE                           
         JE    PATCH020                                                         
PATCH011 LA    R5,1(R5)                                                         
         CR    R5,R6                                                            
         JL    PATCH010                                                         
         J     PATCHX                                                           
*                                                                               
PATCH020 STM   R4,R5,DUB           SAVE IN DUB                                  
         A     R5,PATCHOFS         POINT TO LEVEL INFO                          
         A     R4,PATCHOFS                                                      
         CLC   0(40,R5),0(R4)      MUST MATCH TOO                               
         JE    PATCH030                                                         
         CLC   0(6,R5),=C'LEVEL='                                               
         JNE   PATCH025                                                         
         CLC   0(23,R5),0(R4)      LEVEL CAN MATCH ON 23                        
         JE    PATCH030                                                         
PATCH025 LM    R4,R5,DUB                                                        
         J     PATCH011                                                         
*                                                                               
PATCH030 LM    R4,R5,DUB                                                        
*                                                                               
         LA    R1,32               CHECK INDIVIDUAL PATCH ENTIRES               
PATCH040 LH    RF,0(R1,R2)                                                      
         LTR   RF,RF                                                            
         JZ    PATCH050                                                         
         LM    R4,R5,DUB                                                        
         AR    R5,RF                                                            
         AR    R4,RF                                                            
         LH    RF,2(R1,R2)                                                      
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R5),0(R4)                                                    
         JNE   PATCH025                                                         
*                                                                               
         LA    R1,4(R1)                                                         
         CHI   R1,532                                                           
         JL    PATCH040                                                         
*                                                                               
PATCH050 LA    R0,128                                                           
         LA    R2,PATCHSYS                                                      
         MVC   SCANBLK,0(R2)                                                    
PATCH055 XR    RF,RF                                                            
         MVC   FULL,SSBSYSNA       SET UP NAME IN FULL                          
         LLC   R1,SSBSYSIX         AORS TOO                                     
         SRL   R1,4                                                             
         LTR   R1,R1                                                            
         JZ    *+8                                                              
         LA    R1,X'C0'(R1)                                                     
         STC   R1,FULL+3                                                        
         L     R1,FULL                                                          
*                                                                               
         CS    RF,R1,0(R2)         COMPARE AND SWAP IN                          
         JE    PATCH059                                                         
         AHI   R2,16               NEXT ENTRY                                   
         JCT   R0,PATCH055                                                      
         LA    R1,SCANBLK                                                       
         DC    H'0'                SYSTEM RETURN AREA FULL                      
*                                                                               
PATCH059 LM    R4,R5,DUB                                                        
         ST    R5,4(,R2)                                                        
         L     R2,DATA                                                          
         J     PATCH025            A PATCH AREA                                 
*                                                                               
PATCHX   LAM   AR2,AR4,ZEROS                                                    
         SAC   0                                                                
         SAM24                                                                  
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* APPLY PATCH                                                         *         
***********************************************************************         
PATCHIT  NTR1                                                                   
         LAM   AR2,AR2,SSBPGMTA                                                 
         LAM   AR3,AR3,SSBPGMTA                                                 
         LAM   AR4,AR4,SSBPGMTA                                                 
         SAC   512                                                              
         SAM31                                                                  
*                                                                               
         L     R2,DATA             A PATCH AREA                                 
         USING PATCHWKD,R2                                                      
         LR    R3,R2                                                            
         AHI   R3,-16                                                           
         CLC   0(16,R3),=C'***PATCH AREA***'                                    
         JNE   PATCHITX                                                         
*                                                                               
PACH010  LA    R0,128              UP TO 128 SYSTEM ENTRIES                     
         LA    R2,PATCHSYS                                                      
*                                                                               
PACH015  XR    RF,RF                                                            
         MVC   FULL,SSBSYSNA       SET UP NAME IN FULL                          
         LLC   R1,SSBSYSIX         AORS TOO                                     
         SRL   R1,4                                                             
         LTR   R1,R1                                                            
         JZ    *+8                                                              
         LA    R1,X'C0'(R1)                                                     
         STC   R1,FULL+3                                                        
*                                                                               
         TM    12(R2),X'80'        PATCH REQUIRED                               
         JZ    PATCH016                                                         
         TM    12(R2),X'60'        PATCH MADE OR REJECTED                       
         JNZ   PATCH016                                                         
*                                                                               
         L     R1,FULL                                                          
         ST    R2,FULL             SAVE A(ENTRY)                                
         C     R1,0(,R2)           IS THIS MY SYSTEM                            
         JE    PACH019                                                          
*                                                                               
PATCH016 AHI   R2,16               NEXT ENTRY                                   
         JCT   R0,PACH015                                                       
         J     PATCHITX                                                         
*                                                                               
PACH019  L     R5,4(,R2)           LOAD R5 WITH A(PHASE)                        
*                                                                               
         L     R2,DATA             A PATCH AREA                                 
         USING PATCHWKD,R2                                                      
         L     R3,PATCHWRK         A PATCH WORK AREA                            
         L     R4,PATCHPHS         A PHASE                                      
         CLC   0(64,R5),0(R4)      FIND PHASE IN CORE                           
         JE    PACH020                                                          
         JNE   PACHNO                                                           
*                                                                               
PACH020  STM   R4,R5,DUB           SAVE IN DUB                                  
         A     R5,PATCHOFS         POINT TO LEVEL INFO                          
         A     R4,PATCHOFS                                                      
         CLC   0(40,R5),0(R4)      MUST MATCH TOO                               
         JE    PACH030                                                          
         CLC   0(6,R5),=C'LEVEL='                                               
         JNE   PACHNO                                                           
         CLC   0(23,R5),0(R4)      LEVEL CAN MATCH ON 23                        
         JE    PACH030                                                          
         JNE   PACHNO                                                           
*                                                                               
PACH030  LM    R4,R5,DUB                                                        
         LA    R1,32               CHECK INDIVIDUAL PATCH ENTIRES               
PACH040  LH    RF,0(R1,R2)                                                      
         LTR   RF,RF                                                            
         JZ    PACH050                                                          
         LM    R4,R5,DUB                                                        
         L     R3,PATCHWRK         A PATCH WORK AREA                            
         AR    R5,RF                                                            
         AR    R4,RF                                                            
         AR    R3,RF                                                            
         LH    RF,2(R1,R2)                                                      
         BCTR  RF,0                                                             
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         CLC   0(0,R5),0(R4)       TEST DATA TO BE PATCHED                      
         JNE   PACHNO                                                           
         EXRL  RF,*+10                                                          
         J     *+10                                                             
         MVC   0(0,R5),0(R3)       APPLY PATCH                                  
*                                                                               
         LA    R1,4(R1)                                                         
         CHI   R1,532                                                           
         JL    PACH040                                                          
*                                                                               
PACH050  L     R2,FULL                                                          
         OI    12(R2),X'40'        PATCHED OK                                   
         J     PACH010                                                          
*                                                                               
PACHNO   L     R2,FULL                                                          
         OI    12(R2),X'20'        PATCH REJECTED                               
         J     PACH010                                                          
*                                                                               
PATCHITX LAM   AR2,AR4,ZEROS                                                    
         SAC   0                                                                
         SAM24                                                                  
         J     XIT1                                                             
***********************************************************************         
* VALIDATE OPERATOR INPUT                                             *         
***********************************************************************         
VALCARD  NTR1                                                                   
         XC    ACTNUM,ACTNUM       CLEAR RETURNED VALUES                        
         XC    ADNAME,ADNAME                                                    
         XC    ADNUM,ADNUM                                                      
         XC    JOBNAME,JOBNAME                                                  
         XC    SYNAME,SYNAME                                                    
         XC    SYNUM,SYNUM                                                      
         XC    ASENTRY,ASENTRY                                                  
         XC    FFLAGS,FFLAGS                                                    
         XC    FILNUM,FILNUM                                                    
         XC    FILNAM,FILNAM                                                    
         XC    ADTF,ADTF                                                        
*                                                                               
         GOTO1 ASCANNER,DMCB,(C'C',CARD),(8,SCANBLK),0                          
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)          TEST FOR NO INPUT                            
         JZ    VALCARDX                                                         
*                                                                               
VALC010  LA    R2,SCANBLK          POINT TO SCANNER BLOCK                       
         SR    R1,R1                                                            
         ICM   R1,1,0(R2)          ERROR IF NOT INPUT                           
         JZ    VALC022                                                          
*                                                                               
         L     RF,ACOMTAB          SEARCH COMTAB                                
VALC020  CLM   R1,1,10(RF)                                                      
         JL    VALC021             REJECT IF < MIN LENGTH                       
         BCTR  R1,0                                                             
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   0(0,RF),12(R2)      COMPARE COMMAND TEXT                         
         JE    VALC023                                                          
         LA    R1,1(R1)                                                         
VALC021  LA    RF,16(RF)                                                        
         CLI   8(RF),X'FF'         TEST FOR EOT                                 
         JNE   VALC020                                                          
*                                                                               
VALC022  MVC   COMMAND,12(R2)      SAVE COMMAND TEXT                            
         LTR   RB,RB               RETURN CC NEQ                                
         J     XIT1                                                             
*                                                                               
VALC023  MVC   ACTNUM,8(RF)        SAVE ACTION NUMBER                           
         MVC   FLAG,11(RF)                                                      
         J     VALC100                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADV NAME                                                   *         
***********************************************************************         
VALC100  LA    R2,32(R2)           NEXT SCAN FIELD (ADV NAME)                   
         JCT   R3,*+8                                                           
         J     VALCARDX                                                         
*                                                                               
         CLI   0(R2),0             NO ADV NAME IS OK                            
         JE    VALC200             DISABLE SPECIFICS                            
         CLI   0(R2),3             4 IS ADVN                                    
         JNL   *+12                                                             
         CLI   0(R2),1             1 IS ADVCHR                                  
         JNE   VALC190                                                          
*                                                                               
         L     R1,SSBAFID          GET FACID TAB                                
VALC110  CLI   0(R2),1                                                          
         JNE   *+14                                                             
         CLC   12(1,R2),7(R1)      1 CHR TEST                                   
         J     *+10                                                             
         CLC   12(4,R2),0(R1)      4 CHR TEST                                   
         JE    VALC120                                                          
         LA    R1,8(R1)                                                         
         CLI   0(R1),X'FF'         TEST EOT                                     
         JNE   VALC110                                                          
         J     VALC180                                                          
*                                                                               
VALC120  MVC   ADNUM,4(R1)         SAVE ADV NUM & NAME                          
         MVC   ADNAME,0(R1)                                                     
         J     VALC200                                                          
*                                                                               
VALC180  MVC   JOBNAME,12(R2)      MUST BE JOBNAME THEN                         
         MVC   JOBNAMEL,0(R2)                                                   
         J     VALC200                                                          
*                                                                               
VALC190  LA    R1,4                UNKNOWN ADV SYSTEM                           
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE SE SYSTEM NAME OR RESOURCE                                 *         
***********************************************************************         
VALC200  TM    FLAG,X'20'          TEST RESOURCE REQUIRED                       
         JO    VALC250                                                          
         TM    FLAG,FACOSYPG       TEST SYS/PGM REQUIRED                        
         JO    VALC600                                                          
VALC210  LA    R2,32(R2)           NEXT SCAN FIELD (SE SYS)                     
         JCT   R3,*+8                                                           
         J     VALCARDX                                                         
*                                                                               
         CLC   12(3,R2),=C'ALL'    ALLOW ALL                                    
         JNE   VALC220                                                          
         XC    SYNUM,SYNUM                                                      
         J     VALC300                                                          
*                                                                               
VALC220  XC    SYGROUP,SYGROUP                                                  
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         JZ    VALC300             NO SYSTEM IS OK FOR NOW                      
         CLI   0(R2),4                                                          
         JL    VALC240             BUT < 4 CHRS FOR COMPARE IS NOT              
         STC   R1,SYNAME                                                        
         MVC   SYNAME+1(7),12(R2)                                               
         BRAS  RE,FINDSYS                                                       
         JE    VALC300                                                          
         JNE   VALC240             SEND TO ERROR                                
*                                                                               
VALC240  LA    R1,5                UNKNOWN SYSTEM                               
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE RESOURCE NAME                                              *         
***********************************************************************         
VALC250  LA    R2,32(R2)           NEXT SCAN FIELD (SE SYS)                     
         JCT   R3,*+8                                                           
         J     VALCARDX                                                         
*                                                                               
         CLC   ACTNUM,=H'33'                                                    
         JNE   VALC051                                                          
*                                                                               
         XC    RESCL16,RESCL16                                                  
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         JZ    VALC270             NO RESOURCE NAMED                            
         CHI   R1,16                                                            
         JH    VALC270             TOO LONG                                     
         BCTR  R1,0                                                             
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         MVC   RESCL16(0),12(R2)   COPY UP TO 16 CHRS                           
         J     VALC300                                                          
*                                                                               
VALC051  XC    RESOURCE,RESOURCE                                                
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         JZ    VALC270             NO RESOURCE NAMED                            
         CLI   0(R2),4                                                          
         JL    VALC270             < 4 CHRS FOR COMPARE                         
         BCTR  R1,0                                                             
*                                                                               
         LARL  RF,RESRCTBL         FIND RESOURCE IN TABLE                       
VALC052  CLI   0(RF),X'FF'                                                      
         JE    VALC270             EOT - RESOURCE NOT FOUND                     
*                                                                               
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   0(0,RF),12(R2)      CHECK RESOURCE NAME                          
         JE    *+12                                                             
         LA    RF,10(RF)           NEXT TABLE ENTRY                             
         J     VALC052                                                          
*                                                                               
         MVC   RESOURCE,8(RF)      SAVE RESOURCE NUMBER                         
         J     VALC300                                                          
*                                                                               
VALC270  LA    R1,10               UNKNOWN RESOURCE                             
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILE NAME                                                  *         
***********************************************************************         
VALC300  CLC   ACTNUM,=H'12'       NEXT SCAN FIELD (FILENAME)                   
         JE    VALC400                                                          
         LA    R2,32(R2)           NEXT SCAN FIELD (FILENAME)                   
         JCT   R3,*+8                                                           
         J     VALCARDX                                                         
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         JZ    VALCARDX            NO FILENAME OK FOR NOW                       
         CLI   0(R2),4                                                          
         JL    VALC390             BUT < 4 CHRS FOR COMPARE IS NOT              
*                                                                               
         MVC   FILNAM,12(R2)                                                    
         BRAS  RE,FSYSFIL          VALIDATE FILENAME                            
         JE    VALCARDX                                                         
*                                                                               
VALC390  LA    R1,6                UNKNOWN FILENAME                             
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
*                                                                               
VALCARDX CR    R1,R1               EXIT EQU                                     
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE BROADCAST                                                  *         
***********************************************************************         
VALC400  LA    R2,32(R2)           NEXT SCAN FIELD (FILENAME)                   
         JCT   R3,*+8                                                           
         J     VALC490                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,0(R2)                                                       
         JZ    VALC490             NO BROADCAST                                 
         LARL  RF,BRDTABL                                                       
         BCTR  R1,0                                                             
*                                                                               
VALC410  EXRL  R1,*+10             SCAN THE TABLE FOR BROADCAST EQU             
         J     *+10                                                             
         CLC   0(0,RF),12(R2)                                                   
         JE    VALC420                                                          
         LA    RF,8(RF)                                                         
         CLI   7(RF),X'FF'                                                      
         JNE   VALC410                                                          
         J     VALC490                                                          
*                                                                               
VALC420  MVC   BRDNUM,7(RF)        SAVE THE NUMBER                              
         J     VALCARDX                                                         
*                                                                               
VALC490  LA    R1,8                UNKNOWN BROADCAST                            
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
         EJECT                                                                  
************************************************************                    
*        VALIDATE SYS PRG                                  *                    
************************************************************                    
         USING SCANBLKD,R2                                                      
VALC600  LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD STATE                        
         JCT   R3,*+8                                                           
         J     VALC690                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         JZ    VALC690                                                          
         BCTR  R1,0                                                             
*                                                                               
         L     R6,ASYSLST          R2=SYSLST                                    
         LH    RE,0(R6)                                                         
         L     RF,2(R6)                                                         
         AHI   R6,6                                                             
         USING SYSLSTD,R6                                                       
*                                                                               
VALC605  EXRL  R1,*+10             TEST SHORT NAME                              
         J     *+10                                                             
         CLC   SYSLSHRT(0),SC1STFLD                                             
         JE    VALC620                                                          
         JXLE  R6,RE,VALC605                                                    
         J     VALC690                                                          
*                                                                               
VALC620  MVC   SYNUM+1(1),SYSLNUM  SAVE THE SYS NUMBER                          
*                                                                               
         L     R6,VSELIST                                                       
         LH    RE,0(R6)                                                         
         L     RF,2(R6)                                                         
         AHI   R6,6                                                             
         USING SELISTD,R6                                                       
*                                                                               
VALC625  CLC   SYNUM+1(1),SEOVSYS  TEST SYNUM WITH OVSYS                        
         JE    VALC630                                                          
*                                                                               
         JXLE  R6,RE,VALC625       NEXT                                         
         J     VALC690                                                          
*                                                                               
VALC630  MVC   APGMS,SEPGMS        SAVE A(PGMS)                                 
         ICM   R6,15,APGMS                                                      
         JZ    VALC690                                                          
*                                                                               
         LA    R2,SCBLKLQ(,R2)     NEXT SCAN FIELD MUST BE PGM                  
         JCT   R3,*+8                                                           
         J     VALC690                                                          
*                                                                               
         SR    R1,R1               GET LEN IN R1                                
         ICM   R1,1,SC1STLEN                                                    
         JZ    VALC690                                                          
         BCTR  R1,0                                                             
*                                                                               
         LH    RE,0(R6)            SET BXLE                                     
         L     RF,2(R6)                                                         
         AHI   R6,6                                                             
         USING PGMLSTD,R6                                                       
*                                                                               
         EXRL  R1,*+10                                                          
         J     *+10                                                             
         CLC   PGMNAME(0),SC1STFLD PROG NAME                                    
         JE    VALC640                                                          
         JXLE  R6,RE,*-10          NEXT                                         
         J     VALC690                                                          
*                                                                               
VALC640  MVC   FILNUM,PGMNUM       SAVE PGMNUM IN FILENUM SLOT                  
         J     VALCARDX                                                         
*                                                                               
VALC690  LA    R1,24               UNKNOWN SYS PRG                              
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
         EJECT                                                                  
***********************************************************************         
* ERRORS                                                              *         
***********************************************************************         
ERR1     MVC   OPFILID,=CL8'IS BUSY'                                            
         MVC   OPCMND,=CL8'NO EOJ  '                                            
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
*                                                                               
ERRX     BRAS  RE,CLEANUP                                                       
         LA    R1,1                UNKNOWN COMMAND                              
         BRAS  RE,GETMSG                                                        
         BRAS  RE,WTO                                                           
         J     XMOD1                                                            
         EJECT                                                                  
***********************************************************************         
* WRITE TO OPERATOR                                                   *         
***********************************************************************         
WTO      NTR1                                                                   
         CLI   ONLINE,C'Y'         IF NOT =OPER THEN SEND TO CONSOLE            
         JNE   WTOCONS                                                          
*                                                                               
         L     R5,SRPAR6           =OPER SO SEND CONSOLE MESSAGES               
         USING SROPSFFD,R5         TO LINE 1 OF THE SCREEN                      
         MVC   SRVMSG,OPMSG                                                     
         OI    SRVMSGH+6,X'80'                                                  
         J     XIT1                                                             
         DROP  R5                                                               
*                                                                               
WTOCONS  OC    OPMSG,SPACES                                                     
         GOTO1 VTICTOC,DUB,C'SSET' SUSPEND TIMERS                               
         GOTO1 VDMOD000,DMCB,VWCTYPE,OPMSG,OPMSGL,C'LVL1'                       
         GOTO1 VTICTOC,DUB,C'RSET' RESET TIMERS                                 
         MVC   OPMSGTXT,SPACES                                                  
         J     XIT1                                                             
         EJECT                                                                  
***********************************************************************         
* OPERATOR MESSAGES                                                   *         
***********************************************************************         
GETMSG   NTR1                                                                   
         BCTR  R1,0                                                             
         SLL   R1,5                                                             
         LARL  RF,OPER1                                                         
         AR    R1,RF                                                            
         MVC   OPMSGOPS,0(R1)                                                   
         J     XIT1                                                             
*                                                                               
OPER1    DC    CL32'UNKNOWN COMMAND'                                            
OPER2    DC    CL32'EOJ COMMAND ACCEPTED'                                       
OPER3    DC    CL32'FORCED END OF JOB   '                                       
OPER4    DC    CL32'UNKNOWN ADV SYSTEM  '                                       
OPER5    DC    CL32'UNKNOWN SE SYSTEM   '                                       
OPER6    DC    CL32'UNKNOWN FILENAME    '                                       
OPER7    DC    CL32'SYSTEM IS BUSY - RETRY '                                    
OPER8    DC    CL32'UNKNOWN BROADCAST      '                                    
OPER9    DC    CL32'MISSING DATA           '                                    
OPER10   DC    CL32'UNKNOWN / MISSING RESOURCE '                                
OPER11   DC    CL32'UNABLE TO ACCESS MEDDSPACE '                                
OPER12   DC    CL32'MED DSPACE ENABLED  '                                       
OPER13   DC    CL32'MED DSPACE DISABLED'                                        
OPER14   DC    CL32'XX COMMANDS OUTSTANDING'                                    
OPER15   DC    CL32'COMMAND          COMPLETED'                                 
OPER16   DC    CL32'STARTTTS REQUESTED       '                                  
OPER17   DC    CL32'NOT AUTHORIZED ON THIS ADV'                                 
OPER18   DC    CL32'SSB STOP - USER INPUT INHIBITED'                            
OPER19   DC    CL32'SSB GO - USER INPUT ENABLED'                                
*PER20   DC    CL32'RESET SIN AND DATE TO DD/MM/YY'                             
OPER20   DC    CL32'SIN/DATE RESET NOT SUPPORTED'                               
OPER21   DC    CL32'NEWDAY          SET TO DDMMM'                               
OPER22   DC    CL32'DATE HAS BEEN SET TO TODAY'                                 
OPER23   DC    CL32'REBUILD COMMAND ISSUED    '                                 
OPER24   DC    CL32'UNKNOWN SYS/PRG           '                                 
         EJECT                                                                  
***********************************************************************         
* DATA AREAS                                                          *         
***********************************************************************         
$$DATA   LOCTR ,                                                                
*                                                                               
OPMSG    DS    0CL80  '+FACPAK+ SSSSSSSS FFFFFFFF CCCCCCCCCC                    
OPFACID  DS    CL8                                                              
         DS    CL1                                                              
OPMSGOPS DS    0CL31                                                            
OPMSGTXT DS    0CL70                                                            
OPSYSID  DS    CL8                                                              
         DS    CL1                                                              
OPFILID  DS    CL8                                                              
         DS    CL1                                                              
OPCMND   DS    CL10                                                             
         DS    CL3                                                              
OPEXTRA  DS    CL39                                                             
         ORG   OPMSG                                                            
OPTSKID  DS    CL2                 T#                                           
         DS    CL1                                                              
OPTSKWT  DS    CL2                 T# - WAITING ON TASK                         
         DS    CL1                                                              
OPTSKLU  DS    CL8                                                              
         DS    CL1                                                              
OPTSKSP  DS    CL10                                                             
         ORG                                                                    
OPMSGL   EQU   *-OPMSG                                                          
         EJECT                                                                  
***********************************************************************         
* CONSTANTS AND LITERALS                                              *         
***********************************************************************         
         CNOP  0,8                                                              
FECNO    DC    C'CANNOT FIX ECBLST (TOO MANY ENTRIES) - RETRY FIX'              
FECFIX   DC    C'ONE BAD ECB POSTED'                                            
MYUPDID  DS    0CL16                                                            
UPDMAJ   DC    C'DMGR    '                                                      
UPDMIN   DC    C'SNUM0000'                                                      
*                                                                               
PGMLIST  DC    (PGMLSTX-PGMLSTD)X'00'                                           
         ORG   PGMLIST                                                          
MYPGMLST DC    CL7'SROPS00',X'14',X'1F',X'00',AL1(000),9X'00'                   
         ORG                                                                    
*                                                                               
QCLOSED  DC    CL8'QCLOSED'                                                     
OPENED   DC    CL10'OPENED'                                                     
CLOSED   DC    CL10'CLOSED'                                                     
QUIESCED DC    CL10'QUIESCED'                                                   
UNQUI    DC    CL10'UNQUIESCED'                                                 
QOPENED  DC    CL10'QOPENED'                                                    
STARTED  DC    CL10'STARTED'                                                    
STOPPED  DC    CL10'STOPPED'                                                    
QSTARTED DC    CL10'QSTARTED'                                                   
DMREAD   DC    CL7'DMREAD'                                                      
DMWRT    DC    CL7'DMWRT'                                                       
DADDS    DC    CL7'DADDS'                                                       
ISDDS    DC    CL7'ISDDS'                                                       
FIRSTDA  DC    X'00010101'                                                      
SPACES   DC    CL80' '                                                          
FFS      DC    X'FFFFFFFF'                                                      
DOTS     DC    C'..........................'                                    
ZEROS    DC    4F'0'                                                            
         LTORG                                                                  
*                                                                               
DAOPEN   EQU   14                                                               
RDID     EQU   01                                                               
ADDADR   EQU   11                                                               
DACLOSE  EQU   15                                                               
*                                                                               
         DS    0H                                                               
HDR1     DC    C'SEL TIME     FROM TO       COMMAND  DATA            '          
         DC    C'                              '                                
HDR2     DC    C'--- -------- ---- -------- -------- ----------------'          
         DC    C'------------------------------'                                
         EJECT                                                                  
       ++INCLUDE FACOMTAB                                                       
         EJECT                                                                  
***********************************************************************         
* BROADCAST TABLE                                                     *         
***********************************************************************         
BRDTABL  DC    CL7'RDWEEK ',AL1(03)                                             
         DC    CL7'RDSAT  ',AL1(04)                                             
         DC    CL7'RDSUN  ',AL1(05)                                             
         DC    CL7'READ10 ',AL1(06)                                             
         DC    CL7'RDONLY ',AL1(07)                                             
         DC    CL7'XXXXXXX',AL1(FF)                                             
         EJECT                                                                  
***********************************************************************         
* RESOURCE TABLE                                                      *         
***********************************************************************         
RESRCTBL DC    CL8'MEDDSPC ',AL2(0001)                                          
         DC    CL8'MEDDSPC1',AL2(0002)                                          
         DC    CL8'MEDDSPC2',AL2(0003)                                          
         DC    CL8'MEDDSPC3',AL2(0004)                                          
         DC    CL8'MEDDSPC4',AL2(0005)                                          
         DC    CL8'MEDDSPC5',AL2(0006)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* WORKING STOTAGE DSECT                                               *         
***********************************************************************         
WORKD    DSECT                                                                  
SAVERD   DS    A                                                                
SAVERE   DS    A                                                                
ASENTRY  DS    A                                                                
RELO     DS    A                                                                
*                                                                               
SRPARS   DS    0XL32                                                            
SRPAR1   DS    A                                                                
SRPAR2   DS    A                                                                
SRPAR3   DS    A                                                                
SRPAR4   DS    A                                                                
SRPAR5   DS    A                                                                
SRPAR6   DS    A                                                                
SRPAR7   DS    A                                                                
SRPAR8   DS    A                                                                
*                                                                               
P1       DS    A                                                                
P2       DS    A                                                                
P3       DS    A                                                                
P4       DS    A                                                                
P5       DS    A                                                                
P6       DS    A                                                                
*                                                                               
ASCANNER DS    A                                                                
AHEXOUT  DS    A                                                                
AMQIO    DS    A                                                                
AGETFACT DS    A                                                                
ADATCON  DS    A                                                                
AGETDAY  DS    A                                                                
AADDAY   DS    A                                                                
ASYSLST  DS    A                                                                
*                                                                               
ATIA     DS    A                                                                
AUTL     DS    A                                                                
ATCB     DS    A                                                                
AIOAREA  DS    A                                                                
ABUFF1   DS    A                                                                
ABUFF2   DS    A                                                                
*                                                                               
AREQUEST DS    A                                                                
AFILES   DS    A                                                                
AFILEX   DS    A                                                                
ACOMTAB  DS    A                                                                
*                                                                               
ACOMM    DS    A                                                                
SAVER0   DS    A                   KEEP FOR BCT COUNTER                         
*                                                                               
APGMS    DS    A                                                                
ARPL     DS    A                                                                
NSYM     DS    CL8                                                              
*                                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
FLAG     DS    C                                                                
DMCB     DS    6F                                                               
*                                                                               
TODAY    DS    F                                                                
TOMORROW DS    F                                                                
YESTERDY DS    F                                                                
*                                                                               
MYLUID   DS    CL8                                                              
SYSNAME  DS    CL4                 3 CHR FACPAK NAME                            
SAVETCB  DS    XL10                                                             
COMMAND  DS    CL10                                                             
HEADER   DS    X                                                                
SVSEIND  DS    X                   SAVED SEIND FROM OPEN                        
*                                                                               
ACTNUM   DS    H                   OPER ACTION NUMBER                           
*                                                                               
ADNAME   DS    XL3                 ADV NAME                                     
ADNUM    DS    XL1                 ADV NUMBER                                   
*                                                                               
JOBNAME  DS    CL8                 JOBNAME MATCH                                
JOBNAMEL DS    X                   JOBNAME LENGTH                               
*                                                                               
SYNAME   DS    CL8                 SYS NAME                                     
SYNUM    DS    XL2                 SYS NUM                                      
SYGROUP  DS    A                   A(GROUP LIST)                                
RESOURCE DS    XL2                 RESOURCE NUMBER                              
RESCL16  DS    CL16                RESOURCE FOR DEQ                             
*                                                                               
DISPLOOP DS    F                                                                
DISPFLAG DS    C                                                                
ONLINE   DS    C                   SET TO Y IF =OPER                            
*                                                                               
FFLAGS   DS    XL2                 FILE FLAGS                                   
FILNUM   DS    X                   FILE EXT NUMBER                              
         ORG   *-1                                                              
BRDNUM   DS    X                   BROADCAST NUMBER                             
FILNAM   DS    CL8                 FILE NAME                                    
ADTF     DS    A                   DTF FOR FILE                                 
*                                                                               
WORK     DS    XL80                                                             
WORK1    DS    XL80                                                             
CARD     DS    XL80                                                             
DATA     DS    XL16                                                             
*                                                                               
SCANBLK  DS    CL256                                                            
*                                                                               
IOAREA   DS    4096C               IO AREA                                      
BUFF1    DS    14336C              BUFFER 1                                     
BUFF2    DS    14336C              BUFFER 2                                     
*                                                                               
WORKX    EQU   *                                                                
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
CODEST   DS    CL8                                                              
         DS    CL1                                                              
COCOMM   DS    CL32                                                             
         ORG                                                                    
COLINEL  EQU   *-COLINED                                                        
         EJECT                                                                  
*                                                                               
PATCHWKD DSECT                                                                  
PATCHENQ DS    CL4                 PATCH AREA ENQ FLAG                          
         DS    CL4                                                              
         DS    CL4                                                              
         DS    CL4                                                              
PATCHPHS DS    AL4                 ADDR OF PHASE TO PATCH                       
PATCHLEN DS    AL4                 LENGTH OF PHASE TO PATCH                     
PATCHWRK DS    AL4                 A(WORK AREA)                                 
PATCHOFS DS    XL4                 OFFSET TO LEVEL= INFORMATION                 
PATCHDET DS    128XL4              128 * OFFSET,LEN (PATCH DETAIL)              
*                                                                               
PATCHSYS DS    128XL16             SYSTEM RETURNS                               
*                                                                               
PATCHSYD DSECT                                                                  
PATCHSY4 DS    CL4                 4 CHR SYS ID                                 
PATCHADR DS    AL4                 ADDR OF PHASE                                
PATCHDTL DS    AL4                 DATE/LEVEL OF PHASE                          
         DS    AL4                                                              
*                                                                               
         EJECT                                                                  
SROPSFFD DS    CL64                                                             
       ++INCLUDE SROPSFFD                                                       
         EJECT                                                                  
         DSECT                                                                  
         IEZCIB                                                                 
         IEZCOM                                                                 
         ISTDNIB                                                                
         EJECT                                                                  
* DMDSHDR                                                                       
* DMDTFIS                                                                       
* DMDTFPH                                                                       
* DMSYSFD                                                                       
* DDCOMFACS                                                                     
* CTGENFILE                                                                     
* FADSECTS                                                                      
* FASYSLIST                                                                     
* FAFACTS                                                                       
* FARPLD                                                                        
* FAPIGFACD                                                                     
* DDSCANBLKD                                                                    
       ++INCLUDE DMDSHDR                                                        
       ++INCLUDE DMDSYSHDR                                                      
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DMDTFPH                                                        
       ++INCLUDE DMSYSFD                                                        
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FADSECTS                                                       
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FARPLD                                                         
       ++INCLUDE FAPIGFACD                                                      
       ++INCLUDE FACOMTABD                                                      
       ++INCLUDE DDSCANBLKD                                                     
         IHAASCB LIST=YES                                                       
         PUSH ACONTROL                                                          
         ACONTROL COMPAT(NOCASE),FLAG(NOPAGE0),TYPECHECK(NOREGISTER)            
         IHAASSB LIST=YES                                                       
         POP  ACONTROL                                                          
         IAZJSAB LIST=YES                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SROPS00   03/15/19'                                      
         END                                                                    
