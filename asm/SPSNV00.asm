*          DATA SET SPSNV00    AT LEVEL 071 AS OF 10/31/16                      
*PHASE T21000A                                                                  
*INCLUDE MEDGET                                                                 
*CHANGE BINSRCH(BINSR24)                                                        
*INCLUDE BINSRCH2                                                               
*CHANGE BINSRCH(BINSR31)                                                        
*INCLUDE BINSR31                                                                
*INCLUDE DPTRD                                                                  
*INCLUDE EQVRD                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE TIMPK                                                                  
*INCLUDE TIMUNPK                                                                
*INCLUDE KHDUMMY                                                                
T21000   TITLE 'SPSNV00 - SPOT NEW INVOICE CONTROLLER'                          
T21000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T21000,R7,RR=R2,CLEAR=YES                                
         ST    R2,RELO                                                          
         L     RA,4(R1)            A(TWA)                                       
         USING T210FFD,RA                                                       
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
         ST    R7,BASER7                                                        
         LR    R9,R1                                                            
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      GRABBING 3 I/O AREAS PLUS LABELS             
         USING SYSD,R9                                                          
         MVC   ATIOB,0(R1)         A(TIOB)                                      
         BAS   RE,SYSINIT          INITIALIZE PROGRAM DEPENDENT VALUES          
         MVC   STAFIL,=C'STATION '                                              
         MVC   INVDIR,=C'XSPDIR  '                                              
         MVC   INVFIL,=C'XSPFIL  '                                              
*                                                                               
MAIN10   OI    CONSERVH+1,X'01'    SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    CONSERVH+6,X'80'                                                 
         XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREAS                          
         XC    CONHED2,CONHED2                                                  
         OI    CONHED2H+6,X'80'                                                 
*                                                                               
         CLI   CONRECH+5,0         NO RECORD INPUTTED?                          
         BNE   *+14                                                             
         MVC   CONREC(7),=CL7'INVOICE'  ALWAYS INVOICE RECORD TYPE              
         MVI   CONRECH+5,7                                                      
*                                                                               
         CLI   CONACTH+5,0         NO ACTION INPUTTED?                          
         BNE   *+14                                                             
         MVC   CONACT(4),=CL4'LIST'    DEFAULT IS TO LIST                       
         MVI   CONACTH+5,4                                                      
*                                                                               
         BAS   RE,VALIACT          VALIDATE ACTION                              
         BNE   XIT                                                              
*                                                                               
         TM    CONRECH+4,X'20'     IF RECORD FIELD HAS CHANGED                  
         BO    *+8                                                              
         OI    TRNSTAT,RCHANG      THEN SET RCHANG FLAG                         
*                                                                               
         TM    CONACTH+4,X'20'     IF ACTION FIELD HAS CHANGED                  
         BO    MAIN20                                                           
         OI    TRNSTAT,ACHANG      THEN SET ACHANG FLAG                         
*                                                                               
         CLC   =C'CHA',CONACT      AND IT CHANGED TO 'CHA'                      
         BNE   *+8                                                              
         OI    TRNSTAT,USERCHA     THEN SET USER CAUSED 'CHA' FLAG              
*                                                                               
MAIN20   BAS   RE,GOGENCON         GO OFF TO GENCON                             
*                                                                               
         OI    CONRECH+4,X'20'     VALIDATE RECORD/ACTION FIELDS                
         OI    CONACTH+4,X'20'                                                  
*                                                                               
         TM    MNIOFLAG,X'80'      DO WE NEED TO CLOSE MINIO BUFFER             
         BZ    XIT                                                              
*                                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
***********************************                                             
* CODE FOR THE EXTRA X'FE' ELEMENT                                              
***********************************                                             
MAIN21   XC    MINEKEY,MINEKEY     SPARE ELEMENT FOR UPDATIVE SOON?             
         MVI   MINEKEY,SNVMMELQ    LOOK FOR X'E8' ELEM FIRST                    
         L     R2,MINELEM                                                       
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    MAIN21A                                                          
         CLI   MINERR,MINEEOF      NOT FOUND?                                   
         BE    MAIN22              CAN'T FIND IT                                
         DC    H'0'                                                             
*                                                                               
MAIN21A  CLI   0(R2),SNVMMELQ      DO WE HAVE A X'E8' ELEMENT ALREADY?          
         BE    MAIN25              YES, NO NEED FOR X'FE' ELEMENT THEN          
*                                                                               
MAIN22   MVI   MINEKEY,X'FE'                                                    
         L     R2,MINELEM                                                       
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    MAIN22A                                                          
         CLI   MINERR,MINEEOF      NOT FOUND?                                   
         BE    MAIN23              CAN'T FIND IT                                
         DC    H'0'                                                             
*                                                                               
MAIN22A  CLI   0(R2),X'FE'         HAVE A X'FE' ELEMENT ALREADY?                
         BE    MAIN25              YES, NO NEED FOR X'FE' ELEMENT THEN          
*                                                                               
MAIN23   XC    MELEM,MELEM         ADD THIS SO UPDATIVE SOON WON'T DIE          
         MVI   MELEM,X'FE'            WHEN THE RECORD NEEDS TO SPLIT            
         MVI   MELEM+1,X'4B'                                                    
         GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAIN25   XC    HLDSCID,HLDSCID     CLEAR HELD SECID                             
         XC    MINEKEY,MINEKEY     LOOK FOR THE ACTIVITY ELEMENT                
         MVI   MINEKEY,X'F1'                                                    
         L     R2,MINELEM                                                       
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    MAIN40                                                           
         CLI   MINERR,MINEEOF      NOT FOUND?                                   
         BE    MAIN30              CAN'T FIND IT                                
         DC    H'0'                                                             
***************                                                                 
* ADD ACTIVITY ELEMENT                                                          
***************                                                                 
         USING ACTVD,R2                                                         
MAIN30   XC    0(ACTVLENQ,R2),0(R2)                                             
         MVI   ACTVEL,X'F1'                                                     
         MVI   ACTVLEN,ACTVLENQ                                                 
         GOTO1 DATCON,DMCB,(5,0),(3,ACTVADDT)                                   
         MVC   ACTVCHDT,ACTVADDT                                                
         MVC   ACTVADID,TWAORIG                                                 
         LA    R3,ACTVADID                                                      
         CLC   =C'ADD',CONACT                                                   
         BNE   MAIN45              ADD? NO, SAVE CHANGE DATA                    
         B     MAIN50                                                           
***************                                                                 
* CHANGE ACTIVITY ELEMENT                                                       
***************                                                                 
MAIN40   CLI   ACTVEL,X'F1'        FOUND THE ACTIVITY ELEMENT?                  
         BNE   MAIN30              NO, ADD IT THEN                              
         CLI   ACTVLEN,ACTVLENQ    IF FULL LENGTH ELEM                          
         BL    *+10                                                             
         MVC   HLDSCID,ACTVSCID    HOLD SECID                                   
         GOTO1 MINIO,DMCB,('MINDEL',(R5))  YES, DELETE IT                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    ACTVADDT,ACTVADDT   MY STUPID ACTIVITY ELEMENTS?                 
         BZ    MAIN30              YES, ADD STANDARD ONES                       
*                                                                               
MAIN45   GOTO1 DATCON,DMCB,(5,0),(3,ACTVCHDT)                                   
         MVI   ACTVLEN,ACTVLENQ    UPDATE LENGTH TO INCLUDE ACTVSCID            
         AI    ACTVCHNM,1          INCREMENT THE CHANGE NUMHBER                 
         LA    R3,ACTVCHID                                                      
*                                                                               
MAIN50   MVC   0(L'TWAORIG,R3),TWAORIG                                          
         GOTO1 GETFACT,DMCB,0                                                   
         L     R4,0(R1)                                                         
         USING FACTSD,R4                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE?                  
         BZ    MAIN55                                                           
         MVC   0(2,R3),FAPASSWD                                                 
         OI    2(R3),X'80'                                                      
*                                                                               
MAIN55   DS    0H                                                               
         CLC   =C'ADD',CONACT                                                   
         BNE   *+10                ADD?                                         
         MVC   ACTVCHID,ACTVADID   YES, SAVE ADD ID AS CHANGE ID                
*                                                                               
*        CLC   =C'ADD',CONACT      IF NOT ADDING A NEW RECORD                   
*        BE    MAIN56                                                           
*        MVC   ACTVSCID,HLDSCID    USE THE PID WE ALREADY HAVE                  
*        B     MAIN62                                                           
*                                                                               
MAIN56   DS    0H                                                               
*        OC    ASECBLK,ASECBLK     IS NEW SECURITY ACTIVE?                      
*        BZ    MAIN60            **NOTE-IT ISN'T ACTIVE FOR NINV NOW**          
*                                                                               
         L     R1,ASECBLK2                                                      
         USING SECD,R1                                                          
         MVC   ACTVSCID,SECPID                                                  
         B     MAIN62                                                           
         DROP  R1                                                               
*&&DO                                                                           
MAIN60   DS    0H                  NEW SECURITY NOT ACTIVE                      
         OC    FAPASSWD,FAPASSWD   IF NO PASSWORD                               
         BZ    MAIN62                                                           
         LA    R6,KEY                                                           
         USING SA0KEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   SA0KTYP,C'0'                                                     
         MVC   SA0KAGY,FATAGYSC     AGENCY FOR SECURITY                         
         MVC   SA0KNUM,FAPASSWD                                                 
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO                 
*                                                                               
         L     R6,AIO                                                           
         LA    R6,SA0DATA                                                       
         MVI   ELCODE,X'C3'        PERSONAL ID ELEM                             
         BAS   RE,FIRSTEL                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
         USING SAPALD,R6                                                        
         MVC   ACTVSCID,SAPALPID                                                
*&&                                                                             
*        DROP  R6                                                               
         DROP  R2                                                               
*                                                                               
MAIN62   GOTO1 MINIO,DMCB,('MINADD',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 MINIO,DMCB,('MINCLS',(R5))    YES                                
         CLI   MINERR,0                                                         
         BE    XIT                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
SETPROF  NTR1                                                                   
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
*        CLI   CLTOFFIC,C' '                                                    
*        BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFIC                                              
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
* INITIALIZE PROGRAM DEPENDENT VALUES *                                         
***********************************************************************         
SYSINIT  NTR1                                                                   
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS10    L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS10                                                         
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,PRGCOMM                                                       
         LA    R0,NPRGCOMM                                                      
*                                                                               
SYS20    ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,SYS20                                                         
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES            COUNTER                                      
         LA    R4,COREFACS         POINT TO ADDRESS AREA                        
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)            A(COMFACS)                                   
         L     RF,CCALLOV-COMFACSD(R1)                                          
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS30    MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
         BCT   R0,SYS30                                                         
* FIX GETBROAD ADDRESS                                                          
         MVI   DMCB+7,QGETBRD                                                   
         GOTO1 (RF),(R1)                                                        
         MVC   GETBROAD,DMCB                                                    
* FIX RECUP TO USE FACPAK LINKED VERSION                                        
         L     RE,SYSPARMS                                                      
         L     RE,8(RE)            POINT TO FACILITIES LIST                     
         MVC   RECUP,28(RE)                                                     
*                                                                               
         LA    R0,GOMSPACK         REROUTE MSPACK/MSUNPK ROUTINES               
         ST    R0,MSPACK                                                        
         LA    R0,GOMSUNPK                                                      
         ST    R0,MSUNPK                                                        
*                                                                               
         MVI   SYSTEM,C'S'         PRINT                                        
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         MVC   LKEY,=Y(L'AGYKEY)   DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=Y(L'AGYCTL)                                             
         MVC   DATADISP,=Y(AGYEL-AGYHDR)                                        
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   ACTELOPT,C'N'       DON'T ADD ACTIVITY ELEMENT                   
         MVI   IOOPT,C'Y'          WE'LL DO OUR OWN IO'S                        
         MVI   GETMSYS,2           USES GETMSG FOR SYSTEM 2 (SPOT)              
         MVC   LWORK,=AL4(LENWORK) SET WORK AREA LENGTH                         
         MVC   RCPROG(2),=C'SP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9021000'    PRESET FOR SYSTEM CALLOVS               
         LA    R1,STARTSV          SET SAVED STORAGE START                      
         ST    R1,ASTARTSV                                                      
*                                                                               
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         OC    TWAVPRNT,TWAVPRNT   DON'T BOTHER IF OFFLINE                      
         BNZ   SYS40                                                            
         CLI   TWAOFFC,C'*'        IF NOT DDS TERMINAL                          
         BE    SYS40                                                            
         LA    R1,RECACT1          DON'T ALLOW ALL RECORD TYPES                 
         ST    R1,ARECACT1                                                      
         LA    R1,RECACT2          OR ALL ACTIONS TYPES                         
         ST    R1,ARECACT2                                                      
         LA    R1,RECACT3          OR ALL RECORD/ACTION COMBINATIONS            
         ST    R1,ARECACT3                                                      
*                                                                               
SYS40    LA    R1,CONRECH          SET EFH TAGS                                 
         ST    R1,EFHREC                                                        
         LA    R1,CONACTH                                                       
         ST    R1,EFHACT                                                        
         LA    R1,CONKEYH                                                       
         ST    R1,EFHKEY                                                        
*                                                                               
         OI    CONWHENH+1,X'2C'                                                 
         OI    CONWHENH+6,X'80'                                                 
         LA    R1,CONWHENH                                                      
         ST    R1,EFHWHEN                                                       
         OI    CONOUTH+1,X'2C'                                                  
         OI    CONOUTH+6,X'80'                                                  
         LA    R1,CONOUTH                                                       
         ST    R1,EFHOUT                                                        
         OI    CONDESTH+1,X'2C'                                                 
         OI    CONDESTH+6,X'80'                                                 
         LA    R1,CONDESTH                                                      
         ST    R1,EFHDEST                                                       
         OI    CONOTHH+1,X'2C'                                                  
         OI    CONOTHH+6,X'80'                                                  
         LA    R1,CONOTHH                                                       
         ST    R1,EFHOTH                                                        
*                                                                               
         LA    R1,CONTAGH                                                       
         ST    R1,EFHTAG                                                        
*                                                                               
         OI    GENSTAT1,NOSETEFH+RDUPAPPL                                       
         OI    GENSTAT3,OKVALSEL+RESTXE00                                       
         OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
         MVC   LSVTWA0,=AL2(MAXLTWA0)  L'STORAGE TO SAVE IN TWA0                
         MVI   NTWA,0              DON'T SAVE ANY EXTRA PAGES                   
         MVI   LRECACT,L'RECACT    SET L'RECACT TABLE ENTRY                     
*                                                                               
         L     RF,ATIOB            A(TIOB)                                      
         USING TIOBD,RF                                                         
         SR    R0,R0                                                            
         IC    R0,TIOBAID          PICK UP PFKEY VALUE                          
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         MVC   CURDISP,TIOBCURD    SAVE CURSOR DISPLACEMENT                     
         DROP  RF                                                               
*                                                                               
         LH    RE,=Y(SECBLK-T210FFD)                                            
         AR    RE,RA                                                            
         ST    RE,ASECBLK2         GENCON USES ASECBLK!!!                       
*                                                                               
         LHI   RF,L'SECBLK                                                      
         XCEFL (RE),(RF)                                                        
*                                                                               
         OC    T210FFD+4(2),T210FFD+4   TEST ON NEW SECURITY                    
         BNZ   *+14                                                             
         OC    T210FFD+6(2),T210FFD+6   OR HAVE LIMIT ACCESS                    
         BZ    SYS50                                                            
*                                                                               
*        L     R1,ASECBLK2                                                      
*        XC    0(250,R1),0(R1)     CLEAR TO MAKE SURE INIT WORKS                
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)            A(COMFACS)                                  
         L     RF,CSECRET-COMFACSD(R1)                                          
         GOTO1 (RF),DMCB,('SECPINIT',ASECBLK2),0                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
SYS50    DS    0H                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)            A(COMFACS)                                  
         L     RF,CGETFACT-COMFACSD(,RF)                                        
         GOTO1 (RF),DMCB,(X'80',DUB),F#XA9ADR                                   
         MVC   AXA9BUFF,DUB                                                     
*                                                                               
         BRAS  RE,CLRPTAB           CLEAR XA9 BUFFER AREA                       
*                                                                               
         BRAS  RE,INITBSP                                                       
*                                                                               
SYSX     B     XIT                                                              
*                                                                               
***********************************************************************         
* VALIDATE ACTION TO SEE IF TERMINAL AUTHORIZED                                 
***********************************************************************         
VALIACT  NTR1                                                                   
         CLI   1(RA),C'*'          IF DDS TERMINAL                              
         BE    VACTYES             THEN DON'T CHECK IF AUTHORIZED               
*                                                                               
         LA    R2,CONACTH          CHECK ENTERED ACTION                         
         LA    R3,RECACT2          AGAINST OUR ACTION TABLE                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
*                                                                               
VACTLP   EX    R1,*+8              FIND CORRESPONDING ACTION                    
         B     *+10                                                             
         CLC   CONACT(0),1(R3)                                                  
         BE    VACT10                                                           
         LA    R3,L'RECACT(R3)                                                  
         CLI   0(R3),X'02'                                                      
         BNE   VACTYES                                                          
         B     VACTLP                                                           
*                                                                               
VACT10   MVC   CONACT,1(R3)                                                     
         MVI   CONACTH+5,L'CONACT                                               
         OI    CONACTH+6,X'80'                                                  
*                                                                               
         MVC   HALF,12(R3)         CHECK AUTHORIZATION CODE                     
         NC    HALF,12(RA)                                                      
         BZ    VACTYES                                                          
*                               IF ANY BIT IS ON, THEN NOT AUTHORIZED           
         MVC   CONHEAD(30),=CL30'ERROR: ACTION NOT AUTHORIZED'                  
         OI    CONHEADH+6,X'80'                                                 
         L     R1,ATIOB                                                         
         OI    TIOBINDS-TIOBD(R1),TIOBSETC+TIOBALRM                             
         SR    R2,RA                                                            
         STCM  R2,3,TIOBCURD-TIOBD(R1)                                          
*                                                                               
VACTNO   B     NO                                                               
VACTYES  B     YES                                                              
*                                                                               
***********************************************************************         
* ROUTINE HANDLES PASSING OF CONTROL TO AND FROM GENCON                         
***********************************************************************         
GOGENCON NTR1                                                                   
         BAS   RE,SETRD            SET RD SO GENCON ALWAYS RETURNS              
*                                                                               
GOG10    MVI   GOAGAIN,C'N'        INITIALIZE RETURN SWITCH                     
         OI    TRNSTAT,FRSTMODE    ALLOWS APPL TO DETECT FIRST MODE             
*                                                                               
GOG20    GOTO1 GENCON,DMCB,(R8)    OFF TO GENCON - PASS A(W/S)                  
*                                                                               
         CLI   GOAGAIN,C'Y'        REQUEST BY APPLIC. TO GO BACK                
         BE    GOG10                                                            
         ICM   R1,15,AFRSTKEY      IF CURSOR IS AT FIRST KEY FIELD              
         BZ    GOG40                                                            
         TM    6(R1),X'40'                                                      
         BZ    GOG40                                                            
         CLI   OKNO,2              AND GENCON IS ASKING FOR MORE INPUT          
         BNE   GOG30                                                            
         CLI   GOAGAIN,C'K'        AND WE DIDN'T TRY THIS ALREADY               
         BE    GOG30                                                            
         CLI   ACTNUM,ACTDEL       AND IF ACTION IS NOT DELETE                  
         BE    GOG40                                                            
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         BE    GOG40                                                            
*                                                                               
         MVI   CONKEY,C','         MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   CONKEYH+5,1         APPLICATION GETS A CHANCE TO FILL            
         MVI   GOAGAIN,C'K'        IN KEY FIELDS                                
         B     GOG20               GO BACK                                      
*                                                                               
GOG30    CLI   5(R1),0             IF NOTHING IS IN FIRST KEY FIELD             
         BNE   *+12                                                             
         CLI   ERROR,MISSING       AND MISSING INPUT FIELD ERROR                
         BE    PLSENTER            SWITCH TO PLEASE ENTER FIELDS ...            
*                                                                               
GOG40    CLI   ACTNUM,ACTLIST      IF ACTION IS LIST                            
         BNE   GOGX                                                             
         CLI   OKNO,16             IF GENCON MSG IS "END OF LIST - HIT          
         BE    SELFIRST            ENTER...", CHANGE TO SELECT OR HIT..         
         CLI   OKNO,15             IF MSG IS "LIST DISPLAYED - HIT              
         BE    SELNEXT             ENTER...", CHANGE TO SELECT OF HIT..         
*                                                                               
GOGX     B     XIT                 ALL THROUGH                                  
*                                                                               
SETRD    NTR1                                                                   
         ST    RD,SYSRD            SET RD SO WE GET CONTROL BACK                
         B     XIT                                                              
*                                                                               
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY *                              
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VUSER                                                            
         B     VMED                                                             
         B     VCLT                                                             
         B     VPROD                                                            
         B     VSTAT                                                            
         B     VSLN                                                             
         B     VEST                                                             
         B     VCURSERR                                                         
         B     VMYERR                                                           
         B     VCLEARF                                                          
         B     VINITIAL                                                         
         B     VGETTWA                                                          
         B     VINITMNO                                                         
         B     VGTPRFIL                                                         
         B     VSETMED                                                          
         B     CKSPI2                                                           
         B     ADDPE                                                            
         B     BLDPRTB                                                          
         B     GENI2R                                                           
         B     CLRPRTB                                                          
*                                                                               
BASER7   DC    A(0)                                                             
*                                                                               
TRAPERR  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
* THIS ROUTINE GETS CALLED BY GENCON ON EVERY TRANSACTION                       
* BEFORE CALLING THE APPLICATION                                                
*                                                                               
VUSER    CLI   OFFLINE,C'Y'        ALWAYS DO THIS OFFLINE                       
         BE    VUSER10                                                          
         CLI   TWAFIRST,0          TEST FIRST TIME                              
         BE    VUSER10             YES - READ DATA                              
         MVC   USERNAME(66),SVUSER ELSE MOVED SAVED DATA                        
         B     VUSER20                                                          
*                                                                               
VUSER10  MVI   TWAFIRST,1          WE'VE BEEN THROUGH HERE                      
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),TWAORIG FROM TWA                                     
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIO                 
*                                                                               
         L     R4,AIO                                                           
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'        ORIGIN DETAILS                               
         BAS   RE,FIRSTEL                                                       
         BE    VUSER15                                                          
         LA    R2,CONHEADH                                                      
         XC    8(L'CONHEAD,R2),8(R2)                                            
         MVC   8(L'MSNGORGN,R2),MSNGORGN                                        
         OI    6(R2),X'80'                                                      
         DC    H'0'                                                             
         DC    C'$ABEND'                                                        
MSNGORGN DC    C'MISSING ORIGIN INFO IN IDI RECORD'                             
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
VUSER15  MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSER(66),USERNAME SAVE FOR FUTURE REF                          
         DROP  R6                                                               
*                                                                               
VUSER20  MVC   RECNUM,TWALREC     MOVE SAVED REC/ACT TO CURRENT IN CASE         
         MVC   ACTNUM,TWALACT     WE TAKE ERROR BEFORE GENCON SETS THEM         
*                                                                               
         TM    TRNSTAT,RACHANG     IF RECORD & ACTION FLD HAS CHANGED           
         BNO   VUSERX                                                           
         MVI   CALLSP,0            CLEAR CALLPROG STACK                         
         XC    CONHED2,CONHED2     CLEAR DISPLAY OF SCREENS TO POP TO           
*                                                                               
VUSERX   B     XIT                                                              
*                                                                               
* OVERLAY INITIALIZATION                                                        
*                                                                               
*                                  P1=A(PFKEY VAL. TABLE) OR ZEROS              
*                                                                               
VINITIAL ICM   R3,7,1(R1)          IF PFKEY VALIDATION TABLE PASSED             
         BZ    INIT10                                                           
         MVI   SKIPFLAG,C'N'                                                    
         TM    CTLRFLG1,CF1TSELQ+CF1CKOFF                                       
         BNO   *+8                                                              
         MVI   SKIPFLAG,C'Y'                                                    
         BAS   RE,TESTSEL          TEST FOR SPECIAL SELECT CODE                 
*                                                                               
         CLI   SKIPFLAG,C'Y'                                                    
         BE    INIT05                                                           
         CLI   RECNUM,X'01'        INVOICE REC?                                 
         BNE   INIT05              THIS CHECK FOR INVOICES ONLY                 
         CLI   ACTNUM,X'0A'        ACTION = LIST?                               
         BNE   INIT05                                                           
         CLI   PFKEY,X'00'                                                      
         BE    INIT05                                                           
         CLI   PFKEY,X'01'                                                      
         BE    INIT05                                                           
         LR    RE,RA                                                            
         AH    RE,CURDISP                                                       
         CLC   33(10,RE),=10X'40'                                               
         BE    INIT03                                                           
         CLC   33(10,RE),=XL10'00'                                              
         BNE   INIT05                                                           
INIT03   MVI   GETMSYS,X'02'                                                    
         MVC   GERROR,=AL2(NOINVNUM)                                            
         LR    R2,RE                                                            
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         GOTO1 MYERR                                                            
*                                                                               
INIT05   DS    0H                                                               
         GOTO1 PFVAL,DMCB,(R3)     HANDLE LOCAL PFKEY PRESENCE                  
         BE    DUMMYERR            TAKE DUMMY ERROR EXIT FOR GOAGAIN            
*                                                                               
INIT10   MVI   SCRSTAT,0           CLEAR SCREEN STATUS BYTE                     
*                                                                               
         CLC   TWASCR,SVSCR        TEST SCREEN CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,SCRCHG                                                   
*                                                                               
         CLC   RECNUM,SVREC        TEST RECORD CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,RECCHG                                                   
*                                                                               
         MVC   BYTE,ACTNUM         MOVE CURRENT ACTION TO TEMP. W/S             
         CLI   BYTE,ACTCHA         IF CURRENT ACTION IS CHANGE                  
         BNE   *+16                                                             
         CLI   SVACT,ACTSEL        AND SAVED ACTION WAS SELECT                  
         BNE   *+8                                                              
         MVI   BYTE,ACTSEL         PRETEND CURRENT ACTION IS SELECT             
*                                                                               
         CLC   BYTE,SVACT          TEST ACTION CHANGE                           
         BE    *+8                                                              
         OI    SCRSTAT,ACTCHG                                                   
*                                                                               
         TM    SCRSTAT,RECCHG      ALWAYS CLEAR IF RECORD TYPE CHANGED          
         BO    INIT20                                                           
         TM    SCRSTAT,SCRCHG      NEVER CLEAR IF SCREEN DIDN'T CHANGE          
         BZ    INIT30                                                           
         CLI   BYTE,ACTREP         ALWAYS CLEAR IF ACTION IS NOW REPORT         
         BE    INIT20                                                           
         CLI   SVACT,ACTSEL        IF LAST ACTION NOT SELECT                    
         BE    INIT30                                                           
         CLI   BYTE,ACTSEL         AND THIS ACTION NOT SELECT                   
         BE    INIT30                                                           
*                                                                               
INIT20   TM    CTLRFLG1,CF1NOCLR       DON'T CLEAR APPLICATION STORAGE?         
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1NOCLR  YES, BUT CLEAR IT NEXT TIME             
         B     INIT30                                                           
*                                                                               
         LA    RE,SYSSPARE         CLEAR APPLICATION STORAGE                    
         LH    RF,=AL2(L'SYSSPARE)                                              
         XCEFL                                                                  
         LA    RE,CONHEADH         FIND END OF SCREEN                           
         SR    RF,RF                                                            
         ICM   RF,1,0(RE)                                                       
         BZ    *+10                                                             
         AR    RE,RF                                                            
         B     *-10                                                             
         LA    RE,3(RE)            BUMP PAST CONTROL BYTES                      
         LR    RF,RE                                                            
         SR    RF,RA                                                            
         SH    RF,=AL2(3520+64)    L'AVAIL TWA0 AS DEFINED IN DDGENTWA          
         LCR   RF,RF                                                            
         XCEFL ,                   CLEAR AREA AFTER SCREEN END                  
*                                                                               
INIT30   MVC   SVSCR,TWASCR        SAVE CURRENT SCREEN                          
         MVC   SVREC,RECNUM                     RECORD                          
         MVC   SVACT,BYTE                       ACTION                          
*                                                                               
INITX    B     XIT                                                              
*                                                                               
*              LOCAL ROUTINE TO HANDLE PFKEY PRESENCE                           
*                                                                               
*                                  P1  BYTES 1-3 = A(PFKEY VAL. TABLE)          
PFVAL    NTR1                                                                   
         CLI   PFKEY,0             USER HIT ENTER?                              
         BE    NO                  YES                                          
*                                                                               
         L     RF,0(R1)            RF=A(PFKEY TABLE)                            
         USING PFTABD,RF           LOOK UP PFKEY NUMBER IN TABLE                
PFV2     CLI   0(RF),X'FF'                                                      
         BE    PFERR                                                            
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    PFV3                                                             
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     PFV2                                                             
*                                                                               
PFV3     TM    PFTSTAT2,PFTRETRN   TEST RETURN TO APPLICATION                   
         BO    NO                                                               
*                                                                               
         BAS   RE,PFINVOKE         OK TO INVOKE PFKEY                           
         B     YES                 IF RETURNS, RETURN CC EQUAL                  
*                                                                               
*              ROUTINE TO PROCESS PFKEY REQUEST                                 
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
PFINVOKE NTR1                                                                   
         TM    PFTSTAT2,PFTCLRKY   DO WE CLEAR THE PFKEY?                       
         BNZ   *+8                 NO                                           
         MVI   PFKEY,0             CLEAR PFKEY FOR NEXT SCREEN                  
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         MVI   TIOBAID-TIOBD(RE),0 CLEAR PF KEY HERE AS WELL                    
*                                                                               
         TM    PFTSTAT,PFTCPROG    TEST PFKEY GENERATES CALLPROG CALL           
         BZ    *+8                                                              
         BAS   RE,CPROG                                                         
*                                                                               
         CLI   PFTNKEYS,0          TEST KEY FIELDS PRESENT                      
         BE    *+8                                                              
         BAS   RE,EXPNDKEY         EXPAND THEM INTO 'KEY' FIELD                 
*                                                                               
         TM    PFTSTAT,PFTRPROG    POP NESTED CALL SEQUENCE                     
         BZ    *+12                                                             
         BAS   RE,RPROG            ROUTINE TO RESTORE PREV. SCREEN              
         B     DUMMYERR            TAKE DUMMY ERROR XIT FOR GOAGAIN             
*                                                                               
         CLI   PFTREC,C' '         IF NEW RECORD TYPE DEFINED                   
         BE    PFI8                                                             
         MVC   CONREC,PFTREC       MOVE IT OUT                                  
         OI    CONRECH+6,X'80'     TRANSMIT                                     
         MVI   CONRECH+5,8         SET L'I/P                                    
*                                                                               
         L     RE,EFHKEY           RE=A(KEY FIELD)                              
         CLI   5(RE),0             IF THERE'S NO INPUT IN KEY FIELD             
         BNE   *+12                                                             
         MVI   8(RE),C','          MOVE A COMMA TO KEY FIELD SO THAT            
         MVI   5(RE),1             APPLICATION GETS CONTROL                     
*                                                                               
PFI8     CLI   PFTACT,C' '         TEST FOR ACTION CHANGE                       
         BE    PFIX                                                             
         MVC   CONACT,PFTACT       MOVE IT OUT                                  
         OI    CONACTH+6,X'80'     TRANSMIT                                     
         MVI   CONACTH+5,5         SET L'I/P - NOTE ONLY 5                      
*                                                                               
PFIX     B     XIT                                                              
*                                                                               
*              ROUTINE TO TEST FOR SPECIAL SELECT CODE ON LISTS                 
*              R3=A(PFKEY TABLE)                                                
TESTSEL  NTR1                                                                   
         CLI   ACTNUM,ACTLIST      IF LIST SCREEN                               
         BNE   TSELX                                                            
*                                                                               
         TM    CTLRFLG1,CF1TSELQ         DON'T TEST SEL FIELDS?                 
         BZ    *+12                                                             
         NI    CTLRFLG1,X'FF'-CF1TSELQ   YES, RESET FOR NEXT TIME               
         B     TSELX                                                            
*                                                                               
         L     R2,AFRSTREC         LOOP THROUGH SELECT FIELDS                   
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
*                                                                               
TSEL2    STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         CLI   5(R2),0             TEST SELECT CODE INPUT                       
         BE    TSEL6                                                            
         OC    8(3,R2),SPACES                                                   
         LR    RF,R3               RF=A(START OF TABLE)                         
         USING PFTABD,RF                                                        
TSEL4    CLI   0(RF),X'FF'                                                      
         BE    TSEL6                                                            
         CLC   PFTSEL,8(R2)        MATCH ON EXACT SELECT CODE                   
         BE    TSEL8                                                            
         ZIC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     TSEL4                                                            
*                                                                               
TSEL6    ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    TSELX               (E-O-S)                                      
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE           WHEN ROW CHANGES, PROCESS NEXT               
         BNE   TSEL2               SELECT FIELD                                 
         B     TSEL6                                                            
*                                                                               
TSEL8    MVC   8(3,R2),SPACES      FOUND A MATCH - CLEAR SELECT FIELD           
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
         LR    RE,R2               SAVE A(FIELD)                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         CLI   7(R2),0             TEST THERE'S SOMETHING TO SELECT             
         BE    TSEL6               (NO, SO IGNORE)                              
*                                                                               
         MVC   PFKEY,PFTAID        SET CORRESPONDING PFKEY NUMBER               
         SR    RE,RA                                                            
         STH   RE,CURDISP          SAVE DISP. TO FIELD                          
*                                                                               
TSELX    B     XIT                                                              
*                                                                               
* THIS ROUTINE SAVES THE CURRENT TWA IN THE FIRST HALF OF TEMPSTR               
* RECORD NUMBERS 2.  IT THEN SAVES THE SCREEN NUMBER FOUND IN                   
* TWASCR ONTO A STACK.  THE USE OF THIS ROUTINE IN CONJUNCTION WITH             
* THE CHANGING OF THE RECORD, ACTION, AND KEY FIELDS ALLOWS USERS TO            
* CALL UP A NEW SCREEN AND THEN LATER RETURN TO THE SCREEN THEY WERE            
* WORKING ON.  WHEN THE USER WANTS TO RETURN TO A SCREEN, RETPROG WILL          
* BE CALLED TO RESTORE THE SCREEN.                                              
*                                                                               
CPROG    NTR1                                                                   
         MVC   CALLSTCK,TWASCR     SAVE SCREEN NUMBER ON STACK                  
         MVI   CALLSP,1                                                         
*                                                                               
         L     RE,ATIA             SAVE SCREEN IN FIRST HALF OF TWA             
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    R2,X'82'            WRITE TWA RECORD #2                          
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         LA    R2,X'83'            WRITE TWA RECORD #3                          
         GOTO1 GETTWA,DMCB,((R2),STARTSV)                                       
*                                                                               
         B     XIT                                                              
*                                                                               
* THIS ROUTINE RESTORES THE USER TO THE SCREEN THEY WERE WORKING ON             
* BEFORE CALLING ANOTHER SCREEN WHICH WAS SAVED IN TEMPSTR BY CALLPROG.         
*                                                                               
RPROG    NTR1                                                                   
         CLI   CALLSP,0                                                         
         BE    PFERR               ERROR IF STACK IS EMPTY                      
*                                                                               
         LA    R2,2                READ TWA RECORD #2                           
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
*                                                                               
         L     RE,ATIA             RESTORE SCREEN FROM 1ST HALF OF TWA          
         LH    RF,=Y(TWAMXLEN)                                                  
         L     R0,ATWA                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   ACTNUM,TWALACT      SPECIAL CODE TO KEEP SELECT GOING            
         MVC   RECNUM,TWALREC                                                   
         MVC   CONHEAD(30),=CL30'Came back from another screen.'                
         MVC   CONHEAD+30(30),=CL30'  Please continue ...'                      
*                                                                               
         LA    R2,3                READ TWA RECORD #3                           
         GOTO1 GETTWA,DMCB,((R2),STARTSV)                                       
*                                                                               
         MVI   CALLSP,0            DECREMENT STACK POINTER                      
         MVC   TWASCR,CALLSTCK     EXTRACT TWASCR                               
*                                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         OI    TRNSTAT,RETURNED    SET THAT RETPROG HAS BEEN CALLED             
*                                                                               
         B     XIT                                                              
*                                                                               
*              ROUTINE TO EXPAND KEY FIELDS INTO TMPKEY FIELD                   
*                                                                               
         USING PFTABD,RF           RF=A(PFTABLE ENTRY)                          
EXPNDKEY NTR1                                                                   
         MVC   WORK,SPACES         BUILD KEY FIELD IN WORK FIRST                
         LA    R2,WORK             R2=A(WORK)                                   
         ZIC   R3,PFTNKEYS         R3=N'KEY FIELDS                              
         LA    R4,PFTKEYS          SET R4=A(1ST KEY FIELD)                      
         USING KEYD,R4                                                          
*                                                                               
EXP10    CLI   KEYTYPE,KEYTYCOM    TEST SIMPLY PLACE IMBEDDED COMMA             
         BE    EXP20                                                            
         LR    RF,RA               SET WHERE DATA IS                            
         CLI   KEYTYPE,KEYTYTWA    TWA                                          
         BE    EXP15                                                            
         L     RF,ASTARTSV                                                      
         LA    RF,SYSSPARE                                                      
         CLI   KEYTYPE,KEYTYWS     W/S (SYSSPARE)                               
         BE    EXP15                                                            
         CLI   KEYTYPE,KEYTYCUR    CURSOR LOCATION                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LH    RF,CURDISP          ASSUME THIS IS A SELECT FIELD                
         AR    RF,RA               RF=A(FLD WHERE CURSOR IS)                    
         BAS   RE,BMPTOROW         BUMP TO FIRST FIELD FOR THIS ROW             
         BNE   PFERR                                                            
         L     RF,FULL             RETURNS ADDRESS IN FULL                      
*                                                                               
EXP15    AH    RF,KEYDISP          RF=A(DATA)                                   
         ZIC   RE,KEYLEN           RE=L'DATA-1                                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)       MOVE TO WORK                                 
         AR    R2,RE               BUMP TO LAST CHARACTER OF FIELD              
*                                                                               
         CLI   0(R2),C' '          SHUFFLE BACK TO 1ST NON-SPACE                
         BH    *+10                                                             
         BCTR  R2,0                                                             
         BCT   RE,*-10                                                          
         LA    R2,1(R2)            BUMP TO 1ST POSITION PAST                    
*                                                                               
         CH    R3,=H'1'            TEST THIS IS LAST KEY FIELD                  
         BE    EXPX                SO FINISH UP                                 
*                                                                               
EXP20    MVI   0(R2),C','          INSERT COMMA BEFORE NEXT FIELD               
         LA    R2,1(R2)            BUMP PAST COMMA TO NEXT POSITION             
         LA    R4,KEYNEXT          BUMP TO NEXT KEY FIELD                       
         BCT   R3,EXP10            AND PROCESS                                  
*                                                                               
EXPX     LA    R3,WORK                                                          
         SR    R2,R3               R2=L'TMPKEY FIELD                            
         CLM   R2,1,=AL1(L'TMPKEY)                                              
         BNH   *+6                                                              
         DC    H'0'                MAKE TMPKEY BIGGER                           
*                                                                               
         STC   R2,TMPKEYH+5        STORE LENGTH IN FIELD HEADER                 
         MVI   TMPKEYH,L'TMPKEY+L'TMPKEYH SET LENGTH OF FIELD                   
         LA    RE,TMPKEYH                                                       
         ST    RE,EFHKEY           TELL GENCON TO USE TMPKEY FIELD              
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     XIT                                                              
         MVC   TMPKEY(0),WORK      MOVE DATA TO FAKE KEY FIELD                  
*                                                                               
BMPTOROW NTR1                      BUMP TO FIRST FIELD IN ROW                   
         LR    R2,RF               R2=A(CURRENT FIELD)                          
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         STC   R1,BYTE             SAVE CURRENT ROW NUMBER                      
*                                                                               
         L     R2,AFRSTREC                                                      
BMPT2    LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = ROW NUMBER MINUS 1                      
         CLM   R1,1,BYTE                                                        
         BE    BMPT4                                                            
         ZIC   R1,0(R2)            TRY NEXT FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BNE   BMPT2                                                            
         B     NO                  RETURN CC NE IF REACHED E-O-S                
*                                                                               
BMPT4    ZIC   R1,0(R2)                                                         
         AR    R2,R1               ASSUMING SELECT FIELD -- BUMP PAST           
         LA    R2,8(R2)            AND PAST HEADER OF (FIRST) DATA FLD          
         ST    R2,FULL             MATCH-RETURN A(FLD HEADER) IN FULL           
         B     YES                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE MEDIA FIELD                                        
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF MEDIA FIELD                  
*                                                                               
* ON EXIT:     BAGYMD              BINARY AGENCY/MEDIA COMBINATION              
*              QMED                EBCDIC MEDIA                                 
*              MEDNM               MEDIA NAME                                   
***********************************************************************         
VMED     GOTO1 ANY                                                              
*                                                                               
         XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R1,KEY                                                           
         USING AGYKEY,R1                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,AGENCY                                                   
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
         MVC   SVAFLG1,AGYFLAG1                FLAG 1                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VMED10   BAS   RE,NEXTEL                                                        
         BE    VMED20                                                           
         MVI   GERROR1,INVMED                                                   
         OI    GENSTAT2,USMYERSY                                                
         B     VMYERR                                                           
*                                                                               
         USING AGYMEDEL,R6                                                      
VMED20   CLC   AGYMEDCD,8(R2)                                                   
         BNE   VMED10                                                           
         MVC   QMED,AGYMEDCD       SAVE INPUT MEDIA CODE                        
         MVC   BAGYMD,AGYMEDBT     DIG OUT AGENCY/MEDIA                         
         MVC   MEDNM,AGYMEDEX      MEDIA NAME                                   
*                                                                               
         XC    WORK,WORK           GET SPOTPROF                                 
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGENCY    A/M ONLY - NO CLIENT                         
         MVC   WORK+6(1),QMED                                                   
         GOTO1 GETPROF,DMCB,WORK,SPOTPROF,DATAMGR                               
*                                                                               
         MVI   NETPAKSW,C'N'       SET NETPAK SWITCH                            
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           NET SYSTEM?                                  
         BNE   *+8                                                              
         MVI   NETPAKSW,C'Y'                                                    
         DROP  R1                                                               
*                                                                               
*        CLI   QMED,C'N'                                                        
*        BNE   VMED30                                                           
*        CLI   COUNTRY,C'C'        AND NOT CANADA                               
*        BE    VMED30                                                           
*        MVI   NETPAKSW,C'Y'       IS NETPAK                                    
*                                                                               
VMED30   MVI   CENTS,C'Y'          CENTS                                        
         CLI   NETPAKSW,C'Y'       IF NETPAK                                    
         BE    VMED40                                                           
         CLI   QMED,C'N'           OR IF NOT NETWORK                            
         BNE   VMED40                                                           
         CLI   COUNTRY,C'C'        AND NOT CANADA                               
         BE    VMED40                                                           
         MVI   CENTS,C'N'                                                       
*                                                                               
VMED40   DS    0H                                                               
*                                                                               
VMEDX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE CLIENT FIELD                                       
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF CLIENT FIELD                 
*                                                                               
* ON EXIT:     BCLT                CLIENT CODE                                  
*              BTRACLT             TRAFFIC CLIENT CODE                          
*              BTRAPRD             TRAFFIC PRODUCT CODE                         
*              QCLT                EBCDIC CLIENT                                
*              CLTNM               CLIENT NAME                                  
*              SVCLIST             CLIENT PRODUCT LIST                          
***********************************************************************         
VCLT     GOTO1 ANY                 CLIENT                                       
*                                                                               
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVCLI                                                   
         CLI   5(R2),3                                                          
         BH    VMYERR                                                           
         CLI   5(R2),2                                                          
         BL    VMYERR                                                           
*                                                                               
         MVC   QCLT,WORK                                                        
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   VMYERR                                                           
         NI    GENSTAT2,X'FF'-USMYERSY                                          
*                                                                               
* READ CLIENT HEADER *                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING CKEY,R1                                                          
         MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYCLT,BCLT                                                     
         DROP  R1                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
* SAVE CLIENT PRODUCT LIST *                                                    
*                                                                               
         LA    RE,SVCLIST                                                       
         LHI   RF,1024             L'SVCLIST                                    
         XCEFL                                                                  
*                                                                               
         LA    R0,SVCLIST                                                       
         LA    R1,880                                                           
         LA    RE,CLIST                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LHI   R1,140              LENGTH OF CLIST2                             
         LA    RE,CLIST2                                                        
         LR    RF,R1                                                            
         CLC   CKEY+13(2),=AL2(CLIST2-CLTHDR+1)                                 
         BL    *+6                                                              
         MVCL  R0,RE                                                            
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   CLTNM,CNAME         AND CLIENT NAME                              
         MVC   BTRACLT,CMCLTCOD    SAVE TRAFFIC CLIENT                          
         MVC   BTRAPRD,CMCLTPRD         PRODUCT                                 
         MVC   CLTOFFIC,COFFICE         AND OFFICE                              
         MVC   SVCACCS,CACCESS                                                  
         MVC   SVCOPT4,COPT4                                                    
*                                                                               
         BAS   RE,SETPROF          GET I2X PROF                                 
         MVC   WORK(4),=C'sI2X'                                                 
         GOTO1 GETPROF,DMCB,WORK,PROFI2X,DATAMGR                                
*                                                                               
         MVC   WORK(4),=C'sI2R'                                                 
         GOTO1 GETPROF,DMCB,WORK,PROFI2R,DATAMGR                                
*                                                                               
         MVC   WORK(4),=C'S0TI'                                                 
         GOTO1 GETPROF,DMCB,WORK,PROF0TI,DATAMGR                                
*                                                                               
         MVC   WORK(4),=C'sINV'                                                 
         GOTO1 GETPROF,DMCB,WORK,PROFINV,DATAMGR                                
*                                                                               
*        CHECK CLIENT SECURITY                                                  
*                                                                               
         MVI   GERROR1,SECLOCK                                                  
         OC    TWAACCS(2),TWAACCS  ANY SECURITY LIMIT?                          
         BZ    VCLTX                                                            
         CLI   TWAACCS,C'+'        MKT LOCKOUT?                                 
         BE    VCLTX               YES -SKIP AT CLIENT TIME                     
*                                                                               
         BRAS  RE,CALLOFCR                                                      
*                                                                               
VCLTX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE PRODUCT FIELD                                      
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF PRODUCT FIELD                
*                                                                               
* ON EXIT:     BPRD                PRODUCT CODE                                 
*              QPRD                EBCDIC PRODUCT                               
*              PRDNM               PRODUCT NAME                                 
***********************************************************************         
VPROD    GOTO1 ANY                                                              
*                                                                               
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVPROD                                                  
         CLI   5(R2),2                                                          
         BL    VMYERR                                                           
         CLI   5(R2),3                                                          
         BH    VMYERR                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,WORK                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVC   BPRD,PCODE+1        NETPAK PRODUCT > 250 ARE ALL X'0000'         
         MVC   QPRD,PKEYPRD                                                     
         MVC   PRDNM,PNAME                                                      
         NI    GENSTAT2,X'FF'-USMYERSY                                          
*                                                                               
VPRDX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
*&&DO                                                                           
***********************************************************************         
* THIS ROUTINE VALIDATES THE SPOT LENGTH FIELD                                  
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF SPOT LENGTH FIELD            
*                                                                               
* ON EXIT:     BSLN                BINARY SPOT LENGTH                           
***********************************************************************         
VSLN     GOTO1 ANY                                                              
*                                                                               
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVSLEN                                                  
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    VMYERR                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         CH    R1,=H'255'          SPOT LENGTH MORE THAN 1 BYTE?                
         BH    VMYERR              ERROR IF IT IS                               
         STC   R1,BSLN                                                          
*                                                                               
         LA    R1,SLNTAB           MAKE SURE SPOT LENGTH IS IN OUR              
VSLN10   CLI   0(R1),0                 TABLE                                    
         BE    VMYERR                                                           
         CLC   0(1,R1),BSLN                                                     
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     VSLN10                                                           
         NI    GENSTAT2,X'FF'-USMYERSY                                          
*                                                                               
VSLNX    B     XIT                                                              
*                                                                               
SLNTAB   DS    0C                                                               
       ++INCLUDE SPSLNTAB                                                       
         DC    5X'00'    INSURANCE                                              
SLNTABX  EQU   *                                                                
*&&                                                                             
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE SPOT LENGTH FIELD                                  
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF SPOT LENGTH FIELD            
*                                                                               
* ON EXIT:     BSLN                BINARY SPOT LENGTH                           
***********************************************************************         
VSLN     GOTO1 ANY                                                              
*                                                                               
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVSLEN                                                  
         TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    VMYERR                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         CH    R1,=H'255'          SPOT LENGTH MORE THAN 1 BYTE?                
         BH    VMYERR              ERROR IF IT IS                               
         STC   R1,BSLN                                                          
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,8(R1)            A(COMFACS)                                   
         L     RF,CCALLOV-COMFACSD(R1)                                          
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A57' SLNTAB                                    
         GOTO1 (RF),DMCB,0                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)            A(SLNTAB)                                    
         LR    R1,RF               POINT TO SLNTAB                              
         LH    RE,0(R1)            GET ENTRY LENGTH                             
         L     RF,2(R1)            GET DSPL TO EOT                              
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         SR    R0,R0                                                            
         LA    R0,C'T'                                                          
         CLI   QMED,C'T'                                                        
         BE    VSLN20                                                           
         CLI   QMED,C'N'                                                        
         BE    VSLN20                                                           
         CLI   QMED,C'C'                                                        
         BE    VSLN20                                                           
*                                                                               
         LA    R0,C'R'                                                          
         CLI   QMED,C'R'                                                        
         BE    VSLN20                                                           
         CLI   QMED,C'X'                                                        
         BE    VSLN20                                                           
         DC    H'0'                                                             
*                                                                               
VSLN20   CLC   =C'00',0(R1)        FIND DEFAULT ENTRY                           
         BE    VSLN40                                                           
         CLC   AGENCY,0(R1)        MATCH AGY                                    
         BNE   *+12                                                             
VSLN40   CLM   R0,1,2(R1)          MATCH MEDIA                                  
         BE    VSLN60                                                           
*                                                                               
         BXLE  R1,RE,VSLN20        NEXT ENTRY                                   
         B     VMYERR                                                           
*                                                                               
VSLN60   DS    0H                                                               
         AHI   R1,4                POINT BEYOND TABLE ID                        
         SR    RE,RE                                                            
         IC    RE,BYTE             GET SLN                                      
         AR    RE,RE               X 2                                          
         AR    RE,R1               POINT TO ENTRY                               
         CLI   1(RE),0             TEST SLN VALID                               
         BE    VMYERR              EXIT WITH CC SET                             
*                                                                               
VSLN90   MVC   BSLN,BYTE                                                        
         NI    GENSTAT2,X'FF'-USMYERSY                                          
*                                                                               
VSLNX    B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE ESTIMATE FIELD                                     
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF ESTIMATE FIELD               
*                                                                               
* ON EXIT:     BEST                BINARY ESTIMATE                              
*              QEST                EBCDIC ESTIMATE                              
*              ESTNM               ESTIMATE NAME                                
***********************************************************************         
VEST     GOTO1 ANY                                                              
*                                                                               
         MVC   QEST,WORK                                                        
*                                                                               
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVESTMT                                                 
         TM    4(R2),X'08'         TEST VALID NUMERIC                           
         BZ    VMYERR                                                           
         CLI   5(R2),3                                                          
         BH    VMYERR                                                           
*                                                                               
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'1'            TEST IN RANGE 1-255                          
         BL    VMYERR                                                           
         CH    RE,=H'255'                                                       
         BH    VMYERR                                                           
         STC   RE,BEST             SET BINARY ESTIMATE                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY              READ ESTIMATE HEADER                         
         USING ESTHDRD,R6                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,BEST                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVC   ESTNM,EDESC         SET ESTIMATE NAME                            
         MVC   ESTDYMNU,EDAYMENU   SET ESTIMATE DAYPART MENU                    
         MVC   ESTSTRT,ESTART      SET ESTIMATE PERIOD                          
         MVC   ESTEND,EEND                                                      
         MVC   ESTBOOK,EBOOK       SET ESTIMATE RATING BOOK                     
         MVC   ESTDEMOS,EDEMLST    SET ESTIMATE DEMOS                           
         NI    GENSTAT2,X'FF'-USMYERSY                                          
*                                                                               
VESTX    B     XIT                                                              
         DROP  R6                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE VALIDATES THE STATION CALL LETTERS                               
*                                                                               
* ON ENTRY:    (R2)                FIELD HEADER OF STATION FIELD                
*                                                                               
* ON EXIT:     QSTA                EBCDIC STATION CALL LETTERS                  
*              QMKT                EDCDIC MARKET NUMBER                         
*              BMKTSTA             BINARY MARKET/STATION COMBINATION            
*              QNTWK               EBCDIC NETWORK                               
*              BNTWK               BINARY NETWORK REPRESENTATION                
***********************************************************************         
VSTAT    GOTO1 ANY                                                              
*                                                                               
         MVI   QSUBMED,C' '                                                     
         XC    WORK,WORK                                                        
         XC    QNTWK,QNTWK                                                      
         MVI   BNTWK,0                                                          
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVSTATN                                                 
*                                                                               
         L     R6,AIO1                                                          
         USING STABLKD,R6                                                       
         XC    0(STBLNQ,R6),0(R6)                                               
         MVC   STBMED,QMED         SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVC   STBCTRY,COUNTRY                                                  
         MVC   STBACOM,ACOMFACS                                                 
*&&DO                                                                           
         CLI   8(R2),C'0'                                                       
         BL    VSTA00                                                           
         MVC   GERROR,=AL2(301)    CABLE STATIONS CAN'T BE USED TODAY           
         MVI   GETMSYS,23                                                       
         B     VMYERR                                                           
*&&                                                                             
VSTA00   GOTO1 STAVAL,DMCB,(R6)                                                 
         CLI   STBERR,4                                                         
         BNE   *+14                                                             
         MVC   GERROR,=AL2(ND2ACTVT)                                            
         B     VMYERR                                                           
*                                                                               
         CLI   STBERR,0                                                         
         BNE   VMYERR                                                           
*                                                                               
         MVC   QSTA,STBSTA         SET OUTPUT STATION                           
         CLI   QSTA+4,C' '                                                      
         BH    *+10                                                             
         MVC   QSTA+4(1),QMED                                                   
*                                                                               
         MVC   QNTWK,STBNET        SAVE THE EBCDIC NETWORK                      
***************                                                                 
* READ STATION MASTER RECORD                                                    
***************                                                                 
VSTA10   MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),STAFIL,KEY,AIO                       
         CLI   8(R1),0                                                          
         BNE   VMYERR                                                           
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
*                                                                               
         MVC   SVSFLAG1,SFLAG1                                                  
*                                                                               
* check submedia                                                                
         CLI   NETPAKSW,C'Y'                                                    
         BNE   VSTA11                                                           
*                                                                               
         MVC   QSUBMED,STYPE      SAVE SUBMEDIA                                 
*                                                                               
         GOTO1 GETPRFIL,DMCB,=C'sI2Z',PROFI2Z    !!! lowercase 's' !!!          
         CLI   PROFI2Z+6,C'Y'     READ PROFILES BY SUBMEDIA?                    
         BNE   VSTA11                                                           
         CLC   STYPE,QMED                                                       
         BE    VSTA11                                                           
         CLI   STYPE,C' '                                                       
         BE    VSTA11                                                           
         MVC   SVMEDIA,QMED                                                     
         MVC   QMED,STYPE          GET SUBMEDIA                                 
         GOTO1 GETPRFIL,DMCB,=C'sI2X',PROFI2X  <= notice lowercase 's'          
         GOTO1 (RF),(R1),=C'S0TI',PROF0TI                                       
         MVC   QMED,SVMEDIA                                                     
*                                                                               
VSTA11   DS    0H                                                               
         MVC   QMKT,SMKT                                                        
         GOTO1 MSPACK,DMCB,QMKT,QSTA,BMKTSTA                                    
         BNE   VMYERR                                                           
*                                                                               
***************                                                                 
* READ MARKET RECORD TO IO1+200                                                 
***************                                                                 
VSTA12   LA    R6,200(R6)                                                       
         ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         OI    GENSTAT2,USMYERSY                                                
         MVI   GERROR1,INVMRKT                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),STAFIL,KEY,AIO                       
*                                                                               
         MVC   AIO,AIO1            RESTORE AIO                                  
*                                                                               
         CLC   KEY(MKTKEYLN),0(R6)                                              
         BNE   VMYERR                                                           
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BE    VSTA15                                                           
         LA    RF,TWAACCS                                                       
         CLI   TWAACCS,C'+'        MARKET LOCKOUT?                              
         BE    *+16                                                             
         LA    RF,TWAACCS+2                                                     
         CLI   TWAACCS+2,C'+'        MARKET LOCKOUT?                            
         BNE   VSTA15                                                           
*                                                                               
         LA    R0,3                                                             
         LA    R1,MKTLTACC                                                      
         CLC   1(1,RF),0(R1)                                                    
         BE    VSTA15                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,*-14                                                          
         MVI   GERROR1,NOMKTACC                                                 
         B     VMYERR                                                           
*                                                                               
VSTA15   MVC   MKTNM,MKTNAME       RETURN MARKET NAME TO USER                   
         MVI   BYTE,C'0'           FIND RATING SERVICE MARKET                   
         CLI   BKVALSRC,C'N'                                                    
         BE    *+8                                                              
         MVI   BYTE,C'1'                                                        
         CLC   MKTRS1,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM1                                                    
         CLC   MKTRS2,BYTE                                                      
         BNE   *+10                                                             
         MVC   MKTRS,MKTRSM2                                                    
*                                                                               
         NI    GENSTAT2,X'FF'-USMYERSY                                          
*                                                                               
VSTAX    B     XIT                                                              
*                                                                               
***********************************************************************         
* SET CURSOR TO ERROR POSITION AND XIT TO ERROR ROUTINE                         
*                                                                               
* AT ENTRY, P1 BYTE  1   = LENGTH OF 2ND HALF OF SCANNER FIELDS                 
*              BYTES 2-4 = A(SCANNER BLOCK)                                     
*           P2 BYTE  1   = 0 -- GOTO1 ERREX                                     
*                        = 2 -- GOTO1 ERREX2                                    
*              BYTES 2-4 = A(XL1 CONTAINING INVALID FIELD NUMBER)               
*           R2 POINTS TO OFFENDING FIELD HEADER                                 
***********************************************************************         
VCURSERR SR    R4,R4                                                            
         ICM   R4,7,1(R1)          A(SCANNER BLOCK)                             
         ZIC   RE,0(R1)            LENGTH OF 2ND HALF OF SCANNER FIELDS         
         LA    RE,22(RE)           TOTAL LENGTH OF EACH SCANNER FIELD           
         LA    R3,8(R2)            A(NEW CURSOR POSITION)                       
         SR    R5,R5                                                            
         MVC   BYTE,4(R1)          ERROR ROUTINE SWITCH                         
         ICM   R5,7,5(R1)          A(INVALID FIELD NUMBER)                      
         CLI   0(R5),1             TEST FIRST FIELD IS INVALID                  
         BE    VC100               CURSOR NEED NOT BE ADJUSTED                  
         LA    RF,1                                                             
*                                                                               
VC20     ZIC   R1,0(R4)            LENGTH OF FIRST HALF OF FIELD                
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS ','              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    VC100                                                            
*                                                                               
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    VC40                LENGTH OF SECOND HALF OF FIELD               
         LA    R3,1(R1,R3)         ADD LENGTH TO POSITION PLUS '='              
         LA    RF,1(RF)                                                         
         CLM   RF,1,0(R5)                                                       
         BE    VC100                                                            
*                                                                               
VC40     LA    R4,0(RE,R4)         NEXT SCANNER ENTRY                           
         B     VC20                                                             
*                                                                               
VC100    L     R1,SYSPARMS                                                      
         L     R1,0(R1)            A(TIOB)                                      
         USING TIOBD,R1                                                         
         LR    RF,R2               COMPUTE DISPLACEMENT OF ERROR FLDH           
         S     RF,ATWA             FROM TWA START                               
         STCM  RF,3,TIOBCURD                                                    
         LR    RF,R2                                                            
         LA    RF,8(RF)            RF=A(FIELD START)                            
         SR    R3,RF               COMPUTE INDEX INTO FIELD FOR CURSOR          
         STC   R3,TIOBCURI                                                      
         OI    TIOBINDS,TIOBSETC                                                
*                                                                               
         CLI   BYTE,0              GO TO PROPER ERROR ROUTINE                   
         BE    VC200                                                            
         CLI   BYTE,2                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX2                                                           
VC200    GOTO1 ERREX                                                            
         DROP  R1                                                               
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS           
***********************************************************************         
VMYERR   TM    MNIOFLAG,X'80'      MUST CLOSE MINIO BUFFER?                     
         BZ    VMYERR10            NO                                           
*                                                                               
         LA    R5,MINBLOCK         YES, MUST CLOSE AFTER A MATCH                
         USING MINBLKD,R5                                                       
         GOTO1 MINIO,DMCB,('MINCLS',(R5))                                       
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
VMYERR10 OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
*                                                                               
         MVC   GTMSYS,GETMSYS      SYSTEM MESSAGES                              
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    VMYERR20            NO                                           
         TM    GENSTAT2,USMYERSY   YES, USE GENERAL SYSTEM?                     
         BNZ   VMYERR20                 NO                                      
         MVI   GTMSYS,X'FF'             YES                                     
         DROP  RF                                                               
*                                                                               
VMYERR20 GOTO1 ERREX                                                            
*                                                                               
***********************************************************************         
* ROUTINE TO READ/WRITE TEMPSTR PAGES                                           
*                                                                               
* ON ENTRY:    PARAM 1  BYTE  0    BIT SETTINGS/PAGE NUMBER                     
*              PARAM 1  BYTES 1-3  READ/WRITE ADDRESS                           
***********************************************************************         
VGETTWA  MVC   BYTE,0(R1)          BIT SETTINGS/PAGE NUMBER                     
         L     R2,0(R1)            READ/WRITE ADDRESS                           
*                                                                               
         MVC   COMMAND(6),=C'DMWRT '                                            
         TM    BYTE,X'80'          X'80'=1 IS WRITE, ELSE READ                  
         BO    GTWA10                                                           
         MVC   COMMAND(6),=C'DMRDIR'                                            
         TM    BYTE,X'40'          X'40'=1 IS 2304 BYTE TWAS ELSE 6144          
         BNZ   GTWA10                                                           
         MVC   COMMAND(6),=C'DMREAD'                                            
*                                                                               
GTWA10   NI    BYTE,X'0F'          TURN OFF HIGH ORDER BITS                     
*                                                                               
         MVC   DMCB+8(1),BYTE      PAGE NUMBER                                  
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
*                                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,=C'TEMPSTR',,(R2),0                         
*                                                                               
         CLI   8(R1),0             IF COULDN'T DO IT, DIE                       
         BE    XIT                                                              
         DC    H'0'                                                             
*                                                                               
***********************************************************************         
* INITIALIZE MINIO VALUES                                                       
***********************************************************************         
VINITMNO DS    0H                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         MVC   BYTE,MINBF2         PRESERVE FOR THOSE USING 2 MASTERS           
*                                                                               
         LA    R0,MINBLOCK         CLEAR MINBLOCK                               
         LA    R1,MINBLKL                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   MINBF2,BYTE                                                      
*                                                                               
         MVC   MINRECUP,RECUP                                                   
         MVC   MINCOMF,ACOMFACS    A(COMFACS)                                   
         MVI   MINOPEN,C'N'        SET NOT OPEN                                 
         MVC   MINFIL,INVFIL       FILE NAME                                    
         MVC   MINDIR,INVDIR       DIR NAME                                     
         MVI   MINFKLEN,L'SNVKEY   KEY LENGTH                                   
         MVI   MINEKLEN,L'SNVKMINK   ELEMENT KEY LENGTH                         
         MVI   MINEKDSP,L'SNVKMAST   DISPLACEMENT TO ELEMENT KEY                
         MVC   MINAGYC,AGENCY                                                   
         MVI   MINAGYD,36                                                       
         MVI   MINNCTL,L'SNVDSTAT  NUMBER OF CONTROL BYTES                      
         MVC   MINFRCLM,=AL2(3975) MAXIMUM RECORD LENGTH                        
         MVC   MINBUFF,AIO2        A(FIRST BUFFER)                              
         MVI   MINNBUF,2           USE TWO BUFFERS                              
         L     R1,ASYSD                                                         
         AH    R1,=Y(MINSTRT-SYSD)                                              
         ST    R1,MINRTAB          A(AREA FOR RECORD TABLE)                     
         MVC   MINRTABL,=Y(LENMINIO)  LENGTH OF RECORD TABLE                    
*                                                                               
         LA    RE,MELEM            A(AREA FOR ELEM OR CLUSTER)                  
         ST    RE,MINELEM                                                       
         MVC   MINMAXEL,=Y(L'MELEM)   MAX LENGTH OF ELEM OF CLUSTER             
         XC    0(L'MELEM,RE),0(RE)   CLEAR MINELEM AREA                         
* BUILD MASTER KEY                                                              
         XC    MINMKEY,MINMKEY     CLEAR MASTER KEY FOR MINIO                   
         LA    R4,MINMKEY                                                       
         USING SNVKEY,R4                                                        
         MVI   SNVKTYPE,SNVKTYPQ                                                
         MVI   SNVKSUB,SNVKSUBQ                                                 
         MVC   SNVKAM,BAGYMD                                                    
         MVC   SNVKCLT,BCLT                                                     
         MVC   SNVKSTA,BSTA                                                     
         MVC   SNVKMOS,BMOSS                                                    
         XC    SNVKMOS,=X'FFFF'                                                 
         MVC   SNVKINV,QINVOICE                                                 
         B     XIT                                                              
         DROP  R4,R5                                                            
*                                                                               
***********************************************************************         
* GETS PROFILE REC                                                              
*                                                                               
* ON ENTRY:    P1                  A(PROFILE PROGRAM CODE)                      
*              P2                  A(16 BYTE PROFILE AREA)                      
***********************************************************************         
VGTPRFIL DS    0H                                                               
         L     R2,0(R1)                                                         
         L     R3,4(R1)                                                         
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
*        CLI   CLTOFFIC,C' '                                                    
*        BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFIC                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,0(R3),DATAMGR                                  
GTPRFX   B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SETMED - TRANSLATES BAND TO MEDIA (A,F->R, N,C->N, ETC.)                      
* R0 TO ADDRESS STATION MEDIA BYTE (BAND REALLY)                                
* R1 TO ADDRESS OUTPUT BYTE, WHERE T,N,R,X WILL BE PLACED                       
***********************************************************************         
VSETMED  LR    R2,R0               A(INPUT)                                     
         LA    R3,SETMTAB                                                       
*                                                                               
SETM10   CLI   0(R3),X'FF'                                                      
         BE    NO                                                               
*                                                                               
         CLC   0(1,R3),0(R2)                                                    
         BE    *+12                                                             
         LA    R3,SETMTBLQ(R3)                                                  
         B     SETM10                                                           
*                                                                               
         MVC   0(1,R1),1(R3)                                                    
         B     YES                                                              
*                                                                               
SETMTAB  DC    C'T',C'T'                                                        
SETMTBLQ EQU   *-SETMTAB                                                        
         DC    C'D',C'T'                                                        
         DC    C'A',C'R'                                                        
         DC    C'F',C'R'                                                        
         DC    C'S',C'R'                                                        
         DC    C'N',C'N'                                                        
         DC    C'C',C'N'                                                        
         DC    C'X',C'X'                                                        
         DC    C'L',C'T'                                                        
         DC    X'FF'                                                            
*                                                                               
*                                                                               
* CHECK IF WE NEED TO GENERATE SPECIAL I2 REQUEST FOR MINDSHARE                 
CKSPI2   DS    0H                                                               
         BRAS  RE,CHKIFI2                                                       
         BE    YES                                                              
         B     NO                                                               
*                                                                               
*                                                                               
* ADD P1, P2, ESTIMATE TO GENI2 PRODUCT-ESTIMATE TABLE                          
ADDPE    DS    0H                                                               
         BRAS  RE,ADDP1P2E                                                      
         B     YES                                                              
*                                                                               
*                                                                               
*                                                                               
BLDPRTB  DS    0H                                                               
         BRAS  RE,BLDPTAB                                                       
         B     YES                                                              
*                                                                               
*                                                                               
*                                                                               
CLRPRTB  DS    0H                                                               
         BRAS  RE,CLRPTAB                                                       
         BRAS  RE,INITBSP                                                       
         B     YES                                                              
*                                                                               
*                                                                               
*                                                                               
GENI2R   DS    0H                                                               
         L     R2,AXA9BUFF                                                      
*                                                                               
* TABLE: PRD(3), PR2(3), EST(3)                                                 
*                                                                               
GENI2A   SAM31                                                                  
         MVC   WORK(9),0(R2)                                                    
         SAM24                                                                  
*                                                                               
         BRAS  RE,GENI2REQ                                                      
*                                                                               
         AHI   R2,9                                                             
         SAM31                                                                  
         CLI   0(R2),0                                                          
         SAM24                                                                  
         JNE   GENI2A                                                           
*                                                                               
         B     YES                                                              
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* VCLEARF - CLEAR AND FOUT FIELDS                                               
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
***********************************************************************         
VCLEARF  LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
*                                                                               
VCLEARF2 IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,*+8              BRANCH ACCORDINGLY                           
         B     *+8                                                              
         BC    0,VCLEARF4                                                       
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    6(R2),X'80'         SET TO TRANSMIT                              
*                                                                               
VCLEARF4 LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    VCLEARF2            NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
*                                                                               
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
*                                                                               
* SUBROUTINE SETS A COMMA AFTER LAST DATA CHAR *                                
*                                                                               
SETCOMMA CLI   0(R4),C' '                                                       
         BNE   *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         BR    RE                                                               
*                                                                               
*              LOCAL XIT/ERROR ROUTINES                                         
*                                                                               
CANTPUSH MVI   GERROR1,ERNOPUSH    PUSH ERROR - TOO MANY NEST LEVELS            
         MVI   GETMSYS,23                                                       
         B     RETCURS                                                          
PFERR    MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         MVI   GETMSYS,23                                                       
RETCURS  LR    R2,RA                                                            
         AH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         GOTO1 MYERR                                                            
*                                                                               
PLSENTER MVI   GERROR1,2           PLEASE ENTER FIELDS AS REQUIRED              
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         L     R2,AFRSTKEY         R2 TO 1ST KEY FIELD                          
         L     RE,ATIOB            RE=A(TRANSLATOR I/O BLOCK)                   
         NI    TIOBINDS-TIOBD(RE),X'FF'-TIOBALRM   TURN OFF BEEP                
         GOTO1 MYERR                                                            
*                                                                               
DUMMYERR MVI   GOAGAIN,C'Y'        SET TO RETURN WITH NEW RECORD/ACTION         
DUMYERR1 LA    R2,CONRECH          CURSOR TO RECORD FIELD                       
         GOTO1 ERREX2                                                           
*                                                                               
SELFIRST MVI   GERROR1,10          SELECT OR HIT ENTER FOR FIRST                
         B     *+8                                                              
SELNEXT  MVI   GERROR1,9           SELECT OR HIT ENTER FOR NEXT                 
         MVI   GMSGTYPE,C'I'       INFORMATIONAL MESSAGE                        
         MVI   OKNO,0              CLEAR OKNO SO WON'T LOOP ENDLESSLY           
         L     R2,AFRSTREC         R2 TO 1ST SEL FIELD                          
         GOTO1 MYERR                                                            
*                                                                               
YES      SR    RC,RC               SET CC EQ                                    
NO       LTR   RC,RC               SET CC NEQ                                   
EXIT     EQU   *                                                                
XIT      XIT1                                                                   
*                                                                               
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*                                                                               
PROFI2Z  DS    CL16                I2z PROFILE                                  
SVMEDIA  DS    C                                                                
*                                                                               
PROFINFO DS    XL36                                                             
SKIPFLAG DS    X                                                                
*                                                                               
*                                                                               
***********************************************************************         
* PROVIDE MSPACK ENTRY POINT FOR LINKAGE TO STAPACK                             
***********************************************************************         
GOMSPACK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
*        L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'P'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKT)                                   
         MVC   STAPQMKT,0(RE)                                                   
         L     RE,4(R5)            GET A(STA)                                   
         MVC   STAPQSTA(8),0(RE)                                                
         GOTO1 STAPACK,(R4)                                                     
         CLI   STAPERR,0                                                        
         BNE   MSPKNO                                                           
         L     RE,8(R5)            GET A(MKTSTA)                                
         MVC   0(5,RE),STAPMKST                                                 
*                                                                               
         MVC   BNTWK,STAPSTA+2                                                  
         CLI   SVAPROF+7,C'C'      CANADIAN?                                    
         BE    *+8                                                              
         NI    BNTWK,X'7F'                                                      
         DROP  R4                                                               
*                                                                               
MSPKYES  SR    RC,RC               SET CC EQ                                    
MSPKNO   LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
***********************************************************************         
* PROVIDE MSUNPK ENTRY POINT FOR LINKAGE TO STAPACK                             
***********************************************************************         
GOMSUNPK NTR1  BASE=SYSRB,WORK=(R4,8)                                           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R7,BASER7                                                        
*        L     R8,ASPOOLD                                                       
         LR    R5,R1               SAVE CALLERS R1                              
*                                                                               
         USING STAPACKD,R4                                                      
         XC    0(32,R4),0(R4)                                                   
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPACOM,ACOMFACS                                                
         L     RE,0(R5)            GET A(MKTSTA)                                
         MVC   STAPMKST,0(RE)                                                   
*                                                                               
         GOTO1 STAPACK,(R4)                                                     
         CLI   STAPERR,0                                                        
         BNE   MSUPNO                                                           
*                                                                               
         L     RE,4(R5)            GET A(MKT)                                   
         MVC   0(4,RE),STAPQMKT                                                 
         L     RE,8(R5)            GET A(STA)                                   
         MVC   0(5,RE),STAPQSTA    ALWAYS MOVE 5 STATION BYTES                  
         TM    0(R5),X'80'         DOES USER WANT 8 BYTES                       
         BZ    *+10                                                             
         MVC   0(8,RE),STAPQSTA                                                 
         DROP  R4                                                               
*                                                                               
MSUPYES  SR    RC,RC               SET CC EQ                                    
MSUPNO   LTR   RC,RC                                                            
         XIT1                                                                   
*                                                                               
*                                                                               
RELO     DS    A                                                                
*                                                                               
NOINVNUM EQU   1152      INVOICE CAN'T BE SELECTED: NO INVOICE NUMBER           
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(MEDGET)                                                        
         DC    V(RECUP)                                                         
         DC    V(BINSR24)                                                       
         DC    V(DPTRD)                                                         
         DC    V(GETBROAD)                                                      
         DC    V(TWABLD)                                                        
         DC    V(TIMPK)                                                         
         DC    V(TIMUNPK)                                                       
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QQSORT)                                                      
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(QMINIO)                                                      
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QSPGETBU)                                                    
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
*              DIRECTORY OF RECORDS AND ACTIONS                                 
*                                                                               
RECACT   DS    0CL14                                                            
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                  CL2 ACCESS BITS (SPARE HERE)                 
*                                                                               
RECACT1  DC    X'01',C'INVOICE ',AL1(01),X'0000',AL2(0)                         
         DC    X'01',C'DETAIL  ',AL1(02),X'0000',AL2(0)                         
*                                                                               
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                  CL2 ACCESS BITS (ON = RESTRICTED)            
*                                                                               
RECACT2  DC    X'02',C'ADD     ',AL1(01,01,00),X'8000'                          
         DC    X'02',C'CHANGE  ',AL1(02,01,00),X'8000'                          
         DC    X'02',C'DISPLAY ',AL1(03,01,00),X'8000'                          
         DC    X'02',C'DELETE  ',AL1(04,01,00),X'8000'                          
         DC    X'02',C'RESTORE ',AL1(06,01,00),X'8000'                          
         DC    X'02',C'UPDATE  ',AL1(07,07,00),X'8000'                          
         DC    X'02',C'LIST    ',AL1(10,10,00),X'8000'                          
*        DC    X'02',C'REPORT  ',AL1(12,12,00),AL2(0)                           
         DC    X'02',C'NET     ',AL1(13,13,00),X'8000'                          
         DC    X'02',C'MOVE    ',AL1(14,14,00),X'8000'                          
         DC    X'02',C'REQUEST ',AL1(15,15,00),AL2(0)                           
         DC    X'02',C'COPY    ',AL1(16,16,00),X'8000'                          
         DC    X'02',C'FIXFILMS',AL1(17,17,00),X'8000'                          
         DC    X'02',C'SPLIT   ',AL1(18,18,00),X'8000'                          
*                                                                               
*              DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                   
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                      01=USER MAINTENANCE                      
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                  CL2 ACCESS BITS (SPARE HERE)                 
*                                                                               
RECACT3  DS    0C                                                               
         DC    X'03',AL1(01,01),X'FD02000081',C'    '  INVOICE  MAINT           
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,10),X'FE01000080',C'    '  INVOICE  LIST            
         DC    AL2(0)                                                           
*        DC    X'03',AL1(01,12),X'F906000068',C'SPNV'  INVOICE  REPORT          
*        DC    AL2(0)                                                           
         DC    X'03',AL1(01,13),X'FB04000081',C'    '  INVOICE  NET             
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,14),X'FA05000081',C'    '  INVOICE  MOVE            
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,15),X'F906000081',C'    '  INVOICE  REQUEST         
         DC    AL2(0)                                                           
         DC    X'03',AL1(01,16),X'FA05000081',C'    '  INVOICE  COPY            
         DC    AL2(0)                                                           
*                                                                               
         DC    X'03',AL1(02,07),X'FC03000080',C'    '  DETAIL   UPDATE          
         DC    AL2(0)                                                           
         DC    X'03',AL1(02,17),X'FC07000081',C'    '  DETAIL   FIXFILM         
         DC    AL2(0)                                                           
         DC    X'03',AL1(02,18),X'FC08000081',C'    '  DETAIL   SPLIT           
         DC    AL2(0)                                                           
*                                                                               
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
*                                                                               
CALLOFCR NTR1  BASE=*,LABEL=*,WORK=(R4,8)                                       
*                                                                               
         L     RA,ATWA                                                          
         USING T210FFD,RA                                                       
*                                                                               
         OC    T210FFD+6(2),T210FFD+6   TEST ANY LIMIT ACCESS                   
         JZ    COX                      NO                                      
*                                                                               
         XC    0(64,R4),0(R4)                                                   
         USING OFFICED,R4                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         MVC   OFCAUTH,T210FFD+6                                                
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,CLTOFFIC                                                  
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMD                                                  
         MVC   OFCLMT(4),T210FFD+6                                              
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCACCSM(3),SVMACCS    ACCESS LIST FROM MKTREC                   
         MVC   OFCSECD,ASECBLK2                                                 
*                                                                               
         GOTO1 OFFICER,DMCB,(C'N',(R4)),ACOMFACS                                
         CLI   0(R1),0                                                          
         JE    COX                                                              
*                                                                               
         MVI   GERROR1,SECLOCK                                                  
         J     VMYERR                                                           
*                                                                               
COX      XIT1                                                                   
*                                                                               
         DROP  R4                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* THIS ROUTINE FINDS THE EBCDIC EQUIVALENT OF A BINARY PRODUCT CODE             
*                                                                               
* ON ENTRY:    PARAM1   BYTE 0     BINARY PRODUCT CODE                          
*              SVCLIST             CLIENT'S PRODUCT LIST                        
*                                                                               
* ON EXIT:     PARAM1   BYTE 1-3   EBCDIC PRODUCT                               
***********************************************************************         
FINDQPRD NTR1  BASE=*,LABEL=*                                                   
         LA    R1,SVCLIST                                                       
*                                                                               
FQPRD10  CLI   0(R1),0             ANY MORE PRODUCTS?                           
         BE    FQPRDNO             NO                                           
*                                                                               
         CLC   3(L'BPRD,R1),DMCB   BINARY PRODUCT CODE MATCH?                   
         BE    *+12                                                             
         LA    R1,4(R1)            NO, CHECK NEXT PRODUCT                       
         B     FQPRD10                                                          
*                                                                               
         MVC   DMCB+1(L'QPRD),0(R1)  YES, COPY THE EBCDIC PRODUCT               
*                                                                               
FQPRDYES J     YES                                                              
FQPRDNO  J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
GENI2REQ NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO3                                                          
         USING RCRDD,R6                                                         
*                                                                               
         XC    RCRDCTL,RCRDCTL                                                  
         MVI   RCRDAREA,C' '                                                    
         MVC   RCRDAREA+1(L'RCRDAREA+L'RCRDREC2),RCRDAREA                       
*                                                                               
         MVC   RCRDCODE,=C'I2'                                                  
*                                                                               
         MVC   RCRDAGY,AGENCY                                                   
         MVC   RCRDMED,QMED                                                     
*                                                                               
         MVC   RCRDCLT,QCLT                                                     
         OC    RCRDCLT,SPACES                                                   
*                                                                               
* PRODUCT HERE                                                                  
         MVC   RCRDPRD,WORK                                                     
*                                                                               
* ESTIMATE HERE                                                                 
         MVC   RCRDEST,WORK+6                                                   
*                                                                               
         EDIT  (B2,BMKT),(4,RCRDMKT),FILL=0                                     
*                                                                               
         MVC   RCRDSTA,QSTA        STATION                                      
*                                                                               
         MVC   RCRDAREA+56(1),QSUBMED SUBMEDIA PASSED IN QCOMPARE               
*                                                                               
         MVC   WORK(L'SNVKMOS),MINBLOCK+MINMKEY-MINBLKD+SNVKMOS-SNVKEY          
         XC    WORK(L'SNVKMOS),=X'FFFF'                                         
         GOTO1 DATCON,DMCB,(2,WORK),(0,WORK+2)                                  
         MVC   RCRDSDAT(4),WORK+2  ONLY YYMM                                    
*                                                                               
         MVC   RCRDRQTR(10),=CL10'NINV I2'                                      
*                                                                               
         BRAS  RE,CKEXCP                                                        
         BNE   GENI2R50                                                         
*                                                                               
* EXCEPTION NETWORK HERE                                                        
* FORCE CALENDAR MONTH                                                          
         MVI   RCRDCONT,C'*'       2ND REQ CARD PRESENT                         
         MVI   RCRDMOSO,C'C'       CALENDAR MONTH                               
         OI    RCRDCTL+15,X'12'    SET 2 CARDS                                  
*                                                                               
GENI2R50 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',RCRDCTL,RCRDCTL               
         J     YES                                                              
*                                                                               
         DROP  R6                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* CLEAR XA9 BUFFER AREA                                                         
CLRPTAB  NTR1  BASE=*,LABEL=*                                                   
         L     RE,AXA9BUFF                                                      
         LHI   RF,9*255                                                         
         SAM31                                                                  
         XCEFL                                                                  
         SAM24                                                                  
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
BLDPTAB  NTR1  BASE=*,LABEL=*                                                   
* FIRST FIND OUT IF P1, P2, EST ARE IN HEADER OR DETAILS                        
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVHDELQ    LOOK FOR X'10'(HEADER) ELEMENT               
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                HEADER MUST BE THERE                         
*                                                                               
         XC    SVHDRPPE,SVHDRPPE                                                
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVHDELD,R6                                                      
*                                                                               
         CLC   SNVHDAP1,SPACES                                                  
         BNH   *+14              NO ALPHA PRODUCT, CHECK 1 BYTE BINARY          
         MVC   SVHDRPPE(L'SNVHDAP1),SNVHDAP1                                    
         B     BLDP20              P1 DONE, GET 2ND PRODUCT                     
*                                                                               
         CLI   SNVHDPRD,X'00'                                                   
         BE    BLDP150 NO PRODUCT IN HEADER: P1,P2,EST ARE IN DETAILS           
*                                                                               
         GOTO1 FINDQPRD,DMCB,(SNVHDPRD,0)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVHDRPPE(3),DMCB+1                                               
*                                                                               
* CHECK PIGGYBACK PRODUCT                                                       
BLDP20   DS    0H                                                               
         CLC   SNVHDAP2,SPACES                                                  
         BNH   *+14              NO ALPHA PRODUCT, CHECK 1 BYTE BINARY          
         MVC   SVHDRPPE+3(L'SNVHDAP2),SNVHDAP2                                  
         B     BLDP30              P2 DONE, GET ESTIMATE                        
*                                                                               
         CLI   SNVHDPR2,X'00'                                                   
         BE    BLDP30              NO PIGGYBACK PRODUCT, CHECK ESTIMATE         
*                                                                               
         GOTO1 FINDQPRD,DMCB,(SNVHDPR2,0)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVHDRPPE+3(3),DMCB+1                                             
*                                                                               
BLDP30   DS    0H                                                               
         CLI   SNVHDEST,0                                                       
         BE    BLDP50                                                           
         EDIT  (B1,SNVHDEST),(3,SVHDRPPE+6),FILL=0,WRK=WORK+9                   
         B     BLDP100                                                          
*                                                                               
         DROP  R6                  SNVHDELD                                     
*                                                                               
BLDP50   DS    0H                                                               
* SEE IF ESTIMATE IS IN DETAILS                                                 
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVDTELQ    LOOK FOR X'20'(DTL ORDER) ELEMENT            
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    *+6                 HEADER MUST BE THERE                         
         DC    H'0'                                                             
*                                                                               
         USING SNVDTELD,R6                                                      
         LLC   R0,SNVDTLEN                                                      
         SHI   R0,SNVDTOVQ                                                      
         LA    R1,SNVDTFLD                                                      
*                                                                               
BLDP60   CLI   0(R1),FLDNESTM                                                   
         BE    BLDP100                                                          
         LA    R1,1(R1)                                                         
         BCT   R0,BLDP60                                                        
         MVC   SVHDRPPE+6(3),=C'ALL'                                            
         DROP  R6                                                               
*                                                                               
BLDP100  DS    0H                                                               
         CLC   SVHDRPPE(3),SPACES  PRODUCT IN HEADER?                           
         BNH   BLDP150             NO - READ ALL DETAILS                        
         CLC   SVHDRPPE+6(3),SPACES EST IN HEADER?                              
         BNH   BLDP150             NO - READ ALL DETAILS                        
*                                                                               
* P1, P2, ARE IN HEADER AT THIS POINT                                           
* ESTIMATE IS EITHER IN HEADER OR MISSING                                       
         MVC   WORK(9),SVHDRPPE                                                 
         OC    WORK(9),SPACES                                                   
         BRAS  RE,ADDP1P2E                                                      
         J     YES                                                              
*                                                                               
* P1/ESTIMATE ARE IN DETAILS                                                    
* READ ALL DETAIL ELEMENTS HERE                                                 
BLDP150  DS    0H                                                               
         LA    R5,MINBLOCK                                                      
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,SNVIDELQ    LOOK FOR X'40' ELEMENTS                      
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0                                                         
         BE    BLDP211                                                          
         CLI   MINERR,MINEEOF      NOT FOUND?                                   
         BE    BLDP250                                                          
         DC    H'0'                                                             
*                                                                               
BLDP210  DS    0H                                                               
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0                                                         
         BE    BLDP211                                                          
         CLI   MINERR,MINEEOF      NOT FOUND?                                   
         BE    BLDP250                                                          
         DC    H'0'                                                             
*                                                                               
BLDP211  DS    0H                                                               
         MVC   WORK(9),SVHDRPPE                                                 
         OC    WORK(9),SPACES                                                   
*                                                                               
         L     R6,MINELEM                                                       
         USING SNVIDELD,R6                                                      
*                                                                               
         CLI   SNVIDEL,SNVIDELQ                                                 
         BNE   BLDP250                                                          
*                                                                               
         CLC   SVHDRPPE(3),SPACES  PRD IN HEADER?                               
         BH    BLDP230             YES - DO ESTIMATE                            
*                                                                               
         CLC   SNVIDAP1,SPACES                                                  
         BNH   *+14                                                             
         MVC   WORK(3),SNVIDAP1                                                 
         B     BLDP230                                                          
         GOTO1 FINDQPRD,DMCB,(SNVIDPRD,0)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(3),DMCB+1                                                   
*                                                                               
         CLC   SNVIDAP2,SPACES                                                  
         BNH   BLDP230                                                          
         MVC   WORK+3(3),SNVIDAP2                                               
         B     BLDP230                                                          
         GOTO1 FINDQPRD,DMCB,(SNVIDPR2,0)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+3(3),DMCB+1                                                 
*                                                                               
BLDP230  DS    0H                                                               
         CLC   SVHDRPPE+6(3),SPACES                                             
         BH    BLDP240                                                          
         CLI   SNVIDEST,0                                                       
         BE    BLDP240                                                          
         EDIT  (B1,SNVIDEST),(3,WORK+6),FILL=0,WRK=WORK+9                       
*                                                                               
BLDP240  DS    0H                                                               
         BRAS  RE,ADDP1P2E                                                      
         B     BLDP210                                                          
*                                                                               
BLDP250  DS    0H                  NO MORE '40' ELEMENTS                        
         J     YES                                                              
         LTORG                                                                  
*                                                                               
SVHDRPPE DS    CL9   HEADER P1, P2, EST                                         
*                                                                               
*                                                                               
*                                                                               
INITBSP  NTR1  BASE=*,LABEL=*                                                   
         LARL  R1,BSPARS                                                        
         XC    0(L'BSPARS,R1),0(R1)                                             
*                                                                               
* P1 - A(RECORD)                                                                
         LA    RF,WORK                                                          
         ST    RF,0(R1)                                                         
         LA    R1,4(R1)            POINT TO NEXT PARAMETER                      
* P2 - A(TABLE)                                                                 
         MVC   0(4,R1),AXA9BUFF                                                 
         LA    R1,4(R1)            POINT TO NEXT PARAMETER                      
* P3 - NUMBER OF RECORDS IN TABLE SO FAR, NOT INITIALIZED                       
         LA    R1,4(R1)            POINT TO NEXT PARAMETER                      
* P4 - L'RECORD                                                                 
         MVI   3(R1),9                                                          
         LA    R1,4(R1)            POINT TO NEXT PARAMETER                      
* P5 - L'KEY                                                                    
         MVI   3(R1),9                                                          
         LA    R1,4(R1)            POINT TO NEXT PARAMETER                      
* P6 - MAXIMUM NUMBER OF RECORDS IN THE TABLE                                   
         MVI   3(R1),255           SET AT 255 FOR NOW                           
*                                                                               
         J     YES                                                              
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* ADD 1 RECORD TO GENI2'S P1-P2-EST TABLE                                       
* RECORD EXPECTED TO BE BUILT IN WORK                                           
ADDP1P2E NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CKEXCP           CHECK IF EXCEPTION STATION                   
         BNE   *+10                                                             
         MVC   WORK(6),=C'POL   '  POL REQUEST FOR EXCP STATIONS                
*                                                                               
         MVI   BP4,X'01'           ADD RECORD IF NOT FOUND                      
         SAM31                                                                  
         GOTO1 =V(BINSR31),BSPARS,WORK,RR=RELO                                  
         SAM24                                                                  
         OC    BP1,BP1             TEST TABLE FULL                              
         JNZ   YES                                                              
         DC    H'0'                TABLE FULL                                   
         LTORG                                                                  
*                                                                               
         DS    0F                                                               
BSPARS   DS    0XL32               BINSEARCH PARAMETERS                         
BP1      DC    F'0'                                                             
BP2      DC    F'0'                                                             
BP3      DC    F'0'                                                             
BP4      DC    F'0'                                                             
BP5      DC    F'0'                                                             
BP6      DC    F'0'                                                             
BP7      DC    F'0'                                                             
BP8      DC    F'0'                                                             
*                                                                               
*                                                                               
*                                                                               
* CHECK IF SPECIAL I2 FOR H7 AND FR CUSTENH-2987                                
CHKIFI2  NTR1  BASE=*,LABEL=*                                                   
         CLI   NETPAKSW,C'Y'       NETPAK?                                      
         JNE   NO                                                               
         CLC   =AL2(AGYALPH_SJ),AGENCY          'SJ'                            
         JE    YES                                                              
         CLC   =AL2(AGYALPH_STAR_B),AGENCY      '*B'                            
         JE    YES                                                              
         CLC   =AL2(AGYALPH_H7),AGENCY          'H7'                            
         JE    YES                                                              
         CLC   =AL2(AGYALPH_FR),AGENCY          'FR'                            
         JE    YES                                                              
         J     NO                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* CHECK IF EXCEPTION NETWORK                                                    
CKEXCP   NTR1  BASE=*,LABEL=*                                                   
         LA    R0,NETABLN                                                       
         LA    R1,NETABLE                                                       
*                                                                               
CKEXCP10 CLC   QSTA(4),0(R1)                                                    
         JE    YES                 EXCEPTION STATION                            
         LA    R1,4(,R1)                                                        
         BCT   R0,CKEXCP10                                                      
         J     NO                  EOT, STATION NOT AN EXCEPTION                
*                                                                               
NETABLE  DC    C'ABC CBS FOX NBC CWTV'                                          
NETABLN  EQU   (*-NETABLE)/4                                                    
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
       ++INCLUDE SPSNVWORKD                                                     
*                                                                               
       ++INCLUDE SPGENSNV                                                       
*                                                                               
       ++INCLUDE SPSNVFED                                                       
       ++INCLUDE SPSNVFFD                                                       
         ORG   CONHEADH+6000       SPECIAL SPOT FOR SECBLK                      
SECBLK   DS    CL1024                                                           
*                                                                               
* DDGENTWA                                                                      
* CTGENFILE                                                                     
* DDOFFICED                                                                     
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
* DDACTIVD                                                                      
* FAGETTXTD                                                                     
* FATIOB                                                                        
* FAFACTS                                                                       
* FASECRETD                                                                     
* DDMINBLK                                                                      
* SEACSFILE                                                                     
* SPGENAGY                                                                      
* SPGENCLT                                                                      
* SPGENPRD                                                                      
* SPGENEST                                                                      
* SPGENSTA                                                                      
* SPGENMKT                                                                      
* SPSTABLK                                                                      
* SPSTAPACKD                                                                    
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE SEACSFILE                                                      
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
*                                                                               
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
*                                                                               
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
*                                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE SPSNVRCRD                                                      
       ++INCLUDE SPSNVFEQTB                                                     
       ++INCLUDE DDAGYALPH                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'071SPSNV00   10/31/16'                                      
         END                                                                    
