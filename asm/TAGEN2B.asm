*          DATA SET TAGEN2B    AT LEVEL 039 AS OF 06/27/12                      
*PHASE T7022BA                                                                  
         TITLE 'T7022B - LOCAL MAINTENANCE'                                     
T7022B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7022B                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
                                                                                
         TM    TGSYSTAT,TASYSPID   CHANGE SCREEN IF SHOWING PID                 
         BZ    LOC02                                                            
         MVC   SLOHED1(10),=C'Pid Number'                                       
         OI    SLOHED1H+6,X'80'                                                 
         MVC   SLOHED2(14),=C'2nd Pid Number'                                   
         OI    SLOHED2H+6,X'80'                                                 
                                                                                
LOC02    CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   LOC10                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
                                                                                
         SPACE 3                                                                
LOC10    CLI   THISLSEL,C'D'       IF DELETING FROM A LIST                      
         BE    *+12                DON'T DISPLAY TILL XRECDEL                   
         CLI   MODE,DISPREC                                                     
         BE    LOC15                                                            
         CLI   MODE,XRECADD                                                     
         BE    LOC14               IF MODE IS NEW RECORD ADDED                  
         CLI   MODE,XRECDEL                                                     
         BE    LOC14               OR RECORD DELETED                            
         CLI   MODE,XRECREST                                                    
         BE    LOC14               OR RESTORED                                  
         CLI   MODE,XRECPUT        OR CHANGED                                   
         BNE   LOC20                                                            
LOC14    BRAS  RE,NFYVIT           NOTIFY VITA OF ACTION                        
         SPACE                                                                  
LOC15    BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     XIT                                                              
         SPACE 3                                                                
LOC20    CLI   MODE,VALREC                                                      
         BNE   LOC30               IF MODE IS VALIDATE RECORD                   
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
         SPACE 3                                                                
         USING TLLOD,R3                                                         
LOC30    CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   XIT                                                              
         L     R3,AIO                                                           
         MVC   SLOUN,TLLOUN        UNION                                        
         OI    SLOUNH+6,X'80'                                                   
         MVC   SLOLCL,TLLOLCL      LOCAL                                        
         OI    SLOLCLH+6,X'80'                                                  
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE VALIDATES KEY                                            
                                                                                
VKEY     NTR1                                                                   
         LA    R2,SLOUNH                                                        
         CLI   5(R2),0             IF NO UNION INPUT                            
         BNE   VK2                                                              
         CLC   TGUNI,SPACES                                                     
         BNH   FLDMISS             MUST HAVE GLOBAL UNION                       
         MVC   SLOUN,TGUNI                                                      
         OI    6(R2),X'80'                                                      
         B     VK3                                                              
                                                                                
VK2      MVC   FULL(L'SLOUN),SLOUN                                              
         OC    FULL(L'SLOUN),SPACES  PAD UNION INPUT WITH SPACES                
         GOTO1 UNIVAL,DMCB,FULL      VALIDATE UNION                             
         BNE   FLDINV                                                           
                                                                                
VK3      GOTO1 RECVAL,DMCB,TLLOCDQ,(X'40',SLOLCLH) BUILD THE KEY                
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
                                                                                
DISPLAY  NTR1                                                                   
         TWAXC SLONAMEH                                                         
         XC    SLOSSNN,SLOSSNN     CLEAR SS NAMES                               
         OI    SLOSSNNH+6,X'80'                                                 
         XC    SLOSS2N,SLOSS2N                                                  
         OI    SLOSS2NH+6,X'80'                                                 
                                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,SLONAMEH        NAME                        
         GOTO1 (RF),(R1),TAADELQ,(4,SLOADDRH)       ADDRESS                     
         GOTO1 (RF),(R1),TANUELQ,SLOACCH,TANUTLOC   ACCOUNT NUMBER              
         GOTO1 (RF),(R1),TANUELQ,SLOSS2H,TANUT2ND   2ND SS NUMBER               
                                                                                
         USING TALOD,R4                                                         
         MVI   ELCODE,TALOELQ      LOCAL ELEMENT                                
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   DR08                                                             
         TM    TALOSTAT,TALOSUCP   IF UNION COPIES REQUIRED WITH CHECKS         
         BZ    *+8                                                              
         MVI   SLOUCPY,C'Y'        MOVE Y TO SCREEN                             
                                                                                
         TM    TALOSTAT,TALOSHNW   IF FUND STATUS BIT ON                        
         BZ    *+8                                                              
         MVI   SLOFUND,C'Y'        MOVE Y TO SCREEN                             
                                                                                
         EDIT  (1,TALONCPY),(3,SLOCPY),ALIGN=LEFT                               
                                                                                
         OC    TALOSSN,TALOSSN     IF THERE'S SS NUMBER                         
         BZ    DR02                                                             
         MVC   SLOSSN,TALOSSN                                                   
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',SLOSSN),SLOSSNNH GET NAME             
         MVC   AIO,AIO1            RESTORE AIO                                  
                                                                                
DR02     OC    SLOSS2,SLOSS2       IF THERE'S 2ND SS NUMBER                     
         BZ    DR04                                                             
         MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'8C',SLOSS2),SLOSS2NH GET NAME             
         MVC   AIO,AIO1            RESTORE AIO                                  
                                                                                
DR04     TM    TGSYSTAT,TASYSPID   CHANGE SS#'S TO PIDS NOW IF NEEDED           
         BZ    DR08                                                             
         OC    SLOSS2,SLOSS2                                                    
         BZ    DR06                                                             
         MVC   TGSSN,SLOSS2                                                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLOSS2,SPACES                                                    
         MVC   SLOSS2(L'TGPID),TGPID                                            
         MVI   SLOSS2H+5,6                                                      
         OI    SLOSS2H+6,X'80'                                                  
                                                                                
DR06     OC    SLOSSN,SLOSSN                                                    
         BZ    DR08                                                             
         MVC   TGSSN,SLOSSN                                                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLOSSN,SPACES                                                    
         MVC   SLOSSN(L'TGPID),TGPID                                            
         MVI   SLOSSNH+5,6                                                      
         OI    SLOSSNH+6,X'80'                                                  
                                                                                
DR08     GOTO1 ACTVOUT,DMCB,SLOLCHGH               LAST CHANGED                 
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE                                                                  
BLDREC   NTR1                                                                   
         CLI   ACTNUM,ACTADD       IF NOT ADDING                                
         BE    BLD00               SAVE VITA-NOTIFYING MQ MESSAGE               
         GOTOR BLDMQMSG,DMCB,(X'80',0)     BASED ON INITIAL STATE               
                                                                                
BLD00    GOTO1 NAMIN,DMCB,TANAELQ,SLONAMEH  NAME                                
         GOTO1 (RF),(R1),TANUELQ,(X'80',SLOACCH),TANUTLOC ACCOUNT NUM           
         GOTO1 ADDRIN,DMCB,SLOADDRH ADDRESS                                     
                                                                                
         MVI   ELCODE,TALOELQ                                                   
         GOTO1 REMELEM             DELETE CURRENT LOCAL ELEMENT                 
                                                                                
         USING TALOD,R4                                                         
         XC    ELEMENT,ELEMENT     BUILD NEW LOCAL ELEMENT                      
         LA    R4,ELEMENT                                                       
                                                                                
         LA    R2,SLOFUNDH         IF THERE IS FUND INPUT                       
         CLI   5(R2),0                                                          
         BE    BLD14                                                            
                                                                                
         CLC   SLOUN,=C'AFM'       TEST UNION AFM                               
         BE    BLD02                                                            
         CLC   SLOUN,=C'DGA'       OR DGA                                       
         BE    BLD02                                                            
         CLC   SLOUN,=C'WGA'       OR WGA                                       
         BE    BLD02                                                            
         CLC   SLOUN,=C'ACT'       OR ACT                                       
         BE    BLD02                                                            
         CLC   SLOUN,=C'UDA'       OR UDA                                       
         BNE   NOINPUT                                                          
                                                                                
BLD02    CLI   8(R2),C'N'                                                       
         BE    BLD14               SKIP IF INPUT IS N                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV              INVALID IF INPUT NOT Y                       
                                                                                
         OI    TALOSTAT,TALOSHNW   TURN FUND STATUS BIT ON                      
         LA    R2,SLOSSNH                                                       
         CLI   5(R2),0             SS NUMBER IS REQUIRED                        
         BE    FLDMISS                                                          
         TM    TGSYSTAT,TASYSPID   USING PIDS?                                  
         BZ    BLD04               NO                                           
         CLI   SLOSSNH+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    BLD04               RECVAL CALL DOES NOT CHECK FOR               
         CLI   SLOSSNH+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SLOSSN        UNPACK IF PID ENTERED                        
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   BLD04                                                            
         MVC   SLOSSN,TGSSN                                                     
         MVI   SLOSSNH+5,9                                                      
                                                                                
BLD04    MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SLOSSNNH GET NAME               
         L     R4,AIO                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
                                                                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    BLD06               NO                                           
         MVC   TGSSN,SLOSSN                                                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLOSSN,SPACES                                                    
         MVC   SLOSSN(L'TGPID),TGPID                                            
         MVI   SLOSSNH+5,6                                                      
         OI    SLOSSNH+6,X'80'                                                  
                                                                                
         USING TAW4D,R4                                                         
BLD06    MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    *+12                                                             
         CLI   TAW4TYPE,TAW4TYTR                                                
         BNE   FLDINV              TYPE MUST BE CORPORATION OR TRUSTEE          
                                                                                
         USING TALOD,R4                                                         
         LA    R4,ELEMENT                                                       
         MVC   TALOSSN,TGSSN       SAVE SS NUMBER                               
                                                                                
         LA    R2,SLOSS2H          2ND SS NUMBER                                
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   BLD08                                                            
         CLC   SLOUN,=C'WGA'       REQUIRED FOR WGA                             
         BE    FLDMISS                                                          
         CLC   SLOUN,=C'ACT'       AND ACT                                      
         BE    FLDMISS                                                          
         B     BLD16                                                            
                                                                                
BLD08    CLC   SLOUN,=C'WGA'       ALLOWED ONLY FOR WGA                         
         BE    *+14                                                             
         CLC   SLOUN,=C'ACT'       AND ACT                                      
         BNE   NOINPUT                                                          
                                                                                
         TM    TGSYSTAT,TASYSPID   USING PIDS?                                  
         BZ    BLD10               NO                                           
         CLI   SLOSS2H+5,9         MUST CHECK FOR LENGTH HERE SINCE             
         BE    BLD10               RECVAL CALL DOES NOT CHECK FOR               
         CLI   SLOSS2H+5,6         INVALID ENTRIES DUE TO X'40' PARAM           
         BNE   FLDINV                                                           
         MVC   TGPID,SLOSS2        UNPACK IF PID ENTERED                        
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   BLD10                                                            
         MVC   SLOSS2,TGSSN                                                     
         MVI   SLOSS2H+5,9                                                      
                                                                                
BLD10    MVC   AIO,AIO2            MAKE SURE DON'T CREAM RECORD                 
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',(R2)),SLOSS2NH GET NAME               
         L     R4,AIO                                                           
         MVC   AIO,AIO1            RESTORE AIO                                  
                                                                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    BLD12               NO                                           
         MVC   TGSSN,SLOSS2                                                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SLOSS2,SPACES                                                    
         MVC   SLOSS2(L'TGPID),TGPID                                            
         MVI   SLOSS2H+5,6                                                      
         OI    SLOSS2H+6,X'80'                                                  
                                                                                
         USING TAW4D,R4                                                         
BLD12    MVI   ELCODE,TAW4ELQ      GET W4 DETAILS ELEMENT                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         CLI   TAW4TYPE,TAW4TYCO                                                
         BE    BLD16                                                            
         CLI   TAW4TYPE,TAW4TYTR                                                
         BE    BLD16                                                            
         B     FLDINV              TYPE MUST BE CORPORATION OR TRUSTEE          
                                                                                
BLD14    LA    R2,SLOSSNH          H&W FUND <> Y                                
         CLI   5(R2),0                                                          
         BNE   NOINPUT             ERROR IF THERE'S SS NUMBER                   
         LA    R2,SLOSS2H                                                       
         CLI   5(R2),0                                                          
         BNE   NOINPUT             ERROR IF THERE'S 2ND SS NUMBER               
                                                                                
         USING TALOD,R4                                                         
BLD16    LA    R4,ELEMENT                                                       
         LA    R2,SLOCPYH          N'UNION COPIES                               
         CLI   5(R2),0                                                          
         BE    BLD18               SKIP IF NO INPUT                             
         GOTO1 VALINUM                                                          
         MVC   TALONCPY,ACTUAL                                                  
                                                                                
BLD18    LA    R2,SLOUCPYH         UNION COPY REQUIRED?                         
         CLI   5(R2),0                                                          
         BE    BLD20               SKIP IF NO INPUT                             
         CLI   8(R2),C'N'                                                       
         BE    BLD20               SKIP IF INPUT IS N                           
         CLI   8(R2),C'Y'                                                       
         BNE   FLDINV              INVALID IF INPUT NOT Y                       
         OI    TALOSTAT,TALOSUCP   TURN UNION COPIES REQUIRED BIT ON            
                                                                                
BLD20    OC    ELEMENT,ELEMENT                                                  
         BZ    BLD24               DON'T BOTHER IF NOTHING IN ELEMENT           
                                                                                
         MVI   TALOEL,TALOELQ      ELEM CODE                                    
         MVI   TALOLEN,TALOLNQ     ELEM LENGTH                                  
                                                                                
         GOTO1 ADDELEM             ADD THE ELEMENT                              
                                                                                
         TM    TGSYSTAT,TASYSPID   ARE WE USING PID#                            
         BZ    BLD22               NO                                           
         MVC   TGPID,SLOSS2        UNPACK IF PID ENTERED                        
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   BLD22                                                            
         MVC   SLOSS2,TGSSN                                                     
         MVI   SLOSS2H+5,9                                                      
                                                                                
BLD22    GOTO1 NAMIN,DMCB,TANUELQ,(X'80',SLOSS2H),TANUT2ND  2ND SS NUM          
                                                                                
BLD24    GOTO1 ACTVIN,DMCB,SLOLCHGH  LAST CHANGED                               
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         BNE   BLDR15                                                           
         L     R3,AIO                                                           
         MVC   KEY,0(R3)           JUST RESTORE KEY                             
         B     XIT                                                              
                                                                                
BLDR15   MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLLOCDQ,(X'30',SLOLCLH) ELSE READ FOR UPDATE         
*                                                  TO ENABLE PUTREC             
         MVC   AIO,AIO1            RESTORE AIO                                  
         B     XIT                                                              
         EJECT                                                                  
*              LOCAL ERROR/EXIT ROUTINES                                        
         SPACE                                                                  
NOINPUT  MVI   ERROR,ERNOINP       NO INPUT ALLOWED                             
         B     ERRXIT                                                           
         SPACE                                                                  
FLDINV   MVI   ERROR,INVALID                                                    
         B     ERRXIT                                                           
         SPACE                                                                  
FLDMISS  MVI   ERROR,MISSING                                                    
         B     ERRXIT                                                           
         SPACE                                                                  
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         SPACE                                                                  
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'UNIONLCL',CL8'LIST'                                   
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING MQ MESSAGE                     *         
*        ON ENTRY ... P1 BYTE 0 = X'80' SAVE TO WSSVR BLOCK           *         
*                     AIO1      = A(LOCAL RECORD)                     *         
***********************************************************************         
                                                                                
BLDMQMSG NTR1  BASE=*,LABEL=*                                                   
         MVC   TGBYTE,0(R1)        TGBYTE = WSSVR SAVE STATUS                   
                                                                                
         LA    R2,BLOCK            R2=A(MQ MESSAGE BLOCK)                       
         BAS   RE,BLDIMMSG         BUILD INSERT/MODIFY MESSAGE                  
         JE    BMQ10                                                            
         BAS   RE,BLDDMSG          OR BUILD DELETE MESSAGE                      
         JNE   XIT                 LENGTH RETURNED IN R3                        
                                                                                
         USING FAWSSVRD,R1                                                      
BMQ10    TM    TGBYTE,X'80'        IF SAVING TO WSSVR                           
         JZ    BMQX                DO SO NOW                                    
         LA    R1,WORK                                                          
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSASVE                                                
         ST    R2,FAWSADR                                                       
         STH   R3,FAWSLEN                                                       
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    BMQX                                                             
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
BMQYES   XR    RC,RC                                                            
BMQNO    LTR   RC,RC                                                            
BMQX     XIT1  REGS=(R3)                                                        
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING INSERT/MODIFY MESSAGE          *         
*        ON ENTRY ... AIO1 = A(LOCAL RECORD)                          *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDIMMSG NTR1                                                                   
         CLI   MODE,XRECDEL        IF NOT DELETING RECORD                       
         JE    BMQNO                                                            
                                                                                
         USING IMMSGD,R2                                                        
         LHI   R3,IMMLNQ                                                        
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,IMMSG                                                         
         LR    R1,R3               COPY INSERT MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         CLI   ACTNUM,ACTADD       IF ACTION IS ADD                             
         JE    BIMM10                                                           
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         JNE   BIMM20                                                           
BIMM10   MVC   IMROT,=C'<acpinsert>'                                            
         MVC   IMINS,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMMOD,BMQFALSE                                                   
         MVC   IMROX,=C'</acpinsert>'                                           
         J     BIMM30                                                           
                                                                                
BIMM20   MVC   IMROT,=C'<acpmodify>'                                            
         MVC   IMINS,BMQFALSE      IF ACTION IS CHANGE                          
         MVC   IMMOD,BMQTRUE       SET INSERT AND MODIFY STATUS                 
         MVC   IMROX,=C'</acpmodify>'                                           
                                                                                
         USING TLLOD,R4                                                         
BIMM30   L     R4,AIO1                                                          
         MVC   IMUNI,TLLOUN        COPY UNION CODE INTO XML                     
         GOTO1 ELIMCHAR,DMCB,(L'IMUNI,IMUNI)                                    
         MVC   IMCOD,TLLOLCL       COPY LOCAL CODE INTO XML                     
         GOTO1 ELIMCHAR,DMCB,(L'IMCOD,IMCOD)                                    
         J     BMQYES                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
*        ROUTINE BUILDS VITA-NOTIFYING DELETE MESSAGE                 *         
*        ON ENTRY ... AIO1 = A(UNION LOCAL RECORD)                    *         
*                     R2   = A(MQ MESSAGE BLOCK)                      *         
***********************************************************************         
                                                                                
BLDDMSG  NTR1                                                                   
         CLI   MODE,XRECDEL        IF DELETING RECORD                           
         JNE   BMQNO                                                            
                                                                                
         USING DMSGD,R2                                                         
         LHI   R3,DMLNQ                                                         
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         LA    R0,DMSG                                                          
         LR    R1,R3               COPY DELETE MQ MESSAGE TEMPLATE              
         MVCL  RE,R0               INTO BLOCK                                   
                                                                                
         MVC   DUNI,SLOUN          COPY UNION CODE INTO XML                     
         GOTO1 ELIMCHAR,DMCB,(L'DUNI,DUNI)                                      
         MVC   DCOD,SLOLCL         COPY LOCAL CODE INTO XML                     
         GOTO1 ELIMCHAR,DMCB,(L'DCOD,DCOD)                                      
         J     BMQYES                                                           
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
BMQTRUE  DC    CL5'true'                                                        
BMQFALSE DC    CL5'false'                                                       
                                                                                
IMMSG    DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    CL11' '                                                          
         DC    C'<unionlocal isinsert="'                                        
         DC    CL5' '                                                           
         DC    C'" ismodify="'                                                  
         DC    CL5' '                                                           
         DC    C'" code="'                                                      
         DC    CL3' '                                                           
         DC    C' '                                                             
         DC    CL3' '                                                           
         DC    C'">'                                                            
         DC    C'</unionlocal>'                                                 
         DC    CL12' '                                                          
IMMLNQ   EQU   *-IMMSG                                                          
                                                                                
DMSG     DC    CL16' '                                                          
         DC    C'<?xml version="1.0" encoding="UTF-8"?>'                        
         DC    C'<acpdelete>'                                                   
         DC    C'<unionlocal isdelete="true" '                                  
         DC    C'code="'                                                        
         DC    CL3' '                                                           
         DC    C' '                                                             
         DC    CL3' '                                                           
         DC    C'">'                                                            
         DC    C'</unionlocal>'                                                 
         DC    C'</acpdelete>'                                                  
DMLNQ    EQU   *-DMSG                                                           
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE OUTPUTS MQ MESSAGE                                   *         
*        ON ENTRY ... AIO1 = A(UNION LOCAL RECORD)                    *         
***********************************************************************         
                                                                                
NFYVIT   NTR1  BASE=*,LABEL=*                                                   
         GOTOR BLDMQMSG,DMCB,0     BUILD UPDATED MQ MESSAGE                     
                                                                                
         USING FAWSSVRD,R1                                                      
         CLI   MODE,XRECPUT        IF JUST CHANGED RECORD                       
         JNE   NV10                                                             
         LA    R1,WORK                                                          
         XC    WORK(FAWSSVRL),WORK                                              
         MVC   FAWSTOKN,=CL4'INIT'                                              
         MVI   FAWSACTN,FAWSARST   RECALL INITIAL-STATE BASED                   
         MVC   FAWSADR,AIO3        MESSAGE INTO AIO3                            
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0                                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         DROP  R1                                                               
                                                                                
         LA    RE,BLOCK                                                         
         LR    RF,R3               IF CHANGE HAS BEEN MADE THAT                 
         L     R0,AIO3             WILL AFFECT THE MESSAGE                      
         LR    R1,R3               SEND THE UPDATED MESSAGE                     
         CLCL  RE,R0                                                            
         JE    XIT                                                              
                                                                                
NV10     GOTO1 NTFYVITA,DMCB,BLOCK,(R3),0                                       
         J     XIT                                                              
                                                                                
***********************************************************************         
*        CONSTANTS AND LITERALS                                       *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR2BD                                                       
         EJECT                                                                  
         ORG   SLOWORK                                                          
*                                                                               
SVLAGT   DS    XL2                 LAST AGENT                                   
SVKEY    DS    CL38                SAVED KEY                                    
         EJECT                                                                  
*                                                                               
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* FAWSSVRD                                                                      
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING INSERT/UPDATE MESSAGES           *         
***********************************************************************         
                                                                                
IMMSGD   DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
IMROT    DS    CL11                                                             
         DS    CL22                                                             
IMINS    DS    CL5                                                              
         DS    CL12                                                             
IMMOD    DS    CL5                                                              
         DS    CL8                                                              
IMUNI    DS    CL3                                                              
         DS    CL1                                                              
IMCOD    DS    CL3                                                              
         DS    CL2                                                              
         DS    CL13                                                             
IMROX    DS    CL12                                                             
                                                                                
***********************************************************************         
*        DSECT COVERS VITA-NOTIFYING DELETE MESSAGES                  *         
***********************************************************************         
                                                                                
DMSGD    DSECT                                                                  
         DS    CL16                                                             
         DS    CL38                                                             
         DS    CL11                                                             
         DS    CL28                                                             
         DS    CL6                                                              
DUNI     DS    CL3                                                              
         DS    CL1                                                              
DCOD     DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039TAGEN2B   06/27/12'                                      
         END                                                                    
