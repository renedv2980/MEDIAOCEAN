*          DATA SET SPOMS12    AT LEVEL 001 AS OF 08/19/13                      
*PHASE T23412A                                                                  
T23412   TITLE 'SPOMS12 - ORDER PSEND'                                          
T23412   CSECT                                                                  
         PRINT NOGEN                                                            
BGN      NMOD1 0,*T23412*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)                   STANDARD CODING                       
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA             BASE SCREEN + OUR SCREEN              
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R5,SYSSPARE                R5=A(LOCAL SAVED STORAGE)             
         USING LSSD,R5                                                          
*                                                                               
         LA    R2,CONACTH                 CURSOR TO ACTION FIELD                
         CLI   CALLSP,0                   DID WE PFKEY INTO PSTATUS?            
         BE    INVLFLD                    NO - ERROR                            
*                                                                               
         ST    R3,RELO                    RELO                                  
         ST    RC,BASERC                  BASE RC                               
         BAS   RE,GETPF                   GET PFKEYS                            
*                                                                               
         L     RF,ACOMFACS                A(COMFACS)                            
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
*                                                                               
         CLI   MODE,DISPREC               DISPLAY RECORD?                       
         BE    DR                         YES                                   
         CLI   MODE,VALREC                VALIDATE RECORD?                      
         BE    VR                         YES                                   
         CLI   MODE,XRECPUT               VALIDATE RECORD?                      
         BE    XRPUT                      YES                                   
*                                                                               
XIT      XIT1                                                                   
***********************************************************************         
* DISPLAY THE RECORD                                                  *         
***********************************************************************         
DR       OI    OPSMEDH+1,X'20'            PROTECT                               
         OI    OPSORDRH+1,X'20'           PROTECT                               
         OI    OPSCLTH+1,X'20'            PROTECT                               
         OI    OPSPRDH+1,X'20'            PROTECT                               
         OI    OPSESTH+1,X'20'            PROTECT                               
         OI    OPSFLTH+1,X'20'            PROTECT                               
         OI    OPSMKTH+1,X'20'            PROTECT                               
         OI    OPSSTAH+1,X'20'            PROTECT                               
*                                                                               
         LA    R2,OPSTDOLH                TOTAL DOLLAR                          
         MVI   5(R2),L'OPSTDOL            TOTAL DOLLAR LENGTH                   
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVSPID                             
         CLI   8(R1),0                    HAVE TOTAL DOLLAR?                    
         JNE   NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         LA    R2,OPSTSPTH                TOTAL SPOTS                           
         MVI   5(R2),L'OPSTSPT            TOTAL SPOTS LENGTH                    
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVDQU                              
         CLI   8(R1),0                    HAVE TOTAL SPOTS?                     
         JNE   NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         LA    R2,OPSCSTAH                STATUS                                
         MVI   5(R2),L'OPSCSTA            STATUS LENGTH                         
         GOTO1 VGLOBBER,DMCB,=C'GETF',(R2),,GLVPGM                              
         CLI   8(R1),0                    HAVE STATUS?                          
         JNE   NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'03'               SUPPLEMENTARY ID ELEMENT              
         BAS   RE,GETEL                   HAVE AN X'03' ELEMENT?                
         BNE   DR10                       NO                                    
*                                                                               
         USING  DOSPELD,R6                SUPPLEMENTARY ID ELEM DSECT           
         EDIT  (B2,DOSPVER#),(3,OPSCSND),FILL=0                                 
         OI    OPSCSNDH+6,X'80'           TRANSMIT                              
         EDIT  (B1,DOSPREVN),(3,OPSCREV),FILL=0                                 
         OI    OPSCREVH+6,X'80'           TRANSMIT                              
*                                                                               
         CLI   DOSPDEST,C'R'              DESTINATION REP?                      
         BNE   *+10                       NO                                    
         MVC   OPSCDST(3),=C'REP'         YES - MOVE REP TO SCREEN              
         CLI   DOSPDEST,C'S'              DESTINATION STA?                      
         BNE   *+10                       NO                                    
         MVC   OPSCDST(3),=C'STA'         YES - MOVE STA TO SCREEN              
         OI    OPSCDSTH+6,X'80'           TRANSMIT                              
*                                                                               
         CLI   DOSPMTHD,C'I'              EDI?                                  
         BNE   *+10                       NO                                    
         MVC   OPSCMTH,=C'EDI'            YES                                   
         CLI   DOSPMTHD,C'F'              FAX?                                  
         BNE   *+10                       NO                                    
         MVC   OPSCMTH,=C'FAX'            YES                                   
         CLI   DOSPMTHD,C'E'              EMAIL?                                
         BNE   *+10                       NO                                    
         MVC   OPSCMTH,=C'EML'            YES                                   
         OI    OPSCMTHH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
DR10     L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'12'               NEW STATUS LAYOUT ELEMENT             
         BAS   RE,GETEL                   HAVE A X'12' ELEMENT?                 
         BNE   DR25                       NO                                    
*                                                                               
         USING DOSTELD,R6                 NEW STATUS LAYOUT ELEM DSECT          
         CLI   DOSTSTAT,DDLVRD            DELIVERED?                            
         BE    DR15                       YES                                   
         CLI   DOSTSTAT,DFXDLVD           FAX DELIVERED?                        
         BE    DR15                       YES                                   
         CLI   DOSTSTAT,DEMDLVD           E-MAIL DELIVERED?                     
         BNE   DR20                       NO                                    
*                                                                               
DR15     GOTO1 HEXOUT,DMCB,DOSTTIME,WORK,L'DOSTTIME                             
         MVC   OPSCDLV(2),WORK            HOUR                                  
         MVI   OPSCDLV+2,C'.'             "."                                   
         MVC   OPSCDLV+3(2),WORK+2        MINUTE                                
         OI    OPSCDLVH+6,X'80'           TRANSMIT                              
*                                                                               
         LA    R2,OPSCDATH                IN CASE OF ERROR                      
         BAS   RE,NEXTEL                  HAVE ANOTHER X'12' ELEMENT?           
         BNE   INVSTATS                   NO - ERROR                            
         CLI   DOSTSTAT,DSENT             SENT?                                 
         BNE   INVSTATS                   NO - ERROR                            
*                                                                               
DR20     GOTO1 DATCON,DMCB,(8,DOSTDATE),(11,OPSCDAT)                            
         OI    OPSCDATH+6,X'80'           TRANSMIT                              
*                                                                               
         GOTO1 HEXOUT,DMCB,DOSTTIME,WORK,L'DOSTTIME                             
         MVC   OPSCTIM(2),WORK            HOUR                                  
         MVI   OPSCTIM+2,C'.'             "."                                   
         MVC   OPSCTIM+3(2),WORK+2        MINUTE                                
         OI    OPSCTIMH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
DR25     L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'50'               WHERE'D IT GO ELEMENT                 
         BAS   RE,GETEL                   HAVE A X'50' ELEMENT?                 
         BNE   DRX                        NO                                    
*                                                                               
         USING DOWIGELD,R6                NEW STATUS LAYOUT ELEM DSECT          
         MVC   OPSCRPP,DOWIGRPP           REP PREFIX                            
         OI    OPSCRPPH+6,X'80'           TRANSMIT                              
         MVC   OPSCRPO,DOWIGRPO           REP OFFICE                            
         OI    OPSCRPOH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
DRX      B     XIT                        EXIT                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
VR       LA    R2,OPSPSNDH                SEND METHOD TO PATCH TO               
         TM    OPSPSNDH+1,X'20'           HAS ORDER BEEN PATCHED?               
         BNZ   ERRPF12                    YES - MUST PF12                       
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BE    NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'03'               SUPPLEMENTARY ID ELEMENT              
         BAS   RE,GETEL                   HAVE A X'03' ELEMENT?                 
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING DOSPELD,R6                 SUPPLEMENTARY ID ELEM DSECT           
         MVC   ELEM+100(1),DOSPMTHD       OLD SAVE METHOD                       
         CLC   =C'EDI',8(R2)              EDI?                                  
         BNE   VR01                       NO                                    
         CLI   DOSPMTHD,C'E'              OLD METHOD OE/EMAIL?                  
         BNE   *+8                        NO                                    
         BAS   RE,OETOEDI                 OK TO PATCH FROM OE TO EDI?           
         MVI   DOSPMTHD,C'I'              EDI                                   
         B     VR05                       VALID                                 
*                                                                               
VR01     CLC   =C'FAX',8(R2)              FAX?                                  
         BNE   *+12                       NO                                    
         MVI   DOSPMTHD,C'F'              FAX                                   
         B     VR05                       VALID                                 
         CLC   =C'EML',8(R2)              EML?                                  
         BNE   INVLFLD                    NO - ERROR                            
         MVI   DOSPMTHD,C'E'              EML                                   
*                                                                               
VR05     MVC   BYTE,DOSPMTHD              NEW SAVE METHOD                       
*                                                                               
         LA    R2,OPSPDSTH                DESTINATION TO PATCH TO               
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BE    NEEDFLDS                   NO - ERROR                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         XC    ELEM,ELEM                  CLEAR ELEM                            
         MVC   ELEM+101(1),DOSPDEST       OLD SAVE DESTINATION                  
         CLC   =C'REP',8(R2)              DESTINATION REP?                      
         BNE   *+12                       NO                                    
         MVI   DOSPDEST,C'R'              YES                                   
         B     VR10                       VALID                                 
         CLC   =C'STA',8(R2)              DESTINATION STA?                      
         BNE   INVLFLD                    NO - ERROR                            
         MVI   DOSPDEST,C'S'              YES                                   
         DROP  R6                         DROP R6                               
*                                                                               
VR10     LA    R2,OPSPRPPH                REP PREFIX                            
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         LA    R6,ELEM                    R6 = ELEMENT                          
         USING DOWIGELD,R6                NEW STATUS LAYOUT ELEM DSECT          
         MVI   DOWIGEL,DOWIGELQ           X'50'                                 
         MVI   DOWIGLEN,DOWIGLNQ          ELEM LENGTH                           
         MVC   DOWIGMTH,BYTE              NEW METHOD                            
         CLI   DOWIGMTH,C'E'              METHOD = EML?                         
         BNE   VR20                       NO - DONE                             
         MVC   DOWIGRPP,=CL8'EMAIL'       REP PREFIX = EMAIL                    
         B     VR25                       TEST REP PREFIX/OFFICE                
*                                                                               
VR20     CLI   OPSPDST,C'R'               DEST REP?                             
         BE    VR30                       YES - GO VALIDATE REP PREFIX          
VR25     CLI   OPSPRPPH+5,0               ANY REP PREFIX?                       
         BNE   INVLFLD                    YES - ERROR                           
         CLI   OPSPRPOH+5,0               ANY REP OFFICE?                       
         BNE   INVLFLD                    YES - ERROR                           
         B     VR40                       DONE                                  
*                                                                               
VR30     CLI   5(R2),0                    HAVE ANY INPUT?                       
         BE    NEEDFLDS                   NO - ERROR                            
         BAS   RE,REPCHK                  CHECK IF REP IS VALID                 
         MVC   DOWIGRPP,OPSPRPP           REP PREFIX                            
         OI    OPSPRPPH+6,X'80'           TRANSMIT                              
         CLI   OPSPRPOH+5,0               ANY REP OFFICE?                       
         BE    *+16                       NO                                    
         MVC   DOWIGRPO,OPSPRPO           REP OFFICE                            
         OC    DOWIGRPO,SPACES            SPACE PAD                             
         OI    OPSPRPOH+6,X'80'           TRANSMIT                              
*                                                                               
VR40     L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'50'               WHERE'D IT GO ELEMENT                 
         BAS   RE,GETEL                   HAVE A X'50' ELEMENT?                 
         BE    VR60                       YES - UPDATE EXISTING                 
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         LA    R6,24(R6)                  A(FIRST ELEMENT)                      
         XR    R1,R1                      CLEAR R1                              
*                                                                               
VR45     CLI   0(R6),0                    END OF RECORD?                        
         BE    VR50                       YES                                   
         CLI   0(R6),X'50'                HIGHER THAN A X'50' ELEMENT?          
         BH    VR50                       YES                                   
         IC    R1,1(R6)                   ELEMENT LENGTH                        
         AR    R6,R1                      BUMP TO NEXT ELEMENT                  
         B     VR45                       CHECK NEXT ELEMENT                    
*                                                                               
VR50     GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
         B     VRX                        EXIT                                  
*                                                                               
VR60     MVC   ELEM+102(10),DOWIGRPP      SAVE OLD REP PREFIX/OFFICE            
         MVC   DOWIGEL(DOWIGLNQ),ELEM     OVERWRITE ELEMENT                     
         DROP  R6                         DROP R6                               
*                                                                               
VRX      OI    OPSPSNDH+1,X'20'           PROTECT                               
         OI    OPSPDSTH+1,X'20'           PROTECT                               
         OI    OPSPRPPH+1,X'20'           PROTECT                               
         OI    OPSPRPOH+1,X'20'           PROTECT                               
         B     XIT                        EXIT                                  
***********************************************************************         
* OETOEDI - MAKE SURE ORDER IS IN CANCELLED STATUS                    *         
***********************************************************************         
OETOEDI  NTR1                                                                   
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'12'               STATUS ELEMENT                        
         BAS   RE,GETEL                   HAVE A X'12' ELEMENT?                 
         BNE   OEERROR                    NO - ERROR                            
*                                                                               
         CLI   OPSMED,C'R'                MEDIA R?                              
         BNE   OE10                       NO                                    
         USING DOSTELD,R6                 STATUS ELEMENT DSECT                  
         CLI   DOSTSTAT,QCFMD             CONFIRMED?                            
         BNE   OEERROR                    NO - ERROR                            
         TM    DOSTTYPE,DCNFMCAN          CANCEL CONFIRMED STATUS?              
         BZ    OEERROR                    NO - ERROR                            
         B     XIT                        OK TO CHANGE FROM OE TO EDI           
*                                                                               
OE10     CLI   DOSTSTAT,DDLVRD            DELIVERED STATUS?                     
         BNE   OEERROR                    NO - ERROR                            
         CLC   =C'EMAIL',DOSTDRPP         DELIVERED REP PREFIX =EMAIL?          
         BNE   OEERROR                    NO - ERROR                            
*                                                                               
         BAS   RE,NEXTEL                  HAVE ANOTHER X'12' ELEMENT?           
         BNE   OEERROR                    NO - ERROR                            
         CLI   DOSTSTAT,DSENT             SENT STATUS?                          
         BNE   OEERROR                    NO - ERROR                            
         TM    DOSTTYP2,DOSTCNCL          ORDER SENT W/ 0 SPOTS/$               
         BZ    OEERROR                    NO - ERROR                            
         B     XIT                        OK TO CHANGE FROM OE TO EDI           
         DROP  R6                         DROP USING                            
***********************************************************************         
* REPCHK - CHECK IF REP IS VALID                                      *         
***********************************************************************         
REPCHK   LR    R0,RE                      SAVE RE                               
         LA    R2,OPSPRPPH                REP PREFIX                            
*                                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A1F',0                                      
         CLI   DMCB+4,X'FF'               FOUND A(DDDAREREPS)?                  
         BNE   *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         L     R1,DMCB                    A(DDDAREREPS)                         
         USING DRREPTBD,R1                DDDAREREPS DSECT                      
*                                                                               
         OC    OPSPRPP,SPACES             SPACE PAD                             
*                                                                               
REPCHK10 CLI   0(R1),X'FF'                END OF TABLE?                         
         BE    INVLFLD                    YES - ERROR                           
         CLC   OPSPRPP,DRREPPFX           MATCH ON REP CODE?                    
         BE    REPCHK20                   YES                                   
         LA    R1,DRREPLNQ(R1)            NEXT ENTRY IN REP TABLE               
         B     REPCHK10                   CHECK AGAIN                           
*                                                                               
REPCHK20 LR    RE,R0                      RESTORE RE                            
         BR    RE                         RETURN TO CALLER                      
         DROP  R1                         DROP USING                            
***********************************************************************         
* XRPUT - ADD/CHANGE THE ORDER PATCH RECORD                           *         
***********************************************************************         
XRPUT    XC    ELEM(DOPNLNQ2),ELEM        CLEAR ELEM                            
         LA    R4,ELEM                    R4 = ELEM                             
         USING DOPSENDD,R4                SEND METHOD PATCH ELEM                
         MVI   DOPNEL,DOPNELQ             X'02' ELEMENT                         
         MVI   DOPNLEN,DOPNLNQ            ELEMENT LENGTH                        
         MVC   DOPNOSN(12),ELEM+100       OLD METH/DEST/REP                     
*                                                                               
         L     R6,AIO                     ORDER RECORD                          
         MVI   ELCODE,DOSPELQ             X'03' ELEMENT                         
         BAS   RE,GETEL                   HAVE A X'03' ELEMENT?                 
         BNE   XRXIT                      NO - EXIT                             
*                                                                               
         USING DOSPELD,R6                 SUPPLEMENTARY ID ELEM DSECT           
         MVC   DOPNNSN,DOSPMTHD           NEW SEND METHOD                       
         MVC   DOPNNDS,DOSPDEST           NEW DESTINATION                       
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                     ORDER RECORD                          
         MVI   ELCODE,DOWIGELQ            X'50' ELEMENT                         
         BAS   RE,GETEL                   HAVE A X'50' ELEMENT?                 
         BNE   XRXIT                      NO - EXIT                             
*                                                                               
         USING DOWIGELD,R6                X'50' ELEMENT DSECT                   
         MVC   DOPNNRP(10),DOWIGRPP       NEW REP PREFIX/OFFICE                 
         DROP  R6                         DROP ORDER REC USING                  
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,DOPNDAT)                                   
*                                                                               
         THMS  DDSTIME=YES                GET DDS TIME                          
         STCM  R0,15,PACKOF4B             6:00 AM                               
         ST    R1,FULL                    HHMMSS+                               
         AP    PACKOF4B,FULL              ADD HHMMSS+ TO 6AM                    
         ICM   R1,15,PACKOF4B             DDS TIME                              
         SRL   R1,12                      GET RID OF SECONDS AND SIGN           
         STCM  R1,3,DOPNTIM               TIME (HHMM)                           
*                                                                               
         GOTO1 GETFACT,DMCB,0             CALL GETFACT                          
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING FACTSD,R1                  FACTD DSECT                           
         MVC   DOPNPID,FAPASSWD           PID OF PATCHER                        
         DROP  R1                         DROP R1                               
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#UTLD                                    
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING F@UTLD,R1                  FACTD DSECT                           
         CLC   F@TICKET,SPACES            HAVE A TICKET?                        
         BNH   *+14                       NO                                    
         MVI   DOPNLEN,DOPNLNQ2           LONGER ELEMENT LENGTH                 
         MVC   DOPNTIC,F@TICKET           TICKET NUMBER                         
         DROP  R1                         DROP R1                               
*                                                                               
         MVC   AIO,AIO2                   ADD/GET RECORD INTO AIO2              
         LA    R6,KEY                     ORDER KEY                             
         USING DOKEY,R6                   ORDER PATCH KEY                       
         MVI   DOKCMT,DOKPSTAT            ORDER/PATCH RECORD TYPE               
*                                                                               
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            KEY ALREADY EXISTS?                   
         BE    XR10                       YES - GO UPDATE RECORD                
*                                                                               
         L     R6,AIO                     BUILD RECORD IN AIO2                  
         MVC   DOKEY,KEYSAVE              COPY THE KEY                          
         MVC   DORFRST(DOPNLNQ2),DOPNEL   MOVE THE X'02' ELEMENT IN             
         LLC   R1,DOPNLEN                 ELEMENT LENGTH                        
         AHI   R1,DORFRST-DOKEY           ADD RECORD KEY LENGTH                 
         STCM  R1,3,DORLEN                RECORD LENGTH                         
         MVC   DORAGY,AGENCY              AGENCY                                
         GOTO1 ADDREC                     ADD THE RECORD                        
         B     XRXIT                      EXIT                                  
*                                                                               
XR10     GOTO1 GETREC                     GET THE RECORD                        
*                                                                               
         L     R6,AIO2                    RECORD IN AIO2                        
         LA    R6,24(R6)                  PUT ELEMENT HERE                      
         XR    R1,R1                      CLEAR R1                              
*                                                                               
XR20     CLI   0(R6),0                    EOR?                                  
         BE    XR30                       YES - ELEMENT GOES HERE               
         CLI   0(R6),X'02'                ELEMENT HIGHER THAN X'02'?            
         BH    XR30                       YES - ELEMENT GOES HERE               
         BE    XR30                       EQUAL ALSO GOES HERE                  
         IC    R1,1(R6)                   ELEMENT LENGTH                        
         AR    R6,R1                      BUMP TO NEXT ELEMENT/EOR              
         B     XR20                       CHECK IF WE SHOULD ADD HERE           
*                                                                               
XR30     GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
*                                                                               
         GOTO1 PUTREC                     PUT THE RECORD BACK                   
*                                                                               
XRXIT    MVC   AIO,AIO1                   RESTORE AIO                           
         B     XIT                        EXIT                                  
         DROP  R4,R6                      DROP R4 AND R6                        
***********************************************************************         
* GET THE PFKEY INFORMATION                                           *         
***********************************************************************         
GETPF    NTR1                                                                   
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE       INITIALIZE THE PFKEYS                 
*                                                                               
         B     XIT                        RETURN                                
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                        *         
***********************************************************************         
PFRTNQ   EQU   12                         RETURN                                
*                                                                               
PFTABLE  DS    0C                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,PFRTNQ,PFTRPROG,0,0,0)                               
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
         LTORG                            LTORG                                 
         DROP  R7,RB                                                            
***********************************************************************         
* GENERAL ERROR MESSAGES                                                        
***********************************************************************         
INVLFLD  MVI   GERROR1,INVALID                                                  
         J     ERREXIT                                                          
*                                                                               
OEERROR  LHI   RF,1392             ORDER MUST IN CANCELLED STATUS               
         STCM  RF,3,GERROR                                                      
         L     R1,ATIOB            CURSOR TO BUYER FIELD                        
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         J     ER2EXIT                                                          
*                                                                               
ER2EXIT  MVI   GETMSYS,2           SPOT MESSAGES                                
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
ERRPF12  MVI   GERROR1,241         MUST USE PF12                                
         J     ERREXIT                                                          
*                                                                               
INVLDPF  MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         J     ERREXIT                                                          
*                                                                               
INVSTATS MVI   GERROR1,9           SENT STATUS MUST FOLLOW DELIVERED            
         J     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         J     INFEXIT                                                          
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
MYINFXIT MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
***********************************************************************         
* LOCAL SAVED STORAGE                                                           
***********************************************************************         
LSSD     DSECT                                                                  
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
SAVEKEY  DS    XL13                SAVED KEY                                    
BASERC   DS    A                   BASE RC                                      
RELO     DS    A                   A(RELO)                                      
VGLOBBER DS    A                   A(GLOBBER)                                   
*                                                                               
         ORG   LSSD+L'SYSSPARE                                                  
*                                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSB2D          (OUR PATCHSTA SCREEN)                        
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         PRINT ON                                                               
* DDCOMFACSD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* SPOMSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPOMSWORKD                                                     
         PRINT ON                                                               
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPOMS12   08/19/13'                                      
         END                                                                    
