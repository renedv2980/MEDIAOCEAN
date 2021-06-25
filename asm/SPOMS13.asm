*          DATA SET SPOMS13    AT LEVEL 003 AS OF 03/02/17                      
*PHASE T23413A                                                                  
T23413   TITLE 'SPOMS13 - ORDER PSUPP'                                          
T23413   CSECT                                                                  
         PRINT NOGEN                                                            
BGN      NMOD1 0,*T23413*,R7,RR=R3                                              
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
         CLI   CALLSP,0                   DID WE PFKEY INTO PSUPP?              
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
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'03'               SUPPLEMENTARY ID ELEMENT              
         BAS   RE,GETEL                   HAVE AN X'03' ELEMENT?                
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING DOSPELD,R6                 SUPPLEMENTARY ID ELEM DSECT           
*                                                                               
         MVI   OPSFRSD,C'N'               DEFAULT RESEND TO NO                  
         TM    DOSPFLG2,DOSPRSND          ORDER NEED TO BE RESENT?              
         BZ    *+8                         NO                                   
         MVI   OPSFRSD,C'Y'               SET RESEND TO Y                       
         OI    OPSFRSDH+6,X'80'           TRANSMIT                              
*                                                                               
         MVC   OPSFPER,=C'SP'             SALESPERSON                           
         TM    DOSPFLG1,DOSPPPER          POINTPERSON?                          
         BZ    *+8                        NO                                    
         MVI   OPSFPER,C'P'               YES - POINTPERSON                     
         OI    OPSFPERH+6,X'80'           TRANSMIT                              
*                                                                               
         EDIT  DOSPREVN,OPSFREV,FILL=0    REVISION NUMBER                       
         OI    OPSFREVH+6,X'80'           TRANSMIT                              
*                                                                               
         EDIT  DOSPVER#,OPSFSND,FILL=0    VERSION NUMBER                        
         OI    OPSFSNDH+6,X'80'           TRANSMIT                              
         EDIT  DOSPGAP#,OPSFGAP,FILL=0    GAP SEQ NUMBER                        
         OI    OPSFGAPH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'01'               PRIMARY ID ELEMENT                    
         BAS   RE,GETEL                   HAVE A X'01' ELEMENT?                 
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING DOIDELD,R6                 PRIMARY ID ELEM DSECT                 
         MVC   OPSFCON,DOIDCON            CONTRACT NUMBER                       
         OI    OPSFCONH+6,X'80'           TRANSMIT                              
         DROP R6                          DROP R6                               
*                                                                               
DRX      B     XIT                        EXIT                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
VR       TM    OPSTPERH+1,X'20'           HAS ORDER BEEN PATCHED?               
         BNZ   ERRPF12                    YES - MUST PF12                       
*                                                                               
         LA    R2,OPSTPERH                SP/PP                                 
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,DOSTELQ             X'12' ELEMENT                         
         BAS   RE,GETEL                   HAVE A X'12' ELEMENT?                 
         BNE   ERRUNSNT                   NO - ERROR                            
*                                                                               
         CLI   OPSTPERH+5,0               PP/SP INPUT?                          
         BNE   VRPER                      YES                                   
         CLI   OPSTREVH+5,0               REVISION INPUT?                       
         BNE   VRPER                      YES                                   
         CLI   OPSTRSDH+5,0               NEED TO RESEND INPUT?                 
         BNE   VRPER                      YES                                   
         CLI   OPSTSNDH+5,0               OE SEND COUNT INPUT?                  
         BNE   VRPER                      YES                                   
         CLI   OPSTGAPH+5,0               OE GAP SEQ INPUT?                     
         BNE   VRPER                      YES                                   
         CLI   OPSTCONH+5,0               OE CONTRACT INPUT?                    
         BE    NEEDFLDS                   NO - ERROR                            
*                                                                               
* MAKE SURE SALESPERSON/POINTPERSON IS VALID                                    
*                                                                               
VRPER    DS    0H                                                               
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'03'               SUPPLEMENTARY ID ELEMENT              
         BAS   RE,GETEL                   HAVE A X'03' ELEMENT?                 
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING DOSPELD,R6                 SUPPLEMENTARY ID ELEM DSECT           
         XC    ELEM,ELEM                  CLEAR ELEM (SAVE OLD INFO)            
         MVI   ELEM+100,C'S'              SALESPERSON                           
         TM    DOSPFLG1,DOSPPPER          POINTPERSON?                          
         BZ    *+8                        NO                                    
         MVI   ELEM+100,C'P'              YES                                   
*                                                                               
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BNE   VRPER10                    YES                                   
         MVI   5(R2),2                    INPUT LNEGTH                          
         MVC   8(2,R2),=C'SP'             SALESPERSON                           
         TM    DOSPFLG1,DOSPPPER          POINTPERSON?                          
         BZ    *+10                       NO                                    
         MVC   8(2,R2),=C'PP'             SALESPERSON                           
*                                                                               
VRPER10  CLC   =C'SP',8(R2)               SALESPERSON?                          
         BNE   *+12                       NO                                    
         NI    DOSPFLG1,X'FF'-DOSPPPER    TURN OFF POINTPERSON                  
         B     VRREVN                     VALID                                 
         CLC   =C'PP',8(R2)               POINTPERSON?                          
         BNE   INVLFLD                    NO - ERROR                            
         OI    DOSPFLG1,DOSPPPER          TURN ON POINTPERSON                   
*                                                                               
* MAKE SURE REVISION NUMBER IS VALID                                            
*                                                                               
VRREVN   DS    0H                                                               
         LA    R2,OPSTREVH                REVISION NUMBER TO PATCH TO           
         OI    6(R2),X'80'                TRANSMIT                              
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BNE   VRREVN10                   NO                                    
         EDIT  DOSPREVN,OPSTREV,FILL=0    VERSION NUMBER                        
         MVI   5(R2),3                    INPUT LENGTH OF 3                     
         OI    4(R2),X'08'                NUMERIC                               
*                                                                               
VRREVN10 TM    4(R2),X'08'                VALID NUMERIC?                        
         BZ    INVLFLD                    NO - ERROR                            
         LLC   RE,5(R2)                   LENGTH                                
         BCTR  RE,0                       -1 FOR EXECUTED PACK                  
         EX    RE,*+8                     EXECUTE PACK                          
         B     *+10                       BRANCH OVER PACK                      
         PACK  DUB,8(0,R2)                ** EXECUTED **                        
         CVB   RE,DUB                     CVB                                   
         STC   RE,DOSPREVN                NEW REVISION NUMBER                   
*                                                                               
VRRESEND DS    0H                                                               
         LA    R2,OPSTRSDH                OE SEND COUNT TO PATCH TO             
         OI    6(R2),X'80'                TRANSMIT                              
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BNE   VRRSD10                    YES                                   
*                                                                               
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BNE   VRRSD10                    YES                                   
         MVI   5(R2),1                    INPUT LNEGTH                          
         MVI   8(R2),C'N'                 DEFAULT TO N                          
         TM    DOSPFLG2,DOSPRSND          NEED TO RESEND?                       
         BZ    *+8                         NO                                   
         MVI   8(R2),C'Y'                  SET TO Y                             
*                                                                               
VRRSD10  CLI   8(R2),C'N'                 DOES NOT NEED TO RESEND?              
         BNE   VRRSD20                    NO                                    
         NI    DOSPFLG2,X'FF'-DOSPRSND    TURN OFF RESEND FLAG                  
         B     VRSCGP                     VALID                                 
*                                                                               
VRRSD20  CLI   8(R2),C'Y'                 POINTPERSON?                          
         BNE   INVLFLD                    NO - ERROR                            
         OI    DOSPFLG2,DOSPRSND          TURN ON RESEND FLAG                   
*                                                                               
* MAKE SURE BOTH SENDCOUNT&GAP HAVE VALUE OR NO VALUE                           
*                                                                               
VRSCGP   DS    0H                                                               
         LA    R2,OPSTSNDH                OE SEND COUNT TO PATCH TO             
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BE    VRSCGP01                   NO                                    
         CLI   OPSTGAPH+5,0               YES - GAP MUST HAVE VALUE             
         BE    ERROE                      YES - ERROR                           
         B     VRSDCT                     DONE                                  
*                                                                               
VRSCGP01 CLI   OPSTGAPH+5,0               GAP MUST HAS VALUE?                   
         BNE   ERROE                      YES - ERROR                           
*                                                                               
* MAKE SURE WE HAVE A VALID SENDCOUNT                                           
*                                                                               
VRSDCT   MVC   ELEM+101(2),DOSPVER#       SAVE OLD OE SEND COUNT                
         LA    R2,OPSTSNDH                OE SEND COUNT TO PATCH TO             
         OI    6(R2),X'80'                TRANSMIT                              
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BNE   VRSDCT10                   YES                                   
         EDIT  DOSPVER#,OPSTSND,FILL=0    VERSION NUMBER                        
         MVI   5(R2),5                    INPUT LENGTH OF 5                     
         OI    4(R2),X'08'                NUMERIC                               
*                                                                               
VRSDCT10 TM    4(R2),X'08'                VALID NUMERIC?                        
         BZ    INVLFLD                    NO - ERROR                            
         LLC   RE,5(R2)                   LENGTH                                
         BCTR  RE,0                       -1 FOR EXECUTED PACK                  
         EX    RE,*+8                     EXECUTE PACK                          
         B     *+10                       BRANCH OVER PACK                      
         PACK  DUB,8(0,R2)                ** EXECUTED **                        
         CVB   RE,DUB                     CVB                                   
         STCM  RE,3,DOSPVER#              NEW OE SEND COUNT                     
*                                                                               
* MAKE SURE WE HAVE A VALID GAP SEQ#                                            
*                                                                               
VRGAP    MVC   ELEM+103(2),DOSPGAP#       SAVE OLD GAP SEQ NUM                  
         LA    R2,OPSTGAPH                GAP SEQ NUM TO PATCH TO               
         OI    6(R2),X'80'                TRANSMIT                              
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BNE   VRGAP10                    YES                                   
         EDIT  DOSPGAP#,OPSTGAP,FILL=0    VERSION NUMBER                        
         MVI   5(R2),5                    INPUT LENGTH OF 5                     
         OI    4(R2),X'08'                NUMERIC                               
*                                                                               
VRGAP10  TM    4(R2),X'08'                VALID NUMERIC?                        
         BZ    INVLFLD                    NO - ERROR                            
         LLC   RE,5(R2)                   LENGTH                                
         BCTR  RE,0                       -1 FOR EXECUTED PACK                  
         EX    RE,*+8                     EXECUTE PACK                          
         B     *+10                       BRANCH OVER PACK                      
         PACK  DUB,8(0,R2)                ** EXECUTED **                        
         CVB   RE,DUB                     CVB                                   
         STCM  RE,3,DOSPGAP#              NEW OE SEND COUNT                     
         DROP  R6                         DROP R6                               
*                                                                               
VRCONT   L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'01'               PRIMARY ID ELEMENT                    
         BAS   RE,GETEL                   HAVE A X'01' ELEMENT?                 
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING DOIDELD,R6                 PRIMARY ID ELEM DSECT                 
         MVC   ELEM+105(8),DOIDCON        SAVE OLD CONTRACT NUM                 
         LA    R2,OPSTCONH                CONTRACT NUM TO PATCH TO              
         OI    6(R2),X'80'                TRANSMIT                              
         CLI   5(R2),0                    HAVE ANY INPUT?                       
         BNE   VRCONT10                   YES                                   
         MVC   8(8,R2),DOIDCON            CONTRACT NUMBER                       
         MVI   5(R2),8                    INPUT LENGTH                          
*                                                                               
VRCONT10 CLC   =C'CLEAR',8(R2)            CONTRACT NUMBER = CLEAR?              
         BNE   *+10                       NO                                    
         XC    8(8,R2),8(R2)              YES - CLEAR CONTRACT NUMBER           
         MVC   DOIDCON,8(R2)              CONTRACT NUMBER                       
         CLC   DOIDCON,SPACES             HAVE A CONTRACT?                      
         BNH   VRX                        NO - DON'T SPACE PAD                  
         OC    DOIDCON,SPACES             SPACE PAD                             
         DROP  R6                         DROP R6                               
*                                                                               
VRX      OI    OPSTPERH+1,X'20'           PROTECT                               
         OI    OPSTREVH+1,X'20'           PROTECT                               
         OI    OPSTRSDH+1,X'20'           PROTECT                               
         OI    OPSTSNDH+1,X'20'           PROTECT                               
         OI    OPSTGAPH+1,X'20'           PROTECT                               
         OI    OPSTCONH+1,X'20'           PROTECT                               
         B     XIT                        EXIT                                  
***********************************************************************         
* XRPUT - ADD/CHANGE THE ORDER PATCH RECORD                           *         
***********************************************************************         
XRPUT    XC    ELEM(DOPPLNQ2),ELEM        CLEAR ELEM                            
         LA    R4,ELEM                    R4 = ELEM                             
         USING DOPSUPPD,R4                SUPPLEMENTARY PATCH ELEM              
         MVI   DOPPEL,DOPPELQ             X'73' ELEMENT                         
         MVI   DOPPLEN,DOPPLNQ            ELEMENT LENGTH                        
         MVC   DOPPOSP(13),ELEM+100       OLD SP/SND COUNT/GAP/CONTRACT         
*                                                                               
         L     R6,AIO                     ORDER RECORD                          
         MVI   ELCODE,DOSPELQ             X'03' ELEMENT                         
         BAS   RE,GETEL                   HAVE A X'03' ELEMENT?                 
         BNE   XRXIT                      NO - EXIT                             
*                                                                               
         USING DOSPELD,R6                 SUPPLEMENTARY ID ELEM DSECT           
         MVI   DOPPNSP,C'S'               NEW SALESPERSON                       
         TM    DOSPFLG1,DOSPPPER          POINTPERSON?                          
         BZ    *+8                        NO                                    
         MVI   DOPPNSP,C'P'               YES - POINTPERSON                     
         MVC   DOPPNSC,DOSPVER#           NEW OE SEND COUNT                     
         MVC   DOPPNGS,DOSPGAP#           NEW OE GAP SEQNUM                     
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                     A(ORDER RECORD)                       
         MVI   ELCODE,X'01'               PRIMARY ID ELEMENT                    
         BAS   RE,GETEL                   HAVE A X'01' ELEMENT?                 
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING DOIDELD,R6                 PRIMARY ID ELEM DSECT                 
         MVC   DOPPNCN,DOIDCON            NEW CONTRACT NUM                      
         DROP  R6                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,DOPPDAT)                                   
*                                                                               
         THMS  DDSTIME=YES                GET DDS TIME                          
         STCM  R0,15,PACKOF4B             6:00 AM                               
         ST    R1,FULL                    HHMMSS+                               
         AP    PACKOF4B,FULL              ADD HHMMSS+ TO 6AM                    
         ICM   R1,15,PACKOF4B             DDS TIME                              
         SRL   R1,12                      GET RID OF SECONDS AND SIGN           
         STCM  R1,3,DOPPTIM               TIME (HHMM)                           
*                                                                               
         GOTO1 GETFACT,DMCB,0             CALL GETFACT                          
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING FACTSD,R1                  FACTD DSECT                           
         MVC   DOPPPID,FAPASSWD           PID OF PATCHER                        
         DROP  R1                         DROP R1                               
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#UTLD                                    
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING F@UTLD,R1                  FACTD DSECT                           
         CLC   F@TICKET,SPACES            HAVE A TICKET?                        
         BNH   *+14                       NO                                    
         MVI   DOPPLEN,DOPPLNQ2           LONGER ELEMENT LENGTH                 
         MVC   DOPPTIC,F@TICKET           TICKET NUMBER                         
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
         MVC   DORFRST(DOPPLNQ2),DOPPEL   MOVE THE X'03' ELEMENT IN             
         LLC   R1,DOPPLEN                 ELEMENT LENGTH                        
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
         CLI   0(R6),X'03'                ELEMENT HIGHER THAN X'03'?            
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
ERRUNSNT LHI   RF,1397             CANT PATCH UNSENT                            
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ER2EXIT  STCM  RF,3,GERROR         ERROR NUMBER                                 
         L     R1,ATIOB            SET CURSOR ADDRESS                           
         USING TIOBD,R1            TRANSLATOR DSECT                             
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         DROP  R1                  DROP R1                                      
         MVI   GETMSYS,2           SPOT MESSAGES                                
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
ERRPF12  MVI   GERROR1,241         MUST USE PF12                                
         J     ERREXIT                                                          
*                                                                               
ERROE    MVI   GERROR1,102         SEND COUNT & GAP SEQ MUST BOTH               
         J     ERREXIT                                                          
*                                                                               
INVLDPF  MVI   GERROR1,ERINVPFK    INVALID PF KEY                               
         J     ERREXIT                                                          
***********************************************************************         
* GENERAL INFO MESSAGES                                                         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD    PLEASE ENTER FIELDS AS REQUIRED              
         J     INFEXIT                                                          
*                                                                               
INFEXIT  MVI   GETMSYS,255         GENERAL MESSAGES                             
         MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
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
       ++INCLUDE SPOMSB3D          (OUR PATCHSTA SCREEN)                        
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
**PAN#1  DC    CL21'003SPOMS13   03/02/17'                                      
         END                                                                    
