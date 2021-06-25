*          DATA SET SPOMS14    AT LEVEL 003 AS OF 01/09/15                      
*PHASE T23414A                                                                  
T23414   TITLE 'SPOMS14 - ORDER PMKGD'                                          
T23414   CSECT                                                                  
         PRINT NOGEN                                                            
BGN      NMOD1 0,*T23414*,R7,RR=R3                                              
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
         OI    GENSTAT3,MULTFILS                                                
*                                                                               
         LA    R2,CONACTH                 CURSOR TO ACTION FIELD                
         CLI   CALLSP,0                   DID WE PFKEY INTO PMKGD?              
         BE    INVLFLD                    NO - ERROR                            
*                                                                               
         ST    R3,RELO                    RELO                                  
         ST    RC,BASERC                  BASE RC                               
         BAS   RE,GETPF                   GET PFKEYS                            
*                                                                               
         L     RF,ACOMFACS                A(COMFACS)                            
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
*                                                                               
         CLI   MODE,VALKEY                VALIDATE KEY?                         
         BE    VK                         YES                                   
         CLI   MODE,DISPREC               DISPLAY RECORD?                       
         BE    DR                         YES                                   
         CLI   MODE,VALREC                VALIDATE RECORD?                      
         BE    VR                         YES                                   
         CLI   MODE,XRECPUT               VALIDATE RECORD?                      
         BE    XRPUT                      YES                                   
         CLI   MODE,SETFILE               SETFILE?                              
         BE    SF                                                               
*                                                                               
EXITYES  SR    RC,RC                                                            
EXITNO   LTR   RC,RC                                                            
EXIT     XIT1                                                                   
***********************************************************************         
* SET FILE                                                                      
***********************************************************************         
SF       BRAS  RE,SSV                                                           
         J     EXIT                                                             
***********************************************************************         
* VALIDATE THE KEY                                                    *         
***********************************************************************         
*                                                                               
VK       DS    0H                                                               
         BRAS  RE,RSV                                                           
*                                                                               
         NI    DMINBTS,X'FF'-X'08'        NO DELETED RECORDS                    
*                                                                               
         LA    R2,OPSMEDH                 MEDIA HEADER                          
         GOTO1 VALIMED                    VALIDATE MEDIA                        
*                                                                               
         LA    R2,OPSORDRH                ORDER NUMBER HEADER                   
         GOTOR BINORDR,DMCB,8(R2)         GET BINARY ORDER NUMBER               
*                                                                               
VKGETORD LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING DAREORDD,R6                DARE ORDER DSECT                      
         MVI   DOKTYPE,DOKTYPQ            X'0D'                                 
         MVI   DOKSUBTY,DOKSTYPQ          X'34'                                 
         MVC   DOKAGMD,BAGYMD             A/M                                   
         MVC   DOKORDER,BINORDER          ORDER NUMBER                          
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(9),KEYSAVE             KEY MATCHES?                          
         JNE   *+2                        NO - NO ORDER FOUND                   
*                                                                               
         GOTO1 GETREC                     GET THE ORDER RECORD                  
         JNE   *+2                                                              
*                                                                               
         L     R6,AIO                     R6 = ORDER RECORD                     
         MVI   ELCODE,DOIDELQ             X'01' ELEMENT                         
         BAS   RE,GETEL                   HAVE AN X'01' ELEMENT?                
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
         USING DOIDELD,R6                 ID ELEMENT DSECT                      
         MVC   FULL(3),DOIDBYR            BUYER                                 
         MVC   SVSTATN,DOISTA                                                   
         DROP  R6                                                               
*                                                                               
         LA    R2,OPSMKGDH                MAKEGOOD HEADER                       
*                                                                               
         BRAS  RE,SSV                                                           
         BNE   VKGETMG                                                          
*                                                                               
         LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING MNXKEY,R6                  DARE MAKEGOOD DSECT                   
         MVI   MNXKTYPE,MNXKTYPQ          X'0D'                                 
         MVI   MNXKSBTY,MNXKSBTQ          X'36'                                 
         MVC   MNXKAGMD,BAGYMD            A/M                                   
         MVC   MNXKORDR,BINORDER          ORDER NUMBER                          
         MVC   MNXKGRP,8(R2)              MAKEGOOD GROUP                        
         OC    MNXKGRP,SPACES             SPACE PAD                             
         DROP  R6                         DROP R6                               
*                                                                               
         MVC   KEYSAVE,KEY                PASS BACK DELETED RECORDS             
         GOTO1 HIGH                                                             
         CLC   KEY(L'MNXKEY),KEYSAVE      KEY MATCHES?                          
         BNE   ERRNOORD                   NO - NO ORDER FOUND                   
         B     VKX10                                                            
*                                                                               
VKGETMG  LA    R6,KEY                     R6 = KEY                              
         XC    KEY,KEY                    CLEAR THE KEY                         
         USING DAREMGND,R6                DARE MAKEGOOD DSECT                   
         MVI   MNKTYPE,MNKTYPQ            X'0D'                                 
         MVI   MNKSUBTY,MNKSTYPQ          X'36'                                 
         MVC   MNKAGMD,BAGYMD             A/M                                   
         MVC   MNKBYR,FULL                BUYER                                 
         MVC   MNKORDER,BINORDER          ORDER NUMBER                          
         MVC   MNKGROUP,8(R2)             MAKEGOOD GROUP                        
         OC    MNKGROUP,SPACES            SPACE PAD                             
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            KEY MATCHES?                          
         BNE   ERRNOORD                   NO - NO ORDER FOUND                   
         DROP  R6                         DROP R6                               
*                                                                               
VKX10    MVC   SAVEKEY,KEY                SAVE THE KEY                          
*                                                                               
VKX      J     EXIT                       EXIT                                  
*                                                                               
***********************************************************************         
* DISPLAY THE RECORD                                                  *         
***********************************************************************         
DR       OI    OPSMEDH+1,X'20'            PROTECT                               
         OI    OPSORDRH+1,X'20'           PROTECT                               
         OI    OPSMKGDH+1,X'20'           PROTECT                               
*                                                                               
         L     R6,AIO                     A(MAKEGOOD RECORD)                    
         MVI   ELCODE,X'05'               MKGD GROUP STAT ELEMENT               
         BAS   RE,GETEL                   HAVE AN X'05' ELEMENT?                
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING MNSTELD,R6                 MKGD GROUP STAT ELEM DSECT            
         MVC   OPSFMKG(3),=C'???'         DEFAULT TO UNKNOWN                    
         LA    R1,MKGDTAB                 MAKEGOOD TABLE                        
         XR    RE,RE                      CLEAR RE                              
*                                                                               
         CLI   MNSTSTAT,MNSTDELV          SKIP DELIVERED STATUS                 
         BNE   DR05                                                             
         IC    RE,1(R6)                   LENGTH OF ELEMENT                     
         AR    R6,RE                      BUMP TO NEXT ELEMENT                  
*                                                                               
DR05     CLI   0(R1),X'FF'                END OF TABLE?                         
         BE    DR15                       YES                                   
         CLC   MNSTSTAT,1(R1)             MATCH ON STATUS?                      
         BE    DR10                       YES                                   
         IC    RE,0(R1)                   LENGTH OF ENTRY                       
         AR    R1,RE                      BUMP TO NEXT ENTRY                    
         B     DR05                       CHECK NEXT STATUS                     
*                                                                               
DR10     IC    RE,0(R1)                   ENTRY LENGTH                          
         SHI   RE,3                       MINUS OVERHEAD +1 FO EX               
         EX    RE,*+8                     MOVE STATUS ONTO SCREEN               
         B     *+10                       SO IDF DOESN'T COMPLAIN               
         MVC   OPSFMKG(0),2(R1)           ** EXECUTED **                        
*                                                                               
DR15     OI    OPSFMKGH+6,X'80'           TRANSMIT                              
         DROP  R6                         DROP R6                               
*                                                                               
DRX      J     EXIT                       EXIT                                  
***********************************************************************         
* VALIDATE RECORD                                                     *         
***********************************************************************         
VR       TM    OPSTMKGH+1,X'20'           HAS MAKEGOOD BEEN PATCHED?            
         BNZ   ERRPF12                    YES - MUST PF12                       
*                                                                               
         LA    R2,OPSTMKGH                MAKEGOOD                              
         CLI   5(R2),0                    ANY INPUT?                            
         BE    NEEDFLDS                   NO - ERROR                            
*                                                                               
VR00     OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
         L     R6,AIO                     A(MAKEGOOD RECORD)                    
         MVI   ELCODE,X'05'               MKGD GROUP STAT ELEMENT               
         BAS   RE,GETEL                   HAVE AN X'05' ELEMENT?                
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING MNSTELD,R6                 MKGD GROUP STAT ELEM DSECT            
         MVC   BYTE,MNSTSTAT              MAKEGOOD STATUS                       
         DROP  R6                         DROP R6                               
*                                                                               
         XC    ELEM,ELEM                  CLEAR ELEM                            
         LA    R6,ELEM                    BUILD NEW ELEMENT                     
         USING MNSTELD,R6                 MKGD GROUP STAT ELEM DSECT            
         MVI   MNSTEL,MNSTELQ             X'05'                                 
         MVI   MNSTLEN,MNSTLENQ           ELEMENT LENGTH                        
*                                                                               
         CLC   =C'OKAYED',8(R2)           OKAYED STATUS?                        
         BNE   VR05                       NO                                    
*                                                                               
         CLI   BYTE,MNSTSAPP              SELF APPLIED?                         
         BNE   ERRSELF                    NO, MKGD MUST BE SELF APPLIED         
*                                                                               
         MVI   MNSTSTAT,MNSTOKAY          OKAYED                                
         B     VR15                       DONE                                  
*                                                                               
VR05     CLC   =C'SHMIGGLES',8(R2)        CANCELLED STATUS?                     
         BE    VR10                       YES                                   
         CLC   =C'CANCELLED',8(R2)        CANCELLED STATUS?                     
         BNE   INVLFLD                    NO - ERROR                            
*                                                                               
VR10     DS    0H                                                               
         CLI   BYTE,MNSTOKAY              MAKEGOOD OKAYED?                      
         BE    ERRCAN                     NO - ERROR                            
         CLI   BYTE,MNSTSAPP              MAKEGOOD OKAYED?                      
         BE    ERRCAN                     NO - ERROR                            
         LA    R6,ELEM                    RE-POINT R6 TO ELEM                   
         MVI   MNSTSTAT,MNSTCAN           CANCELLED                             
*                                                                               
VR15     GOTO1 DATCON,DMCB,(5,0),(19,MNSTDATE)                                  
*                                                                               
         THMS  DDSTIME=YES                GET DDS TIME                          
         STCM  R0,15,PACKOF4B             6:00 AM                               
         ST    R1,FULL                    HHMMSS+                               
         AP    PACKOF4B,FULL              ADD HHMMSS+ TO 6AM                    
         ICM   R1,15,PACKOF4B             DDS TIME                              
         SRL   R1,12                      GET RID OF SECONDS AND SIGN           
         STCM  R1,3,MNSTTIME              TIME (HHMM)                           
         DROP R6                          DROP R6                               
*                                                                               
         L     R6,AIO                     A(MAKEGOOD RECORD)                    
         MVI   ELCODE,X'05'               MKGD GROUP STAT ELEMENT               
         BAS   RE,GETEL                   HAVE AN X'05' ELEMENT?                
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         USING MNSTELD,R6                 MKGD GROUP STAT ELEM DSECT            
         MVC   ELEM+100(1),MNSTSTAT       SAVE OLD STATUS                       
         DROP  R6                         DROP R6                               
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
         ORG   *-2                                                              
         CLI   SVSTATN,X'E8'              CABLE?                                
         BL    *+8                                                              
         MVI   DMCB,C'T'                  XSPTFIL                               
         BASR  RE,RF                                                            
*                                                                               
         BRAS  RE,DELOFFER                                                      
*                                                                               
         L     R6,AIO1                    A(MAKEGOOD RECORD)                    
         ST    R6,AIO                                                           
         MVC   KEY,0(R6)                                                        
         GOTO1 HIGH                                                             
         JNE   *+2                                                              
         MVC   AIO,AIO2                   LEAVE CHG RECORD ALONE                
         GOTO1 GETREC                                                           
         JNE   *+2                                                              
         MVC   AIO,AIO1                   POINT BACK TO CHG RECORD              
*                                                                               
         LA    RE,KEY+13                  RE=A(STATUS BYT OF SPOT KEY)          
         LA    RF,15(R6)                  RF=A(STATUS BYT OF SPOT REC)          
         CLI   SVSTATN,X'E8'              CABLE?                                
         BL    VR20                                                             
         LA    RE,KEY+32                  RE=A(STATUS BYT OF XSPOT KEY)         
         LA    RF,34(R6)                  RF=A(STATUS BYT OF XSPOT REC)         
*                                                                               
VR20     OI    0(RE),X'80'                MARK KEY DELETED                      
         OI    0(RF),X'80'                MARK REC DELETED                      
         GOTO1 WRITE                                                            
*                                                                               
VRX      OI    1(R2),X'20'                PROTECT                               
         J     EXIT                       EXIT                                  
***********************************************************************         
* XRPUT - ADD/CHANGE THE ORDER PATCH RECORD                           *         
***********************************************************************         
XRPUT    DS    0H                                                               
*                                                                               
         XC    ELEM(DOPMLNQ2),ELEM        CLEAR ELEM                            
         LA    R4,ELEM                    R4 = ELEM                             
         USING DOPMKGDD,R4                MAKEGOOD PATCH ELEM                   
         MVI   DOPMEL,DOPMELQ             X'04' ELEMENT                         
         MVI   DOPMLEN,DOPMLNQ            ELEMENT LENGTH                        
         MVC   DOPMOST,ELEM+100           OLD STATUS/DATE/TIME                  
*                                                                               
         L     R6,AIO                     MAKEGOOD RECORD                       
         MVI   ELCODE,X'05'               X'05' ELEMENT                         
         BAS   RE,GETEL                   HAVE A X'05' ELEMENT?                 
         BE    *+6                        YES                                   
         DC    H'0'                       DEATH                                 
*                                                                               
         USING MNSTELD,R6                 MKGD GROUP STAT ELEM DSECT            
         MVC   DOPMNST,MNSTSTAT           NEW STATUS                            
         MVC   DOPMDAT,MNSTDATE           NEW DATE                              
         MVC   DOPMTIM,MNSTTIME           NEW TIME                              
         LA    R3,KEY                     MAKEGOOD KEY                          
         USING MNKEY,R3                   MAKEGOOD KEY DSECT                    
         MVC   DOPMMKG,MNKGROUP           MAKEGOOD CODE                         
         DROP  R3,R6                      DROP R3 AND R6                        
*                                                                               
         GOTO1 GETFACT,DMCB,0             CALL GETFACT                          
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING FACTSD,R1                  FACTD DSECT                           
         MVC   DOPMPID,FAPASSWD           PID OF PATCHER                        
         DROP  R1                         DROP R1                               
*                                                                               
         GOTO1 GETFACT,DMCB,(X'80',0),F#UTLD                                    
         L     R1,0(R1)                   A(INPUT BLOCK)                        
         USING F@UTLD,R1                  FACTD DSECT                           
         CLC   F@TICKET,SPACES            HAVE A TICKET?                        
         BNH   *+14                       NO                                    
         MVI   DOPMLEN,DOPMLNQ2           LONGER ELEMENT LENGTH                 
         MVC   DOPMTIC,F@TICKET           TICKET NUMBER                         
         DROP  R1                         DROP R1                               
*                                                                               
         BRAS  RE,RSV                                                           
*                                                                               
         MVC   SAVEKEY,KEY                SAVE THE MAKEGOOD KEY                 
*                                                                               
         XC    KEY,KEY                    CLEAR THE KEY                         
         LA    R6,KEY                     ORDER KEY                             
         USING DOKEY,R6                   ORDER PATCH KEY                       
         MVI   DOKTYPE,DOKTYPQ            X'0D'                                 
         MVI   DOKSUBTY,DOKSTYPQ          X'34'                                 
         MVC   DOKAGMD,BAGYMD             A/M                                   
         MVC   DOKORDER,BINORDER          ORDER                                 
         MVC   DOKSTA,SVSTATN                                                   
         MVI   DOKCMT,DOKPSTAT            ORDER/PATCH RECORD TYPE               
*                                                                               
         MVC   AIO,AIO2                   ADD/GET RECORD INTO AIO2              
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            KEY ALREADY EXISTS?                   
         BE    XR10                       YES - GO UPDATE RECORD                
*                                                                               
         L     R6,AIO                     BUILD RECORD IN AIO2                  
         MVC   DOKEY,KEYSAVE              COPY THE KEY                          
         MVC   DORFRST(DOPMLNQ2),DOPMEL   MOVE THE X'04' ELEMENT IN             
         LLC   R1,DOPMLEN                 ELEMENT LENGTH                        
         AHI   R1,DORFRST-DOKEY           ADD RECORD KEY LENGTH                 
         STCM  R1,3,DORLEN                RECORD LENGTH                         
         MVC   DORAGY,AGENCY              AGENCY                                
         GOTO1 ADDREC                     ADD THE RECORD                        
         B     XR40                       EXIT                                  
*                                                                               
XR10     GOTO1 GETREC                     GET THE RECORD                        
*                                                                               
         L     R6,AIO2                    RECORD IN AIO2                        
         LA    R6,24(R6)                  PUT ELEMENT HERE                      
         XR    R1,R1                      CLEAR R1                              
*                                                                               
XR20     CLI   0(R6),0                    EOR?                                  
         BE    XR30                       YES - ELEMENT GOES HERE               
         CLI   0(R6),X'04'                ELEMENT HIGHER THAN X'04'?            
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
XR40     MVC   KEY,KEYSAVE                GET THE ORDER RECORD                  
         MVI   KEY+12,0                   CLEAR RECORD TYPE                     
         GOTO1 HIGH                       READ HIGH                             
         CLC   KEY(13),KEYSAVE            FOUND THE ORDER RECORD?               
         BE    *+6                        YES                                   
         DC    H'0'                       NO - DEATH                            
*                                                                               
         GOTO1 GETREC                     GET THE RECORD                        
*                                                                               
         LA    R3,SAVEKEY                 MAKEGOOD KEY                          
         USING MNKEY,R3                   MAKEGOOD KEY DSECT                    
         L     R6,AIO2                    RECORD IN AIO2                        
         MVI   ELCODE,MGCOLELQ            X'14' ELEMENT                         
         BAS   RE,GETEL                   GET THE ELEMENT                       
         B     *+8                        GO TEST CC                            
*                                                                               
XR45     BAS   RE,NEXTEL                  GET THE NEXT X'14' ELEMENT            
         BNE   XR50                       ELEMENT NOT FOUND                     
*                                                                               
         USING MGCOLELD,R6                MAKEGOOD COLOR ELEMENT                
         CLC   MGCOLCOD,MNKGROUP          MATCH ON MAKEGOOD CODE?               
         BNE   XR45                       NO - TRY NEXT ELEMENT                 
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)                                  
*                                                                               
         GOTO1 PUTREC                     PUT THE RECORD BACK                   
*                                                                               
XR50     MVC   AIO,AIO1                   RESTORE AIO                           
         J     EXIT                       EXIT                                  
         DROP  R3,R4,R6                   DROP R3,R4 AND R6                     
*                                                                               
***********************************************************************         
* DELETE THE SPOT OFFER RECORDS                                                 
***********************************************************************         
DELOFFER NTR1  LABEL=*                                                          
         L     R2,AIO1                                                          
         USING DAREMGND,R2                DARE MAKEGOOD DSECT                   
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
*                                                                               
         CLI   SVSTATN,X'E8'              CABLE?                                
         BNL   DLOF050                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOKEY,R4                                                         
         MVI   MOKTYPE,MOKTYPQ                                                  
         MVI   MOKSUBTY,MOKSTYPQ                                                
         MVC   MOKAGMD,BAGYMD                                                   
         MVC   MOKORDER,BINORDER                                                
         MVC   MOKMGCD,MNKGROUP                                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
DLOF010  CLC   KEY(MOKSEQ-MOKEY),KEYSAVE                                        
         JNE   DLOFX               EXIT WHEN FINISHED                           
*                                                                               
         BRAS  RE,DELSKEY                                                       
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         GOTO1 GETREC            SAVE                                           
*                                                                               
         BRAS  RE,DELSREC                                                       
*                                                                               
         GOTO1 SEQ                                                              
         J     DLOF010                                                          
******************                                                              
* DELETE THE XSPOT OFFER RECORDS                                                
******************                                                              
DLOF050  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING MOXKEY,R4                                                        
         MVI   MOXKTYPE,MOXKTYPQ                                                
         MVI   MOXKSBTY,MOXKSBTQ                                                
         MVC   MOXKAGMD,BAGYMD                                                  
         MVC   MOXKORDR,BINORDER                                                
         MVC   MOXKMGCD,MNXKGRP                                                 
         DROP  R4,R2                                                            
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
DLOF060  CLC   KEY(MOXKSTTN-MOXKEY),KEYSAVE                                     
         JNE   DLOFX               EXIT WHEN FINISHED                           
*                                                                               
         BRAS  RE,DELXKEY                                                       
*                                                                               
         MVI   DMINBTS,X'80'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,DELXREC                                                       
*                                                                               
         GOTO1 SEQ                                                              
         B     DLOF060                                                          
*                                                                               
DLOFX    J     EXIT                                                             
*************                                                                   
* DELETE SPOT RECORD                                                            
*************                                                                   
DELSREC  NTR1                                                                   
         OI    15(R6),X'80'        MARK SPOT DELETED                            
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 PUTREC                                                           
         J     EXIT                                                             
*************                                                                   
* DELETE X-SPOT RECORD                                                          
*************                                                                   
DELXREC  NTR1                                                                   
         OI    34(R6),X'80'        MARK XPOT REC DELETED                        
         XC    KEY,KEY                                                          
         MVC   KEY(32),0(R6)                                                    
         GOTO1 PUTREC                                                           
         J     EXIT                                                             
*************                                                                   
* DELETE SPOT KEY                                                               
*************                                                                   
DELSKEY  NTR1                                                                   
         MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         MVC   KEYSAVE,KEY                                                      
         JNE   *+2                                                              
*                                                                               
         OI    KEY+13,X'80'        DELETE THE KEY ALSO                          
         GOTO1 WRITE                                                            
         J     EXIT                                                             
*************                                                                   
* DELETE X-SPOT KEY                                                             
*************                                                                   
DELXKEY  NTR1                                                                   
         MVI   DMINBTS,X'80'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         MVC   KEYSAVE,KEY                                                      
         JNE   *+2                                                              
*                                                                               
         OI    KEY+32,X'80'        DELETE THE KEY ALSO                          
         GOTO1 WRITE                                                            
         J     EXIT                                                             
***********************************************************************         
* CONVERTS EBCDIC ORDER NUMBER TO BINARY ORDER NUMBER                 *         
* ON ENTRY:    PARAM 1             A(EBCDIC ORDER NUMBER)             *         
* ON EXIT:     BINORDER            BINARY ORDER NUMBER                *         
***********************************************************************         
BINORDR  NTR1                                                                   
         L     R2,0(R1)                                                         
*                                                                               
         CLI   1(R2),C'3'                                                       
         BNH   BNORDR10                                                         
         PACK  DUB,0(8,R2)         NEW STYLE ORDER NUMBER                       
         SP    DUB,=P'04000000'                                                 
         CVB   R1,DUB                                                           
         STCM  R1,15,BINORDER                                                   
         OI    BINORDER,X'80'                                                   
         XC    BINORDER,=4X'FF'                                                 
         J     EXIT                                                             
*                                                                               
BNORDR10 GOTO1 DATCON,DMCB,(5,0),(15,JDTTODAY)   GET CURRENT DATE               
*                                                                               
         ZAP   FULL,JDTTODAY       CENTURY/YEAR FROM TODAY'S DATE               
         SRP   FULL,61,0                                                        
*                                                                               
         GOTO1 HEXIN,DMCB,(R2),DUB,8   SAVE AS IF ENTRY WAS HEX                 
         MVC   PACKOF4B,DUB            CONVERT IT TO PACK                       
         OI    PACKOF4B+3,X'0F'                                                 
         SRP   PACKOF4B,58,0       ISOLATE THE YEAR                             
         MVC   PACKOF4B+2(1),FULL+2 COPY TODAY'S CENTURY                        
*                                                                               
         CP    PACKOF4B+3(1),FULL+3(1) LESS 10 YEARS?                           
         BNH   *+10                                                             
         SP    PACKOF4B,=P'10'     YEAR DIGIT>TODAY'S YEAR DIGIT                
*                                                                               
         SP    PACKOF4B,=P'90'     CALCULATE FROM 1990                          
         SRP   PACKOF4B,4,0                                                     
         OC    PACKOF4B+1(2),DUB   STICK IN DAYS IN YEAR                        
         SRP   PACKOF4B,63,0                                                    
*                                                                               
         ZAP   DUB,PACKOF4B        SAVE DATE PORTION                            
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDDT                                                    
         XC    BINORDDT,=4X'FF'                                                 
*                                                                               
         PACK  DUB,4(4,R2)         SAVE SEQUENCE PORTION                        
         CVB   R0,DUB                                                           
         STCM  R0,3,BINORDSQ                                                    
         XC    BINORDSQ,=4X'FF'                                                 
         J     EXIT                                                             
*                                                                               
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
SSV      NTR1                                                                   
         CLI   SVSTATN,X'E8'              CABLE?                                
         JL    EXITNO                                                           
                                                                                
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         J     EXITYES                                                          
                                                                                
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
                                                                                
RSV      NTR1                                                                   
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         J     EXIT                                                             
                                                                                
***********************************************************************         
* GET THE PFKEY INFORMATION                                           *         
***********************************************************************         
GETPF    NTR1                                                                   
*                                                                               
         GOTO1 INITIAL,DMCB,PFTABLE       INITIALIZE THE PFKEYS                 
*                                                                               
         J     EXIT                       RETURN                                
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
ERRSELF  LHI   RF,1395             MKGD MUST BE SELF APPLIED                    
         J     ER2EXIT             GO PROCESS THE ERROR                         
*                                                                               
ERRCAN   LHI   RF,1396             MKGD MUST BE OK/SELF APPLIED                 
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
ERRNOORD MVI   GERROR1,215         CANNOT FIND THAT ORDER                       
         J     ERREXIT                                                          
*                                                                               
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
*                                                                               
MKGDTAB  DS   0C                                                                
MKGD1    DC   AL1(MKGD2-*),C'A',C'APPROVED'                                     
MKGD2    DC   AL1(MKGD3-*),C'B',C'SELLER RECALLED'                              
MKGD3    DC   AL1(MKGD4-*),C'C',C'CANCELLED'                                    
MKGD4    DC   AL1(MKGD5-*),C'D',C'DELIVERED'                                    
MKGD5    DC   AL1(MKGD6-*),C'E',C'ERROR'                                        
MKGD6    DC   AL1(MKGD7-*),C'G',C'GOING TO BE OKAYED'                           
MKGD7    DC   AL1(MKGD8-*),C'H',C'ON HOLD FOR MANUAL OKAY'                      
MKGD8    DC   AL1(MKGD9-*),C'M',C'AMENDED'                                      
MKGD9    DC   AL1(MKGD10-*),C'N',C'NEW'                                         
MKGD10   DC   AL1(MKGD11-*),C'O',C'OKAYED'                                      
MKGD11   DC   AL1(MKGD12-*),C'R',C'REJECTED'                                    
MKGD12   DC   AL1(MKGDX-*),C'S',C'SELF APPLIED'                                 
MKGDX    DC   X'FF'                                                             
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
BINORDER DS    0XL4                BINARY ORDER NUMBER                          
BINORDDT DS    XL2                  DATE PORTION                                
BINORDSQ DS    XL2                  SEQUENCE PORTION                            
SVSTATN  DS    XL3                                                              
*                                                                               
         ORG   LSSD+L'SYSSPARE                                                  
*                                                                               
       ++INCLUDE SPOMSFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSB4D          (OUR PATCHSTA SCREEN)                        
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
* SPGENDRMKN                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRMKN                                                     
         PRINT ON                                                               
* SPGENDRMKO                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRMKO                                                     
         PRINT ON                                                               
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPOMS14   01/09/15'                                      
         END                                                                    
