*          DATA SET NEBUY18    AT LEVEL 027 AS OF 02/26/20                      
*PHASE T31118B,+0                                                               
*INCLUDE GETBROAD                                                               
*INCLUDE ADDAY                                                                  
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 027 05DEC19 <SPEC-39994> ALLOW UNITS TO USE THE COS2 FACTOR    *         
*                               FROM THE PACKAGE                      *         
***********************************************************************         
         TITLE 'NETPAK BUY PUP TRANSFER'                                        
T31118   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BY18*,RA,RR=RE                                               
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31118+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL WORKING STORAGE           
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
         LA    RE,PACKREC                                                       
         ST    RE,APACKREC                                                      
*                                                                               
*  TEST FOR FROZEN CLIENT                                                       
*                                                                               
         TM    CLIOPT2,X'08'                                                    
         BZ    MA100                                                            
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,CLIFRERR                                                    
         J     ERROR                                                            
         SPACE 2                                                                
MA100    CLI   ACTION,PD           TEST FOR DISPLAYING PACKAGE                  
         BE    MA500                                                            
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    MAERR                                                            
         CLC   BUYACT+3(61),SVACTLIN                                            
         BNE   MAERR                                                            
*                                                                               
         CLI   ACTION,PC           TEST FOR PACKAGE CHANGE                      
         BE    MA400                                                            
*                                                                               
         CLI   ACTION,PA           TEST FOR PACKAGE ADD                         
         BNE   MA300                                                            
         MVC   SVACTION,ACTION                                                  
*                                                                               
*!!!     BAS   RE,APACK                                                         
*!!!     GOTO1 =A(APACK),RR=MYRELO                                              
         BRAS  RE,APACK                                                         
*                                                                               
         CLI   BUYPROF+11,YES                                                   
         BNE   MA120               MANUEL PACKAGE                               
         BAS   RE,TESTPACK                                                      
         B     *+8                                                              
MA120    BAS   RE,GETPACK                                                       
         MVC   PACKAGE,PACK        SET SAVED PACKAGE                            
         ZIC   R2,PACK                                                          
         EDIT  (R2),(3,BUYPAK),ALIGN=LEFT                                       
         OI    BUYPAKH+6,X'80'     TRANSMIT NUMBER BACK                         
         L     RE,APACKREC                                                      
         MVC   NPKPACK-NPKEY(1,RE),PACK                                         
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,APACKREC                                
         MVI   FERN,PUPPACA                                                     
*                                                                               
* PC ACTION ENDS HERE                                                           
MA140    MVC   NBAIO,APACKREC      POINT TO NEW PACKAGE RECORD                  
         MVI   NBFUNCT,NBFVAL      FORCE VALUEING OF IT                         
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         GOTO1 DIS,(R1),APACKREC                                                
         LA    R2,BUYACTH                                                       
         MVI   9(R2),C'T'                                                       
         OI    6(R2),X'80'                                                      
         OI    1(R2),X'01'                                                      
         LA    R2,BUYACTH                                                       
         B     MAXIT                                                            
*                                                                               
*MA300    MVI   FERN,INVERR                                                     
MA300    DS    0H                                                               
         MVC   XTRA(16),=C'PT ACTION CHANGE'                                    
         CLI   SVACTION,PA                                                      
         BNE   INVERRM                                                          
         MVC   XTRA,SPACES                                                      
         BAS   RE,ASSREC           ADD UNIT RECORDS                             
         MVI   MODE,DISPLAY                                                     
         LA    R2,BUYACTH                                                       
         MVI   9(R2),C'D'          SET UP TO ACTION DISPLAY                     
         OI    6(R2),X'81'                                                      
         MVI   FERN,ALLUTRNS                                                    
*                                                                               
         OI    PUPCPMH+6,X'80'                                                  
         MVC   PUPCPM(42),=C'* REQUEST CPM REPORT TO CORRECT CPM FACT OX        
               R'                                                               
         B     MAXIT                                                            
         SPACE                                                                  
* PACKAGE CHANGE (ONLY RIGTH AFTER PACKAGE ADD)                                 
*MA400    MVI   FERN,INVERR                                                     
MA400    DS    0H                                                               
         CLI   SVACTION,PA                                                      
         BNE   INVERRM                                                          
*!!!     BAS   RE,APACK                                                         
*!!!     GOTO1 =A(APACK),RR=MYRELO                                              
         BRAS  RE,APACK                                                         
*                                                                               
         L     R4,APACKREC                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(L'NPKEY),0(R4)  RE-READ PACKAGE                              
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(L'NPKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         MVI   FERN,PUPPACC                                                     
         B     MA140                                                            
         SPACE                                                                  
MA500    GOTO1 VBLDRQST            GENERATE TURNAROUND REQUEST                  
         OI    MODE,DISPLAY                                                     
         BAS   RE,ACTED                                                         
         BAS   RE,PUPPKG                                                        
         GOTO1 DIS,DMCB,APACKREC   DISPLAY PACKAGE                              
         NI    MODE,X'FF'-DISPLAY  TURN OFF DISPLAY SETTING                     
*                                                                               
         LA    R2,PAKNAMEH                                                      
         MVI   FERN,PKGDIS                                                      
         LA    R2,BUYACTH                                                       
         MVI   9(R2),C'A'                                                       
         OI    6(R2),X'81'                                                      
*                                                                               
         CLC   SVXTRA,SPACES       CHECK IF SOME QTRS PREV UPLOADED             
         BE    MAXIT                                                            
         MVI   FERN,PREUPWAR       GIVE WARNING                                 
         MVC   XTRA(7),SVXTRA                                                   
         SPACE                                                                  
MAXIT    ST    R2,FADDR                                                         
         J     ERROR                                                            
*                                                                               
*MAERR    MVI   FERN,INVERR                                                     
MAERR    DS    0H                                                               
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     INVERRM                                                          
         EJECT                                                                  
* ROUTINE TO CREATE PACKAGE FROM PUP PLANNING RECORD                            
*                                                                               
PUPPKG   NTR1                                                                   
         L     R4,APACKREC                                                      
         LR    RE,R4                                                            
         LA    RF,PAGELEN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR I/O AREA                               
         USING NPRECD,R4                                                        
*                                                                               
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,AGYMED                                                     
         MVC   NPKCLT,CLIPK                                                     
         MVC   NPKNET,NET                                                       
         MVC   NPKEST,EST                                                       
         MVC   NPKPACK,PACK        SET PACKAGE NUMBER IN KEY                    
*                                                                               
         MVC   NPAKEL(2),=XL2'013C'                                             
         MVI   NPKRLEN,60+27+1                                                  
*                                                                               
         L     R3,AIOAREA2         R3 = PLANNING RECORD                         
         USING NPLRECD,R3                                                       
*--CHECK IF IT'S A CABLE PLAN                                                   
         TM    NPLNOPTS,X'80'                                                   
         BZ    *+12                                                             
         MVI   FERN,CBLPLERR                                                    
         J     ERROR                                                            
*--MOVE OUT GUARANTEE FACTOR INFO                                               
***      MVC   PACKGU,NPLNADJA                                                  
         MVC   NPACKGU,NPLNADJP    NEW PACKAGE GUARANTEE                        
         MVC   NDEMOCAT,NPLNDEMS   DEMO FOR NEW DEMO ADJ (3 BYTES)              
**                                 NEW DEMO ADJ FACTOR=X'05' ELEM               
***      MVC   DEMOGU,NPLNADJD     OLD DEMO ADJ                                 
***      MVC   DEMOCAT,NPLNDEMS+2                                               
*--MOVE OUT PACKAGE RECORD INFO                                                 
         MVC   NPAKNAME,NPLNNAME                                                
         MVC   NPAKDP,NPLKDPT                                                   
         MVC   NPAKHUTA,NPLNHAVE                                                
         MVC   NPAKUNCD,NPLNUNIV                                                
         MVC   NPAKHUTS,NPLNHTSC                                                
         MVC   NPAKHTYP,NPLNHTBT                                                
         MVC   NPAKHPCT,NPLNHTPO                                                
         MVC   NPAKHUTF,NPLNHTFL                                                
         MVC   NPAKZONE,NPLNZONE                                                
         MVC   NPAKGCPM,NPLNGCPM                                                
         LA    R3,NPLNEL-NPLKEY(R3)                                             
PD100    CLI   0(R3),0                                                          
         BE    PD200                                                            
         CLI   0(R3),X'05'             NEW GENERAL ELEMENT                      
         BNE   PD110                                                            
         MVC   NDEMOGU,NPNADJD-NPNELEM(R3) NEW DEMO ADJ                         
         OC    NPNDEMO-NPNELEM(3,R3),NPNDEMO-NPNELEM(R3)   DEMO CODE            
         BZ    *+10                                                             
         MVC   NDEMOCAT,NPNDEMO-NPNELEM(R3)                                     
*                                                                               
PD110    CLI   0(R3),X'04'              BUDGET DOLLARS                          
         BNE   PD180                                                            
PD120    ZIC   RF,3(R3)                                                         
         LA    RE,SVQTBLS-1(RF)                                                 
         CLI   0(RE),0                                                          
         BE    PD180                                                            
         CLI   SVLEN1,0                                                         
         BE    PD160                                                            
         CLC   SVLEN1,NPBSEC-NPBELD(R3)                                         
         BE    PD160                                                            
         CLI   SVLEN2,0                                                         
         BE    PD160                                                            
         CLC   SVLEN2,NPBSEC-NPBELD(R3)                                         
         BNE   PD180                                                            
PD160    ICM   RE,15,NPAKCOST                                                   
         A     RE,NPBUDGET-NPBELD(R3)                                           
         STCM  RE,15,NPAKCOST                                                   
PD180    ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     PD100                                                            
*                                                                               
PD200    MVC   NBAIO,APACKREC      POINT TO NEW PACKAGE RECORD                  
         MVI   NBFUNCT,NBFVAL      FORCE VALUEING OF IT                         
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* DISPLAY PACKAGE VALUES ON SCREEN (P1=A(PACKAGE RECORD))                       
*                                                                               
DIS      ST    RE,SAVEREG                                                       
         L     R4,0(R1)            R4 POINTS TO PACKAGE RECORD                  
         USING NPRECD,R4                                                        
         GOTO1 VCLEARF,DMCB,PAKNAMEH,PAKLAST                                    
*                                                                               
         MVC   PAKNAME,NPAKNAME                                                 
*--DAYPART                                                                      
         MVC   PAKDPN,NBACTNDP                                                  
         MVC   PAKDPEX(8),NBDPNAM                                               
         MVC   PAKDPEX+8(6),NBDPNAM2                                            
         OI    PAKDPEXH+6,X'80'     TRANSMIT NAME BACK                          
*                                                                               
         TM    NPAKSTAT,FROZEN                                                  
         BZ    *+14                                                             
         MVC   PAKSTAT(6),=C'FROZEN'                                            
         B     DIS1                                                             
*                                                                               
         TM    NPAKSTAT,LOCKED                                                  
         BZ    DIS1                                                             
         MVC   PAKSTAT(6),=C'LOCKED'                                            
         SPACE                                                                  
DIS1     LA    RE,PAKOPT                                                        
         LR    RF,RE                                                            
         TM    NPAKSTAT,NOPRINT                                                 
         BZ    DIS1A                                                            
         MVC   PAKOPT(8),=C'PRINT=NO'                                           
         LA    RE,8(RE)                                                         
DIS1A    OC    NPAKZONE,NPAKZONE                                                
         BZ    DIS2                                                             
         CR    RE,RF                                                            
         BE    DIS1B                                                            
         MVI   0(RE),COMMA                                                      
         LA    RE,1(RE)                                                         
DIS1B    MVC   0(5,RE),=C'ZONE='                                                
         MVC   5(1,RE),NPAKZONE                                                 
         SPACE 1                                                                
DIS2     EDIT  (B4,NBPAKCST),(8,PAKCOST),ALIGN=LEFT,ZERO=NOBLANK                
         LA    R2,PAKINT           INTEGRATION RATE                             
         TM    MODE,DISPLAY        YES-TEST FOR FORCED DISPLAY                  
         BZ    *+14                DISPLAY PACKAGE TO BE COPIED FIRST           
         OC    NPAKINT,NPAKINT                                                  
         BZ    DIS4                                                             
         EDIT  (B4,NPAKINT),(6,(R2)),2,ALIGN=LEFT                               
         TM    NPAKCNTL,X'80'                                                   
         BNO   *+14                                                             
         MVC   0(3,R2),=C'TBL'                                                  
         LA    R0,3                                                             
         AR    R2,R0               POINT PAST RATE                              
         TM    NPAKSTAT,X'04'      TEST FOR NON-COMMISSIONABLE                  
         BZ    DIS4                                                             
         MVI   0(R2),COMMA         TACK ON ',N'                                 
         MVI   1(R2),C'N'                                                       
         SPACE                                                                  
DIS4     MVI   PAKDBSE,C'V'        DEMO BASE                                    
         TM    NPAKCNTL,X'40'                                                   
         BZ    *+8                                                              
         MVI   PAKDBSE,C'I'                                                     
*                                                                               
         CLI   NPAKHUTS,0          HUT SCHEME                                   
         BE    *+10                                                             
         MVC   PAKHSC(1),NPAKHUTS                                               
         CLI   NPAKHUTA,0          HUT AVERAGE                                  
         BE    *+16                                                             
         MVC   PAKHAVE(1),NPAKHUTA                                              
         MVC   PAKHAVE+1(1),NPAKHUTF                                            
         OC    NPAKHPCT,NPAKHPCT   TEST HUT ADJ PCT.                            
         BZ    DIS4A                                                            
         SR    R2,R2                                                            
         ICM   R2,3,NPAKHPCT                                                    
         EDIT  (R2),(6,PAKHADJ),2,ALIGN=LEFT                                    
DIS4A    OC    NPAKGCPM,NPAKGCPM   TEST GUAR. CPM.                              
         BZ    DIS4B                                                            
         SR    R2,R2                                                            
         ICM   R2,15,NPAKGCPM                                                   
         EDIT  (R2),(6,PAKGCPM),2,ALIGN=LEFT                                    
DIS4B    CLI   NPAKHTYP,0          HUT TYPE                                     
         BE    *+10                                                             
         MVC   PAKHTYP,NPAKHTYP                                                 
         OC    NPAKUNIV,NPAKUNIV   TEST UNIVERSE PCT.                           
         BZ    DIS5                                                             
         SR    R2,R2                                                            
         ICM   R2,3,NPAKUNIV                                                    
         EDIT  (R2),(10,PAKUPCT),2,ALIGN=LEFT                                   
         SPACE                                                                  
DIS5     OC    NPAKUNCD,NPAKUNCD   UNIVERSE CODE                                
         BZ    DIS6                                                             
         MVO   THREE,NPAKUNCD                                                   
         OI    THREE+2,X'0F'                                                    
         EDIT  (P3,THREE),(4,PAKUCOD),ALIGN=LEFT                                
         SPACE                                                                  
DIS6     OC    NPAKFEED,NPAKFEED                                                
         BZ    DIS7                                                             
         MVC   HALF,NPAKFEED                                                    
         LH    R2,HALF                                                          
         EDIT  (R2),(10,PAKFPCT),2,ALIGN=LEFT                                   
         SPACE                                                                  
*        OC    NPAKFMG,NPAKFMG     TEST FOR FEED MKT GROUP                      
*        BZ    DIS7A                                                            
*        MVC   PAKFMGR(1),NPAKFMG                                               
*        UNPK  DUB(3),NPAKFMG+1(2)                                              
*        MVC   PAKFMGR+1(2),DUB                                                 
*                                                                               
DIS7     CLI   NPAKMAST,0                                                       
         BE    DIS8                                                             
*                                                                               
         LA    R0,255                                                           
         LA    RE,CLILIST          POINT TO PRODUCT CODE LIST                   
         CLC   NPAKMAST,3(RE)      TEST ON PRODUCT NUMBER                       
         BE    *+14                                                             
         LA    RE,4(RE)            NEXT ENTRY                                   
         BCT   R0,*-14                                                          
         DC    H'0'                CANNOT FIND PRODUCT NUMBER                   
*                                                                               
         MVC   PAKMALL(3),0(RE)    PRODUCT CODE                                 
         SPACE                                                                  
DIS8     SR    R2,R2               IMPACT PERCENTAGE                            
         ICM   R2,3,NPAKIMP                                                     
         BZ    DIS9                                                             
         EDIT  (R2),(7,PAKIMPC),2,ALIGN=LEFT                                    
         SPACE                                                                  
DIS9     OC    NPAKSREP,NPAKSREP   TEST FOR SPECIAL REP                         
         BZ    DIS10                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,NPAKSREP                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PAKSREP,DUB         DISPLAY CHARACTER OUTPUT                     
         SPACE                                                                  
DIS10    LA    R2,PAKLSTA                                                       
         XC    PAKLSTA,PAKLSTA                                                  
         OI    PAKLSTAH+6,X'80'                                                 
         OC    NPAKACTD,NPAKACTD                                                
         BZ    DIS12                                                            
         GOTO1 VDATCON,DMCB,(2,NPAKACTD),(8,(R2))                               
         LA    R2,9(R2)                                                         
         CLI   NPAKACTA,C'A'                                                    
         BNE   DIS11                                                            
         MVC   0(3,R2),=C'ADD'                                                  
         LA    R2,4(R2)                                                         
         B     DIS12                                                            
         SPACE                                                                  
DIS11    CLI   NPAKACTA,C'C'                                                    
         BNE   DIS12                                                            
         MVC   0(6,R2),=C'CHANGE'                                               
         LA    R2,7(R2)                                                         
         SPACE                                                                  
DIS12    ICM   RE,15,APAKFEL                                                    
         BZ    DIS13                                                            
         MVC   PAKFILT,3(RE)                                                    
         SPACE                                                                  
DIS13    DS    0H                                                               
         MVI   PAKPOSD,C'D'        DEFAULT                                      
         OC    NPAKPDT,NPAKPDT                                                  
         BZ    DIS15                                                            
         MVC   PAKPOSD,NPAKPDT                                                  
         SPACE                                                                  
*IS14    ICM   R3,15,NPAKGCPM                                                   
*        BZ    DIS15                                                            
*        EDIT  (R3),(7,PAKGCPM),2,ALIGN=LEFT                                    
*                                                                               
         DROP  R4                                                               
DIS15    DS    0H                  DISPLAY BUY TYPE                             
         LA    R4,87(R4)           POINT TO FIRST ELEM AFTER X'01'              
         USING NPK2D,R4                                                         
*                                                                               
         CLI   0(R4),X'02'         IS IT AN X'02' ELEM?                         
         BNE   DIS20                                                            
*                                                                               
         MVC   PAKBTYP,NPK2BTYP    DISPLAY BUY TYPE                             
         DROP  R4                                                               
*                                                                               
DIS20    CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DSXIT                                                            
         TM    MODE,DISPLAY                                                     
         B     DSXIT                                                            
*        BO    DSXIT                                                            
*                                                                               
         LA    R4,KEY              READ PACKAGE DIRECTORY ENTRY                 
         USING NPRECD,R4                                                        
*                                                                               
         XC    KEY,KEY             TO OBTAIN DISK ADDRESS                       
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,AGYMED                                                     
         MVC   NPKCLT,CLIPK                                                     
         MVC   NPKNET,NET                                                       
         MVC   NPKEST,EST                                                       
         MVC   NPKPACK,PACK                                                     
         GOTO1 AIO,DMCB,UNT+DIR+READ                                            
         MVC   0(3,R2),=C'DA='                                                  
         GOTO1 VHEXOUT,DMCB,NDXDA,3(R2),L'NDXDA,0                               
         SPACE                                                                  
DSXIT    L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ ASSIGNMENT RECORDS, GET # UNITS & DATES TO ADD            
*                                                                               
ASSREC   NTR1                                                                   
         MVI   PRGSW,C'Y'                                                       
         MVC   KEY(20),SVUAKEY                                                  
         OC    SVUAKEY,SVUAKEY                                                  
         BNZ   AS100                                                            
         MVI   KEY,X'22'                                                        
         MVC   KEY+1(1),AGYMED                                                  
         MVC   KEY+2(2),CLIENT                                                  
         MVC   KEY+4(4),NET                                                     
         MVC   KEY+8(1),PLNDPT                                                  
         MVC   KEY+9(4),PLAN                                                    
AS100    LA    R0,UNT+DIR+HIGH                                                  
         B     *+8                                                              
AS120    LA    R0,UNT+DIR+SEQ                                                   
         GOTO1 AIO,DMCB,(R0)                                                    
         SPACE                                                                  
         CLC   KEY(NPUKPROG-NPUKEY),KEYSAVE                                     
         BNE   AS700                                                            
         CLI   PLHAVE,C'W'                                                      
         BNE   AS150                                                            
         ZIC   RF,KEY+19           CHECK IF VALID QUARTER                       
         CLI   KEY+19,0                                                         
         BNE   AS140                                                            
         LA    RF,4                                                             
AS140    LA    RE,SVQTBLS-1(RF)                                                 
         CLI   0(RE),0                                                          
         BE    AS120                                                            
AS150    CLI   SVUAKEY,0                                                        
         BE    AS600                                                            
         CLI   PRGSW,C'Y'                                                       
         BNE   AS600                                                            
         MVI   PRGSW,C'N'                                                       
         CLI   PUPYES,C'Y'                                                      
         BNE   AS120                                                            
         LA    R4,IOAREA5                                                       
         GOTO1 AIO,DMCB,UNT+FILE+GET,(R4)                                       
         USING NPURECD,R4                                                       
         MVC   PROG,NPUKPROG                                                    
         DROP  R4                                                               
*                                                                               
AS200    LA    R4,IOAREA5                                                       
         LA    R4,NPGDEL-NPUKEY(R4)                                             
AS220    CLI   0(R4),0                                                          
         BE    AS500                                                            
         CLI   0(R4),X'02'                                                      
         BE    AS225                                                            
         CLI   0(R4),X'12'                                                      
         BNE   AS400                                                            
AS225    MVC   BYTE,3(R4)                                                       
         MVC   PUPDEMS,16(R4)                                                   
         CLI   PLHAVE,C'Q'                                                      
         BE    AS260                                                            
         CLI   PLHAVE,C'W'                                                      
         BE    AS270                                                            
         MVC   DUB+1(1),3(R4)                                                   
         MVI   DUB+2,X'01'         START WITH THE 1ST                           
         MVI   BYTE,1                                                           
         CLI   3(R4),4             FIND CORRECT QUATER                          
         BL    AS240                                                            
         MVI   BYTE,2                                                           
         CLI   3(R4),7                                                          
         BL    AS240                                                            
         MVI   BYTE,3                                                           
         CLI   3(R4),9                                                          
         BL    AS240                                                            
         BNE   *+14                                                             
         CLC   PLANYR,2(R4)                                                     
         BE    AS240                                                            
         MVI   BYTE,4                                                           
AS240    ZIC   RE,PLANYR                                                        
         CLI   BYTE,4                                                           
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK)                                    
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+6,VGETDAY,VADDAY,       X        
               RR=MYRELO                                                        
         CLI   3(R4),9             IF SEP CAN BE 4TH OR 3RD QTR                 
         BNE   AS244                                                            
         CLC   PLANYR,2(R4)                                                     
         BE    AS242                                                            
         LA    R0,14                                                            
         GOTO1 VADDAY,(R1),WORK+6,WORK+6,(R0)                                   
         B     AS244                                                            
*                                                                               
AS242    LA    R0,13                                                            
         GOTO1 VADDAY,(R1),WORK+12,WORK+12,(R0)                                 
AS244    GOTO1 VDATCON,(R1),(0,WORK+6),(3,DUB)                                  
         GOTO1 VDATCON,(R1),(0,WORK+12),(3,DUB+3)                               
         B     AS280                                                            
*                                                                               
AS260    ZIC   RF,BYTE                                                          
         MH    RF,=H'2'                                                         
         LA    RF,QTBL1-2(RF)                                                   
         MVC   DUB+1(1),0(RF)                                                   
         MVC   DUB+4(1),1(RF)                                                   
         MVI   DUB+2,1                                                          
         MVI   DUB+5,1                                                          
         ZIC   RE,PLANYR                                                        
         CLI   BYTE,4                                                           
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
         STC   RE,DUB+3                                                         
         GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK)                                    
         GOTO1 VDATCON,(R1),(3,DUB+3),(0,WORK+6)                                
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+12,VGETDAY,VADDAY,      X        
               RR=MYRELO                                                        
         CLI   BYTE,4                4TH QTR                                    
         BNE   AS262                                                            
         LA    R0,14                                                            
         GOTO1 VADDAY,(R1),WORK+12,WORK+12,(R0)                                 
AS262    GOTO1 VDATCON,(R1),(0,WORK+12),(3,DUB)                                 
         SPACE                                                                  
* NOW GET END DATE                                                              
         GOTO1 =V(GETBROAD),(R1),(1,WORK+6),WORK+12,VGETDAY,VADDAY,    X        
               RR=MYRELO                                                        
         CLI   BYTE,3              3RD QTR                                      
         BNE   AS264                                                            
         LA    R0,13                                                            
         GOTO1 VADDAY,(R1),WORK+12,WORK+18,(R0)                                 
AS264    GOTO1 VDATCON,(R1),(0,WORK+18),(3,DUB+3)                               
         B     AS280                                                            
*                                                                               
AS270    LA    RE,IOAREA5          FIND PROPER QUARTER                          
         MVI   BYTE,1                                                           
         CLI   19(RE),1                                                         
         BE    AS273                                                            
         MVI   BYTE,2                                                           
         CLI   19(RE),2                                                         
         BE    AS273                                                            
         MVI   BYTE,3                                                           
         CLI   19(RE),3                                                         
         BE    AS273                                                            
         MVI   BYTE,4                                                           
         CLI   19(RE),0                                                         
         BE    AS273                                                            
AS273    GOTO1 VDATCON,DMCB,(2,2(R4)),(0,WORK+6)                                
         LA    R0,6                                                             
         GOTO1 VADDAY,(R1),WORK+6,WORK+12,(R0)                                  
         GOTO1 VDATCON,(R1),(0,WORK+6),(3,DUB)                                  
         GOTO1 VDATCON,(R1),(0,WORK+12),(3,DUB+3)                               
*                                                                               
AS280    ZIC   RF,BYTE                                                          
         LA    RE,SVQTBLS-1(RF)                                                 
         CLI   0(RE),0                                                          
         BE    AS400                                                            
         CLI   PLHAVE,C'W'                                                      
         BE    AS290                                                            
         CLC   DUB(3),SVSTDTE                                                   
         BNL   *+10                                                             
         MVC   DUB(3),SVSTDTE                                                   
         CLC   DUB+3(3),SVENDTE                                                 
         BNH   *+10                                                             
         MVC   DUB+3(3),SVENDTE                                                 
AS290    EQU   *                                                                
         CLI   0(R4),X'02'                                                      
         BNE   AS295                                                            
         USING NPUAD,R4                                                         
         MVC   OVSHR,NPUASHR                                                    
         MVC   OVHUT,NPUAHUT                                                    
         MVC   OVRTG,NPUARTG                                                    
         MVC   OVVPH,NPUAVOVR                                                   
         MVC   OVRBITS,NPUAOVRD                                                 
         MVC   UNTNUM,NPUAUNS                                                   
         XC    ACTCOSTS,ACTCOSTS                                                
         B     AS300                                                            
*                                                                               
AS295    EQU   *                                                                
         CLI   0(R4),X'12'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NPUBD,R4                                                         
         MVC   OVSHR,NPUBSHR                                                    
         MVC   OVHUT,NPUBHUT                                                    
         MVC   OVRTG,NPUBRTG                                                    
         MVC   OVVPH,NPUBVOVR                                                   
         MVC   OVRBITS,NPUBOVRD                                                 
         NI    OVRBITS,X'EF'                                                    
         MVC   UNTNUM,NPUBUNS                                                   
         MVC   ACTCOSTS,NPUBAMT                                                 
*                                                                               
AS300    CLI   SVLEN1,0                                                         
         BE    AS340                                                            
         LA    R0,4                FIND LENGTH                                  
         LA    RF,LENS                                                          
         LA    R1,UNTNUM                                                        
AS310    CLC   SVLEN1,0(RF)                                                     
         BE    AS320                                                            
         CLI   SVLEN2,0                                                         
         BE    *+14                                                             
         CLC   SVLEN2,0(RF)                                                     
         BE    AS320                                                            
         MVI   0(R1),0             DOESN'T MATCH LENGTH FILTER                  
AS320    LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,AS310                                                         
*                                                                               
AS340    OC    UNTNUM,UNTNUM                                                    
         BZ    AS400                                                            
         BAS   RE,GETDTES                                                       
         BAS   RE,ADDUNTS                                                       
         XC    NUMWKS(3),NUMWKS    3 FIELDSNUMWKS/NUMUNAD/MUNPUAD               
*                                                                               
AS360    ZIC   RF,UNITS                                                         
         L     R0,PUNITS                                                        
         AR    R0,RF                                                            
         ST    R0,PUNITS                                                        
AS400    ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     AS220                                                            
*                                                                               
AS500    MVC   KEY,SVUAKEY                                                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   SVUAKEY,KEY                                                      
         BE    AS120                                                            
         DC    H'0'                                                             
*                                                                               
AS600    LA    R2,PUPNPRGH                                                      
         OI    6(R2),X'80'                                                      
         MVC   8(6,R2),KEY+13                                                   
         LA    R2,PUPYESH                                                       
         OI    6(R2),X'81'                                                      
         MVC   8(3,R2),=C'YES'                                                  
         ST    R2,FADDR                                                         
         CLI   SVUAKEY,0                                                        
         BE    *+8                                                              
         BAS   R3,LISUNIT                                                       
         MVC   SVUAKEY,KEY                                                      
         BAS   RE,UPDPLAN                                                       
         MVI   FERN,PRGUTRNS                                                    
         J     ERROR                                                            
*                                                                               
AS700    LA    R2,PUPNPRGH                                                      
         OI    6(R2),X'80'                                                      
         MVC   8(6,R2),=6CL1'*'                                                 
         BAS   R3,LISUNIT                                                       
         XC    SVAREA18,SVAREA18   ALL DONE CLEAR CHANGES                       
ASXIT    B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* OUTPUT # UNITS PER PROGRAM TO LINE                                            
LISUNIT  L     R0,PUNITS                                                        
         LA    R2,PUPSUMH                                                       
         OI    6(R2),X'80'                                                      
         EDIT  (R0),(4,8(R2)),ALIGN=LEFT                                        
         MVC   13(18,R2),=C'UNITS TRANSFER FOR'                                 
         MVC   32(6,R2),SVUAKEY+13                                              
         BR    R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A UNIT RECORD FOR EACH ENTRY IN SCHEDULE LIST            
*                                                                               
GETDTES  NTR1                                                                   
         GOTO1 VDATCON,DMCB,(3,DUB),(2,HALF)                                    
         GOTO1 VGETPROG,DMCB,HALF  GET 1ST PROGRAM TO CALC DATES                
         CLI   FERN,0                                                           
         BNE   AUERR                                                            
         GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK+6)                                  
         GOTO1 VDATCON,(R1),(3,DUB+3),(0,WORK+12)                               
         GOTO1 VGETDAY,(R1),WORK+6,THREE                                        
         ZIC   R2,0(R1)                                                         
         L     R3,APROGEL                                                       
         USING NPGEL92,R3                                                       
         ZIC   R0,NPGDAYNO         DAY NUMBER OF UNIT'S DAY                     
         SRL   R0,4                USE THE START DAY'S NUMBER                   
         MVC   FSTDTE,WORK+6       SET EBCDIC AIR DATE                          
         SR    R0,R2               DIFFERENCE BETWEEN THE TWO DAYS              
         BZ    GD100               THERE IS NONE                                
         BP    *+8                                                              
         AH    R0,=H'7'                                                         
         GOTO1 VADDAY,(R1),WORK+6,FSTDTE,(R0)                                   
*                                                                               
GD100    CLC   FSTDTE,WORK+12                                                   
         BNH   GD120                                                            
         MVI   FERN,UNTNOTAD       NO UNITS FOR THIS PERIOD                     
         J     ERROR                                                            
GD120    LA    R0,4                                                             
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         LA    R3,UNTNUM                                                        
GD140    IC    RF,0(R3)                                                         
         AR    RE,RF                                                            
         LA    R3,1(R3)                                                         
         BCT   R0,GD140                                                         
         STC   RE,UNITS                                                         
         SR    R4,R4                                                            
         SR    R2,R2                                                            
         B     GD220                                                            
GD200    GOTO1 VADDAY,DMCB,FSTDTE,WORK,(R2)                                     
         CLC   WORK,WORK+12                                                     
         BH    GD240                                                            
GD220    LA    R2,7(R2)                                                         
         LA    R4,1(R4)                                                         
         B     GD200                                                            
*                                                                               
GD240    STC   R4,NUMWKS                                                        
         XC    FULL,FULL                                                        
         STC   R4,FULL+3                                                        
         SR    RE,RE                                                            
         ZIC   RF,UNITS                                                         
         D     RE,FULL                                                          
         STC   RF,NUMUNAD                                                       
         STC   RE,NUMPUAD                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE PLAN RECORDS TO SHOW WHICH                              
* WHICH QTRS WERE TRANSFERRED.                                                  
*                                                                               
UPDPLAN  NTR1                                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'20'                                                        
         MVC   KEY+1(1),AGYMED                                                  
         MVC   KEY+2(2),CLIENT                                                  
         MVC   KEY+5(4),NET                                                     
         MVC   KEY+10(1),PLNDPT                                                 
         MVC   KEY+11(4),PLAN                                                   
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                 HAS TO BE THERE                             
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         L     R3,AIOAREA2                                                      
         USING NPLNEL2,R2                                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',0(R3))                         
         CLI   12(R1),0                                                         
         BNE   UPDPL50                                                          
         L     R2,12(R1)                                                        
         MVC   NPL2UPST,PLNUPLST                                                
         B     UPDPLEX                                                          
*                                                                               
UPDPL50  LA    R2,WORK                                                          
         XC    WORK(30),WORK                                                    
         MVC   WORK(3),=XL3'081E'                                               
         MVC   NPL2UPST,PLNUPLST                                                
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA2,WORK,0                       
*                                                                               
UPDPLEX  GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA2                                   
         B     EXXMOD                                                           
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO BUILD A UNIT RECORD FOR EACH PROGRAM ASS. RECORD               
*                                                                               
ADDUNTS  NTR1                                                                   
         OC    VEDIT,VEDIT                                                      
         BNZ   AU100                                                            
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB                                                       
*                                                                               
AU100    GOTO1 VDATCON,DMCB,(0,FSTDTE),(2,NXTDTE)                               
         GOTO1 VDATCON,(R1),(0,ENDDTE),(2,ENDDTE2)                              
*                                                                               
         LA    R4,BLOCK            ** R4 = UNIT BLOCK **                        
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         MVC   UNALOCAL,AIOAREA2                                                
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'80'      TEST IF INTEGRATION TABLE LOOK UP            
         BNO   AU200                                                            
         DROP  RE                                                               
         LA    RF,INTGTBL                                                       
         ST    RF,UNINGTBL         PRGSW BACK INTG RATES HERE                   
         LA    RF,SVINTTBL                                                      
         ST    RF,INTHLDIT                                                      
         MVI   INTGREAD,0          DO 1ST READ                                  
         MVC   INTGAIO,AIOAREA3                                                 
         MVC   INTGSTDT,NXTDTE                                                  
         ZIC   RE,UNITS                                                         
         BCTR  RE,0                                                             
         MH    RE,=H'6'                                                         
         AR    RE,R2                                                            
         MVC   INTGEDDT,ENDDTE2                                                 
         SPACE                                                                  
AU200    L     R3,AIOAREA1                                                      
         LR    RE,R3                                                            
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING NURECD,R3                                                        
         MVI   NUKTYPE,X'04'                                                    
         MVC   NUKAM,AGYMED                                                     
         MVC   NUKCLT,CLIPK                                                     
         MVC   NUKDATE,NXTDTE                                                   
         MVC   NUKNET,NET                                                       
         MVC   NUKPROG,PROG                                                     
         MVC   NUKEST,EST                                                       
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVC   NUKDP,NPAKDP                                                     
         DROP  RE                                                               
         SPACE                                                                  
         GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
         SPACE                                                                  
         GOTO1 VGETPROG,DMCB,NUKDATE                                            
         CLI   FERN,0                                                           
         BNE   AUERR               ERROR ROUTINE                                
         SPACE                                                                  
         MVC   INTGSTDT,NUKDATE                                                 
         GOTO1 VEDIT,DMCB,(C'N',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+22                TEMP FIX TO BYPASS BAD PLANS***              
         CLI   UNERROR,STERR                                   ***              
         BE    EXXMOD                                          ***              
         MVC   FERN,UNERROR                                                     
         J     ERROR                                                            
*                                                                               
         GOTO1 VEDIT,DMCB,(C'D',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+22                TEMP FIX TO BYPASS BAD PLANS***              
         CLI   UNERROR,STERR                                   ***              
         BE    EXXMOD                                          ***              
         MVC   FERN,UNERROR                                                     
         B     AUERR                                                            
         GOTO1 (RF),(R1),(C'F',(R4))                                            
         SPACE                                                                  
*--CREATE DEFAULT TRAFFIC 21 ELEMENT                                            
         XC    ELMWORK,ELMWORK                                                  
         MVI   ELMWORK,X'21'                                                    
         MVI   ELMWORK+1,80                                                     
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,ELMWORK,0                    
         SPACE                                                                  
*--CREATE TVQ BOOK ELEMENT                                                      
         OC    PLNTVQBK,PLNTVQBK                                                
         BZ    AU260                                                            
         GOTO1 VHELLO,DMCB2,(C'D',UNTFILE),(X'60',(R3)),(1,=C'J')               
         GOTO1 VDATCON,DMCB,(2,PLNTVQBK),(0,DUB)                                
*                                                                               
         GOTO1 VNETWEEK,DMCB2,DUB,VGETDAY,VADDAY                                
         XC    ELMWORK,ELMWORK                                                  
         MVI   ELMWORK,X'60'                                                    
         MVI   ELMWORK+1,7                                                      
         MVI   ELMWORK+2,C'J'                                                   
         MVC   ELMWORK+3(1),4(R1)                                               
         MVC   ELMWORK+4(1),8(R1)                                               
         MVC   ELMWORK+5(2),PLNTVQBK                                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,ELMWORK,0                    
         SPACE                                                                  
*--CREATE SECONDARY 01 ELEMENT SET PUP CREATION INDICATOR                       
AU260    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         ICM   RE,15,12(R1)                                                     
         OI    2(RE),X'10'                                                      
*--SET POSTING TYPE CONTROL BIT                                                 
         MVI   CTPOSTYP,0                                                       
***      CLI   12(RE),C'N'       NETWORK                                        
         CLI   NETTRTYP,C'N'     NETWORK                                        
         BE    AU280                                                            
***      CLI   12(RE),C'C'       CABLE                                          
         CLI   NETTRTYP,C'C'     CABLE                                          
         BNE   *+12                                                             
         OI    CTPOSTYP,X'01'                                                   
         B     AU280                                                            
***      CLI   12(RE),C'S'       SYNDICATION                                    
         CLI   NETTRTYP,C'S'     SYNDICATION                                    
         BNE   *+12                                                             
         OI    CTPOSTYP,X'02'                                                   
         B     AU280                                                            
         OI    CTPOSTYP,X'03'      OTHER                                        
         SPACE                                                                  
AU280    TM    OVRBITS,X'F0'                                                    
         BZ    AU390                                                            
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),=XL6'DD0C00000001'                                       
         TM    OVRBITS,X'80'                                                    
         BZ    AU300                                                            
         MVI   WORK+4,C'S'                                                      
         MVC   WORK+10(2),OVSHR                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU300    TM    OVRBITS,X'40'                                                    
         BZ    AU320                                                            
         MVI   WORK+4,C'P'                                                      
         MVC   WORK+10(2),OVHUT                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU320    TM    OVRBITS,X'20'                                                    
         BZ    AU340                                                            
         MVI   WORK+4,C'R'                                                      
         MVC   WORK+10(2),OVRTG                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
AU340    TM    OVRBITS,X'10'                                                    
         BZ    AU390                                                            
         MVI   WORK+4,C'V'                                                      
         MVC   WORK+10(2),OVVPH                                                 
         ZIC   R0,ESTNDEMS                                                      
         LA    RE,ESTDEMSE                                                      
AU360    CLC   2(1,RE),TRGDEMO                                                  
         BE    AU380                                                            
         LA    RE,3(RE)                                                         
         BCT   R0,AU360                                                         
         B     AU390                                                            
*                                                                               
AU380    MVC   WORK+5(1),TRGDEMO                                                
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
*                                                                               
AU390    TM    OVRBITS,X'40'                                                    
         BNZ   AU400                                                            
         CLI   CALCHUT,C'N'                                                     
         BE    AU400                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'35',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     R2,12(R1)                                                        
         ICM   RE,3,OVHUT                                                       
         SRDA  RE,32                                                            
         M     RE,=F'10'                                                        
         ST    RF,WORK                                                          
         MVC   5(2,R2),WORK+2                                                   
         MVC   3(2,R2),OVRTG                                                    
         MVC   7(2,R2),OVSHR                                                    
         OC    OVSHR,OVSHR                                                      
         BNZ   AU400                                                            
         BAS   RE,CALCSHAR                                                      
         MVC   7(2,R2),WORK                                                     
         SPACE                                                                  
AU400    BAS   RE,CALSHRAT         CALC SHARE OR RATING OVERRIDE                
*                                                                               
         OC    NPACKGU,NPACKGU     NEW PACKAGE GUARANTEE                        
****     OC    PACKGU,PACKGU                                                    
         BZ    AU405                                                            
         XC    WORK(8),WORK                                                     
         MVC   WORK(2),=XL2'B308'                                               
         MVC   WORK+2(4),NPACKGU                                                
****     MVC   WORK(3),=XL3'B10600'                                             
****     MVC   WORK+3(2),PACKGU                                                 
         BAS   RE,PUTEL                                                         
AU405    OC    NDEMOGU,NDEMOGU                                                  
         BZ    AU406                                                            
         OC    NDEMOCAT,NDEMOCAT                                                
         BZ    AU406                                                            
         XC    WORK(11),WORK                                                    
         MVC   WORK(2),=XL2'B40B'                                               
         MVC   WORK+2(3),NDEMOCAT                                               
         MVC   WORK+5(4),NDEMOGU                                                
***AU405    OC    DEMOGU,DEMOGU                                                 
***         BZ    AU406                                                         
***         OC    DEMOCAT,DEMOCAT                                               
***         BZ    AU406                                                         
***         XC    WORK(6),WORK                                                  
***         MVC   WORK(2),=XL2'B206'                                            
***         MVC   WORK+2(1),DEMOCAT                                             
***         MVC   WORK+3(2),DEMOGU                                              
         BAS   RE,PUTEL                                                         
*  ADD DEMO OVERRIDE INFORMATION                                                
AU406    LA    R3,PUPDEMS                                                       
         LA    R2,PLNDEMS                                                       
*                                                                               
         LA    R0,6                                                             
         XC    WORK(20),WORK                                                    
         MVC   WORK(3),=XL3'DD0C00'                                             
AU407    OC    0(2,R3),0(R3)                                                    
         BZ    AU409                                                            
         MVC   WORK+3(3),0(R2)                                                  
         MVI   WORK+4,C'V'                                                      
         MVC   WORK+10(2),0(R3)                                                 
         BAS   RE,SETPRE           SET DEMO PRECISION                           
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
AU409    LA    R2,3(R2)                                                         
         LA    R3,2(R3)                                                         
         BCT   R0,AU407                                                         
         L     R3,AIOAREA1                                                      
*                                                                               
AU415    BAS   RE,GETSUB           ASSIGN SUB-LINE                              
         OI    NUUNST2,X'02'                                                    
         ZIC   R2,NUMUNAD                                                       
         SR    RE,RE                                                            
         ICM   RE,1,NUMPUAD                                                     
         BZ    AU420                                                            
         LA    R2,1(R2)                                                         
         BCTR  RE,0                                                             
         STC   RE,NUMPUAD                                                       
*                                                                               
* LOOP TO ADD RECORDS UPDATEING LINE # ONLY                                     
AU420    STC   R2,LOOP1                                                         
*                                                                               
         LA    R0,4                FIND LENGTH                                  
         LA    RF,LENS                                                          
         LA    R1,UNTNUM                                                        
         LA    R2,ACTCOSTS                                                      
         SR    RE,RE                                                            
AU422    ICM   RE,1,0(R1)                                                       
         BNZ   AU424                                                            
         LA    R1,1(R1)                                                         
         LA    R2,4(R2)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,AU422                                                         
         DC    H'0'                                                             
AU424    BCTR  RE,0                                                             
         STC   RE,0(R1)                                                         
         ZIC   RE,0(RF)                                                         
         STC   RE,NULEN                                                         
         MVC   NUACTUAL,0(R2)                                                   
         OC    NUACTUAL,NUACTUAL                                                
         BZ    *+12                                                             
         BAS   RE,CHKCOS2          CHECK COST2 SETTING                          
         OI    NUUNITST,X'20'      ACTUAL COST INPUTTED                         
         OC    NURSTAT,CTPOSTYP    SET STATUS BIT                               
*                                                                               
         CLC   NUALPHA,=C'SJ'      SJR?                                         
         BE    AU425                                                            
         CLC   NUALPHA,=C'H9'      STARCOM?                                     
         BNE   AU430                                                            
*                                                                               
AU425    CLI   NBSTATYP,C'S'       SYNDICATION?                                 
         BNE   AU430                                                            
         GOTO1 =A(ADDMCST),RR=MYRELO                                            
*                                                                               
AU430    DS    0H                                                               
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R3)                                    
         MVC   UNITDA,NDXDA        SAVE RETURNED DISK ADDRESS                   
         GOTO1 VEDIT,DMCB,(C'P',(R4)),WORK                                      
         LA    R2,WORK                                                          
         LA    R1,NDIRPTRS         N'PRGSWIVE POINTERS                          
         L     RE,NPTRS            N'POINTERS IN TABLE                          
         LR    RF,RE                                                            
         AR    RF,R1               UPDATE POINTER COUNT                         
         ST    RF,NPTRS                                                         
         MH    RE,=Y(NDIRLEN)      INDEX INTO POINTER TABLE                     
         L     R0,AIOAREA4         POINT TO NEXT ENTRY POSITION                 
         AR    RE,R0                                                            
         SPACE                                                                  
AU440    MVC   0(NDIRLEN,RE),0(R2) EXTRACT PRGSWIVE POINTER                     
         MVI   NDIRCTL(RE),0       ZERO CONTROL BYTE                            
         LA    RE,NDIRLEN(RE)      NEXT TABLE POSITION                          
         LA    R2,NDIRLEN(R2)      NEXT POINTER                                 
         BCT   R1,AU440                                                         
         CH    RF,=H'80'           TEST FOR TABLE OVERFLOW                      
         BL    *+8                                                              
         BAS   RE,SRTPTRS                                                       
         SPACE                                                                  
         CLI   NUKSUB,2                                                         
         BNE   *+8                                                              
         BAS   RE,SUBPRT                                                        
*                                                                               
         ZIC   R1,NUKSUB           SAME DATE SO INCREMENT THE                   
         LA    R1,1(R1)            LAST SUB-LINE FOR DATE                       
         CH    R1,=H'255'                                                       
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   R1,NUKSUB                                                        
         ZIC   R2,LOOP1                                                         
         BCT   R2,AU420                                                         
*                                                                               
AU500    GOTO1 VDATCON,DMCB,(2,NUKDATE),(0,FSTDTE)                              
         LA    R2,7                                                             
         GOTO1 VADDAY,DMCB,FSTDTE,FSTDTE,(R2)                                   
         GOTO1 VDATCON,DMCB,(0,FSTDTE),(2,NXTDTE)                               
AU520    OC    UNTNUM,UNTNUM                                                    
         BNZ   AU200                                                            
         SPACE                                                                  
         BAS   RE,SRTPTRS                                                       
         B     EXXMOD                                                           
*                                                                               
SRTPTRS  NTR1                                                                   
         L     R2,NPTRS            SORT POINTERS IN DESCENDING SEQUENCE         
         LTR   R2,R2               0 IF POINTERS JUST = 80                      
         BZ    SP100                                                            
         L     R3,AIOAREA4         R3=POINTS TO PRGSWIVE POINTER TABLE          
         GOTO1 VXSORT,DMCB,(X'FF',0(R3)),(R2),NDIRLEN,L'NUKEY,0                 
         BAS   RE,NEWPTR           ADD NEW POINTER                              
         LA    R3,NDIRLEN(R3)      NEXT POINTER                                 
         BCT   R2,*-8                                                           
SP100    XC    NPTRS,NPTRS                                                      
         L     RE,AIOAREA4                                                      
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE SHARE OR RATING VALUE                                
*                                                                               
CALSHRAT NTR1                                                                   
         TM    OVRBITS,X'A0'       IS BOTH SHARE AND RATING OVERRIDDEN          
         BO    EXXMOD                                                           
*                                                                               
         XC    WORK(20),WORK                                                    
         MVC   WORK(6),=XL6'DD0C00000001'                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'35',0(R3))                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         L     RF,12(R1)                                                        
         SR    R2,R2                                                            
         ICM   R2,3,5(RF)          SHARE = RATING / HUT                         
*                                                                               
         TM    OVRBITS,X'40'       IS HUT OVERRIDDEN                            
         BZ    CALSR020                                                         
         SR    RF,RF                                                            
         ICM   RF,3,OVHUT          MULT OVERRIDDEN HUT VALUE BY 10              
         M     RE,=F'10'                                                        
         LR    R2,RF                                                            
CALSR020 TM    OVRBITS,X'80'       IS SHARE OVERRIDDEN                          
         BO    CALSR050                                                         
         TM    OVRBITS,X'20'       IS RATING OVERRIDDEN                         
         BZ    EXXMOD                                                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,OVRTG          SHARE = RATING / HUT                         
         M     R0,=F'100000'       SCALE THE DIVIDEND                           
         LTR   RE,R2               GET HUT VALUE                                
         BZ    EXXMOD              ZERO DIVISOR                                 
         DR    R0,RE               COMPUTE SHARE TO 1 DECIMAL PLACE             
         AH    R1,=H'5'            ROUND BACK UP TO 1                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         MVI   WORK+4,C'S'                                                      
         STCM  R1,3,WORK+10                                                     
         B     CALSR100                                                         
         SPACE 1                                                                
CALSR050 SR    R0,R0               RATING = SHARE X HUT                         
         ICM   R0,3,OVSHR                                                       
         LR    R1,R2               GET HUT VALE                                 
         MR    R0,R0                                                            
         AH    R1,=H'5000'         ROUND TO ONE DECIMAL PLACE                   
         D     R0,=F'10000'                                                     
         MVI   WORK+4,C'R'                                                      
         STCM  R1,3,WORK+10                                                     
*                                                                               
CALSR100 BAS   RE,SETPRE           SET DEMO PRECISION                           
         BAS   RE,PUTEL                                                         
         B     EXXMOD                                                           
*                                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE SHARE                                                
*                                                                               
CALCSHAR NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,3,OVRTG          SHARE = RATING / HUT                         
         M     R0,=F'100000'       SCALE THE DIVIDEND                           
         OC    OVHUT,OVHUT                                                      
         BZ    EXXMOD              ZERO DIVISOR                                 
         SR    RE,RE                                                            
         ICM   RE,3,OVHUT          GET HUT VALUE                                
         DR    R0,RE               COMPUTE SHARE TO 1 DECIMAL PLACE             
         AH    R1,=H'50'            ROUND BACK UP TO 1                          
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         STCM  R1,3,WORK                                                        
         B     EXXMOD                                                           
         SPACE 2                                                                
* IF SECOND COST OPTION INPUTTED CALCULATE THE                                  
* SECOND COST AND MOVE INTO THE ASSIGNED COST FIELD                             
*                                                                               
CHKCOS2  NTR1                                                                   
         MVC   FULL,CLICOST2       MOVE CLIENT PCT.                             
         OC    ESTCOST2,ESTCOST2   WAS ESTIMATE LEVEL COST INPUTTED             
         BZ    *+10                                                             
         MVC   FULL,ESTCOST2                                                    
         OC    PKGCOST2,PKGCOST2   WAS PACKAGE LEVEL COST INPUTTED              
         BZ    *+10                                                             
         MVC   FULL,PKGCOST2                                                    
         OC    FULL,FULL           WAS ANY COST PCT INPUTTED                    
         BZ    CHCS2EX             NO, EXIT                                     
*                                                                               
         NI    NUUNITST,X'F7'                                                   
         XC    NUASSIGN,NUASSIGN                                                
         TM    FULL,X'80'           WAS COST PCT SET TO ZERO                    
         BZ    CHCS250              NO CALCULATE                                
         OI    NUUNITST,X'08'                                                   
         B     CHCS2EX                                                          
*                                                                               
CHCS250  ZAP   WORK(16),=PL1'0'                                                 
         ICM   R1,15,NUACTUAL       COST                                        
         CVD   R1,WORK+8                                                        
         ICM   R1,15,FULL           PERCENTAGE X.XXXXXX                         
         CVD   R1,DUB                                                           
         MP    WORK(16),DUB+4(4)    MULT COST BY PERCENTAGE                     
         AP    WORK(16),=PL4'500000'  ROUND                                     
         DP    WORK(16),=PL4'1000000'                                           
         CVB   R1,WORK+4                                                        
         STCM  R1,15,NUASSIGN                                                   
         OC    NUASSIGN,NUASSIGN                                                
         BNZ   CHCS2EX                                                          
         OI    NUUNITST,X'08'                                                   
CHCS2EX  B     EXXMOD                                                           
         SPACE 2                                                                
*                                                                               
* ERROR EXIT                                                                    
*                                                                               
AUERR    LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         J     ERROR                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT SUB-LINE NUMBER                                       
*                                                                               
GETSUB   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         L     R1,AIOAREA1                                                      
         MVI   NUKPTYPE,X'84'      USE PASSIVE KEY TO FIND NEXT NUMBER          
         MVC   NUKPAM,NUKAM-NUKEY(R1)                                           
         MVC   NUKPCLT,NUKCLT-NUKEY(R1)                                         
         MVC   NUKPNET,NUKNET-NUKEY(R1)                                         
         MVC   NUKPPROG,NUKPROG-NUKEY(R1)                                       
         MVC   NUKPDATE,NUKDATE-NUKEY(R1)                                       
         MVC   NUKPEST,NUKEST-NUKEY(R1)                                         
         LA    R0,UPDATE+PASSDEL+UNT+DIR+HIGH                                   
         SPACE                                                                  
GETSUB2  GOTO1 AIO,DMCB,(R0)                                                    
         CLC   KEY(NUKPSUB-NUKPKEY),KEYSAVE                                     
         BNE   GETSUB4                                                          
         LA    R0,UPDATE+PASSDEL+UNT+DIR+SEQ                                    
         B     GETSUB2                                                          
         SPACE                                                                  
GETSUB4  LA    R4,KEYSAVE                                                       
         ZIC   R1,NUKPSUB                                                       
         LA    R1,1(R1)                                                         
         L     R4,AIOAREA1         POINT BACK TO RECORD                         
         USING NURECD,R4                                                        
         STC   R1,NUKSUB                                                        
         B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR NEW POINTERS (AT ENTRY, R3 ADDRESSES POINTER)                 
*                                                                               
NEWPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R3)  READ FOR NEW POINTER                         
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF KEY FOUND                           
         BE    NEWPTR2                                                          
         MVC   KEY(NDIRLEN),0(R3)  RESET ENTIRE POINTER                         
         OC    KEY+20(1),CTPOSTYP    SET STATUS BIT                             
         GOTO1 (RF),(R1),UNT+DIR+ADD                                            
         B     NEWPTRX                                                          
         SPACE                                                                  
NEWPTR2  MVC   KEY(NDIRLEN),0(R3)                                               
         OC    KEY+20(1),CTPOSTYP    SET STATUS BIT                             
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
NEWPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO UPDATE FIRST UNIT FOR DATE WHEN SECOND UNIT IS ADDED           
*                                                                               
SUBPRT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,AGYMED                                                    
         MVC   NUKPCLT,CLIPK                                                    
         MVC   NUKPNET,NET                                                      
         MVC   NUKPPROG,PROG                                                    
         MVC   NUKPDATE,NXTDTE                                                  
         MVC   NUKPEST,EST                                                      
         MVI   NUKPSUB,1            SUB-LINE 1                                  
         L     R1,AIOAREA1                                                      
         MVC   NUKPDP,NUKDP-NUKEY(R1) EXTRACT DAYPART                           
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY+1),KEYSAVE                                    
         BNE   SUBPRTX             COULD NOT FIND IT-DELETED                    
         SPACE                                                                  
SUBPRT2  L     R4,AIOAREA2         GET THE RECORD                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,(R4)                                
         USING NURECD,R4                                                        
*        CLI   NPROGS,1                                                         
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   NUSUBPRT,1                                                       
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         SPACE                                                                  
SUBPRTX  B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT PACKAGE NUMBER (NUMBER RETURNED IN PACK)              
*                                                                               
TESTPACK NTR1                                                                   
         LA    R4,KEY                                                           
         USING NPRECD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,AGYMED                                                     
         MVC   NPKCLT,CLIPK                                                     
         MVC   NPKNET,NET                                                       
         MVC   NPKEST,EST                                                       
         MVC   NPKPACK,PACK                                                     
         GOTO1 AIO,DMCB,PASSDEL+UNT+DIR+HIGH                                    
         CLC   KEY(20),KEYSAVE                                                  
         BNE   EXXMOD                                                           
         MVI   FERN,DUPPACK                                                     
         J     ERROR                                                            
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT PACKAGE NUMBER (NUMBER RETURNED IN PACK)              
*                                                                               
GETPACK  NTR1                                                                   
         LA    R4,KEY                                                           
         USING NPRECD,R4                                                        
         XC    KEY,KEY                                                          
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,AGYMED                                                     
         MVC   NPKCLT,CLIPK                                                     
         MVC   NPKNET,NET                                                       
         MVC   NPKEST,EST                                                       
         LA    R0,PASSDEL+UNT+DIR+HIGH                                          
GETPACK2 GOTO1 AIO,DMCB,(R0)                                                    
         CLC   KEY(NPKPACK-NPKEY),KEYSAVE                                       
         BNE   GETPACK4                                                         
         LA    R0,PASSDEL+UNT+DIR+SEQ                                           
         B     GETPACK2                                                         
         SPACE                                                                  
GETPACK4 LA    R4,KEYSAVE                                                       
         ZIC   R1,NPKPACK          GET HIGHEST NUMBER SO FAR                    
         LA    R1,1(R1)                                                         
         STC   R1,PACK                                                          
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ADD THE PRECISION FACTOR TO OVERRIDE ELEMENTS                  
* R4 POINTS TO THE ELEMENT AREA                                                 
*                                                                               
SETPRE   NTR1                                                                   
         LA    R4,WORK             ELEMENT AREA                                 
         LA    R5,DEMPREC          CONVERSION TABLE                             
         LA    RE,7                                                             
*                                                                               
SETP20   CLC   0(1,R5),4(R4)                                                    
         BE    SETP40                                                           
         LA    R5,2(R5)                                                         
         BCT   RE,SETP20                                                        
         DC    H'0'                                                             
SETP40   MVC   7(1,R4),1(R5)                                                    
         B     EXXMOD                                                           
         EJECT                                                                  
PUTEL    LR    R0,RE                                                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTION FIELD                                              
*        READ PLAN RECORD & EXTRACT NEEDED INFO                                 
*                                                                               
ACTED    NTR1                                                                   
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FNDX,1                                                           
         XC    SVAREA18,SVAREA18   CLEAR SAVE AREA IT RE-EDIT ACTION            
         MVC   SVACTLIN,11(R2)                                                  
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AT END OF ACTION            
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACMERR              NO                                           
*                                                                               
         XC    FTERM,FTERM         ** DAYPART                                   
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   *+14                                                             
         MVC   XTRA(7),=C'DAYPART'                                              
         B     ACMERR                                                           
*!!!     MVI   FERN,INVERR                                                      
         CLI   FLDH+5,2                                                         
         BH    INVERRM                                                          
*                                                                               
* NEW DAYPART CHECK WORKING OFF USER DEFINED RECORDS                            
*                                                                               
         OI    FLD+1,X'40'                                                      
         GOTO1 VALDAYPT,DMCB,(0,FLD)                                            
*                                                                               
         CLC   KEY(20),KEYSAVE                                                  
         JNE   ERROR                                                            
         MVC   DUB(1),KEYSAVE+5    SET INTERNAL DAYPART CODE                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'20'                                                        
         MVC   KEY+1(1),AGYMED                                                  
         MVC   KEY+2(2),CLIENT                                                  
         MVC   KEY+5(4),NET                                                     
         MVC   KEY+10(1),DUB       DAYPART CODE                                 
*                                                                               
         MVI   FNDX,2              ** PLAN                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   *+14                                                             
         MVC   XTRA(4),=C'PLAN'                                                 
         B     ACMERR                                                           
*!!!     MVI   FERN,INVERR                                                      
         CLI   FLDH+5,4                                                         
         BH    INVERRM                                                          
         MVC   KEY+11(4),SPACES                                                 
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,MVCPCDE          BUILD KEY - READ RECORD                      
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         JNE   ERROR                                                            
         GOTO1 AIO,DMCB,UNT+FILE+GET,AIOAREA2                                   
         L     R3,AIOAREA2                                                      
         USING NPLRECD,R3                                                       
         CLI   NPLNPRFL,C'C'                                                    
         BE    *+12                                                             
         CLI   NPLNPRFL,C'B'                                                    
         BNE   *+10                                                             
         MVC   NETPROF+3(1),NPLNPRFL  OVERRIDE TO PROFILE                       
         MVC   TRGDEMO,NPLNDEMS+2                                               
         MVC   LENS,NPLNLENS                                                    
         MVC   PLAN,NPLKPLAN                                                    
         MVC   PLNDPT,NPLKDPT                                                   
         MVC   PLHAVE,NPLNPERT                                                  
         MVC   PLNDEMS,NPLNDEMS                                                 
         OC    NPLNPERT,NPLNPERT                                                
         BNZ   *+10                                                             
         MVC   PLHAVE,NPLNHAVE                                                  
*                                                                               
         XC    PLNTVQBK,PLNTVQBK                                                
         LA    RE,NPLNEL-NPLKEY(R3)                                             
AC060    CLI   0(RE),0                                                          
         BE    AC080                                                            
         CLI   0(RE),X'08'             NEW GENERAL ELEMENT                      
         BNE   AC070                                                            
         MVC   PLNTVQBK,NPL2BOOK-NPL2ELEM(RE)                                   
         B     AC080                                                            
AC070    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     AC060                                                            
         SPACE                                                                  
AC080    MVI   FNDX,3                                                           
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   *+14                                                             
         MVC   XTRA(7),=C'QUARTER'                                              
         B     ACMERR                                                           
         CLI   FLDH+5,5                                                         
         JH    ERROR                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,-'                              
         CLI   4(R1),0                                                          
         JE    ERROR                                                            
         CLI   WORK,2                                                           
         JNE   ERROR                                                            
         CLI   WORK+12,C'Q'                                                     
         JNE   ERROR                                                            
         CLI   WORK+13,C'1'                                                     
         JL    ERROR                                                            
         SPACE                                                                  
* GET BROADCAST DATES (START/END)                                               
* QUARTER 3 ENDS SUNDAY OF 2ND WEEK OF SEP.                                     
* QUARTER 4 STARTS MONDAY OF 3RD WEEK OF SEP.                                   
* START QUARTER SET UP START AND END DATES                                      
         XC    HALF,HALF           USE HALY AS SWITCHES FOR 3/4/QTR             
         ZIC   RE,NPLNYEAR                                                      
         STC   RE,PLANYR                                                        
         STC   RE,DUB              SET UP BOTH START/END DATES                  
         STC   RE,DUB+3                                                         
         MVI   DUB+2,1             DEFAULT START DAY                            
         MVI   DUB+5,X'1C'         DEFAULT END DAY                              
         CLI   WORK+13,C'4'                                                     
         JH    ERROR                                                            
         BNE   AC100                                                            
         BCTR  RE,0                                                             
         STC   RE,DUB                                                           
         STC   RE,DUB+3                                                         
         MVI   HALF,C'Y'                                                        
         B     AC120                                                            
*                                                                               
AC100    CLI   WORK+13,C'3'                                                     
         BNE   AC120                                                            
         MVI   HALF+1,C'Y'                                                      
*                                                                               
AC120    ZIC   RF,WORK+13                                                       
         SH    RF,=H'240'                                                       
         LA    R1,SVQTBLS-1(RF)                                                 
         MVI   0(R1),C'Y'                                                       
         MH    RF,=H'2'                                                         
         LA    RF,QTBL-2(RF)                                                    
         MVC   DUB+1(1),0(RF)                                                   
         MVC   DUB+4(1),1(RF)                                                   
*                                                                               
* TEST FOR MORE THAN 1 QUARTER                                                  
AC200    CLI   WORK+1,0                                                         
         BE    AC240                                                            
         CLI   WORK+1,2                                                         
         JNE   ERROR                                                            
         CLC   WORK+13(1),WORK+23                                               
         BE    AC240                                                            
         CLI   WORK+22,C'Q'                                                     
         JNE   ERROR                                                            
         CLI   WORK+23,C'1'                                                     
         JL    ERROR                                                            
         MVI   HALF+1,C'N'                                                      
         CLI   WORK+23,C'3'                                                     
         JH    ERROR                                                            
         BNE   *+8                                                              
         MVI   HALF+1,C'Y'                                                      
         MVC   DUB+3(1),NPLNYEAR                                                
         ZIC   RF,WORK+23                                                       
         SH    RF,=H'240'                                                       
         LA    R4,SVQTBLS-1(RF)                                                 
         MVI   0(R4),C'Y'                                                       
         MH    RF,=H'2'                                                         
         LA    RF,QTBL-2(RF)                                                    
         MVC   DUB+4(1),1(RF)                                                   
         CLI   SVQTBL4,C'Y'                                                     
         BNE   *+8                                                              
         LA    R1,SVQTBL1-1                                                     
         LA    R0,2                                                             
AC220    LA    R1,1(R1)                                                         
         CR    R1,R4                                                            
         BNL   AC240                                                            
         MVI   0(R1),C'Y'                                                       
         BCT   R0,AC220                                                         
*                                                                               
AC240    CLI   NETPROF+3,C'C'      ARE DATES IN CALANDER FORMAT                 
         BE    AC300                                                            
*--CALCULATE DATES ON A BROADCAST BASIS                                         
         GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK)                                    
         GOTO1 VDATCON,(R1),(3,DUB+3),(0,WORK+6)                                
         GOTO1 =V(GETBROAD),(R1),(1,WORK),WORK+12,VGETDAY,VADDAY,      X        
               RR=MYRELO                                                        
         CLI   HALF,C'Y'             4TH QTR                                    
         BNE   AC260                                                            
         LA    R0,14                                                            
         GOTO1 VADDAY,(R1),WORK+12,WORK+12,(R0)                                 
AC260    GOTO1 VDATCON,(R1),(0,WORK+12),(3,DUB)                                 
         SPACE                                                                  
* NOW GET END DATE                                                              
         GOTO1 =V(GETBROAD),(R1),(1,WORK+6),WORK+12,VGETDAY,VADDAY,    X        
               RR=MYRELO                                                        
         CLI   HALF+1,C'Y'         3RD QTR                                      
         BNE   AC280                                                            
         LA    R0,13                                                            
         GOTO1 VADDAY,(R1),WORK+12,WORK+18,(R0)                                 
AC280    GOTO1 VDATCON,(R1),(0,WORK+18),(3,DUB+3)                               
         B     AC350                                                            
         SPACE 3                                                                
*--CALCULATE DATES ON A CALANDER BASIS                                          
AC300    GOTO1 VDATCON,DMCB,(3,DUB),(0,WORK)                                    
         GOTO1 VGETDAY,DMCB,WORK,THREE                                          
         ZIC   RF,0(R1)                                                         
         SR    R0,R0                                                            
         CH    RF,=H'1'                                                         
         BE    AC310                                                            
         LA    R0,8                                                             
         SR    R0,RF                                                            
*                                                                               
AC310    CLI   HALF,C'Y'             4TH QTR                                    
         BNE   AC320                                                            
         A     R0,=F'14'                                                        
AC320    GOTO1 VADDAY,DMCB,WORK,WORK+12,(R0)                                    
         GOTO1 VDATCON,DMCB,(0,WORK+12),(3,DUB)                                 
         SPACE                                                                  
* NOW GET END DATE                                                              
         GOTO1 VDATCON,DMCB,(3,DUB+3),(0,WORK+6)                                
         GOTO1 VGETDAY,DMCB,WORK+6,THREE                                        
         ZIC   RF,0(R1)                                                         
         SR    R0,R0                                                            
         LA    R0,7                                                             
         SR    R0,RF                                                            
*                                                                               
AC330    CLI   HALF+1,C'Y'           3RD QTR                                    
         BNE   AC340                                                            
         A     R0,=F'13'                                                        
AC340    GOTO1 VADDAY,DMCB,WORK+6,WORK+18,(R0)                                  
         GOTO1 VDATCON,DMCB,(0,WORK+18),(3,DUB+3)                               
         SPACE                                                                  
*                                                                               
AC350    GOTO1 VDATCON,DMCB,(3,DUB),(0,STRTDTE)                                 
         GOTO1 VDATCON,(R1),(3,DUB+3),(0,ENDDTE)                                
         MVI   FERN,STERR                                                       
         CLC   ESTSTART,ENDDTE                                                  
         JH    ERROR                                                            
         MVI   FERN,ENDERR                                                      
         CLC   ESTEND,STRTDTE                                                   
         JL    ERROR                                                            
         SPACE                                                                  
         CLC   ESTSTART,STRTDTE                                                 
         BNH   *+10                                                             
         MVC   STRTDTE,ESTSTART                                                 
         CLC   ESTEND,ENDDTE                                                    
         BNL   *+10                                                             
         MVC   ENDDTE,ESTEND                                                    
         GOTO1 VDATCON,(R1),(0,STRTDTE),(3,SVSTDTE)                             
         GOTO1 VDATCON,(R1),(0,ENDDTE),(3,SVENDTE)                              
         SPACE                                                                  
         MVI   FNDX,4              CHECK FOR LENGTH FILTERS OR 'ALL'            
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   *+14                                                             
         MVC   XTRA(19),=C'VALID LENGTH OR ALL'                                 
         B     ACMERR                                                           
         CLI   FLDH+5,9                                                         
         JH    ERROR                                                            
         CLC   =C'ALL',FLD                                                      
         BE    AC400                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,*'                              
         CLI   4(R1),0                                                          
         JE    ERROR               INVALID INPUT                                
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,WORK                                                        
         JZ    ERROR                                                            
         TM    WORK+2,X'80'        TEST IF NUMERIC                              
         JZ    ERROR                                                            
         MVC   THREE,WORK+12                                                    
         BAS   R3,VALLEN           CHECK INPUT LENGTH 1                         
         STC   R0,SVLEN1                                                        
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,WORK+1                                                      
         JZ    AC400                                                            
         TM    WORK+3,X'80'        TEST IF NUMERIC                              
         JZ    ERROR                                                            
         MVC   THREE,WORK+22                                                    
         BAS   R3,VALLEN           CHECK INPUT LENGTH 2                         
         STC   R0,SVLEN2                                                        
*                                                                               
AC400    MVI   FNDX,5              ** HUT TRANSFER                              
         GOTO1 AFVAL,0                                                          
         MVI   CALCHUT,C'N'                                                     
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    AC500                                                            
*!!!     MVI   FERN,INVERR                                                      
         CLI   FLDH+5,6                                                         
         BH    INVERRM                                                          
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=CL6'FREEZE'                                              
         JNE   ERROR                                                            
         MVI   CALCHUT,C'Y'                                                     
*  CHECK TO SEE IF ANY PREVIOUS UPLOADS WERE DONE AGAINST THIS PLAN             
AC500    L     R3,AIOAREA2                                                      
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',0(R3)),0                       
         CLI   12(R1),0                                                         
         BNE   AC600                                                            
         L     RE,12(R1)                                                        
         USING NPLNEL2,RE                                                       
         MVC   SVXTRA,SPACES                                                    
         MVC   PLNUPLST,NPL2UPST                                                
*   FIRST QTR                                                                   
         CLI   SVQTBLS,C'Y'                                                     
         BNE   *+20                                                             
         TM    NPL2UPST,X'40'                                                   
         BZ    *+8                                                              
         MVI   SVXTRA,C'1'                                                      
         OI    PLNUPLST,X'40'                                                   
*   SECOND QTR                                                                  
         CLI   SVQTBLS+1,C'Y'                                                   
         BNE   *+20                                                             
         TM    NPL2UPST,X'20'                                                   
         BZ    *+8                                                              
         MVI   SVXTRA+2,C'2'                                                    
         OI    PLNUPLST,X'20'                                                   
*   THIRD QTR                                                                   
         CLI   SVQTBLS+2,C'Y'                                                   
         BNE   *+20                                                             
         TM    NPL2UPST,X'10'                                                   
         BZ    *+8                                                              
         MVI   SVXTRA+4,C'3'                                                    
         OI    PLNUPLST,X'10'                                                   
*   FOURTH QTR                                                                  
         CLI   SVQTBLS+3,C'Y'                                                   
         BNE   *+20                                                             
         TM    NPL2UPST,X'80'                                                   
         BZ    *+8                                                              
         MVI   SVXTRA+6,C'4'                                                    
         OI    PLNUPLST,X'80'                                                   
*                                                                               
AC600    MVI   FNDX,0                                                           
         XMOD1                                                                  
         SPACE                                                                  
ACMERR   MVI   FERN,MISERR                                                      
         J     ERROR                                                            
INVERRM  MVI   FERN,INVERR                                                      
         J     ERROR                                                            
         DROP  R3,RE                                                            
         SPACE                                                                  
MVCPCDE  MVC   KEY+11(0),FLD                                                    
         SPACE                                                                  
VALLEN   LR    R0,R1               SAVE DATA LENGTH                             
         LA    RE,THREE            RE=DATA POINTER                              
VL100    CLI   0(RE),C'0'          TEST FOR NUMERIC DATA                        
         JL    ERROR                                                            
         CLI   0(RE),C'9'                                                       
         JH    ERROR                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,VL100                                                         
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,PACKLEN                                                       
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0               TEST FOR ZERO                                
         JZ    ERROR                                                            
         CH    R0,=H'255'          TEST FOR MAXIMUM VALUE                       
         JH    ERROR                                                            
         BR    R3                                                               
PACKLEN  PACK  DUB,THREE(0)                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
*                                                                               
QTBL     DC    0CL8' '                                                          
QTBL1    DC    XL1'01',XL1'03'                                                  
QTBL2    DC    XL1'04',XL1'06'                                                  
QTBL3    DC    XL1'07',XL1'09'                                                  
QTBL4    DC    XL1'09',XL1'0C'                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* ROUTINE TO EDIT PACKAGE SCREEN AND ADD PACKAGE RECORD                         
*                                                                               
APACK    NTR1  BASE=*,LABEL=*                                                   
         ST    RE,SAVEREG                                                       
         L     R4,APACKREC                                                      
         USING NPRECD,R4                                                        
         LR    RE,R4                                                            
         LA    RF,PAGELEN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR I/O AREA                               
*                                                                               
         XC    NPKEY,NPKEY         BUILD KEY                                    
         MVI   NPKTYPE,X'02'                                                    
         MVC   NPKAM,AGYMED                                                     
         MVC   NPKCLT,CLIPK                                                     
         MVC   NPKNET,NET                                                       
         MVC   NPKEST,EST                                                       
         MVC   NPKPACK,PACK        SET PACKAGE NUMBER IN KEY                    
*                                                                               
         MVC   NPAKEL(2),=XL2'013C'                                             
         MVI   ANDMASK,X'FF'       INITIALIZE ANDMASK                           
*                                                                               
         OI    NPAKHUTL,X'80'      GET HUTS FORM DEMO FILE                      
*        CLI   BUYPROF+3,YES       TEST FOR 52 WEEK OPTION                      
*        BNE   *+8                 NO                                           
         OI    NPAKHUTL,X'40'      YES                                          
         SPACE                                                                  
NAMED    LA    R2,PAKNAMEH                                                      
*!!!!    MVI   FERN,MISERR                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    ACMERR                                                           
         MVC   NPAKNAME,FLD                                                     
         SPACE                                                                  
DPNED    LA    R2,PAKDPNH          VALIDATE DAYPART NAME                        
*!!!!    MVI   FERN,MISERR                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    ACMERR              MUST HAVE DAYPART                            
*                                                                               
*!!!!    MVI   FERN,INVERR                                                      
*                                                                               
* NEW DAYPART CHECK WORKING OFF USER DEFINED RECORDS                            
*                                                                               
         OI    FLD+1,X'40'                                                      
         GOTO1 VALDAYPT,DMCB,(0,FLD)                                            
         CLC   KEY(20),KEYSAVE                                                  
         BNE   INVERRM                                                          
         MVC   NPAKDP,KEYSAVE+5    SET INTERNAL DAYPART CODE                    
* MOVE DAYPART CODE OUT TO SCREEN                                               
         MVC   PAKDPN,KEYSAVE+21   SET DISPLAY DAYPART CODE                     
         MVC   PAKDPEX(14),KEYSAVE+6                                            
         B     DPNED5                                                           
*                                                                               
*&&DO                                                                           
*DPNED1   MVI   FERN,INVERR                                                     
DPNED1   DS    0H                                                               
         LA    RE,DPTTAB           POINT RE AT DAYPART TABLE                    
         LA    R0,DAYPARTS         COUNTER                                      
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
DPNED2   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),1(RE)        TEST INPUT AGAINST TABLE                     
         BE    DPNED4                                                           
         LA    RE,L'DPTTAB(RE)                                                  
         BCT   R0,DPNED2                                                        
         B     INVERRM                                                          
*                                                                               
DPNED4   MVC   NPAKDP,0(RE)        SET DAYPART CODE                             
         MVC   PAKDPN(L'DPTTAB-1),1(RE)                                         
*&&                                                                             
DPNED5   OI    PAKDPNH+6,X'80'     SEND EXPANDED NAME BACK TO USER              
         OI    PAKDPEXH+6,X'80'     SEND LITERAL BACK TO USER                   
*                                                                               
DPNEDX   B     STAED                                                            
*                                                                               
* EDIT STATUS FIELD                                                             
*                                                                               
STAED    LA    R2,PAKSTATH         VALIDATE STATUS                              
         GOTO1 VGETFLD                                                          
         L     R1,APACKREC         YES-EXTRACT EXISTING STATUS                  
         MVC   OLDSTAT,NPAKSTAT-NPKEY(R1)                                       
         CLI   FLDH+5,0                                                         
         BE    STAEDX                                                           
         SPACE                                                                  
         LA    RE,STATAB           POINT RE AT STATUS TABLE                     
         USING STATABD,RE                                                       
         LA    R0,STAENT           COUNTER                                      
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         SPACE                                                                  
STAED2   CLC   FLDH+5(1),STAMIN    TEST FOR MINIMUM LENGTH                      
         BL    STAED3                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),STANAME      TEST INPUT AGAINST TABLE                     
         BE    STAED4              FOUND IT                                     
STAED3   LA    RE,STATABL(RE)                                                   
         BCT   R0,STAED2                                                        
         B     ERROR                                                            
         SPACE                                                                  
STAED4   MVC   PAKSTAT,STANAME     SEND BACK WHOLE NAME                         
         OI    PAKSTATH+6,X'80'                                                 
         SPACE                                                                  
         OC    NPAKSTAT,STAOR      NOW APPLY THE TABLE ENTRY'S                  
         NC    NPAKSTAT,STAND      OR MASK AND 'AND' MASK                       
         SPACE                                                                  
STAEDX   B     OPTED                                                            
         DROP  RE                                                               
         SPACE                                                                  
* EDIT OPTIONS FIELD                                                            
*                                                                               
OPTED    MVI   NPAKZONE,0                                                       
         LA    R2,PAKOPTH                                                       
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BNE   OPTED2              YES                                          
         CLI   ACTION,CP           TEST FOR CHANGE                              
         BNE   OPTEDX                                                           
         TM    OLDSTAT,NOPRINT     TEST IF PRINT SUPPRESSION SET                
         BZ    OPTEDX                                                           
         NI    NPAKSTAT,PRINT      RESET STATUS                                 
         NI    ANDMASK,PRINT       UPDATE CUMULATIVE AND MASK                   
         B     OPTED7                                                           
         SPACE 1                                                                
OPTED2   XC    FLAST,FLAST         EDIT FIELD INPUT                             
OPTED2A  XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUAL         SEARCH FOR EQUAL SIGN                        
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,EQUAL         TEST IF '=' FOUND                            
         BNE   ERROR                                                            
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,5                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,ZONECOMP         TEST FOR Z(ONE)                              
         BE    OPTED10                                                          
         EX    R1,PRINCOMP         TEST FOR P(RINT)                             
         BNE   ERROR                                                            
         SPACE 1                                                                
OPTED4   XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         NOW GET REST OF FIELD                        
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,3                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5           TEST FOR Y(ES) OR N(O)                       
         BCTR  R1,0                                                             
         EX    R1,YESCOMP                                                       
         BE    OPTED5                                                           
         EX    R1,NOCOMP                                                        
         BE    OPTED6                                                           
         B     ERROR                                                            
         SPACE                                                                  
* PRINT=YES                                                                     
*                                                                               
OPTED5   TM    OLDSTAT,NOPRINT     TEST IF FORMERLY NO PRINT                    
         BZ    OPTEDX              NO-ALL DONE                                  
         NI    NPAKSTAT,PRINT      TURN OFF PRINT SUPPRESSION BIT               
         NI    ANDMASK,PRINT                                                    
         B     OPTED7                                                           
         SPACE 1                                                                
* PRINT=NO                                                                      
*                                                                               
OPTED6   TM    OLDSTAT,NOPRINT     TEST IF FORMERLY NOPRINT                     
         BO    OPTEDX              YES-THEN NO CHANGE IN STATUS                 
         OI    NPAKSTAT,NOPRINT    TURN ON NOPRINT BIT                          
         OI    ORMASK,NOPRINT      UPDATE CUMULATIVE OR MASK                    
         NI    ANDMASK,X'FF'                                                    
*                                                                               
OPTED7   MVI   STATSW,YES                                                       
         MVC   NEWSTAT,NPAKSTAT    UPDATE NEW STATUS                            
         B     OPTEDX                                                           
         SPACE 1                                                                
OPTED10  XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         NOW GET REST OF FIELD                        
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         CLI   FLDH+5,1                                                         
         BH    ERROR                                                            
         CLI   FLD,C'C'                                                         
         BE    OPTED11                                                          
         CLI   FLD,C'M'                                                         
         BE    OPTED11                                                          
         CLI   FLD,C'P'                                                         
         BNE   ERROR                                                            
*                                                                               
OPTED11  MVC   NPAKZONE,FLD                                                     
         SPACE 1                                                                
OPTEDX   CLI   FSTOP,COMMA         TEST IF ',' FOUND                            
         BNE   COSTED                                                           
         B     OPTED2A                                                          
         SPACE 1                                                                
COSTED   LA    R2,PAKCOSTH         VALIDATE COST                                
*!!!!    MVI   FERN,MISERR                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            MUST SPECIFY COST                            
         BE    ACMERR                                                           
*!!!     MVI   FERN,INVERR                                                      
         TM    FLDH+4,X'08'        EST FOR NUMERIC DATA                         
         BZ    INVERRM                                                          
         STCM  R0,15,NPAKCOST                                                   
         SPACE                                                                  
INTED    LA    R2,PAKINTH          INTEGRATION RATE                             
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    INTED3                                                           
         CLC   =C'TBL',FLD                                                      
         BE    INTED4                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(2,BLOCK),0                                   
*!!!     MVI   FERN,INVERR                                                      
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)                                                       
         BZ    INVERRM                                                          
         LA    R2,BLOCK                                                         
*                                                                               
         CLI   1(R2),0                                                          
         BNE   ERROR                                                            
         ZIC   R0,0(R2)                                                         
         GOTO1 VCASHVAL,DMCB,12(R2),(R0)                                        
         CLI   DMCB,0                                                           
         BNE   ERROR                                                            
         MVC   NPAKINT,DMCB+4                                                   
         SH    R3,=H'1'                                                         
         BZ    HSCED                                                            
         SPACE                                                                  
INTED2   LA    R2,32(R2)                                                        
         CLI   1(R2),0                                                          
         BNE   ERROR                                                            
         CLI   0(R2),1                                                          
         BNE   ERROR                                                            
         CLI   12(R2),C'N'         TEST FOR NON-COMM. INT.                      
         BNE   ERROR                                                            
         OI    NPAKSTAT,X'04'                                                   
         B     HSCED                                                            
         SPACE                                                                  
INTED3   CLI   BUYPROF+15,YES                                                   
         BNE   HSCED                                                            
         CLC   =C'ABC ',NET                                                     
         BE    INTED4                                                           
         CLC   =C'CBS ',NET                                                     
         BE    INTED4                                                           
         CLC   =C'NBC ',NET                                                     
         BNE   HSCED                                                            
INTED4   OI    NPAKCNTL,X'80'      USE INTG TABLE                               
         SPACE                                                                  
HSCED    LA    R2,PAKHSCH          HUT SCHEME                                   
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   HSCED2              FIELD HAS INPUT                              
         CLI   BUYPROF,STAR        YES-SEE IF PROFILE HAS A SCHEME              
         BE    *+10                NO                                           
         MVC   NPAKHUTS,BUYPROF    YES                                          
         B     HSCEDX                                                           
         SPACE                                                                  
HSCED2   CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         TM    FLDH+4,X'0C'        TEST FOR ALPHA-NUMERIC                       
         BZ    ERROR               NEITHER-NO SPECIAL CHARACTERS                
         MVC   NPAKHUTS,FLD                                                     
         SPACE                                                                  
HSCEDX   B     HAVED                                                            
         SPACE                                                                  
HAVED    LA    R2,PAKHAVEH         HUT AVERAGE                                  
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    HUTPCT                                                           
         CLI   FLDH+5,2                                                         
         BH    ERROR                                                            
*                                                                               
         CLI   FLD,C'W'                                                         
         BE    HAVE010                                                          
         CLI   FLD,C'M'                                                         
         BE    HAVE010                                                          
         CLI   FLD,C'Q'                                                         
         BNE   ERROR                                                            
*                                                                               
HAVE010  MVC   NPAKHUTF,BUYPROF2+2         DEFAULT FLAVOR                       
         CLI   FLDH+5,1                                                         
         BE    HAVE100                                                          
         CLI   FLD+1,C'N'                                                       
         BE    HAVE090                                                          
         CLI   FLD+1,C'B'                                                       
         BE    HAVE090                                                          
         CLI   FLD+1,C'C'                                                       
         BNE   ERROR                                                            
HAVE090  MVC   NPAKHUTF,FLD+1                                                   
HAVE100  MVC   NPAKHUTA,FLD                                                     
         SPACE                                                                  
HUTPCT   LA    R2,PAKHADJH         HUT ADJUSTMANT PERCENT                       
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    HUTTYP                                                           
         ZIC   R0,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'         TEST FOR ERROR                               
         BE    ERROR                                                            
*                                                                               
         MVC   NPAKHPCT,6(R1)                                                   
         SPACE                                                                  
HUTTYP   LA    R2,PAKHTYPH         HUT TYPE                                     
         GOTO1 VGETFLD                                                          
         MVI   NPAKHTYP,C'D'                                                    
         CLI   FLDH+5,0                                                         
         BE    DEMBASE                                                          
         CLI   FLD,C'A'                                                         
         BE    *+20                                                             
         CLI   FLD,C'D'                                                         
         BE    *+12                                                             
         CLI   FLD,C'I'                                                         
         BNE   ERROR                                                            
         MVC   NPAKHTYP,FLD                                                     
*                                                                               
DEMBASE  LA    R2,PAKDBSEH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    DEMB20                                                           
*                                                                               
DEMB10   CLI   FLD,C'V'                                                         
         BE    GCPM                                                             
         CLI   FLD,C'I'                                                         
         BNE   ERROR                                                            
         OI    NPAKCNTL,X'40'                                                   
         B     GCPM                                                             
*                                                                               
DEMB20   CLI   NBPOSTYP,C'C'       PROFILE FOR CABLE USE ONLE                   
         BNE   GCPM                                                             
         MVC   FLD(1),BUYPROF2+9   IF NO INPUT USE PROFILE                      
         CLI   FLD,C'I'                                                         
         BE    DEMB10                                                           
         MVI   FLD,C'V'                                                         
         B     DEMB10                                                           
         SPACE                                                                  
GCPM     LA    R2,PAKGCPMH         GUARANTEE CPM                                
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    UPCTED                                                           
         ZIC   R3,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R3)                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
         MVC   NPAKGCPM,DMCB+4                                                  
         SPACE                                                                  
UPCTED   LA    R2,PAKUPCTH         UNIVERSE PERCENTAGE                          
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    UCODED                                                           
         ZIC   R3,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R3)                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
         CLC   DMCB+4(4),=F'32767'                                              
         BH    ERROR                                                            
         MVC   NPAKUNIV,DMCB+6                                                  
         SPACE                                                                  
UCODED   LA    R2,PAKUCODH         UNIVERSE CODE                                
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    FEEDED                                                           
         TM    FLDH+4,X'08'        TEST FOR NUMERIC INPUT                       
         BO    *+12                                                             
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
         SRP   DUB+5(3),1,0        SHIFT PACKED DIGITS 1 TO LEFT                
         MVC   NPAKUNCD,DUB+5      TO ISOLATE UNIVERSE CODE PWO.                
         SPACE                                                                  
UCODED2  XC    KEY,KEY             NOW SEE IF UNIVERSE RECORD IS THERE          
         LA    R3,KEY                                                           
         USING NUNRECD,R3                                                       
         MVC   NUNKTYP,=X'0D22'                                                 
         MVC   NUNKAGY,AGYALPH                                                  
         MVI   NUNKTYPE,1          UNIVERSE CODE                                
         MVC   NUNKCODE,NPAKUNCD                                                
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(L'NUNKEY),KEYSAVE    TEST IF FOUND                           
         BE    FEEDED                   YES                                     
         MVI   FERN,UNIVERR                                                     
         B     ERROR                                                            
         DROP  R3                                                               
         SPACE                                                                  
FEEDED   LA    R2,PAKFPCTH         FEED PERCENTAGE                              
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    ALLED                                                            
         ZIC   R3,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   NPAKFEED,DMCB+6                                                  
         SPACE                                                                  
*MGED    LA    R2,PAKFMGRH         FEED MARKET GROUP                            
*        GOTO1 VGETFLD                                                          
*        CLI   FLDH+5,0            TEST FOR ANY INPUT                           
*        BE    ALLED               NO                                           
*        CLI   FLDH+5,3            TEST FOR LENGTH OF 3                         
*        BNE   ERROR                                                            
*        CLI   FLD,C'G'                                                         
*        BL    ERROR                                                            
*        CLI   FLD,C'K'                                                         
*        BH    ERROR                                                            
*                                                                               
*        XC    KEY,KEY                                                          
*        LA    R3,KEY                                                           
*        USING MKGRECD,R3                                                       
*        MVC   MKGKTYP,=X'0D02'                                                 
*        MVC   MKGKAGMD,AGYMED                                                  
*        MVC   MKGKMID,FLD                                                      
*        PACK  DUB(2),FLD+1(3)                                                  
*        MVC   MKGKMGRP(1),DUB     LEFT ALIGNED 2-DIGIT PWO                     
*        GOTO1 AIO,DMCB,SPT+DIR+READ                                            
*        MVC   NPAKFMG,MKGKMID                                                  
*        DROP  R3                                                               
         SPACE                                                                  
ALLED    LA    R2,PAKMALLH         MASTER ALLOCATION                            
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    IMPED                                                            
         CLI   FLDH+5,3            PRODUCT CODE IS 3 CHARS OR LESS              
         BH    ERROR                                                            
         CLC   =C'POL',FLD         TEST FOR POL                                 
         BE    ERROR               YES-DO NOT ALLOW IT                          
*                                                                               
         LA    R0,255                                                           
         LA    RE,CLILIST          POINT RE AT PRODUCT LIST                     
ALLED2   OC    0(4,RE),0(RE)       TEST FOR E-O-L                               
         BZ    ALLEDR                                                           
         CLC   FLD(3),0(RE)                                                     
         BE    ALLED4              FOUND PRODUCT CODE                           
         LA    RE,4(RE)                                                         
         BCT   R0,ALLED2                                                        
*                                                                               
ALLEDR   MVI   FERN,PRDERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
ALLED4   MVC   NPAKMAST,3(RE)      EXTRACT PRODUCT NUMBER                       
*                                                                               
ALLED6   XC    KEY,KEY             NOW VALIDATE BRAND ESTIMATE                  
         LA    R3,KEY                                                           
         USING ESTHDRD,R3                                                       
         MVI   EKEYTYPE,X'00'                                                   
         MVC   EKEYAM,AGYMED                                                    
         MVC   EKEYCLT,CLIPK                                                    
         MVC   EKEYPRD,FLD         PRODUCT CODE                                 
         MVC   EKEYEST,EST                                                      
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(L'EKEY),KEYSAVE TEST IF ESTIMATE FOUND                       
         BE    IMPED               YES                                          
         MVI   FERN,PRESTERR                                                    
         B     ERROR                                                            
         DROP  R3                                                               
         SPACE                                                                  
IMPED    LA    R2,PAKIMPCH         IMPACT PERCENTAGE                            
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    REPED                                                            
         ZIC   R3,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   NPAKIMP,DMCB+6                                                   
         SPACE                                                                  
REPED    LA    R2,PAKSREPH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    REPED4                                                           
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BO    *+12                YES                                          
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
*                                                                               
         LTR   R0,R0               REP CODE CANNOT BE ZERO                      
         BZ    ERROR                                                            
         CH    R0,=H'999'                                                       
         BH    ERROR                                                            
         STCM  R0,3,NPAKSREP       SAVE BINARY VALUE                            
         OI    DUB+7,X'0F'         CREATE 3 CHARACTER REP NUMBER                
         UNPK  THREE,DUB                                                        
         SPACE                                                                  
REPED2   XC    KEY,KEY             NOW VALIDATE THE REP                         
         LA    R3,KEY                                                           
         USING REPRECD,R3                                                       
         MVI   REPKEY,C'0'                                                      
         MVC   REPKEY+1(L'REPKEY-1),REPKEY                                      
         MVI   REPKTYPE,C'R'                                                    
         MVI   REPKMED,C'N'                                                     
         MVC   REPKREP,THREE                                                    
         MVC   REPKAGY,AGYALPH                                                  
         GOTO1 AIO,DMCB,HIGH+FILE+STA,AIOAREA2                                  
         CLC   KEY(L'REPKEY),KEYSAVE  TEST IF REP FOUND                         
         BE    FILTR                  YES                                       
         MVI   FERN,REPERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
REPED4   MVC   NPAKSREP,ESTSREP                                                 
         DROP  R3                                                               
         SPACE                                                                  
FILTR    LA    R2,PAKFILTH                                                      
         GOTO1 VGETFLD                                                          
         GOTO1 VHELLO,DMCB,(C'D',APUNTFIL),(X'08',APACKREC),(1,=C'K')           
         CLI   FLDH+5,0                                                         
         BE    GCPMR                                                            
         MVC   XTRA(15),=C'-,A-Z,1-9 VALID'                                     
         ZIC   RE,FLDH+5                                                        
         LA    RF,FLD                                                           
FILTR10  CLI   0(RF),C'-'                                                       
         BE    FILTR12                                                          
         CLI   0(RF),C'A'          BETWEEN A - 9                                
         BL    ERROR                                                            
         CLI   0(RF),C'9'                                                       
         BH    ERROR                                                            
FILTR12  LA    RF,1(RF)                                                         
         BCT   RE,FILTR10                                                       
         MVC   XTRA,SPACES                                                      
*                                                                               
         XC    WORK(11),WORK                                                    
         MVC   WORK(3),=XL3'080BD2'                                             
         MVC   WORK+3(6),FLD                                                    
         GOTO1 VHELLO,DMCB,(C'P',APUNTFIL),APACKREC,WORK,0                      
         GOTO1 VHELLO,DMCB,(C'G',APUNTFIL),(X'08',0(R4)),(1,=C'K')              
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                JUST ADDED HAS TO BE THERE                   
         MVC   APAKFEL,12(R1)      SAVE FOR DISPLAY                             
         SPACE                                                                  
GCPMR    DS    0H                                                               
         B     POSTDATA                                                         
*        LA    R2,PAKGCPMH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    LSTED                                                            
         ZIC   R3,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R3)                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
         MVC   NPAKGCPM,DMCB+4                                                  
         SPACE                                                                  
POSTDATA LA    R2,PAKPOSDH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BZ    LSTED                                                            
         CLI   FLD,C'D' DEFAULT KEEP AS ZERO                                    
         BE    LSTED                                                            
         CLI   FLD,C'C'                                                         
         BE    POST100                                                          
         CLI   FLD,C'A'                                                         
         BE    POST100                                                          
         CLI   FLD,C'Z'                                                         
         BE    POST100                                                          
         CLI   FLD,C'I'                                                         
         BE    POST100                                                          
         MVI   FERN,INVPMETR                                                    
         B     ERROR                                                            
POST100  MVC   NPAKPDT,FLD                                                      
         SPACE                                                                  
LSTED    MVC   NPAKACTD,TODAYC     LAST ACTIVITY                                
         MVI   NPAKACTA,C'A'                                                    
*                                                                               
VBTYPE   DS    0H                  VALIDATE BUY TYPE                            
         L     R4,APACKREC                                                      
         USING NPRECD,R4                                                        
*                                                                               
         XC    WORK(30),WORK                                                    
         LA    R3,WORK                                                          
         USING NPK2D,R3                                                         
         MVI   NPK2EL,X'02'                                                     
         MVI   NPK2LEN,NPK2ELN                                                  
*                                                                               
         LA    R2,PAKBTYPH                                                      
*!!!     MVI   FERN,INVERR                                                      
         GOTO1 VGETFLD                                                          
*                                                                               
         CLI   FLDH+5,0                                                         
         BE    VBTADD02                                                         
         CLI   FLD,C'O'            OPPORTUNISTIC ?                              
         BE    VBTYP10                                                          
         CLI   FLD,C'S'            SCATTERED ?                                  
         BE    VBTYP10                                                          
         CLI   FLD,C'U'            OPPORTUNISTIC ?                              
         BNE   INVERRM                                                          
*                                                                               
VBTYP10  MVC   NPK2BTYP,FLD                                                     
*                                                                               
VBTADD02 DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'P',APUNTFIL),(R4),WORK,0                          
         DROP  R3,R4                                                            
*                                                                               
APXIT    B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
* MODULE AND SUB-ROUTINE EXIT                                                   
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
*                                                                               
ADDMCST  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',UNTFILE),(X'15',(R3)),0                       
*                                                                               
         XC    ELMWORK,ELMWORK                                                  
*&&DO                                                                           
         LA    RF,ELMWORK                                                       
         USING NUMCSTD,RF                                                       
*                                                                               
         MVI   NUMCSEL,X'15'                                                    
         MVI   NUMCSLEN,NUMCSLNQ                                                
         MVC   NUMCMCS,NUACTUAL                                                 
         DROP  RF                                                               
*&&                                                                             
*                                                                               
         MVI   ELMWORK,X'15'                                                    
         MVI   ELMWORK+1,X'0B'                                                  
         MVC   ELMWORK+2(4),NUACTUAL                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,ELMWORK,0                    
*                                                                               
ADDMCSTX DS    0H                                                               
         XMOD1 1                                                                
         DROP  R3                                                               
*                                                                               
APUNTFIL DC    CL8'UNTFILE'                                                     
* EXECUTED INSTRUCTIONS                                                         
*                                                                               
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO'                                                    
*                                                                               
PRINCOMP CLC   FLD(0),=C'PRINT'                                                 
ZONECOMP CLC   FLD(0),=C'ZONE'                                                  
         SPACE 2                                                                
* TABLE OF DAYPART CODES AND NAMES                                              
*&&DO                                                                           
DPTTAB   DS    0CL9                                                             
         DC    C'D',CL8'DAYTIME'                                                
         DC    C'B',CL8'CBLSPORT'                                               
         DC    C'U',CL8'UNWIRED'                                                
         DC    C'F',CL8'FRINGE'                                                 
         DC    C'P',CL8'PRIME'                                                  
         DC    C'K',CL8'KIDS'                                                   
         DC    C'T',CL8'TEENS'                                                  
         DC    C'Y',CL8'YOUTH'                                                  
         DC    C'S',CL8'SPORTS'                                                 
         DC    C'N',CL8'NEWS'                                                   
         DC    C'E',CL8'EARLY'                                                  
         DC    C'L',CL8'LATE'                                                   
         DC    C'C',CL8'CABLE'                                                  
         DC    C'O',CL8'OLYMPICS'                                               
         DC    C'R',CL8'RADIO'                                                  
         DC    C'X',CL8'SYND.'                                                  
         DC    C'X',CL8'X'                                                      
         DC    C'I',CL8'SPECIAL'                                                
         DC    C'I',CL8'I'                                                      
         DC    C'V',CL8'OVERNITE'                                               
         DC    C'V',CL8'V'                                                      
         DC    C'W',CL8'WKNDPM'                                                 
         DC    C'M',CL8'WKNDAM'                                                 
         DC    C'M',CL8'M'                                                      
         DC    C'A',CL8'ACCESS'                                                 
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
*&&                                                                             
         SPACE 2                                                                
* TABLE OF STATUS ACTIONS (COVERED BY STATABD)                                  
*              BYTES 0-7 = STATUS KEYWORD                                       
*              BYTE  8   = MINIMUM COMPARE LENGTH                               
*              BYTE  9   = OR MASK FOR STATUS                                   
*              BYTE 10   = AND MASK FOR STATUS                                  
*              BYTE 11   = STATUS INDICATORS                                    
*                                                                               
STATAB   DS    0CL(STATABL)                                                     
         DC    CL8'LOCKED',AL1(1),AL1(LOCKED)                                   
         DC    X'FF',AL1(UNITFIX)                                               
*                                                                               
         DC    CL8'UNLOCKED',AL1(3),X'00'                                       
         DC    AL1(UNLOCKED),AL1(UNITFIX)                                       
*                                                                               
         DC    CL8'FROZEN',AL1(1),AL1(FROZEN)                                   
         DC    X'FF',X'00'                                                      
*                                                                               
         DC    CL8'UNFROZEN',AL1(3),X'00'                                       
         DC    AL1(UNFROZEN),X'00'                                              
*                                                                               
STAENT   EQU   (*-STATAB)/L'STATAB                                              
*                                                                               
MAXPTRS  EQU   80                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* PPU TRANSFER SCREEN - IT SMALLER SO IT GOES 1ST                               
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF3D                                                       
         EJECT                                                                  
* PACKAGE SCREEN                                                                
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFDD                                                       
         ORG   TWAD+PAGELEN                                                     
SVAREA18 DS    0CL256                                                           
SVUAKEY  DS    CL20                SAVED UNIT ASS. KEY                          
SVACTLIN DS    CL61                LAST DPT,PLAN,DATES,LEN                      
PLAN     DS    CL4                 PLAN CODE                                    
PLNDPT   DS    CL1                 PLAN DAYPART                                 
PLNTVQBK DS    CL2                 PLAN TVQ BOOK                                
STRTDTE  DS    XL6                 START DATE                                   
ENDDTE   DS    XL6                 END DATE                                     
TRGDEMO  DS    XL1                 TARGET DEMO                                  
LENS     DS    XL4                 LENGTHS FROM PUP RECORD                      
SVLEN1   DS    XL1                 FILTER LENGTHS 1                             
SVLEN2   DS    XL1                 FILTER LENGTHS 2                             
SVQTBLS  DS    0XL4                WHICH QUARTERS ARE IN USE                    
SVQTBL1  DS    XL1                 QUARTERS 1                                   
SVQTBL2  DS    XL1                 QUARTERS 2                                   
SVQTBL3  DS    XL1                 QUARTERS 3                                   
SVQTBL4  DS    XL1                 QUARTERS 4                                   
PLANYR   DS    XL1                 CURRENT PLAN YEAR                            
PLHAVE   DS    XL1                 PLANS HUT AVERAGE                            
SVACTION DS    XL1                 LAST ACTION #                                
SVSTDTE  DS    XL3                 DATE TO START ADDING UNITS                   
SVENDTE  DS    XL3                 LAST DATE TO END ADDDING UNITS               
PACKGU   DS    XL2                 PACKAGE GUARENTEE FACTOR                     
NPACKGU  DS    XL4                 NEW PACKAGE GUARENTEE FACTOR                 
NDEMOGU  DS    XL4                 NEW DEMO GUARENTEE FACTOR                    
NDEMOCAT DS    XL3                 NEW DEMO CATEGORY FOR GUARENTEE              
DEMOGU   DS    XL2                 DEMO GUARENTEE FACTOR                        
DEMOCAT  DS    XL1                 DEMO CATEGORY FOR GUARENTEE                  
CALCHUT  DS    XL1                 IF C'Y' RECALCULATE PUP HUT                  
PLNDEMS  DS    XL18                                                             
PLNUPLST DS    XL1                                                              
IOAREA5  DS    2000X               READ IN ASS. RECORD                          
         ORG                                                                    
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VEDIT    DS    A                                                                
UNITDA   DS    A                                                                
NPTRS    DS    F                   NUMBER OF POINTERS                           
PUNITS   DS    F                   PROGRAM UNITS                                
*                                                                               
STATSW   DS    C                   Y=UPDATE STATUS FIELD ON UNITS               
ORMASK   DS    X                                                                
ANDMASK  DS    X                                                                
OLDSTAT  DS    X                                                                
NEWSTAT  DS    X                                                                
*                                                                               
         DS    0F                                                               
BLOCK    DS    CL150                                                            
ELMWORK  DS    CL130                                                            
OVRBITS  DS    XL1                 BITS FOR OVERRIDES                           
OVSHR    DS    XL2                 SHARE FROM ASSIGNMENT RECORD                 
OVHUT    DS    XL2                 HUT FROM ASSIGNMENT RECORD                   
OVRTG    DS    XL2                 RATING FROM ASSIGNMENT RECORD                
OVVPH    DS    XL2                 VPH FROM ASSIGNMENT RECORD                   
UNTNUM   DS    XL4                 UNITS/LENGTH ASS. RECORD                     
UNITS    DS    XL1                 TOTAL UNITS                                  
ACTCOSTS DS    XL16                ACTUAL COSTS FOR 1-4 LENGTHS                 
COUNT    DS    XL1                 TEMPORARY # WEEKS COUNTER                    
FSTDTE   DS    XL6                 FIRST DATE OF PROGRAM                        
NXTDTE   DS    XL2                 NEXT UNIT DATE - COMPRESSED                  
ENDDTE2  DS    XL2                 END DATE - COMPRESSED                        
NUMWKS   DS    XL1                 NUMBER OF WEEKS IN PERIOD                    
NUMUNAD  DS    XL1                 NUMBER OF UNITS                              
NUMPUAD  DS    XL1                 NUMBER OF PARTIAL WEEKS TO ADD UNIT          
CTPOSTYP DS    XL1                 POSTING TYPE CONTROL BIT SETTING             
PRGSW    DS    XL1                 USED TO CALCULATE # IO'S                     
STLEN    DS    XL1                 UNIT LENGTH                                  
LOOP1    DS    XL1                 NUMBER OF UNITS TO ADD FOR DATE              
NPROGS   DS    XL1                 NUMBER OF PROGRAMS PROCESSED                 
SVXTRA   DS    XL7                 WHICH QTRS WERE PREVIOUSLY UPLOADED          
PUPDEMS  DS    XL20                OVERRIDE PUP DEMO AMOUNTS                    
SVINTTBL DS    1900C                                                            
LCLSPACE EQU   *-MYRELO                                                         
         SPACE 2                                                                
* DSECT TO COVER STATUS TABLE                                                   
*                                                                               
STATABD  DSECT                                                                  
STANAME  DS    CL8                 NAME                                         
STAMIN   DS    X                   MINIMUM COMPARE LENGTH                       
STAOR    DS    X                   OR MASK                                      
STAND    DS    X                   AND MASK                                     
STACTL   DS    X                   CONTROL VALUES (X'80'=CHANGE UNITS)          
STATABL  EQU   *-STATABD                                                        
         SPACE 2                                                                
* MODULE EQUATES                                                                
*                                                                               
UNITFIX  EQU   X'80'               UNITS MUST BE FIXED FOR THIS STATUS          
FROZEN   EQU   X'80'                                                            
LOCKED   EQU   X'20'                                                            
NOPRINT  EQU   X'10'                                                            
UNFROZEN EQU   X'FF'-X'80'                                                      
UNLOCKED EQU   X'FF'-X'20'                                                      
PRINT    EQU   X'FF'-X'10'                                                      
EQUAL    EQU   C'='                                                             
         SPACE 2                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 2                                                                
* NEGENPLAN                                                                     
         PRINT OFF                                                              
       ++INCLUDE NEGENPLAN                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* NEGENPUA                                                                      
         PRINT ON                                                               
       ++INCLUDE NEGENPUA                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENUNIV                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPGENUNIV                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENMKG                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENMKG                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENEST (ESTHDRD)                                                            
         PRINT OFF                                                              
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* SPGENREP (REPRECD)                                                            
         PRINT OFF                                                              
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027NEBUY18   02/26/20'                                      
         END                                                                    
