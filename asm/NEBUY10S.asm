*          DATA SET NEBUY10S   AT LEVEL 003 AS OF 05/01/02                      
*          DATA SET NEBUY10    AT LEVEL 110 AS OF 10/26/99                      
*PHASE T31110C,+0                                                               
         TITLE 'NETPAK BUY PROGRAM PACKAGE OVERLAY - T31110'                    
T31110   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUY10*,RA,RR=RE                                              
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
         USING NPRECD,R4           R4 WILL COVER PACKAGE RECORD                 
         SPACE 2                                                                
PAK      CLI   ACTION,BP           TEST FOR BUYING PACKAGE                      
         BE    PAK2                                                             
         CLI   ACTION,COPYP        TEST FOR COPY                                
         BNE   PAK4                NO                                           
         TM    MODE,DISPLAY        YES-TEST FOR FORCED DISPLAY                  
         BO    PAK8                DISPLAY PACKAGE TO BE COPIED FIRST           
         SPACE 1                                                                
PAK2     BAS   RE,BUILD                                                         
         CLI   BUYPROF+11,YES                                                   
         BNE   PAK3                MANUEL PACKAGE                               
         BAS   RE,TESTPACK                                                      
         B     *+8                                                              
PAK3     BAS   RE,GETPACK                                                       
         MVC   PACKAGE,PACK        SET SAVED PACKAGE                            
         ZIC   R2,PACK                                                          
         EDIT  (R2),(3,BUYPAK),ALIGN=LEFT                                       
         OI    BUYPAKH+6,X'80'     TRANSMIT NUMBER BACK                         
         L     R4,ANEWREC                                                       
         MVC   NPKPACK,PACK        ADD THE RECORD                               
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,ANEWREC                                 
         MVC   NBAIO,ANEWREC       POINT TO NEW PACKAGE RECORD                  
         MVI   NBFUNCT,NBFVAL      FORCE VALUEING OF IT                         
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         GOTO1 DIS,(R1),ANEWREC                                                 
         B     PAKX                                                             
         SPACE                                                                  
PAK4     CLI   ACTION,CP           TEST FOR CHANGE                              
         BNE   PAK6                                                             
         TM    MODE,DISPLAY                                                     
         BO    PAK8                                                             
         BAS   RE,BUILD                                                         
         CLI   STATSW,YES          TEST FOR CHANGING UNITS                      
         BNE   PAK5                                                             
         L     R4,ANEWREC                                                       
         BAS   RE,STATUS                                                        
*                                                                               
         TM    ORMASK,FROZEN+LOCKED TEST FOR UNLOCK, UNFREEZE                   
         BZ    *+14                (ZERO IN ORMASK)                             
         MVC   PAKSTAT,SPACES      YES-CLEAR THE FIELD                          
         OI    PAKSTATH+6,X'80'                                                 
         SPACE                                                                  
PAK5     L     R4,ANEWREC                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NPKEY),0(R4)  RE-READ PACKAGE                              
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(L'NPKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         B     PAKX                                                             
         SPACE                                                                  
PAK6     CLI   ACTION,DELP         TEST FOR DELETE                              
         BNE   PAK8                                                             
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,AUDACTER       CANNOT DELETE IF AUDIT IS SET                
         L     R4,APACKREC                                                      
         TM    NPAKSTAT,X'02'                                                   
         BO    ERROR                                                            
         TM    MODE,DISPLAY                                                     
         BO    PAK8                                                             
         BAS   RE,DELETE                                                        
         B     PAKX                                                             
         SPACE                                                                  
PAK8     GOTO1 DIS,DMCB,APACKREC   DISPLAY PACKAGE                              
         B     PAKX                                                             
         SPACE                                                                  
PAKX     BAS   RE,MSG              MESSAGE AND CURSOR POSITION                  
         NI    MODE,X'FF'-DISPLAY  TURN OFF DISPLAY SETTING                     
         B     EXXMOD                                                           
         EJECT                                                                  
* DISPLAY PACKAGE VALUES ON SCREEN (P1=A(PACKAGE RECORD))                       
*                                                                               
DIS      NTR1                                                                   
         L     R4,0(R1)            R4 POINTS TO PACKAGE RECORD                  
         GOTO1 VCLEARF,DMCB,PAKNAMEH,PAKLAST                                    
*                                                                               
         MVC   PAKNAME,NPAKNAME                                                 
         MVC   PAKDPN,NBDPNAM                                                   
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
DIS4     MVI   PAKDBSE,C'V'                                                     
         TM    NPAKCNTL,X'40'                                                   
         BZ    *+8                                                              
         MVI   PAKDBSE,C'I'                                                     
*                                                                               
         CLI   NPAKHUTS,0          HUT SCHEME                                   
         BE    *+10                                                             
         MVC   PAKHSC(1),NPAKHUTS                                               
         CLI   NPAKHUTA,0          HUT AVERAGE                                  
         BE    *+10                                                             
         MVC   PAKHAVE(1),NPAKHUTA                                              
         CLI   NPAKHUTF,0          HUT FLAVOR                                   
         BE    *+10                                                             
         MVC   PAKHAVE+1(1),NPAKHUTF                                            
         OC    NPAKHPCT,NPAKHPCT   TEST HUT ADJ PCT.                            
         BZ    DIS4A                                                            
         SR    R2,R2                                                            
         ICM   R2,3,NPAKHPCT                                                    
         EDIT  (R2),(6,PAKHADJ),2,ALIGN=LEFT                                    
DIS4A    CLI   NPAKHTYP,0          HUT TYPE                                     
         BE    *+10                                                             
         MVC   PAKHTYP,NPAKHTYP                                                 
         OC    NPAKLENG,NPAKLENG   LENGTH                                       
         BZ    DIS4B                                                            
         EDIT  (B1,NPAKLENG),(3,PAKLEN),ALIGN=LEFT                              
DIS4B    OC    NPAKUNIV,NPAKUNIV   TEST UNIVERSE PCT.                           
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
*IS7     OC    NPAKFMG,NPAKFMG     TEST FOR FEED MKT GROUP                      
*        BZ    DIS7A                                                            
*        MVC   PAKFMGR(1),NPAKFMG                                               
*        UNPK  DUB(3),NPAKFMG+1(2)                                              
*        MVC   PAKFMGR+1(2),DUB                                                 
*                                                                               
DIS7     CLI   NPAKMAST,0                                                       
         BE    DIS8                                                             
*                                                                               
         LA    R0,220                                                           
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
         SPACE 1                                                                
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
         BZ    DIS14                                                            
         MVC   PAKPOSD,NPAKPDT                                                  
         SPACE                                                                  
DIS14    ICM   R3,15,NPAKGCPM                                                   
         BZ    DIS15                                                            
         EDIT  (R3),(8,PAKGCPM),2,ALIGN=LEFT                                    
         SPACE                                                                  
*--DISPLAY SECONDARY ELEMENT INFORMATION                                        
*                                                                               
DIS15    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',0(R4)),0                       
         CLI   12(R1),0                                                         
         BNE   DIS16                                                            
         L     RE,12(R1)           FOR DISPLAY                                  
         USING NPK2D,RE                                                         
         MVC   PAKNAM2(16),NPK2NME2    SECOND PACKAGE NAME                      
         DROP  RE                                                               
         SPACE                                                                  
*                                                                               
DIS16    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'09',0(R4)),0                       
         CLI   12(R1),0                                                         
         BNE   DIS20                                                            
         L     RE,12(R1)           FOR DISPLAY                                  
         USING NAUDD,RE                                                         
         MVC   PAKAUDG(4),NAUDGRP      AUDIT GROUP                              
         MVC   PAKAUDC(16),NAUDCOM     AUDIT COMMENT                            
         DROP  RE                                                               
         SPACE                                                                  
DIS20    CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DISX                                                             
*                                                                               
         LA    R4,KEY              READ PACKAGE DIRECTORY ENTRY                 
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
*                                                                               
DISX     B     EXXMOD                                                           
         EJECT                                                                  
* ROUTINE TO EDIT PACKAGE SCREEN AND BUILD PACKAGE RECORD                       
*                                                                               
BUILD    NTR1                                                                   
         L     R4,AIOAREA1                                                      
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
         CLI   ACTION,CP           TEST FOR ACTION CHANGE                       
         BNE   *+10                NO                                           
         MVC   NPKPACK,PACK        SET PACKAGE NUMBER IN KEY                    
*                                                                               
         MVI   NPAKEL,X'01'                                                     
         MVI   NPAKLEN,60                                                       
         ZIC   R1,NPAKLEN                                                       
         LA    R1,NPAKEL-NPKEY+1(R1)  SET INITIAL RECORD LENGTH                 
         STH   R1,NPKRLEN                                                       
*                                                                               
         MVI   ANDMASK,X'FF'       INITIALIZE ANDMASK                           
*                                                                               
         CLI   ACTION,BP           TEST FOR PACKAGE ADD                         
         BNE   *+12                NO                                           
         OI    NPAKCNTL,X'20'      SET NO BUYS UNDER PACKAGE                    
         B     HUTED                                                            
*                                                                               
         L     R1,APACKREC                                                      
         MVC   OLDCNTL,NPAKCNTL-NPKEY(R1)                                       
         NI    OLDCNTL,X'28'                                                    
         OC    NPAKCNTL(1),OLDCNTL                                              
*                                                                               
HUTED    OI    NPAKHUTL,X'80'      GET HUTS FORM DEMO FILE                      
*        CLI   BUYPROF+3,YES       TEST FOR 52 WEEK OPTION                      
*        BNE   *+8                 NO                                           
         OI    NPAKHUTL,X'40'      YES                                          
         SPACE 1                                                                
NAMED    LA    R2,PAKNAMEH                                                      
         MVI   FERN,MISERR                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVC   NPAKNAME,FLD                                                     
         SPACE                                                                  
*--ALL INFO THAT GOES INTO THE X'02' ELEMENT SHOULD GO HERE                     
*                                                                               
NME2D    GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'02',AIOAREA1),0                    
         XC    WORK(30),WORK                                                    
         LA    R3,WORK                                                          
         USING NPK2D,R3                                                         
         MVI   NPK2EL,X'02'                                                     
         MVI   NPK2LEN,NPK2ELN                                                  
         LA    R2,PAKNAM2H                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    *+10                                                             
         MVC   NPK2NME2(16),PAKNAM2                                             
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
         DROP  R3                                                               
         SPACE                                                                  
DPNED    LA    R2,PAKDPNH          VALIDATE DAYPART NAME                        
         MVI   FERN,MISERR                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    ERROR               MUST HAVE DAYPART                            
*                                                                               
         MVI   FERN,INVERR                                                      
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
         B     ERROR                                                            
         SPACE                                                                  
DPNED4   MVC   NPAKDP,0(RE)        SET DAYPART CODE                             
         MVC   PAKDPN(L'DPTTAB-1),1(RE)                                         
         OI    PAKDPNH+6,X'80'     SEND EXPANDED NAME BACK TO USER              
         CLI   ACTION,CP           TEST FOR ACTION CHANGE                       
         BNE   DPNEDX                                                           
         L     R3,APACKREC         POINT TO OLD PACKAGE RECORD                  
         CLC   NPAKDP,NPAKDP-NPKEY(R3) TEST FOR ANY CHANGE                      
         BE    DPNEDX                                                           
         LA    R0,NPAKDP-NPKEY(R3)                                              
         GOTO1 CHKUN,DMCB2,(R0)    CHECK FOR ANY UNITS                          
         BNE   DPNEDX              NONE                                         
         MVI   FERN,DPCHGERR                                                    
         B     ERROR                                                            
         SPACE 1                                                                
DPNEDX   B     GCPM                                                             
         SPACE                                                                  
GCPM     LA    R2,PAKGCPMH         GUARANTEE CPM                                
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    STAED                                                            
         ZIC   R3,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R3)                                           
         CLI   DMCB,X'FF'          TEST FOR ERROR                               
         BE    ERROR                                                            
         MVC   NPAKGCPM,DMCB+4                                                  
         SPACE                                                                  
* EDIT STATUS FIELD                                                             
*                                                                               
STAED    LA    R2,PAKSTATH         VALIDATE STATUS                              
         GOTO1 VGETFLD                                                          
         CLI   ACTION,CP           TEST FOR ACTION CHANGE                       
         BNE   *+14                                                             
         L     R1,APACKREC         YES-EXTRACT EXISTING STATUS                  
         MVC   OLDSTAT,NPAKSTAT-NPKEY(R1)                                       
*                                                                               
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   STAED1              YES                                          
         CLI   ACTION,CP           NO INPUT-TEST ACTION CHANGE                  
         BNE   STAEDX              NO                                           
         MVC   NPAKSTAT,OLDSTAT    YES-INITIALIZE STATUS                        
         TM    OLDSTAT,FROZEN+LOCKED  TEST IF PKG. WAS FROZEN/LOCKED            
         BZ    STAEDX              NO                                           
         MVI   FERN,MISERR         MUST 'UNDO' STATUS FIRST                     
         B     STAED30                                                          
         SPACE                                                                  
STAED1   LA    RE,STATAB           POINT RE AT STATUS TABLE                     
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
         CLI   ACTION,CP           TEST FOR CHANGE                              
         BNE   *+10                                                             
         MVC   NPAKSTAT,OLDSTAT    YES-INITIALIZE STATUS                        
         SPACE                                                                  
STAED6   OC    NPAKSTAT,STAOR      NOW APPLY THE TABLE ENTRY'S                  
         NC    NPAKSTAT,STAND      OR MASK AND 'AND' MASK                       
         MVC   NEWSTAT,NPAKSTAT    SAVE NEW STATUS                              
         TM    STACTL,BLDAUDT      TEST IF AUDIT ELEM SHOULD BE BUILT           
         BZ    *+8                                                              
         MVI   AUDSW,YES                                                        
         TM    STACTL,UNITFIX      TEST IF STATUS CHANGES UNITS                 
         BZ    STAED7                                                           
         MVI   STATSW,YES                                                       
         OC    ORMASK,STAOR        UPDATE CUMULATIVE OR/AND MASKS               
         NC    ANDMASK,STAND                                                    
         SPACE                                                                  
STAED7   CLI   ACTION,CP           TEST FOR ACTION CHANGE                       
         BNE   STAED8                                                           
         TM    STACTL,CKACCNT      TEST ACCOUNTING CHECK                        
         BZ    *+8                                                              
         BAS   RE,CHKACC           YES-LOOK FOR BILL/PAY ACTIVITY               
         TM    STACTL,RESUPLD      RESET UPLOAD BIT                             
         BZ    STAED8              NO                                           
         OI    NPAKCNTL,X'20'                                                   
         NI    NPAKCNTL,X'F7'                                                   
         SPACE 1                                                                
STAED8   TM    STACTL,SETUPLD                                                   
         BZ    STAED12                                                          
         TM    NPAKCNTL,X'20'      CANNOT CABLE LOCK WITH UNITS                 
         BNZ   *+12                                                             
         MVI   FERN,CLOCKERR                                                    
         B     ERROR                                                            
         OI    NPAKCNTL,X'08'                                                   
* STOP DIRECT SWITCH BETWEEN LOCKED/FROZEN - MUST UNDO STATUS FIRST             
*                                                                               
STAED12  CLI   ACTION,CP           TEST ACTION CHANGE                           
         BNE   STAEDX              NO-ALL DONE                                  
         TM    OLDSTAT,FROZEN      TEST IF PREVIOUSLY FROZEN                    
         BZ    *+16                NO                                           
         TM    NEWSTAT,LOCKED      TEST IF CHANGED TO LOCKED                    
         BO    STAED30             YES-ITS AN ERROR                             
         B     STAED20             COMBINE OLD/NEW BITS                         
         TM    OLDSTAT,LOCKED      TEST IF FORMERLY LOCKED                      
         BZ    STAED20             NO                                           
         TM    NEWSTAT,FROZEN      TEST IF CHANGED TO FROZEN                    
         BNZ   STAED30             NO                                           
* STOP DIRECT SWITCH BETWEEN LOCKED/CABLE LOCKED                                
*                                                                               
STAED20  TM    NPAKCNTL,X'08'      TEST IF UPLSET                               
         BZ    STAEDX              NO                                           
         TM    NEWSTAT,LOCKED      IS PACKAGE BEING LOCKED                      
         BO    STAED40             YES-ITS AN ERROR                             
         TM    OLDSTAT,LOCKED      TEST IF FORMERLY LOCKED                      
         BO    STAED40             YES-ITS AN ERROR                             
         B     STAEDX                                                           
         SPACE                                                                  
STAED30  MVC   XTRA(19),=C'''UNDO'' STATUS FIRST'                               
         B     ERROR                                                            
*                                                                               
STAED40  MVC   BUYMSG(L'LKSTERR),LKSTERR       LOCK STATUS ERROR                
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
         SPACE                                                                  
STAEDX   B     OPTED                                                            
         DROP  RE                                                               
         SPACE 1                                                                
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
         MVI   FERN,MISERR                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            MUST SPECIFY COST                            
         BE    ERROR                                                            
         MVI   FERN,INVERR                                                      
         TM    FLDH+4,X'08'        EST FOR NUMERIC DATA                         
         BZ    ERROR                                                            
         STCM  R0,15,NPAKCOST                                                   
         SPACE                                                                  
INTED    LA    R2,PAKINTH          INTEGRATION RATE                             
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    INTED3                                                           
         CLC   =C'TBL',FLD                                                      
         BE    INTED4                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(2,BLOCK),0                                   
         MVI   FERN,INVERR                                                      
         SR    R3,R3                                                            
         ICM   R3,1,4(R1)                                                       
         BZ    ERROR                                                            
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
         MVC   NEWSTAT,NPAKSTAT                                                 
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
         CLI   ACTION,BP           TEST FOR PACKAGE ADD                         
         BNE   HSCEDX              NO                                           
         CLI   BUYPROF,STAR        YES-SEE IF PROFILE HAS A SCHEME              
         BE    *+10                NO                                           
         MVC   NPAKHUTS,BUYPROF    YES                                          
         B     HSCEDX                                                           
         SPACE 1                                                                
HSCED2   CLI   FLDH+5,1                                                         
         BNE   ERROR                                                            
         TM    FLDH+4,X'0C'        TEST FOR ALPHA-NUMERIC                       
         BZ    ERROR               NEITHER-NO SPECIAL CHARACTERS                
         MVC   NPAKHUTS,FLD                                                     
         SPACE                                                                  
HSCEDX   B     HAVED                                                            
         SPACE 1                                                                
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
HAVE010  CLI   FLDH+5,1                                                         
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
         MVI   NPAKHTYP,C'A'                                                    
         CLI   FLDH+5,0                                                         
         BE    DEMBASE                                                          
         CLI   FLD,C'A'                                                         
         BE    *+20                                                             
         CLI   FLD,C'D'                                                         
         BE    *+12                                                             
         CLI   FLD,C'I'                                                         
         BNE   ERROR                                                            
         MVC   NPAKHTYP,FLD                                                     
         SPACE                                                                  
DEMBASE  CLI   ACTION,BP           TEST FOR PACKAGE ADD                         
         BE    DEMB05              NO                                           
         L     R1,APACKREC                                                      
         MVC   OLDCNTL,NPAKCNTL-NPKEY(R1)                                       
         NI    OLDCNTL,X'40'                                                    
*        OC    NPAKCNTL(1),OLDCNTL                                              
*        B     LENGED                                                           
*                                                                               
DEMB05   LA    R2,PAKDBSEH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    DEMB20                                                           
*                                                                               
DEMB10   CLI   FLD,C'V'                                                         
         BE    DEMB15                                                           
         CLI   FLD,C'I'                                                         
         BNE   ERROR                                                            
         OI    NPAKCNTL,X'40'                                                   
*                                                                               
DEMB15   CLI   ACTION,BP           TEST FOR PACKAGE ADD                         
         BE    LENGED              YES NEXT EDIT                                
         MVC   BYTE,NPAKCNTL                                                    
         NI    BYTE,X'40'                                                       
         CLC   BYTE,OLDCNTL        CHECK FOR CHANGE IN BASE FIELD               
         BNE   ERROR                                                            
         B     LENGED                                                           
*                                                                               
DEMB20   CLI   NBPOSTYP,C'C'       PROFILE ONLY FOR CABLE                       
         BNE   LENGED                                                           
         CLI   BUYPROF2+9,C'X'     NO DEFAULT ALLOWED                           
         BE    ERROR                                                            
         MVC   FLD(1),BUYPROF2+9   IF NO INPUT USE PROFILE                      
         CLI   FLD,C'I'                                                         
         BE    DEMB10                                                           
         MVI   FLD,C'V'                                                         
         B     DEMB10                                                           
*                                                                               
LENGED   LA    R2,PAKLENH          LENGTH (SECONDS)                             
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    UPCTED                                                           
         ZIC   R1,FLDH+5                                                        
         LR    R0,R1               SAVE DATA LENGTH                             
         LA    RE,FLD              RE=DATA POINTER                              
VALL100  CLI   0(RE),C'0'          TEST FOR NUMERIC DATA                        
         BL    ERROR                                                            
         CLI   0(RE),C'9'                                                       
         BH    ERROR                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,VALL100                                                       
*                                                                               
         LR    R1,R0               RESTORE ORIGINAL LENGTH                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
*                                                                               
         LTR   R0,R0               TEST FOR ZERO                                
         BZ    ERROR                                                            
         CH    R0,=H'255'          TEST FOR MAXIMUM VALUE                       
         BH    ERROR                                                            
         STC   R0,NPAKLENG                                                      
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
         LA    R0,220                                                           
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
         BE    AUDED                                                            
         ZIC   R3,FLDH+5                                                        
         GOTO1 VCASHVAL,DMCB,FLD,(R3)                                           
         CLI   DMCB,X'FF'                                                       
         BE    ERROR                                                            
         MVC   NPAKIMP,DMCB+6                                                   
         SPACE                                                                  
AUDED    LA    R2,PAKAUDGH         AUDIT GROUP/COMMENT                          
         GOTO1 VGETFLD                                                          
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'09',AIOAREA1),0                    
         CLI   FLDH+5,0                                                         
         BE    AUDED5                                                           
         XC    WORK(30),WORK                                                    
         LA    R3,WORK                                                          
         USING NAUDD,R3                                                         
         MVI   NAUDEL,X'09'                                                     
         MVI   NAUDLEN,NAUDELN                                                  
         MVC   NAUDGRP(4),PAKAUDG                                               
         MVC   NAUDCOM(16),PAKAUDC                                              
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
         B     REPED                                                            
AUDED5   TM    NPAKSTAT,X'02'                                                   
         BZ    REPED                                                            
         MVI   FERN,MISERR                                                      
         B     ERROR                                                            
         DROP  R3                                                               
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
         SPACE 1                                                                
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
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'08',AIOAREA1),(1,=C'K')            
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
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'08',0(R4)),(1,=C'K')               
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
         CLI   FLD,C'A'                                                         
         BE    PD100                                                            
         CLI   FLD,C'C'                                                         
         BE    PD100                                                            
         CLI   FLD,C'I'                                                         
         BE    PD100                                                            
         CLI   FLD,C'Z'                                                         
         BE    PD100                                                            
         MVI   FERN,INVPMETR                                                    
         B     ERROR                                                            
PD100    MVC   NPAKPDT,FLD                                                      
         SPACE                                                                  
LSTED    MVC   NPAKACTD,TODAYC     LAST ACTIVITY                                
         MVI   NPAKACTA,C'A'                                                    
         CLI   ACTION,CP           TEST FOR CHANGE                              
         BNE   *+8                 NO                                           
         MVI   NPAKACTA,C'C'       YES                                          
         ST    R4,ANEWREC          SAVE NEW RECORD ADDRESS                      
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT PACKAGE NUMBER (NUMBER RETURNED IN PACK)              
*                                                                               
TESTPACK NTR1                                                                   
         LA    R4,KEY                                                           
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
         B     ERROR                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT PACKAGE NUMBER (NUMBER RETURNED IN PACK)              
*                                                                               
GETPACK  NTR1                                                                   
         LA    R4,KEY                                                           
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
         EJECT                                                                  
* SUB-ROUTINE TO CHANGE THE STATUS FIELD ON ALL UNITS FOR THE PACKAGE           
*                                                                               
STATUS   NTR1                                                                   
*                                                                               
         XC    BLOCK,BLOCK                                                      
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'09',0(R4)),0                       
         CLI   12(R1),0                                                         
         BNE   STA20                                                            
         L     RE,12(R1)                                                        
         MVC   BLOCK(22),0(RE)         SAVE AUDIT ELEMENT                       
*                                                                               
STA20    LA    R2,300              UNLOCK TABLE COUNT                           
*                                                                               
         MVI   NBSELMOD,NBPROCUN                                                
         MVC   NBSELDP,NPAKDP      SET PACKAGE DAYPART                          
         MVC   NBSELSTR,ESTSTART                                                
         MVC   NBSELEND,ESTEND                                                  
         MVI   NBUSER+13,NO        FORCE BACK PRE-EMPTS                         
         MVI   NBDATA,C'U'         READ UNITS                                   
         MVI   NBSEQ,C'D'                                                       
         MVC   NBAIO,AIOAREA2                                                   
         LA    R1,STAHOOK          PASS ADDRESS OF HOOK                         
         ST    R1,NBHOOK                                                        
*                                                                               
STA40    GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,0           TEST FOR ERROR                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     TEST FOR END-OF-FILE                         
         BE    EXXMOD              YES-ALL DONE AND EXIT                        
         BCT   R2,STA40                                                         
         GOTO1 VDATAMGR,DMCB,(0,=C'DMUNLK'),=CL8'UNTFILE' FREE LOCK TAB         
         GOTO1 VDATAMGR,DMCB,(0,=C'DMUNLK'),=CL8'UNTDIR'  FREE LOCK TAB         
         LA    R2,300              RESET UNLOCK COUNT                           
         B     STA40               NO-RETURN TO NETIO FOR NEXT RECORD           
         SPACE 1                                                                
STAHOOK  ST    RE,SAVEREG          SAVE RETURN POINT TO NETIO                   
         CLI   NBMODE,NBPROCUN     TEST FOR A UNIT RECORD                       
         BNE   STAHOOKX                                                         
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         OC    NUPACKST,ORMASK     OR MASK                                      
         NC    NUPACKST,ANDMASK    AND MASK                                     
         MVI   NBUPUNIT,YES        SET UPDATE FLAG                              
*  UPDATE AUDIT INFO                                                            
         CLI   AUDSW,YES                                                        
         BNE   STAHOOKX                                                         
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'09',AIOAREA2),0                    
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA2,BLOCK,0                      
*                                                                               
STAHOOKX L     RE,SAVEREG          RETURN TO NETIO                              
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DELETE A PACKAGE, FIRST CHECKING IF IT HAS ANY                 
* UNITS                                                                         
*                                                                               
DELETE   NTR1                                                                   
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,PKDELERR       SET ERROR CURSOR POSITION/MESSAGE            
         SPACE 1                                                                
DELETE2  GOTO1 CHKUN,DMCB2,NPAKDP  LOOK FOR ANY UNITS FIRST                     
         BE    ERROR               YES-SO ITS AN ERROR                          
         SPACE 1                                                                
DELETE4  XC    KEY,KEY             NOW DELETE PACKAGE RECORD                    
         LA    R4,KEY                                                           
         L     RE,APACKREC                                                      
         MVC   NPKEY,0(RE)                                                      
         GOTO1 AIO,DMCB,UPDATE+UNT+DIR+READ                                     
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,AIOAREA1                           
         OI    KEY+NDIRCTL,X'80'   TURN ON DELETE BIT                           
         L     R4,AIOAREA1         ADDRESSABILITY FOR RECORD                    
         OI    NPKRSTAT,X'80'      PUT RECORD BACK AS DELETED FOR RECV          
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         GOTO1 (RF),(R1),UNT+FILE+PUT,AIOAREA1                                  
         XC    PACKAGE,PACKAGE     CLEAR SAVED PACKAGE NUMBER                   
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR ANY UNITS ON PACKAGE                                 
* CALLED BEFORE DELETE AND FOR CHANGE IN DAYPART                                
*                                                                               
* ON ENTRY, P1=A(PACKAGE DAYPART CODE)                                          
* ON EXIT, CC=EQ FOR UNITS ON PACKAGE,  CC=NEQ FOR NO UNITS                     
*                                                                               
CHKUN    NTR1                                                                   
         L     R2,0(R1)            R2=A(DAYPART)                                
         MVI   NBSELMOD,NBPROCUN                                                
         MVC   NBSELSTR,ESTSTART                                                
         MVC   NBSELEND,ESTEND                                                  
         MVC   NBSELDP,0(R2)       SET DAYPART CODE OF PACKAGE                  
         MVI   NBUSER+13,NO        **FORCE BACK PRE-EMPTS                       
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVC   NBAIO,AIOAREA2                                                   
CHKUN2   GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD      TEST FOR MODULE ERROR                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    CHKUNNO                                                          
         CLI   NBMODE,NBPROCUN     TEST IF UNIT FOUND                           
         BE    CHKUNONE            YES-ACTION CANNOT BE DONE                    
         B     CHKUN2                                                           
*                                                                               
CHKUNNO  LTR   RB,RB               SET CC=NEQ FOR NO UNITS                      
         B     CHKUNX                                                           
*                                                                               
CHKUNONE CR    RB,RB               SET CC=EQ FOR ONE UNIT                       
*                                                                               
CHKUNX   B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CHECK FOR BILL/PAY ACTIVITY ON PACKAGE TO BE LOCKED            
*                                                                               
CHKACC   NTR1                                                                   
         L     RE,AIOAREA3         SAVE THE NETBLOCK IN IO3                     
         LA    RF,NEBLOCKL                                                      
         LR    R1,RF                                                            
         LA    R0,NEBLOCKD                                                      
         MVCL  RE,R0                                                            
*                                                                               
CHKACC1  MVI   NBSELMOD,NBPROCUN   READ THE PACKAGE'S UNITS                     
         MVC   NBSELSTR,ESTSTART                                                
         MVC   NBSELEND,ESTEND                                                  
         MVC   NBSELDP,NPAKDP                                                   
         MVI   NBUSER+13,NO        FORCE RETURN OF PRE-EMPTS                    
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVC   NBAIO,AIOAREA2                                                   
*                                                                               
CHKACC2  GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BE    CHKACC8             YES                                          
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RECORD                         
         BNE   CHKACC2                                                          
*                                                                               
         L     R3,NBAIO            R3=ELEMENT POINTER                           
         LA    R3,NUMAINEL-NUKEY(R3)                                            
         SR    R0,R0                                                            
*                                                                               
CHKACC4  CLI   0(R3),0             TEST FOR EOR                                 
         BE    CHKACC2             YES-GET NEXT RECORD                          
         CLI   0(R3),X'10'         TEST FOR BILLING ELEMENT                     
         BNE   CHKACC4A            NO                                           
         USING NUBILD,R3                                                        
         TM    NUBILST,X'20'       TEST FOR UNBILLED ELEMENT                    
         BO    CHKACC5             YES-LOOK AT NEXT ELEMENT                     
         B     CHKACC6                                                          
*                                                                               
CHKACC4A CLI   0(R3),X'12'         TEST FOR PAYING ELEMENT                      
         BE    CHKACC6                                                          
*                                                                               
CHKACC5  IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     CHKACC4                                                          
*                                                                               
CHKACC6  MVI   FERN,LOCKERR        ERROR EXIT                                   
         B     ERROR                                                            
*                                                                               
CHKACC8  LA    RE,NEBLOCKD         SUCCESSFUL CHECK EXIT-                       
         LA    RF,NEBLOCKL         RESTORE NETBLOCK                             
         LR    R1,RF                                                            
         L     R0,AIOAREA3                                                      
         MVCL  RE,R0                                                            
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DEVELOP MESSAGE AND TO SET CURSOR POSITION                     
*                                                                               
MSG      ST    RE,SAVEREG                                                       
         MVC   BUYMSG(7),=C'PACKAGE'                                            
         LA    R3,BUYMSG+8         SET R3 AS OUTPUT POINTER                     
         EDIT  (B1,PACK),(3,(R3)),ALIGN=LEFT                                    
         AR    R3,R0                                                            
         CLI   ACTION,BP           TEST FOR ACTION BUY                          
         BE    MSG2                                                             
         CLI   ACTION,DP           TEST FOR ACTION DISPLAY OR                   
         BE    *+12                                                             
         TM    MODE,DISPLAY        FORCED DISPLAY                               
         BZ    MSG2                                                             
*                                                                               
         MVC   1(9,R3),=C'DISPLAYED'                                            
         LA    R3,11(R3)                                                        
         LA    R2,BUYACTH          SET CURSOR POSITION                          
         CLI   ACTION,DP           TEST FOR ACTION DISPLAY                      
         BE    MSGX                YES-ALL DONE                                 
         LA    R2,PAKNAMEH                                                      
         MVC   0(15,R3),=C'- ENTER CHANGES'                                     
         CLI   ACTION,CP           TEST FOR ACTION CHANGE                       
         BE    MSGX                YES-DONE                                     
         OI    1(R2),X'01'         CONVERT CURSOR FIELD TO MODIFIED             
         MVC   0(18,R3),=C'- NOW YOU MAY COPY'                                  
         CLI   ACTION,COPYP                                                     
         BE    MSGX                                                             
         MVC   14(6,R3),=C'DELETE' REPLACE 'COPY' WITH 'DELETE'                 
         B     MSGX                                                             
         SPACE                                                                  
MSG2     LA    R2,BUYACTH                                                       
         MVC   1(5,R3),=C'ADDED'                                                
         CLI   ACTION,BP                                                        
         BE    MSGX                                                             
         MVC   1(6,R3),=C'COPIED'                                               
         CLI   ACTION,COPYP                                                     
         BE    MSGX                                                             
         MVC   1(7,R3),=C'CHANGED'                                              
         CLI   ACTION,CP                                                        
         BE    MSGX                                                             
         LA    R2,BUYCLIH                                                       
         MVC   1(7,R3),=C'DELETED'                                              
         B     MSGX                                                             
         SPACE                                                                  
MSGX     ST    R2,FADDR                                                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* MODULE AND SUB-ROUTINE EXIT                                                   
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* EXECUTED INSTRUCTIONS                                                         
*                                                                               
YESCOMP  CLC   FLD(0),=C'YES'                                                   
NOCOMP   CLC   FLD(0),=C'NO'                                                    
         SPACE 1                                                                
PRINCOMP CLC   FLD(0),=C'PRINT'                                                 
ZONECOMP CLC   FLD(0),=C'ZONE'                                                  
         SPACE 2                                                                
* TABLE OF DAYPART CODES AND NAMES                                              
*                                                                               
DPTTAB   DS    0CL9                                                             
         DC    C'D',CL8'DAYTIME'                                                
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
         DC    C'H',CL8'OTHER'                                                  
         DC    C'H',CL8'H'                                                      
         DC    C'J',CL8'PROMO-ID'                                               
         DC    C'J',CL8'J'                                                      
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
         DC    C'B',CL8'CBLSPORT'                                               
         DC    C'B',CL8'B'                                                      
         DC    C'Q',CL8'INTRACTV'                                               
         DC    C'Q',CL8'Q'                                                      
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
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
         DC    X'FF',AL1(UNITFIX+CKACCNT)                                       
*                                                                               
         DC    CL8'UNLOCKED',AL1(3),X'00'                                       
         DC    AL1(UNLOCKED),AL1(UNITFIX)                                       
*                                                                               
         DC    CL8'AUDIT',AL1(1),AL1(AUDITON)                                   
         DC    X'FF',AL1(UNITFIX+BLDAUDT)                                       
*                                                                               
*        DC    CL8'AUDOFF',AL1(3),X'00'                                         
*        DC    AL1(AUDITOFF),AL1(UNITFIX)                                       
*                                                                               
         DC    CL8'UPLRESET',AL1(4),X'00'                                       
         DC    X'FF',AL1(RESUPLD)                                               
*                                                                               
         DC    CL8'UPLSET',AL1(4),X'00'                                         
         DC    X'FF',AL1(SETUPLD)                                               
*                                                                               
         DC    CL8'FROZEN',AL1(1),AL1(FROZEN)                                   
         DC    X'FF',X'00'                                                      
*                                                                               
         DC    CL8'UNFROZEN',AL1(3),X'00'                                       
         DC    AL1(UNFROZEN),X'00'                                              
*                                                                               
         DC    CL8'NETINT',AL1(1),AL1(NETINT)                                   
         DC    X'FF',AL1(UNITFIX)                                               
*        DC    X'FF',AL1(UNITFIX+CKACCNT)                                       
*                                                                               
         DC    CL8'UNNETINT',AL1(3),X'00'                                       
         DC    AL1(UNNETINT),AL1(UNITFIX)                                       
*        DC    AL1(UNNETINT),AL1(UNITFIX+CKACCNT)                               
*                                                                               
STAENT   EQU   (*-STATAB)/L'STATAB                                              
UNTFILE  DC    CL8'UNTFILE'                                                     
*                                                                               
LKSTERR  DC    C'** ERROR - LOCKED AND UPLSET STATUS CANNOT COEXIST'            
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* PACKAGE SCREEN                                                                
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFDD                                                       
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
ANEWREC  DS    A                                                                
*                                                                               
OLDCNTL  DS    X                   PACKAGE CONTROL BEFORE CHANGE                
OLDSTAT  DS    X                   PACKAGE STATUS BEFORE CHANGE                 
NEWSTAT  DS    X                   PACKAGE STATUS AFTER CHANGE                  
STATSW   DS    C                   Y=UPDATE STATUS FIELD ON UNITS               
AUDSW    DS    C                   BUILD AUDIT ELEMENT IN UNITS                 
ORMASK   DS    X                                                                
ANDMASK  DS    X                                                                
*                                                                               
BLOCK    DS    CL64                                                             
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
CKACCNT  EQU   X'40'               ACCOUNT INFORMATION MUST BE CHECKED          
RESUPLD  EQU   X'20'               RESET UPLOAD BIT IN PACKAGE RECORD           
SETUPLD  EQU   X'10'               SET CABLE LOCK FOR UPLOAD                    
BLDAUDT  EQU   X'08'               BUILD AUDIT ELEMENT IN UNITS                 
FROZEN   EQU   X'80'                                                            
LOCKED   EQU   X'20'                                                            
NOPRINT  EQU   X'10'                                                            
NETINT   EQU   X'04'                                                            
AUDITON  EQU   X'02'                                                            
UNFROZEN EQU   X'FF'-X'80'                                                      
UNLOCKED EQU   X'FF'-X'20'                                                      
PRINT    EQU   X'FF'-X'10'                                                      
UNNETINT EQU   X'FF'-X'04'                                                      
AUDITOFF EQU   X'FF'-X'02'                                                      
EQUAL    EQU   C'='                                                             
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
**PAN#1  DC    CL21'003NEBUY10S  05/01/02'                                      
         END                                                                    
