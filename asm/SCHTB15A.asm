*          DATA SET SCHTB15A   AT LEVEL 016 AS OF 05/01/02                      
*          DATA SET NEBUY15    AT LEVEL 035 AS OF 10/29/99                      
*PHASE T31115A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - MULTIPLE DIS/CHA OVERLAY - T31115'         
T31115   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**MULT**,RA,RR=R2                                              
         LA    RC,2048(RA)         RC IS THIRD BASE REGISTER                    
         LA    RC,2048(RC)                                                      
         USING T31115+8192,RC                                                   
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK                                                       
         USING TEMPD,R7                                                         
         ST    R1,MYPARM                                                        
         ST    R2,MYRELO                                                        
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         SPACE                                                                  
MULT     MVI   NBDEMRAW,C'Y'       RETRIEVE RAW DEMO NUMBERS                    
         GOTO1 VCALLOV,DMCB,(X'30',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDISPLAY,DMCB       GENERAL UNIT DISPLAY                         
         GOTO1 (RF),(R1),(X'35',0),ATWA                                         
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB          GENERAL UNIT EDIT                            
         SPACE                                                                  
         XC    NBHUNOPT,NBHUNOPT                                                
         CLI   NBPOSTYP,C'S'                                                    
         BE    MULT1                                                            
         CLI   NBPOSTYP,C'H'                                                    
         BE    MULT1                                                            
         CLI   NBPOSTYP,C'N'                                                    
         BE    MULT1                                                            
         MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
MULT1    GOTO1 =A(ACTED),RR=MYRELO                                              
         BAS   RE,CONTROL                                                       
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BZ    *+8                 NO                                           
         BAS   RE,SCRGEN           YES-GENERATE A SCREEN FIRST                  
         BAS   RE,FINDSCR          SET POINTERS TO SCREEN FIELDS                
         SPACE                                                                  
MULT2    CLI   ACTION,CM           TEST FOR ACTION CHANGE                       
         BNE   MULT3                                                            
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    MULT3               YES                                          
         MVI   FERN,PAKLERR                                                     
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BO    ERROR               NO CHANGES THEN                              
         BAS   RE,CHA                                                           
         MVC   BUYMSG(17),=C'CHANGES COMPLETED'                                 
         GOTO1 VBLDRQST                                                         
         B     MULTX                                                            
         DROP  RE                                                               
         SPACE                                                                  
MULT3    BAS   RE,DIS                                                           
         MVC   BUYMSG(34),=C'DATA DISPLAYED - ENTER NEXT ACTION'                
         CLI   MORESW,YES                                                       
         BNE   *+10                                                             
         MVC   BUYMSG+36(14),=C'(MORE TO COME)'                                 
         SPACE                                                                  
MULTX    NI    MODE,X'FF'-DISPLAY                                               
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         OI    6(R2),X'81'         SEND IT BACK AS MODIFIED                     
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO SET MODE AND TO MAINTAIN SAVE AREA VALUES                      
*                                                                               
CONTROL  TM    MODE,FIRST+DISPLAY  TEST FOR FIRST TIME/BASE                     
         BZ    *+10                SCREEN FIELD CHANGE                          
         XC    SVAREA,SVAREA       YES-START SAVE AREA AGAIN                    
         CLC   KEYLIST(CONLISTL),SVKEYL TEST FOR CHANGE IN CONTROL              
         BE    *+8                 VALUES                                       
         OI    MODE,DISPLAY        FORCE DISPLAY FOR CHANGE IN CONTROLS         
         TM    MODE,DISPLAY                                                     
         BZ    *+14                                                             
         XC    SVLDATE,SVLDATE     FOR DISPLAY, CLEAR LAST DATE                 
         MVI   SVLSUB,0            ON SCREEN                                    
         MVC   SVLACT,ACTION       UPDATE SAVE AREA VALUES                      
         MVC   SVKEYL(SVVALSLN),KEYLIST                                         
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GENERATE A SCREEN                                              
*                                                                               
SCRGEN   NTR1                                                                   
         LA    RE,SVAREA           BUILD A PROTECTED LINE FIRST                 
         ST    RE,BLDTWAX                                                       
         MVI   BLDLIST+1,78                                                     
         MVI   BLDLIST+2,X'20'                                                  
         GOTO1 VBLDFLD                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(3,R2),=X'000101'  ERASE/WRITE TO CLEAR SCREEN                  
         SPACE                                                                  
SCRGEN2  LA    R3,KEYSTACK         FIND LENGTH NEEDED FOR KEY DATA              
         USING KEYTABD,R3                                                       
         ZIC   R0,KEYCOUNT                                                      
         LR    R2,R0               INITIALIZE FIELD COUNT                       
         SR    R4,R4               CLEAR DATA LENGTH                            
         SR    RF,RF                                                            
         LTR   R0,R0                                                            
         BZ    SCRGEN4                                                          
*                                                                               
SCRGEN3  IC    RF,KEYDLEN                                                       
         AR    R4,RF               UPDATE LENGTH ACCUMULATOR                    
         LA    R3,KEYENTL(R3)                                                   
         BCT   R0,SCRGEN3                                                       
*                                                                               
SCRGEN4  ICM   RF,1,ESTDEMOS       GET NUMBER OF ESTIMATED DEMOS                
         BZ    SCRGEN4A                                                         
         AR    R2,RF               UPDATE FIELD COUNT                           
         GOTO1 LENDEM,DMCB,(ESTDEMOS,ESTLIST),ESTMODS                           
         A     R4,DMCB             UPDATE DATA LENGTH                           
*                                                                               
SCRGEN4A SR    RF,RF                                                            
         ICM   RF,1,ACTDEMOS                                                    
         BZ    SCRGEN5                                                          
         AR    R2,RF                                                            
         GOTO1 LENDEM,DMCB,(ACTDEMOS,ACTLIST),ACTMODS                           
         A     R4,DMCB                                                          
         SPACE                                                                  
SCRGEN5  AR    R4,R2               ADD IN ROOM FOR COMMAS + 1 BYTE              
         XC    BLDLIST,BLDLIST                                                  
         MVC   BLDLIST(8),PROTECT  PROTECTED FIELD VALUES                       
         LA    R1,BLDLIST+8                                                     
         MVI   0(R1),1             ONE LEADING SPACE                            
         STC   R4,1(R1)                                                         
         MVI   3(R1),1                                                          
         GOTO1 VCENFLD                                                          
         BNE   SCRGEN6                                                          
         MVC   BUYMSG(L'TOOLONG),TOOLONG                                        
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
         SPACE                                                                  
SCRGEN6  SR    R3,R3                                                            
SCRGEN7  GOTO1 VBLDFLD                                                          
         BNE   SCRGENX                                                          
         LA    R3,1(R3)                                                         
         B     SCRGEN7                                                          
         SPACE                                                                  
SCRGENX  STC   R3,FIELDS                                                        
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CALCULATE REQUIRED DATA LENGTH FOR DEMOS                       
*                                                                               
* P1  BYTE 0    = COUNT OF DEMOS                                                
*     BYTES 1-3 = A(DEMO LIST)                                                  
* P2  BYTES 1-3 = A(DEMO MODIFIER TABLE)                                        
*                                                                               
LENDEM   NTR1                                                                   
         LM    R3,R4,0(R1)         R3=A(DEMO LIST), R4=A(MOD. TABLE)            
         ZIC   R2,0(R1)            R2=COUNT OF DEMOS                            
         XC    0(4,R1),0(R1)       CLEAR LENGTH REQUIRED                        
*                                                                               
LENDEM2  LR    RE,R4               POINT TO START OF MODIFIER TABLE             
         SR    R0,R0               CLEAR WORK REGISTER                          
*                                                                               
LENDEM3  CLC   3(1,R3),0(RE)       TEST MODIFIER VS. TABLE                      
         BE    LENDEM4                                                          
         LA    RE,2(RE)            NEXT TABLE ENTRY                             
         CLI   0(RE),X'FF'         TEST FOR EOT                                 
         BNE   LENDEM3             NO                                           
         DC    H'0'                DUMP IF MODIFIER NOT FOUND                   
*                                                                               
LENDEM4  IC    R0,1(RE)            GET LENGTH OF DATA                           
         AH    R0,=H'1'            ADD IN ONE FOR STAR                          
         A     R0,0(R1)            UPDATE TOTAL LENGTH                          
         ST    R0,0(R1)                                                         
         LA    R3,4(R3)            NEXT DEMO                                    
         BCT   R2,LENDEM2                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO FIND START AND END FIELDS AND ON SCREEN                        
*                                                                               
FINDSCR  SR    R0,R0                                                            
         LA    R2,BUYACTH                                                       
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,ABLKLIN                                                       
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,AFSTFLD                                                       
         SPACE                                                                  
FINDSCR2 CLI   0(R2),0             TEST FOR END OF SCREEN                       
         BE    FINDSCR4                                                         
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     FINDSCR2                                                         
         SPACE                                                                  
FINDSCR4 ST    R2,ALSTFLD                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO READ UNIT RECORDS AND TO DISPLAY DATA ON SCREEN                
*                                                                               
DIS      NTR1                                                                   
         MVC   AFLD,AFSTFLD        INITIALIZE FIELD POINTER                     
         GOTO1 VCLEARF,DMCB,(1,ABLKLIN),ALSTFLD                                 
         GOTO1 (RF),(R1),AFSTFLD,ALSTFLD                                        
         SPACE                                                                  
DIS2     L     RE,APACKREC                                                      
         MVC   NBSELDP,NPAKDP-NPRECD(RE)                                        
         MVC   NBSELPRG,PROG                                                    
         OC    FILSTART,FILSTART                                                
         BZ    *+16                                                             
         MVC   NBSELSTR,FILSTART                                                
         MVC   NBSELEND,FILEND                                                  
         OC    SVLDATE,SVLDATE     TEST IF CONTINUING A DISPLAY                 
         BZ    DIS3                NO                                           
         GOTO1 VDATCON,DMCB,(2,SVLDATE),NBSELSTR                                
         OC    NBSELEND,NBSELEND   IF NO END DATE IS THERE, PLUG IN             
         BNZ   DIS3                THE ESTIMATE END DATE                        
         MVC   NBSELEND,ESTEND                                                  
         SR    R0,R0               USE XX DAYS PAST END FOR MG'S                
         ICM   R0,1,BUYPROF+14                                                  
         BZ    DIS3                                                             
         GOTO1 VADDAY,(R1),ESTEND,NBSELEND,(R0)                                 
         SPACE                                                                  
DIS3     CLI   FILPRN,0                                                         
         BE    *+10                                                             
         MVC   NBSELPNM,FILPRN                                                  
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'Q'                                                       
         MVI   NBUSER+13,NO        ** FORCE BACK PRE-EMPTS                      
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBESTOPT,C'M'                                                    
         OI    NBSPLOPT,X'10'      DONT BREAK OUT COPY SPLIT NUMBERS            
         MVC   NBAIO,AIOAREA1                                                   
         SPACE                                                                  
* READ UNIT RECORDS THROUGH NETIO                                               
*                                                                               
*        BAS   RE,SAVGUAR                                                       
DIS4     GOTO1 VNETIO,DMCB,NEBLOCKD                                             
*        BAS   RE,RESGUAR                                                       
         CLI   NBERROR,0                                                        
         BE    DIS4A                                                            
         CLI   NBERROR,7                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MORESW,NO                                                        
         B     EXXMOD                                                           
*                                                                               
DIS4A    CLI   NBMODE,NBPROCUN                                                  
         BE    DIS5                                                             
         CLI   NBMODE,NBREQLST     TEST FOR EOF                                 
         BNE   DIS4                NO                                           
         CLI   RECORDS,0           YES-TEST FOR ANY RECORDS DISPLYED            
         BNE   DISX                YES                                          
         MVC   BUYMSG(L'NOMORE),NOMORE                                          
         XC    SVAREA,SVAREA       CLEAR SAVE AREA AND GO RIGHT BACK            
         MVI   MODE,0              TO USER                                      
         OI    BUYACTH+6,X'81'     MODIFY NEXT TIME FOR RE-ENTRY                
         GOTO1 VEXIT                                                            
         SPACE                                                                  
DIS5     OC    SVLDATE,SVLDATE     TEST IF RESUMING DISPLAY                     
         BZ    DIS6                NO                                           
         CLC   NBACTDAT,SVLDATE    TEST IF PAST LAST DATE                       
         BH    DIS6                YES                                          
         CLC   NBACTSUB,SVLSUB     SAME DATE-TEST IF PAST SUB-LINE              
         BNH   DIS4                NO-READ ANOTHER RECORD                       
         SPACE 1                                                                
* APPLY FILTERS                                                                 
*                                                                               
DIS6     CLI   FILDAY,0            DAY FILTER                                   
         BE    *+14                                                             
         CLC   FILDAY,NBDAY                                                     
         BNE   DIS4                                                             
         CLI   FILLEN,0            LENGTH FILTER                                
         BE    DIS6A                                                            
         ICM   RE,1,NBLEN1                                                      
         BZ    *+18                                                             
         CLC   FILLEN,NBLEN1                                                    
         BNE   DIS4                                                             
         B     DIS6A                                                            
         CLC   FILLEN,NBLEN                                                     
         BNE   DIS4                                                             
DIS6A    CLI   FILLEN2,0           LENGTH FILTER                                
         BE    DIS6B                                                            
         ZIC   RE,NBLEN                                                         
         ZIC   RF,NBLEN1                                                        
         SR    RE,RF                                                            
         ZIC   RF,FILLEN2                                                       
         CR    RE,RF                                                            
         BNE   DIS4                                                             
DIS6B    CLI   FILPRN,0            TEST FOR BRAND FILTER                        
         BE    DIS7                NO                                           
         CLC   FILPRN,NBPRD        YES-COMPARE AGAINST BOTH PRODUCTS            
         BE    DIS7                                                             
         CLC   FILPRN,NBPRD2                                                    
         BNE   DIS4                                                             
         SPACE 1                                                                
DIS7     OC    FILACT,FILACT       ACTIVITY DATE FILTER                         
         BZ    DIS8                                                             
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',NBAIO),0                       
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,12(R1)                                                        
         USING NUACTD,R3                                                        
         CLC   NUACTCDT,FILACT                                                  
         BNE   DIS4                                                             
         DROP  R3                                                               
         SPACE                                                                  
DIS8     L     R2,AFLD                                                          
         C     R2,ALSTFLD                                                       
         BNL   DISX                                                             
         ZIC   R1,RECORDS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,RECORDS                                                       
         SPACE                                                                  
DIS10    GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         CLI   FERN,0                                                           
         BNE   DIS11                                                            
         GOTO1 VDISPROG                                                         
*                                                                               
DIS11    CLI   ESTDEMOS,0          TEST FOR ANY ESTIMATED DEMOS                 
         BE    DIS11A              NONE                                         
         MVI   DEMTYPE,C'E'                                                     
         GOTO1 LOOKDEM,DMCB,(ESTDEMOS,ESTLIST),ESTVAL                           
*                                                                               
DIS11A   CLI   ACTDEMOS,0                                                       
         BE    DIS12                                                            
         MVI   DEMTYPE,C'A'                                                     
         GOTO1 LOOKDEM,DMCB,(ACTDEMOS,ACTLIST),ACTVALS                          
         SPACE                                                                  
DIS12    BAS   RE,PROTDIS                                                       
         BAS   RE,FLDOUT                                                        
         MVC   SVLDATE,NBACTDAT                                                 
         MVC   SVLSUB,NBACTSUB                                                  
         B     DIS4                                                             
         SPACE 1                                                                
DISX     MVI   MORESW,NO                                                        
         CLI   NBMODE,NBPROCUN     TEST IF A RECORD DID NOT FIT                 
         BNE   *+8                 ON THIS SCREEN                               
         MVI   MORESW,YES          YES                                          
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO LOOK UP DEMOS BY MEANS OF NETVAL                               
*                                                                               
* AT ENTRY    P1   BYTE   0  = NUMBER OF DEMOS                                  
*                  BYTES 1-3 = A(DEMO LIST)                                     
*             P2   BYTES 1-3 = A(OUTPUT AREA)                                   
* DEMTYPE SET TO 'E'=ESTIMATED OR 'A'=ACTUAL                                    
*                                                                               
LOOKDMU  NTR1                                                                   
         L     R2,AIOAREA3         LOOK UP UNIVERSE ALSO                        
         LA    R2,NDBLKLEN(R2)                                                  
         B     LOOK1                                                            
*                                                                               
LOOKDEM  NTR1                                                                   
         SR    R2,R2                                                            
LOOK1    MVC   NBADEM,AIOAREA3                                                  
         L     R4,NBADEM           POINT R4 AT DEMO OUTPUT BLOCK                
         USING NETDEMOD,R4                                                      
         ST    R2,NDAUBLOK         R2 = ADDRESS OF UNIVERSE LOOK UP             
         LM    R2,R3,0(R1)                                                      
         CLI   DEMTYPE,C'A'        TEST FOR ACTUAL LOOKUP                       
         BNE   *+8                                                              
         MVI   NBACTOPT,YES        SET NETVALUE ACTUAL OPTION                   
         XC    NDDEMOS,NDDEMOS                                                  
         MVC   NDUSRNMS,ESTUSNS    SET USER DEMO NAME LIST                      
         MVC   USERNMS,ESTUSNS     KEEP ADDRESSABLE NAME LIST                   
         LR    R1,R2                                                            
         SRL   R1,24               ISOLATE DEMO COUNT                           
         LR    R5,R1               SAVE DEMO COUNT                              
*                                                                               
         LA    RE,NDDEMOS          RE=A(NETBLOCK DEMO LIST)                     
         LR    RF,R2               RF=A(INTERNAL DEMO LIST)                     
         MVC   0(3,RE),0(RF)       EXTRACT 3-BYTE DEMO CODE                     
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)            NEXT 4-BYTE CODE                             
         BCT   R1,*-14                                                          
*                                                                               
*        BAS   RE,SAVGUAR                                                       
         GOTO1 VNETVAL,DMCB,NETBLOCK                                            
*        BAS   RE,RESGUAR                                                       
         ICM   RF,15,NDAUBLOK      LOOKING UP DEMOS OR UNIVERSES                
         BNZ   LOOK4               B - UNIVERSES                                
         LA    RE,NDESTDEM         POINT TO START OF OUTPUT                     
         CLI   DEMTYPE,C'E'        TEST FOR ESTIMATED CALL                      
         BE    *+8                 YES                                          
         LA    RE,NDACTDEM         NO-POINT TO RETURNED ACTUALS                 
         LR    R1,R2               SAVE LIST OF DEMOS                           
         SPACE                                                                  
LOOK2    LH    R0,0(RE)            GET VPH                                      
         CLI   3(R1),C'V'                                                       
         BE    LOOK3                                                            
         LH    R0,2(RE)            ELSE GET RATING                              
         CLI   3(R1),C'R'          TEST FOR RATING                              
         BE    LOOK3               YES                                          
         L     R0,4(RE)            GET THE IMPRESSION                           
         SPACE 1                                                                
LOOK3    ST    R0,0(R3)                                                         
         LA    R1,4(R1)            NEXT DEMO                                    
         LA    R3,4(R3)            NEXT OUTPUT POSITION                         
         LA    RE,8(RE)            NEXT DEMO BLOCK POSITION                     
         BCT   R5,LOOK2                                                         
         B     LOOK10                                                           
         SPACE                                                                  
         USING NETUNIVD,RF                                                      
LOOK4    CLI   DEMTYPE,C'A'        TEST FOR ACTUAL LOOKUP                       
         BNE   LOOK5                                                            
         LA    R3,ACTUNIV                                                       
         MVC   0(32,R3),NDUAHOME                                                
         B     LOOK6                                                            
*                                                                               
LOOK5    LA    R3,ESTUNIV                                                       
         MVC   0(32,R3),NDUEHOME                                                
         DROP  RF                                                               
*                                                                               
LOOK6    CLI   1(R2),USERMOD       TEST FOR USER DEMO                           
         BNE   LOOK8                                                            
         ZIC   RF,2(R2)            GET USER DEMO NUMBER                         
         SLL   RF,2                                                             
         L     R0,NDUSRUNV-4(RF)   GET UNIVERSE VALUE                           
         ST    R0,0(R3)                                                         
*                                                                               
LOOK8    LA    R2,4(R2)            NEXT DEMO                                    
         LA    R3,4(R3)            NEXT UNIVERSE POSITION                       
         BCT   R5,LOOK6                                                         
*                                                                               
LOOK10   MVI   NBACTOPT,NO                                                      
         MVI   NBUPUNIT,0          ZERO UPDATE UNIT SWITCH                      
         SPACE                                                                  
LOOKX    B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY THE UNIT DATE AND SUB-LINE NUMBER                      
*                                                                               
PROTDIS  ST    RE,SAVEREG                                                       
         L     R2,AFLD                                                          
         GOTO1 VDATCON,DMCB,(2,NBACTDAT),(4,8(R2))                              
         CLI   NBACTSUB,1          TEST FOR SUB-LINE=1                          
         BE    PROTDISX                                                         
         MVI   13(R2),DASH                                                      
         ZIC   R0,NBACTSUB                                                      
         EDIT  (R0),(3,14(R2)),ALIGN=LEFT                                       
         SPACE                                                                  
PROTDISX TM    NBUNITST,X'02'      TEST FOR MISSING                             
         BZ    *+8                 NO                                           
         MVI   17(R2),C'M'                                                      
         TM    NBUNITST,X'40'      TEST FOR PRE-EMPT                            
         BZ    *+8                                                              
         MVI   17(R2),C'P'                                                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,AFLD             UPDATE SCREEN POINTER                        
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY KEYWORD AND DEMO DATA IN UNPROTECTED FIELD             
*                                                                               
FLDOUT   NTR1                                                                   
         L     R2,AFLD                                                          
         LA    R4,8(R2)            POINT TO OUTPUT                              
         ST    R4,AOUT             SAVE START POINT                             
         SR    R0,R0                                                            
         ICM   R0,1,KEYCOUNT                                                    
         BZ    FLD10               NO KEYS                                      
         LA    R3,KEYSTACK         POINT TO KEYS FOR OUTPUT                     
         USING KEYTABD,R3                                                       
*                                                                               
* KEYWORD DATA DISPLAY                                                          
*                                                                               
FLD2     TM    KEYCTL,INTERNAL     TEST FOR DELETE                              
         BO    FLD6                                                             
         OC    KEYDIS,KEYDIS       TEST FOR SPECIAL ROUTINE                     
         BNZ   FLD3                YES                                          
         MVC   BYTE,KEYDATA        DATA TYPE                                    
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         B     FLD4                                                             
         SPACE                                                                  
FLD3     SR    RF,RF                                                            
         ICM   RF,7,KEYDIS                                                      
         GOTO1 (RF),RR=MYRELO                                                   
         SPACE                                                                  
FLD4     CLM   R0,1,KEYCOUNT       TEST FOR FIRST KEY                           
         BE    *+12                YES                                          
         MVI   0(R4),COMMA         NO-PUT A COMMA IN FRONT OF DATA              
         LA    R4,1(R4)                                                         
         CLI   FLDH+7,0            TEST FOR DATA TO DISPLAY                     
         BE    FLD6                NO                                           
         ZIC   R1,FLDH+7                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),FLD                                                      
         LA    R4,1(R1,R4)         UPDATE OUTPUT POINTER                        
         SPACE                                                                  
FLD6     LA    R3,KEYENTL(R3)                                                   
         BCT   R0,FLD2                                                          
*                                                                               
* DISPLAY ESTIMATED AND ACTUAL DEMOS                                            
*                                                                               
FLD10    MVI   BYTE,NO             SET ACTUAL DEMO SWITCH                       
         SR    R5,R5               R5 IS A COUNTER                              
         ICM   R5,1,ESTDEMOS                                                    
         BZ    FLD20                                                            
         LA    R2,ESTLIST          R2 POINTS TO DEMO LIST                       
         LA    R3,ESTVAL           R3 POINTS TO DEMO VALUES                     
         MVI   DEMOEL,X'DD'        SET ESTIMATED DEMO OVERRIDE EL CODE          
         MVI   USEREL,X'CD'                                                     
         SPACE                                                                  
FLD12    L     R0,0(R3)                                                         
         C     R4,AOUT             TEST FOR START OF FIELD                      
         BE    *+12                                                             
         MVI   0(R4),COMMA                                                      
         LA    R4,1(R4)                                                         
*                                                                               
         MVC   ELCODE,DEMOEL       SET DEMO OVERRIDE ELEMENT                    
         MVI   LFIL,5                                                           
         XC    DUB,DUB                                                          
         MVC   DUB+1(3),0(R2)      FILTER ON MODIFIER/CATEGORY                  
         CLI   1(R2),USERMOD                                                    
         BNE   FLD13                                                            
         MVC   ELCODE,USEREL                                                    
         MVI   LFIL,8                                                           
         MVC   DUB(1),3(R2)                                                     
         ZIC   R1,2(R2)            USER DEMO NUMBER                             
         BCTR  R1,0                                                             
         MH    R1,=H'7'                                                         
         LA    R1,USERNMS(R1)                                                   
         MVC   DUB+1(7),0(R1)      EXTRACT USER DEMO NAME                       
*                                                                               
FLD13    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(ELCODE,NBAIO),(LFIL,DUB)             
         CLI   12(R1),0                                                         
         BNE   *+16                                                             
         L     RF,12(R1)           ADDRESS OF ELEMENT                           
         MVI   0(R4),STAR                                                       
         LA    R4,1(R4)                                                         
         CLI   3(R2),C'R'          TEST FOR RATING                              
         BE    FLD14                                                            
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+12                                                             
         CLI   3(R2),C'T'          TEST FOR HUNDREDS IMP                        
         BE    FLD14B                                                           
         EDIT  (R0),(5,(R4)),ALIGN=LEFT,ZERO=NOBLANK                            
         B     FLD15                                                            
         SPACE                                                                  
FLD14    TM    DEMPREC+1,X'82'     CHECK 2 DECIMAL PLACES FOR RATING            
         BO    FLD14A                                                           
         EDIT  (R0),(4,(R4)),1,ALIGN=LEFT,ZERO=NOBLANK                          
         B     FLD15                                                            
         SPACE                                                                  
FLD14A   EDIT  (R0),(5,(R4)),2,ALIGN=LEFT,ZERO=NOBLANK                          
         B     FLD15                                                            
         SPACE                                                                  
FLD14B   EDIT  (R0),(7,(R4)),1,ALIGN=LEFT,ZERO=NOBLANK                          
         SPACE                                                                  
FLD15    AR    R4,R0               UPDATE OUTPUT POINTER                        
         LA    R2,4(R2)            NEXT DEMO                                    
         LA    R3,4(R3)            NEXT VALUE                                   
         BCT   R5,FLD12                                                         
         SPACE 1                                                                
FLD20    CLI   BYTE,YES            TEST IF ACTUALS DONE                         
         BE    FLDX                YES                                          
         ICM   R5,1,ACTDEMOS                                                    
         BZ    FLDX                NO ACTUALS                                   
         LA    R2,ACTLIST                                                       
         LA    R3,ACTVALS                                                       
         MVI   DEMOEL,X'DE'        ACTUAL OVERRIDE EL CODE                      
         MVI   USEREL,X'CE'                                                     
         MVI   BYTE,YES                                                         
         B     FLD12               GO BACK AND OUTPUT ACTUALS                   
         SPACE                                                                  
FLDX     L     R2,AFLD                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,AFLD             UPDATE SCREEN POINTER                        
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* OVERRIDE DISPLAY ROUTINES                                                     
*                                                                               
DPREMPT  MVI   FLDH+7,1                                                         
         MVI   FLD,C'N'                                                         
         TM    NBUNITST,X'40'                                                   
         BZ    *+8                                                              
         MVI   FLD,C'Y'                                                         
         BR    RE                                                               
         EJECT                                                                  
* DISPLAY DELETE HOMES IMPRESSION DEFAULT                                       
*                                                                               
DDHI     ST    RE,SAVEREG                                                       
         MVI   FLDH+7,1                                                         
         MVI   FLD,C'Y'                                                         
         MVI   ELCODE,X'DD'                                                     
         MVI   LFIL,4                                                           
         MVC   DUB(4),=XL4'0000E301' T1                                         
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(ELCODE,AIOAREA1),(LFIL,DUB)          
         CLI   12(R1),0            TEST IF HOME IMP ELEMENT FOUND               
         BE    DDHIN               YES                                          
         MVC   DUB(4),=XL4'0000C801' H1                                         
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(ELCODE,AIOAREA1),(LFIL,DUB)          
         CLI   12(R1),0            TEST IF HOME IMP ELEMENT FOUND               
         BNE   DDHIX               NO                                           
DDHIN    MVI   FLD,C'N'                                                         
DDHIX    L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* OVERRIDE DISPLAY ROUTINES                                                     
*                                                                               
DDATE    ST    RE,SAVEREG                                                       
         GOTO1 VDATCON,DMCB,(2,NBACTDAT),(4,FLD)                                
         MVI   FLDH+7,5                                                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* OVERRIDE DISPLAY ROUTINES                                                     
*                                                                               
*--DISPLAY PRODUCT                                                              
DPROD    NTR1                                                                   
*--CHECK TO SEE IF MORE THEN 2 PRODUCTS                                         
         MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BNE   DPRD200                                                          
*--MORE THEN 2 PRODUCT DISPLAY                                                  
         L     R4,12(R1)           PRODUCT ELEMENT                              
         USING NUPRDD,R4                                                        
*-GET NUMBER OF PRODUCTS                                                        
         SR    RE,RE                                                            
         ZIC   RF,NUPRDLEN                                                      
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
*                                                                               
         MVC   HUTBLK(100),SPACES                                               
         LA    R2,NUPRDPR          FIRST PRODUCT                                
         LA    R3,HUTBLK           OUTPUT                                       
         LR    R4,RF               PRODUCT COUNT                                
         DROP  R4                                                               
*                                                                               
DPRD100  MVC   NBPRD,0(R2)                                                      
         MVI   BYTE,UPRD                                                        
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         ZIC   R1,FLDH+7           LENGTH OF OUTPUT                             
         EX    R1,FLDMOVE                                                       
         AR    R3,R1                                                            
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         LA    R2,6(R2)            GET NEXT PRODUCT                             
*                                                                               
         BCT   R4,DPRD100                                                       
         SH    R3,=H'1'            GET TO PREVIOUS FIELD                        
         MVI   0(R3),X'40'         NO DASH FOR LAST PRODUCT                     
         MVI   NBPRD,0                                                          
         MVC   FLD,HUTBLK          MOVE DATA TO OUTPUT                          
*--GET OUTPUT LENGTH                                                            
         LA    RE,HUTBLK                                                        
         LA    RF,24                                                            
         SR    R1,R1                                                            
DPRD160  CLI   0(RE),X'40'                                                      
         BE    DPRD180                                                          
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,DPRD160                                                       
         DC    H'0'                                                             
*                                                                               
DPRD180  STC   R1,FLDH+7           STORE OUTPUT LENGTH                          
         B     DPRDEX                                                           
*                                                                               
*--REGULAR UNIT                                                                 
DPRD200  MVI   BYTE,UPRD                                                        
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
*                                                                               
DPRDEX   B     EXXMOD                                                           
         EJECT                                                                  
         USING KEYTABD,R3                                                       
* OVERRIDE DISPLAY ROUTINES                                                     
*                                                                               
*--DISPLAY FEED PERCENT OR 1ST PRODUCT PERCENT                                  
DFEDSHR  NTR1                                                                   
*                                                                               
         USING KEYTABD,R3                                                       
         MVC   HALF(1),KEYDATA     MOVE DATA TYPE INTO HALF                     
         DROP  R3                                                               
*                                                                               
*--CHECK TO SEE IF MORE THEN 2 PRODUCTS                                         
         MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BNE   DFDSH300                                                         
*                                                                               
         L     R4,12(R1)           PRODUCT ELEMENT                              
         USING NUPRDD,R4                                                        
*-GET NUMBER OF PRODUCTS                                                        
         SR    RE,RE                                                            
         ZIC   RF,NUPRDLEN                                                      
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
*                                                                               
         MVC   HUTBLK(100),SPACES                                               
         LA    R2,NUPRDPR          FIRST PRODUCT                                
         LA    R3,HUTBLK           OUTPUT                                       
         LR    R4,RF               PRODUCT COUNT                                
         DROP  R4                                                               
*                                                                               
DFDSH100 MVC   NBPRD,0(R2)                                                      
         MVC   NBP1SHR,2(R2)       1ST PRODUCT PCT                              
         MVC   NBFEED,4(R2)        FEED PCT                                     
         MVI   BYTE,UPRD                                                        
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         ZIC   R1,FLDH+7           LENGTH OF OUTPUT                             
         EX    R1,FLDMOVE                                                       
         AR    R3,R1                                                            
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         MVC   FULL(1),NBPRD                                                    
         GOTO1 VDISPLAY,DMCB,(HALF,(R9))                                        
         ZIC   R1,FLDH+7           LENGTH OF OUTPUT                             
         EX    R1,FLDMOVE                                                       
         AR    R3,R1                                                            
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R2,6(R2)            GET NEXT PRODUCT                             
*                                                                               
         BCT   R4,DFDSH100                                                      
         SH    R3,=H'1'            GET TO PREVIOUS FIELD                        
         MVI   0(R3),X'40'         NO DASH FOR LAST PRODUCT                     
         MVC   FLD,HUTBLK          MOVE DATA TO OUTPUT                          
*                                                                               
         MVI   NBPRD,0             CLEAR PRODUCT                                
         XC    NBP1SHR,NBP1SHR     CLEAR 1ST PRODUCT PCT                        
         XC    NBFEED,NBFEED       CLEAR FEED PCT                               
*--GET OUTPUT LENGTH                                                            
         LA    RE,HUTBLK                                                        
         LA    RF,59                                                            
         SR    R1,R1                                                            
DFDSH200 CLI   0(RE),X'40'                                                      
         BE    DFDSH320                                                         
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,DFDSH200                                                      
         DC    H'0'                CHECK OUTPUT                                 
DFDSH320 STC   R1,FLDH+7                                                        
         B     DFDSHEX                                                          
*                                                                               
*--REGULAR UNIT                                                                 
DFDSH300 GOTO1 VDISPLAY,DMCB,(HALF,(R9))                                        
*                                                                               
DFDSHEX  B     EXXMOD                                                           
         EJECT                                                                  
* OVERRIDE DISPLAY ROUTINES                                                     
*                                                                               
*--DISPLAY TRAFFIC FEED CODES                                                   
DFEEDC   NTR1                                                                   
         TM    NBUNST3,X'40'       IS BUY A COPY-SPLIT                          
         BZ    DFEDC400                                                         
*--CHECK TO SEE IF MORE THEN 2 PRODUCTS                                         
         MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BE    DFEDC200                                                         
*--RETRIEVE NATIONAL PRODUCT FROM 21 ELEMENT                                    
         USING NUCMLEL,R4                                                       
         MVI   BYTE,X'21'                                                       
         BAS   RE,GETEL                                                         
         BNE   DFEDC050                                                         
         L     R4,12(R1)                                                        
         MVC   FULL+1(1),NUCMLPRD                                               
         DROP  R4                                                               
*                                                                               
DFEDC050 MVI   BYTE,UPRD                                                        
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         MVC   FLDH+5(1),FLDH+7                                                 
         GOTO1 VSCANNER,DMCB,FLDH,(2,HUTBLK),C',=* '                            
         L     R2,4(R1)                                                         
         XC    FULL,FULL                                                        
         MVC   FULL(1),NBPRD                                                    
         MVI   FULL+1,1                                                         
         MVI   BYTE,UFEEDCD                                                     
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         MVC   22(10,R2),FLD                                                    
         MVC   1(1,R2),FLDH+7                                                   
*                                                                               
         MVC   FULL(1),NBPRD2                                                   
         MVI   FULL+1,2                                                         
         MVI   BYTE,UFEEDCD                                                     
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         MVC   54(10,R2),FLD                                                    
         MVC   33(1,R2),FLDH+7                                                  
*--BUILD OUTPUT LINE                                                            
         LA    R3,FLD                                                           
         LA    R4,2                                                             
*--MOVE FIRST PRODUCT                                                           
DFEDC080 MVC   0(4,R3),12(R2)                                                   
         BAS   RE,FINDBLNK                                                      
         CLC   22(10,R2),SPACES                                                 
         BE    DFEDC090                                                         
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         MVC   0(10,R3),22(R2)                                                  
         BAS   RE,FINDBLNK                                                      
DFEDC090 MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R2,32(R2)                                                        
         BCT   R4,DFEDC080                                                      
         BCTR  R3,0                                                             
         MVI   0(R3),C' '          REMOVE LAST DASH                             
*--GET OUTPUT LENGTH                                                            
         LA    R2,HUTBLK           START OF SCANNER BLOCK                       
         SR    RF,RF                                                            
         ZIC   RE,0(R2)                                                         
         AR    RF,RE                                                            
         ZIC   RE,1(R2)                                                         
         AR    RF,RE                                                            
         ZIC   RE,32(R2)                                                        
         AR    RF,RE                                                            
         ZIC   RE,33(R2)                                                        
         AR    RF,RE                                                            
         LA    RF,3(RF)            FOR DELIMITERS                               
         STC   RF,FLDH+7                                                        
         B     DFEDCEX             EXIT                                         
*                                                                               
DFEDC200 L     R4,12(R1)           PRODUCT ELEMENT                              
         USING NUPRDD,R4                                                        
*-GET NUMBER OF PRODUCTS                                                        
         SR    RE,RE                                                            
         ZIC   RF,NUPRDLEN                                                      
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
*                                                                               
         MVC   HUTBLK(100),SPACES                                               
         LA    R2,NUPRDPR          FIRST PRODUCT                                
         LA    R3,HUTBLK           OUTPUT                                       
         LR    R4,RF               PRODUCT COUNT                                
         MVI   FULL+1,1            FIRST PRODUCT                                
         DROP  R4                                                               
*                                                                               
DFEDC250 MVC   NBPRD,0(R2)                                                      
         MVI   BYTE,UPRD                                                        
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         ZIC   R1,FLDH+7           LENGTH OF OUTPUT                             
         EX    R1,FLDMOVE                                                       
         AR    R3,R1                                                            
*                                                                               
         MVC   FULL(1),NBPRD                                                    
         MVI   BYTE,UFEEDCD                                                     
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         CLI   FLDH+7,0                                                         
         BE    DFEDC270                                                         
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         ZIC   R1,FLDH+7           LENGTH OF OUTPUT                             
         EX    R1,FLDMOVE                                                       
         AR    R3,R1                                                            
DFEDC270 MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R2,6(R2)            GET NEXT PRODUCT                             
*                                                                               
         ZIC   R1,FULL+1                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,1,FULL+1         BUMP TO NEXT PRODUCT                         
         BCT   R4,DFEDC250                                                      
         SH    R3,=H'1'            GET TO PREVIOUS FIELD                        
         MVI   0(R3),X'40'         NO DASH FOR LAST PRODUCT                     
         MVI   NBPRD,0                                                          
         MVC   FLD,HUTBLK          MOVE DATA TO OUTPUT                          
*--GET OUTPUT LENGTH                                                            
         LA    RE,HUTBLK                                                        
         LA    RF,54                                                            
         SR    R1,R1                                                            
DFEDC300 CLI   0(RE),X'40'                                                      
         BE    DFEDC320                                                         
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   RF,DFEDC300                                                      
         DC    H'0'                CHECK OUTPUT                                 
DFEDC320 STC   R1,FLDH+7                                                        
         B     DFEDCEX                                                          
*                                                                               
*--REGULAR UNIT                                                                 
DFEDC400 MVI   BYTE,UFEEDCD                                                     
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
*                                                                               
DFEDCEX  B     EXXMOD                                                           
FLDMOVE  MVC   0(0,R3),FLD                                                      
*                                                                               
*--FINDBLNK FIND FIRST SPACE IN A SCREEN LINE                                   
*--R3 POINTS TO OUTPUT LINE                                                     
FINDBLNK LA    RF,40                                                            
*                                                                               
FNDBLK20 CLI   0(R3),X'40'                                                      
         BE    FNDBLKEX                                                         
         LA    R3,1(R3)                                                         
         BCT   RF,FNDBLK20                                                      
         DC    H'0'                                                             
*                                                                               
FNDBLKEX BR    RE                                                               
         EJECT                                                                  
*                                                                               
*--DISPLAY STATION TYPE                                                         
DSTYPE   NTR1                                                                   
         MVC   FLD(1),NBSTATYP     STATION TYPE                                 
         MVC   FLD+1(1),NBPOSTYP   POSTING TYPE                                 
         MVI   FLDH+7,2                                                         
         B     EXXMOD                                                           
*                                                                               
*--DISPLAY MIRROR CODE                                                          
DMIRROR  NTR1                                                                   
         MVC   FLD(1),NBMIRTYP     MIRROR CODE                                  
         MVI   FLDH+7,1                                                         
         B     EXXMOD                                                           
*                                                                               
*--DISPLAY NAD DEFINITION CODE                                                  
DNAD     NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'62',NBAIO),0                       
         CLI   12(R1),0            TEST IF NAD ELEMENT FOUND                    
         BNE   EXXMOD              NO                                           
         L     RF,12(R1)                                                        
         MVC   FLD(6),2(RF)                                                     
         MVI   FLDH+7,6                                                         
         B     EXXMOD                                                           
*                                                                               
*--DISPLAY AUDIT GROUP CODE                                                     
DAUDGRP  NTR1                                                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'09',NBAIO),0                       
         CLI   12(R1),0            TEST IF AUDIT GROUP ELEM FOUND               
         BNE   EXXMOD              NO                                           
         L     RF,12(R1)                                                        
         MVC   FLD(4),2(RF)                                                     
         MVI   FLDH+7,4                                                         
         B     EXXMOD                                                           
*                                                                               
* SUB-ROUTINE TO EDIT DATA FIELDS AND TO CHANGE UNIT RECORDS                    
*                                                                               
CHA      NTR1                                                                   
         L     R2,AFSTFLD          GET FIRST PROTECTED FIELD ADCON              
         L     RE,APACKREC         FILL IN REMAINING NETBLK VALUES              
         MVC   NBSELDP,NPAKDP-NPRECD(RE)                                        
         MVC   NBSELPRG,PROG                                                    
         SPACE                                                                  
CHA2     C     R2,ALSTFLD          TEST FOR END OF SCREEN                       
         BNL   CHAX                                                             
         GOTO1 VGETFLD             EXTRACT FIELS DATA                           
         CLI   FLDH+5,0            TEST FOR EMPTY FIELD                         
         BE    CHAX                YES-NO MORE RECORDS TO CHANGE                
*                                                                               
         GOTO1 VDATVAL,DMCB,(1,FLD),DUB                                         
         MVI   SUB,1               DEFAULT SUB-LINE IS 1                        
         CLI   FLD+5,DASH          TEST FOR SUB-LINE                            
         BNE   CHA3                JUST DATE                                    
         LA    RE,FLD+8            POINT TO LAST DIGIT POSITION                 
         LA    R1,3                COUNTER                                      
         CLI   0(RE),C' '          TEST FOR NON-SPACE CHARACTER                 
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB2,FLD+6(0)       PACK SUB-LINE NUMBER                         
         CVB   R0,DUB2                                                          
         STC   R0,SUB                                                           
         SPACE                                                                  
CHA3     MVC   DUB(2),ESTSTART     FIND YEAR FOR DATE                           
         CLC   ESTSTART(2),ESTEND                                               
         BE    CHA4                                                             
         CLC   DUB+2(4),ESTSTART+2                                              
         BNL   *+10                                                             
         MVC   DUB(2),ESTEND                                                    
         SPACE                                                                  
CHA4     ST    R2,APROTFLD         SAVE PROTECTED FIELD ADDRESS                 
         MVC   UNITDATE,DUB        SAVE UNIT DATE                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               BUMP TO UNPROTECTED FIELD                    
         ST    R2,AUNPRFLD         SAVE ITS ADDRESS                             
         CLI   8(R2),PLUS          TEST FOR EDIT SUPPRESSION CHAR               
         BE    CHA30                                                            
         CLI   GLOBSW,YES          TEST FOR GLOBAL VALUE INPUT                  
         BE    *+12                YES                                          
         TM    4(R2),X'80'         TEST IF FIELD SENT THIS TIME                 
         BZ    CHA30               NO-SKIP EDIT                                 
*                                                                               
         BAS   RE,GETUNIT                                                       
         BNE   CHA30               DID NOT FIND IT                              
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         CLI   FERN,0              TEST FOR ERROR                               
         BNE   CHA30               BIG TROUBLE-SKIP EDIT                        
         GOTO1 VDISPROG                                                         
         L     R1,NBAIO                                                         
         MVC   OLDKEY,0(R1)        SAVE KEY BEFORE EDIT                         
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,NBAIO                                                     
         MVC   UNALOCAL,AIOAREA4                                                
         MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
         L     RE,UNALOCAL                                                      
         LA    RF,PAGELEN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR LOCAL STORAGE FOR EDIT                 
         GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
*                                                                               
         CLI   ESTDEMOS,0          TEST IF DEMOS PRESENT                        
         BE    CHA5                NO                                           
         MVI   DEMTYPE,C'E'                                                     
         GOTO1 LOOKDEM,DMCB,(ESTDEMOS,ESTLIST),ESTVAL                           
         SPACE                                                                  
CHA5     CLI   ACTDEMOS,0                                                       
         BE    CHA6                                                             
         MVI   DEMTYPE,C'A'                                                     
         GOTO1 LOOKDEM,DMCB,(ACTDEMOS,ACTLIST),ACTVALS                          
         SPACE 1                                                                
CHA6     MVC   FADDR,AUNPRFLD      SET FIELD HEADER POINTER                     
         XC    FLAST,FLAST         EDIT FROM START OF FIELD                     
         CLI   KEYCOUNT,0          TEST FOR ANY KEYWORDS                        
         BE    CHA8                                                             
         LA    R3,KEYSTACK                                                      
         USING KEYTABD,R3                                                       
*                                                                               
         CLI   KEYNUM,DELUN        TEST FOR DELETE                              
         BNE   *+12                                                             
         BAS   RE,DELU                                                          
         B     CHA30                                                            
*                                                                               
         CLI   UNALLOC,YES         TEST FOR UNALLOCATION                        
         BNE   *+12                                                             
         BAS   RE,UNALLO                                                        
         B     CHA15                                                            
*                                                                               
         CLI   ESTLOOK,YES         TEST FOR ESTIMATE LOOK-UP                    
         BNE   CHA7                                                             
         GOTO1 VBLDTVQ,DMCB,NBAIO  CREATE DEFAULT TVQ ELEMENTS                  
         B     CHA10                                                            
*                                                                               
CHA7     L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         MVC   OLDSTAT,NURSTAT                                                  
         NI    OLDSTAT,X'03'                                                    
         DROP  R3                                                               
         BAS   RE,KEYED                                                         
*                                                                               
         SPACE                                                                  
CHA8     CLI   ESTDEMOS,0          TEST FOR ESTIMATED DEMOS                     
         BE    CHA9                NONE                                         
*--CALL EDDEM ROUTINE                                                           
         GOTO1 =A(OVFLRTN),DMCB,                                       X        
               (0,DUB),                                                X        
               (RC),                                                   X        
               (R9),                                                   X        
               (ESTDEMOS,ESTLIST),                                     X        
               RR=MYRELO                                                        
*                                                                               
         MVI   DEMTYPE,C'E'                                                     
         GOTO1 LOOKDMU,DMCB,(ESTDEMOS,ESTLIST),ESTVAL                           
         GOTO1 OVERDEM,DMCB,(ESTDEMOS,ESTLIST),ESTVAL,ESTGLOB,ESTUNIV           
         SPACE                                                                  
CHA9     CLI   ACTDEMOS,0          TEST FOR ACTUAL DEMOS                        
         BE    CHA10                                                            
*--CALL EDDEM ROUTINE                                                           
         GOTO1 =A(OVFLRTN),DMCB,                                       X        
               (0,DUB),                                                X        
               (RC),                                                   X        
               (R9),                                                   X        
               (ACTDEMOS,ACTLIST),                                     X        
               RR=MYRELO                                                        
*                                                                               
         MVI   DEMTYPE,C'A'                                                     
         GOTO1 LOOKDMU,DMCB,(ACTDEMOS,ACTLIST),ACTVALS                          
         GOTO1 OVERDEM,(R1),(ACTDEMOS,ACTLIST),ACTVALS,ACTGLOB,ACTUNIV          
         SPACE 1                                                                
CHA10    L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         CLC   NUKDATE,OLDKEY+4    TEST FOR DATE/TIME CHANGE                    
         BE    *+14                                                             
         BAS   RE,GETSUB           GET NEW SUB-LINE                             
         MVC   NUKSUB,SUB          SET IN KEY                                   
         GOTO1 VGETPROG,DMCB,NUKDATE                                            
         CLI   FERN,0              TEST IF STILL COVERED BY PROGRAM             
         BNE   ERRED               NO                                           
         GOTO1 VDISPROG                                                         
*                                                                               
         TM    NUUNST2,X'01'       DEMO RETREIVAL                               
         BZ    CHA10A                                                           
         OC    NBMGBPCD(NBMGBLEN),NBMGBPCD   TEST IF A MAKE-GOOD                
         BZ    CHA11               NO                                           
         MVI   UNNOLOOK,YES        SUPPRESS LOOKUP OF ESTIMATED DEMOS           
         B     CHA12                                                            
*                                                                               
CHA10A   OC    NBMGFPCD(NBMGFLEN),NBMGFPCD   TEST IF A MAKE-GOOD                
         BZ    CHA11               NO                                           
         CLI   BUYPROF+1,C'Y'      TEST IF M-G HAS OWN ESTIMATES                
         BE    CHA11               YES                                          
         MVI   UNNOLOOK,YES        SUPPRESS LOOKUP OF ESTIMATED DEMOS           
         B     CHA12                                                            
*                                                                               
CHA11    MVC   UNESTLK,ESTLOOK     SET ESTIMATE LOOK-UP OPTION                  
*                                                                               
CHA12    GOTO1 VEDIT,DMCB,(C'D',(R4))   RE-LOOK UP ESTIMATED DEMOS              
         CLI   UNERROR,0                                                        
         BE    CHA15                                                            
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
         SPACE                                                                  
* RE-LOOKUP RECORD AND DEMO VALUES FOR RE-DISPLAY                               
*                                                                               
CHA15    GOTO1 VEDIT,DMCB,(C'F',(R4))                                           
         XC    NBADEM,NBADEM       CLEAR DEMO BLOCK POINTER                     
*        BAS   RE,SAVGUAR                                                       
         GOTO1 VNETVAL,DMCB,NETBLOCK    RE-VALUE UNIT                           
*        BAS   RE,RESGUAR                                                       
         CLI   ESTLOOK,YES                                                      
         BE    CHA20                                                            
         CLI   ESTDEMOS,0                                                       
         BE    CHA16                                                            
         MVI   DEMTYPE,C'E'                                                     
         GOTO1 LOOKDEM,DMCB,(ESTDEMOS,ESTLIST),ESTVAL                           
         SPACE 1                                                                
CHA16    CLI   ACTDEMOS,0                                                       
         BE    CHA17                                                            
         MVI   DEMTYPE,C'A'                                                     
         GOTO1 LOOKDEM,DMCB,(ACTDEMOS,ACTLIST),ACTVALS                          
         SPACE                                                                  
CHA17    L     R2,APROTFLD                                                      
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
         ST    R2,AFLD             SET POINTER FOR RE-DISPLAY ROUTINES          
         BAS   RE,PROTDIS                                                       
         L     R2,AUNPRFLD                                                      
         ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES                                                   
         OI    6(R2),X'80'                                                      
         BAS   RE,FLDOUT                                                        
         SPACE                                                                  
* I/O HANDLING FOR FILE UPDATE                                                  
*                                                                               
CHA20    L     R3,NBAIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),OLDKEY                                              
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                MUST FIND OLD KEY                            
         MVC   OLDDA,NDXDA         SAVE ITS DISK ADDRESS                        
         MVC   NEWKEY,0(R3)        SAVE NEW KEY                                 
         CLC   OLDKEY,NEWKEY       TEST IF KEY CHANGED                          
         BNE   CHA21A              YES                                          
*                                                                               
         CLC   OLDSTAT,NEWSTAT     DID THE STATUS CHANGE                        
         BE    CHA21                                                            
         NI    KEY+NDIRCTL,X'FC'   STATUS CHANGE CLEAR STATION TYPE             
         OC    KEY+NDIRCTL(1),NEWSTAT   LOAD NEW STATUS                         
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
CHA21    MVC   NEWDA,OLDDA         RE-READ AND PUT RECORD                       
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,NBAIO                                     
         B     CHA23                                                            
         SPACE                                                                  
CHA21A   GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA2                            
         L     R2,AIOAREA2         POINT TO OLD RECORD                          
         OI    NURSTAT-NUKEY(R2),X'80' MARK IT DELETE AND PUT FOR RECV          
*                                                                               
*--SAVE NEW KEY ON DELETED REORD TO KEEP A LINK                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,X'90'                                                       
         MVI   WORK+1,22                                                        
         MVC   WORK+2(20),NEWKEY   NEW RECORDS KEY                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(R2),WORK,0                           
*                                                                               
* GET RIDE OF TRAFFIC ELEMENTS UNLESS DATE CHANGES ALSO                         
         CLC   NUKDATE-NUKEY(2,R2),NUKDATE-NUKEY(R3)                            
         BNE   CHA21B              DATE CHANGE                                  
         GOTO1 VHELLO,(R1),(C'D',UNTFILE),(X'21',(R2)),0                        
         GOTO1 VHELLO,(R1),(C'D',UNTFILE),(X'22',(R2)),0                        
         GOTO1 VHELLO,(R1),(C'D',UNTFILE),(X'23',(R2)),0                        
*                                      GET RIDE OF TRAFFIC ELEMENTS             
CHA21B   OI    KEY+NDIRCTL,X'80'   KEY CHANGE - DELETE OLD KEY FIRST            
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
         GOTO1 (RF),(R1),UNT+FILE+PUT,AIOAREA2                                  
         XC    KEY,KEY                                                          
         MVC   KEY(L'NEWKEY),NEWKEY READ TO SEE IF NEWKEY IS THERE              
         GOTO1 (RF),(R1),UPDATE+PASSDEL+UNT+DIR+HIGH                            
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF NEW KEY FOUND                       
         BE    CHA22               YES                                          
*                                                                               
* KEY CHANGE AND NEW KEY IS NOT ON FILE SO JUST DO ADDREC                       
*                                                                               
         GOTO1 (RF),(R1),UNT+FILE+ADDREC,NBAIO                                  
         MVC   NEWDA,NDXDA         SAVE NEW DISK ADDRESS                        
         B     CHA23                                                            
         SPACE                                                                  
* KEY CHANGE AND NEW KEY IS THERE MARKED DELETE - GET DELETED                   
* RECORD, UNDELETE POINTER, THEN REPLACE WITH NEW RECORD                        
*                                                                               
CHA22    MVC   NEWDA,NDXDA         SAVE DISK ADDRESS OF NEW KEY                 
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+FILE+GET,AIOAREA2                    
         NI    KEY+NDIRCTL,X'FF'-X'80' UNDELETE MAJOR POINTER-WRITE             
         NI    NURSTAT,X'FF'-X'80' TURN OFF DELETE BIT ON NEW REC               
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         GOTO1 (RF),(R1),UNT+FILE+PUT,NBAIO                                     
         B     CHA23                                                            
         SPACE 1                                                                
CHA23    GOTO1 VEDIT,DMCB,(C'P',UNBLOCK),(C'O',AIOAREA2)                        
         L     R2,AIOAREA2                                                      
         LA    R2,1000(R2)         GENERATE NEW KEY POINTERS                    
         GOTO1 (RF),(R1),,(C'N',(R2))                                           
         LR    R4,R2               POINT R4 AT NEW POINTERS                     
         L     R2,AIOAREA2         POINT R2 AT OLD POINTERS                     
         SPACE                                                                  
CHA24    CLI   0(R2),X'FF'         TEST FOR E-O-L                               
         BE    CHA25                                                            
         BAS   RE,OLDPTR           DELETE OLD POINTER                           
         BAS   RE,NEWPTR           ADD NEW POINTER                              
         LA    R2,NDIRLEN(R2)                                                   
         LA    R4,NDIRLEN(R4)                                                   
         B     CHA24                                                            
         SPACE                                                                  
CHA25    CLI   NBACTSUB,2          TEST FOR SUB-LINE 2                          
         BNE   CHA30                                                            
         LA    RE,OLDKEY                                                        
         CLC   NUKDATE,NUKDATE-NUKEY(RE)  TEST FOR DATE CHANGE                  
         BE    CHA30                                                            
         BAS   RE,SUBPRT           SUB-PRINT UPDATE ON LINE 1                   
         SPACE                                                                  
CHA30    L     R2,AUNPRFLD         BUMP TO NEXT FIELD                           
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     CHA2                                                             
         SPACE                                                                  
CHAX     B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO GET UNIT RECORD                                                
*                                                                               
GETUNIT  ST    RE,SAVEREG                                                       
         MVC   NBSELSTR,UNITDATE                                                
         MVC   NBSELEND,NBSELSTR                                                
         MVC   NBSELSUB,SUB                                                     
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBUSER+13,NO        ** FORCE BACK PRE-EMPTS                      
         MVI   NBDIRECT,YES                                                     
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBESTOPT,C'M'       LOOK UP HOMES VALUES                         
         OI    NBSPLOPT,X'10'      DONT BREAK OUT COPY SPLIT NUMBERS            
         MVC   NBAIO,AIOAREA1                                                   
         SPACE                                                                  
*        BAS   RE,SAVGUAR                                                       
GETUNIT2 GOTO1 VNETIO,DMCB,NEBLOCKD                                             
*        BAS   RE,RESGUAR                                                       
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RETURNED                       
         BE    OKEXIT              YES                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ERREXIT                                                          
         B     GETUNIT2                                                         
         SPACE 2                                                                
OKEXIT   CR    RB,RB                                                            
         B     *+6                                                              
ERREXIT  LTR   RB,RB                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET NEXT SUB-LINE NUMBER                                       
*                                                                               
GETSUB   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         L     R1,NBAIO                                                         
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
         CLI   NUKPSUB,SUBMAX      TEST AGAINST SUB-LINE LIMIT                  
         BL    GETSUB2             NO-GET NEXT DIRECTORY ENTRY                  
         B     ERRED                                                            
         SPACE                                                                  
GETSUB4  LA    R4,KEYSAVE                                                       
         ZIC   R1,NUKPSUB                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SUB                                                           
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DELETE OLD PASSIVE POINTERS (AT ENTRY, R2 ADDRESSES            
* POINTERS)                                                                     
*                                                                               
OLDPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R2)  DELETE OLD POINTER                           
         GOTO1 AIO,DMCB,UPDATE+UNT+DIR+HIGH                                     
         CLC   KEY(L'NUKEY),KEYSAVE                                             
         BNE   OLDPTRX                                                          
         OI    KEY+NDIRCTL,X'80'   DELETE BIT                                   
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
OLDPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR NEW POINTERS (AT ENTRY, R4 ADDRESSES POINTER)                 
*                                                                               
NEWPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R4)  READ FOR NEW POINTER                         
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF KEY FOUND                           
         BE    NEWPTR2                                                          
         MVC   KEY(L'NUKEY+1),0(R4)                                             
         MVC   KEY+NDIRDA(NDIRLDA),NEWDA                                        
         GOTO1 (RF),(R1),UNT+DIR+ADD                                            
         B     NEWPTRX                                                          
         SPACE                                                                  
NEWPTR2  MVI   KEY+NDIRCTL,0                                                    
         MVC   KEY+NDIRDA(NDIRLDA),NEWDA                                        
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
NEWPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO UPDATE FIRST UNIT FOR DATE WHEN SECOND UNIT IS ADDED           
*                                                                               
SUBPRT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    RE,KEY                                                           
         USING NUKPKEY,RE                                                       
         MVI   NUKPTYPE,X'84'                                                   
         MVC   NUKPAM,AGYMED                                                    
         MVC   NUKPCLT,CLIPK                                                    
         MVC   NUKPNET,NET                                                      
         MVC   NUKPPROG,PROG                                                    
         MVC   NUKPDATE,DATE                                                    
         MVC   NUKPEST,EST                                                      
         MVI   NUKPSUB,1           SUB-LINE 1                                   
         L     R1,AIOAREA1                                                      
         MVC   NUKPDP,NUKDP-NUKEY(R1) EXTRACT DAYPART                           
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY+1),KEYSAVE                                    
         BNE   SUBPRTX             COULD NOT FIND IT-DELETED                    
         DROP  RE                                                               
         SPACE                                                                  
SUBPRT2  L     R4,AIOAREA4         GET THE RECORD                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,(R4)                                
         USING NURECD,R4                                                        
         MVI   NUSUBPRT,1                                                       
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         SPACE                                                                  
SUBPRTX  B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DELETE A UNIT RECORD - AT ENTRY R4 POINTS TO UNBLOCK           
*                                                                               
DELU     NTR1                                                                   
         USING UNBLOCKD,R4                                                      
         XC    FTERM,FTERM                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    DELX                                                             
         MVI   FERN,AUDACTER       IF AUDIT IS SET , CANT DELETE                
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'                                                   
         BO    ERRED                                                            
         DROP  RE                                                               
         MVI   FERN,INVERR                                                      
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,DELCOMP                                                       
         BNE   ERRED                                                            
         MVI   FERN,DELERR                                                      
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         TM    NUUNITST,X'03'      TEST IF MISSED/AMKE-GOOD                     
         BNZ   ERRED               CANNOT DELETE                                
         CLI   UNBILLSW,YES        TEST IF UNIT BILLED                          
         BNE   *+14                NO                                           
         MVC   XTRA(6),=C'BILLED'                                               
         B     ERRED                                                            
         CLI   UNPAYSW,YES         TEST IF UNIT PAID                            
         BNE   *+14                                                             
         MVC   XTRA(4),=C'PAID'                                                 
         B     ERRED                                                            
         SPACE                                                                  
DEL2     XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),NUKEY                                               
         GOTO1 AIO,DMCB,UPDATE+UNT+DIR+READ                                     
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,NBAIO                              
         OI    NURSTAT,X'80'       SET DELETE BIT IN RECORD                     
         OI    KEY+NDIRCTL,X'80'                                                
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         GOTO1 (RF),(R1),UNT+FILE+PUT,NBAIO                                     
*                                                                               
         GOTO1 VEDIT,DMCB,(C'P',(R4)),(C'O',AIOAREA2)                           
         L     R2,AIOAREA2                                                      
         SPACE                                                                  
DEL4     CLI   0(R2),X'FF'                                                      
         BE    DEL6                                                             
         BAS   RE,OLDPTR                                                        
         LA    R2,NDIRLEN(R2)                                                   
         B     DEL4                                                             
         SPACE                                                                  
DEL6     L     R2,FADDR            POINT TO FIELD HEADER                        
         MVI   8(R2),PLUS          EDIT SUPPRESSION FOR DOUBLE DELETION         
         OI    6(R2),X'80'         SEND FIELD BACK                              
         SPACE                                                                  
DELX     B     EXXMOD                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
DELCOMP  CLC   FLD(0),=C'DEL'                                                   
         EJECT                                                                  
* SUB-ROUTINE TO 'UNALLOCATE' A UNIT - AT ENTRY R3 POINTS TO KEYSTACK           
*                                                                               
UNALLO   NTR1                                                                   
         USING KEYTABD,R3                                                       
         USING UNBLOCKD,R4                                                      
         L     R2,FADDR                                                         
         ST    R2,UNFLDH                                                        
         ZIC   R0,KEYCOUNT                                                      
         MVI   FLDH+5,0                                                         
         MVC   FLD,SPACES                                                       
         SPACE                                                                  
UNALLO2  MVC   UNEDATA,KEYDATA                                                  
         GOTO1 VEDIT,DMCB,(C'X',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+14                                                             
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
*                                                                               
         LA    R3,KEYENTL(R3)                                                   
         BCT   R0,UNALLO2                                                       
         MVI   8(R2),PLUS          RETURN FIELD W EDIT SUPPRESSION CHAR         
         OI    6(R2),X'80'                                                      
         B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT KEYWORD INPUT AND TO UPDATE RECORD                        
*                                                                               
KEYED    NTR1                                                                   
         ZIC   R2,KEYCOUNT                                                      
         LA    R3,KEYSTACK                                                      
         USING KEYTABD,R3                                                       
         USING UNBLOCKD,R4         AT ENTRY, R4 POINTS TO UNIT BLOCK            
         SPACE                                                                  
KEYED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         OC    KEYVALUE,KEYVALUE   TEST FOR GLOBAL VALUE                        
         BZ    KEYED3              NO                                           
*                                  YES-GLOBAL VALUE FOR THE KEYWORD             
         CLI   FLDH+5,0            TEST FOR POSITIONAL COMMA IN FIELD           
         BNE   KEYED2A             NO                                           
         CLI   FSTOP,X'FF'                                                      
         BE    KEYED2A                                                          
         ICM   R1,7,FLAST          BUMP STRING POINTER PAST COMMA               
         LA    R1,1(R1)            SO EDIT CAN RESUME CORRECTLY                 
         STCM  R1,7,FLAST                                                       
*                                                                               
KEYED2A  MVC   FLDH,KEYFLDH        YES-PLUG IN GLOBAL VALUES                    
         MVC   FLD(L'KEYVALUE),KEYVALUE                                         
         TM    FLDH+4,X'08'        TEST FOR NUMERIC GLOBAL VALUE                
         BZ    KEYED3              NO                                           
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         SPACE                                                                  
KEYED3   CLI   FLDH+5,0                                                         
         BNE   KEYED4                                                           
         CLI   FSTOP,X'FF'                                                      
         BE    KEYEDX                                                           
         ICM   R1,7,FLAST          BUMP STRING POINTER PAST                     
         LA    R1,1(R1)            POSITIONAL COMMA                             
         STCM  R1,7,FLAST                                                       
         B     KEYED6                                                           
         SPACE                                                                  
KEYED4   TM    KEYCTL,DISONLY      TEST FOR DISPLAY ONLY DATA                   
         BO    KEYED6              YES-SKIP TO NEXT KEYWORD                     
         TM    NBUNST3,X'40'                                                    
         BZ    KEYED4A                                                          
         TM    KEYCTL,COPSDIS      TEST FOR COPY SPLIT DISPLAY                  
         BO    KEYED6              YES-SKIP TO NEXT KEYWORD                     
*                                                                               
KEYED4A  OC    KEYEDIT,KEYEDIT     TEST FOR OVERRIDE EDIT ROUTINE               
         BNZ   KEYED5                                                           
         CLI   KEYDATA,UINT                                                     
         BNE   KEYED4B                                                          
         CLC   =C'TBL',FLD                                                      
         BNE   KEYED4B                                                          
*                                                                               
         LA    RF,INTGTBL          READ RECORD EACH TIME (NO IO AREAS)          
         ST    RF,UNINGTBL         PASS BACK INTG RATES HERE                    
         LA    RF,DUB              DUMMY AREA TO SAVE RECORD                    
         ST    RF,INTHLDIT                                                      
         MVI   INTGREAD,0          DO 1ST READ                                  
         MVC   INTGAIO,AIOAREA3                                                 
         MVC   INTGSTDT,NBACTDAT                                                
         MVC   INTGEDDT,NBACTDAT                                                
*                                                                               
KEYED4B  MVC   UNFLDH,FADDR                                                     
         MVC   UNEDATA,KEYDATA                                                  
         GOTO1 VEDIT,DMCB,(C'X',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    KEYED6                                                           
         MVC   FERN,UNERROR                                                     
         B     KEYEDR                                                           
         SPACE                                                                  
KEYED5   SR    RF,RF                                                            
         ICM   RF,7,KEYEDIT                                                     
         GOTO1 (RF),RR=MYRELO                                                   
         SPACE                                                                  
KEYED6   LA    R3,KEYENTL(R3)                                                   
         BCT   R2,KEYED2                                                        
         SPACE                                                                  
*--SET POSTING TYPE IN THE STATUS FIELD                                         
*                                                                               
         USING NUSDRD,RE                                                        
         MVI   BYTE,X'02'                                                       
         BAS   RE,GETEL                                                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
*                                                                               
         LA    RF,NEWSTAT                                                       
*                                                                               
         L     R1,AIOAREA1                                                      
         USING NURECD,R1                                                        
*                                                                               
         MVI   0(RF),0                                                          
         NI    NURSTAT,X'FC'                                                    
         CLI   NUPOSTYP,C'N'       NETWORK                                      
         BE    KEYEDX                                                           
         CLI   NUPOSTYP,C'C'       CABLE                                        
         BNE   *+16                                                             
         OI    0(RF),X'01'                                                      
         OI    NURSTAT,X'01'                                                    
         B     KEYEDX                                                           
         CLI   NUPOSTYP,C'S'       SYNDICATION                                  
         BNE   *+16                                                             
         OI    0(RF),X'02'                                                      
         OI    NURSTAT,X'02'                                                    
         B     KEYEDX                                                           
         OI    0(RF),X'03'        OTHER                                         
         OI    NURSTAT,X'03'                                                    
         SPACE                                                                  
KEYEDX   B     EXXMOD                                                           
*                                                                               
KEYEDR   B     ERRED                                                            
         DROP  R3,RE,R1                                                         
         EJECT                                                                  
* ROUTINE TO HANDLE ERROR CONDITION IN EDIT - PUT EDIT SUPPRESSION              
* CHARACTER IN ALL FIELDS BEFORE ERROR AND FORCE ANY CHANGED FIELD              
* AFTER ERROR TO BE TRANSMITTED NEXT TIME.                                      
*                                                                               
ERRED    L     R3,FADDR            GET ERROR FIELD ADDRESS                      
         L     R2,AFSTFLD          POINT TO FIRST FIELD                         
         SPACE                                                                  
ERRED2   SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         CR    R2,R3                                                            
         BNL   ERRED6                                                           
         SPACE                                                                  
ERRED4   ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         MVI   8(R2),PLUS                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R2),WORK                                                     
         B     ERRED2                                                           
         SPACE                                                                  
ERRED6   LR    R2,R3               POINT R2 AT ERROR FIELD                      
         L     R3,ALSTFLD          POINT R3 AT END OF SCREEN                    
         SR    R0,R0                                                            
*                                                                               
ERRED7   IC    R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CR    R2,R3               TEST FOR END OF SCREEN                       
         BNL   ERREDX                                                           
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    ERRED7              YES                                          
         TM    4(R2),X'80'         TEST IF SENT THIS TIME                       
         BZ    ERRED7              NO-MEANS NO CHANGES                          
         OI    6(R2),X'81'         YES-XMIT BACK AS MODIFIED NEXT TIME          
         B     ERRED7              TO FORCE FIELD TO BE EDITED                  
         SPACE                                                                  
ERREDX   B     ERROR                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT PREEMPTS                                                  
*                                                                               
* EPREMPT  L     R1,NBAIO                                                       
*          USING NURECD,R1                                                      
*          NI    NUUNITST,X'BF'                                                 
*          CLI   FLD,C'Y'                                                       
*          BNE   EPREEX                                                         
*          OI    NUUNITST,X'40'                                                 
* EPREEX   BR    RE                                                             
*          DROP  R1                                                             
*          SPACE 2                                                              
*                                                                               
* SUB-ROUTINE TO EDIT PREEMPTS                                                  
*                                                                               
EPREMPT  NTR1                                                                   
         L     R3,NBAIO                                                         
         USING NURECD,R3                                                        
         MVC   BYTE,NUUNITST                                                    
         NI    NUUNITST,X'BF'                                                   
*                                                                               
         CLI   FLD,NO              TEST FOR 'NO'                                
         BE    EPREEX              YES                                          
         MVI   FERN,INVERR                                                      
         CLI   FLD,C'Y'            TEST FOR 'YES'                               
         BNE   ERRED                                                            
         MVI   FERN,PREERR         MUST REMOVE MAKE-GOOD REFERENCE              
         TM    NUUNITST,X'02'      TEST IF UNIT MISSED                          
         BO    ERRED                                                            
         OI    NUUNITST,X'40'      UNIT STATUS IS PRE-EMPT                      
*                                                                               
         L     R3,NBAIO            R3 POINTS TO RECORD                          
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)                                                        
         OI    5(RE),X'08'         LAST CHANGE IS A PREEMPT                     
         SPACE 1                                                                
*-CHECK PROFILE TO SEE IF PRE-EMPT CAN BE CHANGED IF UNIT IS PAYED              
         LA    RE,BLOCK                                                         
         USING UNBLOCKD,RE                                                      
         TM    BYTE,X'40'          WAS IT SET                                   
         BNZ   EPREEX              YES, NO CHANGE IN STATUS                     
         CLI   BUYPROF2+13,C'A'                                                 
         BNE   EPREEX                                                           
         CLI   UNPAYSW,YES                                                      
         BNE   EPREEX                                                           
         MVI   FERN,PAYCHGNA                                                    
         B     ERRED                                                            
*                                                                               
EPREEX   L     RE,SAVEREG                                                       
         B     EXXMOD                                                           
         DROP  R3,RE                                                            
*                                                                               
* SUB-ROUTINE TO EDIT PREEMPTS                                                  
*                                                                               
EDHI     CLI   FLD,C'N'                                                         
         BE    EDHIX                                                            
         MVI   ELCODE,X'DD'                                                     
         MVI   LFIL,4                                                           
         MVC   DUB(4),=XL4'0000E301' T1                                         
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(ELCODE,AIOAREA1),(LFIL,DUB)          
         MVC   DUB(4),=XL4'0000C801' H1                                         
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(ELCODE,AIOAREA1),(LFIL,DUB)          
         MVI   FLD,C'Y'                                                         
EDHIX    B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT ADU'S                                                     
*                                                                               
EADU     NTR1                                                                   
*--RETRIEVE NUUNST3 FROM 02 ELEMENT                                             
         USING NUSDRD,RE                                                        
         MVI   BYTE,X'02'                                                       
         BAS   RE,GETEL                                                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         NI    NUSDST3,X'FD'       TURN OFF ADU BIT                             
         CLI   FLD,C'Y'                                                         
         BNE   EADUEX                                                           
*                                                                               
         L     R1,NBAIO                                                         
         USING NURECD,R1                                                        
         OC    NUACTUAL,NUACTUAL   ACTAUL COST NOT ALLOWED                      
         BNZ   EADUER                                                           
         OC    NUINTEG,NUINTEG     INTEGRATION COST NOT ALLOWED                 
         BNZ   EADUER                                                           
         OI    NUSDST3,X'02'                                                    
EADUEX   B     EXXMOD                                                           
*                                                                               
EADUER   MVI   FERN,ADUERR                                                      
         B     ERRED                                                            
         DROP  RE,R1                                                            
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT DATE INPUT                                                
*                                                                               
EDATE    ST    RE,SAVEREG                                                       
         GOTO1 VDATVAL,DMCB,(1,FLD),DUB                                         
         MVI   FERN,DATERR                                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    ERRED                                                            
         CLC   FLDH+5(1),3(R1)                                                  
         BNE   ERRED                                                            
         MVC   DUB(2),ESTSTART                                                  
         CLC   ESTSTART(2),ESTEND                                               
         BE    EDATE2                                                           
         CLC   DUB+2(4),ESTSTART+2                                              
         BNL   *+10                                                             
         MVC   DUB(2),ESTEND                                                    
         SPACE                                                                  
EDATE2   GOTO1 VDATCON,(R1),DUB,(2,HALF)                                        
         L     R1,NBAIO                                                         
         USING NURECD,R1                                                        
         MVC   NUKDATE,HALF        SET INPUT DATE                               
         GOTO1 VGETDAY,DMCB,DUB,FULL2                                           
         MVC   FLD,SPACES                                                       
         MVC   FLD(3),FULL2        DAY                                          
         MVI   FLDH+5,3            SET DATA LENGTH                              
         OI    FLDH+4,X'04'        AND ALPHA BITS                               
         MVC   UNFLDH,FADDR                                                     
         MVI   UNEDATA,UDAY                                                     
         GOTO1 VEDIT,DMCB,(C'X',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    EDATEX                                                           
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
         SPACE                                                                  
EDATEX   L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
* SUB-ROUTINE TO HUT AVERAGE INPUT                                              
*                                                                               
EHUTAV   ST    RE,SAVEREG                                                       
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         MVC   UNAREC,NBAIO                                                     
         ST    R9,UNAGLOB                                                       
         MVC   UNALOCAL,AIOAREA4                                                
         MVC   UNFLDH,FADDR                                                     
         MVI   UNEDATA,UHAVE                                                    
         GOTO1 VEDIT,DMCB,(C'X',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    EHUTA20                                                          
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
EHUTA20  GOTO1 VEDIT,DMCB,(C'D',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    EHUTA50                                                          
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
         SPACE                                                                  
EHUTA50  L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT TRAFFIC FEED CODES                                        
*                                                                               
EFEEDC   NTR1                                                                   
         MVI   BYTE,0              INITIALIZE PRODUCT CODE FIELD                
         MVI   DUB,0                                                            
         LA    R3,1                                                             
         XC    OVEREL,OVEREL                                                    
*--SET UP EDIT BLOCK                                                            
         MVC   HUTBLK,BLOCK                                                     
         LA    R4,HUTBLK                                                        
         USING UNBLOCKD,R4                                                      
         MVC   UNAREC,NBAIO                                                     
         ST    R9,UNAGLOB                                                       
         MVC   UNALOCAL,AIOAREA4                                                
         MVC   UNFLDH,FADDR                                                     
*--SAVE CURRENT PRODUCTS IN TRPRSAVE MOVE PRODUCTS TO TRPRDHLD                  
         XC    TRPRDHLD,TRPRDHLD                                                
         MVC   TRPRSAVE,NBPRD                                                   
         MVC   TRPRDHLD(2),NBPRD                                                
         XC    NBPRD,NBPRD                                                      
*                                                                               
         TM    NBUNST3,X'40'       IS BUY A COPY SPLIT                          
         BZ    EFEDC080            NO PROCESS NORMALLY                          
*--CHECK TO SEE IF MORE THEN 2 PRODUCTS                                         
         LA    R3,2                DEFAULT NUMBER OF PRODS TO 2                 
*                                                                               
         MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BNE   EFEDC018                                                         
*                                                                               
         XC    HALF2,HALF2                                                      
         L     R5,12(R1)           PRODUCT ELEMENT                              
         USING NUPRDD,R5                                                        
*-GET NUMBER OF PRODUCTS                                                        
         SR    RE,RE                                                            
         ZIC   RF,NUPRDLEN                                                      
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
*                                                                               
         LR    R3,RF               PRODUCT COUNT                                
         LA    RE,TRPRDHLD         PRODUCT STORE AREA                           
         LA    R1,NUPRDPR          FIRST PRODUCT                                
EFEDC010 MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,6(R1)            NEXT PRODUCT                                 
         BCT   RF,EFEDC010                                                      
*                                                                               
EFEDC018 GOTO1 VSCANNER,DMCB,(20,FLDH),(6,BLOCK),C',=-='                        
         L     R5,DMCB+4                                                        
         CLI   4(R1),0                                                          
         BE    EFEDC300                                                         
*                                                                               
*-- VALIDATE PRODUCT                                                            
*                                                                               
*--THE FOLLOWING FOUR LINES ARE THERE TO COMPENSATE                             
*--FOR A SCANNER BUG THAT PUTS OUT NULL LINES. SORRY.                           
*                                                                               
EFEDC020 OC    0(2,R5),0(R5)                                                    
         BNZ   EFEDC022                                                         
         LA    R5,42(R5)                                                        
         B     EFEDC020                                                         
EFEDC022 MVC   FLD,SPACES                                                       
         MVC   FLD(10),12(R5)      PRODUCT                                      
         MVC   FLDH+5(1),0(R5)     SET DATA LENGTH                              
         OI    FLDH+4,X'04'        AND ALPHA BITS                               
*                                                                               
         MVI   UNEDATA,UPRD                                                     
         MVC   NBPRD,TRPRDHLD      VALIDATE CURRENT PRODUCT                     
         GOTO1 VEDIT,DMCB,(C'S',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    EFEDC030                                                         
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
*--LOAD NATIONAL PRODUCT (NBPRD) INTO THE 21 ELEMENT                            
EFEDC030 CLI   OVEREL,0            PROCESS CODE FIRST PASS ONLY                 
         BNE   EFEDC040                                                         
         MVC   OVEREL(1),BYTE                                                   
         MVI   BYTE,X'21'                                                       
         BAS   RE,GETEL                                                         
         BE    EFEDC035                                                         
*--ELEMENT NOT FOUND BUILD 21 ELEMENT                                           
         MVC   BYTE,OVEREL                                                      
         XC    OVEREL,OVEREL                                                    
         LA    RE,OVEREL                                                        
         USING NUCMLEL,RE                                                       
         MVC   NUCMLEID(2),=X'2134'                                             
         MVC   NUCMLPRD(1),BYTE                                                 
         BAS   RE,PUTEL                                                         
         DROP  RE                                                               
         B     EFEDC040                                                         
*                                                                               
EFEDC035 MVC   BYTE,OVEREL                                                      
         L     RE,12(R1)                                                        
         USING NUCMLEL,RE                                                       
         MVC   NUCMLPRD(1),BYTE                                                 
         NI    NUCMLFL2,X'FE'       SET FOR NATIONAL                            
         DROP  RE                                                               
*                                                                               
EFEDC040 MVI   FERN,PRDERR                                                      
         CLC   BYTE,TRPRDHLD       DO PRODUCTS MATCH                            
         BNE   ERRED                                                            
         MVC   TRPRDHLD(5),TRPRDHLD+1    BUMP TO NEXT PRODUCT                   
*-- CREATE TRAFFIC FEED CODE ELEMENTS                                           
EFEDC050 CLC   22(2,R5),=C'* '                                                  
         BE    EFEDC120                                                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(20),22(R5)      FEED CODES                                   
         MVC   FLDH+5(1),1(R5)     SET DATA LENGTH                              
         OI    FLDH+4,X'04'        AND ALPHA BITS                               
*                                                                               
EFEDC080 LA    R4,HUTBLK                                                        
         USING UNBLOCKD,R4                                                      
         MVI   UNEDATA,UFEEDCD                                                  
         GOTO1 VEDIT,DMCB,(C'X',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    EFEDC100                                                         
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
*                                                                               
EFEDC100 MVI   DUB,X'FF'                                                        
EFEDC120 LA    R5,42(R5)                                                        
         XC    HALF,HALF                                                        
         BCT   R3,EFEDC020                                                      
*                                                                               
         TM    NBUNST3,X'40'                                                    
         BZ    EFEDC300                                                         
*                                                                               
         USING NUFDCEL,R4                                                       
         MVI   BYTE,X'23'                                                       
         BAS   RE,GETEL                                                         
         BNE   ERRED                                                            
         L     R4,12(R1)                                                        
         B     EFEDC150                                                         
*                                                                               
EFEDC130 ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),X'23'                                                      
         BNE   EFEDC300                                                         
EFEDC150 OI    NUFDCFL2,X'01'                                                   
         B     EFEDC130                                                         
*                                                                               
EFEDC300 MVI   BYTE,X'23'                                                       
         USING NUFDCEL,R4                                                       
         BAS   RE,GETEL                                                         
         BNE   EFEDCEX                                                          
         L     R4,12(R1)                                                        
         B     EFEDC350                                                         
*                                                                               
EFEDC330 ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
EFEDC350 CLI   0(R4),X'23'                                                      
         BNE   EFEDCEX                                                          
*                                                                               
         TM    NUFDCFL2,X'40'                                                   
         BO    EFEDC330                                                         
         TM    NUFDCFL2,X'80'                                                   
         BZ    EFEDC330                                                         
         MVC   DUB(4),NUFDCFED                                                  
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'23',NBAIO),(4,DUB)                 
         B     EFEDC350                                                         
*                                                                               
EFEDCEX  XC    FERN,FERN                                                        
         MVC   BLOCK,HUTBLK                                                     
         MVC   NBPRD,TRPRSAVE                                                   
         B     EXXMOD                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EDIT FEED AND SHARE PERCENTS                                   
*                                                                               
EFEDSHR  NTR1                                                                   
         USING KEYTABD,R3                                                       
         MVC   KDATASV,KEYDATA     SAVE DATA TYPE                               
         MVI   BYTE,0              INITIALIZE PRODUCT CODE FIELD                
         MVI   DUB,0                                                            
         SR    R2,R2               POINTER TO 14 ELEMENT                        
         LA    R3,1                                                             
         XC    OVEREL,OVEREL                                                    
*--SET UP EDIT BLOCK                                                            
         MVC   HUTBLK,BLOCK                                                     
         LA    R4,HUTBLK                                                        
         USING UNBLOCKD,R4                                                      
         MVC   UNAREC,NBAIO                                                     
         ST    R9,UNAGLOB                                                       
         MVC   UNALOCAL,AIOAREA4                                                
         MVC   UNFLDH,FADDR                                                     
*--SAVE CURRENT PRODUCTS IN TRPRSAVE MOVE PRODUCTS TO TRPRDHLD                  
         XC    TRPRDHLD,TRPRDHLD                                                
         MVC   TRPRSAVE,NBPRD                                                   
         MVC   TRPRDHLD(2),NBPRD                                                
         XC    NBPRD,NBPRD                                                      
*                                                                               
         TM    NBUNST3,X'40'       IS BUY A COPY SPLIT                          
         BZ    EFDSH080            NO PROCESS NORMALLY                          
*--CHECK TO SEE IF MORE THEN 2 PRODUCTS                                         
         LA    R3,1                DEFAULT NUMBER OF PRODS TO 2                 
*                                                                               
         MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BNE   EFDSH080                                                         
*                                                                               
         XC    HALF2,HALF2                                                      
         L     R5,12(R1)           PRODUCT ELEMENT                              
         USING NUPRDD,R5                                                        
         LA    R2,NUPRDPR                                                       
*-GET NUMBER OF PRODUCTS                                                        
         SR    RE,RE                                                            
         ZIC   RF,NUPRDLEN                                                      
         SH    RF,=H'3'                                                         
         D     RE,=F'6'                                                         
         STCM  RF,1,NUMPRD                                                      
*                                                                               
         LR    R3,RF               PRODUCT COUNT                                
         LA    RE,TRPRDHLD         PRODUCT STORE AREA                           
         LA    R1,NUPRDPR          FIRST PRODUCT                                
EFDSH010 MVC   0(1,RE),0(R1)                                                    
         LA    RE,1(RE)                                                         
         LA    R1,6(R1)            NEXT PRODUCT                                 
         BCT   RF,EFDSH010                                                      
*                                                                               
EFDSH018 GOTO1 VSCANNER,DMCB,(10,FLDH),(6,BLOCK),C',=-='                        
         L     R5,DMCB+4                                                        
         CLI   4(R1),0                                                          
         BE    EFDSHEX                                                          
         CLI   22(R5),C'C'         DO THEY WANT A RECALC                        
         BNE   EFDSH020                                                         
         BAS   RE,BLDPCT           RE-CALC ALL TH EPCTS.                        
         B     EFDSHEX                                                          
*                                                                               
*-- VALIDATE PRODUCT                                                            
*                                                                               
*--THE FOLLOWING FOUR LINES ARE THERE TO COMPENSATE                             
*--FOR A SCANNER BUG THAT PUTS OUT NULL LINES. SORRY.                           
*                                                                               
EFDSH020 OC    0(2,R5),0(R5)                                                    
         BNZ   EFDSH022                                                         
         LA    R5,32(R5)                                                        
         B     EFDSH020                                                         
EFDSH022 MVC   FLD,SPACES                                                       
         MVC   FLD(10),12(R5)      PRODUCT                                      
         MVC   FLDH+5(1),0(R5)     SET DATA LENGTH                              
         OI    FLDH+4,X'04'        AND ALPHA BITS                               
*                                                                               
         MVI   UNEDATA,UPRD                                                     
         MVC   NBPRD,TRPRDHLD      VALIDATE CURRENT PRODUCT                     
         GOTO1 VEDIT,DMCB,(C'S',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    EFDSH040                                                         
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
*                                                                               
EFDSH040 MVI   FERN,PRDERR                                                      
         CLC   BYTE,TRPRDHLD       DO PRODUCTS MATCH                            
         BNE   ERRED                                                            
         MVC   TRPRDHLD(5),TRPRDHLD+1    BUMP TO NEXT PRODUCT                   
*-- CREATE TRAFFIC FEED CODE ELEMENTS                                           
EFDSH050 MVC   FLD,SPACES                                                       
         MVC   FLD(20),22(R5)      FEED OR PROD PCT                             
         MVC   FLDH+5(1),1(R5)     SET DATA LENGTH                              
         OI    FLDH+4,X'08'        AND NUMERIC BITS                             
*                                                                               
EFDSH080 LA    R4,HUTBLK                                                        
         USING UNBLOCKD,R4                                                      
         MVC   UNEDATA,KDATASV                                                  
         GOTO1 VEDIT,DMCB,(C'X',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    EFDSH100                                                         
         MVC   FERN,UNERROR                                                     
         B     ERRED                                                            
*                                                                               
EFDSH100 LTR   R2,R2               IS THERE A 14 ELEMENT                        
         BZ    EFDSHEX             NO PROCESS NORMALLY                          
         BAS   RE,LOADPCT          MOVE PCT TO 14 ELEMENT (R2)                  
         LA    R5,32(R5)                                                        
         LA    R2,6(R2)            GET TO NEXT PRODUCT                          
         BCT   R3,EFDSH020                                                      
*                                                                               
EFDSHEX  XC    FERN,FERN                                                        
         MVC   BLOCK,HUTBLK                                                     
         MVC   NBPRD,TRPRSAVE                                                   
         BAS   RE,CHKPCT           MAKE SUR PCTS. ADD UP TO 100                 
         B     EXXMOD                                                           
         DROP  R3,R4,R5                                                         
         SPACE 3                                                                
* MOVE NUMBERS TO THE 14 ELEMENT                                                
* R2 - POINTS TO PRODUCT FIELD IN 14 ELEMENT                                    
*                                                                               
LOADPCT  NTR1                                                                   
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         CLI   KDATASV,UP1SHR      ARE WE DOING PROD SHARE                      
         BNE   LDPCT100                                                         
*--MOVE FIRST PRODUCT PERCENT OUT                                               
         MVC   2(2,R2),NUP1SHR                                                  
         XC    NUP1SHR,NUP1SHR                                                  
         B     LDPCTEX                                                          
*--MOVE FEED PERCENT OUT                                                        
LDPCT100 MVC   4(2,R2),NUFEED                                                   
         XC    NUFEED,NUFEED                                                    
         B     LDPCTEX                                                          
*                                                                               
LDPCTEX  B     EXXMOD                                                           
         DROP  RE                                                               
         SPACE 3                                                                
* MAKE SURE PCTS DO EQUAL 100                                                   
*                                                                               
CHKPCT   NTR1                                                                   
         CLI   NUMPRD,2            IF LESS THEN 2 PRODUCTS                      
         BNH   CHKPCEX             NO NEED FOR THIS CHECK                       
*                                                                               
*                                                                               
*-READ ELEMENT SET THE POINTERS                                                 
         MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                 MORE THEN 2 PRODUCTS                         
         DC    H'0'                ELEMENT MUST BE THERE                        
         L     R5,12(R1)           PRODUCT ELEMENT                              
         USING NUPRDD,R5                                                        
*                                                                               
         LA    R4,NUPRDPCT                                                      
         CLI   KDATASV,UP1SHR      ARE WE DOING PROD SHARE                      
         BE    *+8                                                              
         LA    R4,NUPRDFD          NO SET UP FOR FEED PCT                       
*                                                                               
         ZIC   R3,NUMPRD           NUMBER OF PRODUCTS                           
         SR    R1,R1                                                            
         SR    RE,RE                                                            
CHKPC140 ICM   RE,3,0(R4)                                                       
         AR    R1,RE               TOTAL UP THE PERCENTS                        
         LA    R4,6(R4)            NEXT PRODUCT ENTRY                           
         BCT   R3,CHKPC140                                                      
*                                                                               
*- PCT MUST ADD UP TO 100                                                       
         C     R1,=F'10000'                                                     
         BE    CHKPCEX                                                          
         MVI   FERN,INVERR                                                      
         B     ERRED                                                            
*                                                                               
CHKPCEX  B     EXXMOD                                                           
         DROP  R5                                                               
         SPACE 3                                                                
*- RECALCULATE THE PERCENT UPDATE THE RECORD                                    
BLDPCT   NTR1                                                                   
         CLI   NUMPRD,0                                                         
         BE    BLDPCEX                                                          
         SR    R4,R4                                                            
         SR    R5,R5                                                            
         A     R5,=F'10000'                                                     
         ZIC   R3,NUMPRD                                                        
         DR    R4,R3                                                            
*-ARE THERE MORE THEN 2 PRODUCTS                                                
         CLI   NUMPRD,2                                                         
         BH    BLDPC80                                                          
         LA    RE,NBP1SHR                                                       
         CLI   KDATASV,UP1SHR      ARE WE DOING PROD SHARE                      
         BE    *+8                                                              
         LA    RE,NBFEED           NO SET UP FOR FEED PCT                       
         STCM  R5,3,0(RE)          UPDATE UNIT                                  
         B     BLDPCEX             EXIT                                         
*                                                                               
*-FILL IN THE TABLE                                                             
BLDPC80  MVI   BYTE,X'14'                                                       
         BAS   RE,GETEL                                                         
         BE    *+6                 MORE THEN 2 PRODUCTS                         
         DC    H'0'                ELEMENT MUST BE THERE                        
         L     R2,12(R1)           PRODUCT ELEMENT                              
         USING NUPRDD,R2                                                        
*                                                                               
         LA    R1,NUPRDPCT                                                      
         CLI   KDATASV,UP1SHR      ARE WE DOING PROD SHARE                      
         BE    *+8                                                              
         LA    R1,NUPRDFD          NO SET UP FOR FEED PCT                       
*                                                                               
         ZIC   R3,NUMPRD           NUMBER OF PRODUCTS                           
BLDPC140 SR    RE,RE                                                            
         AR    RE,R4               ADD REMAINDER TO FIRST ENTRY                 
         AR    RE,R5               ADD QUOTIENT                                 
         SR    R4,R4               CLEAR REMAINDER                              
         STCM  RE,3,0(R1)                                                       
         LA    R1,6(R1)            NEXT PRODUCT ENTRY                           
         BCT   R3,BLDPC140                                                      
         B     BLDPCEX                                                          
*                                                                               
BLDPCEX  B     EXXMOD                                                           
         DROP  R2                                                               
         EJECT                                                                  
* SUB-ROUTINE TO TEST FOR DEMO OVERRIDES AND TO GENERATE ELEMENTS FOR           
* THEM                                                                          
*                                                                               
* ON ENTRY   P1  BYTE 0     = COUNT OF DEMOS                                    
*                BYTES 1-3  = A(DEMO LIST)                                      
*            P2  BYTES 1-3  = A(DEMO VALUES)                                    
*            P3  BYTES 1-3  = A(GLOBAL VALUES)                                  
*            P4  BYTES 1-3  = A(UNIVERSES)                                      
* ASSUMES    DEMTYPE SET BY USER TO 'E' OR 'A'                                  
*            INPVALS CONTAINS INPUT DEMO VALUES                                 
*                                                                               
OVERDEM  NTR1                                                                   
         LM    R2,R3,0(R1)         R2=A(DEMO LIST), R3=A(DEMO VALUES)           
         L     R5,12(R1)           R5=A(UNIVERSES)                              
         MVC   SVHOMUNV,0(R5)      SAVE HOMES UNIVERSE                          
         ZIC   R0,0(R1)            GET COUNT OF DEMOS                           
         LR    RF,R0               SAVE COUNT IN RF                             
         L     RE,8(R1)            POINT RE AT GLOBAL VALUES                    
         LA    R4,INPVALS          R4 POINTS TO INPUT VALUES                    
OV100    OC    0(4,RE),0(RE)       TEST FOR NON-ZERO GLOBAL VALUE               
         BZ    *+10                                                             
         MVC   0(4,R4),0(RE)       REPLACE INPUT VALUE W GLOBAL ONE             
         LA    R4,4(R4)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,OV100                                                         
*                                                                               
         LR    R0,RF               RESTORE DEMO COUNT                           
         LA    R4,INPVALS                                                       
         MVI   DEMOEL,X'DD'        ESTIMATED OVERRIDE EL CODE                   
         MVI   USEREL,X'CD'                                                     
         CLI   DEMTYPE,C'E'                                                     
         BE    *+12                                                             
         MVI   DEMOEL,X'DE'                                                     
         MVI   USEREL,X'CE'                                                     
         SPACE                                                                  
OV200    CLC   0(4,R4),0(R3)       TEST INPUT AGAINST RECORD VALUE              
         BE    OV800                                                            
*                                                                               
         MVI   LFIL,5                                                           
         XC    DUB,DUB                                                          
         MVC   DUB+1(3),0(R2)      EXTRACT MODIFIER/CATEGORY                    
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   OV220                                                            
*        CLI   DEMOEL,X'DE'        ACTUAL                                       
*        BNE   OV220                                                            
         CLI   DUB+2,C'T'          IMPRESSION                                   
         BNE   OV220                                                            
         MVI   DUB+2,C'H'          HUNDREDS OVERRIDE                            
OV220    MVC   ELCODE,DEMOEL                                                    
         CLI   1(R2),USERMOD       TEST FOR USER DEMO                           
         BNE   OV240               NO                                           
         MVC   ELCODE,USEREL                                                    
         MVI   LFIL,8                                                           
         ZIC   R1,2(R2)            GET USER DEMO NUMBER                         
         BCTR  R1,0                                                             
         MH    R1,=H'7'            INDEX INTO USER DEMO NAMES                   
         LA    R1,USERNMS(R1)                                                   
         MVC   DUB(1),3(R2)        MODIFIER                                     
         MVC   DUB+1(7),0(R1)      USER DEMO NAME                               
*                                                                               
OV240    CLI   DEMTYPE,C'E'                                                     
         BNE   OV300                                                            
         MVI   FERN,PAKFERR                                                     
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BO    ERROR               CHANGES TO EST. DEMOS NOT ALLOWED            
         SPACE 1                                                                
OV300    GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(ELCODE,AIOAREA1),(LFIL,DUB)          
         MVC   DUB2,DUB                                                         
         CLI   1(R2),USERMOD       TEST FOR USER DEMO                           
         BE    OV360               YES                                          
         CLI   3(R2),C'T'          CHECK FOR IMPRESSION                         
         BE    OV320                                                            
         CLI   3(R2),C'R'          TEST FOR RATING                              
         BNE   OV400               NO                                           
         CLI   DEMTYPE,C'E'        TEST FOR ESTIMATED DEMOS                     
         BNE   OV320               YES                                          
         MVI   DUB2+2,C'V'         DELETE VPH OVERRIDE ALSO                     
         GOTO1 (RF),(R1),,,(LFIL,DUB2)                                          
OV320    MVI   DUB2+2,C'T'         NO-DELETE THE IMP OVERRIDE ALSO              
         GOTO1 (RF),(R1),,,(LFIL,DUB2)                                          
         MVI   DUB2+2,C'H'                                                      
         GOTO1 (RF),(R1),,,(LFIL,DUB2)                                          
         B     OV400                                                            
         SPACE 1                                                                
*--USER DEMOS                                                                   
OV360    CLI   3(R2),C'T'          CHECK FOR IMPRESSION                         
         BE    OV380                                                            
         CLI   3(R2),C'R'          TEST FOR RATING                              
         BNE   OV400               NO                                           
         CLI   DEMTYPE,C'E'        TEST FOR ESTIMATED DEMOS                     
         BNE   OV380               YES                                          
         MVI   DUB2,C'V'           DELETE VPH OVERRIDE ALSO                     
         GOTO1 (RF),(R1),,,(LFIL,DUB2)                                          
OV380    MVI   DUB2,C'T'           NO-DELETE THE IMP OVERRIDE ALSO              
         GOTO1 (RF),(R1),,,(LFIL,DUB2)                                          
         MVI   DUB2,C'H'                                                        
         GOTO1 (RF),(R1),,,(LFIL,DUB2)                                          
*                                                                               
OV400    CLC   0(4,R4),=F'-1'      TEST FOR OVERRIDE VALUE                      
         BE    OV800                                                            
*                                                                               
         CLI   2(R2),1             RATING HOMES NO CHG IF PACK. GUART.          
         BNE   OV420                                                            
         CLI   DEMTYPE,C'A'        TEST FOR ACTUAL - ALLOW CHG                  
         BE    OV420                                                            
*--SINCE WE ARE NOW SHOWING THE RAW DATA DEMOS CAN BE CHANGED                   
*--EVEN IF A PACKAGE GUARANTEE EXISTS                                           
         CLC   NBACTDAT,=XL2'B32B' SEP11/89                                     
         BNL   OV415                                                            
         MVI   FERN,PGNOSHR                                                     
         MVI   BYTE,X'B3'                                                       
         BAS   RE,GETEL                                                         
         BE    ERROR                                                            
         MVI   BYTE,X'B4'                                                       
         BAS   RE,GETEL                                                         
         BE    ERROR                                                            
OV415    MVI   FERN,0                                                           
*                                                                               
OV420    ST    R3,SAVEREG3                                                      
         XC    OVEREL,OVEREL                                                    
         LA    R3,OVEREL                                                        
         CLI   1(R2),USERMOD                                                    
         BE    OV500                                                            
*                                                                               
         USING NUOVD,R3                                                         
         MVC   NUOVEL,ELCODE                                                    
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),0(R2)                                                 
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   OV440                                                            
         CLI   NUOVMOD,C'T'                                                     
         BNE   OV440                                                            
         MVI   NUOVMOD,C'H'                                                     
OV440    BAS   RE,SETPRE           SET DEMO PRECISION                           
         MVC   BYTE,NUOVPRE        BYTE CONTAINS PRECISION                      
         CLC   0(4,R4),=F'-2'      TEST FOR ZERO VALUE                          
         BE    OV600                                                            
         MVC   NUOVVAL(4),0(R4)                                                 
         B     OV600                                                            
*                                                                               
         USING UDOVD,R3                                                         
OV500    MVC   UDOVEL,ELCODE                                                    
         MVI   UDOVLEN,14                                                       
         MVC   UDOVMOD(8),DUB                                                   
         CLC   0(4,R4),=F'-2'      TEST FOR ZERO VALUE                          
         BE    OV600                                                            
         MVC   UDOVVAL,0(R4)                                                    
*                                                                               
OV600    L     R3,SAVEREG3                                                      
         BAS   RE,PUTEL                                                         
         CLI   3(R2),C'R'          TEST FOR RATING OVERRIDE                     
         BNE   OV700                                                            
         CLI   DEMTYPE,C'E'                                                     
         BNE   *+16                                                             
         BAS   RE,VPH              ALSO GENERATE THE VPH FOR EST.               
         BAS   RE,EIMP             ALSO GENERATE THE IMP FOR EST.               
         B     *+8                                                              
         BAS   RE,CALCIMP          GENERATE THE IMP FOR ACTUALS                 
*                                                                               
OV700    L     R1,NBAIO                                                         
         USING NURECD,R1                                                        
         OI    NUACTWHY,X'08'                                                   
         SPACE                                                                  
OV800    LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,OV200                                                         
         SPACE                                                                  
OVERX    B     EXXMOD                                                           
         DROP  R1,R3                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CALCULATE ESTIMATED VPH AND TO ADD OVERRIDE                    
*                                                                               
VPH      NTR1                                                                   
         CLI   2(R2),1             HOMES RATING                                 
         BE    VPHX                                                             
         LH    R1,NBESTHOM+2       HOMES RATING                                 
         M     R0,SVHOMUNV         X HOMES UNIVERSE                             
         TM    BYTE,X'82'                                                       
         BO    VPH20                                                            
*                                                                               
         AH    R1,=H'50'                                                        
         D     R0,=F'100'                                                       
         B     VPH30                                                            
*--FOR RATINGS TO 2 DECIMALS                                                    
VPH20    AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
VPH30    LTR   R1,R1                                                            
         BZ    VPHX                EXIT IF ZERO                                 
         MVC   DEMO,0(R2)          SAVE 4-BYTE DEMO CODE                        
         L     R3,4(R5)            GET UNIVERSE VALUE                           
         L     R2,0(R4)            GET RATING VALUE                             
         MR    R2,R2               RATING X UNIVERSE                            
         M     R2,=F'10000'        SCALING                                      
         DR    R2,R1               DIVIDE BY HOMES IMP                          
         SR    R2,R2                                                            
         TM    BYTE,X'82'                                                       
         BO    VPH40                                                            
*                                                                               
         AH    R3,=H'500'          ROUND DOWN TO INTEGER PRECISION              
         D     R2,=F'1000'                                                      
         LR    R2,R3                                                            
         B     VPH50                                                            
*                                                                               
VPH40    A     R3,=F'5000'        ROUND DOWN TO INTEGER PRECISION               
         D     R2,=F'10000'                                                     
         LR    R2,R3                                                            
VPH50    XC    OVEREL,OVEREL                                                    
         LA    R3,OVEREL                                                        
         CLI   DEMO+1,USERMOD      TEST FOR USER DEMO                           
         BE    VPH100              YES                                          
*                                                                               
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),DUB2+1   MODIFIER/CATEGORY                            
         MVI   NUOVMOD,C'V'                                                     
         CLC   0(4,R4),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R2,15,NUOVVAL                                                    
         BAS   RE,SETPRE                                                        
         B     VPH120                                                           
*                                                                               
         USING UDOVD,R3                                                         
VPH100   MVI   UDOVEL,X'CD'                                                     
         MVI   UDOVLEN,14                                                       
         MVC   UDOVMOD(8),DUB2                                                  
         CLC   0(4,R4),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R2,15,UDOVVAL                                                    
*                                                                               
VPH120   BAS   RE,PUTEL                                                         
         SPACE                                                                  
VPHX     B     EXXMOD                                                           
         DROP  R3                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE THE ESTIMATE IMP AND TO ADD AN OVERRIDE              
*                                                                               
* AT ENTRY R4 = RATING OVERRIDE                                                 
*          R5 = UNIVERSE                                                        
*          BYTE = DEMO PRECISION                                                
*                                                                               
EIMP     NTR1                                                                   
         CLI   1(R2),USERMOD       TEST USER DEMOS                              
         BE    EIMPX               YES                                          
         CLI   2(R2),1             HOMES RATING                                 
         BE    EIMPX                                                            
         L     R1,4(R5)            UNIVERSE                                     
         L     R0,0(R4)            PUT RATING IN R0                             
         MR    R0,R0               RATING*UNIVERSE                              
         TM    BYTE,X'82'                                                       
         BO    EIMP20                                                           
*                                                                               
         AH    R1,=H'50'           ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'100'                                                       
         B     EIMP30                                                           
*                                                                               
EIMP20   AH    R1,=H'500'          ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'1000'                                                      
EIMP30   CLI   NBHUNOPT,C'Y'                                                    
         BNE   *+8                                                              
         MH    R1,=H'10'                                                        
*                                                                               
         XC    OVEREL,OVEREL                                                    
         LA    R3,OVEREL                                                        
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),DUB+1                                                 
         MVI   NUOVMOD,C'T'                                                     
         CLI   NBHUNOPT,C'Y'                                                    
         BNE   *+8                                                              
         MVI   NUOVMOD,C'H'                                                     
         BAS   RE,SETPRE                                                        
         CLC   0(4,R4),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R1,15,NUOVVAL                                                    
         BAS   RE,PUTEL                                                         
*                                                                               
EIMPX    B     EXXMOD                                                           
         DROP  R3                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE THE ACTUAL IMPRESSION AND TO ADD OVERRIDE            
*                                                                               
CALCIMP  NTR1                                                                   
         CLI   1(R2),USERMOD       TEST USER DEMOS                              
         BE    CALCIMPX            YES                                          
****     CLI   2(R2),1             TEST FOR HOMES                               
****     BE    CALCIMPX            YES-LET FORMULA WORK IT OUT (R*U)            
         L     R0,0(R4)            GET RATING VALUE                             
         L     R1,4(R5)            GET UNIVERSE VALUE                           
         LTR   R1,R1                                                            
         BZ    CALCIMPX                                                         
         MR    R0,R0               RATING*UNIVERSE                              
         TM    BYTE,X'82'                                                       
         BO    CIMP20                                                           
*                                                                               
         AH    R1,=H'50'           ROUND TO THOUSANDS                           
         SR    R0,R0                                                            
         D     R0,=F'100'          R1 CONTAINS IMP                              
         B     CIMP30                                                           
*                                                                               
CIMP20   AH    R1,=H'500'          ROUND TO THOUSANDS                           
         SR    R0,R0                                                            
         D     R0,=F'1000'         R1 CONTAINS IMP                              
CIMP30   CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+8                                                              
         MH    R1,=H'10'           X 10 FOR DECIMAL POINT                       
*                                                                               
         XC    OVEREL,OVEREL                                                    
         LA    R3,OVEREL                                                        
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DE'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),DUB2+1                                                
         BAS   RE,SETPRE                                                        
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+8                                                              
         MVI   NUOVMOD,C'H'                                                     
         CLC   0(4,R4),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R1,15,NUOVVAL                                                    
         BAS   RE,PUTEL                                                         
CALCIMPX B     EXXMOD                                                           
         DROP  R3                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ADD THE PRECISION FACTOR TO OVERRIDE ELEMENTS                  
* R3 POINTS TO THE ELEMENT AREA                                                 
*                                                                               
SETPRE   NTR1                                                                   
         LA    R4,DEMPREC          CONVERSION TABLE                             
         LA    RE,7                                                             
*                                                                               
SETP20   CLC   0(1,R4),4(R3)                                                    
         BE    SETP40                                                           
         LA    R4,2(R4)                                                         
         BCT   RE,SETP20                                                        
         DC    H'0'                                                             
SETP40   MVC   7(1,R3),1(R4)                                                    
         B     EXXMOD                                                           
         SPACE 2                                                                
* SUB-ROUTINE TO SAVE PACKAGE GUARANTIES AND CHANGE TO 100 PERCENT              
*                                                                               
SAVGUAR  NTR1                                                                   
         CLC   NBACTDAT,=XL2'B32B' SEP11/89                                     
         BL    SAVGUAR8                                                         
         USING NUNGUD,R3                                                        
         XC    PNGUAHLD,PNGUAHLD                                                
***      XC    PGUARHLD,PGUARHLD                                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B3',NBAIO),0                       
         CLI   12(R1),0            TEST IF PACKAGE GUARANTEE FOUND              
         BNE   SAVGUAR4            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   PNGUAHLD,NUNGUFAC                                                
         MVC   NUNGUFAC,=XL4'000F4240'                                          
***      MVC   PGUARHLD,NUGUAFAC                                                
***      MVC   NUGUAFAC,=XL2'2710'                                              
*                                                                               
SAVGUAR4 XC    DGUARHLD,DGUARHLD                                                
         XC    DNGUAHLD,DNGUAHLD                                                
*                                                                               
         USING NUNDGD,R3                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B4',NBAIO),0                       
         CLI   12(R1),0            TEST IF DEMO GUARANTEE FOUND                 
         BNE   SAVGUAR8            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   DNGUAHLD,NUNDGFAC                                                
         MVC   NUNDGFAC,=XL4'000F4240'                                          
***      MVC   DGUARHLD,NUGUAFAC                                                
***      MVC   NUGUAFAC,=XL2'2710'                                              
*                                                                               
SAVGUAR8 B     EXXMOD                                                           
         DROP  R3                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO RESTORE PACKAGE GUARANTIES                                     
*                                                                               
RESGUAR  NTR1                                                                   
         CLC   NBACTDAT,=XL2'B32B' SEP11/89                                     
         BL    RESGUAR8                                                         
         USING NUNGUD,R3                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B3',NBAIO),0                       
         CLI   12(R1),0            TEST IF PACKAGE GUARANTEE FOUND              
         BNE   RESGUAR4            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   NUNGUFAC,PNGUAHLD                                                
         MVC   NBNGUFAC,PNGUAHLD    RESTORE PACKAGE GUARANTEE                   
***      MVC   NUGUAFAC,PGUARHLD                                                
***      MVC   NBGUAFAC(2),PGUARHLD    RESTORE PACKAGE GUARANTEE                
*                                                                               
         USING NUNDGD,R3                                                        
RESGUAR4 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B4',NBAIO),0                       
         CLI   12(R1),0            TEST IF DEMO GUARANTEE FOUND                 
         BNE   RESGUAR8            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   NUNDGFAC,DNGUAHLD                                                
*                                                                               
RESGUAR8 B     EXXMOD                                                           
         DROP  R3                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO PUT AND OVERRIDE ELEMENT                                       
*                                                                               
PUTEL    ST    RE,SAVEREG                                                       
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,OVEREL,0                     
         CLI   12(R1),0                                                         
         BE    PUTELX                                                           
         CLI   12(R1),5                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FERN,TOOLARGE                                                    
         B     ERRED                                                            
         SPACE                                                                  
PUTELX   L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET ELEMENT (AT ENTRY, ELCODE CONTAINS ELEMENT CODE)           
* ON EXIT, CC=EQ IF ELEMENT FOUND, CC=NEQ IF NOT FOUND                          
*                                                                               
GETEL    ST    RE,SAVEREG                                                       
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(BYTE,AIOAREA1),0                     
         CLI   12(R1),0            SET CC ON EXIT                               
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* MODULE AND ROUTINE EXIT                                                       
*                                                                               
EXXMOD   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR    GOTO1 VERROR                                                           
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
PROTECT  DC    XL8'010A200000000000'                                            
TOOLONG  DC    C'**DATA REQUESTED CANNOT FIT ON SCREEN**'                       
NOMORE   DC    C'**NO MORE RECORDS TO DISPLAY**'                                
         SPACE 2                                                                
* DEMO MODIFIER TABLES (BYTE 0=MODIFIER, BYTE 1=LENGTH OF DATA)                 
*                                                                               
ESTMODS  DC    C'R',X'05'                                                       
         DC    C'V',X'05'                                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
ACTMODS  DC    C'R',X'05'                                                       
         DC    C'V',X'05'                                                       
         DC    C'T',X'06'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0F                                                               
         DROP  RA,RB,RC                                                         
OVFLRTN  NMOD1 0,**15OV**                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
         L     R9,8(R1)                                                         
         USING BUYWRKD,R9                                                       
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     EDDEM                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EDIT DEMO INPUT AND PLACE VALUES IN FULLWORD LIST              
* ON EXIT INPVALS CONTAINS VALUES                                               
*                                                                               
* ON ENTRY  P1  BYTE 0    = COUNT OF DEMOS                                      
*               BYTES 1-3 = A(DEMO LIST)                                        
*                                                                               
EDDEM    XC    INPVALS,INPVALS                                                  
         L     R2,12(R1)           R2 POINTS TO DEMOS                           
         ZIC   R3,12(R1)           R3 IS A COUNTER                              
         LA    R4,INPVALS                                                       
         SPACE                                                                  
ED100    XC    FTERM,FTERM                                                      
         MVC   FTERM(3),=C'*.,'                                                 
         GOTO1 AFVAL,0                                                          
         ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         MVI   FERN,INVERR                                                      
         CLI   FSTOP,STAR          TEST FOR STAR                                
         BNE   ED200                                                            
         CLI   FLDH+5,0            NOTHING CAN PRECEDE IT                       
         BNE   EDDEMR                                                           
         XC    FTERM,FTERM         SKIP OVER IT                                 
         MVC   FTERM(2),=C'.,'                                                  
         ICM   R1,7,FLAST          SET STRING POINTER TO ONE AFTER STAR         
         LA    R1,1(R1)                                                         
         STCM  R1,7,FLAST                                                       
         GOTO1 AFVAL,0                                                          
         SPACE                                                                  
ED200    CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BNE   ED300                                                            
         MVI   FERN,MISERR                                                      
         CLI   FSTOP,C'.'          ERROR UNLESS LEADING DECIMAL POINT           
         BNE   EDDEMR              FOUND                                        
         ICM   R1,7,FLAST          BUMP STRING POINTER PAST POINT               
         LA    R1,1(R1)                                                         
         STCM  R1,7,FLAST                                                       
         SR    R5,R5                                                            
         B     ED400                                                            
         SPACE                                                                  
ED300    MVI   FERN,INVERR                                                      
         CLI   FLDH+5,1                                                         
         BNE   ED320                                                            
         CLI   FLD,SLASH           TEST FOR SLASH                               
         BNE   ED320                                                            
         CLI   FSTOP,COMMA         SKIP EDIT FOR DEMO - ERASE OVERRIDE          
         BE    ED800                                                            
         CLI   FSTOP,X'FF'                                                      
         BE    ED800                                                            
         B     EDDEMR                                                           
         SPACE                                                                  
ED320    CLI   FLD,C'X'                                                         
         BNE   ED340                                                            
         CLI   FLDH+5,1                                                         
         BNE   EDDEMR                                                           
         L     R0,=F'-1'                                                        
         B     ED750                                                            
         SPACE                                                                  
ED340    TM    FLDH+4,X'08'                                                     
         BZ    EDDEMR                                                           
         CVB   R0,DUB                                                           
         CLI   3(R2),C'R'                                                       
         BE    ED370                                                            
         CLI   3(R2),C'V'                                                       
         BE    ED380                                                            
         MVI   FERN,INVERR         IF EDIT IS FOR RATING, ALLOW DECIMAL         
*--IMPRESSION SETUP                                                             
         CLI   3(R2),C'T'                                                       
         BNE   EDDEMR                                                           
         CLI   FLDH+5,5            OR 4 BEFORE POINT IF CABLE IMP               
         BH    EDDEMR                                                           
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   ED360                                                            
         MH    R0,=H'10'           SCALE TO TENTHS                              
         B     ED390                                                            
ED360    CLI   FSTOP,C'.'                                                       
         BE    EDDEMR                                                           
         B     ED390                                                            
*-RATING SETUP                                                                  
ED370    CLI   FLDH+5,3                                                         
         BH    EDDEMR                                                           
         MH    R0,=H'10'           SCALE TO TENTHS                              
         TM    DEMPREC+1,X'81'     CHECK RATING PRECISION                       
         BO    ED390                                                            
         MH    R0,=H'10'           SCALE TO HUNDREDS                            
         B     ED390                                                            
*-VPH SETUP                                                                     
ED380    CLI   FLDH+5,5                                                         
         BH    EDDEMR                                                           
         CLI   FSTOP,C'.'          DEC. NOT ALLOWED                             
         BE    EDDEMR                                                           
         B     ED750               STORE THE NUMBER                             
*                                                                               
ED390    ST    R0,0(R4)                                                         
         LR    R5,R0                                                            
         SPACE                                                                  
ED400    CLI   FSTOP,C'.'                                                       
         BNE   ED800                                                            
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
*--IMPRESSION EDIT                                                              
         CLI   3(R2),C'T'                                                       
         BNE   ED450                                                            
         CLI   FLDH+5,1                                                         
         BH    EDDEMR                                                           
         BL    ED800                                                            
         B     ED500                                                            
*--RATING EDIT                                                                  
ED450    CLI   3(R2),C'R'                                                       
         BNE   EDDEMR                                                           
         CLI   FLDH+5,0                                                         
         BE    ED800                                                            
         TM    DEMPREC+1,X'82'     CHECK RATING PRECISION                       
         BNO   ED460                                                            
         CLI   FLDH+5,2                                                         
         BH    EDDEMR                                                           
         B     ED700                                                            
ED460    CLI   FLDH+5,1                                                         
         BNE   EDDEMR                                                           
         B     ED500                                                            
*                                                                               
ED500    TM    FLDH+4,X'08'                                                     
         BZ    EDDEMR                                                           
         CVB   R0,DUB                                                           
         AR    R0,R5                                                            
         B     ED750                                                            
         SPACE                                                                  
ED700    TM    FLDH+4,X'08'                                                     
         BZ    EDDEMR                                                           
         CVB   R0,DUB                                                           
         CLI   FLDH+5,1                                                         
         BNE   *+8                                                              
         MH    R0,=H'10'           ALLIGN THE PRECISION                         
         AR    R0,R5                                                            
ED750    ST    R0,0(R4)                                                         
*                                                                               
ED800    LA    R2,4(R2)                                                         
         LA    R4,4(R4)                                                         
         BCT   R3,ED100                                                         
         SPACE                                                                  
EDDEMX   B     OVEXIT                                                           
         SPACE 3                                                                
EDDEMR   B     OVERRED                                                          
         EJECT                                                                  
* ROUTINE TO HANDLE ERROR CONDITION IN EDIT - PUT EDIT SUPPRESSION              
* CHARACTER IN ALL FIELDS BEFORE ERROR AND FORCE ANY CHANGED FIELD              
* AFTER ERROR TO BE TRANSMITTED NEXT TIME.                                      
*                                                                               
OVERRED  L     R3,FADDR            GET ERROR FIELD ADDRESS                      
         L     R2,AFSTFLD          POINT TO FIRST FIELD                         
         SPACE                                                                  
OVERRED2 SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'                                                      
         BO    *-10                                                             
         CR    R2,R3                                                            
         BNL   OVERRED6                                                         
         SPACE                                                                  
OVERRED4 ZIC   R1,0(R2)                                                         
         SH    R1,=H'9'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         MVI   8(R2),PLUS                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   9(0,R2),WORK                                                     
         B     OVERRED2                                                         
         SPACE                                                                  
OVERRED6 LR    R2,R3               POINT R2 AT ERROR FIELD                      
         L     R3,ALSTFLD          POINT R3 AT END OF SCREEN                    
         SR    R0,R0                                                            
*                                                                               
OVERRED7 IC    R0,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R0                                                            
         CR    R2,R3               TEST FOR END OF SCREEN                       
         BNL   OVERREDX                                                         
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    OVERRED7            YES                                          
         TM    4(R2),X'80'         TEST IF SENT THIS TIME                       
         BZ    OVERRED7            NO-MEANS NO CHANGES                          
         OI    6(R2),X'81'         YES-XMIT BACK AS MODIFIED NEXT TIME          
         B     OVERRED7            TO FORCE FIELD TO BE EDITED                  
         SPACE                                                                  
OVERREDX B     ERROR3                                                           
         SPACE 2                                                                
ERROR3   GOTO1 VERROR                                                           
         SPACE 2                                                                
OVEXIT   XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTION FIELD                                              
*                                                                               
         DS    0F                                                               
         DROP  RA,RB                                                            
ACTED    NMOD1 0,**ACTE**                                                       
         LA    RC,2048(RB)         RC IS THIRD BASE REGISTER                    
         LA    RC,2048(RC)                                                      
         USING ACTED+4096,RC                                                    
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         XC    OTHERSW,OTHERSW                                                  
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST         FIND THE ACTION FIRST                        
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,COMMA         TEST IF COMMA FOLLOWED ACTION                
         BE    ACTED2              YES                                          
         GOTO1 ADDKEY,DELETE       ONLY ACTION CODE IN FIELD MEANS DEL          
         B     ACTEDX                                                           
         SPACE                                                                  
ACTED2   MVI   FNDX,2              LOOK AT SECOND FIELD                         
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BNE   *+18                                                             
         MVI   FERN,MISERR                                                      
         MVC   XTRA(13),=C'DATA KEYWORDS'                                       
         B     ERROR2                                                           
*                                                                               
         CLC   FLD(5),=C'ALLOC'    TEST FOR 'ALLOC' SPECIAL                     
         BNE   ACTED3              NO                                           
         CLI   FLDH+5,5            TEST FOR LENGTH OF 5                         
         BE    ACTED5              YES-ONLY KEYWORD                             
         CLI   FLD+5,EQUAL         TEST FOR EQUALS SIGN                         
         BNE   ACTEDR              NO                                           
         BAS   RE,ALLOCVAL         VALIDATE ALLOCATION GLOBAL VALUES            
         B     ACTED5                                                           
         SPACE                                                                  
ACTED3   CLC   FLD(7),=C'UNALLOC'                                               
         BNE   ACTED6                                                           
         CLI   FLDH+5,7            TEST FOR KEYWORD ONLY                        
         BE    ACTED4              YES                                          
         CLI   FLD+7,EQUAL         TEST FOR UNALLOC=                            
         BNE   ACTEDR              NO-MUST BE AN ERROR                          
         ZIC   R1,FLDH+5           TEST FOR UNALLOC=P(RODUCT)                   
         SH    R1,=H'9'                                                         
         BM    ACTEDR              NOTHING AFTER EQUALS SIGN                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD+8(0),=C'PRODUCT'                                             
         BNE   ACTEDR              NO-MUST BE ERROR                             
         SPACE                                                                  
ACTED4   MVI   UNALLOC,YES         SET UNALLOCATE SWITCH                        
         MVI   GLOBSW,YES          UNALLOC AFFECTS EVERY UNIT                   
         SPACE                                                                  
ACTED5   MVI   ALLOCSW,YES         SET ALLOCATE SWITCH                          
         MVC   KEYH,PRDFLDH        SET PRODUCT GLOBAL VALUES IF ANY             
         MVC   KEYVAL,PRDVAL                                                    
         GOTO1 ADDKEY,PRODUCT                                                   
         CLI   UNALLOC,YES         TEST FOR UNALLOCATION                        
         BNE   *+12                NO                                           
         CLI   FLD+8,C'P'          TEST FOR UNALLOCATING BRANDS ONLY            
         BE    ACTED5X             YES                                          
         MVC   KEYH,ACTFLDH        SET ACTUAL COST GLOBAL VALUES                
         MVC   KEYVAL,ACTVAL                                                    
         GOTO1 (RF),ACTUAL                                                      
*                                                                               
ACTED5X  B     ACTED50             SET TO EDIT NEXT FIELD                       
         SPACE                                                                  
ACTED6   CLI   FLDH+5,5                                                         
         BL    ACTED8                                                           
         CLI   FLDH+5,7                                                         
         BH    ACTED8                                                           
         CLC   FLD(4),=C'EST='     TEST FOR ESTIMATE RE LOOK-UP                 
         BNE   ACTED8                                                           
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'5'                                                         
         EX    R1,YESCOMP                                                       
         BNE   ACTEDR                                                           
         MVI   FERN,PAKFERR                                                     
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BO    ERROR2                                                           
         MVI   ESTLOOK,YES         FORCE ESTIMATE RE LOOK-UP                    
         MVI   GLOBSW,YES                                                       
         GOTO1 ADDKEY,ESTLK                                                     
         B     ACTED50                                                          
         DROP  RE                                                               
         SPACE                                                                  
ACTED8   CLI   FLDH+5,4                                                         
         BL    ACTED9                                                           
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,FROZCOMP         TEST FOR FROZEN OVERIDE                      
         BNE   ACTED9                                                           
         MVI   BUYUNFR,C'Y'                                                     
         B     ACTED50                                                          
         SPACE                                                                  
ACTED9   MVI   FLEN,0              NOT A SPECIAL KEYWORD                        
         MVI   FNDX,1                                                           
         B     ACTED10             SET TO RE-EDIT SECOND FIELD                  
         SPACE 2                                                                
YESCOMP  CLC   FLD+4(0),=C'YES'                                                 
FROZCOMP CLC   FLD(0),=C'UNFROZEN'                                              
         SPACE 2                                                                
* PROCESS REMAINING KEY AND FILTER FIELDS IN LOOP                               
*                                                                               
ACTED10  XC    FTERM,FTERM                                                      
         MVC   FTERM(2),=C'=,'                                                  
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BNE   ACTED12                                                          
         CLI   FSTOP,X'FF'         TEST FOR END OF FIELD                        
         BE    ACTEDX              YES-EXIT                                     
         CLI   FNDX,1              TEST FOR RE-EDIT OF SECOND FIELD             
         BNE   *+8                 NO                                           
         MVI   FNDX,2              YES-SET INDEX TO TWO                         
         B     ACTEDR              LONE TERMINATOR - ERROR                      
         SPACE                                                                  
ACTED12  ZIC   RE,FNDX                                                          
         LA    RE,1(RE)                                                         
         STC   RE,FNDX                                                          
         SPACE                                                                  
*                                                                               
*  EDIT FOR REASON CODE                                                         
         MVI   FERN,INVERR                                                      
         CLC   FLD(2),=C'RS'       CHECK REASON CODE                            
         BNE   ACTED14                                                          
         XC    FTERM,FTERM         NOW LOOK FOR A GLOBAL VALUE                  
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR SOMETHING AFTER EQUALS              
         BE    ERROR2                                                           
         CLI   FLDH+5,4                                                         
         BH    ERROR2                                                           
         MVC   AUDREASN,FLD                                                     
         OC    AUDREASN,SPACES                                                  
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         B     ACTED10                                                          
         SPACE 2                                                                
ACTED14  CLI   FLDH+5,L'KEYNAME    TEST IF MORE CHARS THAN A KEYWORD            
         BH    ACTED25             YES-TRY DEMO/FILTER                          
         LA    R0,KEYS             COUNTER                                      
         LA    R3,KEYTAB                                                        
         USING KEYTABD,R3                                                       
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
ACTED15  CLC   FLDH+5(1),KEYMINL                                                
         BL    ACTED16                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),KEYNAME                                                   
         BE    ACTED18                                                          
ACTED16  LA    R3,KEYTABL(R3)                                                   
         BCT   R0,ACTED15                                                       
         B     ACTED25             NOT A KEYWORD, TRY DEMO/FILTER               
         SPACE                                                                  
ACTED18  MVI   WORK,60                                                          
         MVC   WORK+1(10),KEYNAME                                               
         GOTO1 VDATAMGR,DMCB,=C'DMTRACE',=C'DATA',WORK                          
         TM    KEYCTL,INTERNAL                                                  
         BO    ACTEDR              NO KEYS FOR DELETE                           
         BAS   RE,SEQOTH                                                        
         BNZ   ACTEDR                                                           
         CLC   KEYNAME(5),=CL5'OTHER'                                           
         BNE   ACTED18B                                                         
         BAS   RE,VALOTH                                                        
         BZ    ACTED10                                                          
         BM    ACTEDR                                                           
ACTED18B CLI   FSTOP,EQUAL         TEST FOR EQUALS SIGN                         
         BNE   ACTED20             NO-ONLY A KEY                                
*                                                                               
         XC    FTERM,FTERM         NOW LOOK FOR A GLOBAL VALUE                  
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR SOMETHING AFTER EQUALS              
         BE    ACTEDR              NO                                           
         TM    KEYCTL,DISONLY      NO VALUES FOR DISPLAY ONLY KEYS              
         BO    ACTEDR                                                           
         TM    NBUNST3,X'40'                                                    
         BZ    ACTED18D                                                         
         TM    KEYCTL,COPSDIS      TEST FOR COPY SPLIT DISPLAY                  
         BO    ACTEDR              YES-SKIP TO NEXT KEYWORD                     
*                                                                               
ACTED18D MVC   KEYH,FLDH                                                        
         MVC   KEYVAL,FLD          SAVE KEYWORD GLOBAL VALUE                    
         MVI   GLOBSW,YES                                                       
*                                                                               
         CLI   KEYNUM,DATE         TEST FOR DATE                                
         BNE   ACTED19                                                          
*                                                                               
         GOTO1 VDATVAL,DMCB,(1,FLD),DUB                                         
         MVI   FERN,DATERR                                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR2              NO ERROR                                     
         CLC   FLDH+5(1),3(R1)     TEST DATE IS ENTIRE VALUE                    
         BE    ACTED20                                                          
         B     ERROR2                                                           
*                                                                               
ACTED19  LA    R4,BLOCK            CHECK THE SYNTAX OF THE VALUE                
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNALOCAL,AIOAREA4                                                
         MVC   UNEDATA,KEYDATA                                                  
         GOTO1 VEDIT,DMCB,(C'S',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    ACTED20                                                          
         MVC   FERN,UNERROR                                                     
         B     ERROR2                                                           
         SPACE                                                                  
ACTED20  GOTO1 ADDKEY,KEYTABD                                                   
         CLI   ALLOCSW,YES                                                      
         BE    ACTEDR                                                           
         CLI   ESTLOOK,YES                                                      
         BE    ACTEDR                                                           
         OC    ESTDEMOS(2),ESTDEMOS KEYS MUST PRECEDE DEMOS AND                 
         BNZ   ACTEDR              FILTERS                                      
         CLI   FILTERS,0                                                        
         BNE   ACTEDR                                                           
         B     ACTED50             DO NEXT FIELD                                
         SPACE                                                                  
* VALIDATE DEMO EXPRESSION                                                      
*                                                                               
* E.G. E=DEMO=NNN/DEMO/DEMO=NNN, ETC.                                           
*                                                                               
ACTED25  CLI   FSTOP,EQUAL         TEST FOR EQUALS SIGN                         
         BNE   ACTED30             NO TRY FILTER                                
         CLI   FLDH+5,1                                                         
         BNE   ACTED30                                                          
         CLI   FLD,C'A'            TEST FOR ACTUALS                             
         BE    ACTED26             YES                                          
         CLI   FLD,C'E'            TEST FOR ESTIMATED                           
         BNE   ACTED30             NO-TRY FILTERS                               
         CLI   ACTDEMOS,0          YES-MAKE SURE THEY PRECEDE ACTUALS           
         BNE   ACTEDR                                                           
         SPACE                                                                  
ACTED26  MVC   DEMTYPE,FLD         SET TYPE OF DEMOS TO BE VALIDATED            
         CLI   FILTERS,0           TEST DEMOS PRECEDE FILTERS                   
         BNE   ACTEDR              NO                                           
         CLI   ALLOCSW,YES         AND ARE NOT USED                             
         BE    ACTEDR                                                           
         CLI   ESTLOOK,YES                                                      
         BE    ACTEDR                                                           
         SPACE                                                                  
ACTED27  XC    FTERM,FTERM                                                      
         MVC   FTERM(3),=C'=/,'                                                 
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BNE   ACTED28                                                          
         LA    RE,ESTDEMOS         POINT AT ESTIMATED DEMO COUNTER              
         CLI   DEMTYPE,C'E'        TEST FOR EST.                                
         BE    *+8                                                              
         LA    RE,ACTDEMOS                                                      
         CLI   0(RE),0             TEST THAT AT LEAST ONE DEMO PRESENT          
         BE    ACTEDR              NO                                           
         B     ACTED50                                                          
         SPACE                                                                  
ACTED28  LA    RE,DBLOCKA                                                       
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'EVN'      SET FOR EVN-ESTIMATED                        
         MVI   DBSELMED,C'V'                                                    
         CLI   DEMTYPE,C'E'        TEST FOR EST.                                
         BE    *+14                                                             
         MVC   DBFILE,=C'NTI'      NO-SET FOR NTI-ACTUALS                       
         MVI   DBSELMED,C'N'                                                    
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CDEMOVAL,DMCB,FLDH,(1,FULL),DBLOCKA                              
         MVC   DEMO,FULL           SAVE DEMO CODE                               
         MVC   DEMO+3(1),DEMO+1    AND SET DEMO+3=MODIFIER                      
         CLI   4(R1),1             TEST FOR 1 VALID DEMO                        
         BE    *+12                YES                                          
         BAS   RE,USRED            EDIT FOR A USER DEMO                         
         BNE   ACTEDR                                                           
*                                                                               
         CLI   FSTOP,EQUAL         TEST FOR EQUALS AFTER DEMO                   
         BNE   ACTED29                                                          
*                                                                               
         XC    FTERM,FTERM         GET GLOBAL VALUE                             
         MVC   FTERM(2),=C'/,'                                                  
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR SOMETHING                           
         BE    ACTEDR              NO                                           
         MVI   WORK+50,0                                                        
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   AC700                                                            
         CLI   DEMO+3,C'T'         TEST FOR IMP                                 
         BE    *+12                                                             
AC700    CLI   DEMO+3,C'R'         TEST FOR RATING                              
         BNE   *+8                                                              
         MVI   WORK+50,C'Y'                                                     
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,.'                              
         CLI   4(R1),0             TEST FOR SCAN ERROR                          
         BE    ACTEDR                                                           
         CLI   WORK,5              TEST FOR MAX LEN                             
         BH    ACTEDR                                                           
         CLI   DEMO+3,C'R'         TEST FOR RATING                              
         BNE   AC720                                                            
         L     RE,WORK+4                                                        
         C     RE,=F'100'          YES-NO MORE THAN 100                         
         BH    ACTEDR                                                           
AC720    TM    WORK+2,X'80'        TEST FOR NUMERIC DATA                        
         BZ    ACTEDR              NO                                           
         L     R0,WORK+4                                                        
*--CHECK FOR TWO DECIMAL RATING PRECISION                                       
         CLI   DEMO+3,C'R'                                                      
         BNE   AC740                                                            
         TM    DEMPREC+1,X'82'                                                  
         BO    AC760                                                            
*-ONE DECIMAL EDIT                                                              
AC740    CLI   WORK+50,C'Y'        TEST FOR RATING/CABLE IMP                    
         BNE   *+8                                                              
         MH    R0,=H'10'           YES-SCALE VALUE                              
         CLI   WORK+1,0            TEST FOR SECOND FIELD                        
         BE    ACTED28A                                                         
         CLI   WORK+1,1            ONLY ONE DECIMAL PLACE                       
         BNE   ACTEDR                                                           
         TM    WORK+3,X'80'                                                     
         BZ    ACTEDR                                                           
         CLI   WORK+50,C'Y'        AND ONLY FOR RATING/CABLE IMP                
         BNE   ACTEDR                                                           
         A     R0,WORK+8                                                        
         B     ACTED28A                                                         
*--TWO DECIMAL EDIT                                                             
AC760    CLI   WORK+50,C'Y'        TEST FOR RATING/CABLE IMP                    
         BNE   *+8                                                              
         MH    R0,=H'100'          YES-SCALE VALUE                              
         CLI   WORK+1,0            TEST FOR SECOND FIELD                        
         BE    ACTED28A                                                         
         CLI   WORK+1,2            ONLY TWO DECIMAL PLACE                       
         BH    ACTEDR                                                           
         TM    WORK+3,X'80'                                                     
         BZ    ACTEDR                                                           
         CLI   WORK+50,C'Y'        AND ONLY FOR RATING/CABLE IMP                
         BNE   ACTEDR                                                           
         L     RF,WORK+8                                                        
         CLI   WORK+1,1                                                         
         BNE   *+8                                                              
         MH    RF,=H'10'           SCALE THE VALUE                              
         AR    R0,RF                                                            
*                                                                               
ACTED28A LTR   R0,R0                                                            
         BNZ   *+8                                                              
         L     R0,=F'-2'                                                        
         ST    R0,DEMGLOB                                                       
         MVI   GLOBSW,YES                                                       
         SPACE                                                                  
ACTED29  BAS   RE,DEMADD           UPDATE DEMO LIST                             
         CLI   FSTOP,SLASH         TEST FOR SLASH                               
         BE    ACTED27             YES-GO BACK FOR ANOTHER DEMO                 
         B     ACTED50             NO-RESUME EDIT                               
         DROP  R4,RE                                                            
         SPACE                                                                  
* FILTER PROCESSING                                                             
*                                                                               
ACTED30  XC    FTERM,FTERM         RE-EDIT FIELD                                
         MVI   FTERM,COMMA                                                      
         MVI   FLEN,0                                                           
         GOTO1 AFVAL,0                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ACTEDR                                                           
         CLI   WORK,0              TEST FOR SOLE COMMA                          
         BE    ACTEDR              YES-ERROR                                    
         CLI   WORK+1,0            TEST FOR SECOND FIELD                        
         BE    ACTED40             NO-LOOK FOR A DATE                           
*                                                                               
         LA    R3,FILTAB                                                        
         USING FILTABD,R3                                                       
         LA    R0,FILENTS          COUNTER                                      
ACTED31  CLC   FILKEYL,WORK        TEST FOR MATCH ON LENGTH                     
         BNE   *+14                                                             
         CLC   FILKEY,WORK+12                                                   
         BE    *+16                FOUND VALID FILTER KEYWORD                   
         LA    R3,FILTABL(R3)                                                   
         BCT   R0,ACTED31                                                       
         B     ACTEDR              INVALID FILTER                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,FILEDIT        ROUTINE TO EDIT FILTER VALUE                 
         GOTO1 (RF),RR=MYRELO                                                   
         BNE   ACTEDR                                                           
         GOTO1 FILTADD,FILTABD                                                  
         B     ACTED50             ALL DONE                                     
         SPACE                                                                  
* EDIT FOR A DATE EXPRESSION                                                    
*                                                                               
ACTED40  GOTO1 VDATVAL,DMCB,WORK+12,DUB                                         
         ICM   R4,15,0(R1)                                                      
         BZ    ACTED42             NOT A VALID MMDDYY                           
         MVC   DUB2,DUB            SET END DATE=START DATE                      
         CLM   R4,1,WORK           DOES DATE MAKE UP ALL OF SECOND FLD          
         BE    ACTED44             YES                                          
         LA    R2,WORK+12(R4)      POINT TO END OF DATE                         
         CLI   0(R2),DASH          TEST FOR A DASH                              
         BNE   ACTEDR              ERROR IF NOT THERE                           
         LA    R4,1(R4)            INCREMENT LENGTH FOR DASH                    
         CLM   R4,1,WORK           TEST FOR SEOMTHING AFTER DASH                
         BE    ACTEDR              NO-ITS AND ERROR                             
         LA    R2,1(R2)                                                         
         GOTO1 (RF),(R1),(R2),DUB2                                              
         ICM   RE,15,0(R1)                                                      
         BZ    ACTEDR              ERROR                                        
         LA    R4,0(RE,R4)                                                      
         CLM   R4,1,WORK                                                        
         BE    ACTED44             OK-ADD FILTER ENTRY                          
         B     ACTEDR              ERROR                                        
         SPACE                                                                  
* MONTH/YEAR DATE EDIT                                                          
*                                                                               
ACTED42  GOTO1 VDATVAL,DMCB,(2,WORK+12),DUB                                     
         ICM   RE,15,0(R1)                                                      
         BZ    ACTEDR                                                           
         CLM   RE,1,WORK                                                        
         BNE   ACTEDR                                                           
         CLC   DUB(2),=C'75'       TEST YEAR AGAINST 75                         
         BL    ACTEDR                                                           
         MVC   DUB+4(2),=C'01'     SET START DATE OF MONTH                      
         PACK  DUB2,DUB+2(2)       MONTH                                        
         CVB   R1,DUB2                                                          
         BCTR  R1,0                                                             
         SLL   R1,1                                                             
         LA    R1,MONTAB(R1)                                                    
         MVC   DUB2+4(2),0(R1)                                                  
         MVC   DUB2(4),DUB                                                      
         CLC   DUB(4),ESTSTART     TEST FOR EST START MMYY                      
         BNE   *+10                                                             
         MVC   DUB+4(2),ESTSTART+4 YES-USE DD FROM EST START                    
         CLC   DUB2(4),ESTEND      TEST FOR EST END MMYY                        
         BNE   *+10                                                             
         MVC   DUB2+4(2),ESTEND+4  YES-USE DAY FROM EST END                     
         B     ACTED44             ADD ENTRY                                    
         SPACE                                                                  
* CHECK FILTER DATES AGAINST ESTIMATE RANGE                                     
*                                                                               
ACTED44  OC    FILSTART,FILSTART                                                
         BZ    *+14                                                             
         MVC   XTRA(16),=C'DUPLICATE FILTER'                                    
         B     ACTEDR                                                           
         MVC   FILSTART,DUB                                                     
         MVC   FILEND,DUB2                                                      
         MVI   FERN,STERR                                                       
         CLC   FILSTART,ESTSTART                                                
         BL    ERROR2                                                           
         MVI   FERN,ENDERR                                                      
         CLC   FILEND,ESTEND                                                    
         BH    ERROR2                                                           
         MVI   FERN,SEQERR                                                      
         CLC   FILSTART,FILEND                                                  
         BH    ERROR2                                                           
         B     ACTED45                                                          
         SPACE                                                                  
ACTED45  GOTO1 FILTADD,FILTDATE                                                 
         B     ACTED50             ALL DONE                                     
         SPACE                                                                  
ACTEDR   MVI   FERN,INVERR                                                      
         B     ERROR2                                                           
         SPACE 2                                                                
* GO BACK TO EDIT NEXT FIELD                                                    
*                                                                               
ACTED50  B     ACTED10                                                          
         SPACE                                                                  
* ROUTINE EXIT                                                                  
*                                                                               
ACTEDX   CLI   BUYACT,C'D'         IS ACTION DISPLAY                            
         BE    ACTEDX30            DONT CHECK REASON CODE                       
         MVI   FERN,AUDITERR                                                    
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'      IS REASON CODE REQUIRED                      
         BO    ACTEDX10            YES                                          
         OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ACTEDX30                                                         
         B     ERROR2                                                           
ACTEDX10 OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ERROR2                                                           
*                                                                               
ACTEDX30 MVI   FNDX,0                                                           
         CLI   KEYCOUNT,0                                                       
         BNE   EXXMOD2                                                          
         OC    ESTDEMOS(2),ESTDEMOS                                             
         BNZ   EXXMOD2                                                          
         GOTO1 ADDKEY,DELETE                                                    
EXXMOD2  XMOD1 1                                                                
         DROP  R3                                                               
         SPACE 3                                                                
*--ROUTINE TO CHECK "OTHER" OPTIONS                                             
*                                                                               
VALOTH   NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(60),FADDR                                                   
         MVI   OTHERSW,C'Y'                                                     
*                                                                               
         MVC   FTERM(2),=C'=,'                                                  
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    VALOTERR                                                         
         CLI   FSTOP,C'='                                                       
         BE    VALOTGLB                                                         
         B     VALOTSNG                                                         
*                                                                               
VALOTERR SR    RF,RF                                                            
         BCTR  RE,0                                                             
         B     VALOTEX                                                          
*                                                                               
VALOTSNG SR    RF,RF                                                            
         B     VALOTEX                                                          
*                                                                               
VALOTGLB LA    RF,1                                                             
*                                                                               
VALOTEX  MVC   FADDR(60),WORK                                                   
         XC    WORK,WORK                                                        
         LTR   RF,RF                                                            
         B     EXXMOD2                                                          
         SPACE 3                                                                
*--ROUTINE TO CHECK "OTHER" OPTIONS                                             
*                                                                               
SEQOTH   NTR1                                                                   
         SR    RF,RF                                                            
         CLI   OTHERSW,C'Y'                                                     
         BE    SEQOTEX                                                          
         USING KEYTABD,R3                                                       
         LA    RE,OTHERS                                                        
         LA    RF,7                                                             
*                                                                               
SEQOT20  CLC   0(7,RE),KEYNAME                                                  
         BE    SEQOTBAD                                                         
         LA    RE,KEYTABL(RE)                                                   
         BCT   RF,SEQOT20                                                       
*                                                                               
SEQOTBAD LTR   RF,RF                                                            
         B     EXXMOD2                                                          
*                                                                               
SEQOTEX  MVI   OTHERSW,C'N'                                                     
         LTR   RF,RF                                                            
         B     EXXMOD2                                                          
         DROP  R3                                                               
*                                                                               
         EJECT                                                                  
* SUB-ROUTINE TO ADD KEYWORD ENTRIES TO INTERNAL STACK                          
* AT ENTRY, R1 POINTS TO TABLE ENTRY                                            
*                                                                               
ADDKEY   NTR1                                                                   
         USING KEYTABD,R1                                                       
         LA    RE,KEYSTACK                                                      
ADDKEY1  OC    0(KEYENTL,RE),0(RE)      TEST FOR VACANT POSITION                
         BZ    ADDKEY3                                                          
         CLC   KEYTABD(KEYTABL),0(RE)   TEST FOR DUPLICATE ENTRY                
         BE    ADDKEY2                                                          
         CLC   KEYEXCP,KEYNUM-KEYTABD(RE)                                       
         BE    ADDKEY2A                                                         
         LA    RE,KEYENTL(RE)                                                   
         B     ADDKEY1                                                          
         SPACE                                                                  
ADDKEY2  MVC   XTRA(17),=C'DUPLICATE KEYWORD'                                   
ADDKEY2A MVI   FERN,INVERR                                                      
         B     ERROR2                                                           
         SPACE                                                                  
ADDKEY3  MVC   0(KEYTABL,RE),KEYTABD                                            
         MVC   KEYFLDH-KEYTABD(L'KEYFLDH,RE),KEYH                               
         MVC   KEYVALUE-KEYTABD(L'KEYVALUE,RE),KEYVAL SET GLOBAL VALUE          
         ZIC   RF,KEYCOUNT                                                      
         LR    RE,RF                                                            
         LA    RF,1(RF)                                                         
         CH    RF,=H'25'                                                        
         BH    ADDKEY4                                                          
         STC   RF,KEYCOUNT                                                      
         LA    RF,KEYLIST(RE)                                                   
         MVC   0(1,RF),KEYNUM      SAVE KEY NUMBER                              
         XC    KEYH,KEYH           CLEAR GLOBAL FIELD HEADER                    
         XC    KEYVAL,KEYVAL       CLEAR GLOBAL VALUE FIELD                     
         B     EXXMOD2                                                          
ADDKEY4  MVC   XTRA(16),=C'OVER 25 KEYWORDS'                                    
         MVI   FERN,INVERR                                                      
         B     ERROR2                                                           
         DROP  R1                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO MAINTAIN DEMO LISTS                                            
*                                                                               
* AT ENTRY, DEMO CONTAINS DEMO CODE                                             
*           DEMGLOB CONTAINS GLOBAL VALUE OR ZERO                               
*           DEMTYPE HAS BEEN SET IN ACTED25 CODE TO 'E' OR 'A'                  
*                                                                               
DEMADD   NTR1                                                                   
         LA    R2,ESTDEMOS         SET UP POINTERS FOR ESTIMATED DEMOS          
         LA    R3,ESTLIST                                                       
         LA    R4,ESTGLOB                                                       
         LA    RE,ESTMODS2                                                      
         CLI   DEMTYPE,C'E'                                                     
         BE    DEMADD1                                                          
         LA    R2,ACTDEMOS                                                      
         LA    R3,ACTLIST                                                       
         LA    R4,ACTGLOB                                                       
         LA    RE,ACTMODS2                                                      
         SPACE                                                                  
DEMADD1  CLI   0(RE),X'FF'         TEST FOR EOL                                 
         BE    DEMADD1R            YES-INVALID MODIFIER                         
         CLC   DEMO+3(1),0(RE)     DEMO VS. LIST                                
         BE    DEMADD2             VALID                                        
         LA    RE,2(RE)            TRY NEXT ONE                                 
         B     DEMADD1                                                          
*                                                                               
DEMADD1R MVC   XTRA(13),=C'DEMO MODIFIER'                                       
         B     DEMERR                                                           
*--HOMES IMPRESSIONS CAN ONLY BE CHANGED FOR CABLE NETWORK                      
DEMADD2  CLI   NBPOSTYP,C'C'                                                    
         BE    DEMADD2A                                                         
         CLI   ACTION,CM                                                        
         BNE   DEMADD2A                                                         
         CLC   DEMO+1(2),=X'E301'                                               
         BE    DEMADD2R                                                         
         SPACE 1                                                                
DEMADD2A CLI   DEMO+1,USERMOD      TEST FOR USER DEMO                           
         BE    DEMADD3                                                          
         LA    RE,ESTDEMS                                                       
         ZIC   R0,ESTNDEMS                                                      
         CLC   DEMO(1),0(RE)       TEST IF NAD CATEGORY EQUAL                   
         BNE   DEMADD2B                                                         
         CLC   DEMO+2(1),2(RE)     TEST IF CATEGORY IN EST. HEADER              
         BE    DEMADD3                                                          
DEMADD2B LA    RE,3(RE)                                                         
         BCT   R0,*-24                                                          
*                                                                               
         MVC   XTRA(19),=C'NOT IN EST./NAD HDR'                                 
         B     DEMERR                                                           
DEMADD2R MVC   XTRA(13),=C'DEMO CATEGORY'                                       
         B     DEMERR                                                           
         SPACE 1                                                                
DEMADD3  OC    0(4,R3),0(R3)       DUPLICATE DEMO CHECK                         
         BZ    DEMADD4                                                          
         CLC   DEMO,0(R3)          TEST FOR DUPLICATION                         
         BE    *+12                                                             
         LA    R3,4(R3)                                                         
         B     DEMADD3                                                          
*                                                                               
         MVC   XTRA(14),=C'DUPLICATE DEMO'                                      
         B     DEMERR                                                           
         SPACE                                                                  
DEMADD4  MVC   0(L'DEMO,R3),DEMO   SEED DEMO INTO LIST                          
         ZIC   R1,0(R2)                                                         
         LR    RE,R1                                                            
         SLL   RE,2                INDEX INTO GLOBAL VALUE LIST                 
         LA    R4,0(RE,R4)                                                      
         MVC   0(4,R4),DEMGLOB                                                  
         LA    R1,1(R1)            INCREMENT DEMO COUNTER                       
         STC   R1,0(R2)                                                         
         XC    DEMGLOB,DEMGLOB                                                  
         B     DEMADDX                                                          
         SPACE                                                                  
DEMERR   MVI   FERN,INVERR                                                      
         B     ERROR2                                                           
         SPACE                                                                  
DEMADDX  B     EXXMOD2                                                          
         SPACE 2                                                                
* SUB-ROUTINE TO ADD A FILTER ENTRY - AT ENTRY, R1 POINTS TO FILTER             
* TABLE ENTRY                                                                   
*                                                                               
FILTADD  NTR1                                                                   
         LR    R3,R1               A(FILTER ENTRY)                              
         USING FILTABD,R3                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(1),FILNUM                                                   
         ICM   RE,7,FILWORK                                                     
         LA    RE,TEMPD(RE)                                                     
         ZIC   R1,FILINTL                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+2(0),0(RE)     FILTER VALUES                                
         LA    R1,3(R1)                                                         
         STC   R1,WORK+1           ENTRY LENGTH                                 
         SPACE                                                                  
FILTADD2 SR    R0,R0                                                            
         LA    R2,FILSTACK                                                      
FILTADD3 CLI   0(R2),0             TEST FOR E-O-L                               
         BE    FILTADD5                                                         
         CLC   FILNUM,0(R2)        TEST FOR DUPLICATE ENTRIES                   
         BE    FILTADD4                                                         
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         B     FILTADD3                                                         
         SPACE                                                                  
FILTADD4 MVI   FERN,INVERR                                                      
         MVC   XTRA(16),=C'DUPLICATE FILTER'                                    
         B     ERROR2                                                           
         SPACE                                                                  
FILTADD5 BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK        ADD ENTRY TO STACK                           
         ZIC   R1,FILTERS                                                       
         LA    R1,1(R1)                                                         
         STC   R1,FILTERS                                                       
         B     EXXMOD2                                                          
         DROP  R3                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO SCAN FOR ALLOCATION GLOBAL VALUES                              
*                                                                               
ALLOCVAL NTR1                                                                   
         XC    FTERM,FTERM                                                      
         MVI   FTERM,EQUAL         BUMP TO EQUALS                               
         MVI   FLEN,0              RE-EDIT FIELD                                
         GOTO1 AFVAL,0                                                          
         XC    FTERM,FTERM                                                      
         MVI   FTERM,SLASH                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,SLASH         TEST IF SLASH FOUND                          
         BNE   ACTEDR                                                           
         CLI   FLDH+5,0                                                         
         BE    ACTEDR                                                           
         SPACE                                                                  
ALLOCV2  MVC   PRDFLDH,FLDH        SAVE PRODUCT VALUES                          
         MVC   PRDVAL,FLD                                                       
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNALOCAL,AIOAREA4                                                
         MVI   UNEDATA,UPRD                                                     
         GOTO1 VEDIT,DMCB,(C'S',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    ALLOCV4                                                          
         MVC   FERN,UNERROR                                                     
         B     ERROR2                                                           
         SPACE                                                                  
ALLOCV4  XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0                                                         
         BE    ACTEDR                                                           
         MVC   ACTFLDH,FLDH                                                     
         MVC   ACTVAL,FLD                                                       
         MVI   UNEDATA,UACT                                                     
         GOTO1 VEDIT,DMCB,(C'S',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    ALLOCV6                                                          
         MVC   FERN,UNERROR                                                     
         B     ERROR2                                                           
         SPACE                                                                  
ALLOCV6  MVI   GLOBSW,YES          GLOBAL VALUE HAS BEEN INPUT                  
         B     EXXMOD2                                                          
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT USER DEMO EXPRESSION - CALLED FROM ACTED28                
*                                                                               
* AT ENTRY, FLD CONTAINS A POTENTIAL USER DEMO EXPRESSION                       
* CODE RE-EDITS FLD FROM LEFT TO RIGHT LOOKING FOR                              
*     MODIFIER(USER DEMO NAME)  OR  (USER DEMO NAME)                            
*                                                                               
* AT EXIT, CC=EQ FOR VALID EXPRESSION, CC=NEQ FOR INVALID                       
*          DEMO CONTAINS 3-BYTE DEMO NAME PLUS MODIFIER                         
*                                                                               
USRED    NTR1                                                                   
         XC    DEMO,DEMO                                                        
         XC    FTERM,FTERM                                                      
         MVI   FTERM,LPAREN        SEARCH FOR LEFT PARENTHESIS                  
         MVI   FLEN,0              START EDIT OVER AGAIN                        
         GOTO1 AFVAL,0                                                          
*                                                                               
         CLI   FSTOP,LPAREN        TEST IF '9' FOUND                            
         BNE   USREDR              NO-ERROR                                     
         CLI   FLDH+5,1            TEST FOR 0 OR 1 BYTE BEFORE '('              
         BH    USREDR                                                           
         BE    USRED2                                                           
*                                                                               
         MVI   DEMO+3,C'T'         SET DEFAULT TO IMPRESSION                    
         ICM   R1,7,FLAST          BUMP POINTER PAST '('                        
         LA    R1,1(R1)                                                         
         STCM  R1,7,FLAST                                                       
         B     USRED4                                                           
*                                                                               
USRED2   MVC   DEMO+3(1),FLD       SET EXTRACTED MODIFIER                       
         TM    FLDH+4,X'04'        TEST FOR A LETTER                            
         BZ    USREDR              NO                                           
*                                                                               
USRED4   XC    FTERM,FTERM                                                      
         MVI   FTERM,RPAREN        NOW LOOK FOR '('                             
         GOTO1 AFVAL,0                                                          
         CLI   FSTOP,RPAREN        TEST IF ')' FOUND                            
         BNE   USREDR                                                           
         CLI   FLDH+5,0            USER DEMO NAME IS 1-7 LONG                   
         BE    USREDR                                                           
         CLI   FLDH+5,7                                                         
         BH    USREDR                                                           
*                                                                               
         LA    R0,4                R0=K'USER DEMOS                              
         LA    R1,1                R1=USER DEMO NUMBER                          
         LA    RE,ESTUSNS          RE=A(USER NAME LIST)                         
*                                                                               
USRED5   CLC   FLD(7),0(RE)        TEST FOR MATCH ON NAME                       
         BE    USRED6                                                           
         LA    RE,7(RE)            NEXT DEMO NAME                               
         LA    R1,1(R1)            INCREMENT USER NUMBER                        
         BCT   R0,USRED5                                                        
         MVC   XTRA(15),=C'NOT IN EST. HDR'                                     
         B     USREDR                                                           
*                                                                               
USRED6   MVI   DEMO+1,USERMOD                                                   
         STC   R1,DEMO+2           SET DEMO NUMBER                              
*                                                                               
         XC    FTERM,FTERM         SET TO RESUME EDIT                           
         MVC   FTERM(3),=C'=/,'                                                 
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST THAT NOTHING FOLLOWS ')'                
         BNE   USREDR                                                           
         CLI   FSTOP,X'FF'         TEST IF TERMINATOR FOUND                     
         BE    USRED8              NO                                           
         ICM   R1,7,FLAST          YES-PUSH THE STRING POINTER AHEAD            
         LA    R1,1(R1)                                                         
         STCM  R1,7,FLAST                                                       
*                                                                               
USRED8   CR    RB,RB               SET CC=EQ                                    
         B     USREDX                                                           
*                                                                               
USREDR   LTR   RB,RB               SET CC=NEQ ON ERROR                          
*                                                                               
USREDX   B     EXXMOD2                                                          
         EJECT                                                                  
* SUB-ROUTINES TO VALIDATE FILTER PARAMETER VALUES - ON ENTRY, WORK             
* CONTAINS A SCANNER BLOCK                                                      
*                                                                               
VALDAY   ST    RE,SAVEREG                                                       
         ZIC   R0,WORK+1                                                        
         GOTO1 VDAYVAL,DMCB,((R0),WORK+22),BYTE,HALF                            
         CLI   BYTE,0                                                           
         BE    ERREXIT2                                                         
         MVC   FILDAY,BYTE                                                      
         B     OKEXIT2                                                          
         SPACE 2                                                                
VALLEN   ST    RE,SAVEREG                                                       
*                                                                               
         MVC   WORK(8),FLDH                                                     
         ZIC   RE,FLDH+5                                                        
         AH    RE,=H'-2'           (L=)                                         
         STC   RE,WORK+5                                                        
         MVC   WORK+8(8),WORK+22                                                
         GOTO1 VSCANNER,DMCB2,WORK,(1,WORK+30),C',=,*'                          
         CLI   4(R1),0                                                          
         BE    ERREXIT2            INVALID INPUT                                
*                                                                               
         CLI   WORK+30,0                                                        
         BE    ERREXIT2                                                         
         TM    WORK+32,X'80'       TEST IF NUMERIC                              
         BZ    ERREXIT2                                                         
         ICM   R0,15,WORK+34                                                    
         BZ    ERREXIT2                                                         
         CH    R0,=H'255'                                                       
         BH    ERREXIT2                                                         
         STC   R0,FILLEN                                                        
*                                                                               
         CLI   WORK+31,0                                                        
         BE    OKEXIT2                                                          
         TM    WORK+33,X'80'       TEST IF NUMERIC                              
         BZ    ERREXIT2                                                         
         ICM   R0,15,WORK+38                                                    
         BZ    ERREXIT2                                                         
         CH    R0,=H'255'                                                       
         BH    ERREXIT2                                                         
         STC   R0,FILLEN2                                                       
         B     OKEXIT2                                                          
         SPACE 2                                                                
VALPRD   ST    RE,SAVEREG                                                       
         CLI   WORK+1,2                                                         
         BE    *+12                                                             
         CLI   WORK+1,3                                                         
         BNE   ERREXIT2                                                         
         TM    WORK+2,X'40'                                                     
         BZ    ERREXIT2                                                         
         MVC   THREE,WORK+22                                                    
*                                                                               
         LA    R0,220                                                           
         LA    RF,CLILIST                                                       
         MVI   BYTE,0                                                           
VALPRD2  OC    0(4,RF),0(RF)                                                    
         BZ    VALPRD3                                                          
         CLC   THREE,0(RF)                                                      
         BE    VALPRD4                                                          
         LA    RF,4(RF)                                                         
         BCT   R0,VALPRD2                                                       
         SPACE                                                                  
VALPRD3  MVC   XTRA(11),=C'BAD PRODUCT'                                         
         B     ERREXIT2                                                         
         SPACE                                                                  
VALPRD4  MVC   FILPRD,THREE        BRAND CODE                                   
         MVC   FILPRN,3(RF)        BRAND NUMBER                                 
         B     OKEXIT2                                                          
         SPACE 2                                                                
VALACT   ST    RE,SAVEREG                                                       
         CLC   WORK+22(5),=C'TODAY'                                             
         BNE   VALACT2                                                          
         GOTO1 VDATCON,DMCB,(2,TODAYC),(3,FILACT)                               
         B     OKEXIT2                                                          
         SPACE                                                                  
VALACT2  GOTO1 VDATVAL,DMCB,WORK+22,DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    ERREXIT2                                                         
         GOTO1 VDATCON,DMCB,DUB,(3,FILACT)                                      
         B     OKEXIT2                                                          
         SPACE 2                                                                
OKEXIT2  CR    RB,RB                                                            
         B     *+6                                                              
ERREXIT2 LTR   RB,RB                                                            
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
ERROR2   GOTO1 VERROR                                                           
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH2   DC    XL32'00'                                                         
         SPACE 2                                                                
* MONTH TABLE                                                                   
*                                                                               
MONTAB   DS    0CL24                                                            
         DC    C'312931303130313130313031'                                      
         EJECT                                                                  
* TABLE OF FILTERS AND THEIR VALUES (COVERED BY FILTABD)                        
*                                                                               
FILTAB   DS    0CL15                                                            
         DC    CL6'P',AL1(1),AL1(PROD),AL1(3),AL3(VALPRD)                       
         DC    AL3(FILPRD-TEMPD)                                                
*                                                                               
         DC    CL6'L',AL1(1),AL1(LENGTH),AL1(1),AL3(VALLEN)                     
         DC    AL3(FILLEN-TEMPD)                                                
*                                                                               
         DC    CL6'D',AL1(1),AL1(DAY),AL1(1),AL3(VALDAY)                        
         DC    AL3(FILDAY-TEMPD)                                                
*                                                                               
         DC    CL6'ACTIVE',AL1(6),AL1(ACTIVE),AL1(12),AL3(VALACT)               
         DC    AL3(FILACT-TEMPD)                                                
*                                                                               
FILTDATE DC    CL6'D',AL1(0),AL1(DATE),AL1(12),AL3(0)                           
         DC    AL3(FILSTART-TEMPD)                                              
*                                                                               
FILENTS  EQU   (*-FILTAB)/L'FILTAB                                              
*                                                                               
ESTMODS2 DC    C'R',X'05'                                                       
         DC    C'V',X'05'                                                       
         DC    X'FF'                                                            
         SPACE 1                                                                
ACTMODS2 DC    C'R',X'05'                                                       
         DC    C'V',X'05'                                                       
         DC    C'T',X'06'                                                       
         DC    X'FF'                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* TABLE OF KEYWORDS AND THEIR ASSOCIATED VALUES (COVERED BY KEYTABD)            
*                                                                               
KEYTAB   DS    0CL19                                                            
         DC    CL7'DAY',AL1(DAY),AL1(3),AL1(UDAY),AL1(4),AL1(DATE)              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'ROTATE',AL1(ROT),AL1(3),AL1(UROT),AL1(12),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'STYPE',AL1(STYPE),AL1(5),AL1(0),AL1(2),X'00'                 
         DC    AL1(DISONLY),AL3(DSTYPE),AL3(0)                                  
*                                                                               
         DC    CL7'MIRROR',AL1(MIRROR),AL1(3),AL1(0),AL1(1),X'00'               
         DC    AL1(DISONLY),AL3(DMIRROR),AL3(0)                                 
*                                                                               
         DC    CL7'AUDGRP',AL1(MIRROR),AL1(3),AL1(0),AL1(4),X'00'               
         DC    AL1(DISONLY),AL3(DAUDGRP),AL3(0)                                 
*                                                                               
         DC    CL7'DATE',AL1(DATE),AL1(4),AL1(0),AL1(5),AL1(DAY)                
         DC    X'00',AL3(DDATE),AL3(EDATE)                                      
*                                                                               
         DC    CL7'ADU',AL1(ADU),AL1(3),AL1(USTAT),AL1(3)                       
         DC    AL1(ACT),X'00',AL3(0),AL3(EADU)                                  
*                                                                               
         DC    CL7'PREEMPT',AL1(PREMPT),AL1(3),AL1(UPRE),AL1(1)                 
         DC    AL1(INT),X'00',AL3(DPREMPT),AL3(EPREMPT)                         
*                                                                               
         DC    CL7'TIME',AL1(TIME),AL1(1),AL1(UTIME),AL1(11),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'LEN',AL1(LENGTH),AL1(3),AL1(ULEN),AL1(8),X'00'               
         DC    AL1(COPSDIS),AL3(0),AL3(0)                                       
*                                                                               
         DC    CL7'NAME',AL1(NAME),AL1(1),AL1(UPRGN),AL1(16),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'NAD',AL1(NAD),AL1(3),AL1(0),AL1(6),X'00'                     
         DC    AL1(DISONLY),AL3(DNAD),AL3(0)                                    
*                                                                               
         DC    CL7'DHI',AL1(DHI),AL1(3),AL1(UDHI),AL1(1)                        
         DC    X'00',X'00',AL3(DDHI),AL3(EDHI)                                  
*                                                                               
PRODUCT  DC    CL7'PRODUCT',AL1(PROD),AL1(3),AL1(UPRD),AL1(23),X'00'            
         DC    AL1(COPSDIS),AL3(DPROD),AL3(0)                                   
*                                                                               
         DC    CL7'FPRDSHR',AL1(FPR),AL1(2),AL1(UP1SHR),AL1(59),X'00'           
         DC    X'00',AL3(DFEDSHR),AL3(EFEDSHR)                                  
*                                                                               
         DC    CL7'ASS',AL1(ASS),AL1(3),AL1(UASS),AL1(8),X'00'                  
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
OTHERS   DC    CL7'AUTH',AL1(OTH),AL1(03),AL1(UOTHER),AL1(13),X'00'             
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'REASON',AL1(OTH),AL1(03),AL1(UOTHER),AL1(23),X'00'           
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'ZONE',AL1(OTH),AL1(03),AL1(UOTHER),AL1(6),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'TSUPP',AL1(OTH),AL1(03),AL1(UOTHER),AL1(11),X'00'            
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'PROMO',AL1(OTH),AL1(04),AL1(UOTHER),AL1(10),X'00'            
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'POS',AL1(OTH),AL1(03),AL1(UOTHER),AL1(7),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'ECOST',AL1(OTH),AL1(03),AL1(UOTHER),AL1(15),X'00'            
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'MR',AL1(OTH),AL1(02),AL1(UOTHER),AL1(5),X'00'                
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'DP',AL1(OTH),AL1(02),AL1(UOTHER),AL1(11),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'PB',AL1(OTH),AL1(02),AL1(UOTHER),AL1(4),X'00'                
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'WINDOW',AL1(OTH),AL1(02),AL1(UOTHER),AL1(15),X'00'           
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'AFT',AL1(OTH),AL1(02),AL1(UOTHER),AL1(12),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'EFT',AL1(OTH),AL1(02),AL1(UOTHER),AL1(12),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'CMT',AL1(OTH),AL1(03),AL1(UOTHER),AL1(29),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'TA',AL1(OTH),AL1(02),AL1(UOTHER),AL1(9),X'00'                
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'CA',AL1(OTH),AL1(02),AL1(UOTHER),AL1(9),X'00'                
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'SF',AL1(OTH),AL1(02),AL1(UOTHER),AL1(14),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'BT',AL1(OTH),AL1(02),AL1(UOTHER),AL1(14),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
*                                                                               
**       DC    CL7'COMM',AL1(OTH),AL1(03),AL1(UOTHER),AL1(29),X'00'             
**       DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'OTHER',AL1(OTH),AL1(03),AL1(UOTHER),AL1(23),X'00'            
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
ACTUAL   DC    CL7'ACT',AL1(ACT),AL1(3),AL1(UACT),AL1(10),X'00'                 
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'TCAR',AL1(TCAR),AL1(4),AL1(UTCAR),AL1(1),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'INTEG',AL1(INT),AL1(1),AL1(UINT),AL1(8),AL1(PREMPT)          
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'SHARE',AL1(SHARE),AL1(1),AL1(USHR),AL1(5),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'HUT',AL1(HUT),AL1(1),AL1(UHUT),AL1(5),X'00'                  
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'HAVG',AL1(HUTAV),AL1(3),AL1(UHAVE),AL1(1),X'00'              
         DC    X'00',AL3(0),AL3(EHUTAV)                                         
*                                                                               
         DC    CL7'RTG',AL1(RAT),AL1(1),AL1(URAT),AL1(5),X'00'                  
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'UNIV',AL1(UNIV),AL1(1),AL1(UUNCD),AL1(4),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'UPER',AL1(UPCT),AL1(2),AL1(UUNPC),AL1(6),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'HSCHEME',AL1(HSC),AL1(2),AL1(UHSC),AL1(1),X'00'              
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'FEED',AL1(FEED),AL1(1),AL1(UFEED),AL1(59),X'00'              
         DC    AL1(COPSDIS),AL3(DFEDSHR),AL3(EFEDSHR)                           
*                                                                               
         DC    CL7'FEEDCOD',AL1(FCD),AL1(5),AL1(UFEEDCD),AL1(54),X'00'          
         DC    X'00',AL3(DFEEDC),AL3(EFEEDC)                                    
*                                                                               
         DC    CL7'BB',AL1(BB),AL1(2),AL1(UBB),AL1(2),X'00'                     
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'AFFID',AL1(AFFID),AL1(5),AL1(UAFFID),AL1(5),X'00'            
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'IMPACT',AL1(IMP),AL1(3),AL1(UIMP),AL1(6),X'00'               
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'PG',AL1(PG),AL1(2),AL1(UPG),AL1(10),X'00'                    
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'DG',AL1(DG),AL1(2),AL1(UDG),AL1(15),X'00'                    
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'REP',AL1(SREP),AL1(3),AL1(USREP),AL1(3),X'00'                
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'STATUS',AL1(STAT),AL1(6),AL1(USTAT),AL1(3),X'00'             
         DC    AL1(DISONLY),AL3(0),AL3(0)                                       
*                                                                               
         DC    CL7'PFIL',AL1(PFIL),AL1(2),AL1(UPF),AL1(6),X'00'                 
         DC    AL1(DISONLY),AL3(0),AL3(0)                                       
*                                                                               
         DC    CL7'NTI',AL1(NTI),AL1(1),AL1(UNTI),AL1(5),X'00'                  
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'NSI',AL1(NSI),AL1(3),AL1(UNSI),AL1(5),X'00'                  
         DC    X'00',AL3(0),AL3(0)                                              
*                                                                               
         DC    CL7'BILL',AL1(BILL),AL1(1),AL1(UBILL),AL1(49),X'00'              
         DC    AL1(DISONLY),AL3(0),AL3(0)                                       
*                                                                               
         DC    CL7'PAY',AL1(PAY),AL1(3),AL1(UPAY),AL1(49),X'00'                 
         DC    AL1(DISONLY),AL3(0),AL3(0)                                       
*                                                                               
ESTLK    DC    CL7'EST',AL1(ESTDE),AL1(3),AL1(0),AL1(1),X'00'                   
         DC    AL1(INTERNAL),AL3(0),AL3(0)                                      
*                                                                               
DELETE   DC    CL7'DELETE',AL1(DELUN),AL1(255),AL1(0),AL1(3),X'00'              
         DC    AL1(INTERNAL),AL3(0),AL3(0)                                      
*                                                                               
KEYS     EQU   (*-KEYTAB)/L'KEYTAB                                              
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         SPACE                                                                  
         ORG   TWAD+PAGELEN                                                     
SVAREA   DS    CL256                                                            
         ORG   SVAREA                                                           
SVLACT   DS    X                   LAST ACTION                                  
SVKEYL   DS    CL(KEYS)            LAST KEYWORD LIST                            
SVEDEML  DS    CL60                LAST ESTIMATED DEMO LIST                     
SVADEML  DS    CL60                LAST ACTUAL DEMO LIST                        
SVFILTL  DS    CL60                LAST FILTER LIST                             
SVKEYS   DS    X                                                                
SVEDEMOS DS    X                                                                
SVADEMOS DS    X                                                                
SVFILTS  DS    X                                                                
SVVALSLN EQU   *-SVKEYL                                                         
*                                                                               
SVLDATE  DS    XL2                 LAST UNIT DATE DISPLAYED ON SCREEN           
SVLSUB   DS    X                   LAST UNIT SUB-LINE                           
         EJECT                                                                  
* DSECT TO COVER KEYWORD TABLE ENTRIES                                          
*                                                                               
KEYTABD  DSECT                                                                  
KEYNAME  DS    CL7                 KEYWORD NAME                                 
KEYNUM   DS    X                   KEYWORD NUMBER                               
KEYMINL  DS    X                   MINIMUM LENGTH                               
KEYDATA  DS    X                   DATA TYPE                                    
KEYDLEN  DS    X                   MAXIMUM DATA LENGTH                          
KEYEXCP  DS    X                   INCOMPATIBLE KEYWORD NUMBER                  
KEYCTL   DS    X                   CONTROL BITS                                 
*                                  X'80' = DISPLAY ONLY                         
*                                  X'40' = INTERNAL                             
*                                  X'20' = COPY SPLIT DISPLAY ONLY              
KEYDIS   DS    AL3                 OVERRIDE DISPLAY ROUTINE                     
KEYEDIT  DS    AL3                 OVERRIDE EDIT ROUTINE                        
KEYTABL  EQU   *-KEYTABD                                                        
KEYFLDH  DS    CL8                 SAVED FIELD HDR FOR GLOBAL VALUE             
KEYVALUE DS    CL16                GLOBAL CHANGE VALUE FOR KEY                  
KEYENTL  EQU   *-KEYTABD           LENGTH OF KEYSTACK ENTRIES                   
         SPACE 2                                                                
* DSECT TO COVER FILTER TABLE ENTRIES                                           
*                                                                               
FILTABD  DSECT                                                                  
FILKEY   DS    CL6                 FILTER KEYWORD NAME                          
FILKEYL  DS    X                   FILTER KEYWORD LENGTH                        
FILNUM   DS    X                   FILTER NUMBER                                
FILINTL  DS    X                   INTERNAL DATA LENGTH                         
FILEDIT  DS    AL3                 OVERRIDE EDIT ROUTINE                        
FILWORK  DS    AL3                 DISPLACEMENT TO WORKING STORAGE FLD          
FILTABL  EQU   *-FILTABD                                                        
         SPACE 2                                                                
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
SAVEREG3 DS    A                                                                
VDISPLAY DS    V                   V(GENERAL UNIT DISPLAY)                      
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
*                                                                               
FILSTART DS    CL6                 FILTERED START DATE                          
FILEND   DS    CL6                 FILTERED END DATE                            
FILDAY   DS    C                   DAY FILTER                                   
FILLEN   DS    X                   LENGTH FILTER                                
FILLEN2  DS    X                   SECOND LENGTH FILTER                         
FILPRD   DS    CL3                 PRODUCT CODE FILTER                          
FILPRN   DS    X                   PRODUCT NUMBER FILTER                        
FILACT   DS    XL3                 ACTIVITY DATE FILTER (BINARY)                
*                                                                               
ABLKLIN  DS    A                   A(BLANK LINE AFTER ACTION)                   
AFSTFLD  DS    A                   A(FIRST FIELD HEADER)                        
ALSTFLD  DS    A                   A(END OF SCREEN)                             
AFLD     DS    A                   SCREEN POINTER                               
AOUT     DS    A                   OUTPUT FIELD POINTER                         
APROTFLD DS    A                   A(PROTECTED FIELD HEADER)                    
AUNPRFLD DS    A                   A(UNPROTECTED FIELD HEADER)                  
*                                                                               
SUB      DS    X                                                                
PNGUAHLD DS    CL4                 NEW PACKAGE GUARANTEE HOLD AREA              
DNGUAHLD DS    CL4                 NEW DEMO GUARANTEE HOLD AREA                 
PGUARHLD DS    CL2                 PACKAGE GUARANTEE HOLD AREA                  
DGUARHLD DS    CL2                 DEMO GUARANTEE HOLD AREA                     
UNITDATE DS    CL6                 YYMMDD                                       
OLDKEY   DS    CL(L'NUKEY)                                                      
NEWKEY   DS    CL(L'NUKEY)                                                      
OLDDA    DS    XL4                                                              
NEWDA    DS    XL4                                                              
OLDSTAT  DS    XL1                                                              
NEWSTAT  DS    XL1                                                              
*                                                                               
ALLOCSW  DS    C                   ALLOCATE SWITCH (Y)                          
UNALLOC  DS    C                   UNALLOCATION FLAG (Y)                        
DFIELDS  DS    X                   COUNT OF DATA FIELDS                         
FIELDS   DS    X                   COUNT OF SCREEN FIELDS                       
RECORDS  DS    X                                                                
OTHERSW  DS    C                   CHECK THAT "OTHER" ARE IN COR.FORMAT         
MORESW   DS    C                   MORE RECORDS ON NEXT SCREEN (Y/N)            
GLOBSW   DS    C                   GLOBAL VALUE HAS BEEN INPUT (Y)              
ESTLOOK  DS    C                   FORCE LOOK-UP OF ESTIMATED DEMOS (Y)         
DEMTYPE  DS    C                   E=ESTIMATED, A=ACTUAL                        
KDATASV  DS    X                   SAVED KEYDATA INFO                           
NUMPRD   DS    X                   NUMBER OF PRODUCTS                           
*                                                                               
DEMO     DS    XL4                 4-BYTE INTERNAL DEMO CODE                    
DEMOEL   DS    X                   DEMO OVERRIDE ELEMENT CODE                   
USEREL   DS    X                   USER OVERRIDE ELEMENT CODE                   
LFIL     DS    X                                                                
OVEREL   DS    CL60                DEMO OVERRIDE ELEMENT AREA                   
USERNMS  DS    CL28                USER DEMO NAMES                              
*                                                                               
TRPRDHLD DS    CL6                 TRAFFIC PRODUCT HOLD AREA                    
TRPRSAVE DS    CL2                 TRAFFIC SAVE OF NBPRD, NBPRD2                
*                                                                               
KEYLIST  DS    CL(KEYS)                                                         
ESTLIST  DS    CL60                ESTIMATED DEMO LIST                          
ACTLIST  DS    CL60                ACTUAL DEMO LIST                             
FILSTACK DS    CL60                                                             
CONLISTL EQU   *-KEYLIST           LENGTH OF CONTROL VALUES                     
KEYCOUNT DS    X                                                                
ESTDEMOS DS    X                   NUMBER OF ESTIMATED DEMOS                    
ACTDEMOS DS    X                                                                
FILTERS  DS    X                                                                
KEYSTACK DS    25CL(KEYENTL)       ENOUGH FOR 25 ENTRIES                        
*                                                                               
         DS    0F                                                               
BLOCK    DS    CL256                                                            
PRDFLDH  DS    CL8                 PRODUCT GLOBAL FLDH (ALLOC)                  
PRDVAL   DS    CL16                PRODUCT GLOBAL VALUE (ALLOC)                 
ACTFLDH  DS    CL8                 ACTUAL COST FLDH (ALLOC)                     
ACTVAL   DS    CL16                ACTUAL COST GLOBAL VALUE (ALLOC)             
KEYH     DS    CL8                 GLOBAL FIELD HEADER                          
KEYVAL   DS    CL16                GLOBAL KEYWORD VALUE                         
DEMGLOB  DS    F                   INPUT DEMO GLOBAL VALUE                      
ESTVAL   DS    CL60                ESTIMATED DEMOS AREA (RECORD)                
ACTVALS  DS    CL60                ACTUAL DEMOS AREA (RECORD)                   
INPVALS  DS    CL60                INPUT VALUES WORK AREA                       
ESTUNIV  DS    CL60                ESTIMATED UNIVERSES                          
ACTUNIV  DS    CL60                ACTUAL UNIVERSES                             
ESTGLOB  DS    CL60                GLOBAL ESTIMATED DEMOS (USER INPUT)          
ACTGLOB  DS    CL60                GLOBAL ACTUAL DEMOS (USER INPUT)             
SVHOMUNV DS    F                   HOMES UNIVERSE FOR VPH OVERRIDE              
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
DISONLY  EQU   X'80'                                                            
INTERNAL EQU   X'40'                                                            
COPSDIS  EQU   X'20'                                                            
DAY      EQU   1                                                                
TIME     EQU   2                                                                
LENGTH   EQU   3                                                                
NAME     EQU   4                                                                
PROD     EQU   5                                                                
FPR      EQU   6                                                                
ASS      EQU   7                                                                
ACT      EQU   8                                                                
INT      EQU   9                                                                
SHARE    EQU   10                                                               
HUT      EQU   11                                                               
RAT      EQU   12                                                               
UNIV     EQU   13                                                               
FEED     EQU   14                                                               
AFFID    EQU   15                                                               
IMP      EQU   16                                                               
STAT     EQU   17                                                               
UPCT     EQU   18                                                               
OTH      EQU   19                                                               
NTI      EQU   20                                                               
NSI      EQU   21                                                               
HSC      EQU   22                                                               
SREP     EQU   23                                                               
FCD      EQU   24                                                               
PG       EQU   25                                                               
PFIL     EQU   26                                                               
DG       EQU   27                                                               
BB       EQU   28                                                               
ROT      EQU   29                                                               
STYPE    EQU   30                                                               
BILL     EQU   31                                                               
PAY      EQU   32                                                               
HUTAV    EQU   33                                                               
MIRROR   EQU   34                                                               
NAD      EQU   35                                                               
PREMPT   EQU   36                                                               
ADU      EQU   37                                                               
DHI      EQU   38                                                               
TCAR     EQU   39                                                               
ESTDE    EQU   50                                                               
DATE     EQU   100                                                              
ACTIVE   EQU   150                                                              
DELUN    EQU   200                                                              
*                                                                               
USERMOD  EQU   X'21'               USER DEMO MODIFIER                           
*                                                                               
PLUS     EQU   C'+'                                                             
SLASH    EQU   C'/'                                                             
EQUAL    EQU   C'='                                                             
LPAREN   EQU   C'('                                                             
RPAREN   EQU   C')'                                                             
         SPACE 2                                                                
* DDCOMFACS                                                                     
*                                                                               
*        PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* NETDEMOD (DSECT COVERING NETWORK DEMO BLOCK)                                  
*                                                                               
*        PRINT OFF                                                              
NETDEMOD DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* NETUNIVD (DSECT COVERING NETWORK UNIVERSE BLOCK)                              
*                                                                               
*        PRINT OFF                                                              
NETUNIVD DSECT                                                                  
       ++INCLUDE NETUNIVD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016SCHTB15A  05/01/02'                                      
         END                                                                    
