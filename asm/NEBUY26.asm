*          DATA SET NEBUY26    AT LEVEL 084 AS OF 04/06/11                      
*PHASE T31126A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - SPECIAL RATE MAINT - T31126'               
T31126   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUYUN*,RA,RR=RE                                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31126+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA             R8 POINTS TO THE TWA                         
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL STORAGE                   
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
*                                                                               
         L     RF,NBACOM                                                        
         USING COMFACSD,RF                                                      
         MVC   ASWITCH,CSWITCH                                                  
         DROP  RF                                                               
*                                                                               
* READ BS1 PROFILE FOR NEW BILL REC READ                                        
*                                                                               
         NI    FLAGS,X'FF'-NEWBILL                                              
*                                                                               
         OI    NBINDS2,NBNOBLRD    DEFAULT-DONT READ NEGENUBILL RECS            
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB1S'                                                 
         NI    WORK,X'BF'          LOWER CASE                                   
         MVC   WORK+4(2),NBSELAGY     AGENCY                                    
         GOTO1 NBGTPROF,DMCB,(X'C0',WORK),WORK+20,NBDM                          
         CLI   WORK+24,C'Y'        MUST READ NEW RECORDS?                       
         BNE   MAIN10                                                           
         OI    FLAGS,NEWBILL                                                    
         B     UNIT                                                             
*                                                                               
MAIN10   DS    0H                                                               
*                                                                               
         XC    SPRHED1+40(25),SPRHED1+40                                        
         OI    SPRHED1H+6,X'80'                                                 
         XC    SPRHED2+40(25),SPRHED2+40                                        
         OI    SPRHED2H+6,X'80'                                                 
*                                                                               
UNIT     TM    MODE,FIRST          FIRST TIME CLEAR SAVE AREA                   
         BZ    *+10                                                             
         XC    SVDATA,SVDATA                                                    
*                                                                               
         OI    BUYACTH+1,X'01'     FORCE MODIFIED FOR DISPLAY                   
*                                                                               
         BAS   RE,ACTED            EDIT ACTION FIELD                            
         CLC   DATE,SVDATE         TEST FOR CHANGE IN AIR DATE                  
         BE    *+8                 NO                                           
         OI    MODE,DISPLAY        YES-FORCE DISPLAY                            
         MVC   SVDATE,DATE                                                      
         CLC   SUB,SVSUB           TEST FOR CHANGE IN SUB-LINE                  
         BE    *+8                                                              
         OI    MODE,DISPLAY        YES-FORCE DISPLAY                            
         MVC   SVSUB,SUB                                                        
         SPACE                                                                  
*                                                                               
* ACTION CHANGE                                                                 
*                                                                               
UNIT010  CLI   ACTION,CSR          TEST FOR ACTION CHANGE                       
         BE    UNIT030                                                          
         CLI   ACTION,ASR          ADD NEW RATE ELEMENTS                        
         BE    UNIT035                                                          
         B     UNIT190                                                          
*                                                                               
UNIT030  DS    0H                                                               
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    UNIT190             YES                                          
*                                                                               
UNIT035  DS    0H                                                               
         BAS   RE,LOCKPACK                                                      
         BAS   RE,GETUNIT                                                       
*                                                                               
         BAS   RE,BSTATAB          BUILD TABLE OF CUT-IN STATIONS               
         BAS   RE,BSRTAB           BUILD TABLE OF SPECIAL CHARGES               
*                                                                               
         CLI   ACTION,CSR                                                       
         BE    *+12                                                             
         BAS   RE,ADDSR            ADD SPECIAL RATE                             
         B     *+8                                                              
*                                                                               
         BAS   RE,VSR              VALIDATE SPECIAL RATES                       
*                                                                               
         L     R4,NBAIO                                                         
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',NBAIO),0                       
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   UNIT050                                                          
*                                                                               
         L     R4,12(R1)                                                        
         USING NUACTD,R4                                                        
         GOTO1 VDATCON,DMCB,(5,0),(3,NUACTCDT)                                  
         DROP  R4                                                               
*                                                                               
UNIT050  L     R4,NBAIO                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R4)                                               
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE MAKE SURE KEY IS THERE                      
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         L     R4,AIOAREA1                                                      
*                                                                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA4                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)  PUT NEW RECORD BACK                 
         SPACE                                                                  
         USING NURECD,R4                                                        
         SPACE                                                                  
UNIT130  BAS   RE,REACT            RE-CONSTRUCT ACTION FIELD                    
         GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
*                                                                               
         CLI   ACTION,ASR                                                       
         BE    *+8                                                              
         BAS   RE,DISUNIT          RE-DISPLAY CHANGED UNIT                      
*                                                                               
         GOTO1 VBLDRQST                                                         
*                                                                               
         CLI   ACTION,ASR                                                       
         BNE   UNITX                                                            
         XC    SVSUB,SVSUB         RE-INITIALIZE FOR DISPLAY                    
         XC    SVDATE,SVDATE                                                    
         B     UNITX                                                            
*                                                                               
* ACTION DISPLAY OR FORCED DISPLAY                                              
*                                                                               
UNIT190  BAS   RE,GETUNIT                                                       
         BAS   RE,BSRTAB           BUILD TABLE OF X'03' ELEMS                   
         BAS   RE,DISUNIT                                                       
         B     UNITX                                                            
         SPACE                                                                  
* SET MESSAGE AND CURSOR THEN EXIT                                              
*                                                                               
UNITX    BAS   RE,MSG                                                           
         NI    MODE,X'FF'-DISPLAY                                               
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTION FIELD                                              
*                                                                               
ACTED    ST    RE,SAVEREG                                                       
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA AT END OF ACTION            
         GOTO1 AFVAL,0                                                          
         MVI   CAACTSW,NO                                                       
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTM                NO                                           
         CLC   FLD(2),=C'CA'                                                    
         BNE   *+8                                                              
         MVI   CAACTSW,YES                                                      
         SPACE                                                                  
ACTED2   XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         GOTO1 AFVAL,0                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    ACTM                                                             
         MVI   FERN,INVERR                                                      
         MVI   FNDX,2                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),C',=,-'                              
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         SPACE                                                                  
ACTED4   MVI   FERN,DATERR                                                      
         MVI   DDMMYYIP,NO                                                      
         GOTO1 VDATVAL,(R1),(0,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ACTED4A                                                          
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         CLC   DUB(6),ESTSTART     TEST IF DATE BEFORE EST START                
         BL    ERROR               YES                                          
         SR    R0,R0                                                            
         ICM   R0,1,BUYPROF+14                                                  
         BZ    ERROR                                                            
         GOTO1 VADDAY,(R1),ESTEND,DUB2,(R0)                                     
         CLC   DUB(6),DUB2         TEST IF OUTSIDE OF EST + PROF DAYS           
         BH    ERROR               NO-SAME YEAR AS EST START                    
         MVI   DDMMYYIP,YES        YES-MUST BE YEAR OF ESTIMATE END             
         B     ACTED5                                                           
         SPACE                                                                  
ACTED4A  GOTO1 VDATVAL,(R1),(1,WORK+12),DUB                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         CLC   WORK(1),3(R1)       DOES DATE MAKE UP FIRST HALF OF FLD          
         BNE   ERROR               NO                                           
         MVC   DUB(2),ESTSTART     ESTIMATE START YEAR (INPUT=MMDD)             
         CLC   ESTSTART(2),ESTEND  TEST IF EST START/END IN SAME YEAR           
         BE    ACTED5              YES                                          
         CLC   DUB+2(4),ESTSTART+2 TEST IF INPUT MMDD LT EST ST MMDD            
         BNL   *+10                NO-SAME YEAR AS EST START                    
         MVC   DUB(2),ESTEND       YES-MUST BE YEAR OF ESTIMATE END             
         SPACE                                                                  
ACTED5   GOTO1 VDATCON,(R1),DUB,(2,DATE)                                        
         MVC   CHARDATE,DUB        SAVE YYMMDD DATE                             
         SPACE                                                                  
ACTED6   MVI   SUB,1               DEFAULT IS SUB-LINE=1                        
         CLI   WORK+1,0            TEST FOR SUB-LINE NOTATION                   
         BE    ACTED8                                                           
         MVI   FNDX,0                                                           
         MVI   FERN,INVERR                                                      
         MVC   XTRA,SPACES                                                      
         MVC   XTRA(8),=C'SUB-LINE'                                             
         CLI   WORK+1,3                                                         
         BH    ERROR                                                            
         TM    WORK+3,X'80'        TEST IF NUMERIC                              
         BO    *+12                                                             
         MVI   FERN,NUMERR                                                      
         B     ERROR                                                            
*                                                                               
         ICM   R0,15,WORK+8                                                     
         BZ    ERROR                                                            
         CH    R0,=H'255'                                                       
         BH    ERROR                                                            
         STC   R0,SUB                                                           
         B     ACTED8                                                           
         SPACE                                                                  
ACTED8   MVI   FNDX,3                                                           
ACTED8A  CLI   FSTOP,COMMA         TEST FOR COMMA                               
         BNE   ACTEDX                                                           
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA         SEARCH FOR COMMA TO NEXT PARAMETER           
         MVC   XTRA,SPACES                                                      
         GOTO1 AFVAL,0                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         MVI   FERN,INVERR         EDIT FOR RE LOOK-UP OF ESTIMATED             
         CLC   FLD(4),=C'EST='     DEMOS                                        
         BNE   ACTED10                                                          
         CLI   FLDH+5,5                                                         
         BL    ERROR                                                            
         CLI   FLDH+5,7                                                         
         BH    ERROR                                                            
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'5'                                                         
         EX    R1,YESCOMP                                                       
         BNE   ERROR                                                            
         MVI   FERN,PAKFERR                                                     
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BO    ERROR               CANNOT CHANGE ESTIMATED DEMOS                
         MVI   ESTLOOK,YES                                                      
         MVI   FNDX,4                                                           
         B     ACTED8A             CHECK NEXT PARAMETER                         
*                                                                               
ACTED10  MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ERROR                                                            
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         MVI   FNDX,4                                                           
         B     ACTED8A                                                          
         DROP  RE                                                               
         SPACE                                                                  
ACTM     MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
         SPACE                                                                  
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
         B     ERROR                                                            
ACTEDX10 OC    AUDREASN,AUDREASN   WAS REASON CODE INPUTTED                     
         BZ    ERROR                                                            
*                                                                               
ACTEDX30 MVI   FNDX,0                                                           
         MVC   XTRA,SPACES                                                      
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  RE                                                               
*                                                                               
YESCOMP  CLC   FLD+4(0),=C'YES'                                                 
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR LOCKED PACKAGE                                       
*                                                                               
LOCKPACK LR    R0,RE               SAVE RETURN POINT                            
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BZ    LOCKPACX                                                         
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,PAKLERR                                                     
         B     ERROR                                                            
         SPACE                                                                  
LOCKPACX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET UNIT RECORD                                                
*                                                                               
GETUNIT  ST    RE,SAVEREG                                                       
         L     RE,APACKREC                                                      
         MVC   NBSELDP,NPAKDP-NPRECD(RE)                                        
         GOTO1 VDATCON,DMCB,(2,DATE),NBSELSTR                                   
         MVC   NBSELEND,NBSELSTR                                                
         MVC   NBSELPRG,PROG                                                    
         MVC   NBSELSUB,SUB                                                     
         MVI   NBDATA,C'U'                                                      
         MVI   NBSEQ,C'D'                                                       
         MVI   NBDIRECT,YES                                                     
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBESTOPT,C'M'       LOOK UP HOMES VALUES                         
         MVI   NBUSER+13,NO        **FUDGE PROFILE TO RETURN PRE-EMPTS          
         MVC   NBAIO,AIOAREA1                                                   
         MVI   FERN,NOTFOUND                                                    
         SPACE                                                                  
GETUNIT2 GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RETURNED                       
         BE    GETUNIT5            YES                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ERROR                                                            
         B     GETUNIT2                                                         
*                                                                               
GETUNIT5 DS    0H                                                               
         NI    MYFLAG,X'FF'-SMAP                                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'18',NBAIO),0                       
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   GETUNITX                                                         
*                                                                               
         L     R4,12(R1)                                                        
         USING NUDTAD,R4                                                        
*                                                                               
         CLI   NUDTAMYR,0          SECTIONAL MAP?                               
         BE    GETUNITX                                                         
*                                                                               
         OI    MYFLAG,SMAP                                                      
         MVC   SVMYR,NUDTAMYR      SAVE MAP YEAR                                
         MVC   SVMCODE,NUDTAMCD    SAVE MAP CODE                                
         DROP  R4                                                               
*                                                                               
GETUNITX L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO RECONSTRUCT ACTION FIELD                                       
*                                                                               
REACT    NTR1                                                                   
         MVC   BUYACT,SPACES                                                    
         OI    BUYACTH+6,X'80'                                                  
         LA    R3,BUYACT                                                        
         MVC   0(3,R3),=CL2'DSR'                                                
         CLI   ACTION,DSR                                                       
         BE    RA100                                                            
         MVC   0(3,R3),=CL2'CSR'                                                
         CLI   ACTION,CSR                                                       
         BE    RA100                                                            
         MVC   0(3,R3),=CL3'ASR'                                                
         LA    R3,3(R3)                                                         
         B     *+8                                                              
RA100    LA    R3,2(R3)                                                         
         MVI   0(R3),COMMA                                                      
         LA    R3,1(R3)                                                         
         CLI   DDMMYYIP,YES        IF YEAR INPUT YEAR OUTPUT                    
         BNE   RA200                                                            
         GOTO1 VDATCON,DMCB,(2,DATE),(5,(R3))                                   
         LA    R3,8(R3)                                                         
         B     RA220                                                            
*                                                                               
RA200    GOTO1 VDATCON,DMCB,(2,DATE),(4,(R3))                                   
         LA    R3,5(R3)                                                         
RA220    CLI   SUB,0                                                            
         BE    REACTX                                                           
         CLI   SUB,1                                                            
         BE    REACTX                                                           
         MVI   0(R3),C'-'                                                       
         ZIC   R2,SUB                                                           
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
         SPACE                                                                  
REACTX   MVC   SVDATE,DATE         UPDATE SAVE DATE/SUB-LINE IN                 
         MVC   SVSUB,SUB           CASE CALENDARIZING TOOK PLACE.               
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* ADD SPECIAL RATES TO UNIT REC                                                 
*                                                                               
ADDSR    NTR1                                                                   
         MVI   FERN,MISERR                                                      
*                                                                               
         LA    R1,13                                                            
         LA    R3,SPRTYP1H                                                      
*                                                                               
ASR05    LR    R2,R3                                                            
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         TM    4(R2),X'80'                                                      
         BO    ASR10                                                            
         BAS   RE,BUMPFLD                                                       
         LA    R3,SPRTYP2H-SPRTYP1H(R3)                                         
         BCT   R1,ASR05                                                         
*                                                                               
         LA    R2,SPRTYP1H                                                      
         ST    R2,FADDR                                                         
*                                                                               
         BAS   RE,CLRSCRN                                                       
         B     ERROR                                                            
*                                                                               
ASR10    DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'03',AIOAREA1),0                    
*                                                                               
         BAS   RE,E03TAB           FIND END OF X'03' TABLE                      
*                                                                               
         LA    R2,SPRTYP1H                                                      
         LA    R3,13                                                            
         MVI   FERN,INVERR                                                      
*                                                                               
ASR20    DS    0H                                                               
         ST    R2,FADDR                                                         
         CLI   5(R2),0                                                          
         BE    ASR50                                                            
*                                                                               
         XC    WORK,WORK                                                        
         LA    RF,WORK                                                          
         USING NUSPRD,RF                                                        
*                                                                               
         MVI   NUSPREL,X'03'                                                    
         MVI   NUSPRLEN,NUSPRLN4   LENGTH                                       
         DROP  RF                                                               
*                                                                               
         BAS   RE,BUILD            BUILDS NEW X'03' ELEMENT IN WORK             
*                                                                               
         L     RF,A03INDX                                                       
         MVC   0(32,RF),WORK       MOVE IT INTO TABLE                           
*                                                                               
         L     RF,A03INDX                                                       
         LA    RF,33(RF)                                                        
         ST    RF,A03INDX                                                       
*                                                                               
         LA    R2,SPRTYP2H-SPRTYP1H(R2)                                         
         BCT   R3,ASR20                                                         
*                                                                               
ASR50    DS    0H                                                               
         L     RF,A03INDX                                                       
         MVI   0(RF),X'FF'                                                      
         XC    A03INDX,A03INDX                                                  
*                                                                               
         L     R3,AIOAREA2                                                      
         MVI   SRSEQ,X'01'                                                      
*                                                                               
ASR110   DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    ASR120                                                           
*                                                                               
ASR115   DS    0H                                                               
         MVI   2(R3),0             INIT SEQ# TO ZERO                            
         CLI   3(R3),C'C'          ACTUAL CASH%?                                
         BE    ASR116                                                           
         CLI   3(R3),C'F'          ASSIGNED CASH%?                              
         BE    ASR116                                                           
*                                                                               
         MVC   2(1,R3),SRSEQ       MOVE IN SEQUENCE #                           
         ZIC   RF,SRSEQ                                                         
         AHI   RF,1                                                             
         STC   RF,SRSEQ                                                         
*                                                                               
ASR116   CLI   SRSEQ,X'24'         MORE THAN 35 CHARGES?                        
         BH    CHERR                                                            
*                                                                               
ASR118   LA    R3,33(R3)                                                        
         B     ASR110                                                           
*                                                                               
ASR120   L     R3,AIOAREA2         ADD NEW ELEMENTS TO RECORD                   
*                                                                               
ASR125   DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    ASR140                                                           
*                                                                               
         CLI   3(R3),C'F'          ASSIGNED CASH%?                              
         BE    ASR130              SKIP IT                                      
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(32),0(R3)                                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
*                                                                               
ASR130   DS    0H                                                               
         LA    R3,33(R3)                                                        
         B     ASR125                                                           
*                                                                               
ASR140   DS    0H                                                               
*                                                                               
ADDSRX   DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* VALIDATE AND BUILD TABLE OF X'03' ELEMENTS TO ADD TO UNIT REC                 
*                                                                               
VSR      NTR1                                                                   
         MVI   FERN,INVERR                                                      
*                                                                               
         L     RF,AIOAREA2                                                      
         CLI   0(RF),X'FF'         ANY ELEMENTS ALREADY?                        
         BE    VSR10               NO                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'03',AIOAREA1),0                    
*                                                                               
VSR10    DS    0H                                                               
         BAS   RE,F03FRST          FIND FIRST X'03' ON SCREEN IN TABLE          
*                                                                               
         LA    R2,SPRTYP1H                                                      
         LA    R3,13                                                            
*                                                                               
VSR20    DS    0H                                                               
         ST    R2,FADDR                                                         
*                                                                               
         CLC   8(2,R2),=CL2'DE'                                                 
         BNE   VSR25                                                            
         L     RF,A03INDX                                                       
         OI    32(RF),X'80'        DELETE THIS RATE                             
         B     VSR50                                                            
*                                                                               
VSR25    CLC   8(2,R2),=CL2'  '                                                 
         BE    VSR50                                                            
         CLC   8(2,R2),=2X'00'                                                  
         BE    VSR50                                                            
*                                                                               
         L     RF,A03INDX                                                       
         CLI   0(RF),X'FF'         CAN'T ADD ANY MORE                           
         BE    ERROR                                                            
*                                                                               
         BAS   RE,CHKPAY                                                        
         BAS   RE,CHKBIL                                                        
*                                                                               
         L     RF,A03INDX                                                       
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(32),0(RF)                                                   
*                                                                               
         LA    RF,WORK                                                          
         USING NUSPRD,RF                                                        
*                                                                               
         MVC   SVTPROD,NUSPRTPR    SAVE AWAY TRAFFIC PRODUCT                    
         MVI   NUSPRLEN,NUSPRLN4                                                
         DROP  RF                                                               
*                                                                               
         BAS   RE,BUILD            BUILDS NEW X'03' ELEMENT IN WORK             
*                                                                               
         L     RF,A03INDX                                                       
         MVC   0(32,RF),WORK       MOVE IT INTO TABLE                           
*                                                                               
         BAS   RE,PAYEDIT                                                       
*                                                                               
VSR50    DS    0H                  BUMP TO NEXT RATE IN SCREEN & TABLE          
         L     RF,A03INDX                                                       
         LA    RF,33(RF)                                                        
         ST    RF,A03INDX                                                       
*                                                                               
         LA    R2,SPRTYP2H-SPRTYP1H(R2)                                         
         BCT   R3,VSR20                                                         
*                                                                               
VSR100   DS    0H                  FILL IN SEQUENCE #'S                         
         L     R3,AIOAREA2                                                      
         MVI   SRSEQ,X'01'                                                      
*                                                                               
VSR110   DS    0H                                                               
         CLI   0(R3),X'FF'         END OF TABLE?                                
         BE    VSR120                                                           
*                                                                               
         TM    32(R3),X'80'        DELETE THIS RATE?                            
         BO    VSR115                                                           
*                                                                               
         MVI   2(R3),0             INIT SEQ# TO ZERO                            
         CLI   3(R3),C'F'          ASSIGNED CASH%?                              
         BE    VSR115                                                           
         CLI   3(R3),C'C'          ACTUAL CASH%?                                
         BE    VSR115                                                           
*                                                                               
         MVC   2(1,R3),SRSEQ       MOVE IN SEQUENCE #                           
         ZIC   RF,SRSEQ                                                         
         AHI   RF,1                                                             
         STC   RF,SRSEQ                                                         
*                                                                               
VSR115   DS    0H                                                               
         LA    R3,33(R3)                                                        
         B     VSR110                                                           
*                                                                               
VSR120   DS    0H                  ADD UPDATED ELEMENTS TO RECORD               
         L     R3,AIOAREA2                                                      
*                                                                               
VSR125   DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    VSR140                                                           
*                                                                               
         CLI   3(R3),C'F'          ASSIGNED CASH%?                              
         BE    VSR130                                                           
*                                                                               
         TM    32(R3),X'80'        DELETE THIS ONE?                             
         BO    VSR130                                                           
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(32),0(R3)                                                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,WORK,0                       
         MVC   SV03INDX,SV03FRST                                                
*                                                                               
VSR130   DS    0H                                                               
         LA    R3,33(R3)                                                        
         B     VSR125                                                           
*                                                                               
VSR140   DS    0H                                                               
*                                                                               
VSRX     DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* GET ADDRESS OF FIRST AVAILABLE ENTRY IN X'03' TABLE                           
*                                                                               
E03TAB   NTR1                                                                   
         L     RF,AIOAREA2                                                      
*                                                                               
E03T10   DS    0H                                                               
         CLI   0(RF),X'FF'         FOUND MATCH?                                 
         BE    E03TABX                                                          
*                                                                               
         LA    RF,33(RF)                                                        
         B     E03T10                                                           
*                                                                               
E03TABX  DS    0H                                                               
         ST    RF,A03INDX          FIRST AVAILABLE POSITION IN TAB              
         B     EXXMOD                                                           
*                                                                               
* GET ADDRESS IN X'03' TABLE INTO A03INDX                                       
*                                                                               
F03FRST  NTR1                                                                   
         L     RF,AIOAREA2                                                      
         CLI   0(RF),X'FF'         ANY SPECIAL RATES ON THIS UNIT YET?          
         BE    ASRERR                                                           
*                                                                               
F03F10   DS    0H                                                               
         CLI   0(RF),X'FF'         FOUND MATCH?                                 
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLC   SV03FRST,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,33(RF)                                                        
         B     F03F10                                                           
*                                                                               
         ST    RF,A03INDX          SAVE AWAY ADDR. IN TABLE                     
*                                                                               
F03FX    DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* ROUTINE TO EDIT SCREEN AND BUILD UNIT RECORD                                  
* R2 POINTS TO FIRST FIELD ON A LINE                                            
*                                                                               
BUILD    NTR1                                                                   
         LA    R4,BLOCK            INITIALIZE EDIT BLOCK AND CALL               
         USING UNBLOCKD,R4         EDIT FOR INITIALIZATION                      
*                                                                               
         LA    R5,WORK                                                          
         USING NUSPRD,R5                                                        
*                                                                               
         XC    NUSPRTYP,NUSPRTYP                                                
*                                                                               
         LA    R1,TYPTAB                                                        
         LA    R3,13                                                            
BLD020   CLC   8(2,R2),0(R1)                                                    
         BE    BLD040                                                           
         LA    R1,3(R1)                                                         
         BCT   R3,BLD020                                                        
         MVI   FERN,INVERR                                                      
         ST    R2,FADDR                                                         
         B     ERROR                                                            
*                                                                               
BLD040   DS    0H                                                               
         ST    R2,FADDR                                                         
         CLI   2(R1),C'F'          ASSIGNED CASH% - DISPLAY ONLY                
         BE    BLD042                                                           
         CLI   2(R1),C'Q'          EARNED COST - DISPLAY ONLY                   
         BE    BLD042                                                           
         CLI   2(R1),C'C'          ACTUAL CASH%- DISPLAY ONLY                   
         BNE   *+12                                                             
BLD042   TM    4(R2),X'80'         USER ENTERED?                                
         BO    ERROR                                                            
*                                                                               
         L     RE,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,RE                                                       
         CLI   2(R1),C'M'          MIDAS ELEMENT - MIDAS STATION ONLY           
         BNE   *+12                                                             
         TM    HEADFLG1,HMIDFLG                                                 
         BZ    ERROR                                                            
         DROP  RE                                                               
*                                                                               
         MVC   NUSPRTYP,2(R1)                                                   
*                                                                               
         BAS   RE,BUMPFLD          FIND NEXT FIELD                              
         BAS   RE,BUMPFLD          FIND NEXT FIELD                              
*                                                                               
* EDIT THE AMOUNT FIELD                                                         
*                                                                               
         NI    MYFLAG,X'FF'-CUTIN                                               
         CLI   2(R1),C'U'                                                       
         BNE   *+8                                                              
         OI    MYFLAG,CUTIN                                                     
*                                                                               
BLD045   BAS   RE,ASS                                                           
*                                                                               
BLD060   BAS   RE,BUMPFLD          FIND NEXT FIELD                              
*                                                                               
* EDIT THE SREP FIELD                                                           
*                                                                               
         MVI   FERN,INVERR                                                      
         ST    R2,FADDR                                                         
         CLI   NUSPRTYP,C'M'        TEST FOR MIDAS ELEMENT                      
         BNE   *+12                                                             
         CLI   5(R2),0              SREP NOT ALLOWED FOR MIDAS                  
         BNE   ERROR                                                            
*                                                                               
         MVC   THREE,=3X'00'                                                    
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB          GENERAL EDIT MODULE                          
         SPACE                                                                  
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         LA    RE,TEMPD+1500                                                    
         ST    RE,UNALOCAL         LOCAL STORAGE TO EDIT MODULE                 
*                                                                               
         CLI   ACTION,CSR          TEST FOR ACTION CHANGE                       
         BNE   BLD080              NO                                           
         MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
*                                                                               
BLD080   GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
*                                                                               
         LA    R3,EDLIST           POINT R3 AT EDIT LIST                        
         L     RF,VEDIT                                                         
         LA    R1,DMCB                                                          
*                                                                               
BLD100   CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    BLD100                                                           
         ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVC   UNEDATA,0(R3)       SET DATA TYPE                                
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    BLD110                                                           
         GOTO1 VEDIT,(R1),(C'S',(R4))                                           
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    *+14                NONE                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
BLD110   DS    0H                                                               
         MVC   NUSPRREP,THREE                                                   
*                                                                               
BLD120   LA    R3,1(R3)                                                         
         BAS   RE,BUMPFLD          FIND NEXT FIELD                              
*                                                                               
* EDIT THE COMMISION FIELD                                                      
*                                                                               
*                                                                               
         MVI   FERN,INVERR                                                      
         ST    R2,FADDR                                                         
         CLI   NUSPRTYP,C'M'        TEST FOR MIDAS ELEMENT                      
         BNE   *+8                                                              
         MVI   8(R2),C'N'           COMMISION NOT ALLOWED FOR MIDAS             
*                                                                               
         LA    R1,TYPTAB1                                                       
         LA    R3,2                                                             
BLD140   CLC   8(1,R2),0(R1)                                                    
         BE    BLD160                                                           
         LA    R1,3(R1)                                                         
         BCT   R3,BLD140                                                        
         MVI   NUSPRCOM,C'C'                                                    
         B     *+10                                                             
BLD160   MVC   NUSPRCOM,2(R1)                                                   
*                                                                               
* VALIDATE THE CUT-IN STATION FIELD                                             
*                                                                               
         BAS   RE,BUMPFLD                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,INVERR                                                      
         CLI   NUSPRTYP,C'M'        TEST FOR MIDAS ELEMENT                      
         BNE   *+12                                                             
         CLI   5(R2),0              CUT-IN STA NOT ALLOWED FOR MIDAS            
         BNE   ERROR                                                            
*                                                                               
         XC    NUSPRCIS,NUSPRCIS                                                
*                                                                               
         CLI   8(R2),0                                                          
         BE    BLD200                                                           
*                                                                               
         TM    MYFLAG,CUTIN        ONLY VALID FOR CUT-INS                       
         BZ    CIERR                                                            
*                                                                               
* SWITCH TO SPOT SYSTEM                                                         
*                                                                               
         GOTO1 ASWITCH,DMCB,=C'SPT',0                                           
         CLI   4(R1),0                                                          
         BE    BLD165                                                           
*                                                                               
         MVI   FERN,111            USER NOT AUTHORIZED ERROR                    
         B     ERROR                                                            
*                                                                               
BLD165   DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'T'                                                       
         MVC   KEY+2(4),8(R2)                                                   
         MVI   KEY+6,C'T'                                                       
         OC    KEY+2(5),SPACES                                                  
         MVC   KEY+7(2),NBSELAGY   AGENCY                                       
         MVC   KEY+9(3),=C'000'    AGENCY SPECIFIC                              
*                                                                               
         GOTO1 AIO,DMCB,STA+FILE+HIGH,AIOAREA4                                  
         CLC   KEY(9),KEYSAVE                                                   
         BNE   STAERR                                                           
*                                                                               
         MVC   SVSTA,KEY+2         CUT-IN STATION                               
         MVI   SVSTA+4,C'T'        MUST BE TV                                   
         L     RF,AIOAREA4                                                      
         MVC   SVMKT,18(RF)        MARKET #                                     
*                                                                               
         GOTO1 VMSPACK,DMCB,SVMKT,SVSTA,DUB                                     
         CLI   0(R1),X'FF'                                                      
         BE    STAERR                                                           
*                                                                               
         TM    4(R2),X'80'                                                      
         BZ    BLD180                                                           
*                                                                               
         LA    RF,STATAB           CHECK IF DUPLICATE CUT-IN STATION            
*                                                                               
BLD170   CLI   0(RF),X'FF'                                                      
         BE    BLD180                                                           
         CLC   DUB+2(3),0(RF)                                                   
         BE    STAERR2                                                          
         LA    RF,3(RF)                                                         
         B     BLD170                                                           
*                                                                               
BLD180   MVC   NUSPRCIS,DUB                                                     
*                                                                               
* SWITCH BACK TO NET SYSTEM                                                     
*                                                                               
         GOTO1 ASWITCH,DMCB,=C'NET',0                                           
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
BLD200   DS    0H                                                               
         MVI   NUSPRBPR,0                                                       
         MVI   NUSPRTPR,0                                                       
*                                                                               
* VALIDATE THE BILLING PRODUCT                                                  
*                                                                               
         BAS   RE,BUMPFLD                                                       
         ST    R2,FADDR                                                         
         CLI   8(R2),0                                                          
         BE    BLD220                                                           
         MVI   FERN,INVERR                                                      
         CLI   NUSPRTYP,C'M'        TEST FOR MIDAS ELEMENT                      
         BE    ERROR                BILLING PROD NOT ALLOWED FOR MIDAS          
*                                                                               
         CLI   NUSPRTYP,C'F'                                                    
         BE    BLD220                                                           
         CLI   NUSPRTYP,C'C'                                                    
         BE    BLD220                                                           
*                                                                               
         BAS   RE,VPROD                                                         
         MVC   NUSPRBPR,PROD                                                    
         MVC   NUSPRBPC,PRODA                                                   
*                                                                               
BLD220   DS    0H                                                               
*                                                                               
* VALIDATE THE TRAFFIC PRODUCT                                                  
*                                                                               
         BAS   RE,BUMPFLD                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,INVERR                                                      
         CLI   NUSPRTYP,C'M'        TEST FOR MIDAS ELEMENT                      
         BNE   *+12                                                             
         CLI   5(R2),0              TRAFFIC POD NOT ALLOWED FOR MIDAS           
         BNE   ERROR                                                            
*                                                                               
         XC    FEEDA,FEEDA                                                      
*                                                                               
         CLI   8(R2),0                                                          
         BE    BLD230                                                           
*                                                                               
         TM    MYFLAG,CUTIN                                                     
         BO    BLD222                                                           
*                                                                               
         CLI   8(R2),C'*'         FEED ONLY, NOT A CUT-IN                       
         BNE   FEEDERR2                                                         
         B     BLD226                                                           
*                                                                               
BLD222   DS    0H                                                               
         BAS   RE,VPROD                                                         
*                                                                               
         CLI   ACTION,CSR          CHANGING THIS RATE?                          
         BNE   BLD225                                                           
         CLC   SVTPROD,PROD        USER CHANGE TRAFFIC PRODUCT?                 
         BE    *+8                                                              
         OI    NUSPRSTA,X'80'      TRAFFIC PRODUCT CHANGE                       
*                                                                               
BLD225   DS    0H                                                               
         MVC   NUSPRTPR,PROD                                                    
         MVC   NUSPRTPC,PRODA                                                   
*                                                                               
BLD226   DS    0H                                                               
         BAS   RE,VFEED                                                         
         MVC   NUSPRFED(4),FEEDA                                                
         B     BLD232                                                           
*                                                                               
BLD230   DS    0H                                                               
         TM    MYFLAG,CUTIN                                                     
         BZ    BLDX                                                             
*                                                                               
BLD232   DS    0H                                                               
*!!!     MVI   NUSPRLEN,NUSPRLN3   CUT-IN ELEM LENGTH                           
         ZIC   RF,NUSPRLEN         ELEM LENGTH                                  
*                                                                               
         OC    FEEDA,FEEDA                                                      
         BZ    BLD235                                                           
*                                                                               
         LA    RF,1(RF)                                                         
         CLI   FEEDA+1,C' '                                                     
         BE    BLD235                                                           
         LA    RF,1(RF)                                                         
         CLI   FEEDA+2,C' '                                                     
         BE    BLD235                                                           
         LA    RF,1(RF)                                                         
         CLI   FEEDA+3,C' '                                                     
         BE    BLD235                                                           
         LA    RF,1(RF)                                                         
*                                                                               
BLD235   DS    0H                                                               
*!!!     STC   RF,NUSPRLEN                                                      
         DROP  R5                                                               
*                                                                               
BLDX     DS    0H                                                               
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
         B     EXXMOD                                                           
*                                                                               
TYPTAB   DC    CL2'CI',C'U'                                                     
         DC    CL2'BO',C'B'                                                     
         DC    CL2'CS',C'S'                                                     
         DC    CL2'OT',C'O'                                                     
         DC    CL2'TX',C'X'                                                     
         DC    CL2'SE',C'E'                                                     
         DC    CL2'AD',C'A'                                                     
         DC    CL2'LC',C'L'                                                     
         DC    CL2'DA',C'D'                                                     
         DC    CL2'EC',C'Q'                                                     
         DC    CL2'BC',C'C'                                                     
         DC    CL2'BA',C'F'                                                     
         DC    CL2'TC',C'M'                                                     
TYPTAB1  DC    CL2'Y ',C'C'                                                     
         DC    CL2'N ',C' '                                                     
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* GET SYSTEM EQUATES                                                            
*                                                                               
GETSEQ   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5REC,R4                                                        
*                                                                               
         MVI   CT5KTYP,C'5'                                                     
         MVC   CT5KALPH,NBSELAGY   ALPHA AGENCY                                 
*                                                                               
*!!!     GOTO1 AIO,DMCB,CTL+FILE+HIGH,AIOAREA4                                  
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIOAREA4             
         CLC   KEY(L'CT5KEY),KEYSAVE     MAKE SURE KEY IS THERE                 
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
         XC    SVSPTSE,SVSPTSE                                                  
         XC    SVNETSE,SVNETSE                                                  
*                                                                               
         L     R4,AIOAREA4                                                      
         LA    R4,CT5DATA                                                       
*                                                                               
GSEQ20   DS    0H                                                               
         CLI   0(R4),X'21'                                                      
         BE    GSEQ40                                                           
*                                                                               
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GSEQ20                                                           
*                                                                               
GSEQ40   DS    0H                                                               
         CLI   0(R4),X'21'                                                      
         BNE   GSEQ60                                                           
*                                                                               
         CLI   2(R4),2             SPOT                                         
         BNE   *+14                                                             
         MVC   SVSPTSE,3(R4)                                                    
         B     GSEQ50                                                           
*                                                                               
         CLI   2(R4),3             NET                                          
         BNE   *+10                                                             
         MVC   SVNETSE,3(R4)                                                    
*                                                                               
GSEQ50   DS    0H                                                               
         ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     GSEQ40                                                           
*                                                                               
GSEQ60   DS    0H                                                               
         CLI   SVSPTSE,0                                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   SVNETSE,0                                                        
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
GETSEQX  DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* DISPLAY THE PRODUCT ENTERED                                                   
*                                                                               
DPROD    NTR1                                                                   
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
         LA    RF,CLILIST          CLIENT PRODUCT LIST                          
*                                                                               
DPROD10  DS    0H                                                               
         OC    0(4,RF),0(RF)       ANY MORE PRODUCTS?                           
         BNZ   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLC   PROD,3(RF)          VALID PRODUCT?                               
         BNE   *+14                                                             
         MVC   PRODA,0(RF)         SAVE 3 BYTE PRODUCT CODE                     
         B     DPRODX                                                           
*                                                                               
         LA    RF,4(RF)                                                         
         B     DPROD10                                                          
*                                                                               
DPRODX   DS    0H                                                               
         OC    PROD,SPACES                                                      
         B     EXXMOD                                                           
*                                                                               
* VALIDATE THE PRODUCT ENTERED                                                  
*                                                                               
VPROD    NTR1                                                                   
         ST    R2,FADDR                                                         
*                                                                               
         XC    PRODA,PRODA                                                      
         LA    R2,8(R2)                                                         
         LA    RF,PRODA                                                         
         LA    R3,3                MAX 3 CHARACTERS                             
*                                                                               
VPROD5   DS    0H                                                               
         CLI   0(R2),C'/'                                                       
         BE    VPROD7                                                           
         MVC   0(1,RF),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         BCT   R3,VPROD5                                                        
*                                                                               
VPROD7   DS    0H                                                               
         OC    PRODA,SPACES                                                     
         XC    PROD,PROD                                                        
         L     R2,FADDR                                                         
*                                                                               
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R5                                                       
*                                                                               
VPROD10  DS    0H                                                               
         XC    KEY,KEY                                                          
         L     RF,NBAIO                                                         
         MVC   KEY(2),=X'0DF1'                                                  
         MVC   KEY+2(1),1(RF)      AGY/MED                                      
         MVC   KEY+3(2),NBSELCL2   CLIENT                                       
         MVC   KEY+6(3),PRODA      PRODUCT                                      
*                                                                               
         CLC   PRODA,=C'POL'                                                    
         BE    PRODERR                                                          
         CLC   PRODA,=C'AAA'                                                    
         BE    PRODERR                                                          
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(9),KEYSAVE     VALID PRODUCT?                                
         BNE   PRODERR             NO                                           
*                                                                               
         MVC   PROD,KEY+10        BINARY PRD + 1  (0 FOR EXTRA PRODS)           
*                                                                               
         XC    KEY,KEY                                                          
         L     RF,NBAIO                                                         
         MVC   KEY+1(1),1(RF)      AGY/MED                                      
         MVC   KEY+2(2),NBSELCL2   CLIENT                                       
         MVC   KEY+4(3),PRODA      PRODUCT                                      
         MVC   KEY+7(1),NBSELEST   ESTIMATE                                     
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(13),KEYSAVE     VALID PRODUCT?                               
         BNE   PRODERR             NO                                           
*                                                                               
         B     VPRODX                                                           
*                                                                               
VPRODX   DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* VALIDATE THE TRAFFIC PRODUCT FEED                                             
*                                                                               
VFEED    NTR1                                                                   
         ST    R2,FADDR                                                         
*                                                                               
         CLI   8(R2),C'*'         FEED ONLY, NOT A CUT-IN                       
         BNE   *+12                                                             
         LA    R2,8(R2)                                                         
         B     VFEED10                                                          
*                                                                               
         LA    R2,10(R2)                                                        
         CLI   0(R2),C'/'                                                       
         BE    VFEED10                                                          
         LA    R2,1(R2)                                                         
         CLI   0(R2),C'/'                                                       
         BNE   VFEEDX                                                           
*                                                                               
VFEED10  DS    0H                                                               
         MVC   FEEDA,1(R2)                                                      
         OC    FEEDA,SPACES                                                     
         L     R2,FADDR                                                         
*                                                                               
         LA    R3,KEY              CHECK CLIENT LEVEL                           
         USING FEEDRECD,R3                                                      
*                                                                               
         L     RF,NBAIO                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   FEEDKID,=XL2'0A2B'                                               
         MVC   FEEDKAM,1(RF)       AGENCY/MEDIA                                 
         MVC   FEEDKNET,NBSELNET   NETWORK                                      
         OC    FEEDKNET,SPACES                                                  
*                                                                               
         MVC   FEEDKCLT,2(RF)      CLIENT                                       
         MVC   FEEDKFD,FEEDA                                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(13),KEYSAVE     VALID FEED?                                  
         BE    VFEED50             YES                                          
*                                                                               
         LA    R3,KEY              CHECK AGENCY LEVEL                           
         USING FEEDRECD,R3                                                      
*                                                                               
         L     RF,NBAIO                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   FEEDKID,=XL2'0A2B'                                               
         MVC   FEEDKAM,1(RF)       AGENCY/MEDIA                                 
         MVC   FEEDKNET,NBSELNET   NETWORK                                      
         OC    FEEDKNET,SPACES                                                  
         MVC   FEEDKFD,FEEDA                                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 AIO,DMCB,SPT+DIR+HIGH                                            
         CLC   KEY(13),KEYSAVE     VALID FEED?                                  
         BNE   FEEDERR             NO                                           
*                                                                               
VFEED50  DS    0H                                                               
         TM    MYFLAG,SMAP         IS THIS A SECTIONAL MAP?                     
         BZ    VFEEDX                                                           
*                                                                               
         GOTO1 AIO,DMCB,SPT+FILE+GET,AIOAREA4                                   
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SMAPERR                                                          
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',SPTFILE),(X'40',AIOAREA4),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   SMAPERR                                                          
*                                                                               
         L     RF,12(R1)                                                        
         USING FEEDSMEL,RF                                                      
*                                                                               
         CLC   SVMYR,FEEDSMYR      SAME MAP YEAR?                               
         BNE   SMAPERR                                                          
         CLC   SVMCODE,FEEDSMCD    SAME MAP CODE?                               
         BNE   SMAPERR                                                          
         DROP  RF                                                               
*                                                                               
VFEEDX   DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* ASSIGNED COST                                                                 
*                                                                               
ASS      NTR1                                                                   
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
*                                                                               
         MVC   FLDH(18),0(R2)                                                   
*                                                                               
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    ASSX                                                             
         ZIC   R1,FLDH+5                                                        
         LA    RE,FLD-1(R1)        LOOK AT LAST CHARACTER                       
         CLI   0(RE),C'-'          TEST FOR A DASH                              
         BNE   ASS4                                                             
         MVI   0(RE),C' '                                                       
         MVI   MINUSSW,YES                                                      
         SH    R1,=H'1'                                                         
         BZ    ASSR                ITS AN ERROR - ONLY DASH IN FIELD            
         SPACE 1                                                                
ASS4     CLI   FLD,C'N'                                                         
         BNE   ASS8                                                             
         CLI   WORK+3,C'M'         NET COST NOT ALLOWED FOR MIDAS               
         BE    ASSR                                                             
         MVI   NETSW,YES                                                        
         SH    R1,=H'1'                                                         
         BZ    ASSR                ITS AN ERROR - ONLY DASH IN FIELD            
         MVC   FLD(78),FLD+1                                                    
         SPACE 1                                                                
ASS8     LR    R0,R1               DATA LENGTH                                  
         GOTO1 VCASHVAL,DMCB2,FLD,(R0)                                          
         CLI   0(R1),X'FF'                                                      
         BE    ASSR                                                             
         ICM   RF,15,4(R1)         GET AMOUNT                                   
         CLI   NETSW,YES                                                        
         BNE   ASS10                                                            
         SR    RE,RE               GROSS UP THE NET VALUE                       
         LA    R1,100                                                           
         MR    RE,R1                                                            
         SR    RE,RE                                                            
         LA    R1,85                                                            
         DR    RE,R1                                                            
*                                                                               
ASS10    CLI   MINUSSW,YES         TEST FOR MINUS AMOUNT                        
         BNE   *+6                                                              
         LNR   RF,RF               YES-FORCE COST NEGATIVE                      
         STCM  RF,15,WORK+5                                                     
*                                                                               
ASSX     XC    MINUSSW(2),MINUSSW  CLEAR INDICATORS                             
         B     EXXMOD                                                           
         SPACE 1                                                                
ASSR     MVI   FERN,INVERR                                                      
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
* BUILD TABLE OF ALL CUT-IN STAIONS                                             
*                                                                               
BSTATAB  NTR1                                                                   
         XC    STATAB,STATAB                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BSTATABX                                                         
*                                                                               
         L     R4,12(R1)                                                        
         USING NUSPRD,R4                                                        
*                                                                               
         LA    R3,STATAB                                                        
         MVI   0(R3),X'FF'                                                      
*                                                                               
BSTA10   DS    0H                                                               
         CLI   0(R4),X'03'         ANY MORE ELEMENTS?                           
         BNE   BSTA20              NO                                           
*                                                                               
         CLI   NUSPRTYP,C'U'       CUT-IN?                                      
         BNE   BSTA15                                                           
         OC    NUSPRCIS+2(3),NUSPRCIS+2    ANY CUT-IN STATION?                  
         BZ    BSTA15                                                           
*                                                                               
         MVC   0(3,R3),NUSPRCIS+2  SAVE CUT-IN STATION                          
         LA    R3,3(R3)                                                         
*                                                                               
BSTA15   ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
         B     BSTA10                                                           
*                                                                               
BSTA20   DS    0H                                                               
         MVI   0(R3),X'FF'                                                      
*                                                                               
BSTATABX DS    0H                                                               
         B     EXXMOD                                                           
         DROP  R4                                                               
*                                                                               
* BUILD TABLE OF ALL X'03' ELEMENTS                                             
*                                                                               
BSRTAB   NTR1                                                                   
         L     RE,AIOAREA2                                                      
         LA    RF,2000                                                          
         XCEF                                                                   
*                                                                               
         L     RE,AIOAREA2                                                      
         MVI   0(RE),X'FF'                                                      
*                                                                               
         L     R3,AIOAREA2                                                      
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'76',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BSRT05                                                           
         L     R4,12(R1)                                                        
*                                                                               
         MVC   0(NUASCLN4,R3),0(R4)   ASS. COST CREDIT ELEM                     
         LA    R3,33(R3)                                                        
*                                                                               
BSRT05   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   BSRTABX                                                          
         L     R4,12(R1)                                                        
*                                                                               
BSRT10   DS    0H                                                               
         CLI   0(R4),X'03'         ANY MORE ELEMENTS?                           
         BNE   BSRT20              NO                                           
*                                                                               
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       SAVE AWAY ELEMENT                            
*                                                                               
         ZIC   RF,1(R4)            BUMP TO NEXT ELEMENT                         
         AR    R4,RF                                                            
*                                                                               
         LA    R3,33(R3)                                                        
         B     BSRT10                                                           
*                                                                               
BSRT20   DS    0H                                                               
         MVI   0(R3),X'FF'                                                      
*                                                                               
BSRTABX  DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
* SUB-ROUTINE TO DISPLAY UNIT RECORD VALUES                                     
*                                                                               
DISUNIT  NTR1                                                                   
         TM    FLAGS,NEWBILL                                                    
         BZ    DIS004                                                           
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
*                                                                               
         L     R4,12(R1)                                                        
         USING NUSDRD,R4                                                        
*                                                                               
         TM    NUSDST4,X'04'      LIMIT SPECIAL CHARGE SCREEN?                  
         BZ    *+8                                                              
         NI    FLAGS,X'FF'-NEWBILL                                              
         DROP  R4                                                               
*                                                                               
DIS004   DS    0H                                                               
*  CLEAR THE SCREEN                                                             
         BAS   RE,CLRSCRN                                                       
         LA    R2,SPRTYP1H                                                      
*        GOTO1 VCLEARF,DMCB,(R2),SPRLAST                                        
*                                                                               
         OC    SV03INDX,SV03INDX   MIDDLE OF SCROLLING?                         
         BZ    DIS010                                                           
         CLI   SV03INDX,X'03'      MUST BE X'03'                                
         BNE   DIS005                                                           
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(1),SV03INDX+2                                                
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),(1,DUB)              
         CLI   12(R1),0                                                         
         BE    DIS020                                                           
DIS005   XC    SV03INDX,SV03INDX                                                
*                                                                               
DIS010   DS    0H                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'76',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   DIS008                                                           
         L     R4,12(R1)                                                        
         B     DIS025                                                           
*                                                                               
DIS008   GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    DISEXIT             NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    DIS020                                                           
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
*                                                                               
DIS020   L     R4,12(R1)                                                        
         USING NUSPRD,R4                                                        
*                                                                               
DIS025   MVC   SV03FRST,0(R4)      SAVE AWAY FIRST RATE LISTED                  
         LA    R2,SPRTYP1H                                                      
         LA    R3,13                                                            
*                                                                               
* FIND RECORD TYPE                                                              
DIS040   DS    0H                                                               
         NI    MYFLAG,X'FF'-DISONLY                                             
         NI    MYFLAG,X'FF'-PAID                                                
*                                                                               
         NI    1(R2),X'FF'-X'20'   TURN OFF PROTECT BIT                         
*                                                                               
         TM    FLAGS,NEWBILL                                                    
         BO    DIS042                                                           
*                                                                               
DIS042   TM    NUSPRSTA,NUSPRCPD   CHARGE PAID?                                 
         BZ    *+12                                                             
         OI    MYFLAG,PAID                                                      
         OI    1(R2),X'20'                                                      
*                                                                               
DIS045   CLI   NUSPRTYP,C'U'                                                    
         BNE   DIS050                                                           
         MVC   8(2,R2),=C'CI'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(6,R2),=C'CUT-IN'                                               
         B     DIS133                                                           
DIS050   CLI   NUSPRTYP,C'B'                                                    
         BNE   DIS060                                                           
         MVC   8(2,R2),=C'BO'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(8,R2),=C'BLACKOUT'                                             
         B     DIS133                                                           
DIS060   CLI   NUSPRTYP,C'S'                                                    
         BNE   DIS070                                                           
         MVC   8(2,R2),=C'CS'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(10,R2),=C'COPY-SPLIT'                                          
         B     DIS133                                                           
DIS070   CLI   NUSPRTYP,C'X'                                                    
         BNE   DIS080                                                           
         MVC   8(2,R2),=C'TX'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(3,R2),=C'TAX'                                                  
         B     DIS133                                                           
DIS080   CLI   NUSPRTYP,C'E'                                                    
         BNE   DIS090                                                           
         MVC   8(2,R2),=C'SE'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(9,R2),=C'SECTIONAL'                                            
         B     DIS133                                                           
DIS090   CLI   NUSPRTYP,C'A'                                                    
         BNE   DIS100                                                           
         MVC   8(2,R2),=C'AD'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(5,R2),=C'ADMIN'                                                
         B     DIS133                                                           
DIS100   CLI   NUSPRTYP,C'L'                                                    
         BNE   DIS110                                                           
         MVC   8(2,R2),=C'LC'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(9,R2),=C'LATE CHRG'                                            
         B     DIS133                                                           
DIS110   CLI   NUSPRTYP,C'D'                                                    
         BNE   DIS115                                                           
         MVC   8(2,R2),=C'DA'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(9,R2),=C'DELIV ADJ'                                            
         B     DIS133                                                           
DIS115   CLI   NUSPRTYP,C'Q'                                                    
         BNE   DIS120                                                           
         MVC   8(2,R2),=C'EC'                                                   
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    MYFLAG,DISONLY                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(5,R2),=C'ECADJ'                                                
         B     DIS133                                                           
*                                                                               
DIS120   CLI   NUSPRTYP,C'C'       ACTUAL CASH %?                               
         BNE   DIS122                                                           
         MVC   8(2,R2),=C'BC'                                                   
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    MYFLAG,DISONLY                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(9,R2),=C'BARTR ACT'                                            
         B     DIS133                                                           
*                                                                               
DIS122   CLI   NUSPRTYP,C'F'       ASSIGNED CASH %?                             
         BNE   DIS124                                                           
         MVC   8(2,R2),=C'BA'                                                   
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    MYFLAG,DISONLY                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(9,R2),=C'BARTR ASG'                                            
         B     DIS133                                                           
*                                                                               
DIS124   CLI   NUSPRTYP,C'M'                                                    
         BNE   DIS131                                                           
         MVC   8(2,R2),=C'TC'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(10,R2),=C'TRD CREDIT'                                          
         B     DIS133                                                           
*                                                                               
DIS131   MVC   8(2,R2),=C'OT'                                                   
         BAS   RE,TRANFLD                                                       
         MVC   8(5,R2),=C'OTHER'                                                
DIS133   BAS   RE,TRANFLD          TRANSMIT FIELD                               
*                                                                               
         NI    1(R2),X'FF'-X'20'                                                
         TM    MYFLAG,DISONLY                                                   
         BZ    *+8                                                              
         OI    1(R2),X'20'         EARNED COST - DISPLAY ONLY                   
         TM    MYFLAG,PAID                                                      
         BZ    *+8                                                              
         OI    1(R2),X'20'         CHARGE PAID - DISPLAY ONLY                   
*                                                                               
* AMOUNT FIELD                                                                  
*                                                                               
         ICM   RE,15,NUSPRAMT                                                   
         BZ    DIS140                                                           
         TM    5(R4),X'80'         TEST FOR MINUS UNIT                          
         BZ    *+6                                                              
         LNR   RE,RE               CONVERT COST TO NEGATIVE NUMBER              
         LR    R0,RE               SAVE COST VALUE                              
         SRDA  RE,32               PREPARE DIVIDEND                             
         D     RE,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LA    R1,8(R2)            SET UP OUTPUT FIELD                          
         LTR   RE,RE               TEST REMAINDER (PENNIES)                     
         BNZ   DIS135              YES                                          
         ST    RF,FULL                                                          
         EDIT  FULL,(10,(R1)),ALIGN=LEFT,MINUS=YES                              
         BAS   RE,TRANFLD          TRANSMIT FIELD                               
         B     DIS145                                                           
*                                                                               
DIS135   ST    R0,FULL             RESTORE COST VALUE W PENNIES                 
         EDIT  FULL,(10,(R1)),2,ALIGN=LEFT,MINUS=YES                            
DIS140   BAS   RE,TRANFLD          TRANSMIT FIELD                               
*                                                                               
DIS145   NI    1(R2),X'FF'-X'20'                                                
         TM    MYFLAG,PAID                                                      
         BZ    *+8                                                              
         OI    1(R2),X'20'         CHARGE PAID - DISPLAY ONLY                   
*                                                                               
* SPECIAL REP FIELD                                                             
DIS150   MVC   8(3,R2),NUSPRREP                                                 
         OI    4(R2),X'08'         MARK AS NUMERIC DATA                         
         BAS   RE,TRANFLD          TRANSMIT FIELD                               
*                                                                               
         NI    1(R2),X'FF'-X'20'                                                
*                                                                               
         TM    MYFLAG,DISONLY                                                   
         BZ    *+8                                                              
         OI    1(R2),X'20'         EARNED COST - DISPLAY ONLY                   
*                                                                               
         TM    MYFLAG,PAID                                                      
         BZ    *+8                                                              
         OI    1(R2),X'20'         CHARGE PAID - DISPLAY ONLY                   
*                                                                               
* COMMISSION FIELD                                                              
         MVI   8(R2),C'N'                                                       
         CLI   NUSPRCOM,C'N'       IF NO, THEN NUSPRCOM IS N OR ' '             
         BE    DIS160                                                           
         CLI   NUSPRCOM,C' '                                                    
         BE    DIS160                                                           
         MVI   8(R2),C'Y'                                                       
DIS160   BAS   RE,TRANFLD          TRANSMIT FIELD                               
*                                                                               
         TM    FLAGS,NEWBILL                                                    
         BO    DIS160A                                                          
*                                                                               
         BAS   RE,BUMPFLD                                                       
         BAS   RE,BUMPFLD                                                       
         BAS   RE,BUMPFLD                                                       
         BAS   RE,BUMPFLD                                                       
         B     DIS180                                                           
*                                                                               
DIS160A  NI    1(R2),X'FF'-X'20'                                                
         TM    MYFLAG,DISONLY                                                   
         BZ    *+8                                                              
         OI    1(R2),X'20'         EARNED COST - DISPLAY ONLY                   
         TM    MYFLAG,PAID                                                      
         BZ    *+8                                                              
         OI    1(R2),X'20'         CHARGE PAID - DISPLAY ONLY                   
*                                                                               
         CLI   NUSPRLEN,NUSPRLN2   OLD STYLE X'03' ELEMENTS?                    
         BH    DIS161                                                           
         BAS   RE,BUMPFLD                                                       
*                                                                               
         CLI   NUSPRLEN,NUSPRLN1                                                
         BH    DIS163                                                           
         BAS   RE,BUMPFLD                                                       
         BAS   RE,BUMPFLD                                                       
         BAS   RE,BUMPFLD                                                       
         B     DIS180                                                           
*                                                                               
* NEW CUT-IN FIELDS HERE                                                        
*                                                                               
DIS161   OC    NUSPRCIS,NUSPRCIS                                                
         BZ    DIS162                                                           
*                                                                               
         GOTO1 VMSUNPK,DMCB,NUSPRCIS,SVMKT,SVSTA                                
         MVC   8(4,R2),SVSTA                                                    
*                                                                               
DIS162   BAS   RE,TRANFLD                                                       
*                                                                               
DIS163   DS    0H                                                               
         NI    1(R2),X'FF'-X'20'                                                
         TM    MYFLAG,DISONLY                                                   
         BZ    *+8                                                              
         OI    1(R2),X'20'         EARNED COST - DISPLAY ONLY                   
         TM    MYFLAG,PAID                                                      
         BZ    *+8                                                              
         OI    1(R2),X'20'         CHARGE PAID - DISPLAY ONLY                   
*                                                                               
         CLI   NUSPRLEN,NUSPRLN4                                                
         BE    DIS164                                                           
         CLI   NUSPRBPR,0          ANY BILLING PRODUCT                          
         BE    DIS165              NO                                           
         MVC   PROD,NUSPRBPR                                                    
         BAS   RE,DPROD                                                         
         MVC   8(3,R2),PRODA       BILLING PRODUCT                              
         B     DIS165                                                           
*                                                                               
DIS164   OC    NUSPRBPC,NUSPRBPC                                                
         BZ    DIS165                                                           
         MVC   PROD,NUSPRBPR                                                    
         MVC   8(3,R2),NUSPRBPC                                                 
*                                                                               
DIS165   BAS   RE,TRANFLD                                                       
*                                                                               
         NI    1(R2),X'FF'-X'20'                                                
         TM    MYFLAG,DISONLY                                                   
         BZ    *+8                                                              
         OI    1(R2),X'20'         EARNED COST - DISPLAY ONLY                   
         TM    MYFLAG,PAID                                                      
         BZ    *+8                                                              
         OI    1(R2),X'20'         CHARGE PAID - DISPLAY ONLY                   
*                                                                               
         CLI   NUSPRLEN,NUSPRLN4                                                
         BE    DIS165A                                                          
         CLI   NUSPRTPR,0          ANY TRAFFIC PRODUCT                          
         BE    DIS165AA            NO                                           
         MVC   PROD,NUSPRTPR                                                    
         BAS   RE,DPROD                                                         
         MVC   8(3,R2),PRODA       TRAFFIC PRODUCT                              
         B     DIS165C                                                          
*                                                                               
DIS165A  OC    NUSPRTPC,NUSPRTPC                                                
         BNZ   DIS165B                                                          
DIS165AA CLI   NUSPRLEN,NUSPRLN3   ANY FEED?                                    
         BNH   DIS170                                                           
         CLI   NUSPRFED,0                                                       
         BE    DIS170                                                           
*                                                                               
         ST    R2,DUB                                                           
         MVI   8(R2),C'*'                                                       
         LA    R2,9(R2)                                                         
         B     DIS168                                                           
*                                                                               
DIS165B  DS    0H                                                               
         MVC   PROD,NUSPRTPR                                                    
         MVC   8(3,R2),NUSPRTPC                                                 
*                                                                               
DIS165C  CLI   NUSPRTYP,C'U'       CUT-IN?                                      
         BNE   DIS170                                                           
         CLI   NUSPRLEN,NUSPRLN3   ANY FEED?                                    
         BNH   DIS170                                                           
         CLI   NUSPRFED,0                                                       
         BE    DIS170                                                           
*                                                                               
         ST    R2,DUB                                                           
         LA    R2,8(R2)                                                         
DIS166   CLI   0(R2),C' '          ANY MORE?                                    
         BE    DIS167                                                           
         CLI   0(R2),0                                                          
         BE    DIS167                                                           
*                                                                               
         LA    R2,1(R2)                                                         
         B     DIS166                                                           
*                                                                               
DIS167   MVI   0(R2),C'/'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
DIS168   DS    0H                                                               
         ZIC   R1,NUSPRLEN                                                      
         SHI   R1,NUSPRLN3         SUBTRACT OFF A(NUSPRFED)-NUSPRD              
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),NUSPRFED                                                 
         L     R2,DUB              RESTORE POINTER                              
*                                                                               
DIS170   BAS   RE,TRANFLD                                                       
*                                                                               
         BAS   RE,BUMPFLD                                                       
         DROP  R4                                                               
*                                                                               
* GET NEXT ELEMENT                                                              
*                                                                               
DIS180   DS    0H                                                               
         CLI   0(R4),X'76'         DISPLAYING ASS. COST CREDIT?                 
         BNE   DIS190                                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'03',AIOAREA1),0                    
         L     R4,12(R1)                                                        
         CLI   12(R1),6            TEST IF FOUND                                
         BE    DISEXIT             NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    DIS195                                                           
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
*                                                                               
DIS190   ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
*                                                                               
DIS195   CLI   0(R4),X'03'                                                      
         BNE   DISEXIT                                                          
         BCT   R3,DIS040           DISPLACEMENT FROM FIRST DISPLAY HDR.         
*                                                                               
DISEXIT  DS    0H                                                               
         MVC   SV03INDX,0(R4)      FOR SCROLLING                                
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY UNIT RECORD VALUES                                     
*                                                                               
CLRSCRN  NTR1                                                                   
*  CLEAR THE SCREEN                                                             
         LA    R1,13                                                            
         LA    R3,SPRTYP1H                                                      
*                                                                               
         TM    FLAGS,NEWBILL                                                    
         BO    CLRS020                                                          
*                                                                               
*&&DO                                                                           
         CLC   AGENCY,=C'H9'                                                    
         BE    CLRS020                                                          
         CLC   AGENCY,=C'SJ'                                                    
         BE    CLRS020                                                          
*&&                                                                             
         B     CLRS050                                                          
*                                                                               
CLRS020  LR    R2,R3                                                            
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(2,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         XC    8(10,R2),8(R2)                                                   
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(10,R2),8(R2)                                                   
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(3,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(1,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(4,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(3,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(8,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         LA    R3,SPRTYP2H-SPRTYP1H(R3)                                         
         BCT   R1,CLRS020                                                       
         B     CLRSX                                                            
*                                                                               
CLRS050  LR    R2,R3                                                            
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(2,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         XC    8(10,R2),8(R2)                                                   
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(10,R2),8(R2)                                                   
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(3,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         NI    1(R2),X'FF'-X'20'                                                
         XC    8(1,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         OI    1(R2),X'20'                                                      
         XC    8(4,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         OI    1(R2),X'20'                                                      
         XC    8(3,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         OI    1(R2),X'20'                                                      
         XC    8(8,R2),8(R2)                                                    
         MVI   5(R2),0                                                          
         BAS   RE,TRANFLD                                                       
         LA    R3,SPRTYP2H-SPRTYP1H(R3)                                         
         BCT   R1,CLRS050                                                       
*                                                                               
CLRSX    B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO CHECK TO SEE IF PAYABLE EXISTS FOR SPECIAL RATE TYPE           
*  SRPAYED=PAID INDICATOR FIELD                                                 
*  R3=A(PAYING ELEMENT)                                                         
*  R4=A(SPECIAL RATE ELEMENT)                                                   
CHKPAY   NTR1                                                                   
         XC    SRPAYED,SRPAYED                                                  
         CLI   NOELSW,X'FF'                                                     
         BE    CHKEXIT                                                          
         LR    R4,RF                                                            
         USING NUPAYD,R3                                                        
         USING NUSPRD,R4                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    CHKEXIT             NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R3,12(R1)                                                        
CHKPY050 CLC   NUPAYTYP,NUSPRTYP   FIND NEXT UNPROTECTED FIELD                  
         BNE   CHKPY100                                                         
         CLC   NUPAYSRP,NUSPRREP                                                
         BNE   CHKPY100                                                         
         MVI   SRPAYED,X'FF'                                                    
         B     CHKEXIT                                                          
CHKPY100 ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'12'                                                      
         BE    CHKPY050                                                         
CHKEXIT  B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CHECK TO SEE IF BILLABLE EXISTS FOR SPECIAL RATE TYPE          
*  SRBILED=BILL INDIOCATOR FIELD                                                
*  R3=A(BILLING ELEMENT)                                                        
*  R4=A(SPECIAL RATE ELEMENT)                                                   
CHKBIL   NTR1                                                                   
         XC    SRBILED,SRBILED                                                  
         XC    FULL,FULL                                                        
         CLI   NOELSW,X'FF'                                                     
         BE    CHKBEXIT                                                         
         LR    R4,RF                                                            
         USING NUBILD,R3                                                        
         USING NUSPRD,R4                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'10',AIOAREA1),0                    
         CLI   12(R1),6            TEST IF FOUND                                
         BE    CHKBEXIT            NO ELEMENT EXISTS EXIT ROUTINE               
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF BAD RETURN CODE                
         L     R3,12(R1)                                                        
CHKBL050 CLC   NUBILTYP,NUSPRTYP   FIND NEXT UNPROTECTED FIELD                  
         BNE   CHKBL100                                                         
         ICM   RE,15,FULL                                                       
         ICM   RF,15,NUBILGRS                                                   
         AR    RE,RF                                                            
         STCM  RE,15,FULL                                                       
CHKBL100 ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         CLI   0(R3),X'10'                                                      
         BE    CHKBL050                                                         
CHKBL150 OC    FULL,FULL                                                        
         BZ    CHKBEXIT                                                         
         MVI   SRBILED,X'FF'                                                    
CHKBEXIT B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO CHECK TO FLAG CHANGES TO PAID SPECIAL RATE CHARGES             
*  SRPAYED=PAID INDICATOR FIELD                                                 
*  RF=A(SPECIAL RATE ELEMENT)                                                   
*  WORK=UPDATED SPECIAL RATE ELEMENT                                            
PAYEDIT  NTR1                                                                   
         CLI   SRPAYED,X'FF'       DOES THIS SRATE HAVE PAYABLE AGAINST         
         BNE   PAYE050                                                          
*                                                                               
         MVI   FERN,REPDERR                                                     
         CLC   8(2,R2),=CL2'DE'    IS DELETE ACTION REQUESTED                   
         BE    ERROR                                                            
         MVI   FERN,REPCHERR                                                    
         CLC   9(3,RF),WORK+9      HAS REP BEEN CHANGED                         
         BNE   ERROR                                                            
         CLC   3(1,RF),WORK+3      HAS PAY TYPE BEEN CHANGED                    
         BNE   ERROR                                                            
*                                                                               
PAYE050  CLI   SRBILED,X'FF'       DOES THIS SRATE HAVE BILLING AGAINST         
         BNE   PAYEDEX                                                          
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   8(2,R2),=CL2'DE'    IS DELETE ACTION REQUESTED                   
         BE    ERROR                                                            
PAYEDEX  B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO TRANSMIT SCREEN FIELDS                                         
*                                                                               
*  CLEAR THE SCREEN                                                             
TRANFLD  OI    6(R2),X'80'         TRANSMIT FIELD                               
BUMPFLD  SR    R0,R0               FIND NEXT UNPROTECTED FIELD                  
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
PRODERR  DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'PRODERRQ),PRODERRQ                                      
         B     ERROR                                                            
*                                                                               
ASRERR   DS    0H                                                               
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'ASRERRQ),ASRERRQ                                        
         B     ERROR                                                            
*                                                                               
STAERR   DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'STAERRQ),STAERRQ                                        
         B     ERROR                                                            
*                                                                               
STAERR2  DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'STAERR2Q),STAERR2Q                                      
         B     ERROR                                                            
*                                                                               
CIERR    DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'CIERRQ),CIERRQ                                          
         B     ERROR                                                            
*                                                                               
CHERR    DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'CHERRQ),CHERRQ                                          
         B     ERROR                                                            
*                                                                               
FEEDERR  DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'FEEDERRQ),FEEDERRQ                                      
         B     ERROR                                                            
*                                                                               
FEEDERR2 DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'FEEDER2Q),FEEDER2Q                                      
         B     ERROR                                                            
*                                                                               
SMAPERR  DS    0H                                                               
         MVI   FERN,USERERR                                                     
         MVC   BUYMSG(L'SMAPERRQ),SMAPERRQ                                      
         B     ERROR                                                            
*                                                                               
* SUB-ROUTINE TO OUTPUT MESSAGE AND TO SET CURSOR                               
*                                                                               
MSG      ST    RE,SAVEREG                                                       
         LA    RE,ACTTAB                                                        
         LA    R0,ACTS             COUNTER                                      
         CLC   ACTION,0(RE)                                                     
         BE    *+14                                                             
         LA    RE,L'ACTTAB(RE)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         CLI   ACTION,ASR                                                       
         BNE   MSG1                                                             
         MVC   BUYMSG(21),=C'SPECIAL CHARGES ADDED'                             
         OI    BUYMSGH+6,X'80'                                                  
         BAS   RE,CLRSCRN                                                       
         B     MSGX                                                             
*                                                                               
MSG1     MVC   BUYMSG(4),=C'UNIT'                                               
         MVC   BUYMSG+5(9),1(RE)                                                
         LA    R2,BUYACTH          SET CURSOR POSITION                          
         TM    MODE,DISPLAY        TEST FOR CHANCE OF FORCED DISPLAY            
         BZ    MSGX                NO                                           
         SPACE                                                                  
MSG2     CLI   ACTION,CSR          TEST FOR ACTION BUY                          
         BE    MSGX                ALL DONE                                     
         CLI   ACTION,DSR          TEST FOR DISPLAY                             
         BE    MSGX                YES                                          
         CLI   ACTION,ASR                                                       
         BE    MSGX                                                             
         MVC   BUYMSG+5(9),=C'DISPLAYED'                                        
         LA    R3,BUYMSG+15                                                     
         LA    R2,SPRTYP1H                                                      
         MVC   0(15,R3),=C'- ENTER CHANGES'                                     
         CLI   ACTION,CSR                                                       
         BE    MSGX                                                             
         CLI   ACTION,ASR                                                       
         BE    MSGX                                                             
         MVC   0(20,R3),=C'- NOW YOU MAY DELETE'                                
         OI    1(R2),X'01'         CONVERT CURSOR TO MODIFIED                   
         B     MSGX                                                             
         SPACE                                                                  
MSGX     ST    R2,FADDR                                                         
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
* PATCH AREA                                                                    
*                                                                               
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
SPTFILE  DC    CL8'SPTFILE'                                                     
NOCHGERR DC    C'** ERROR - MAKE-GOOD DETAILS CANNOT BE CHANGED FOR UNIX        
               T**'                                                             
FEEDERRQ DC    C'** ERROR - INVALID FEED **'                                    
FEEDER2Q DC    C'** ERROR - ONLY VALID FOR CUT-INS **'                          
PRODERRQ DC    C'** ERROR - INVALID PRODUCT **'                                 
ASRERRQ  DC    C'** ERROR - MUST USE ASR ACTION **'                             
STAERRQ  DC    C'** ERROR - INVALID STATION **'                                 
STAERR2Q DC    C'** ERROR - DUPLICATE CUT-IN STATION **'                        
CIERRQ   DC    C'** ERROR - ENTRY ONLY VALID FOR CUT-INS **'                    
CHERRQ   DC    C'** ERROR - CAN NOT HAVE MORE THAN 35 CHARGES **'               
SMAPERRQ DC    C'** ERROR - FEED NOT ON SECTIONAL MAP **'                       
         SPACE 2                                                                
* LIST OF FIELDS TO EDIT                                                        
*                                                                               
EDLIST   DS    0H                                                               
         DC    AL1(USREP)                                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF ACTIONS AND THEIR NAMES                                              
*                                                                               
ACTTAB   DS    0CL10                                                            
         DC    AL1(CSR),CL9'CHANGED'                                            
         DC    AL1(DSR),CL9'DISPLAYED'                                          
         DC    AL1(ASR),CL9'ADDED'                                              
ACTS     EQU   (*-ACTTAB)/L'ACTTAB                                              
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
*                                                                               
* UNIT SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYF5D                                                       
*                                                                               
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    CL256                                                            
         ORG   SVDATA                                                           
SVDATE   DS    XL2                 SAVED AIR DATE                               
SVSUB    DS    X                   SAVED SUB-LINE                               
SV03FRST DS    XL3                 FIRST X'03' ELEM DISPLAYED ON SCREEN         
SV03INDX DS    XL3                 NEXT X'03' ELEM TO DISPLAY                   
         EJECT                                                                  
* DSECT TO COVER LOCAL MORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
VDISPLAY DS    V                   V(GENERAL UNIT DISPLAY)                      
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
*                                                                               
UNITDA   DS    XL4                 UNIT RECORD DISK ADDRESS                     
*                                                                               
FLAGS    DS    XL1                                                              
NEWBILL  EQU   X'01'               USE NEW BILL PRODUCT SCREEN                  
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
CUTIN    EQU   X'01'               THIS IS A CUT-IN                             
DISONLY  EQU   X'02'               DISPLAY ONLY                                 
PAID     EQU   X'04'               CHARGE WAS PAID                              
SMAP     EQU   X'08'               UNIT HAS A SECTIONAL MAP                     
*                                                                               
SVTPROD  DS    XL1                 SAVE AWAY TRAFFIC PRODUCT CODE               
PROD     DS    XL1                 1 BYTE PRODUCT CODE                          
PRODA    DS    CL3                 3 BYTE PRODUCT CODE ALPHA                    
FEEDA    DS    CL4                 4 BYTE FEED                                  
SRSEQ    DS    XL1                 SEQUENCE # FOR ELEMENT                       
SVSTA    DS    CL5                 SPOT STATION                                 
SVMKT    DS    CL4                 SPOT MARKET                                  
*                                                                               
ASWITCH  DS    A                   A(SWITCH)                                    
*                                                                               
SVSPTSE  DS    XL1                 SPOT SYSTEM EQUATE                           
SVNETSE  DS    XL1                 NET SYSTEM EQUATE                            
*                                                                               
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
CHARDATE DS    CL6                 DATE FROM ACTION FIELD - YYMMDD              
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
TIME     DS    X                   SQH (SAVED)                                  
*                                                                               
A03INDX  DS    A                   A(IN X'03' TABLE)                            
*                                                                               
MGCHGSW  DS    C                   MAKE-GOOD DETAIL CHANGE (Y/N)                
ESTLOOK  DS    C                   FORCE LOOKUP OF ESTIMATED DEMOS (Y)          
DDMMYYIP DS    X                   MAKE-GOOD YEAR INPUT                         
SVBYPR14 DS    X                   TELL EDIT TO SKIP MG OUT OF EST CK           
CAACTSW  DS    CL1                 ALLOW CHANGE TO PAYED UNIT                   
MAKEGDSW DS    CL1                 THIS IS OR WAS A MAKEGOOD                    
FRCSW    DS    CL1                 IF SINGLE MG USE IT'S OWN DEMO'S             
NOELSW   DS    CL1                 SPECIAL RATE ELEMENT SW                      
MINUSSW  DS    CL1                 NEGATIVE AMOUNT SW                           
NETSW    DS    CL1                 NET VALUE SW                                 
SRPAYED  DS    CL1                 SPECIAL RATE HAS PAID AGAINST IT             
SRBILED  DS    CL1                 SPECIAL RATE HAS BILLING AGAINST IT          
SVINTTBL DS    CL70                SAVED INTEGRATION TABLES                     
MGDETS   DS    0XL104                                                           
MGCODE   DS    CL6                 MAKE-GOOD PROGRAM CODE                       
MGDATE   DS    XL2                 MAKE-GOOD DATE                               
MGSUB    DS    X                   MAKE-GOOD SUB-LINE                           
MGPNAM   DS    CL16                MAKE-GOOD PROGRAM NAME                       
MGPSTAT  DS    X                   MAKE-GOOD STATUS                             
         DS    CL78                                                             
         DS    XL1                 0 FOR NEW MAKEGOODS                          
OMGDETS  DS    0CL36               OLD TABLE OF UP TO 4 MAKEGOODS               
OMGCODE  DS    CL6                 OLD PROGRAM CODE                             
OMGDATE  DS    XL2                 OLD DATE                                     
OMGSUB   DS    XL1                 OLD NUMBER                                   
         DS    CL27                                                             
         DS    XL1                 0 FOR OLD MAKEGOODS                          
*                                                                               
SVMYR    DS    XL1                 MAP YEAR                                     
SVMCODE  DS    CL8                 MAP CODE                                     
*                                                                               
         DS    0F                                                               
STATAB   DS    XL106                                                            
BLOCK    DS    CL256                                                            
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENFILE                                                      
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
FEEDRECD DSECT                                                                  
       ++INCLUDE SPTRNFEED                                                      
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'084NEBUY26   04/06/11'                                      
         END                                                                    
