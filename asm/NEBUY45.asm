*          DATA SET NEBUY45    AT LEVEL 223 AS OF 12/08/20                      
*PHASE T31145B,+0                                                               
*&&ONLIN SET   Y                    ONLINE ONLY PROGRAM                         
         TITLE 'NETPAK BUY PROGRAM - STEWARD BUY/DIS/CHA/DEL - T31145'          
T31145   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**STBUY*,RA,RR=RE                                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31145+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL STORAGE                   
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS         R5 POINTS TO BUY VALUES                      
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   ASWITCH,CSWITCH                                                  
         DROP  RF                                                               
         USING BUYVALD,R5                                                       
         LA    RF,STEWREC          SAVE STEWARD INFORMATION                     
         L     RE,AIOAREA4                                                      
         LA    R1,RUPLEN3                                                       
*        IF LARGER THEN 2000 BYTES STEWREC MUST BE MADE LARGER                  
         MOVE  ((RF),(R1)),(RE)                                                 
         SPACE                                                                  
         TWAXC UNTMGFH,CLRINPUTLEN=Y                                            
         SPACE                                                                  
UNIT     TM    MODE,FIRST          FIRST TIME CLEAR SAVE AREA                   
         BZ    *+10                                                             
         XC    SVDATA,SVDATA                                                    
*                                                                               
         GOTO1 VCALLOV,DMCB,(X'30',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDISPLAY,DMCB       GENRAL DISPLAY MODULE                        
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
* ACTION BUY                                                                    
*                                                                               
UNIT2    CLI   ACTION,B            TEST FOR BUY                                 
         BE    UNIT2A                                                           
         CLI   ACTION,REFB         TEST FOR REFRESH BUY                         
         BNE   UNIT6                                                            
*                                                                               
*                                                                               
UNIT2A   BAS   RE,CLIFRZEN         CHECK FOR FROZEN CLIENT                      
         BAS   RE,LOCKPACK         CHECK FOR LOCKED PACKAGE                     
*                                                                               
         XC    FLD,FLD                                                          
         GOTO1 VDATCON,DMCB,(2,DATE),(8,FLD)                                    
         GOTO1 VDATVAL,DMCB,(0,FLD),DUB                                         
         MVI   FERN,DATERR                                                      
         OC    0(4,R1),0(R1)       CHECK UNIT DATE WITHIN EST DATE              
         JZ    OVERROR                                                          
         MVI   FERN,STERR                                                       
         CLC   DUB(6),ESTSTART                                                  
         JL    OVERROR                                                          
         MVI   FERN,ENDERR                                                      
         CLC   DUB(6),ESTEND                                                    
         JH    OVERROR                                                          
*                                                                               
         GOTO1 VGETPROG,DMCB,DATE                                               
         L     R4,AIOAREA1         CLEAR I/O AREA                               
         LR    RE,R4                                                            
         LA    RF,PAGELEN                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         USING NURECD,R4                                                        
         MVI   NUKTYPE,X'04'       BUILD SKELETON KEY                           
         MVC   NUKAM,AGYMED                                                     
         MVC   NUKCLT,CLIPK                                                     
         MVC   NUKDATE,DATE        SET DATE INPUT IN ACTION                     
*        CLC   =X'E65D',DATE     TRAP BAD FEB2915 UNIT DATE                     
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         MVC   NUKNET,NET                                                       
         MVC   NUKPROG,PROG                                                     
         MVC   NUKEST,EST                                                       
         L     RE,APACKREC         EXTRACT PACKAGE DP                           
         MVC   NUKDP,NPAKDP-NPRECD(RE)                                          
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),(R9),RR=MYRELO                     
         BAS   RE,BUILD            EDIT SCREEN/BUILD RECORD                     
         CLI   ACTION,REFB         TEST FOR REFRESH BUY                         
         BE    UNITX                                                            
****     GOTO1 VBLDRQST                                                         
         GOTO1 AIO,DMCB,UNT+FILE+ADDREC,(R4)                                    
         MVC   UNITDA,NDXDA        SAVE DISK ADDRESS                            
         GOTO1 VEDIT,DMCB,(C'P',BLOCK),AIOAREA4                                 
         SPACE                                                                  
         L     R3,AIOAREA4                                                      
UNIT3    CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    UNIT4               ALL DONE                                     
         BAS   RE,NEWPTR           I/O FOR NEW POINTERS                         
         LA    R3,NDIRLEN(R3)      NEXT POINTER                                 
         B     UNIT3                                                            
         SPACE                                                                  
UNIT4    CLI   SUB,2               TEST IF SECOND UNIT ADDED                    
         BNE   *+8                 NO                                           
         BRAS  RE,SUBPRT           UPDATE FIRST UNIT FOR DATE                   
         BAS   RE,REACT            RE-CONSTRUCT ACTION FIELD                    
         BAS   RE,CHGNMIS                                                       
         SPACE                                                                  
UNIT5    MVC   NBAIO,AIOAREA1      SET RECORD ADDRESS                           
         MVI   NBFUNCT,NBFVAL      FORCE VALUEING OF RECORD                     
         MVI   NBESTOPT,YES        LOOK UP ESTIMATED HOMES DEMOS                
         OI    NBSPLOPT,X'10'      DONT SPLIT COPY SPLIT NUMBERS                
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         BAS   RE,DISUNIT          RE-DISPLAY UNIT FOR HOMES DEMOS              
*                                                                               
         L     RE,APACKREC                                                      
         MVC   KEY(20),NPKTYPE-NPRECD(RE)                                       
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(20),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO,DMCB,UNT+FILE+GET+UPDATE,AIOAREA4                            
         L     RE,AIOAREA4                                                      
         NI    NPAKCNTL-NPRECD(RE),X'DF'                                        
         GOTO1 AIO,DMCB,UNT+FILE+PUT,AIOAREA4                                   
         XC    SVDATE,SVDATE                                                    
         B     UNITX                                                            
         SPACE                                                                  
* ACTION CHANGE                                                                 
*                                                                               
UNIT6    CLI   ACTION,C            TEST FOR ACTION CHANGE                       
         BE    UNIT6A                                                           
         CLI   ACTION,REFC         TEST FOR ACTION REFRESH CAHNGE               
         BNE   UNIT15                                                           
UNIT6A   TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    UNIT20              YES                                          
*                                                                               
         BAS   RE,LOCKPACK                                                      
         BAS   RE,GETUNIT                                                       
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         L     R4,NBAIO                                                         
         MVC   OLDKEY,0(R4)        SAVE KEY BEFORE EDIT                         
         BAS   RE,BUILD                                                         
         CLI   ACTION,REFC         TEST FOR ACTION REFRESH CAHNGE               
         BE    UNITX                                                            
****     GOTO1 VBLDRQST                                                         
         SPACE                                                                  
UNIT7    XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),OLDKEY                                              
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE MAKE SURE KEY IS THERE                      
         BE    *+6                                                              
         DC    H'0'                SOMETHING WRONG                              
         MVC   UNITDA,NDXDA        SAVE OLD DISK ADDRESS                        
         MVC   NEWKEY,0(R4)        SAVE NEW KEY                                 
         CLC   OLDKEY,NEWKEY       TEST FOR KEY CHANGE                          
         BE    UNIT8               NONE                                         
*                                                                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA4                            
         L     R3,AIOAREA4         POINT TO OLD RECORD                          
         OI    NURSTAT-NUKEY(R3),X'80' AND MARK IT DELETE                       
*                                                                               
*--SAVE NEW KEY ON DELETED REORD TO KEEP A LINK                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,X'90'                                                       
         MVI   WORK+1,22                                                        
         MVC   WORK+2(20),NEWKEY   NEW RECORDS KEY                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(R3),WORK,0                           
*                                                                               
*--CHECK UP TO EFFECTIVE DATE IF KEY CHANGED                                    
*--DONT DELETE TRAFFIC INFO.                                                    
*                                                                               
         CLC   OLDKEY(6),NEWKEY    TEST FOR KEY UP TO DATE                      
         BNE   UNIT7A              NONE                                         
*                                  DELETE TRAFFIC ELEMENTS                      
         GOTO1 VHELLO,(R1),(C'D',UNTFILE),(X'21',(R3)),0                        
         GOTO1 VHELLO,(R1),(C'D',UNTFILE),(X'22',(R3)),0                        
         GOTO1 VHELLO,(R1),(C'D',UNTFILE),(X'23',(R3)),0                        
UNIT7A   OI    KEY+NDIRCTL,X'80'   DELETE OLD KEY                               
         GOTO1 AIO,DMCB,UNT+DIR+WRITE                                           
         GOTO1 (RF),(R1),UNT+FILE+PUT,AIOAREA4                                  
         XC    KEY,KEY             READ TO SEE IF NEW KEY IS ON                 
         MVC   KEY(L'NUKEY),NEWKEY DIRECTORY MARKED DELETE                      
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE                                             
         BE    UNIT7D              YES                                          
*                                                                               
* KEY CHANGE - NEW KEY DOES NOT EXIST SO JUST DO ADDREC                         
*                                                                               
         GOTO1 (RF),(R1),UNT+FILE+ADDREC,(R4)                                   
         MVC   UNITDA,NDXDA        SAVE NEW DISK ADDRESS                        
         B     UNIT9                                                            
         SPACE                                                                  
*                                                                               
* KEY CHANGE - NEW KEY IS THERE MARKED DELETE SO GET DELETED RECORD             
* AND UNDELETE DIRECTORY ENTRY BEFORE REPLACING RECORD ON FILE                  
*                                                                               
UNIT7D   MVC   UNITDA,NDXDA        SAVE THE DISK ADDRESS                        
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+FILE+GET,AIOAREA4                    
         NI    KEY+NDIRCTL,X'FF'-X'80' TURN OFF DELETE BIT                      
         NI    NURSTAT,X'FF'-X'80' TURN OFF DELETE BIT ON RECORD                
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         B     UNIT9                                                            
         SPACE 1                                                                
* NO KEY CHANGE - REGULAR GETREC, PUTREC UPDATE                                 
*                                                                               
UNIT8    GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,AIOAREA4                            
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)  PUT NEW RECORD BACK                 
         SPACE                                                                  
         USING NURECD,R4                                                        
UNIT9    L     R2,AIOAREA4         OLD KEY POINTERS LOCATION                    
         GOTO1 VEDIT,DMCB,(C'P',BLOCK),(C'O',(R2))                              
         LA    R3,1000(R2)         NEW KEY POINTERS                             
         GOTO1 (RF),(R1),,(C'N',(R3))                                           
         SPACE                                                                  
UNIT10   CLI   0(R2),X'FF'         TEST FOR E-O-L                               
         BE    UNIT12                                                           
*                                                                               
         BAS   RE,OLDPTR                                                        
         BAS   RE,NEWPTR                                                        
         LA    R2,NDIRLEN(R2)                                                   
         LA    R3,NDIRLEN(R3)                                                   
         B     UNIT10                                                           
         SPACE                                                                  
UNIT12   BAS   RE,REACT            RE-CONSTRUCT ACTION FIELD                    
         CLI   SUB,2               TEST FOR SECOND UNIT                         
         BNE   *+18                                                             
         CLC   DATE,NBACTDAT       TEST FOR DATE CHANGE                         
         BE    *+8                                                              
         BRAS  RE,SUBPRT           CHANGE FIRST UNIT'S SUBPRINT                 
*                                                                               
UNIT13   BAS   RE,CHGOMIS                                                       
         BAS   RE,CHGNMIS                                                       
         SPACE                                                                  
UNIT14   BAS   RE,SAVGUAR                                                       
*        OI    NBINDS9,NBI9DOV     SET DEMO OVERRIDE INDICATORS                 
         GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
         BAS   RE,RESGUAR                                                       
         BAS   RE,DISUNIT          RE-DISPLAY CHANGED UNIT                      
*                                                                               
         GOTO1 VCHGBILL,DMCB,NEWKEY,OLDKEY,AIOAREA3,BKEYAREA                    
*                                                                               
         XC    SVDATE,SVDATE                                                    
         B     UNITX                                                            
         SPACE                                                                  
* ACTION DELETE                                                                 
*                                                                               
UNIT15   CLI   ACTION,DEL          TEST FOR ACTION DELETE                       
         BNE   UNIT20                                                           
*                                                                               
         MVI   FERN,AUDACTER       IF AUDIT IS SET , CANT DELETE                
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'02'                                                   
         BO    ERROR                                                            
         DROP  RE                                                               
*                                                                               
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    UNIT20                                                           
         BAS   RE,GETUNIT                                                       
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         GOTO1 VDISPROG                                                         
         L     R4,NBAIO            R4 POINTS TO RECORD                          
         USING NURECD,R4                                                        
*                                                                               
         LA    R3,BLOCK                                                         
         USING UNBLOCKD,R3                                                      
         XC    UNBLOCK,UNBLOCK     BUILD EDIT BLOCK                             
         ST    R9,UNAGLOB                                                       
         LA    RE,TEMPD+3400                                                    
         ST    RE,UNALOCAL                                                      
         ST    R4,UNAREC                                                        
         MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB                                                       
         GOTO1 VEDIT,DMCB,(C'I',(R3))                                           
         SPACE 1                                                                
         TM    NUUNITST,X'03'      TEST IF MISSED AND/OR MAKE-GOOD              
         BNZ   UNIT16              YES                                          
         CLI   UNBILLSW,YES        TEST IF UNIT BILLED                          
         BNE   *+14                                                             
         MVC   XTRA(6),=C'BILLED'                                               
         B     UNIT16                                                           
         CLI   UNPAYSW,YES         TEST IF UNIT PAID                            
         BNE   UNIT17              NO                                           
         MVC   XTRA(4),=C'PAID'                                                 
         SPACE 1                                                                
UNIT16   MVI   FERN,DELERR                                                      
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
UNIT17   XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R4)                                               
         GOTO1 AIO,DMCB,UPDATE+UNT+DIR+READ                                     
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,NBAIO                              
         OI    KEY+NDIRCTL,X'80'   TURN ON DELETE BIT                           
         OI    NURSTAT,X'80'       PUT RECORD BACK AS DELETE TO RECV            
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         GOTO1 (RF),(R1),UNT+FILE+PUT,NBAIO                                     
         GOTO1 VEDIT,(R1),(C'P',(R3)),(C'O',AIOAREA2)                           
         L     R2,AIOAREA2                                                      
         SPACE 1                                                                
UNIT18   CLI   0(R2),X'FF'                                                      
         BE    UNITX                                                            
         BAS   RE,OLDPTR           DELETE OLD POINTER                           
*                                                                               
         LA    R2,NDIRLEN(R2)                                                   
         B     UNIT18                                                           
         SPACE                                                                  
* ACTION DISPLAY OR FORCED DISPLAY                                              
*                                                                               
UNIT20   BAS   RE,GETUNIT                                                       
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         GOTO1 VDISPROG                                                         
         BAS   RE,DISUNIT                                                       
*--  VALIDATE RECORD FOR STEWARD CHANGE                                         
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),(R9),RR=MYRELO                     
*                                                                               
         NI    MODE,X'FF'-DISPLAY                                               
         NI    MODE,X'FF'-FIRST                                                 
         B     UNIT                                                             
         SPACE                                                                  
* SET MESSAGE AND CURSOR THEN EXIT                                              
*                                                                               
UNITX    GOTO1 =A(OVFLRTN),DMCB,(2,DUB),(RC),(R9),RR=MYRELO                     
*                                   BUILD STEWARD RECORD                        
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
         MVI   SCRUPSW,NO                                                       
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTM                NO                                           
         CLC   FLD(2),=C'CA'                                                    
         BNE   *+8                                                              
         MVI   CAACTSW,YES                                                      
         CLI   FLD+1,C'U'          CHECK FOR UPLOAD                             
         BNE   *+8                                                              
         MVI   SCRUPSW,YES                                                      
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
ACTED8   CLI   FSTOP,COMMA         TEST FOR COMMA                               
         BNE   ACTEDX                                                           
         XC    FTERM,FTERM                                                      
         MVI   FTERM,COMMA                                                      
         MVC   XTRA,SPACES                                                      
         GOTO1 AFVAL,0                                                          
         MVI   FNDX,3                                                           
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    ERROR                                                            
         ZIC   RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,FROZCOMP                                                      
         BNE   *+12                                                             
         MVI   BUYUNFR,C'Y'        SET UNFROZEN OVERIDE SWITCH                  
         B     ACTED8                                                           
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
         CLI   BUYUNFR,C'Y'        CHECK UNFROZEN OVERRIDE SWITCH               
         BE    *+12                                                             
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BO    ERROR               CANNOT CHANGE ESTIMATED DEMOS                
         MVI   ESTLOOK,YES                                                      
         B     ACTED8                                                           
ACTED10  MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ERROR                                                            
         CLI   ACTION,REFB         TEST FOR REFRESH BUY                         
         BE    ACTED8                                                           
         CLI   ACTION,REFC         TEST FOR REFRESH BUY                         
         BE    ACTED8                                                           
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         B     ACTED8                                                           
         DROP  RE                                                               
         SPACE                                                                  
ACTM     MVI   FERN,MISERR                                                      
         MVC   XTRA(9),=C'UNIT DATE'                                            
         B     ERROR                                                            
         SPACE                                                                  
ACTEDX   CLI   BUYACT,C'D'         IS ACTION DISPLAY                            
         BE    ACTEDX30            DONT CHECK REASON CODE                       
         CLI   ACTION,REFB         TEST FOR REFRESH BUY                         
         BE    ACTEDX30                                                         
         CLI   ACTION,REFC         TEST FOR REFRESH BUY                         
         BE    ACTEDX30                                                         
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
         SPACE 2                                                                
YESCOMP  CLC   FLD+4(0),=C'YES'                                                 
FROZCOMP CLC   FLD(0),=C'UNFROZEN'                                              
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR FROZEN CLIENT                                        
*                                                                               
CLIFRZEN LR    R0,RE               SAVE RETURN POINT                            
         TM    CLIOPT2,X'08'       TEST FOR FROZEN CLIENT                       
         BZ    CLIFRZEX                                                         
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,CLIFRERR                                                    
         B     ERROR                                                            
         SPACE                                                                  
CLIFRZEX LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR LOCKED PACKAGE                                       
*                                                                               
LOCKPACK LR    R0,RE               SAVE RETURN POINT                            
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'20'      TEST FOR LOCKED PACKAGE                      
         BO    LOCKPACA                                                         
         TM    NPAKCNTL,X'08'      TEST FOR CABLE LOCKED                        
         BO    LOCKPACB                                                         
         BZ    LOCKPACX                                                         
LOCKPACA LA    R2,BUYACTH          LOCK ERROR                                   
         ST    R2,FADDR                                                         
         MVI   FERN,PAKLERR                                                     
         B     ERROR                                                            
LOCKPACB LA    R2,BUYACTH          CABLE LOCK ERROR                             
         ST    R2,FADDR                                                         
         MVI   FERN,UCBLKERR                                                    
         B     ERROR                                                            
         SPACE                                                                  
LOCKPACX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO GET UNIT RECORD                                                
*                                                                               
GETUNIT  ST    RE,SAVEREG                                                       
         BAS   RE,SAVGUAR                                                       
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
         OI    NBSPLOPT,X'10'      DONT SPLIT COPY SPLIT NUMBERS                
         MVI   FERN,NOTFOUND                                                    
         SPACE                                                                  
GETUNIT2 GOTO1 VNETIO,DMCB,NEBLOCKD                                             
*                                                                               
         CLI   NBMODE,NBREQLST                                                  
         BE    ERROR                                                            
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RETURNED                       
         BNE   GETUNIT2            NO TRY AGAIN                                 
*                                                                               
         BAS   RE,SAVGUAR                                                       
*        OI    NBINDS9,NBI9DOV     SET DEMO OVERRIDE INDICATORS                 
         GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
         BAS   RE,RESGUAR                                                       
         SPACE                                                                  
GETUNITX L     RE,SAVEREG                                                       
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO RE-READ AND PUT RECORD (AT ENTRY R4 POINTS TO RECORD)          
*                                                                               
UPREC    LR    R0,RE               SAVE RETURN POINT                            
         MVC   KEY(L'NUKEY),0(R4)  RE-READ REC INTO IO4 AND PUT IT BACK         
         GOTO1 AIO,DMCB,UNT+DIR+READ                                            
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,AIOAREA4                           
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         LR    RE,R0                                                            
         BR    RE                                                               
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
* SUB-ROUTINE FOR NEW POINTERS (AT ENTRY, R3 ADDRESSES POINTER)                 
*                                                                               
NEWPTR   ST    RE,SAVEREG                                                       
         XC    KEY,KEY                                                          
         MVC   KEY(L'NUKEY),0(R3)  READ FOR NEW POINTER                         
         GOTO1 AIO,DMCB,UPDATE+PASSDEL+UNT+DIR+HIGH                             
         CLC   KEY(L'NUKEY),KEYSAVE TEST IF KEY FOUND                           
         BE    NEWPTR2                                                          
         MVC   KEY(L'NUKEY+1),0(R3)                                             
         MVC   KEY+NDIRDA(NDIRLDA),UNITDA                                       
         GOTO1 (RF),(R1),UNT+DIR+ADD                                            
         B     NEWPTRX                                                          
         SPACE                                                                  
NEWPTR2  MVI   KEY+NDIRCTL,0                                                    
         MVC   KEY+NDIRDA(NDIRLDA),UNITDA                                       
         GOTO1 (RF),(R1),UNT+DIR+WRITE                                          
         SPACE                                                                  
NEWPTRX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO RECONSTRUCT ACTION FIELD                                       
*                                                                               
REACT    NTR1                                                                   
         MVC   BUYACT,SPACES                                                    
         OI    BUYACTH+6,X'80'                                                  
         LA    R3,BUYACT                                                        
         MVI   0(R3),C'B'                                                       
         CLI   ACTION,B                                                         
         BE    RA100                                                            
         MVI   BUYACT,C'C'         ITS CHANGE                                   
         CLI   CAACTSW,YES         TEST FOR ACTION CHANGE ACCOUNTING            
         BNE   RA100                                                            
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'A'                                                       
RA100    CLI   SCRUPSW,YES         TEST FOR UPLOAD                              
         BNE   RA120                                                            
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'U'                                                       
*                                                                               
RA120    LA    R3,1(R3)                                                         
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
         BE    RA300                                                            
         CLI   SUB,1                                                            
         BE    RA300                                                            
         MVI   0(R3),C'-'                                                       
         ZIC   R2,SUB                                                           
         EDIT  (R2),(3,1(R3)),ALIGN=LEFT                                        
         SPACE                                                                  
*                                                                               
RA300    OC    AUDREASN,AUDREASN   REASON CODE                                  
         BZ    REACTX                                                           
         LA    RE,6                                                             
RA310    CLI   0(R3),X'40'                                                      
         BNH   RA320                                                            
         LA    R3,1(R3)                                                         
         BCT   RE,RA310                                                         
*                                                                               
RA320    MVC   0(4,R3),=CL4',RS='                                               
         ZIC   R2,SUB                                                           
         MVC   4(4,R3),AUDREASN                                                 
         SPACE                                                                  
*                                                                               
REACTX   MVC   SVDATE,DATE         UPDATE SAVE DATE/SUB-LINE IN                 
         MVC   SVSUB,SUB           CASE CALENDARIZING TOOK PLACE.               
         B     EXXMOD                                                           
         SPACE 2                                                                
         EJECT                                                                  
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
         MVC   NUKPDATE,DATE                                                    
         MVC   NUKPEST,EST                                                      
         MVI   NUKPSUB,1           SUB-LINE 1                                   
         L     R1,AIOAREA1                                                      
         MVC   NUKPDP,NUKDP-NUKEY(R1) EXTRACT DAYPART                           
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY+1),KEYSAVE                                    
         BNE   SUBPRTX             COULD NOT FIND IT-DELETED                    
         SPACE                                                                  
SUBPRT2  L     R4,AIOAREA4         GET THE RECORD                               
         GOTO1 AIO,DMCB,UPDATE+UNT+FILE+GET,(R4)                                
         USING NURECD,R4                                                        
         MVI   NUSUBPRT,1                                                       
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         SPACE                                                                  
SUBPRTX  B     EXXMOD                                                           
         DROP  R3,R4                                                            
*                                                                               
* ROUTINE TO EDIT SCREEN AND BUILD UNIT RECORD                                  
*                                                                               
BUILD    NTR1                                                                   
         NI    MYFLAG,X'FF'-MGDD                                                
*                                                                               
         MVI   MAKEGDSW,C'N'                                                    
         CLI   DDMMYYIP,YES        IF YEAR ENTERED MUST BE A MG                 
         BNE   BLD1                                                             
         MVI   MAKEGDSW,C'Y'                                                    
         CLI   UNTMGFH+5,0                                                      
         BNE   *+12                                                             
DTERRR   MVI   FERN,DATERR                                                      
         B     ERROR                                                            
         SPACE                                                                  
BLD1     GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB          GENERAL EDIT MODULE                          
         SPACE                                                                  
         CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    BLD1A               NO                                           
         CLI   ACTION,REFB         TEST FOR ACTION REFRESH BUY                  
         BNE   BLD2                NO                                           
*                                                                               
BLD1A    CLI   BUYPROF+12,YES      NO DAY CHANGE                                
         BE    BLD2                                                             
         L     RE,APROGEL                                                       
         USING NPGELEM,RE                                                       
         CLI   NPGDAY,X'7C'        TEST FOR A ROTATION                          
         BE    *+12                YES-IT MONDAY-FRIDAY                         
         CLI   NPGDAY,X'7F'        MONDAY-SUNDAY                                
         BNE   BLD2                                                             
         GOTO1 VGETDAY,DMCB,CHARDATE,THREE                                      
         MVC   UNTDAY,SPACES       FORCE THE DAY TO AGREE W DATE                
         MVC   UNTDAY(L'THREE),THREE                                            
         MVI   UNTDAYH+5,3                                                      
         OI    UNTDAYH+6,X'80'     XMIT FORCED DAY BACK                         
         DROP  RE                                                               
         SPACE 1                                                                
BLD2     LA    R4,BLOCK            INITIALIZE EDIT BLOCK AND CALL               
         USING UNBLOCKD,R4         EDIT FOR INITIALIZATION                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         LA    RE,TEMPD+3400                                                    
         ST    RE,UNALOCAL         LOCAL STORAGE TO EDIT MODULE                 
         CLI   CAACTSW,YES         TEST FOR ACTION CHANGE ACCOUNTING            
         BNE   *+8                                                              
         MVI   UNBITOPT,X'80'      ALLOW CHANGE IF PAYED                        
*                                                                               
         CLI   UNTINTH+5,0         NO INPUT IN INTEG                            
         BNE   *+16                                                             
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'80'      TEST FOR TABLE DEFAULT                       
         BO    *+14                YES SET UP FOR TABLE                         
         DROP  RE                                                               
*                                                                               
         CLC   =C'TBL',UNTINT      TEST IF INTEGRATION TABLE LOOK UP            
         BNE   BLD2A                                                            
         LA    RF,INTGTBL                                                       
         ST    RF,UNINGTBL         PASS BACK INTG RATES HERE                    
         MVI   INTGREAD,0          DO 1ST READ                                  
         LA    RF,SVINTTBL                                                      
         ST    RF,INTHLDIT                                                      
         MVC   INTGAIO,AIOAREA3                                                 
         MVC   INTGSTDT,DATE                                                    
         MVC   INTGEDDT,DATE                                                    
BLD2A    CLI   ACTION,C            TEST FOR ACTION CHANGE                       
         BE    BLD2B               NO                                           
         CLI   ACTION,REFC         TEST FOR ACTION REFRESH CHANGE               
         BNE   BLD3                NO                                           
BLD2B    MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
         SPACE                                                                  
BLD3     GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
*                                                                               
*-CHECK PROFILE TO SEE IF MAKE-GOODS CAN BE CHANGED IF UNIT IS PAYED            
         MVI   PAYLOKSW,0                                                       
         CLI   BUYPROF2+13,C'C'                                                 
         BE    *+12                                                             
         CLI   BUYPROF2+13,C'A'                                                 
         BNE   BLD4                                                             
         CLI   UNPAYSW,YES                                                      
         BNE   BLD4                                                             
         MVI   PAYLOKSW,C'A'                                                    
*                                                                               
         SPACE                                                                  
BLD4     BAS   RE,MGED             EDIT MAKE-GOOD FIELDS                        
         BAS   RE,PREEMPT          EDIT AND HANDLE PRE-EMPTIONS                 
*                                                                               
* CHECK FOR DATE CHANGE                                                         
*                                                                               
         L     R1,AIOAREA1                                                      
         USING NURECD,R1                                                        
         OC    NEWDATE,NEWDATE                                                  
         BZ    *+10                                                             
         MVC   NUKDATE,NEWDATE                                                  
         DROP  R1                                                               
*                                                                               
         LA    R2,UNTDAYH          BEGIN EDIT AT DAY                            
         LA    R3,EDLIST           POINT R3 AT EDIT LIST                        
         L     RF,VEDIT                                                         
         LA    R1,DMCB                                                          
         MVC   SVBYPR14,BUYPROF+14 TELL EDIT IM NOT A MG                        
         CLI   MAKEGDSW,C'Y'                                                    
         BE    BLD5                                                             
         MVI   BUYPROF+14,0                                                     
         SPACE                                                                  
BLD5     CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    BLD6                                                             
         ST    R2,FADDR                                                         
         XC    FLAST,FLAST                                                      
         MVC   FTERM(5),=CL5'FIELD'    GET SCREEN HEADER INFO                   
         GOTO1 AFVAL,0                                                          
         MVC   4(2,R2),FLDH+4      SET STATUS AND INPUT LENGTH                  
         ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVC   UNEDATA,0(R3)       SET DATA TYPE                                
         PRINT GEN                                                              
         GOTO1 VEDIT,DMCB,(C'E',(R4))                                           
         PRINT NOGEN                                                            
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    *+14                NONE                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
         LA    R3,1(R3)                                                         
         SR    R0,R0               FIND NEXT UNPROTECTED FIELD                  
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST IF PROTECTED                            
         BO    *-10                YES                                          
         B     BLD5                                                             
         SPACE                                                                  
*                                                                               
BLD6     CLI   ASSFREEZ,C'N'                                                    
         BNE   BLD6D                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)                                                        
         NI    2(RE),X'7F'         TURN OFF FROZEN ASSIGNED COST BIT            
*                                                                               
BLD6D    MVC   BUYPROF+14(1),SVBYPR14                                           
         L     R1,AIOAREA1                                                      
         USING NURECD,R1                                                        
         MVC   DATE,UNDATE         SAVE KEY AIR DATE AND SQH                    
         MVC   TIME,UNSQH          VALUES FOR 'REACT' ROUTINE                   
         CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    BLD6E                                                            
         CLI   ACTION,REFB         TEST FOR ACTION REFRESH BUY                  
         BE    BLD6E                                                            
         CLC   NUKDATE,NBACTDAT    TEST FOR DATE CHANGE                         
         BE    BLD7                NONE-SUB-LINE STAYS THE SAME                 
BLD6E    BAS   RE,GETSUB           FIND NEW SUB-LINE                            
         MVC   NUKSUB,SUB          RETURN SUB-LINE                              
         SPACE                                                                  
BLD7     GOTO1 VGETPROG,DMCB,DATE                                               
         GOTO1 VDISPROG                                                         
*                                                                               
         TM    MYFLAG,MGDD         DO WE HAVE OVERRIDE ELEMENTS?                
         BZ    *+8                                                              
         BAS   RE,MVDDPROG         MOVE THE DD ELEMS INTO PROGREC               
*                                                                               
         BAS   RE,CKSELFMG         ERROR IF MAKEGOOD = SELF                     
         SPACE                                                                  
BLD8     GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'04',AIOAREA1),0                    
         LA    R2,UNTCOM1H                                                      
         MVI   BYTE,NO             SET SECOND COMMENT LINE SWITCH               
         SPACE                                                                  
BLD9     GOTO1 VGETFLD                                                          
         L     R1,NBAIO                                                         
         TM    FLDH+4,X'80'        TEST IF FIELD SENT THIS TIME                 
         BZ    *+8                 NO                                           
         OI    NUACTWHY,X'10'      YES-NOTE COMMENT CHANGE                      
         CLI   FLDH+5,0            TEST FOR INPUT                               
         BE    BLD11               ALL DONE                                     
         DROP  R1                                                               
*                                                                               
         LA    R3,BLOCK+L'UNBLOCK                                               
         USING NUCOMD,R3                                                        
         MVI   NUCOMEL,X'04'                                                    
         ZIC   R1,FLDH+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NUCOMMNT(0),FLD                                                  
         LA    R1,NUCOMMNT-NUCOMEL+1(R1)     DEVELOP EL LENGTH                  
         STC   R1,NUCOMLEN                                                      
         MVI   NUCOMTYP,C'C'                                                    
         MVI   NUCOMLIN,1                                                       
         CLI   BYTE,YES            TEST FOR SECOND LINE                         
         BNE   *+8                                                              
         MVI   NUCOMLIN,2                                                       
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,(R3),0                       
         CLI   12(R1),0            TEST FOR ERROR                               
         BE    BLD10               NO                                           
         MVI   FERN,TOOLARGE                                                    
         CLI   12(R1),5                                                         
         BE    ERROR                                                            
         DC    H'0'                                                             
         SPACE                                                                  
BLD10    CLI   BYTE,YES            TEST IF SECOND LINE DONE                     
         BE    BLD11                                                            
         LA    R2,UNTCOM2H         POINT TO SECOND LINE                         
         MVI   BYTE,YES            SET SWITCH                                   
         B     BLD9                                                             
*                                                                               
* ON NEW BUYS ADD DEFAULT 21 ELEMENT                                            
*                                                                               
BLD11    LA    R3,BLOCK+L'UNBLOCK                                               
         USING NUCMLEL,R3                                                       
         XC    0(80,R3),0(R3)                                                   
         MVI   NUCMLEID,X'21'                                                   
         MVI   NUCMLELN,80                                                      
*                                                                               
         CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    BLD11A                                                           
         CLI   ACTION,REFB         TEST FOR ACTION REFRESH BUY                  
         BNE   BLD12                                                            
BLD11A   GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,(R3),0                       
*                                                                               
BLD12    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            CHECK TO SEE IF IT IS THERE                  
         BNE   BLD11A              IF NOT ADD                                   
*                                                                               
         BAS   RE,CHMG             DECIDE IF CHANGE IN MAKE-GOOD DETS.          
         BAS   RE,NEWMISS          ADD MISSED ELEMENTS                          
         SPACE                                                                  
BLD15    CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    *+12                                                             
         CLI   ACTION,REFB         TEST FOR ACTION REFRESH BUY                  
         BNE   BLD16                                                            
         CLI   FRCSW,C'Y'          TEST FORCE OR COMPLEX MAKEGOOD               
         BE    BLD20               YES-DO INDEPENDENT LOOKUP                    
         OC    MGDETS,MGDETS       TEST IF ADDING MAKE-GOOD                     
         BZ    BLD20                                                            
         MVC   UNAMISS,AIOAREA2    YES-PASS MISSING RECORD                      
         B     BLD20                                                            
         SPACE                                                                  
BLD16    CLI   BUYPROF+1,C'Y'      TEST M-G HAS ITS OWN ESTIMATES               
         BE    BLD16A              YES-PICK UP USER LOOKUP OPTION               
         CLI   FRCSW,C'Y'          TEST FORCE OR COMPLEX MAKEGOOD               
         BE    BLD17               YES-DO INDEPENDENT LOOKUP                    
         OC    MGDETS,MGDETS       TEST IF MAKE-GOOD                            
         BNZ   BLD17               YES                                          
         OC    OMGDETS,OMGDETS     OR IF FORMERLY A MAKE-GOOD                   
         BNZ   BLD17                                                            
*                                                                               
BLD16A   MVC   UNESTLK,ESTLOOK     NEITHER                                      
         B     BLD20                                                            
         SPACE                                                                  
BLD17    OC    OMGDETS,OMGDETS     TEST IF FORMERLY A M-G                       
         BZ    BLD18               NO-MUST BE ADDING DETAILS                    
         CLI   MGCHGSW,YES         TEST IF CHANGING OR DELETING DETAILS         
         BE    BLD18               YES                                          
         CLI   MGCHGSW,NO          MUST BE NO CHANGE IN DETAILS                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   UNNOLOOK,YES        SUPPRESS LOOK-UP                             
         B     BLD20                                                            
         SPACE                                                                  
BLD18    CLI   FRCSW,C'Y'          TEST FORCE OR COMPLEX MAKEGOOD               
         BE    *+20                YES                                          
         OC    MGDETS,MGDETS       TEST FOR MAKE-GOOD REFERENCE                 
         BZ    *+10                NO                                           
         MVC   UNAMISS,AIOAREA2    SET MISSING RECORD ADDRESS                   
         MVI   UNESTLK,YES         FORCE LOOK-UP OF ESTIMATED DEMOS             
         B     BLD20                                                            
         SPACE                                                                  
BLD20    GOTO1 VEDIT,DMCB,(C'D',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    BLD25                                                            
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
         SPACE                                                                  
BLD25    GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),(R9),RR=MYRELO                     
         GOTO1 VEDIT,DMCB,(C'F',(R4))                                           
BLD30    B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EDIT MAKE-GOOD DETAILS AND TO GET MISSING UNIT                 
*                                                                               
MGED     NTR1                                                                   
         L     RF,AIOAREA4                                                      
         MVI   0(RF),X'FF'                                                      
*                                                                               
         MVI   FRCSW,C'N'                                                       
         LA    R2,UNTMGFH                                                       
         GOTO1 VGETFLD                                                          
         MVI   FERN,MISERR                                                      
         CLI   FLDH+5,0                                                         
         BE    CO100                                                            
*                                                                               
         CLI   BUYPROF2+14,C'Y'    COMMENT REQ FOR MAKE-GOOD                    
         BNE   MG050               NO, BYPASS                                   
         CLI   COMINPT,C'Y'        CHECK IF COMMENT INPUTTED                    
         BE    MG050                                                            
         CLI   UNTCOM1H+5,0        COMMENT ALREADY EXIST                        
         BNE   MG050                                                            
         B     MGCOMERR                                                         
MG050    LA    R4,MGDETS                                                        
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVC   FTERM(3),=C',-*'                                                 
         SPACE                                                                  
MG100    GOTO1 AFVAL,0                                                          
         MVC   0(6,R4),PROG        USE THIS PROGRAM AS DEFAULT                  
         CLI   FSTOP,C'*'                                                       
         BNE   DT100                                                            
         SPACE 2                                                                
* SAVE PROGRAM RECORD                                                           
         MVC   0(6,R4),FLD         SAVE PROGRAM CODE                            
         SPACE 2                                                                
* CHECK IF YEAR INPUT & CAN PASS ALLOWABLE RULES                                
         GOTO1 AFVAL,0                                                          
*                                                                               
DT100    MVI   FERN,DATERR                                                      
         GOTO1 VDATVAL,DMCB,(0,FLD),DUB                                         
         OC    0(4,R1),0(R1)                                                    
         BZ    DT200                                                            
         CLC   DUB(6),ESTSTART     TEST IF DATE BEFORE EST START                
         BL    ERROR               YES                                          
         CLC   DUB(6),ESTEND       TEST IF DATE BEFORE EST END                  
         BNH   DT400               NO                                           
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,BUYPROF+14                                                  
         BZ    ERROR                                                            
         GOTO1 VADDAY,(R1),ESTEND,DUB2,(R0)                                     
         CLC   DUB(6),DUB2         TEST IF OUTSIDE OF EST + PROF DAYS           
         BH    ERROR               NO-SAME YEAR AS EST START                    
         B     DT320                                                            
         SPACE                                                                  
DT200    GOTO1 VDATVAL,(R1),(1,FLD),DUB                                         
         OC    DMCB(4),DMCB                                                     
         BZ    ERROR                                                            
         MVC   DUB(2),ESTSTART     ESTIMATE START YEAR                          
         CLC   ESTSTART(2),ESTEND  TEST IF ESTIMATE IS IN SAME YEAR             
         BE    DT300               YES                                          
         CLC   DUB+2(4),ESTSTART+2 TEST INPUT MMDD VS. EST START                
         BNL   *+10                                                             
         MVC   DUB(2),ESTEND       USE END DATE YEAR                            
*                                                                               
DT300    CLC   DUB,ESTEND                                                       
         BH    ERROR                                                            
         SPACE                                                                  
* FIND 90 DAY LIMIT FOR MAKE-GOOD YEAR INPUT                                    
DT320    LH    R0,=H'90'                                                        
         GOTO1 VADDAY,DMCB,CHARDATE,DUB2,(R0)                                   
         CLC   DUB2(6),DUB         TEST IF LT ACTION DATE                       
         BNL   DT340                                                            
         CLI   DDMMYYIP,YES                                                     
         BE    DT400                                                            
DTERR    LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         MVI   FERN,DATERR                                                      
         MVC   XTRA(13),=C'YEAR REQUIRED'                                       
         B     ERROR                                                            
         SPACE                                                                  
DT340    CLC   DATE,ESTE           MG OUT OF EST YEAR OK                        
         BH    DT400                                                            
         CLI   DDMMYYIP,YES        DATE WITHIN EST CAN'T HAVE YEAR              
         BE    DTERR                                                            
         SPACE                                                                  
DT400    GOTO1 VDATCON,(R1),DUB,(2,6(R4))                                       
         LA    RE,MGDETS                                                        
         CR    RE,R4                                                            
         BE    *+8                                                              
         MVI   FRCSW,C'Y'                                                       
         SPACE 2                                                                
* SUB LINE VALIDATATION                                                         
SL100    MVI   8(R4),1             SET SUB-LINE TO 1 AS DEFAULT                 
         CLI   FSTOP,C'-'          TEST FOR SUB-LINE REFERENCE                  
         BNE   RD100               NONE                                         
         GOTO1 AFVAL,0                                                          
*                                                                               
         TM    FLDH+4,X'08'        TEST FOR NUMERIC DATA                        
         BZ    SLERR                                                            
         CVB   R0,DUB                                                           
         LTR   R0,R0                                                            
         BZ    SLERR                                                            
         CH    R0,=H'255'                                                       
         BNH   *+14                                                             
SLERR    MVC   XTRA(8),=C'SUB-LINE'                                             
         B     ERROR                                                            
*                                                                               
         STC   R0,8(R4)                                                         
         SPACE                                                                  
RD100    XC    KEY,KEY             NOW GET REFERENCED RECORD USING              
         MVI   KEY+NUKPTYPE-NURECD,X'84' PASSIVE KEY                            
         MVC   KEY+NUKPAM-NURECD(1),AGYMED                                      
         MVC   KEY+NUKPCLT-NURECD(2),CLIPK                                      
         MVC   KEY+NUKPNET-NURECD(4),NET                                        
         MVC   KEY+NUKPPROG-NURECD(6),0(R4)                                     
         MVC   KEY+NUKPDATE-NURECD(2),6(R4)                                     
         MVC   KEY+NUKPEST-NURECD(1),EST                                        
         MVC   KEY+NUKPSUB-NURECD(1),8(R4)                                      
         SPACE                                                                  
         MVI   FERN,MGNFERR                                                     
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY),KEYSAVE                                      
         BNE   ERROR               DID NOT FIND IT                              
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,AIOAREA2                           
         L     R3,AIOAREA2         MAKE SURE SAME PACKAGE                       
         USING NURECD,R3                                                        
         CLC   PACKAGE,NUPACK                                                   
         BNE   ERROR                                                            
*                                                                               
         MVC   9(16,R4),NUPROGNM   PROGRAM NAME                                 
         MVC   25(1,R4),NUUNITST   COPY MISSED UNIT STATUS                      
         OI    25(R4),X'02'        SET MISSED BIT FOR MG ELEMENT                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         MVC   26(1,R4),NUSDST3-NUSDRD(RE)                                      
*                                                                               
         CLI   BUYPROF+13,C'Y'     PASS BILLBOARD INFO                          
         BNE   RD160                                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   RD160                                                            
         L     RE,12(R1)           GET THE BILLBOARD BIT                        
         MVC   27(1,R4),NUCMLFLG-NUCMLEL(RE)                                    
         NI    27(R4),X'04'        CLEAR ALL BITS BUT BILLBOARD                 
*                                                                               
RD160    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'12',(R3)),0                        
         CLI   12(R1),0            IS UNIT PAID                                 
         BNE   RD180                                                            
         CLI   BUYPROF2+13,C'C'    CAN CHANGE BE MADE TO PAID                   
         BE    *+12                                                             
         CLI   BUYPROF2+13,C'A'    CAN CHANGE BE MADE TO PAID                   
         BNE   RD180                                                            
         MVI   PAYLOKSW,C'A'       MAKEGOOD CHANGE NOT ALLOWED                  
*                                                                               
RD180    TM    NUUNITST,X'01'      TEST IF MISSED UNIT IS A MAKE-GOOD           
         BZ    RD190               NO                                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'06',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)           COPY THE STATUS OF ITS MISSING UNIT          
         MVC   25(1,R4),NUMGSTAT-NUMGD(RE)                                      
*                                                                               
RD190    DS    0H                                                               
         L     R3,AIOAREA2                                                      
         TM    NUUNST2,X'01'       IS M/G DEMO = N?                             
         BO    RD200               IF NOT, DON'T PROCESS OVERRIDES              
*                                                                               
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKCNTL,X'40'      PACKAGE IMPRESSION BASED?                    
         BZ    RD200               IF NOT, DON'T PROCESS OVERRIDES              
         DROP  RE                                                               
*                                                                               
         CLI   PROG+3,X'F0'        THE 4TH AND 5TH CHARACTER IN THE             
         BL    RD200               M/G PROGRAM RECORD MUST BE NUMERIC           
         CLI   PROG+3,X'F9'        (CABLE EDI)                                  
         BH    RD200                                                            
         CLI   PROG+4,X'F0'                                                     
         BL    RD200                                                            
         CLI   PROG+4,X'F9'                                                     
         BH    RD200                                                            
*                                                                               
         BAS   RE,MGOVER           PROCESS M/G OVERRIDES                        
*                                                                               
RD200    CLI   FSTOP,X'FF'         THAT'S ALL FOLKS                             
         BE    ME100                                                            
         LA    R4,28(R4)           NEXT MAKGOOD                                 
         LA    RE,MGDETS+112                                                    
         CR    RE,R4                                                            
         BE    ERROR                                                            
         B     MG100                                                            
         DROP  R3                                                               
         SPACE 2                                                                
ME100    CLI   FRCSW,C'Y'                                                       
         BNE   CO100                                                            
         SPACE                                                                  
ME200    LA    R0,3                CHECK FOR DUPLICATES                         
         SR    R2,R2                                                            
         MVI   FERN,INVERR                                                      
         LA    RE,MGDETS                                                        
         CLI   28(RE),0            AT LEAST 2                                   
         BE    CO100                                                            
ME220    LA    RF,28(RE)                                                        
         LR    R1,R0                                                            
         BCTR  R1,0                                                             
ME240    CLI   0(RF),0                                                          
         BE    ME300                                                            
         CLC   0(9,RE),0(RF)                                                    
         BNE   *+14                                                             
         MVC   XTRA(9),=C'DUPLICATE'                                            
         B     ERROR                                                            
         LA    RF,28(RF)                                                        
         BCT   R1,ME240                                                         
         SPACE                                                                  
ME300    LA    RE,28(RE)                                                        
         LA    R2,1(R2)            COUNT # OF MAKGOODS                          
         CLI   0(RE),0                                                          
         BE    *+8                                                              
         BCT   R0,ME220                                                         
         SPACE                                                                  
         GOTO1 VXSORT,DMCB,MGDETS,(R2),28,9,0                                   
         SPACE                                                                  
CO100    LA    R2,UNTCOMH                                                       
         ST    R2,FADDR                                                         
         CLI   UNTMGFH+5,0         HAS TO BE A MAKEGOOD                         
         BNE   *+12                                                             
         CLI   5(R2),0                                                          
         BE    MGEDX                                                            
         MVI   FERN,MISERR                                                      
         CLI   5(R2),0                                                          
         BE    ERROR                                                            
         CLI   8(R2),C'N'                                                       
         BE    MGEDX                                                            
         MVI   FERN,CPLXERR                                                     
         CLI   8(R2),C'Y'                                                       
         BNE   ERROR                                                            
******   CLI   UNTMGFH+5,0         HAS TO BE A MAKEGOOD                         
******   BE    ERROR                                                            
         CLI   UNTMGF,X'40'                                                     
         BNH   ERROR                                                            
         MVI   FRCSW,C'Y'                                                       
         SPACE                                                                  
MGEDX    B     EXXMOD                                                           
         SPACE                                                                  
MGCOMERR LA    R2,UNTCOM1H                                                      
         ST    R2,FADDR                                                         
         MVC   BUYMSG(L'MGCOMM),MGCOMM                                          
         MVI   FERN,USERERR                                                     
         B     ERROR                                                            
         EJECT                                                                  
* SUB-ROUTINE TO GET NEXT SUB-LINE NUMBER                                       
*                                                                               
GETSUB   NTR1                                                                   
         LA    R4,KEY                                                           
         USING NUKPKEY,R4                                                       
         L     R1,AIOAREA1         POINT R1 AT RECORD                           
         XC    KEY,KEY                                                          
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
         BL    GETSUB2             LOW-KEEP READING                             
         MVI   FERN,SUBERR                                                      
         GOTO1 VDATCON,DMCB,(2,NUKPDATE),(4,XTRA)                               
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
GETSUB4  LA    R4,KEYSAVE                                                       
         ZIC   R1,NUKPSUB                                                       
         LA    R1,1(R1)                                                         
         STC   R1,SUB                                                           
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* CHECK TO SEE IF THE UNIT I'M TRYING IS BEING MADEGOOD FOR                     
CKSELFMG ST    RE,SAVEREG                                                       
         L     RF,AIOAREA1                                                      
         USING NURECD,RF                                                        
         LA    RE,MGDETS                                                        
         LA    R0,4                                                             
CS100    CLI   0(RE),0                                                          
         BE    CSXIT                                                            
         CLC   NUKPROG,0(RE)       TEST FOR MAKE-GOOD SELF-REFERENCE            
         BNE   CS200                                                            
         CLC   NUKDATE,6(RE)                                                    
         BNE   CS200                                                            
         CLC   NUKSUB,8(RE)                                                     
         BNE   CS200                                                            
         MVI   FERN,MGINVERR       PROGRAM/DATE-SUBLINE ARE THE SAME            
         LA    R2,UNTMGFH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
*                                                                               
CS200    LA    RE,28(RE)                                                        
*                                                                               
         BCT   R0,CS100                                                         
CSXIT    L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DECIDE IF MAKE-GOOD DETAILS HAVE CHANGED                       
*                                                                               
CHMG     NTR1                                                                   
         MVI   MGCHGSW,NO                                                       
         CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    *+12                                                             
         CLI   ACTION,REFB         TEST FOR ACTION REFRESH BUY                  
         BNE   CM020                                                            
         CLI   UNTMGFH+5,0         IS UNIT A MAKEGOOD                           
         BE    CM500                                                            
         MVI   MGCHGSW,YES         SET MAKEGOOD SWITCH                          
         B     CM500               MUST BE ADDING MAKE-GOOD DETAILS             
*                                                                               
CM020    L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         TM    NUUNST2,X'01'       TEST IF UNIT IS A MAKE-GOOD                  
         BZ    CM080               NO-THEREFORE ADDING DETAILS                  
         CLI   FRCSW,C'Y'                                                       
         BE    CM100                                                            
         MVI   MGCHGSW,YES                                                      
         B     CM100                                                            
*                                                                               
CM080    CLI   FRCSW,C'Y'                                                       
         BNE   CM100                                                            
         MVI   MGCHGSW,YES                                                      
*                                                                               
CM100    LA    RE,27(R4)           PULL OUT OLD MAKEGOODS                       
         XC    OMGDETS,OMGDETS                                                  
         LA    R2,OMGDETS                                                       
CM120    CLI   0(RE),0                                                          
         BE    CM300                                                            
         CLI   0(RE),X'06'                                                      
         BNE   CM140                                                            
         MVC   0(6,R2),NUMGPCOD-NUMGD(RE)                                       
         MVC   6(3,R2),NUMGDATE-NUMGD(RE)                                       
         LA    R2,9(R2)                                                         
CM140    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     CM120                                                            
*--I MAY BE WRONG BUT I DONT THINK THIS CODE IS NEEDED                          
*                                                                               
*CM200    OC    MGDETS,MGDETS       TEST IF DELETING MAKE-GOOD DETAILS          
*         BNZ   CM300               NO                                          
*         MVI   MGCHGSW,YES         MAKE-GOOD DETAILS HAVE CHANGED              
*         B     CM500                                                           
*                                                                               
CM300    LA    R2,MGDETS           CHECK IF OLD NEW CHANGED                     
         LA    RE,OMGDETS                                                       
         LA    R0,4                                                             
CM320    CLC   0(9,R2),0(RE)                                                    
         BNE   CM400                                                            
         LA    RE,9(RE)            BOTH ARE IN SORTED ORDER                     
         LA    R2,28(R2)                                                        
         BCT   R0,CM320                                                         
         B     CM500                                                            
*                                                                               
CM400    MVI   MGCHGSW,YES         SET CHANGED MG DETAILS SWITCH                
         SPACE                                                                  
*-CHECK TO SEE IF MAKE-GOOD CHANGE ALLOWED                                      
CM500    CLI   MGCHGSW,YES         TEST IF EXISTING MAKE-GOOD                   
         BNE   CMXIT               DETAILS CHANGED                              
         CLI   PAYLOKSW,C'A'       CAN PAID UNITS BE CHANGED                    
         BNE   CMXIT                                                            
         MVI   FERN,PAYCHGNA                                                    
         LA    R2,UNTMGFH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
*                                                                               
CMXIT    B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO MAINTAIN NEW MISSED DATA                                       
*                                                                               
NEWMISS  NTR1                                                                   
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         CLI   ACTION,B                                                         
         BE    WM100                                                            
         CLI   ACTION,REFB                                                      
         BE    WM100                                                            
         CLI   MGCHGSW,YES         TEST IF EXISTING MAKE-GOOD                   
         BE    *+14                DETAILS CHANGED                              
         OC    OMGDETS,OMGDETS                                                  
         BNZ   WMXIT               DETAILS ARE SAME IF SW=Y AND MG NOW          
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'06',AIOAREA1),0                    
         NI    NUUNITST,X'FF'-X'01' TURN OFF M-G IN CASE OF DELETE              
*                                                                               
WM100    NI    NUUNST2,X'FE'       FORCED                                       
         CLI   FRCSW,C'Y'                                                       
         BNE   *+8                                                              
         OI    NUUNST2,X'01'       FORCED                                       
         OC    MGDETS,MGDETS       TEST FOR MAKE-GOOD DETAILS                   
         BZ    WMXIT               EXIT-NONE                                    
         OI    NUUNITST,X'01'      MAKE-GOOD STATUS                             
         OI    NUACTWHY,X'04'      LAST ACTIVITY-MAKE GOOD                      
         LA    R2,MGDETS                                                        
         LA    R4,4                MAX NUMBER                                   
*                                                                               
WM120    CLI   0(R2),0                                                          
         BE    WMXIT                                                            
         XC    WORK,WORK                                                        
         MVI   WORK,X'06'          ADD MISSING DETAILS ELEMENT                  
         MVI   WORK+1,32           TO MAKE-GOOD UNIT                            
         MVC   WORK+2(6),0(R2)     PROGRAM CODE                                 
         MVC   WORK+8(16),9(R2)    PROGRAM NAME                                 
         MVC   WORK+24(2),6(R2)    AIR DATE                                     
         MVC   WORK+26(1),8(R2)    SUB-LINE                                     
         MVC   WORK+27(1),25(R2)   COPY STATUS                                  
         MVC   WORK+28(1),26(R2)   COPY STATUS 3                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(R3),WORK,0                           
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'21',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         L     RE,12(R1)                                                        
         OC    NUCMLFLG-NUCMLEL(1,RE),27(R2)  SET BILLBOARD STATUS              
*                                                                               
         LA    R2,28(R2)                                                        
         BCT   R4,WM120                                                         
         SPACE                                                                  
WMXIT    B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHANGE OLD MISSED RECORDS                                      
*                                                                               
CHGOMIS  NTR1                                                                   
         CLI   MGCHGSW,YES         TEST IF MAKE-GOOD DETAILS CHANGED            
         BNE   OMXIT                                                            
         SPACE 1                                                                
         LA    R2,OMGDETS                                                       
         CLI   0(R2),0                                                          
         BE    OMXIT                                                            
*                                                                               
OM100    LA    R3,MGDETS           CHECK IF OLD NEW CHANGED                     
OM120    CLI   0(R3),0                                                          
         BE    OM200                                                            
         CLC   0(6,R2),0(R3)                                                    
         BNE   OM140                                                            
         CLC   6(3,R2),6(R3)                                                    
         BE    OM400                                                            
OM140    LA    R3,28(R3)                                                        
         B     OM120                                                            
         SPACE                                                                  
OM200    XC    KEY,KEY                                                          
         MVI   KEY+NUKPTYPE-NURECD,X'84'                                        
         MVC   KEY+NUKPAM-NURECD(1),AGYMED                                      
         MVC   KEY+NUKPCLT-NURECD(2),CLIPK                                      
         MVC   KEY+NUKPNET-NURECD(4),NET                                        
         MVC   KEY+NUKPPROG-NURECD(6),0(R2)                                     
         MVC   KEY+NUKPDATE-NURECD(2),6(R2)                                     
         MVC   KEY+NUKPEST-NURECD(1),EST                                        
         MVC   KEY+NUKPSUB-NURECD(1),8(R2)                                      
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                MUST FIND REFERENCED ITEM                    
         GOTO1 (RF),(R1),UNT+FILE+GET+UPDATE,AIOAREA3                           
         SPACE                                                                  
         L     R4,AIOAREA3         PULL OUT OLD MAKEGOODS                       
         L     R3,AIOAREA1                                                      
         USING NURECD,R3                                                        
         NI    NUUNITST-NURECD(R4),X'FD' TURN OFF MISSED                        
         LR    R0,R4                                                            
         LA    RE,27(R4)                                                        
OM220    CLI   0(RE),0                                                          
         BE    OM300                                                            
         CLI   0(RE),X'07'                                                      
         BNE   OM260                                                            
         CLC   NUKPROG,NUMGPCOD-NUMGD(RE)                                       
         BNE   OM240                                                            
         CLC   NUKDATE,NUMGDATE-NUMGD(RE)                                       
         BNE   OM240                                                            
         CLC   NUKSUB,NUMGSUB-NUMGD(RE)                                         
         BNE   OM240                                                            
         SR    R0,R0                                                            
         MVI   0(RE),X'FF'         DELETE ELEMENT ONLY                          
         B     OM260                                                            
OM240    OI    NUUNITST-NURECD(R4),X'02' STILL MISSED                           
OM260    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     OM220                                                            
*                                                                               
OM300    LTR   R0,R0               MUST BE THERE                                
         BZ    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'FF',AIOAREA3),0                    
         GOTO1 AIO,(R1),UNT+FILE+PUT,(R4)                                       
*                                                                               
OM400    LA    R2,9(R2)                                                         
         CLI   0(R2),0                                                          
         BE    *+8                                                              
         B     OM100                                                            
         SPACE                                                                  
OMXIT    B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO CHANGE NEW MISSED DATA                                         
*                                                                               
CHGNMIS  NTR1                                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         OC    MGDETS,MGDETS                                                    
         BZ    NMXIT               EXIT-NONE                                    
*                                                                               
         LA    R2,MGDETS                                                        
NM100    LA    RE,OMGDETS          CHECK IF OLD NEW CHANGED                     
NM120    CLI   0(RE),0                                                          
         BE    NM200                                                            
         CLC   0(6,R2),0(RE)                                                    
         BNE   NM140                                                            
         CLC   6(3,R2),6(RE)                                                    
         BE    NM400                                                            
NM140    LA    RE,9(RE)                                                         
         B     NM120                                                            
*                                                                               
NM200    XC    KEY,KEY                                                          
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(1),AGYMED                                                  
         MVC   KEY+2(2),CLIPK                                                   
         MVC   KEY+4(4),NET                                                     
         MVC   KEY+8(6),0(R2)                                                   
         MVC   KEY+14(2),6(R2)                                                  
         MVC   KEY+16(1),EST                                                    
         MVC   KEY+17(1),8(R2)                                                  
*                                                                               
         GOTO1 AIO,DMCB,UNT+DIR+HIGH                                            
         CLC   KEY(NUKPDP-NUKPKEY),KEYSAVE                                      
         BE    *+6                                                              
         DC    H'0'                MUST FIND REFERENCED ITEM                    
         GOTO1 (RF),(R1),UNT+FILE+GET+UPDATE,AIOAREA2                           
         L     R3,AIOAREA2                                                      
         OI    NUUNITST-NURECD(R3),X'02' TURN ON M-G                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   NM300                                                            
         L     RE,12(R1)                                                        
         OI    5(RE),X'04'         LAST CHANGE IS A MISSED                      
*                                                                               
NM300    XC    WORK,WORK                                                        
         MVI   WORK,X'07'          ADD MISSING DETAILS ELEMENT                  
         MVI   WORK+1,32           TO MAKE-GOOD UNIT                            
         MVC   WORK+2(6),NUKPROG   PROGRAM CODE                                 
         MVC   WORK+8(16),NUPROGNM PROGRAM NAME                                 
         MVC   WORK+24(2),NUKDATE  AIR DATE                                     
         MVC   WORK+26(1),NUKSUB   SUB-LINE                                     
         MVC   WORK+27(1),NUUNITST COPY STATUS                                  
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         MVC   WORK+28(1),NUSDST3-NUSDRD(RE)                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(R3),WORK,0                           
*                                                                               
* MAX MAKEGOODS ATTACHED TO SINGLE UNIT IS 20                                   
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'07',(R3)),0                        
         L     RE,12(R1)                                                        
         LA    RF,19                                                            
         CLI   12(R1),0            TEST IF FOUND                                
         BE    NM325                                                            
         DC    H'0'                JUST ADDED THE ELEMENT                       
*                                                                               
NM325    ZIC   R1,1(RE)                                                         
         AR    RE,R1                                                            
         CLI   0(RE),X'07'                                                      
         BNE   NM340                                                            
         BCT   RF,NM325                                                         
         MVI   FERN,MAXMGS         MAX NUMBER OF MAKEGOODS EXCEEDED             
         B     ERROR                                                            
*                                                                               
*  UPDATE ACTIVITY ELEMENT                                                      
NM340    GOTO1 VDATCON,DMCB2,(2,TODAYC),(3,THREE)                               
         XC    WORK,WORK                                                        
         LA    RE,WORK             BUILD ACTIVITY ELEMENT                       
         USING NUACTD,RE                                                        
         MVI   NUACTEL,X'99'                                                    
         MVI   NUACTLEN,23                                                      
         MVC   NUACTADT,THREE      ADD DATE                                     
         MVC   NUACTCDT,THREE      LAST ACTIVITY DATE                           
         MVC   NUACTAID,SVPASSWD   ADD PERSONAL ID                              
         MVC   NUACTCID,SVPASSWD   LAST PERSONAL ID                             
         MVC   NUACTAGD,SECAGYA    SECURITY AGENCY                              
         MVC   NUACTRSN,AUDREASN   REASON CODE                                  
         MVC   NUACTOVL,OVLAYNUM   OVERLAY NUMBER                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'99',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RF,12(R1)           COPY THE 3RD STATUS BIT                      
         LA    RE,WORK             RESET RE TO WORK                             
         MVC   NUACTACD,2(RF)      COPY OVER ADD AUTH. CODE                     
         MVC   NUACTADT,4(RF)      AND THE CREATION DATE                        
         CLI   1(RF),13                                                         
         BL    NM350                                                            
         MVC   NUACTAID,12(RF)     COPY OVER ADD PERSONAL ID                    
         MVC   NUACTAGD,16(RF)     AND SECURITY AGENCY                          
         SPACE                                                                  
NM350    GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'99',(R3))                          
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),(R3),WORK,0                           
         DROP  RE                                                               
*                                                                               
         GOTO1 AIO,(R1),UNT+FILE+PUT,(R3)                                       
*                                                                               
NM400    LA    R2,28(R2)                                                        
         CLI   0(R2),0                                                          
         BE    *+8                                                              
         B     NM100                                                            
         SPACE                                                                  
NMXIT    B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE FOR PRE-EMPTIONS                                                  
*                                                                               
PREEMPT  NTR1                                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         MVC   BYTE,NUUNITST                                                    
         NI    NUUNITST,X'FF'-X'40' TURN OFF PRE-EMPT BIT                       
         LA    R2,UNTPREH                                                       
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0                                                         
         BE    PR100                                                            
*  FOLLOWING CODE IS FOR SCRIPT BUY UPLOAD ONLY                                 
         CLI   BUYACT+1,C'U'       CHECK FOR UPLOAD                             
         BNE   PR040                                                            
         CLI   FLD,C'B'            CHECK BONUS SET                              
         BNE   PR020                                                            
         OI    NUUNITST,X'04'      SET BUNUS FOR UPLOAD ONLY                    
         B     PREX                                                             
PR020    CLI   FLD,C'A'            CHECK ADU SET                                
         BNE   PR040                                                            
         BAS   RE,SETUPADU         SET ADU BIT (UPLOAD ONLY)                    
         B     PREX                                                             
*                                                                               
PR040    CLI   FLD,NO              TEST FOR 'NO'                                
         BE    PR100               YES                                          
         MVI   FERN,INVERR                                                      
         CLI   FLD,YES             TEST FOR 'YES'                               
         BNE   ERROR                                                            
         MVI   FERN,PREERR         MUST REMOVE MAKE-GOOD REFERENCE              
         TM    NUUNITST,X'02'      TEST IF UNIT MISSED                          
         BO    ERROR                                                            
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
         BNZ   PREX                YES, NO CHANGE IN STATUS                     
         CLI   BUYPROF2+13,C'C'                                                 
         BE    *+12                                                             
         CLI   BUYPROF2+13,C'A'                                                 
         BNE   PREX                                                             
         CLI   UNPAYSW,YES                                                      
         BNE   PREX                                                             
         MVI   FERN,PAYCHGNA                                                    
         B     ERROR                                                            
         DROP  RE                                                               
*                                                                               
PR100    TM    BYTE,X'40'          WAS IT SET                                   
         BZ    PREX                                                             
         ICM   RE,15,NUINTEG                                                    
         BZ    PREX                                                             
         MVC   NBINTEG,NUINTEG                                                  
         ICM   R2,15,NUINTEG                                                    
         TM    NBUNITST,X'80'      TEST FOR MINUS UNIT                          
         BZ    *+6                                                              
         LNR   R2,R2               CONVERT COST TO NEGATIVE NUMBER              
         LR    R0,R2               SAVE COST VALUE                              
         SRDA  R2,32               PREPARE DIVIDEND                             
         D     R2,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   R2,R2               TEST REMAINDER (PENNIES)                     
         BNZ   PX100               YES                                          
         EDIT  (R3),(7,UNTINT),ALIGN=LEFT,MINUS=YES                             
         B     PX200                                                            
         SPACE                                                                  
PX100    LR    R2,R0               RESTORE COST VALUE W PENNIES                 
         EDIT  (R2),(8,UNTINT),2,ALIGN=LEFT,MINUS=YES                           
PX200    STC   R0,UNTINTH+5                                                     
         NI    UNTINTH+4,X'F7'     TURN OFF NUMERIC BIT                         
         SPACE                                                                  
PREX     B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
*  SETS ADU BIT IN 02 ELEMENT FOR SCRIPT BUY UPLOAD ONLY                        
*                                                                               
SETUPADU NTR1                                                                   
         L     R3,NBAIO            R3 POINTS TO RECORD                          
         USING NUSDRD,RE                                                        
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',(R3)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)                                                        
         OI    NUSDST3,X'02'       SET ADU BIT                                  
         B     EXXMOD                                                           
         DROP  RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY UNIT RECORD VALUES                                     
*                                                                               
DISUNIT  NTR1                                                                   
         SPACE                                                                  
DIS10    MVC   XTRA,SPACES                                                      
         SPACE                                                                  
DIS20    L     RF,VDISPLAY                                                      
         LA    R1,DMCB                                                          
         LA    R3,DISTAB           POINT R3 AT DISPLAY TABLE                    
         SPACE                                                                  
DIS30    CLI   0(R3),X'FF'         TEST FOR E-O-T                               
         BE    DIS32                                                            
         ZIC   R0,0(R3)            DATA TYPE                                    
         ICM   R2,7,1(R3)          DISPLACEMENT FROM FIRST DISPLAY HDR.         
         LA    R2,UNTMGFH(R2)                                                   
         GOTO1 (RF),(R1),((R0),(R9))                                            
         ZIC   RE,0(R2)            DEVELOP FIELD DATA LENGTH                    
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD         OUTPUT DATA TO SCREEN                        
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         LA    R3,L'DISTAB(R3)                                                  
         B     DIS30                                                            
         SPACE                                                                  
DIS32    LA    R2,UNTMGFH                                                       
         MVI   ELCODE,X'06'                                                     
         MVI   MAGDOVSW,0                                                       
         BAS   RE,DSPLY67                                                       
         MVC   UNTMGF(55),SVMGFLDS                                              
         SPACE                                                                  
         LA    R2,UNTMISSH                                                      
         MVI   ELCODE,X'07'                                                     
         MVI   MAGDOVSW,0                                                       
         BAS   RE,DSPLY67                                                       
         MVC   UNTMISS(55),SVMGFLDS                                             
         SPACE                                                                  
DIS40    LA    R2,UNTCOM1H                                                      
         GOTO1 VCLEARF,DMCB,(R2),UNTLAST                                        
         MVI   BYTE,NO             SET SECOND FIELD SWITCH                      
         MVI   HALF,C'C'           FILTER ON COMMENT ELEMENT                    
         MVI   HALF+1,1                                                         
         SPACE                                                                  
DIS50    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'04',NBAIO),(2,HALF)                
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   DISX                NO-EXIT                                      
*                                                                               
         L     R3,12(R1)                                                        
         USING NUCOMD,R3                                                        
         LA    R0,L'UNTCOM1        SCREEN FIELD LENGTH                          
         ZIC   R1,NUCOMLEN                                                      
         SH    R1,=H'4'            COMMENT LENGTH                               
         CR    R0,R1               NO LONGER THAN SCREEN FIELD                  
         BNL   *+6                                                              
         LR    R1,R0                                                            
         STCM  R1,1,5(R2)                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),NUCOMMNT                                                 
*                                                                               
         CLI   BYTE,YES            TEST IF SECOND FIELD DONE                    
         BE    DISX                YES-EXIT                                     
         MVI   BYTE,YES                                                         
         LA    R2,UNTCOM2H                                                      
         MVI   HALF+1,2            FILTER ON LINE 2                             
         B     DIS50                                                            
         SPACE                                                                  
DISX     B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO OUTPUT MAKEGOOD & MISSED LINES                                 
*                                                                               
DSPLY67  ST    RE,SAVEREG                                                       
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,CLCMG                                                         
DP020    XC    SVMGFLDS,SVMGFLDS                                                
         LA    R2,SVMGFLDS                                                      
         L     R3,AIOAREA1                                                      
         LA    R3,27(R3)                                                        
         LA    R4,3                MAX 4                                        
DP100    CLI   0(R3),0                                                          
         BE    DP500                                                            
         CLC   ELCODE,0(R3)                                                     
         BNE   DP300                                                            
*                                  PUT OUT COMMA                                
         LA    RF,3                                                             
         CR    R4,RF                                                            
         BE    *+12                                                             
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                  PROGRAM CODE (IF DIFFERENT)                  
         CLI   MAGDOVSW,X'FF'                                                   
         BE    DP110                                                            
         CLI   ELCODE,X'07'        DON'T PRINT PROGRAM ON MGD                   
         BE    *+14                                                             
DP110    CLC   PROG,NUMGPCOD-NUMGD(R3)                                          
         BE    DP200                                                            
         LA    RE,6                                                             
         LA    RF,NUMGPCOD-NUMGD(R3)                                            
DP120    CLI   0(RF),X'40'                                                      
         BE    DP140                                                            
         MVC   0(1,R2),0(RF)                                                    
         LA    R2,1(R2)                                                         
         LA    RF,1(RF)                                                         
         BCT   RE,DP120                                                         
*                                                                               
DP140    MVI   0(R2),C'*'                                                       
         LA    R2,1(R2)                                                         
*                                  DATE MMMDD OR MMMDD/YY                       
DP200    GOTO1 VDATCON,DMCB,(2,NUMGDATE-NUMGD(R3)),(0,DUB)                      
         LH    R0,=H'90'                                                        
         GOTO1 VADDAY,(R1),DUB,DUB2,(R0)                                        
         GOTO1 VDATCON,(R1),(0,DUB2),(2,DUB)                                    
         CLC   NUMGDATE-NUMGD(2,R3),ESTE                                        
         BH    *+14                                                             
         CLC   DUB,NUMGDATE-NUMGD(R3)                                           
         BNL   DP220                                                            
         GOTO1 VDATCON,(R1),(2,NUMGDATE-NUMGD(R3)),(5,0(R2))                    
         LA    R2,8(R2)                                                         
         B     DP240                                                            
DP220    GOTO1 VDATCON,(R1),(2,NUMGDATE-NUMGD(R3)),(4,0(R2))                    
         LA    R2,5(R2)                                                         
*                                                                               
DP240    CLI   NUMGSUB-NUMGD(R3),2 TEST FOR SUB-LINE                            
         BL    DP260                                                            
         ZIC   R0,NUMGSUB-NUMGD(R3)                                             
         EDIT  (R0),(3,1(R2)),ALIGN=LEFT                                        
         MVI   0(R2),DASH                                                       
         LA    R2,1(R2)                                                         
         AR    R2,R0                                                            
DP260    BCT   R4,DP300                                                         
         B     DP500                                                            
*                                                                               
DP300    ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     DP100                                                            
*                                                                               
DP500    CLI   SVMGFLDS+55,0       CHECK FIELD OVERFLOW                         
         BE    DP550                                                            
         CLI   MAGDOVSW,X'FF'      IS OVERFLOW SWITCH ALREADY SET               
         BE    DP550                                                            
         MVI   MAGDOVSW,X'FF'      SET OVERFLOW SWITCH                          
         B     DP020               RE-CREATE OUTPUT LINE                        
DP550    LA    R2,UNTCOMH                                                       
         L     RE,AIOAREA1                                                      
         USING NURECD,RE                                                        
         OI    6(R2),X'80'                                                      
         MVI   5(R2),1                                                          
         MVI   8(R2),C'N'                                                       
         TM    NUUNST2,X'01'                                                    
         BZ    DPXIT                                                            
         MVI   8(R2),C'Y'                                                       
*                                                                               
DPXIT    L     RE,SAVEREG                                                       
         BR    RE                                                               
CLCMG    XC    8(0,R2),8(R2)                                                    
         DROP  RE                                                               
         EJECT                                                                  
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
         MVC   BUYMSG(4),=C'UNIT'                                               
         MVC   BUYMSG+5(9),1(RE)                                                
         LA    R2,BUYACTH          SET CURSOR POSITION                          
         TM    MODE,DISPLAY        TEST FOR CHANCE OF FORCED DISPLAY            
         BZ    MSGX                NO                                           
         SPACE                                                                  
MSG2     CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    MSGX                ALL DONE                                     
         CLI   ACTION,REFB         TEST FOR ACTION REFRESH BUY                  
         BE    MSGX                ALL DONE                                     
         CLI   ACTION,D            TEST FOR DISPLAY                             
         BE    MSGX                YES                                          
         MVC   BUYMSG+5(9),=C'DISPLAYED'                                        
         LA    R3,BUYMSG+15                                                     
         LA    R2,UNTMGFH                                                       
         MVC   0(15,R3),=C'- ENTER CHANGES'                                     
         CLI   ACTION,C                                                         
         BE    MSGX                                                             
         CLI   ACTION,REFC                                                      
         BE    MSGX                                                             
         MVC   0(20,R3),=C'- NOW YOU MAY DELETE'                                
         OI    1(R2),X'01'         CONVERT CURSOR TO MODIFIED                   
         B     MSGX                                                             
         SPACE                                                                  
MSGX     ST    R2,FADDR                                                         
         L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
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
         MVC   NBNGUFAC,PNGUAHLD    RESTORE PACKAGE GUARENTEE                   
****     MVC   NUGUAFAC,PGUARHLD                                                
****     MVC   NBGUAFAC(2),PGUARHLD    RESTORE PACKAGE GUARENTEE                
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
**********************************************************************          
*   ADD OVERRIDES TO M/G UNIT PROGRAM RECORD                                    
*        INPUT:    AIOAREA2 - MISSED UNIT                                       
*        OUTPUT:   AIOAREA4 - TABLE OF OVERRIDE ELEMENTS FROM                   
*                             MISSED UNITS                                      
**********************************************************************          
MGOVER   NTR1                                                                   
         L     R3,AIOAREA2         A(MISSED UNIT)                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'DD',(R3)),0                        
         CLI   12(R1),0            ANY OVERRIDES?                               
         BNE   MGOVERX                                                          
         L     R3,12(R1)                                                        
*                                                                               
MGO10    DS    0H                                                               
         CLI   0(R3),X'DD'                                                      
         BNE   MGOVERX                                                          
         USING NUOVD,R3                                                         
*                                                                               
         MVC   ENCDEMO,NUOVCAT                                                  
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
*                                                                               
         CLI   DECDEMO+1,C'T'      ONLY PICK UP IMPRESSION                      
         BE    *+12                                                             
         CLI   DECDEMO+1,C'H'      AND HISPANIC OVERRIDES                       
         BNE   MGO100                                                           
*                                                                               
         L     R4,AIOAREA4         SEE IF DD IS ALREADY IN TABLE                
MGO20    DS    0H                                                               
         CLI   0(R4),X'FF'         OVERRIDE ALREADY EXIST?                      
         BE    MGO50               NO - ADD IT                                  
*                                                                               
         CLC   0(8,R4),0(R3)       FOUND A MATCH?                               
         BE    *+12                                                             
         LA    R4,16(R4)                                                        
         B     MGO20                                                            
*                                                                               
         ICM   RE,15,8(R4)         TAKE VALUE FROM TABLE                        
         ICM   RF,15,NUOVVAL       AND VALUE FROM MISSED UNIT                   
*                                                                               
         AR    RE,RF               AND TOTAL THEM                               
         STCM  RE,15,8(R4)         AND UPDATE THE ELEMENT IN TABLE              
*                                                                               
*  GET EQUIVALENCED OVERRIDE VALUES AND PUT THEM IN TABLE ALSO                  
*  EQUIVALENCED OVERRIDE = (RAW VALUE X MISSED UNIT LEN) / EQUIV. LEN           
*                                                                               
         L     RF,AIOAREA2         MISSED UNIT LEN                              
         USING NURECD,RF                                                        
         SR    R6,R6                                                            
         ICM   R6,1,NULEN          UNIT LENGTH                                  
         DROP  RF                                                               
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,NUOVVAL       RAW VALUE                                    
*                                                                               
         CLI   NETPROF+1,X'1E'     EQUIVALENCED?                                
         BNE   MGO40                                                            
         MR    RE,R6               (RAW VALUE X MISSED) UNIT LEN                
         SR    R6,R6                                                            
         ICM   R6,1,NETPROF+1                                                   
         DR    RE,R6                                                            
*                                                                               
MGO40    ICM   RE,15,12(R4)        TAKE VALUE FROM TABLE                        
         AR    RE,RF               AND TOTAL THEM                               
         STCM  RE,15,12(R4)        AND UPDATE THE ELEMENT IN TABLE              
*                                                                               
         B     MGO100                                                           
*                                                                               
MGO50    DS    0H                                                               
         MVC   0(12,R4),0(R3)      COPY X'DD' FROM MISSED UNIT TO TAB           
         OI    MYFLAG,MGDD                                                      
*                                                                               
*  GET EQUIVALENCED OVERRIDE VALUES AND PUT THEM IN TABLE ALSO                  
*  EQUIVALENCED OVERRIDE = (RAW VALUE X MISSED UNIT LEN) / EQUIV. LEN           
*                                                                               
         L     RF,AIOAREA2         MISSED UNIT LEN                              
         USING NURECD,RF                                                        
         SR    R6,R6                                                            
         ICM   R6,1,NULEN          UNIT LENGTH                                  
         DROP  RF                                                               
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,15,NUOVVAL       RAW VALUE                                    
*                                                                               
         CLI   NETPROF+1,X'1E'     EQUIVALENCED?                                
         BNE   MGO60                                                            
         MR    RE,R6               (RAW VALUE X MISSED) UNIT LEN                
         SR    R6,R6                                                            
         ICM   R6,1,NETPROF+1                                                   
         DR    RE,R6                                                            
MGO60    STCM  RF,15,12(R4)        AND UPDATE THE ELEMENT IN TABLE              
*                                                                               
         LA    R4,16(R4)                                                        
         MVI   0(R4),X'FF'                                                      
*                                                                               
MGO100   DS    0H                                                               
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     MGO10                                                            
         DROP  R3                                                               
*                                                                               
MGOVERX  DS    0H                                                               
         B     EXXMOD                                                           
**********************************************************************          
*   ADD OVERRIDES TO M/G UNIT PROGRAM RECORD                                    
*        INPUT:    AIOAREA4 - TABLE OF OVERRIDE ELEMENTS FROM                   
*                             MISSED UNITS                                      
*        OUTPUT:   PROGREC  - UPDATED M/G PROGRAM REC WITH DD'S                 
**********************************************************************          
MVDDPROG NTR1                                                                   
         L     R4,AIOAREA4                                                      
         L     R3,APROGREC                                                      
*                                                                               
MDP10    DS    0H                                                               
         CLI   0(R4),X'FF'           NO MORE DD'S?                              
         BE    MDPX                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(12),0(R4)        MOVE IN RAW VALUES                         
*                                                                               
         CLI   FRCSW,C'Y'            MORE THAN ONE MISSED?                      
         BNE   *+10                                                             
         MVC   WORK+8(4),12(R4)      MOVE IN EQUIVALENCED VALUES                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',SPTFILE),(R3),WORK,0                           
*                                                                               
         LA    R4,16(R4)                                                        
         B     MDP10                                                            
*                                                                               
MDPX     DS    0H                                                               
         B     EXXMOD                                                           
*                                                                               
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
MGCOMM   DC    C'** ERROR - COMMENT REQUIRED FOR MAKE-GOOD'                     
         SPACE 2                                                                
* TABLE OF DISPLAY FIELDS                                                       
*                                                                               
         DS    0F                                                               
DISTAB   DS    0CL4                                                             
         DC    AL1(UOTHER),AL3(UNTOTHH-UNTMGFH)                                 
         DC    AL1(UPRE),AL3(UNTPREH-UNTMGFH)                                   
         DC    AL1(USCHG),AL3(UNTCHRGH-UNTMGFH)                                 
         DC    AL1(UDA),AL3(UNTDAH-UNTMGFH)                                     
         DC    AL1(UDAY),AL3(UNTDAYH-UNTMGFH)                                   
         DC    AL1(UTIME),AL3(UNTTIMEH-UNTMGFH)                                 
         DC    AL1(UPRGN),AL3(UNTPROGH-UNTMGFH)                                 
         DC    AL1(ULEN),AL3(UNTLENH-UNTMGFH)                                   
         DC    AL1(UPRD),AL3(UNTPRDH-UNTMGFH)                                   
         DC    AL1(UP1SHR),AL3(UNTP1PCH-UNTMGFH)                                
         DC    AL1(UASS),AL3(UNTASSH-UNTMGFH)                                   
         DC    AL1(UACT),AL3(UNTACTH-UNTMGFH)                                   
         DC    AL1(UINT),AL3(UNTINTH-UNTMGFH)                                   
         DC    AL1(USHR),AL3(UNTSHRH-UNTMGFH)                                   
         DC    AL1(UHUT),AL3(UNTHUTH-UNTMGFH)                                   
         DC    AL1(URAT),AL3(UNTRATH-UNTMGFH)                                   
         DC    AL1(UHAVE),AL3(UNTHAVEH-UNTMGFH)                                 
         DC    AL1(UHSC),AL3(UNTHSCH-UNTMGFH)                                   
         DC    AL1(UHUTADJ),AL3(UNTHADJH-UNTMGFH)                               
         DC    AL1(UUNCD),AL3(UNTUCODH-UNTMGFH)                                 
         DC    AL1(UROT),AL3(UNTROTH-UNTMGFH)                                   
         DC    AL1(UIMP),AL3(UNTIMPH-UNTMGFH)                                   
         DC    AL1(UUNPC),AL3(UNTUPCTH-UNTMGFH)                                 
         DC    AL1(UFEED),AL3(UNTFEEDH-UNTMGFH)                                 
         DC    AL1(UAFFID),AL3(UNTAFFH-UNTMGFH)                                 
         DC    AL1(UNTI),AL3(UNTNTIH-UNTMGFH)                                   
         DC    AL1(UNSI),AL3(UNTNSIH-UNTMGFH)                                   
         DC    AL1(USREP),AL3(UNTSREPH-UNTMGFH)                                 
         DC    AL1(UBUYD),AL3(UNTBUYDH-UNTMGFH)                                 
         DC    AL1(ULSTA),AL3(UNTACTDH-UNTMGFH)                                 
         DC    AL1(USTAT),AL3(UNTSTATH-UNTMGFH)                                 
         DC    X'FF'                                                            
         SPACE 2                                                                
* LIST OF FIELDS TO EDIT                                                        
*                                                                               
EDLIST   DS    0H                                                               
         DC    AL1(UDAY)                                                        
         DC    AL1(UTIME)                                                       
         DC    AL1(UPRGN)                                                       
         DC    AL1(ULEN)                                                        
         DC    AL1(UPRD)                                                        
         DC    AL1(UP1SHR)                                                      
         DC    AL1(UASS)                                                        
         DC    AL1(UACT)                                                        
         DC    AL1(UINT)                                                        
         DC    AL1(USHR)                                                        
         DC    AL1(UHUT)                                                        
         DC    AL1(URAT)                                                        
         DC    AL1(UHAVE)                                                       
         DC    AL1(UHSC)                                                        
         DC    AL1(UHUTADJ)                                                     
         DC    AL1(UUNCD)                                                       
         DC    AL1(UROT)                                                        
         DC    AL1(UIMP)                                                        
         DC    AL1(UUNPC)                                                       
         DC    AL1(UFEED)                                                       
         DC    AL1(UAFFID)                                                      
         DC    AL1(UNTI)                                                        
         DC    AL1(UNSI)                                                        
         DC    AL1(USREP)                                                       
         DC    AL1(UOTHER)                                                      
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE OF ACTIONS AND THEIR NAMES                                              
*                                                                               
ACTTAB   DS    0CL10                                                            
         DC    AL1(B),CL9'ADDED'                                                
         DC    AL1(C),CL9'CHANGED'                                              
         DC    AL1(D),CL9'DISPLAYED'                                            
         DC    AL1(DEL),CL9'DELETED'                                            
ACTS     EQU   (*-ACTTAB)/L'ACTTAB                                              
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--STEWARD ROUTINES                                                             
*                                                                               
         DS    0F                                                               
         DROP  RA,RB,RC                                                         
OVFLRTN  NMOD1 0,**45ST**                                                       
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
OVBRANCH B     OVSTEWVL                                                         
         B     OVSTEWBL                                                         
         B     OVBLDREC                                                         
         EJECT                                                                  
* UPDATE SCREEN PUTTING IN VALUES FROM THE PC                                   
*                                                                               
OVSTEWVL L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
*                                                                               
         LA    R5,STEWREC                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
*                                                                               
         XC    XTRA,XTRA                                                        
         XC    FLAST,FLAST                                                      
         XC    FTERM,FTERM                                                      
         MVI   FTERM,C':'          SEARCH FOR COMMA AT END OF ACTION            
*                                                                               
*  CHECK ACTUAL COST                                                            
*                                                                               
         CLI   RUPNACTL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOACTL,0          CHECK FOR ANY INPUT                          
         BE    STEWV020                                                         
         LA    RF,UNTACTH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOACTL,0                                                       
         BE    STEWV010                                                         
         CLC   RUPOACT,NBACTUAL                                                 
         BNE   STEWVERR                                                         
STEWV010 MVC   NBACTUAL,RUPNACT                                                 
         OI    NBUNITST,X'20'       IF THERE IS A COST SET COST BIT             
         CLI   RUPNACTL,0           IS THERE A NEW ACTUAL COST                  
         BNE   *+8                                                              
         NI    NBUNITST,X'FF'-X'20'  RESET ACTUAL COST INPUT BIT                
         LA    R3,STDISACT          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK ASSIGNED COST                                                          
*                                                                               
STEWV020 MVI   ASSFREEZ,0                                                       
         TM    RUPSTAT,X'40'                                                    
         BZ    *+8                                                              
         MVI   ASSFREEZ,C'N'        DONT SET ASSIGNED COST TO FROZEN            
         CLI   RUPNASSL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOASSL,0          CHECK FOR ANY INPUT                          
         BE    STEWV040                                                         
         LA    RF,UNTASSH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOASSL,0                                                       
         BE    STEWV030                                                         
         CLC   RUPOASS,NBASSIGN                                                 
         BNE   STEWVERR                                                         
STEWV030 MVC   NBASSIGN,RUPNASS                                                 
         OI    NBUNITST,X'08'       IF THERE IS A COST SET COST BIT             
         CLI   RUPNASSL,0           IS THERE A NEW ASSIGNED COST                
         BNE   *+8                                                              
         NI    NBUNITST,X'FF'-X'08'  RESET ACTUAL COST INPUT BIT                
         LA    R3,STDISASS          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK INTEGRATION COST                                                       
*                                                                               
STEWV040 CLI   RUPNINTL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOINTL,0          CHECK FOR ANY INPUT                          
         BE    STEWV060                                                         
         LA    RF,UNTINTH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOINTL,0                                                       
         BE    STEWV050                                                         
         CLC   RUPOINT,NBINTEG                                                  
         BNE   STEWVERR                                                         
STEWV050 MVC   NBINTEG,RUPNINT                                                  
         OI    NBUNST4,X'80'        IF THERE IS A COST SET COST BIT             
         CLI   RUPNINTL,0           IS THERE A NEW INTEGRATION COST             
         BNE   *+8                                                              
         NI    NBUNST4,X'FF'-X'80'  RESET INTEGRATION COST INPUT BIT            
         LA    R3,STDISINT          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK DAY                                                                    
*                                                                               
STEWV060 CLI   RUPNDAYL,0                                                       
         BNE   *+12                                                             
         CLI   RUPODAYL,0          CHECK FOR ANY INPUT                          
         BE    STEWV080                                                         
         LA    RF,UNTDAYH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPODAYL,0                                                       
         BE    STEWV070                                                         
         CLC   RUPODAY,NBDAY                                                    
         BNE   STEWVERR                                                         
STEWV070 DS    0H                                                               
         BCDAY NBDAYNAM,RUPNDAY                                                 
         LA    R3,STDISDAY          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK ROTATION                                                               
*                                                                               
STEWV080 CLI   RUPNROTL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOROTL,0          CHECK FOR ANY INPUT                          
         BE    STEWV100                                                         
         LA    RF,UNTROTH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOROTL,0                                                       
         BE    STEWV090                                                         
         CLC   RUPOROT,NBSDROT                                                  
         BNE   STEWVERR                                                         
STEWV090 MVC   NBSDROT,RUPNROT                                                  
         LA    R3,STDISROT          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK TIME                                                                   
*                                                                               
STEWV100 CLI   RUPNTIML,0                                                       
         BNE   *+12                                                             
         CLI   RUPOTIML,0          CHECK FOR ANY INPUT                          
         BE    STEWV120                                                         
         LA    RF,UNTTIMEH                                                      
         ST    RF,FADDR                                                         
         CLI   RUPOTIML,0                                                       
         BE    STEWV110                                                         
         CLC   RUPOTIM,NBTIME                                                   
         BNE   STEWVERR                                                         
STEWV110 MVC   NBTIME,RUPNTIM                                                   
         LA    R3,STDISTIM          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK LENGTH                                                                 
*                                                                               
STEWV120 CLI   RUPNLENL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOLENL,0          CHECK FOR ANY INPUT                          
         BE    STEWV140                                                         
         LA    RF,UNTLENH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOLENL,0                                                       
         BE    STEWV130                                                         
         CLC   RUPOLEN,NBLEN                                                    
         BNE   STEWVERR                                                         
STEWV130 MVC   NBLEN,RUPNLEN                                                    
         LA    R3,STDISLEN          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK PRODUCT                                                                
*                                                                               
STEWV140 CLI   RUPNPRDL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOPRDL,0          CHECK FOR ANY INPUT                          
         BE    STEWV160                                                         
         LA    RF,UNTPRDH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOPRDL,0                                                       
         BE    STEWV150                                                         
*                                                                               
         XC    FLAST,FLAST                                                      
         LA    RF,UNTPRDH                                                       
         ST    RF,FADDR                                                         
         GOTO1 AFVAL,0                                                          
         CLC   RUPOPRDL,FLDH+5                                                  
         BNE   STEWVERR                                                         
         ZIC   R1,RUPOPRDL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+12                                                          
         BE    STEWV150                                                         
         B     STEWVERR                                                         
         CLC   RUPOPRD(0),UNTPRD                                                
*                                                                               
STEWV150 MVC   UNTPRDH+5(1),RUPNPRDL                                            
         XC    UNTPRD,UNTPRD                                                    
         CLI   RUPNPRDL,0                                                       
         BE    STEWV160                                                         
         ZIC   R1,RUPNPRDL                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     STEWV160                                                         
         MVC   UNTPRD(0),RUPNPRD                                                
*                                                                               
*  CHECK NTI CODE                                                               
*                                                                               
STEWV160 CLI   RUPNNTIL,0                                                       
         BNE   *+12                                                             
         CLI   RUPONTIL,0          CHECK FOR ANY INPUT                          
         BE    STEWV180                                                         
         LA    RF,UNTNTIH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPONTIL,0                                                       
         BE    STEWV170                                                         
         CLC   RUPONTI,NBNTI                                                    
         BNE   STEWVERR                                                         
STEWV170 MVC   NBNTI,RUPNNTI                                                    
         LA    R3,STDISNTI          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK HUT CODE                                                               
*                                                                               
STEWV180 CLI   RUPNHUTL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOHUTL,0          CHECK FOR ANY INPUT                          
         BE    STEWV200                                                         
         LA    RF,UNTHUTH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOHUTL,0                                                       
         BE    STEWV190                                                         
         CLC   RUPOHUT,NBESTHUT                                                 
         BNE   STEWVERR                                                         
STEWV190 MVC   NBESTHUT,RUPNHUT                                                 
         LA    R3,STDISHUT          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK SHARE                                                                  
*                                                                               
STEWV200 CLI   RUPNSHRL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOSHRL,0          CHECK FOR ANY INPUT                          
         BE    STEWV220                                                         
         LA    RF,UNTSHRH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOSHRL,0                                                       
         BE    STEWV210                                                         
         CLC   RUPOSHR,NBESTSHR                                                 
         BNE   STEWVERR                                                         
STEWV210 MVC   NBESTSHR,RUPNSHR                                                 
         LA    R3,STDISSHR          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK HOMES RATING                                                           
*                                                                               
STEWV220 CLI   RUPNHRTL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOHRTL,0          CHECK FOR ANY INPUT                          
         BE    STEWV240                                                         
*                                                                               
         BRAS  RE,CHKHRTG          CHECK HOMES RATING                           
         LA    R3,STDISRTG          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK AFFID TIME                                                             
*                                                                               
STEWV240 CLI   RUPNATML,0                                                       
         BNE   *+12                                                             
         CLI   RUPOATML,0          CHECK FOR ANY INPUT                          
         BE    STEWV260                                                         
         LA    RF,UNTAFFH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOATML,0                                                       
         BE    STEWV250                                                         
         CLC   RUPOATM,NBAFFTIM                                                 
         BNE   STEWVERR                                                         
STEWV250 MVC   NBAFFTIM,RUPNATM                                                 
         LA    R3,STDISAFF          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
         CLI   RUPOATML,0          CHECK FOR ANY INPUT                          
         BNE   STEWV260                                                         
         XC    NBAFFTIM,NBAFFTIM    DONT HOLD AFFID INFO IF ADD                 
*                                                                               
*  CHECK SPECIAL REP                                                            
*                                                                               
STEWV260 CLI   RUPNREPL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOREPL,0          CHECK FOR ANY INPUT                          
         BE    STEWV280                                                         
         LA    RF,UNTAFFH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOREPL,0                                                       
         BE    STEWV270                                                         
         CLC   RUPOREP,NBSREP                                                   
         BNE   STEWVERR                                                         
STEWV270 MVC   NBSREP,RUPNREP                                                   
         LA    R3,STDISREP          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK HUT ADJSUTMENT                                                         
*                                                                               
STEWV280 CLC   RUPPKUI(5),=CL5'PACKU'    CHECK PACKU CALL                       
         BNE   STEWV290                                                         
         CLI   RUPHADJL,0                                                       
         BE    *+10                                                             
         MVC   NBHUTPCT,RUPHTADJ   MOVE IN NEW HUT VALUE                        
         LA    R3,STDISHAD          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK UNIVERSE                                                               
*                                                                               
STEWV290 CLC   RUPPKUI(5),=CL5'PACKU'    CHECK PACKU CALL                       
         BNE   STEWV300                                                         
         CLI   RUPUNIVL,0                                                       
         BE    *+10                                                             
         MVC   NBUNCODE,RUPUNIV    MOVE IN NEW UNIVERSE                         
         LA    R3,STDISUNV          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  CHECK UNIVERSE PERCENT                                                       
*                                                                               
STEWV300 CLC   RUPPKUI(5),=CL5'PACKU'    CHECK PACKU CALL                       
         BNE   STEWV400                                                         
         CLI   RUPUNPCL,0                                                       
         BE    *+10                                                             
         MVC   NBUNIV,RUPUNPCT     MOVE IN NEW UNIVERSE PCT                     
         LA    R3,STDISUPC          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  THE FOLLOWING FIELDS DO NOT DO A SCREEN CHECK                                
*                                                                               
*  CHECK UNIT DATE (CAUSES A DAY CHANGE)                                        
*                                                                               
STEWV400 XC    NEWDATE,NEWDATE                                                  
         OC    RUPNDAT,RUPNDAT                                                  
         BZ    STEWV420                                                         
         BAS   RE,SETDATE           CHG DATE AND DAY FIELDS IN NETBLOCK         
         LA    R3,STDISDAY          MOVE TO SCREEN                              
         BAS   RE,STEWVDIS                                                      
*                                                                               
*  PROGRAM NAME                                                                 
STEWV420 OC    RUPNPNM,RUPNPNM                                                  
         BZ    *+16                                                             
         MVC   UNTPROG,RUPNPNM                                                  
         MVC   UNTPROGH+5(1),RUPNPNML                                           
*  M/G DEMO                                                                     
         CLI   UNTCOM,X'40'        WAS VALUE INPUTTED                           
         BH    *+12                                                             
         MVI   UNTCOM,C'N'                                                      
         MVI   UNTCOMH+5,1                                                      
         OC    RUPNMGD,RUPNMGD                                                  
         BZ    *+14                                                             
         MVC   UNTCOM,RUPNMGD                                                   
         MVI   UNTCOMH+5,1                                                      
*  PRE-EMPT                                                                     
         OC    RUPNPRE,RUPNPRE                                                  
         BZ    *+14                                                             
         MVC   UNTPRE,RUPNPRE                                                   
         MVI   UNTPREH+5,1                                                      
*  COMMENT LINE 1                                                               
         MVI   COMINPT,C'N'                                                     
         CLI   RUPCOMT1,X'FF'      CLEAR COMMENT FIELD                          
         BNE   STEWV500                                                         
         MVI   UNTCOM1H+5,0                                                     
         XC    UNTCOM1,UNTCOM1                                                  
         B     STEWV520                                                         
STEWV500 OC    RUPCOMT1,RUPCOMT1                                                
         BZ    *+14                                                             
         MVC   UNTCOM1,RUPCOMT1                                                 
         MVI   COMINPT,C'Y'                                                     
*  COMMENT LINE 2                                                               
*  IF FIELD BEGINS WITH "DSC###" OR HAS FLT### +40 IT IS NOT                    
*  A COMMENT IT IS BEING USED TO HOLD THE DEAL/CONTRACT/SERIAL#                 
*  OR THE FLIGHT NUMBER.                                                        
STEWV520 CLC   RUPCOMT2(6),=CL6'DSC###'  CHECK IF COMMENT                       
         BE    STEWV525                                                         
         CLC   RUPCOMT2+40(6),=CL6'FLT###'  CHECK IF COMMENT                    
         BE    STEWV525                                                         
         CLI   RUPCOMT2,X'FF'      CLEAR COMMENT FIELD                          
         BNE   STEWV530                                                         
STEWV525 MVI   UNTCOM2H+5,0                                                     
         XC    UNTCOM2,UNTCOM2                                                  
         B     STEWV540                                                         
STEWV530 OC    RUPCOMT2,RUPCOMT2                                                
         BZ    *+14                                                             
         MVC   UNTCOM2,RUPCOMT2                                                 
         MVI   COMINPT,C'Y'                                                     
*  MAKEGOOD                                                                     
STEWV540 CLC   RUPPKUI(5),=CL5'PACKU'                                           
         BE    STEWVEX                                                          
         OC    RUPMISS,RUPMISS                                                  
         BZ    *+16                                                             
         MVC   UNTMGF,RUPMISS                                                   
         MVC   UNTMGFH+5(1),RUPMISSL                                            
         B     STEWVEX                                                          
*                                                                               
STEWVERR MVI   FERN,STCHAERR                                                    
*        LA    RE,HUTBLK                                                        
*        USING ERRDATA,RE                                                       
*        MVC   ERMXFLD(6),=CL6'FIELD='                                          
*        MVC   ERMXTRA(20),XTRA                                                 
*        LA    R2,BUYACTH                                                       
*        ST    R2,FADDR                                                         
         B     OVERROR                                                          
*        DROP  RE                                                               
*                                                                               
STEWVEX  B     OVEXIT                                                           
         DROP  R5                                                               
*                                                                               
         SPACE 3                                                                
STEWVDIS NTR1                                                                   
         ZIC   R0,0(R3)            DATA TYPE                                    
         ICM   R2,7,1(R3)          DISPLACEMENT FROM FIRST DISPLAY HDR.         
         LA    R2,UNTMGFH(R2)                                                   
         L     RF,VDISPLAY                                                      
         PRINT GEN                                                              
         GOTO1 (RF),DMCB,((R0),(R9))                                            
         PRINT NOGEN                                                            
         ZIC   RE,0(R2)            DEVELOP FIELD DATA LENGTH                    
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD         OUTPUT DATA TO SCREEN                        
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         B     STEWVEX                                                          
         SPACE 3                                                                
* SUB-ROUTINE TO EDIT DATE INPUT                                                
*                                                                               
SETDATE  NTR1                                                                   
         L     R3,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,R3                                                       
         LA    R5,STEWREC                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         XC    FLD,FLD                                                          
         GOTO1 VDATCON,DMCB,(2,RUPNDAT),(8,FLD)                                 
         GOTO1 VDATVAL,DMCB,(0,FLD),DUB                                         
         MVI   FERN,DATERR                                                      
         OC    0(4,R1),0(R1)                                                    
         BZ    OVERROR                                                          
         MVI   FERN,STERR                                                       
         CLC   DUB(6),ESTSTART                                                  
         BL    OVERROR                                                          
         MVI   FERN,ENDERR                                                      
         CLC   DUB(6),ESTEND                                                    
         BH    OVERROR                                                          
*******  MVC   DUB(2),ESTSTART                                                  
*******  CLC   ESTSTART(2),ESTEND                                               
*******  BE    STDATE2                                                          
*******  CLC   DUB+2(4),ESTSTART+2                                              
*******  BNL   *+10                                                             
*******  MVC   DUB(2),ESTEND                                                    
         SPACE                                                                  
STDATE2  GOTO1 VDATCON,(R1),DUB,(2,HALF)                                        
         MVC   NEWDATE,HALF        SET INPUT DATE                               
         GOTO1 VGETDAY,DMCB,DUB,NBDAYNAM                                        
         SPACE                                                                  
         B     STEWVEX                                                          
         DROP  R3,R5                                                            
         SPACE 3                                                                
* TABLE OF DISPLAY FIELDS FOR STEWARD CHANGES                                   
*                                                                               
STDISDAY DC    AL1(UDAY),AL3(UNTDAYH-UNTMGFH)                                   
STDISTIM DC    AL1(UTIME),AL3(UNTTIMEH-UNTMGFH)                                 
STDISLEN DC    AL1(ULEN),AL3(UNTLENH-UNTMGFH)                                   
STDISASS DC    AL1(UASS),AL3(UNTASSH-UNTMGFH)                                   
STDISACT DC    AL1(UACT),AL3(UNTACTH-UNTMGFH)                                   
STDISAFF DC    AL1(UAFFID),AL3(UNTAFFH-UNTMGFH)                                 
STDISINT DC    AL1(UINT),AL3(UNTINTH-UNTMGFH)                                   
STDISROT DC    AL1(UROT),AL3(UNTROTH-UNTMGFH)                                   
STDISNTI DC    AL1(UNTI),AL3(UNTNTIH-UNTMGFH)                                   
STDISSHR DC    AL1(USHR),AL3(UNTSHRH-UNTMGFH)                                   
STDISHUT DC    AL1(UHUT),AL3(UNTHUTH-UNTMGFH)                                   
STDISRTG DC    AL1(URAT),AL3(UNTRATH-UNTMGFH)                                   
STDISREP DC    AL1(USREP),AL3(UNTSREPH-UNTMGFH)                                 
STDISHAD DC    AL1(UHUTADJ),AL3(UNTHADJH-UNTMGFH)                               
STDISUNV DC    AL1(UUNCD),AL3(UNTUCODH-UNTMGFH)                                 
STDISUPC DC    AL1(UUNPC),AL3(UNTUPCTH-UNTMGFH)                                 
         SPACE 2                                                                
         EJECT                                                                  
* BUILD THE BUY RECORD FROM THE STEWARD RECORD                                  
*                                                                               
OVSTEWBL L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         LA    R3,STEWREC                                                       
         USING BUYUPLDD,R3                                                      
*                                                                               
* ADU                                                                           
         CLI   RUPNADU,X'40'                                                    
         BNH   STEWB20                                                          
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'02',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         USING NUSDREL,RE                                                       
*                                                                               
         CLI   RUPNADU,C'Y'                                                     
         BE    STEWB05                                                          
         CLI   RUPNADU,C'N'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    NUSDST3,X'FD'        TURN OFF ADU SETTING                        
         B     STEWB20                                                          
*                                                                               
STEWB05  MVI   FERN,ADUERR                                                      
         OC    NUACTUAL,NUACTUAL                                                
         BNZ   OVERROR                                                          
         OC    NUINTEG,NUINTEG                                                  
         BNZ   OVERROR                                                          
         OI    NUSDST3,X'02'        TURN ON ADU SETTING                         
         DROP  RE                                                               
*                                                                               
* BILLBOARD                                                                     
STEWB20  CLI   RUPNBLB,X'40'                                                    
         BNH   STEWB30                                                          
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'21',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    STEWB25                                                          
*                                                                               
         XC    ELEMHLD,ELEMHLD                                                  
         MVC   ELEMHLD(2),=XL2'2152'                                            
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,ELEMHLD,0                    
         B     STEWB20                                                          
*                                                                               
STEWB25  L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         USING NUCMLEL,RE                                                       
*                                                                               
         CLI   RUPNBLB,C'Y'                                                     
         BNE   *+12                                                             
         OI    NUCMLFLG,X'04'       SET BILLBOARD                               
         B     STEWB30                                                          
         CLI   RUPNBLB,C'N'                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    NUCMLFLG,X'FB'       TURN OFF BILLBOARD                          
         DROP  RE                                                               
*                                                                               
* DEAL CONTRACT SEIAL NUMBER                                                    
STEWB30  CLC   RUPCOMT2(6),=CL6'DSC###'                                         
         BNE   STEWB40                                                          
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'75',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    STEWB35                                                          
*                                                                               
         XC    ELEMHLD,ELEMHLD                                                  
         MVC   ELEMHLD(2),=XL2'7528'                                            
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,ELEMHLD,0                    
         B     STEWB30                                                          
*                                                                               
STEWB35  L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         USING NUSQD,RE                                                         
         MVC   NUSQSER(32),RUPCOMT2+6                                           
         DROP  RE                                                               
*                                                                               
* FLIGHT NUMBER                                                                 
STEWB40  CLC   RUPCOMT2+40(6),=CL6'FLT###'                                      
         BNE   STEWB45                                                          
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'60',(R4)),(1,=C'1')               
         CLC   RUPCOMT2+46(3),=CL3'DEL'  REMOVE FLIGHT OVERRIDE                 
         BE    STEWB45                                                          
*                                                                               
*  BUILD THE ELEMENT                                                            
         XC    ELEMHLD,ELEMHLD                                                  
         LA    RE,ELEMHLD                                                       
         USING NUOTH,RE                                                         
         MVI   NUOTEL,X'60'                                                     
         MVI   NUOTLEN,13                                                       
         MVI   NUOTTYP,C'1'                                                     
         MVC   NUOTHER(10),RUPCOMT2+46                                          
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,ELEMHLD,0                    
*                                                                               
* BUY TYPE                                                                      
STEWB45  OC    RUPNBYTL,RUPNBYTL                                                
         BZ    STEWB50                                                          
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'60',(R4)),(1,=C'F')               
*                                                                               
         CLI   RUPNBYT,X'40'       REMOVE THE BUY TYPE?                         
         BNH   STEWB60                                                          
*                                                                               
         MVC   WORK(2),=XL2'6004'                                               
         MVI   WORK+2,C'F'                                                      
         MVC   WORK+3(1),RUPNBYT                                                
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
*&&DO                                                                           
* COMBO                                                                         
STEWB50  OC    RUPNCBO,RUPNCBO                                                  
         BZ    STEWB55                                                          
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'60',(R4)),(1,=C'I')               
*                                                                               
         CLI   RUPNCBO,C'^'        REMOVE THE BUY TYPE?                         
         BE    STEWB55                                                          
*                                                                               
         MVC   WORK(2),=XL2'6009'                                               
         MVI   WORK+2,C'I'                                                      
         MVC   WORK+3(3),RUPNCBO                                                
         ZIC   R6,RUPNCBO+3                                                     
         EDIT  (R6),(3,WORK+6),ALIGN=LEFT,FILL=0,WRK=WORK+20                    
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*&&                                                                             
* COMBO IS NOW FREEFORM (SPEC-43030)                                            
STEWB50  OC    RUPNCBO2,RUPNCBO2                                                
         BZ    STEWB55                                                          
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'60',(R4)),(1,=C'I')               
*                                                                               
         CLI   RUPNCBO2,C'^'       REMOVE COMBO CODE?                           
         BE    STEWB55                                                          
*                                                                               
         MVI   WORK,X'60'          OTHER ELEMENT                                
         MVI   WORK+1,9                                                         
         MVI   WORK+2,C'I'         COMBO CODE                                   
         MVC   WORK+3(6),RUPNCBO2                                               
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
* POD                                                                           
STEWB55  OC    RUPNPOD,RUPNPOD                                                  
         BZ    STEWB60                                                          
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'60',(R4)),(1,=C'Y')               
*                                                                               
         CLI   RUPNPOD,C'^'         REMOVE THE POD                              
         BE    STEWB60                                                          
*                                                                               
         MVC   WORK(2),=XL2'6006'                                               
         MVI   WORK+2,C'Y'                                                      
         MVC   WORK+3(3),RUPNPOD                                                
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
* SUB DAYPART                                                                   
STEWB60  CLI   RUPNSDPL,0                                                       
         BE    STEWB80                                                          
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'60',(R4)),(1,=C'D')               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'60',(R4)),(1,=C'U')               
*                                                                               
         CLI   RUPNSDP,X'0'         REMOVE THE SUB DAYPART                      
         BE    STEWB80                                                          
*                                                                               
         GOTO1 VALDAYPT,DMCB,(1,RUPNSDP)                                        
*                                                                               
         LA    RE,DPTTAB                                                        
         LA    R0,DAYPARTS                                                      
*                                                                               
         MVI   WORK,X'60'                                                       
         MVI   WORK+2,C'U'                                                      
         MVC   WORK+3(2),KEY+21                                                 
         MVC   WORK+5(14),KEY+6                                                 
*                                                                               
* GET LENGTH OF DESCRIPTION                                                     
* STORE ELEMENT LENGTH                                                          
*                                                                               
         LA    RE,KEY+19                                                        
         LA    RF,14                                                            
*                                                                               
STEWB65  CLI   0(RE),X'40'                                                      
         BH    STEWB70                                                          
         BCTR  RE,0                                                             
         BCT   RF,STEWB65                                                       
STEWB70  LA    RE,5                                                             
         AR    RE,RF                                                            
         STCM  RE,1,WORK+1                                                      
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
* NEW PROGRAM RECORD INFO                                                       
STEWB80  GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'18',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         ZIC   R1,1(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),0(RE)                                                    
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'18',(R4)),0                       
*                                                                               
         LA    R6,WORK              REMOVE THE SUB DAYPART                      
         USING NUDTAD,R6                                                        
*--PROGRAM TYPE                                                                 
         CLI   RUPPCNTL,0                                                       
         BE    STEWB82                                                          
         XC    NUDTATYP,NUDTATYP                                                
         XC    NUDTASPT,NUDTASPT                                                
         CLC   RUPPCONT,SPACES                                                  
         BE    STEWB82                                                          
         MVC   NUDTATYP,RUPPCONT                                                
         MVC   NUDTASPT,RUPPCONT+2                                              
*--NEW OR RETURNINIG                                                            
STEWB82  CLI   RUPPNEWL,0                                                       
         BE    STEWB83                                                          
         MVI   NUDTANEW,0                                                       
         CLI   RUPPCONT,X'40'                                                   
         BE    STEWB83                                                          
         MVC   NUDTANEW,RUPPNEW                                                 
*                                                                               
*--TIER                                                                         
STEWB83  CLI   RUPPTIRL,0                                                       
         BE    STEWB84                                                          
         MVI   NUDTATIR,0                                                       
         CLI   RUPPTIER,X'40'                                                   
         BE    STEWB84                                                          
         MVC   NUDTATIR,RUPPTIER                                                
*                                                                               
*---PROGRAM RATING CODE                                                         
STEWB84  CLI   RUPPRTGL,0                                                       
         BE    STEWB85                                                          
         XC    NUDTARAT,NUDTARAT                                                
         CLC   RUPPRTGC,SPACES                                                  
         BE    STEWB85                                                          
         MVC   NUDTARAT,RUPPRTGC                                                
*                                                                               
*---INVOICE NUMBER                                                              
STEWB85  CLC   RUPINV(7),=CL7'INVOICE'                                          
         BNE   STEWB86                                                          
         CLI   RUPINVTP,C'I'        CHECK INTEGRATION ONLY INVOICE              
         BE    STEWB85C                                                         
         XC    NUDTINVN,NUDTINVN                                                
         CLC   RUPINV+7(10),=10C','   CLEAR INVOICE INDICATOR                   
         BE    *+10                                                             
         MVC   NUDTINVN,RUPINV+7    MOVE IN TIME INVOICE NUMBER                 
*                                                                               
STEWB85C CLI   RUPINVTP,C'T'        CHECK TIME ONLY INVOICE                     
         BE    STEWB85E                                                         
         XC    NUDTIIVN,NUDTIIVN                                                
         CLC   RUPINV+7(10),=10C','   CLEAR INVOICE INDICATOR                   
         BE    *+10                                                             
         MVC   NUDTIIVN,RUPINV+7    MOVE IN TNTG INVOIVE NUMBER                 
STEWB85E XC    RUPINV,RUPINV                                                    
*                                                                               
*---GAP AFFID OVERIDE                                                           
STEWB86  CLI   RUPGAPAL,1                                                       
         BL    STEWB87                                                          
         XC    NUDTGAFD,NUDTGAFD                                                
         CLC   RUPGAPAF,=H'3000'    CHECK FOR DELETE TIME                       
         BH    STEWB87                                                          
         MVC   NUDTGAFD,RUPGAPAF                                                
*                                                                               
* VIEWING STREAM                                                                
STEWB87  CLC   RUPPKUI(5),=CL5'PACKU'    CHECK PACKU CALL                       
         BNE   STEWB88                                                          
         CLI   RUPVSTR,X'40'                                                    
         BNH   STEWB88                                                          
         XC    NUDVTYPE,NUDVTYPE                                                
         NI    NUUNST5,X'FF'-NST5PMBM     SET POD INDICATOR                     
         CLI   RUPVSTR,C'^'                                                     
         BE    STEWB88                                                          
         MVC   NUDVTYPE(2),RUPVSTR                                              
         CLI   RUPVSTR+2,C'P'       CHECK FOR POD                               
         BNE   *+8                                                              
         OI    NUUNST5,NST5PMBM     SET POD INDICATOR                           
*                                                                               
* HUB INDICATORS                                                                
STEWB88  TM    RUPST1,X'80'        IS THIS FROM THE HUB                         
         BZ    STEWB90                                                          
         CLI   ACTION,B            TEST FOR BUY                                 
         BNE   *+12                                                             
         OI    NUUNST5,NST5HUBA                                                 
         B     STEWB90                                                          
         CLI   ACTION,C            TEST FOR CHANGE                              
         BNE   STEWB90                                                          
         OI    NUUNST5,NST5HUBC                                                 
*                                                                               
STEWB90  GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
* SPECIAL CHARGES                                                               
*                                                                               
         BRAS  RE,STEWSCHR                                                      
*                                                                               
* COPY SPLIT HANDLER                                                            
*                                                                               
         BRAS  RE,STEWCSPT                                                      
*                                                                               
* CHECK WINDOW DATE                                                             
*                                                                               
         BRAS  RE,STEWWIN                                                       
*                                                                               
* CHECK COMSCORE SERIES #                                                       
*                                                                               
         BRAS  RE,STEWCS#                                                       
*                                                                               
* CHECK AIR NETWORK                                                             
*                                                                               
         BRAS  RE,STEWAIR                                                       
*                                                                               
* PACKAGE FILTER                                                                
STEWB300 CLC   RUPPKUI(5),=CL5'PACKU'    CHECK PACKU CALL                       
         BNE   STEWB310                                                         
         CLI   RUPPKFLT,X'40'                                                   
         BNH   STEWB310                                                         
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'08',(R4)),(1,=C'K')               
         CLI   RUPPKFLT,C'^'       REMOVE PACKAGE FILTER                        
         BE    STEWB310                                                         
*                                                                               
*  BUILD THE ELEMENT                                                            
         XC    ELEMHLD,ELEMHLD                                                  
         LA    RE,ELEMHLD                                                       
         USING NUFILD,RE                                                        
         MVI   NUFILEL,X'08'                                                    
         MVI   NUFILLEN,11                                                      
         MVI   NUFILSCM,C'K'                                                    
         MVC   NUFILTER,RUPPKFLT                                                
         OC    NUFILTER,SPACES                                                  
         DROP  RE                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,ELEMHLD,0                    
*                                                                               
* AUDIT INFORMATION                                                             
STEWB310 CLC   RUPPKUI(5),=CL5'PACKU'    CHECK PACKU CALL                       
         BNE   STEWB330                                                         
         XC    WORK,WORK                                                        
         MVI   WORK,X'09'                                                       
         MVI   WORK+1,NUAUDELN                                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'09',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   STEWB320                                                         
         L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         ZIC   R1,1(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   WORK(0),0(RE)                                                    
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'09',(R4)),0                        
*                                                                               
STEWB320 LA    RE,WORK                                                          
         USING NUAUDD,RE                                                        
*                                                                               
         CLI   RUPANME,X'40'                                                    
         BNH   *+10                                                             
         MVC   NUAUDCOM,RUPANME                                                 
         CLI   RUPACDE,X'40'                                                    
         BNH   *+10                                                             
         MVC   NUAUDGRP,RUPACDE                                                 
         OC    NUAUDGRP(20),NUAUDGRP                                            
         BZ    STEWB330                                                         
         DROP  RE                                                               
                                                                                
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'P',OVUNFLE),(X'09',(R4)),WORK                    
**                                                                              
*                                                                               
STEWB330 OC    NUDTINVN,NUDTINVN    CHECK IF TIME INVOICE EXISTS                
         BNZ   STEWB400                                                         
         OC    NUDTIIVN,NUDTIIVN    CHECK IF INTG INVOICE EXISTS                
         BNZ   STEWB400                                                         
*                                                                               
*** IF NO INVOICES EXIST UNDER UNIT CLEAR                                       
*** OUT THE AFFID DATE, AND AFFID TIME FIELDS                                   
*                                                                               
         B     STEWB400                                                         
**************************                                                      
********  THIS CODE IS BEING BYPASSED  ******************                       
**************************                                                      
         XC    NUAFFTIM,NUAFFTIM                                                
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'02',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)           COPY THE 3RD STATUS BIT                      
         USING NUSDRD,RE                                                        
         XC    NUSDAFDT,NUSDAFDT                                                
         DROP  R6,RE                                                            
*                                                                               
*  HOMES IMPRESSION                                                             
STEWB400 CLI   RUPNHIML,0                                                       
         BE    STEWB500                                                         
***      OC    RUPNHIM,RUPNHIM                                                  
***      BZ    STEWB500                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(3),=XL3'00C801'                                             
         MVC   WORK+3(4),RUPNHIM                                                
         LA    R6,WORK                                                          
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,STEWDEMO                                                      
*                                                                               
*  DEMO OVERRIDES ESTIMATED                                                     
STEWB500 LA    R6,RUPDEMOS                                                      
         MVI   ELCODE,X'DD'                                                     
         BAS   RE,STEWDEMO                                                      
*                                                                               
*  ACTUAL HOMES RATING                                                          
         CLI   RUPNHRAL,0                                                       
         BE    STEWB550                                                         
         XC    WORK,WORK                                                        
         MVC   WORK(3),=XL3'00D901'                                             
         MVC   WORK+5(2),RUPNHRA                                                
         LA    R6,WORK                                                          
         MVI   ELCODE,X'DE'                                                     
         BAS   RE,STEWDEMO                                                      
*                                                                               
*  DEMO OVERRIDES ACTUAL                                                        
STEWB550 LA    R6,RUPDEMOA                                                      
         MVI   ELCODE,X'DE'                                                     
         BAS   RE,STEWDEMO                                                      
         MVI   ELCODE,0                                                         
*                                                                               
*  PRISMA ID                                                                    
STEWB560 OC    RUPPRSID,RUPPRSID   ANY PRISMA ID?                               
         JZ    STEWBEX                                                          
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'26',(R4)),0                        
*                                                                               
         LA    R6,WORK                                                          
         USING NUPSELD,R6                                                       
         XC    WORK,WORK                                                        
         MVI   NUPSEL,NUPSELQ                                                   
         MVI   NUPSLEN,NUPSLENQ                                                 
         GOTO1 VDATCON,DMCB,(5,0),(2,NUPSCDT)                                   
         MVC   NUPSID,RUPPRSID                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 VHELLO,DMCB2,(C'P',OVUNFLE),(X'26',(R4)),WORK                    
*                                                                               
STEWBEX  B     OVEXIT                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
*          DATA SET NEBUY35    AT LEVEL 030 AS OF 06/30/00                      
*  DAYPART TABLE                                                                
DPTTAB   DS    0CL10       BYTE 9 = LENGTH OF ENTRY PLUS 1                      
         DC    C'D',CL8'DAYTIME',XL1'8'                                         
         DC    C'F',CL8'FRINGE',XL1'7'                                          
         DC    C'P',CL8'PRIME',XL1'6'                                           
         DC    C'K',CL8'KIDS',XL1'5'                                            
         DC    C'T',CL8'TEENS',XL1'6'                                           
         DC    C'Y',CL8'YOUTH',XL1'6'                                           
         DC    C'S',CL8'SPORTS',XL1'7'                                          
         DC    C'N',CL8'NEWS',XL1'5'                                            
         DC    C'E',CL8'EARLY',XL1'6'                                           
         DC    C'L',CL8'LATE',XL1'5'                                            
         DC    C'H',CL8'OTHER',XL1'6'                                           
         DC    C'J',CL8'PROMO-ID',XL1'9'                                        
         DC    C'C',CL8'CABLE',XL1'6'                                           
         DC    C'O',CL8'OLYMPICS',XL1'9'                                        
         DC    C'R',CL8'RADIO',XL1'6'                                           
         DC    C'X',CL8'SYND.',XL1'6'                                           
         DC    C'I',CL8'SPECIAL',XL1'8'                                         
         DC    C'V',CL8'OVERNITE',XL1'9'                                        
         DC    C'W',CL8'WKNDPM',XL1'7'                                          
         DC    C'M',CL8'WKNDAM',XL1'7'                                          
         DC    C'A',CL8'ACCESS',XL1'7'                                          
         DC    C'B',CL8'CBLSPORT',XL1'9'                                        
         DC    C'Q',CL8'INTRACTV',XL1'9'                                        
         DC    C'U',CL8'UNWIRED',XL1'8'                                         
DAYPARTS EQU   (*-DPTTAB)/L'DPTTAB                                              
         EJECT                                                                  
* SUB-ROUTINE TO BUILD RECORDS FOR STEWARD                                      
*                                                                               
OVBLDREC CLI   ACTION,DEL          CHECK ACTION DELETE                          
         BE    BLDR180                                                          
*                                                                               
         BAS   RE,REVALUE           CALL NETVALUE REVALUE THE NETBLOCK          
*                                                                               
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
BLDR180  L     R4,AIOAREA4                                                      
         USING DRAFRECD,R4                                                      
         XCEF  (R4),1000                                                        
*                                                                               
         LA    RE,STEWREC                                                       
         USING BUYUPLDD,RE                                                      
         MVC   DRFSEQN,RUPSEQ                                                   
         CLI   ACTION,DEL           CHECK ACTION DELETE                         
         BNE   BLDR190                                                          
         MVC   DRFNUM(3),=CL3'DEL'  SET DELETE STATUS                           
         B     BLDR400                                                          
         DROP  RE                                                               
*                                                                               
BLDR190  CLI   ACTION,REFC         TEST FOR REFRESH CHANGE                      
         BNE   *+8                                                              
         OI    DRFSTAT,X'80'       SET REFRESH STATUS                           
         MVC   DRFDATE,NBACTDAT                                                 
         MVC   DRFDAY(1),NBDAY                                                  
         MVC   DRFTIME,NBTIME                                                   
         MVC   DRFLEN,NBLEN                                                     
         MVC   DRFROT,NBSDROT                                                   
         MVC   DRFDPT,NBACTDP                                                   
         MVC   DRFSQTR,NBACTSQH                                                 
         MVC   DRFINT,NBINTEG                                                   
         MVC   DRFPNAM,NBPROGNM                                                 
         MVC   DRFNTI,NBNTI                                                     
         MVC   DRFSLN,NBACTSUB                                                  
         MVC   DRFMEDTP,NBSTATYP                                                
         MVC   DRFPSTTP,NBPOSTYP                                                
* DEMO BASE FROM PACKAGE                                                        
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         MVI   DRFDEMBS,C'V'                                                    
         TM    NPAKCNTL,X'40'                                                   
         BZ    *+8                                                              
         MVI   DRFDEMBS,C'I'                                                    
         DROP  RE                                                               
* HOMES DEMOS                                                                   
         MVC   DRFHOMSH,NBESTSHR                                                
         MVC   DRFHOMHT,NBESTHUT                                                
         MVC   DRFHOMRT,NBESTHOM+2                                              
         MVC   DRFHOMIM,NBESTHOM+4                                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'60',AIOAREA1),(1,=C'F')            
         CLI   12(R1),0                                                         
         BNE   BLDR260                                                          
         L     RE,12(R1)                                                        
         USING NUOTH,RE                                                         
         MVC   DRFBTYP,NUOTHER                                                  
         DROP  RE                                                               
*                                                                               
BLDR260  L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         LA    RF,STEWREC                                                       
         USING BUYUPLDD,RF                                                      
         MVC   DRFDEMS,NDESTDEM                                                 
         XC    DRFRESLT,DRFRESLT    CLEAR RESULT CODE                           
         TM    RUPSTAT,X'10'        WAS ACTUAL DEMOS REQUESTED                  
         BZ    BLDR280                                                          
* HOMES DEMOS                                                                   
         MVC   DRFHOMSH,NBACTSHR                                                
         MVC   DRFHOMHT,NBACTHUT                                                
         MVC   DRFHOMRT,NBACTHOM+2                                              
         MVC   DRFHOMIM,NBACTHOM+4                                              
*                                                                               
         MVC   DRFDEMS,NDACTDEM     MOVE OUT ACTUAL DEMOS                       
         DROP  RE,RF                                                            
* MOVE OUT THE RESULT CODE (ACTUAL DEMOS ONLY)                                  
         CLI   NBRESULT,0                                                       
         BE    *+10                                                             
         MVC   DRFRESLT(1),NBRESULT                                             
*--MOVE CABLE LEVEL INFO                                                        
         CLI   NBPOSTYP,C'C'                                                    
         BNE   BLDR280                                                          
         CLI   NBUSER2+5,C'Y'                                                   
         BE    BLDR280                                                          
         CLI   NBUSER2+5,C'N'                                                   
         BE    BLDR280                                                          
         CLI   NBUSER2+5,X'40'                                                  
         BNH   BLDR280                                                          
         MVC   DRFRESLT+1(1),NBUSER2+5                                          
*********************************                                               
BLDR280  LA    RE,STEWREC                                                       
         USING BUYUPLDD,RE                                                      
         TM    RUPSTAT2,X'07'       CHECK 2 SETS OF DEMOS                       
         BZ    BLDR400                                                          
         MVI   DRFDEMO2,C'Y'                                                    
         OI    NBINDS9,NBI9DOV     SET DEMO OVERRIDE INDICATORS                 
*                                                                               
         TM    RUPSTAT2,X'01'       CHECK ACTUALS                               
         BZ    BLDR282                                                          
         MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'N'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         NI    NBINDS9,X'FF'-NBI9GDM                                            
         B     BLDR300                                                          
*                                                                               
BLDR282  TM    RUPSTAT2,X'02'       ESTIMATED RAW                               
         BZ    BLDR284                                                          
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'Y'                                                    
         B     BLDR300                                                          
*                                                                               
BLDR284  TM    RUPSTAT2,X'04'       ESTIMATED GUARANTEES                        
         BZ    BLDR300                                                          
         MVI   NBACTOPT,C'N'                                                    
         MVI   NBESTOPT,C'Y'                                                    
         MVI   NBDEMRAW,C'N'                                                    
         OI    NBINDS9,NBI9GDM     REQUESTING GUARANTEES                        
         DROP  RE                                                               
*                                                                               
*  CALL NETVALUE GET SECOND SET OF DEMOS                                        
BLDR300  GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
*                                                                               
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         LA    RF,STEWREC                                                       
         USING BUYUPLDD,RF                                                      
*                                                                               
         TM    RUPSTAT2,X'01'       WAS ACTUAL DEMOS REQUESTED                  
         BO    BLDR320                                                          
* HOMES DEMOS ESTIMATED                                                         
         MVC   DRFHOMS2,NBESTSHR                                                
         MVC   DRFHOMH2,NBESTHUT                                                
         MVC   DRFHOMR2,NBESTHOM+2                                              
         MVC   DRFHOMI2,NBESTHOM+4                                              
         MVC   DRFDEMS2,NDESTDEM                                                
         B     BLDR400                                                          
* HOMES DEMOS ACTUAL                                                            
BLDR320  MVC   DRFHOMS2,NBACTSHR                                                
         MVC   DRFHOMH2,NBACTHUT                                                
         MVC   DRFHOMR2,NBACTHOM+2                                              
         MVC   DRFHOMI2,NBACTHOM+4                                              
         MVC   DRFDEMS2,NDACTDEM     MOVE OUT ACTUAL DEMOS                      
         DROP  RE,RF                                                            
*        ************                                                           
* MOVE OUT THE RESULT CODE (ACTUAL DEMOS ONLY)                                  
         CLI   NBRESULT,0                                                       
         BE    *+10                                                             
         MVC   DRFRESLT(1),NBRESULT                                             
*--MOVE CABLE LEVEL INFO                                                        
         CLI   NBPOSTYP,C'C'                                                    
         BNE   BLDR400                                                          
         CLI   NBUSER2+5,C'Y'                                                   
         BE    BLDR400                                                          
         CLI   NBUSER2+5,C'N'                                                   
         BE    BLDR400                                                          
         CLI   NBUSER2+5,X'40'                                                  
         BNH   BLDR400                                                          
         MVC   DRFRESLT+1(1),NBUSER2+5                                          
*                                                                               
* DO WSSVR CALL PASS RECORD TO NAVIGATOR                                        
BLDR400  XC    WORK,WORK+4                                                      
         LA    R2,WORK                                                          
         USING FAWSSVRD,R2                                                      
         MVC   FAWSTOKN,=CL4'NBUY'                                              
         MVI   FAWSACTN,FAWSUSVE                                                
         MVC   FAWSADR,AIOAREA4                                                 
         MVC   FAWSLEN,=H'1000'                                                 
         PRINT GEN                                                              
         L     RF,ACOMFACS                                                      
         L     RF,CWSSVR-COMFACSD(RF)                                           
         GOTOR (RF),FAWSSVRD                                                    
         PRINT NOGEN                                                            
         CLI   FAWSRTN,FAWSROK                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(4),=CL4'NBUY'    PASS TOKEN TO NAVIGATOR                    
         DROP  R2                                                               
*                                                                               
*                                                                               
*  WRITE TO TEMPSTORE                                                           
***BLDR400  XC    DMCB(24),DMCB                                                 
***         L     RE,ATWA                                                       
***         MVC   DMCB+10(2),2(RE)    TERMINAL NUMBER                           
***         MVI   DMCB+8,1            PAGE NUMBER                               
***         MVI   DMCB+9,0                                                      
***         MVC   DMCB+20(2),=C'L='                                             
***         MVC   DMCB+22(2),=X'03E8'  WRITE 1000 BYTES                         
***         MVC   WORK(4),DMCB+8       SAVE FOR GLOBBER CALL                    
***         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R4),0                   
***         CLI   8(R1),0             BLOW ON ANY ERROR HERE                    
***         BE    *+6                                                           
***         DC    H'0'                                                          
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,4,GLVBUY2                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
BLDREX   B     OVEXIT                                                           
         DROP  R6                                                               
         EJECT                                                                  
* BUILD THE DEMO OVERIIDES                                                      
* R6 POINTS TO DEMO INFORMATION  BYTES 1-3 = CATEGORY                           
*                                BYTES 4-7 = DEMO VALUE                         
STEWDEMO NTR1                                                                   
         LA    R3,STEWREC                                                       
         USING BUYUPLDD,R3                                                      
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
*                                                                               
STEWDM50 CLI   1(R6),USERMOD        CHECK USER DEMO                             
         BE    SETDM200                                                         
*                                                                               
         OC    0(3,R6),0(R6)        ANY MORE DEMOS?                             
         JZ    STEWDMEX                                                         
*                                                                               
         CLI   2(R6),0              COMSCORE DEMO?                              
         BNE   *+12                                                             
         BRAS  RE,SETCDEM           ADD COMSCORE OVERRIDE                       
         B     SETDM500                                                         
*                                                                               
         CLI   1(R6),0                                                          
         BE    STEWDMEX                                                         
*                                                                               
         MVC   ENCDEMO,0(R6)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
*  FOR OVERRIDES MAKE SURE IMPRESSIONS GO IN AS C'H'                            
         CLI   DECDEMO+1,C'T'                                                   
         BNE   *+8                                                              
         MVI   DECDEMO+1,C'H'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   0(3,R6),ENCDEMO                                                  
*  REMOVE CURRENT OVERRIDE VALUE                                                
         XC    FULL,FULL                                                        
         MVC   FULL+1(3),0(R6)                                                  
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(ELCODE,(R4)),(4,FULL)               
*  IF RATING, REMOVE ALL OVERRIDES                                              
         BRAS  RE,REMRTG           REMOVE RATING OVERRIDES                      
*  IF IMPRESSION MAKE SURE YOU CHECK FOR THE 'H' AND THE 'T' MODIFIER           
STEWDM60 CLI   DECDEMO+1,C'H'                                                   
         BNE   *+12                                                             
         MVI   DECDEMO+1,C'T'                                                   
         B     STEWDM75                                                         
         CLI   DECDEMO+1,C'T'                                                   
         BNE   STEWDM80                                                         
         MVI   DECDEMO+1,C'H'                                                   
STEWDM75 GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   FULL+1(3),ENCDEMO                                                
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(ELCODE,(R4)),(4,FULL)               
*                                                                               
STEWDM80 CLI   2(R6),X'01'         HOMES?                                       
         JNE   STEWDM82                                                         
         CLC   =X'0000FFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETDM500                                                         
STEWDM82 CLC   =X'00FFFFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETDM500                                                         
         CLC   =X'0FFFFFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETDM500                                                         
*                                                                               
         XC    WORK+20(20),WORK+20                                              
         LA    R5,WORK+20                                                       
*                                                                               
         USING NUOVD,R5                                                         
         MVC   NUOVEL,ELCODE                                                    
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),0(R6)                                                 
         MVC   NUOVVAL,3(R6)                                                    
* SET THE DEMO PRECISSION                                                       
         MVC   ENCDEMO,NUOVCAT                                                  
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         LA    RF,DEMPREC          CONVERSION TABLE                             
         LA    R1,7                                                             
*                                                                               
SETDM100 CLC   0(1,RF),DECDEMO+1                                                
         BE    SETDM120                                                         
         LA    RF,2(RF)                                                         
         BCT   R1,SETDM100                                                      
         DC    H'0'                                                             
SETDM120 MVC   NUOVPRE,1(RF)                                                    
                                                                                
* SET THE DEMO PREFIX                                                           
         LA    RF,NEBLOCKA         CONVERSION TABLE                             
         USING NEBLOCKD,RF                                                      
         MVI   BYTE,C'T'                                                        
         CLI   NBPOSTYP,C'N'                                                    
         BE    SETDM130                                                         
         CLI   NBPOSTYP,C'S'                                                    
         BE    SETDM130                                                         
         MVI   BYTE,C'H'                                                        
         DROP  RF                                                               
*                                                                               
SETDM130 CLI   DECDEMO+1,C'H'                                                   
         BE    *+12                                                             
         CLI   DECDEMO+1,C'T'                                                   
         BNE   SETDM140                                                         
         MVC   DECDEMO+1(1),BYTE                                                
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   NUOVMOD(1),ENCDEMO+1                                             
*  WRITE THE OVRIIDE TO THE UNIT RECORD                                         
SETDM140 GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
         DROP  R5                                                               
*                                                                               
* CREATE DEPENDENT OVERRIDES                                                    
*                                                                               
         CLI   DECDEMO+1,C'R'      TEST FOR RATING                              
         BNE   SETDM500                                                         
         BAS   RE,REVALUE          FILL DEMO BLOCK VIA NETVALUE                 
         BAS   RE,HOLDUNIV         GET CATEGORY UNIVERSE                        
         CLI   ELCODE,X'DE'                                                     
         BE    SETDM160                                                         
         BAS   RE,VPH              CALCULATE THE ESTIMATE VPH                   
         BRAS  RE,EIMP             CALCULATE THE ESTIMATE IMP                   
         B     SETDM500                                                         
*                                                                               
SETDM160 BRAS  RE,IMP              CALCULATE THE ACTUAL IMP                     
         B     SETDM500                                                         
*                                                                               
*   HANDLE USER DEMOS                                                           
*                                                                               
SETDM200 L     RE,ABUYVALS         RE POINTS TO BUY VALUES                      
         USING BUYVALD,RE                                                       
         ZIC   R1,2(R6)            EXTRACT USER DEMO NAME BY INDEXING           
         MH    R1,=H'7'            INTO NAME LIST WITH CATEGORY                 
         LA    RE,ESTUSNS-7(R1)                                                 
         MVC   DUB2+1(7),0(RE)                                                  
         MVC   DUB2(1),0(R6)                                                    
         MVC   DUB,DUB2                                                         
         DROP  RE                                                               
*                                                                               
*  FOR OVERRIDES MAKE SURE IMPRESSIONS GO IN AS C'H'                            
         CLI   DUB,C'T'                                                         
         BNE   *+8                                                              
         MVI   DUB,C'H'                                                         
*  REMOVE CURRENT OVERRIDE VALUE                                                
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'CD',(R4)),(8,DUB)                 
*  IF IMPRESSION MAKE SURE YOU CHECK FOR THE 'H' AND THE 'T' MODIFIER           
         CLI   DUB,C'H'                                                         
         BNE   *+12                                                             
         MVI   DUB,C'T'                                                         
         B     SETDM240                                                         
         CLI   DUB,C'T'                                                         
         BNE   SETDM260                                                         
         MVI   DUB,C'H'                                                         
SETDM240 GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'CD',(R4)),(8,DUB)                 
*                                                                               
*  DELETE PARTNER OVERRIDES                                                     
SETDM260 LA    RE,DEMDELTB                                                      
SETDM270 CLI   0(RE),X'FF'                                                      
         BE    SETDM300                                                         
         CLC   DUB(1),0(RE)                                                     
         BE    SETDM280                                                         
         LA    RE,2(RE)                                                         
         B     SETDM270                                                         
SETDM280 MVC   DUB(1),1(RE)                                                     
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(X'CD',(R4)),(8,DUB)                 
*                                                                               
SETDM300 CLI   2(R6),X'01'         HOMES?                                       
         JNE   *+14                                                             
         CLC   =X'0000FFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETDM500                                                         
         CLC   =X'00FFFFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETDM500                                                         
         CLC   =X'0FFFFFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETDM500                                                         
*                                                                               
         XC    WORK+20(20),WORK+20                                              
         LA    R5,WORK+20                                                       
*                                                                               
         USING UDOVD,R5                                                         
         MVI   UDOVEL,X'CD'                                                     
         MVI   UDOVLEN,14                                                       
         MVC   UDOVMOD(8),DUB2                                                  
         MVC   UDOVVAL,3(R6)                                                    
* SET THE DEMO PREFIX                                                           
         LA    RF,NEBLOCKA         CONVERSION TABLE                             
         USING NEBLOCKD,RF                                                      
         MVI   BYTE,C'T'                                                        
         CLI   NBPOSTYP,C'N'                                                    
         BE    SETDM330                                                         
         CLI   NBPOSTYP,C'S'                                                    
         BE    SETDM330                                                         
         MVI   BYTE,C'H'                                                        
         DROP  RF,R3                                                            
*                                                                               
SETDM330 CLI   UDOVMOD,C'H'                                                     
         BE    *+12                                                             
         CLI   UDOVMOD,C'T'                                                     
         BNE   SETDM350                                                         
         MVC   UDOVMOD,BYTE                                                     
*  WRITE THE OVRIIDE TO THE UNIT RECORD                                         
SETDM350 GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
*                                                                               
* CREATE DEPENDENT OVERRIDES                                                    
*                                                                               
         CLI   0(R6),C'R'          TEST FOR RATING                              
         BNE   SETDM400                                                         
         BAS   RE,REVALUE          FILL DEMO BLOCK VIA NETVALUE                 
         BAS   RE,HOLDUNIV         GET CATEGORY UNIVERSE                        
         BAS   RE,VPH              CALCULATE THE ESTIMATE VPH                   
         B     SETDM500                                                         
*                                                                               
SETDM400 CLI   0(R6),C'T'          TEST FOR IMP                                 
         BE    SETDM420                                                         
         CLI   0(R6),C'H'          TEST FOR IMP                                 
         BNE   SETDM500                                                         
SETDM420 BAS   RE,REVALUE          FILL DEMO BLOCK VIA NETVALUE                 
         BAS   RE,HOLDUNIV         GET CATEGORY UNIVERSE                        
         BAS   RE,ERTG             CALCULATE THE ESTIMATE VPH                   
         B     SETDM500                                                         
*                                                                               
SETDM500 LA    R6,7(R6)                                                         
         B     STEWDM50                                                         
*                                                                               
STEWDMEX B     OVEXIT                                                           
         DROP  R5                                                               
DEMDELTB DC    CL2'RV'                                                          
         DC    CL2'TR'                                                          
         DC    CL2'HR'                                                          
         DC    XL1'FF'                                                          
         EJECT                                                                  
*                                                                               
*--HOLDUNIV GETS THE CATEGORY UNIVERSE VALUE                                    
HOLDUNIV NTR1                                                                   
*                                                                               
         LA    R5,UNIVBLCK                                                      
         USING NETUNIVD,R5                                                      
* CHECK FOR HOMES                                                               
         LA    R4,NDUAHOME                                                      
         LA    R5,NDUEHOME                                                      
         CLI   2(R6),1             IS THIS HOMES                                
         BE    HLDUN60                                                          
*                                                                               
         LA    RF,20                                                            
         LA    RE,STEWDEMS                                                      
         LA    R4,NDUADEM                                                       
         LA    R5,NDUEDEM                                                       
*                                                                               
HLDUN50  CLC   2(1,R6),2(RE)                                                    
         BE    HLDUN60                                                          
         LA    RE,3(RE)                                                         
         LA    R5,4(R5)                                                         
         LA    R4,4(R4)                                                         
         BCT   RF,HLDUN50                                                       
         DC    H'0'                                                             
*                                                                               
HLDUN60  MVC   CATUNIVE,0(R5)      HOLD THE EST .UNIVERS VALUE                  
         MVC   CATUNIVA,0(R4)      HOLD THE ACT. UNIVERS VALUE                  
         B     OVEXIT                                                           
         DROP  R5                                                               
         SPACE 3                                                                
*                                                                               
* ROUTINE CALLS NETVALUE DOES A REVALUE CALL AND SETS                           
* THE DEMO INFORMATION INCLUDING UNIVERSES IN THE BLOCK                         
*                                                                               
REVALUE  NTR1                                                                   
*        LA    RE,NEBLOCKA                                                      
*        SR    RF,RF                                                            
*        LHI   RF,NEBLOCKL                                                      
*        XCEF                                                                   
*                                                                               
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
*                                                                               
         LA    R3,STEWREC                                                       
         USING BUYUPLDD,R3                                                      
*                                                                               
         L     R4,AIOAREA1                                                      
         ST    R4,NBAIO                                                         
         MVI   NBFUNCT,NBFVAL                                                   
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         ST    RE,NBADEM                                                        
         MVI   NBESTOPT,C'M'                                                    
*  FIND DEMO PRECISSION                                                         
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         CLI   NUPOSTYP,C'S'                                                    
         BE    REVAL40                                                          
         CLI   NUPOSTYP,C'H'                                                    
         BE    REVAL40                                                          
         CLI   NUPOSTYP,C'N'                                                    
         BE    REVAL40                                                          
         MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         MVI   NBPREOPT,C'Y'       REATINGS TO 2 DECIMAL PLACES                 
         DROP  RE                                                               
*                                                                               
REVAL40  L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         LA    RF,1000                                                          
         XCEF                                                                   
*                                                                               
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         LA    RF,UNIVBLCK                                                      
         ST    RF,NDAUBLOK                                                      
         MVC   NDDEMOS,STEWDEMS                                                 
         LA    R1,25                                                            
         LA    R5,NDDEMOS                                                       
         DROP  RE                                                               
*                                                                               
REVAL60  CLI   0(R5),X'FF'                                                      
         BE    REVAL70                                                          
         CLI   2(R5),0             COMSCORE DEMO?                               
         JE    REVAL65                                                          
         CLI   1(R5),USERMOD                                                    
         BE    REVAL65                                                          
         BRAS  RE,CONVIMP                                                       
REVAL65  LA    R5,3(R5)                                                         
         BCT   R1,REVAL60                                                       
REVAL70  MVI   0(R5),0                                                          
*                                                                               
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         L     RF,ABUYVALS         RE POINTS TO BUY VALUES                      
         USING BUYVALD,RF                                                       
         MVC   NDUSRNMS,ESTUSNS     USER DEMO NAMES                             
*  CHECK DEMOS NEEDED FOR RETURN                                                
         OI    NBINDS9,NBI9GDM      REQUESTING GUARANTEES                       
         TM    RUPSTAT,X'20'        CHECK FOR NO GUARANTEES                     
         BZ    *+12                                                             
         MVI   NBDEMRAW,C'Y'        RETURN RAW DEMOS                            
         NI    NBINDS9,X'FF'-NBI9GDM                                            
*  CHECK OVERRIDE EQUIVALENCING OPTIONS                                         
         MVC   PROFHOLD(1),NBUSER+1    IMP EQUIVALENCE PROFILE                  
         MVC   PROFHOLD+1(1),NBUSER2   RATING EQUIVALENCE PROFILE               
         TM    RUPSTAT,X'08'                                                    
         BZ    *+8                                                              
         MVI   NBUSER2,30                                                       
         TM    RUPSTAT,X'04'                                                    
         BZ    *+8                                                              
         MVI   NBUSER2,0                                                        
         TM    RUPSTAT,X'02'                                                    
         BZ    *+8                                                              
         MVI   NBUSER+1,30                                                      
         TM    RUPSTAT,X'01'                                                    
         BZ    *+8                                                              
         MVI   NBUSER+1,0                                                       
*                                                                               
         TM    RUPSTAT,X'10'        WAS ACTUAL DEMOS REQUESTED COL 1            
         BNZ   REVAL100                                                         
         TM    RUPSTAT2,X'01'       WAS ACTUAL DEMOS REQUESTED COL 2            
         BZ    *+16                                                             
REVAL100 MVI   NBACTOPT,C'Y'                                                    
         MVI   NBESTOPT,C'N'                                                    
         MVI   NBNOWRIT,C'N'                                                    
*                                                                               
         LA    RF,NBXBLK           NETBLKXTND BLOCK                             
         STCM  RF,15,NBEXTEND                                                   
         USING NBXTNDD,RF                                                       
         LA    RE,RUPCDEMS         COMSORE DEMO NAME LIST                       
         STCM  RE,15,NBXCDNL       A(COMSCORE DEMO NAME LIST)                   
         DROP  RF                                                               
*                                                                               
         GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         MVC   NBUSER+1(1),PROFHOLD    IMP EQUIVALENCE PROFILE                  
         MVC   NBUSER2(1),PROFHOLD+1   RATING EQUIVALENCE PROFILE               
         OC    NDUSRUNV,NDUSRUNV                                                
*                                                                               
REVALEX  B     OVEXIT                                                           
         DROP  R3,R6,RE                                                         
         SPACE 5                                                                
*&&DO                                                                           
*        CONVERT DEMO TO IMPRESSION                                             
*                                                                               
CONVIMP  NTR1                                                                   
         MVC   ENCDEMO,0(R5)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         MVI   DECDEMO+1,C'I'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   1(1,R5),ENCDEMO+1                                                
         B     OVEXIT                                                           
*&&                                                                             
* SUB-ROUTINE TO CALCULATE ESTIMATED VPH AND TO ADD AN OVERRIDE                 
* ELEMENT FOR IT (IMPRESSION BASED CALCULATION)                                 
* AT ENTRY     R4 CONTAINS ESTIMATED IMPRESSION VALUE                           
* AT ENTRY     3(R6) CONTAINS ESTIMATED IMPRESSION VALUE                        
*              R6 CONTAINS DEMO                                                 
*              BYTE CONTAINS OVERRIDE RATING PRECISION FACTOR                   
*                                                                               
IVPH     NTR1                                                                   
         LA    R4,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R4                                                      
         ICM   R3,15,3(R6)                                                      
         LTR   R3,R3               IMPRESSION                                   
         BZ    IVPHX                                                            
         SR    R2,R2                                                            
         CLC   NBESTHOM+4(4),=F'-1'                                             
         BE    IVPHX                                                            
         OC    NBESTHOM+4(4),NBESTHOM+4                                         
         BZ    IVPHX                                                            
         L     R1,NBESTHOM+4       HOME IMPRESSION                              
*                                                                               
         M     R2,=F'10000'        GET IMPRESSION TO PROPER PREC.               
         DR    R2,R1               IMPRESSION/HOME IMPRESSION                   
         A     R3,=F'5'                                                         
         SR    R2,R2                                                            
         D     R2,=F'10'                                                        
         LR    R2,R3                                                            
*                                                                               
         XC    WORK+20(20),WORK+20                                              
         LA    R3,WORK+20                                                       
         CLI   1(R6),USERMOD                                                    
         BE    IVPH2                                                            
*                                                                               
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   ENCDEMO,DUB2+1                                                   
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         MVI   DECDEMO+1,C'V'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   NUOVCAT(3),ENCDEMO  MODIFIER/CATEGORY                            
         STCM  R2,15,NUOVVAL                                                    
         BAS   RE,SETPRE           SET OVERRIDE PRECISION FACTOR                
         B     IVPH4                                                            
         SPACE                                                                  
         USING UDOVD,R3                                                         
IVPH2    LTR   R3,R3               TEST FOR ZERO VPH                            
         BZ    IVPHX               YES-EXIT                                     
         MVI   UDOVEL,X'CD'                                                     
         MVI   UDOVLEN,14                                                       
         MVI   UDOVMOD,C'V'                                                     
         MVC   UDOVNAM,DUB2+1      USER DEMO NAME                               
         STCM  R2,15,UDOVVAL                                                    
         SPACE 1                                                                
IVPH4    GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'DD',AIOAREA1),(5,WORK+22)          
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
         SPACE 1                                                                
IVPHX    B     OVEXIT                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
         EJECT                                                                  
* SUB-ROUTINE TO CALCULATE ESTIMATED VPH AND TO ADD OVERRIDE                    
* R6 BYTES=0-2 CATEGORY BYTES=3-6 RATING VALUE                                  
VPH      NTR1                                                                   
         CLI   1(R6),USERMOD       TEST FOR USER DEMO                           
         BNE   VPH10               NO                                           
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         ZIC   RF,2(R6)                                                         
         SLL   RF,2                INDEX TO UNIVERSE VALUE                      
         L     R0,NDUSRUNV-4(RF)                                                
         ST    R0,CATUNIVE         SLOT INTO POSITION IN UNIV VALUES            
         DROP  RE                                                               
*                                                                               
VPH10    LA    R4,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R4                                                      
         LA    R5,UNIVBLCK                                                      
         USING NETUNIVD,R5                                                      
*                                                                               
         CLI   1(R6),USERMOD       TEST FOR USER DEMO                           
         BE    *+12                YES                                          
         CLI   2(R6),1             HOMES RATING                                 
         BE    VPHX                                                             
         LH    R1,NBESTHOM+2       HOMES RATING                                 
         M     R0,NDUEHOME         X HOMES UNIVERSE                             
         TM    DEMPREC+1,X'82'                                                  
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
*                                                                               
         LA    RF,20                                                            
         L     R3,CATUNIVE         GET UNIVERSE VALUE                           
         L     R2,3(R6)            GET RATING VALUE                             
         MR    R2,R2               RATING X UNIVERSE                            
         M     R2,=F'10000'        SCALING                                      
         DR    R2,R1               DIVIDE BY HOMES IMP                          
         SR    R2,R2                                                            
         TM    DEMPREC+1,X'82'                                                  
         BO    VPH80                                                            
*                                                                               
         AH    R3,=H'500'          ROUND DOWN TO INTEGER PRECISION              
         D     R2,=F'1000'                                                      
         LR    R2,R3                                                            
         B     VPH90                                                            
*                                                                               
VPH80    A     R3,=F'5000'        ROUND DOWN TO INTEGER PRECISION               
         D     R2,=F'10000'                                                     
         LR    R2,R3                                                            
VPH90    XC    WORK+20(20),WORK+20                                              
         LA    R3,WORK+20                                                       
         CLI   1(R6),USERMOD       TEST FOR USER DEMO                           
         BE    VPH100              YES                                          
*                                                                               
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   ENCDEMO,0(R6)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         MVI   DECDEMO+1,C'V'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   NUOVCAT(3),ENCDEMO  MODIFIER/CATEGORY                            
         CLC   3(4,R6),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R2,15,NUOVVAL                                                    
         BAS   RE,SETPRE                                                        
         B     VPH120                                                           
*                                                                               
         USING UDOVD,R3                                                         
VPH100   MVI   UDOVEL,X'CD'                                                     
         MVI   UDOVLEN,14                                                       
         MVI   UDOVMOD,C'V'                                                     
         MVC   UDOVNAM,DUB2+1                                                   
         CLC   3(4,R6),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R2,15,UDOVVAL                                                    
*                                                                               
VPH120   GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'DD',AIOAREA1),(5,WORK+22)          
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
         SPACE                                                                  
VPHX     B     OVEXIT                                                           
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
*          DATA SET NEBUY28    AT LEVEL 004 AS OF 08/26/02                      
* SUB-ROUTINE TO CALCULATE THE ESTIMATE RTG AND TO ADD AN OVERRIDE              
*                                                                               
* AT ENTRY 3(R6) CONTAINS OVERRIDE IMPRESSION                                   
* AT ENTRY DUB2 CONTAINS OVERRIDE RATING PRECISION FACTOR                       
*                                                                               
ERTG     NTR1                                                                   
*                                                                               
* GET UNIVERSE VALUE                                                            
         L     RE,AIOAREA3                                                      
         A     RE,=F'2000'                                                      
         USING NETDEMOD,RE                                                      
         ZIC   RF,2(R6)                                                         
         SLL   RF,2                INDEX TO UNIVERSE VALUE                      
         L     R0,NDUSRUNV-4(RF)                                                
         ST    R0,CATUNIVE         SLOT INTO POSITION IN UNIV VALUES            
         DROP  RE                                                               
*                                                                               
         OC    CATUNIVE,CATUNIVE                                                
         BZ    ERTGX                                                            
         L     R3,CATUNIVE         PUT UNIVERSE INTO R3                         
         L     R1,3(R6)            PUT IMPRESSION IN R1                         
         SR    R0,R0                                                            
*                                                                               
         LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
ERTG10   CLI   0(RE),C'R'                                                       
         BE    ERTG15                                                           
         LA    RE,2(RE)                                                         
         BCT   RE,ERTG10                                                        
         DC    H'0'                                                             
ERTG15   MVC   BYTE,1(RE)          MOVE PRECISSION INTO BYTE                    
*                                                                               
         M     R0,=F'1000'         GET RATING TO PROPER PRECISSION              
         TM    BYTE,X'82'          CHECK PRECISION FOR HUNDREDS RATING          
         BNO   ERTG20              ADJUST THE ROUND                             
         M     R0,=F'10'           INCREASE PRECISON                            
*                                                                               
ERTG20   DR    R0,R3               IMPRESSION/UNIVERSE                          
*                                                                               
         AH    R1,=H'5'            ROUND THE IMP TO THOUSANDS                   
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
*                                                                               
ERTG30   XC    WORK+20(20),WORK+20                                              
         LA    R4,WORK+20                                                       
         USING UDOVD,R4                                                         
         MVI   UDOVEL,X'CD'                                                     
         MVI   UDOVLEN,14                                                       
         MVI   UDOVMOD,C'R'                                                     
         MVC   UDOVNAM,DUB2+1                                                   
         STCM  R1,15,UDOVVAL                                                    
******** GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'CD',AIOAREA1),(8,UDOVMOD)          
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
*                                                                               
ERTGX    B     OVEXIT                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ADD THE PRECISION FACTOR TO OVERRIDE ELEMENTS                  
* R3 POINTS TO THE ELEMENT AREA                                                 
*                                                                               
SETPRE   NTR1                                                                   
         LA    R4,DEMPREC          CONVERSION TABLE                             
         LA    RE,7                                                             
         MVC   ENCDEMO,3(R3)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
*                                                                               
SETP20   CLC   0(1,R4),DECDEMO+1                                                
         BE    SETP40                                                           
         LA    R4,2(R4)                                                         
         BCT   RE,SETP20                                                        
         DC    H'0'                                                             
SETP40   MVC   7(1,R3),1(R4)                                                    
         B     OVEXIT                                                           
         SPACE 2                                                                
         EJECT                                                                  
OVEXIT   XMOD1 1                                                                
*                                                                               
OVERROR  GOTO1 VERROR                                                           
         SPACE 2                                                                
OVUNFLE  DC    CL8'UNTFILE'                                                     
         LTORG                                                                  
         EJECT                                                                  
*        CONVERT DEMO TO IMPRESSION                                             
*                                                                               
CONVIMP  NTR1  BASE=*,LABEL=*                                                   
         MVC   ENCDEMO,0(R5)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         MVI   DECDEMO+1,C'I'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   1(1,R5),ENCDEMO+1                                                
         J     OVEXIT                                                           
*                                                                               
* CHECK HOMES RATING                                                            
*                                                                               
CHKHRTG  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         LA    R5,STEWREC                                                       
         USING BUYUPLDD,R5                                                      
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
*                                                                               
         LA    RF,UNTRATH                                                       
         ST    RF,FADDR                                                         
         CLI   RUPOHRTL,0                                                       
         BE    CHRTG20                                                          
         TM    NBINDS9,NBI9DOV     DEMO OVERRIDES?                              
         BO    CHRTG10                                                          
         CLC   RUPOHRT,NBESTHOM+2                                               
         JNE   STEWVERR                                                         
         B     CHRTG20                                                          
*                                                                               
CHRTG10  MVC   HALF,NBESTHOM+2                                                  
         TM    HALF,X'80'          OVERRIDE VALUE?                              
         JZ    *+8                                                              
         NI    HALF,X'FF'-X'80'                                                 
         CLC   RUPOHRT,HALF                                                     
         JNE   STEWVERR                                                         
CHRTG20  MVC   NBESTHOM+2(2),RUPNHRT                                            
         CLC   RUPNHRT,=X'FFFF'    REMOVE OVERRIDE?                             
         JNE   *+10                                                             
         XC    NBESTHOM+2(2),NBESTHOM+2                                         
*                                                                               
CHKHRTGS J     OVEXIT                                                           
         DROP  R4,R5,R6                                                         
         LTORG                                                                  
*                                                                               
* REMOVE RATING OVERRIDES                                                       
*                                                                               
REMRTG   NTR1  BASE=*,LABEL=*                                                   
         CLI   DECDEMO+1,C'R'                                                   
         BNE   REMRTGX                                                          
*                                                                               
         CLI   ELCODE,NUOVDEQ                                                   
         BE    REMRTG10                                                         
         MVI   DECDEMO+1,C'V'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   FULL+1(3),ENCDEMO                                                
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(ELCODE,(R4)),(4,FULL)               
         MVI   DECDEMO+1,C'H'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   FULL+1(3),ENCDEMO                                                
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(ELCODE,(R4)),(4,FULL)               
         B     REMRTGX                                                          
*                                                                               
REMRTG10 MVI   DECDEMO+1,C'T'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   FULL+1(3),ENCDEMO                                                
         GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(ELCODE,(R4)),(4,FULL)               
*                                                                               
REMRTGX  J     OVEXIT                                                           
         LTORG                                                                  
*                                                                               
* COMSCORE DEMO OVERRIDES                                                       
* ON ENTRY, R6 IS POINTING TO DEMO CATEGORY + VALUE                             
*                                                                               
SETCDEM  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,STEWREC                                                       
         USING BUYUPLDD,R3                                                      
*                                                                               
         ZIC   RF,1(R6)            COMSCORE DEMO INDEX #                        
         SHI   RF,1                                                             
         MH    RF,=H'8'                                                         
         LA    RE,RUPCDEMS         COMSCORE DEMO NAMES                          
         AR    RF,RE               GET COMSCORE DEMO NAME                       
         MVC   CDEMNAME,0(RF)                                                   
*                                                                               
         MVI   BYTE,NUCOV1LQ                                                    
         CLI   ELCODE,X'DD'        ESTIMATED OVERRIDES                          
         JE    *+8                                                              
         MVI   BYTE,NUCOV2LQ       ACTUAL OVERRIDES                             
*                                                                               
* DELETE EXISTING OVERRIDE ELEMENT                                              
*                                                                               
         XC    DUB(9),DUB                                                       
         MVC   DUB(8),CDEMNAME     CATEGORY                                     
         MVI   DUB+8,NUCOVMTQ      IMPRESSION OVERRIDE                          
*        CLI   CDEMNAME,C'I'                                                    
*        JE    SCDEM10                                                          
*        CLI   CDEMNAME,C'X'                                                    
*        JE    SCDEM10                                                          
*        MVI   DUB+8,NUCOVMRQ      RATING OVERRIDE                              
SCDEM10  GOTO1 VHELLO,DMCB2,(C'D',OVUNFLE),(BYTE,(R4)),(9,DUB)                  
*                                                                               
         CLI   2(R6),X'01'         HOMES?                                       
         JNE   SCDEM12                                                          
         CLC   =X'0000FFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETCDEMX                                                         
SCDEM12  CLC   =X'00FFFFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETCDEMX                                                         
         CLC   =X'0FFFFFFF',3(R6)  SENT FOR DELETION?                           
         JE    SETCDEMX                                                         
*                                                                               
         XC    WORK+20(NUCOVLNQ),WORK+20                                        
         LA    R5,WORK+20                                                       
         USING NUCOVD,R5                                                        
*                                                                               
         MVC   NUCOVEL,BYTE        ELEM CODE                                    
         MVI   NUCOVLEN,NUCOVLNQ   LENGTH                                       
*                                                                               
         MVC   NUCOVCAT,CDEMNAME   CATEGORY                                     
         MVI   NUCOVMOD,NUCOVMTQ   IMPRESSION OVERRIDE                          
         MVI   HALF,C'T'           IMPRESSIONS                                  
*        CLI   CDEMNAME,C'I'                                                    
*        JE    SCDEM20                                                          
*        CLI   CDEMNAME,C'X'                                                    
*        JE    SCDEM20                                                          
*        MVI   NUCOVMOD,NUCOVMRQ   RATING OVERRIDE                              
*        MVI   HALF,C'R'           RATINGS                                      
*                                                                               
SCDEM20  LA    RF,DEMPREC          CONVERSION TABLE                             
         LA    R1,7                                                             
*                                                                               
SCDEM22  CLC   0(1,RF),HALF        FOUND MATCH?                                 
         BE    SCDEM24                                                          
         AHI   RF,2                                                             
         BCT   R1,SCDEM22                                                       
         DC    H'0'                                                             
SCDEM24  MVC   NUCOVPRE,1(RF)      PRECISION                                    
         MVC   NUCOVVAL,3(R6)      VALUE                                        
         DROP  R5                                                               
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
*                                                                               
SETCDEMX J     OVEXIT                                                           
         LTORG                                                                  
         DROP  R3                                                               
* SUB-ROUTINE TO CALCULATE THE ESTIMATE IMP AND TO ADD AN OVERRIDE              
*                                                                               
* AT ENTRY R5 = UNIVERSE                                                        
*          R6   BYTES=0-2 CATEGORY BYTES=3-6 RATING VALUE                       
*                                                                               
EIMP     NTR1  BASE=*,LABEL=*                                                   
         LA    R4,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R4                                                      
*                                                                               
         CLI   1(R6),USERMOD       TEST USER DEMOS                              
         BE    EIMPX               YES                                          
         CLI   2(R6),1             HOMES RATING                                 
         BE    EIMPX                                                            
         L     R1,CATUNIVE         UNIVERSE                                     
         L     R0,3(R6)            PUT RATING IN R0                             
         MR    R0,R0               RATING*UNIVERSE                              
         TM    DEMPREC+1,X'82'                                                  
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
         XC    WORK+20(20),WORK+20                                              
         LA    R3,WORK+20                                                       
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         CLC   3(4,R6),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R1,15,NUOVVAL                                                    
         MVC   ENCDEMO,0(R6)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         MVI   DECDEMO+1,C'H'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   NUOVCAT(3),ENCDEMO  MODIFIER/CATEGORY                            
         BRAS  RE,SETPRE                                                        
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'DD',AIOAREA1),(5,WORK+22)          
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
*                                                                               
EIMPX    XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE THE ACTUAL IMP AND TO ADD AN OVERRIDE                
*                                                                               
* AT ENTRY CATUNIVA = UNIVERSE                                                  
*          R6   BYTES=0-2 CATEGORY BYTES=3-6 RATING VALUE                       
*                                                                               
IMP      NTR1  BASE=*,LABEL=*                                                   
         LA    R4,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R4                                                      
         ST    RE,SAVEREG                                                       
         CLI   1(R6),USERMOD       TEST USER DEMOS                              
         BE    IMPX                YES                                          
***      CLI   2(R6),1             HOMES RATING                                 
***      BE    IMPX                                                             
         MVC   ENCDEMO,THREE                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         L     R1,CATUNIVA                                                      
         L     R0,3(R6)            PUT RATING IN R0                             
         LTR   R1,R1                                                            
         BZ    IMPX                                                             
         MR    R0,R0               RATING*UNIVERSE                              
*                                                                               
         TM    DEMPREC+1,X'82'                                                  
         BO    IMP20                                                            
*                                                                               
         AH    R1,=H'50'           ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'100'                                                       
         B     IMP30                                                            
*                                                                               
IMP20    AH    R1,=H'500'          ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'1000'                                                      
IMP30    CLI   NBHUNOPT,C'Y'                                                    
         BNE   *+8                                                              
         MH    R1,=H'10'                                                        
*                                                                               
         XC    WORK+20(20),WORK+20                                              
         LA    R3,WORK+20                                                       
         USING NUOVD,R3                                                         
         MVI   NUOVEL,X'DE'                                                     
         MVI   NUOVLEN,12                                                       
         CLC   3(4,R6),=F'-2'      TEST FOR ZERO RATING                         
         BE    *+8                                                              
         STCM  R1,15,NUOVVAL                                                    
         MVC   ENCDEMO,0(R6)                                                    
         GOTO1 VENCDEC,DMCB,(2,ENCDEMO)                                         
         MVI   DECDEMO+1,C'T'                                                   
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+8                                                              
         MVI   DECDEMO+1,C'H'                                                   
         GOTO1 VENCDEC,DMCB,(1,DECDEMO)                                         
         MVC   NUOVCAT(3),ENCDEMO  MODIFIER/CATEGORY                            
         BRAS  RE,SETPRE                                                        
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'DE',AIOAREA1),(5,WORK+22)          
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK+20,0                    
*                                                                               
IMPX     XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE TO HANDLE WINDOW AIR DATE                                             
*                                                                               
STEWWIN  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         LA    R3,STEWREC                                                       
         USING BUYUPLDD,R3                                                      
*                                                                               
         CLI   RUPNWINL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOWINL,0          CHECK FOR ANY INPUT                          
         BE    STWWIN20                                                         
*                                                                               
STWWIN10 MVI   FERN,PRWINERR                                                    
         L     RE,APROGREC         MAKE SURE PROGRAM REC HAS WINDOW             
         LA    RE,24(RE)                                                        
STWWIN12 CLI   0(RE),0                                                          
         JE    ERROR                                                            
         CLI   0(RE),NPG3ELQ                                                    
         JE    *+16                                                             
         ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         J     STWWIN12                                                         
*                                                                               
         USING NPGEL03,RE                                                       
         TM    NPGSTATB,X'80'      WINDOW TURNED ON?                            
         JZ    ERROR                                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'60',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   STWWIN18            NO - NOT ON RECORD                           
         L     RE,12(R1)                                                        
*                                                                               
         USING NUOTH,RE                                                         
STWWIN14 CLI   NUOTEL,0            END OF RECORD?                               
         JE    STWWIN18            NO - NOT ON RECORD                           
         CLI   NUOTEL,X'60'        OTHER ELEMENT?                               
         JNE   STWWIN18                                                         
         CLI   NUOTTYP,C'W'        WINDOWS BOOK                                 
         JE    *+16                                                             
         ZIC   RF,NUOTLEN                                                       
         SR    RE,RF                                                            
         J     STWWIN14                                                         
*                                                                               
         OC    RUPNWIN,RUPNWIN     ANY WINDOW DATE?                             
         JNZ   *+12                                                             
         OI    NUOTEL,X'FF'        DELETE THE ELEMENT                           
         J     STWWIN20                                                         
         MVC   NUOTHER(2),RUPNWIN  UPDATE EXISTING ELEMENT                      
         J     STWWIN20                                                         
         DROP  RE                                                               
*                                                                               
STWWIN18 OC    RUPNWIN,RUPNWIN     ANY WINDOW DATE?                             
         JZ    STWWIN20                                                         
         MVI   WORK,X'60'          NOT ON RECORD, ADD NEW ONE                   
         MVI   WORK+1,5                                                         
         MVI   WORK+2,C'W'                                                      
         MVC   WORK+3(2),RUPNWIN                                                
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
STWWIN20 GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'02',(R4)),0                        
         CLI   12(R1),0            SET CC ON EXIT                               
         JE    *+6                                                              
         DC    H'0'                                                             
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         NI    NUSDST3,X'F7'       CLEAR WINDOW BYTE                            
         OC    RUPNWIN,RUPNWIN     ANY WINDOW VALUE?                            
         JZ    *+8                                                              
         OI    NUSDST3,X'08'       MARK WINDOW                                  
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'FF',AIOAREA1),0                    
*                                                                               
STEWWINX XIT1                                                                   
         DROP  R3,R4,RE                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE TO HANDLE AIR NETWORK                                                 
*                                                                               
STEWAIR  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         LA    R3,STEWREC                                                       
         USING BUYUPLDD,R3                                                      
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
*                                                                               
         CLI   RUPNAIRL,0                                                       
         BNE   *+12                                                             
         CLI   RUPOAIRL,0          CHECK FOR ANY INPUT                          
         BE    STEWAIRX                                                         
*                                                                               
         OC    NBAFFTIM,NBAFFTIM   AFFID SEEDED?                                
         JZ    *+12                                                             
         MVI   FERN,AFFIDLCK                                                    
         J     ERROR                                                            
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'60',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   STWAIR20            NO - NOT ON RECORD                           
         L     RE,12(R1)                                                        
*                                                                               
         USING NUOTH,RE                                                         
STWAIR10 CLI   NUOTEL,0            END OF RECORD?                               
         JE    STWAIR20            NO - NOT ON RECORD                           
         CLI   NUOTEL,X'60'        OTHER ELEMENT?                               
         JNE   STWAIR20                                                         
         CLI   NUOTTYP,C'V'        AIR                                          
         JE    *+16                                                             
         ZIC   RF,NUOTLEN                                                       
         AR    RE,RF                                                            
         J     STWAIR10                                                         
*                                                                               
         OC    RUPNAIR,RUPNAIR     ANY AIR NETWORK?                             
         JNZ   *+12                                                             
         OI    NUOTEL,X'FF'        DELETE THE ELEMENT                           
         J     STWAIR20            AND ADD NEW ELEMENT                          
         MVC   NUOTHER(L'RUPNAIR),RUPNAIR  UPDATE EXISTING ELEMENT              
         J     STEWAIRX                                                         
         DROP  RE                                                               
*                                                                               
STWAIR20 OC    RUPNAIR,RUPNAIR     ANY AIR DATE?                                
         JZ    STWAIR30                                                         
         MVI   WORK,X'60'          NOT ON RECORD, ADD NEW ONE                   
         MVI   WORK+1,13                                                        
         MVI   WORK+2,C'V'                                                      
         MVC   WORK+3(L'RUPNAIR),RUPNAIR                                        
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
STWAIR30 GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'FF',AIOAREA1),0                    
*                                                                               
STEWAIRX XIT1                                                                   
         DROP  R3,R4,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE TO HANDLE COMSCORE SERIES NUMBER                                      
*                                                                               
STEWCS#  NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         LA    R3,STEWREC                                                       
         USING BUYUPLDD,R3                                                      
*                                                                               
         CLI   RUPNCS#L,0                                                       
         BNE   *+12                                                             
         CLI   RUPOCS#L,0          CHECK FOR ANY INPUT                          
         BE    STEWCS#X                                                         
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'25',(R4)),0                        
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   STEWCS#2            NO - NOT ON RECORD                           
         L     RE,12(R1)                                                        
         USING NUCSELD,RE                                                       
*                                                                               
         MVC   NUCSSN,RUPNCS#      UPDATE EXISTING ELEMENT                      
         J     STEWCS#X                                                         
         DROP  RE                                                               
*                                                                               
STEWCS#2 XC    WORK,WORK                                                        
         MVI   WORK,NUCSELQ        NOT ON RECORD, ADD NEW ONE                   
         MVI   WORK+1,NUCSLENQ                                                  
         MVC   WORK+2(10),RUPNCS#                                               
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,WORK,0                       
*                                                                               
STEWCS#X XIT1                                                                   
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE HANDLES THE EDITING OF THE SPECIAL CHARGE ELEMENTS                    
*                                                                               
STEWSCHR NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         LA    R5,STEWREC                                                       
         USING BUYUPLDD,R5                                                      
         LA    R6,ELEMHLD                                                       
         USING NUSPRD,R6                                                        
         LA    R2,RUPSPCHR                                                      
         USING SPCHRTAB,R2                                                      
         LA    R3,5                                                             
         MVI   ADDSCCNT,X'70'      DEFAULT SEQ #'S FOR ADDS                     
*                                                                               
STEWS050 CLI   SPCHRTYP,C'M'       TEST FOR MIDAS                               
         BE    *+12                                                             
         CLI   SPCHRTYP,C'T'       TEST FOR MIDAS                               
         BNE   STEWS070                                                         
         L     RF,ABUYVALS         R5 POINTS TO BUY VALUES                      
         USING BUYVALD,RF                                                       
         TM    CLIOPT4,COP4MIDS     TEST IF MIDAS CLIENT                        
         BZ    *+12                 NO                                          
         TM    NETFLAG1,SMIDAS      TEST MIDAS STATION                          
         BNZ   STEWS070             YES                                         
         DROP  RF                                                               
         MVI   FERN,MIDASERR        NOT AUTHORIZED FOR MIDAS                    
         GOTO1 VERROR                                                           
*                                                                               
STEWS070 CLI   SPCHRACT,0          CHECK END OF TABLE                           
         BE    STEWS310            RESET SEQUENCE NUMBERS                       
         CLI   SPCHRACT,C'D'       IS THIA A DELETE                             
         BE    STEWS100                                                         
         CLI   SPCHRACT,C'A'       IS THIS A DELETE                             
         BE    STEWS200                                                         
*                                                                               
*  CHANGE ACTION                                                                
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'03',AIOAREA1),            X        
               (1,SPCHRSEQ)                                                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
*  MOVE ELEMENT INFO TO ELEMHLD                                                 
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMHLD(0),0(R6)     MOVE ELEMENT INFO TO ELEMHLD                
         LA    R6,ELEMHLD                                                       
*                                                                               
*  DELETE THE ELEMENT                                                           
*  IF CHANGE ACTION ELEMENT WILL BE ADDED FURTER DOWN                           
STEWS100 GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'03',AIOAREA1),            X        
               (1,SPCHRSEQ)                                                     
         CLI   SPCHRACT,C'D'       IS THIS A DELETE                             
         BE    STEWS300                                                         
         B     STEWS220             HANDLE CHANGE ACTION                        
*                                                                               
* UPDATE THE ELEMENT                                                            
STEWS200 LA    R6,ELEMHLD                                                       
         XC    ELEMHLD,ELEMHLD                                                  
         MVI   NUSPREL,X'03'                                                    
         MVC   NUSPRTYP,SPCHRTYP                                                
         MVC   NUSPRSEQ,ADDSCCNT    DUMMY SEQ NUMBER                            
         ZIC   RF,ADDSCCNT                                                      
         LA    RF,1(RF)                                                         
         STCM  RF,1,ADDSCCNT                                                    
STEWS220 MVI   NUSPRLEN,NUSPRLN4                                                
         MVC   NUSPRAMT,SPCHRAMT                                                
         MVC   NUSPRREP,SPCHRREP                                                
         MVI   NUSPRCOM,X'40'       INIT COMMISION TO BLANK                     
         CLI   SPCHRCOM,C'C'                                                    
         BNE   *+10                                                             
         MVC   NUSPRCOM,SPCHRCOM                                                
         MVC   NUSPRBPC,SPCHRBPR                                                
         MVC   NUSPRTPC,SPCHRTPR                                                
*                                                                               
* MOVE IN STATION MARKET NUMBER                                                 
*                                                                               
         OC    SPCHRSTA,SPCHRSTA                                                
         BZ    STEWS270                                                         
* SWITCH TO SPOT SYSTEM                                                         
*                                                                               
         GOTO1 ASWITCH,DMCB,=C'SPT',0                                           
         CLI   4(R1),0                                                          
         BE    STEWS240                                                         
*                                                                               
         MVI   FERN,NOTAUTH        USER NOT AUTHORIZED ERROR                    
         B     OVERROR                                                          
*                                                                               
STEWS240 DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'T'                                                       
         MVC   KEY+2(4),SPCHRSTA                                                
         MVI   KEY+6,C'T'                                                       
         OC    KEY+2(5),SPACES                                                  
         MVC   KEY+7(2),AGENCY     AGENCY                                       
         MVC   KEY+9(3),=C'000'    AGENCY SPECIFIC                              
*                                                                               
         MVI   FERN,CISTAERR                                                    
         GOTO1 AIO,DMCB,STA+FILE+HIGH,AIOAREA3                                  
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+8                                                              
         B     OVERROR                                                          
*                                                                               
         MVC   SVSTA,KEY+2         CUT-IN STATION                               
         MVI   SVSTA+4,C'T'        MUST BE TV                                   
         L     RF,AIOAREA3                                                      
         MVC   SVMKT,18(RF)        MARKET #                                     
*                                                                               
         GOTO1 VMSPACK,DMCB,SVMKT,SVSTA,DUB                                     
         CLI   0(R1),X'FF'                                                      
         BE    OVERROR                                                          
*                                                                               
STEWS260 MVC   NUSPRCIS,DUB                                                     
*                                                                               
* SWITCH BACK TO NET SYSTEM                                                     
*                                                                               
         GOTO1 ASWITCH,DMCB,=C'NET',0                                           
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
STEWS270 GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,ELEMHLD,0                    
*                                                                               
STEWS300 LA    R2,RUPSPCHL(R2)                                                  
         BCT   R3,STEWS050                                                      
*                                                                               
*        ALL RECORDS PROCESSED RESET SEQUENCE NUMBERS                           
STEWS310 LA    R2,1                                                             
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'03',AIOAREA1),0                    
         CLI   12(R1),0                                                         
         BNE   STEWSCEX                                                         
         L     R6,12(R1)                                                        
*                                                                               
STEWS320 MVI   NUSPRSEQ,0                                                       
         CLI   NUSPRTYP,C'F'        ASSIGNED CASH% NO SEQUENCE #                
         BE    STEWS330                                                         
         CLI   NUSPRTYP,C'C'        ACTUUAL CASH% NO SEQUENCE #                 
         BE    STEWS330                                                         
         STCM  R2,1,NUSPRSEQ                                                    
         LA    R2,1(R2)                                                         
STEWS330 ZIC   R3,NUSPRLEN                                                      
         AR    R6,R3                                                            
         CLI   0(R6),X'03'                                                      
         BNE   STEWSCEX                                                         
         B     STEWS320                                                         
*                                                                               
STEWSCEX XIT1                                                                   
         DROP  R2,R4,R5,R6                                                      
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* ROUTINE HANDLES COPYSPLITS                                                    
*                                                                               
STEWCSPT NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIOAREA1                                                      
         USING NURECD,R4                                                        
         LA    R5,STEWREC                                                       
         USING BUYUPLDD,R5                                                      
         LA    R6,ELEMHLD                                                       
         USING NUPDED,R6                                                        
         LA    R2,RUPCSPCT                                                      
         USING CPYSPPCT,R2                                                      
*                                                                               
         OC    CPYSPPRD,CPYSPPRD    ARE WE DOING COPYSPLIT MAINT                
         BZ    STWCS500                                                         
*                                                                               
* CHECK FOR UNALLOCATTING COPYSPLIT                                             
         CLC   CPYSPPRD,=CL3'999'                                               
         BNE   STWCS010                                                         
         BAS   RE,CSPRDREM                                                      
         J     STWCS500                                                         
*                                                                               
STWCS010 GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         OI    NUSDST3,X'40'        TURN ON COPYSPLIT BIT                       
         DROP  RE                                                               
*                                                                               
         XC    NUPRD,NUPRD                                                      
         XC    NUPRD2,NUPRD2                                                    
         XC    NUP1SHR,NUP1SHR                                                  
         XC    NUFEED,NUFEED                                                    
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'19',AIOAREA1),0                    
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'14',AIOAREA1),0                    
                                                                                
*                                                                               
STWCS020 XC    ELEMHLD,ELEMHLD                                                  
         MVI   NUPDEEL,X'19'                                                    
         OI    NUPDEIND,X'40'       SET COPYSPLIT INDICATOR                     
*                                                                               
         LA    R1,3                 R1=ELEMENT LENGTH                           
         LA    RF,MAXPRODS          MAXIMUM NUMBER OF PRODUCTS                  
         LA    R3,NUPDEPR                                                       
*                                                                               
STWCS030 OC    CPYSPPRD,CPYSPPRD    ARE WE DOING COPYSPLIT MAINT                
         BZ    STWCS040             NO                                          
         MVC   0(L'NUPDEPR,R3),CPYSPPRD     PRODUCT                             
         MVC   3(L'NUPDEPCT,R3),CPYSP1PT    PRODUCT PERCENT                     
         MVC   5(L'NUPDEFD,R3),CPYSPFPT     FEED PERCENT                        
         LA    R1,7(R1)                                                         
         LA    R2,7(R2)                                                         
         LA    R3,7(R3)                                                         
         BCT   RF,STWCS030                                                      
*                                                                               
STWCS040 STCM  R1,1,NUPDELEN        STORE ELEMENT LENGTH                        
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,ELEMHLD,0                    
         DROP  R2,R6                                                            
*                                                                               
* NOW HANDLE THE NATIONAL FEED                                                  
*                                                                               
         LA    R2,RUPCSFD                                                       
         USING CPYSFEED,R2                                                      
* START WITH NATIONAL ELEMENT                                                   
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NUCMLEL,R6                                                       
*                                                                               
* HANDLE THE NO NATIONAL LOGIC                                                  
*                                                                               
STWCS100 TM    RUPSTAT2,X'10'      CHECK IF NO NATIONAL SET                     
         BZ    STWCS150                                                         
         MVI   NUCMLPRD,0                                                       
         MVI   NUCMPPOS,0                                                       
         XC    NUCMPROD,NUCMPROD                                                
         CLI   ACTION,B            TEST ACTION BUY                              
         BE    STWCS120                                                         
         TM    NUCMLFL2,NUCMLFFD   WAS THERE A CHANGE IN THE STATUS             
         BO    STWCS300            GO HANDLE FEEDS                              
         OI    NUCMLFLG,NUCMLFPC   SET PRODUCT CHANGE                           
STWCS120 OI    NUCMLFL2,NUCMLFFD   NO NATIONAL FEED SET                         
         B     STWCS300                                                         
*                                                                               
* HANDLE REGULAR NATIONAL LOGIC                                                 
*                                                                               
STWCS150 LA    R1,MAXPRODS          MAX NUMBER OF PRODS                         
STWCS160 CLC   CPYFFEED,=CL4'*N  '                                              
         BE    STWCS180                                                         
         LA    R2,CPYSPFLN(R2)                                                  
         BCT   R1,STWCS160                                                      
         DC    H'0'                A NATIONAL PROD MUST BE THERE                
* CHECK FOR CHANGE IN ELEMENT                                                   
STWCS180 MVC   BYTE,CPYFPROD        CONVERT PRODUCT INFO                        
         BAS   RE,CPSPROD           GET 1 BYTE PROD CODE AND POSITION           
         OI    CPYFPROD,X'80'       MARK TABLE ENTRY                            
*                                                                               
         CLI   ACTION,B            TEST ACTION BUY                              
         BE    STWCS200                                                         
         CLC   NUCMPROD,FULL       HAS PRODUCT CHANGED                          
         BE    STWCS300            NO GO HANDLE FEEDS                           
*  UPDATE NATIONAL FEED ELEMENT                                                 
STWCS200 OI    NUCMLFLG,NUCMLFPC                                                
         MVC   NUCMPROD,FULL                                                    
         MVC   NUCMLPRD,FULL+3                                                  
         MVC   NUCMPPOS,FULL+4                                                  
*                                                                               
*  HANDLING OF THE FEED ELEMENTS                                                
*  DELETE AND CHANGE EXISTING FEEDS                                             
*                                                                               
                                                                                
*                                                                               
*  GET FIRST FEED ELEMENT                                                       
STWCS300 GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'23',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   STWCS400                                                         
         L     R6,12(R1)                                                        
         USING NUFDCEL,R6                                                       
*                                                                               
STWCS320 LA    R2,RUPCSFD                                                       
         LA    R3,MAXPRODS                                                      
STWCS340 CLC   NUFDCFED,CPYFFEED                                                
         BE    STWCS360                                                         
         LA    R2,CPYSPFLN(R2)                                                  
         BCT   R3,STWCS340                                                      
         MVI   NUFDCFED,X'FF'       MARK ELEMENT TO DELETE                      
         B     STWCS380                                                         
*                                                                               
* UPDATE EXISTING FEED WITH NEW PRODUCT                                         
STWCS360 MVC   BYTE,CPYFPROD                                                    
         BAS   RE,CPSPROD           GET 1 BYTE PROD CODE AND POSITION           
         CLC   NUFDPROD,FULL                                                    
         BE    STWCS380                                                         
         MVC   NUFDPROD,FULL        PRODUCT                                     
         MVC   NUFDCPRD,FULL+3      PRODUCT EQUATE                              
         MVC   NUFDPPOS,FULL+4      PRODUCT POSITION                            
         OI    NUFDCFL2,X'01'       ADDED BY MEDIA                              
*                                                                               
*  GET NEXT ELEMENT                                                             
STWCS380 OI    CPYFPROD,X'80'       MARK TABLE ENTRY                            
         ZIC   RF,NUFDCLEN                                                      
         AR    R6,RF                                                            
         CLI   0(R6),NUFDCELQ       MORE 23 ELEMENTS                            
         BE    STWCS320                                                         
* REMOVE THE FEEDS THAT ARE NOT IN FEED TABLE                                   
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'23',AIOAREA1),(1,=X'FF')           
*                                                                               
*  ADD NEW FEEDS                                                                
*                                                                               
STWCS400 LA    R2,RUPCSFD                                                       
         LA    R3,MAXPRODS                                                      
STWCS420 CLI   CPYFFEED,0           CHECK END OF TABLE                          
         BE    STWCS500                                                         
         TM    CPYFPROD,X'80'       HAS THIS BEEN PROCESSED ALREADY             
         BZ    STWCS460                                                         
STWCS440 LA    R2,CPYSPFLN(R2)                                                  
         BCT   R3,STWCS420                                                      
         B     STWCS500                                                         
*                                                                               
*                                                                               
* CREATE NEW FEEDS                                                              
STWCS460 MVC   BYTE,CPYFPROD                                                    
         BAS   RE,CPSPROD           GET 1 BYTE PROD CODE AND POSITION           
*                                                                               
         LA    R6,ELEMHLD                                                       
         XC    ELEMHLD,ELEMHLD                                                  
         MVI   NUFDCEID,NUFDCELQ    ELEMENT CODE (23)                           
         MVI   NUFDCLEN,NUFDCELN    ELEMENT LENGTH                              
         MVC   NUFDCFED,CPYFFEED    FEED                                        
         MVC   NUFDPROD,FULL        PRODUCT                                     
         MVC   NUFDCPRD,FULL+3      PRODUCT EQUATE                              
         MVC   NUFDPPOS,FULL+4      PRODUCT POSITION                            
         OI    NUFDCFL2,X'01'       ADDED BY MEDIA                              
         GOTO1 VHELLO,DMCB,(C'P',OVUNFLE),AIOAREA1,ELEMHLD,0                    
         OI    CPYFPROD,X'80'       SET THAT IT HAD BEEN PROCESSED              
         B     STWCS440             GET NEXT ENTRY                              
*                                                                               
*  RESET THE FLAGS USED IN THE ROUTINE                                          
STWCS500 LA    R3,MAXPRODS                                                      
         LA    R2,CPYFPROD                                                      
*                                                                               
STWCS520 NI    CPYFPROD,X'7F'                                                   
         LA    R2,CPYSPFLN(R2)                                                  
         BCT   R3,STWCS520                                                      
*                                                                               
STWCSEX  XIT1                                                                   
         DROP  R2,R4,R5,R6                                                      
         EJECT                                                                  
*                                                                               
* GET 1 BYTE PRODUCT CODE AND PRODUCT POSITION                                  
* INPUT BYTE CONTAINS 1 BYTE DISPLACEMENT                                       
* OUTPUT FULL   = 3 BYTE PRODUCT CODE                                           
*        FULL+3 = 1 BYTE PRODUCT EQUATE (0 MEANS OVERFLOW)                      
*        FULL+4 = PRODUCT POSITION                                              
*                                                                               
CPSPROD  NTR1                                                                   
         L     RF,ABUYVALS         RE POINTS TO BUY VALUES                      
         USING BUYVALD,RF                                                       
         LA    R5,STEWREC                                                       
         USING BUYUPLDD,R5                                                      
*                                                                               
         LA    R0,CPYSPPLN          LENGTH OF ENTRY                             
         ZIC   R1,BYTE              BYTE HAS DISPLACEMENT                       
         MR    R0,R0                                                            
         LA    R0,RUPCSPCT                                                      
         AR    R1,R0                                                            
         MVC   FULL(3),0(R1)                                                    
*                                                                               
         MVI   FULL+3,0                                                         
*                                                                               
         LA    R1,255                                                           
         LA    RE,CLILIST                                                       
*                                                                               
CPSPRD20 CLC   FULL(3),0(RE)                                                    
         BE    CPSPRD40                                                         
         LA    RE,4(RE)                                                         
         BCT   R1,CPSPRD20                                                      
         B     CPSPRD60                                                         
*                                                                               
CPSPRD40 MVC   FULL+3(1),3(RE)                                                  
         DROP  RF                                                               
*                                                                               
*  GET PRODUCT POSITION                                                         
*  POSITION IS BYTE +1, SINCE BYTE IS ATE ZERO OFFSET                           
CPSPRD60 ZIC   R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         STCM  R1,1,FULL+4          STORE PRODUCT POSITION NUMBER               
*                                                                               
CPSPRDEX XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* UNNALLOCATE COPYSPLIT UNIT                                                    
* REMOVE FEED ELEMENTS                                                          
* REMOVE PROD FROM 21 ELEMENET                                                  
* DELETE 14 AND 19 ELEMENTS                                                     
* RESET SWITCHES TO NOT BEING COPYSPLIT ON UNIT                                 
*                                                                               
CSPRDREM NTR1                                                                   
*                                                                               
* DELETE 19/14/23 ELEMENTS                                                      
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'19',AIOAREA1),0                    
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'14',AIOAREA1),0                    
         GOTO1 VHELLO,DMCB,(C'D',OVUNFLE),(X'23',AIOAREA1),0                    
*                                                                               
* UPDATE 21 ELEMENT                                                             
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'21',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING NUCMLEL,R6                                                       
*                                                                               
* REMOVE PRODUCT INFO FROM NATIONAL                                             
         OC    NUCMPROD,NUCMPROD   HAS PRODUCT CHANGED                          
         BZ    CSREM100            NO GO HANDLE FEEDS                           
*  UPDATE NATIONAL FEED ELEMENT                                                 
         OI    NUCMLFLG,NUCMLFPC                                                
         XC    NUCMPROD,NUCMPROD                                                
         XC    NUCMLPRD,NUCMLPRD                                                
         XC    NUCMPPOS,NUCMPPOS                                                
         DROP  R6                                                               
*                                                                               
CSREM100 GOTO1 VHELLO,DMCB,(C'G',OVUNFLE),(X'02',AIOAREA1),0                    
         CLI   12(R1),0            TEST IF FOUND                                
         BE    *+6                                                              
         DC    H'0'                TAKE A HIT IF NOT THERE-BAD RECORD           
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         NI    NUSDST3,X'BF'       TURN OFF COPY SPLIT STATUS                   
         DROP  RE                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* UNIT SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFCD                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVDATA   DS    CL256                                                            
         ORG   SVDATA                                                           
SVDATE   DS    XL2                 SAVED AIR DATE                               
SVSUB    DS    X                   SAVED SUB-LINE                               
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                     CANNOT EXCEED 2400 BYTES                     
STEWDEMS DS    CL75                STEWARD REQUEST DEMOS FROM BASE              
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
ASWITCH  DS    A                                                                
VDISPLAY DS    V                   V(GENERAL UNIT DISPLAY)                      
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
*                                                                               
OLDKEY   DS    CL(L'NUKEY)         OLD UNIT RECORD KEY                          
NEWKEY   DS    CL(L'NUKEY)         NEW UNIT RECORD KEY                          
DELKEY   DS    CL(L'NUKEY)         UNIT RECORD KEY JUST DELETED                 
*                                                                               
UNITDA   DS    XL4                 UNIT RECORD DISK ADDRESS                     
*                                                                               
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
CHARDATE DS    CL6                 DATE FROM ACTION FIELD - YYMMDD              
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
TIME     DS    X                   SQH (SAVED)                                  
NEWDATE  DS    XL2                 CHANGED DATE VALUE                           
PROFHOLD DS    XL2                 HOLD EQUIVALENCE PROFILES                    
*                                                                               
PNGUAHLD DS    F                   NEW PACKAGE GUARANTEE HOLD AREA              
DNGUAHLD DS    F                   NEW DEMO GUARENTEE HOLD AREA                 
CATUNIVE DS    F                   UNIVERSE VALUE OF DEMO CAT EST.              
CATUNIVA DS    F                   UNIVERSE VALUE OF DEMO CAT ACT.              
PGUARHLD DS    H                   PACKAGE GUARENTEE HOLD AREA                  
DGUARHLD DS    H                   DEMO GUARENTEE HOLD AREA                     
*                                                                               
MGCHGSW  DS    C                   MAKE-GOOD DETAIL CHANGE (Y/N)                
PAYLOKSW DS    C                   IF Y UNIT PAID CANNOT CHANGE                 
ESTLOOK  DS    C                   FORCE LOOKUP OF ESTIMATED DEMOS (Y)          
DDMMYYIP DS    X                   MAKE-GOOD YEAR INPUT                         
SVBYPR14 DS    X                   TELL EDIT TO SKIP MG OUT OF EST CK           
CAACTSW  DS    CL1                 ALLOW CHANGE TO PAYED UNIT                   
SCRUPSW  DS    CL1                 SCRIPT UPLOAD SWITCH                         
MAKEGDSW DS    CL1                 THIS IS OR WAS A MAKEGOOD                    
FRCSW    DS    CL1                 IF SINGLE MG USE IT'S OWN DEMO'S             
ASSFREEZ DS    CL1                 N=DONT FREEZE ASSIGNED COST                  
COMINPT  DS    CL1                 COMMENT INPUTTED SWITCH                      
SVINTTBL DS    CL70                SAVED INTEGRATION TABLES                     
SVMGFLDS DS    CL100               SAVED MAKEGOOD SCREEN                        
MAGDOVSW DS    CL1                 CHECK FOR OVERFLOW ON MAKEGOOD FIELD         
MGDETS   DS    0XL112                                                           
MGCODE   DS    CL6                 MAKE-GOOD PROGRAM CODE                       
MGDATE   DS    XL2                 MAKE-GOOD DATE                               
MGSUB    DS    X                   MAKE-GOOD SUB-LINE                           
MGPNAM   DS    CL16                MAKE-GOOD PROGRAM NAME                       
MGPSTAT  DS    X                   MAKE-GOOD STATUS                             
MGPSTAT2 DS    X                   MAKE-GOOD STATUS 2                           
MGPBILLB DS    X                   BILL-BOARD STATUS                            
         DS    CL84                                                             
         DS    XL1                 0 FOR NEW MAKEGOODS                          
OMGDETS  DS    0CL36               OLD TABLE OF UP TO 4 MAKEGOODS               
OMGCODE  DS    CL6                 OLD PROGRAM CODE                             
OMGDATE  DS    XL2                 OLD DATE                                     
OMGSUB   DS    XL1                 OLD NUMBER                                   
         DS    CL27                                                             
         DS    XL1                 0 FOR OLD MAKEGOODS                          
ELEMHLD  DS    XL100               ELEMENT HOLD AREA                            
*                                                                               
BKEYAREA DS    XL100               BILLING KEY AREA                             
SVSTA    DS    CL5                 SPOT STATION                                 
SVMKT    DS    CL4                 SPOT MARKET                                  
ADDSCCNT DS    XL1                 SEQUENCE # FOR ADDED SPECIAL CHARGES         
MYFLAG   DS    XL1                 FLAGS                                        
MGDD     EQU   X'01'               M/G PROGRAM REC HAS OVERRIDES FROM           
*                                  MISSED UNITS                                 
*                                                                               
CDEMNAME DS    CL8                 COMSCORE DEMO NAME                           
*                                                                               
         DS    0F                                                               
BLOCK    DS    CL256                                                            
UNIVBLCK DS    CL250                UNIVERSE BLOCK                              
STEWREC  DS    CL2000              STEWARD RECORD PASSED FROM BASE              
NBXBLK   DS    CL1600              NETBLKXTND BLOCK                             
TEMPLEN  EQU   *-STEWDEMS                                                       
         SPACE 2                                                                
*   EQUATES                                                                     
USERMOD  EQU   33                                                               
MAXPRODS EQU   6                                                                
         SPACE 2                                                                
NETDEMOD DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
* NETDEMOE (DSECT COVERING NETWORK DEMO BLOCK SUPPORTS 50 DEMOS)                
*                                                                               
NETDEMOE DSECT                                                                  
       ++INCLUDE NETDEMOP                                                       
*                                                                               
       ++INCLUDE NAVDSECTS                                                      
NETUNIVD DSECT                                                                  
       ++INCLUDE NETUNIVD                                                       
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE NETBLKXTND                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'223NEBUY45   12/08/20'                                      
         END                                                                    
