*          DATA SET NEBUY12    AT LEVEL 065 AS OF 02/25/19                      
*PHASE T31112B,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - UNIT BUY/DIS/CHA/DEL - T31112'             
T31112   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUYUN*,RA,RR=RE                                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31112+8192,RC                                                   
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
         USING BUYVALD,R5                                                       
         SPACE                                                                  
UNIT     TM    MODE,FIRST          FIRST TIME CLEAR SAVE AREA                   
         BZ    *+10                                                             
         XC    SVDATA,SVDATA                                                    
         OI    NBINDS,X'20'        DO NOT APPLY GUARANTEE                       
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
         BNE   UNIT6                                                            
*                                                                               
*                                                                               
         BAS   RE,CLIFRZEN         CHECK FOR FROZEN CLIENT                      
         BAS   RE,LOCKPACK         CHECK FOR LOCKED PACKAGE                     
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
         MVC   NUKNET,NET                                                       
         MVC   NUKPROG,PROG                                                     
         MVC   NUKEST,EST                                                       
         L     RE,APACKREC         EXTRACT PACKAGE DP                           
         MVC   NUKDP,NPAKDP-NPRECD(RE)                                          
         BAS   RE,BUILD            EDIT SCREEN/BUILD RECORD                     
         GOTO1 VBLDRQST                                                         
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
         BAS   RE,SUBPRT           UPDATE FIRST UNIT FOR DATE                   
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
         B     UNITX                                                            
         SPACE                                                                  
* ACTION CHANGE                                                                 
*                                                                               
UNIT6    CLI   ACTION,C            TEST FOR ACTION CHANGE                       
         BNE   UNIT15                                                           
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    UNIT20              YES                                          
*                                                                               
         BAS   RE,LOCKPACK                                                      
         BAS   RE,GETUNIT                                                       
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         L     R4,NBAIO                                                         
         MVC   OLDKEY,0(R4)        SAVE KEY BEFORE EDIT                         
         BAS   RE,BUILD                                                         
         GOTO1 VBLDRQST                                                         
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
         BAS   RE,SUBPRT           CHANGE FIRST UNIT'S SUBPRINT                 
*                                                                               
UNIT13   BAS   RE,CHGOMIS                                                       
         BAS   RE,CHGNMIS                                                       
         SPACE                                                                  
UNIT14   BAS   RE,SAVGUAR                                                       
         GOTO1 VNETVAL,DMCB,NEBLOCKD                                            
         BAS   RE,RESGUAR                                                       
         BAS   RE,DISUNIT          RE-DISPLAY CHANGED UNIT                      
*                                                                               
         GOTO1 VCHGBILL,DMCB,NEWKEY,OLDKEY,AIOAREA3,BKEYAREA                    
*                                                                               
         B     UNITX                                                            
         SPACE                                                                  
* ACTION DELETE                                                                 
*                                                                               
UNIT15   CLI   ACTION,DEL          TEST FOR ACTION DELETE                       
         BNE   UNIT20                                                           
*                                                                               
         MVI   FERN,AUDACTER       IF AUDIT IS SET, CANT DELETE                 
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
         LA    RE,TEMPD+1500                                                    
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
*  AFTER ESTIMATE YEAR IS ADDED TO THE ACTION DATE ANOTHER                      
*  DATVAL IS DONE TO MAKE SURE THE DATE IS VALID. THIS IS                       
*  TO PREVENT PROBLEMS WITH LEAP YEAR.                                          
ACTED5   XC    DUB2(12),DUB2       MUST CLEAR 4 BYTES AFTER DUB2                
*                                  B/C DATVAL DOESN'T WORK FOR 2019/20          
         GOTO1 VDATCON,(R1),(0,DUB),(5,DUB2)                                    
         GOTO1 VDATVAL,(R1),(0,DUB2),DUB                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    ERROR                                                            
         GOTO1 VDATCON,(R1),DUB,(2,DATE)                                        
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
         GOTO1 VCHKAIR,DMCB,NBAIO  CHECK FOR AIR STAION INPUT                   
         BAS   RE,SAVGUAR                                                       
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
         DROP  R4                                                               
         EJECT                                                                  
* ROUTINE TO EDIT SCREEN AND BUILD UNIT RECORD                                  
*                                                                               
BUILD    NTR1                                                                   
         NI    MYFLAG,X'FF'-MGDD                                                
         NI    MYFLAG,X'FF'-PRCHERRQ                                            
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
         BNE   BLD2                NO                                           
*                                                                               
         CLI   BUYPROF+12,YES      NO DAY CHANGE                                
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
         DROP  R3,RE                                                            
         SPACE 1                                                                
BLD2     LA    R4,BLOCK            INITIALIZE EDIT BLOCK AND CALL               
         USING UNBLOCKD,R4         EDIT FOR INITIALIZATION                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         LA    RE,TEMPD+1500                                                    
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
         BNE   BLD3                NO                                           
         MVC   UNODATE,NBACTDAT    SET ORIGINAL RECORD VALUES                   
         MVC   UNOSUB,NBACTSUB                                                  
         MVC   UNOTIME,NBACTSQH                                                 
         MVC   UNODAY,NBDAY                                                     
         SPACE                                                                  
BLD3     GOTO1 VEDIT,DMCB,(C'I',(R4))                                           
*                                                                               
*-CHECK PROFILE TO SEE IF MAKE-GOODS CAN BE CHANGED IF UNIT IS PAYED            
         MVI   PAYLOKSW,0                                                       
         CLI   BUYPROF2+13,C'A'                                                 
         BE    BLD3B                                                            
         CLI   BUYPROF2+13,C'B'                                                 
         BE    BLD3B                                                            
         CLI   BUYPROF2+13,C'C'                                                 
         BNE   BLD4                                                             
BLD3B    CLI   UNPAYSW,YES                                                      
         BNE   BLD4                                                             
         MVI   PAYLOKSW,C'A'                                                    
*                                                                               
         SPACE                                                                  
BLD4     BAS   RE,MGED             EDIT MAKE-GOOD FIELDS                        
         BAS   RE,PREEMPT          EDIT AND HANDLE PRE-EMPTIONS                 
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
         ST    R2,UNFLDH           FIELD HEADER POINTER                         
         ST    R2,FADDR                                                         
         MVC   UNEDATA,0(R3)       SET DATA TYPE                                
         GOTO1 (RF),(R1),(C'E',(R4))                                            
         CLI   UNERROR,0           TEST FOR ERROR                               
         BE    BLD5B                                                            
         CLI   UNERROR,CHGPRERR    COPY-SPLIT PRODUCT CHANGE ERR?               
         BNE   BLD5A                                                            
         OI    MYFLAG,PRCHERRQ                                                  
         MVI   UNERROR,0                                                        
         B     BLD5B                                                            
*                                                                               
BLD5A    MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
BLD5B    LA    R3,1(R3)                                                         
         SR    R0,R0               FIND NEXT UNPROTECTED FIELD                  
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST IF PROTECTED                            
         BO    *-10                YES                                          
         B     BLD5                                                             
         SPACE                                                                  
BLD6     MVC   BUYPROF+14(1),SVBYPR14                                           
         L     R1,AIOAREA1                                                      
         USING NURECD,R1                                                        
         MVC   DATE,UNDATE         SAVE KEY AIR DATE AND SQH                    
         MVC   TIME,UNSQH          VALUES FOR 'REACT' ROUTINE                   
         CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    *+14                                                             
         CLC   NUKDATE,NBACTDAT    TEST FOR DATE CHANGE                         
         BE    BLD7                NONE-SUB-LINE STAYS THE SAME                 
         BAS   RE,GETSUB           FIND NEW SUB-LINE                            
         MVC   NUKSUB,SUB          RETURN SUB-LINE                              
         SPACE                                                                  
BLD7     DS    0H                                                               
         GOTO1 VGETPROG,DMCB,DATE                                               
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
BLD11    CLI   ACTION,B            TEST FOR ACTION BUY                          
         BNE   BLD12                                                            
         LA    R3,BLOCK+L'UNBLOCK                                               
         USING NUCMLEL,R3                                                       
         XC    0(80,R3),0(R3)                                                   
         MVI   NUCMLEID,X'21'                                                   
         MVI   NUCMLELN,80                                                      
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,(R3),0                       
*                                                                               
*                                                                               
BLD12    BAS   RE,CHMG             DECIDE IF CHANGE IN MAKE-GOOD DETS.          
         BAS   RE,NEWMISS          ADD MISSED ELEMENTS                          
         SPACE                                                                  
BLD15    CLI   ACTION,B            TEST FOR ACTION BUY                          
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
BLD25    GOTO1 VEDIT,DMCB,(C'F',(R4))                                           
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
         CLI   UNTCOM1H+5,0                                                     
         BE    MGCOMERR                                                         
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
         CLI   BUYPROF2+13,C'A'    CAN CHANGE BE MADE TO PAID                   
         BE    RD165                                                            
         CLI   BUYPROF2+13,C'B'    CAN CHANGE BE MADE TO PAID                   
         BE    RD165                                                            
         CLI   BUYPROF2+13,C'C'    CAN CHANGE BE MADE TO PAID                   
         BNE   RD180                                                            
RD165    MVI   PAYLOKSW,C'A'       MAKEGOOD CHANGE NOT ALLOWED                  
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
         CLI   UNTMGFH+5,0         HAS TO BE A MAKEGOOD                         
         BE    ERROR                                                            
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
*  UPDATE ACTIVITY ELEMENT                                                      
         GOTO1 VDATCON,DMCB2,(2,TODAYC),(3,THREE)                               
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
         CLI   BUYPROF2+13,C'A'                                                 
         BE    PR060                                                            
         CLI   BUYPROF2+13,C'B'                                                 
         BE    PR060                                                            
         CLI   BUYPROF2+13,C'C'                                                 
         BNE   PREX                                                             
PR060    CLI   UNPAYSW,YES                                                      
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
         GOTO1 VCALLOV,DMCB,(X'30',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDISPLAY,DMCB       GENRAL DISPLAY MODULE                        
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
         TM    MYFLAG,PRCHERRQ     PRODUCT CHANGE ERROR?                        
         BZ    MSG1                                                             
         MVC   BUYMSG(39),=C'UNIT PRODUCT NOT CHANGED FOR COPY-SPLIT'           
         LA    R2,BUYACTH                                                       
         B     MSGX                                                             
*                                                                               
MSG1     MVC   BUYMSG(4),=C'UNIT'                                               
         MVC   BUYMSG+5(9),1(RE)                                                
         LA    R2,BUYACTH          SET CURSOR POSITION                          
         TM    MODE,DISPLAY        TEST FOR CHANCE OF FORCED DISPLAY            
         BZ    MSGX                NO                                           
         SPACE                                                                  
MSG2     CLI   ACTION,B            TEST FOR ACTION BUY                          
         BE    MSGX                ALL DONE                                     
         CLI   ACTION,D            TEST FOR DISPLAY                             
         BE    MSGX                YES                                          
         MVC   BUYMSG+5(9),=C'DISPLAYED'                                        
         LA    R3,BUYMSG+15                                                     
         LA    R2,UNTMGFH                                                       
         MVC   0(15,R3),=C'- ENTER CHANGES'                                     
         CLI   ACTION,C                                                         
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
         CLI   DECDEMO+3,C'T'      ONLY PICK UP IMPRESSION                      
         BE    *+12                                                             
         CLI   DECDEMO+3,C'H'      AND HISPANIC OVERRIDES                       
         BNE   MGO100                                                           
******   CLI   NUOVMOD,C'T'        ONLY PICK UP IMPRESSION                      
******   BE    *+12                                                             
******   CLI   NUOVMOD,C'H'        AND HISPANIC OVERRIDES                       
******   BNE   MGO100                                                           
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
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
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
*                                                                               
PNGUAHLD DS    F                   NEW PACKAGE GUARANTEE HOLD AREA              
DNGUAHLD DS    F                   NEW DEMO GUARENTEE HOLD AREA                 
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
*                                                                               
MYFLAG   DS    XL1                 FLAGS                                        
MGDD     EQU   X'01'               M/G PROGRAM REC HAS OVERRIDES FROM           
*                                  MISSED UNITS                                 
PRCHERRQ EQU   X'02'               PRODUCT CHANGE ERROR                         
*                                                                               
OLDUKEY  DS    XL20                OLD UNIT KEY                                 
NEWUKEY  DS    XL20                NEW UNIT KEY                                 
BKEYAREA DS    XL100               BILLING KEY AREA                             
*                                                                               
         DS    0F                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'065NEBUY12   02/25/19'                                      
         END                                                                    
