*          DATA SET NEBUY13S   AT LEVEL 200 AS OF 05/01/02                      
*PHASE T31113A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - DEMO DIS/CHA OVERLAY - T31113'             
T31113   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**DEMO**,RA,RR=RE                                              
         L     R9,0(R1)                                                         
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL STORAGE                   
         USING TEMPD,R7                                                         
         ST    R1,MYPARM                                                        
         ST    RE,MYRELO                                                        
         LA    R6,NEBLOCKA                                                      
         USING NEBLOCKD,R6                                                      
         L     R5,ABUYVALS                                                      
         USING BUYVALD,R5                                                       
         SPACE                                                                  
DEM      TM    MODE,FIRST          FIRST TIME CLEAR SAVE AREA                   
         BZ    *+10                                                             
         XC    SVAREA,SVAREA                                                    
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
DEM2     BAS   RE,GETUNIT          GET THE UNIT RECORD                          
         BAS   RE,SAVGUAR          SAVE THE GUARANTEE FACTOR                    
         GOTO1 VGETPROG,DMCB,NBACTDAT                                           
         GOTO1 VDISPROG                                                         
         GOTO1 VGETNADS                                                         
         BAS   RE,DEMLIST                                                       
         CLI   ACTION,DD           TEST FOR ACTION DISPLAY                      
         BE    DEM4                YES                                          
         TM    MODE,DISPLAY        TEST FOR FORCED DISPLAY                      
         BO    DEM4                                                             
         SPACE                                                                  
* ACTION CHANGE                                                                 
*                                                                               
DEM3     OC    DEMS,DEMS                                                        
         BZ    DEMX                                                             
         BAS   RE,LOCKPACK                                                      
         BAS   RE,LOOKDEM          LOOK UP RECORD VALUES                        
         GOTO1 =A(OVFLRTN),DMCB,(0,DUB),(RC),VALTAB,RR=MYRELO GETVAL            
         BAS   RE,GETHOM           EDIT ACTUAL HOMES RATING                     
         BAS   RE,EDINIT           EDIT INITIALIZATION                          
         BAS   RE,HOMOVER          HOMES RATING OVERRIDES                       
         BAS   RE,OVER             GENERATE ANY OVERRIDE ELEMENTS               
         BAS   RE,EDHEAD           EDIT HEADLINE FIELDS                         
         BAS   RE,ADJED            EDIT DEMO PERCENTAGE ADJUSTEMENTS            
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
         TM    NBUNITST,X'01'      TEST FOR MAKE-GOOD                           
         BZ    DEM3A               NO                                           
*                                                                               
         CLI   BUYPROF+1,C'Y'      TEST IF M-G HAS ITS OWN ESTIMATES            
         BE    DEM3A               YES                                          
         MVI   UNNOLOOK,YES        NO-SUPPRESS ESTIMATED LOOKUP                 
         MVI   ESTLOOK,NO                                                       
*                                                                               
DEM3A    MVC   UNESTLK,ESTLOOK     ESTIMATED LOOKUP OPTION                      
         GOTO1 VEDIT,DMCB,(C'D',BLOCK)                                          
*--SINCE WE ARE NOW SHOWING DEMOS IN THERE RAW STATE                            
*--DEMOS CAN NOW BE CHANGED EVEN IF PACKAGE GUARANTEES EXIST                    
         CLI   UNERROR,PGNOSHR                                                  
         BNE   DEM3C                                                            
         MVC   FERN,UNERROR                                                     
         LA    RE,DEMSHRH                                                       
         CLI   UNSHR,X'FD'                                                      
         BE    DEM3B                                                            
         LA    RE,DEMHUTH                                                       
         CLI   UNHUT,X'FD'                                                      
         BE    DEM3B                                                            
         LA    RE,DEMRATH                                                       
DEM3B    ST    RE,FADDR                                                         
         B     ERROR                                                            
*                                                                               
DEM3C    BAS   RE,LOOKDEM          LOOK UP DEMO VALUES AGAIN                    
         BAS   RE,DISHEAD          RE-DISPLAY HEADLINE FIELDS                   
         BAS   RE,DISRES           RE-DISPLAY RESULT CODE                       
         BAS   RE,HOMESD           RE-DISPLAY HOMES                             
         BAS   RE,DISRAT           DISPLAY ACTUAL HOMES RATING                  
         BAS   RE,DISVAL           RE-DISPLAY OTHER DEMO VALUES                 
         BAS   RE,RESGUAR          RESTORE GUARANTEE FACTOR                     
         GOTO1 VEDIT,DMCB,(C'F',BLOCK)                                          
         DROP  R4                                                               
*                                                                               
         L     R4,AIOAREA1         POINT TO RECORD                              
         USING NURECD,R4                                                        
         MVC   KEY(L'NUKEY),0(R4)  USE RECORD KEY                               
         GOTO1 AIO,DMCB,UNT+DIR+READ  RE-READ RECORD AND REPLACE                
         GOTO1 (RF),(R1),UPDATE+UNT+FILE+GET,AIOAREA4                           
         GOTO1 (RF),(R1),UNT+FILE+PUT,(R4)                                      
         GOTO1 VBLDRQST             GENERATE TURN-AROUND REQUEST                
         B     DEMX                                                             
         DROP  R4                                                               
         SPACE 2                                                                
* ACTION DISPLAY                                                                
*                                                                               
DEM4     BAS   RE,DISHEAD          DISPLAY HEADLINE FIELDS                      
         GOTO1 VCLEARF,DMCB,DEMEVWH,DEMLAST                                     
         GOTO1 (RF),(R1),(1,DEMEVWH),DEMLAST                                    
         OC    DEMS,DEMS                                                        
         BZ    DEMX                                                             
         BAS   RE,DCAT                                                          
         BAS   RE,LOOKDEM          LOOK UP DEMOS FOR SCREEN                     
         BAS   RE,DISRES           DISPLAY RESULT FIELD                         
         BAS   RE,HOMESD           DISPLAY HOMES LINE                           
         BAS   RE,DISRAT           DISPLAY ACTUAL HOMES RATING                  
         BAS   RE,DISVAL           DISPLAY REST OF DEMO VALUES                  
         BAS   RE,RESGUAR          RESTORE GUARANTEE FACTOR                     
         GOTO1 =A(OVFLRTN),DMCB,(1,DUB),(RC),RR=MYRELO DISGUAR                  
         SPACE 2                                                                
* SET MESSAGE AND EXIT MODULE                                                   
*                                                                               
DEMX     BAS   RE,MSG                                                           
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
         CLI   FSTOP,COMMA         TEST IF COMMA FOUND                          
         BNE   ACTM                NO                                           
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
*                                                                               
         MVI   FERN,INVERR                                                      
         CLC   FLD(3),=C'RS='      CHECK REASON CODE                            
         BNE   ACTED10                                                          
         GOTO1 VSCANNER,DMCB,FLDH,(1,WORK),0                                    
         CLI   4(R1),0                                                          
         BE    ERROR                                                            
         CLI   WORK+1,4                                                         
         BH    ERROR                                                            
         MVC   AUDREASN,WORK+22                                                 
         GOTO1 VCKREASN,DMCB,AUDREASN                                           
         B     ACTED8                                                           
*                                                                               
ACTED10  CLC   FLD(4),=C'EST='     EDIT FOR RE-LOOK UP OF EST DEMOS             
         BNE   ERROR                                                            
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
         BO    ERROR               YES-CANNOT CHANGE ESTIMATED DEMOS            
         MVI   ESTLOOK,YES                                                      
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
         MVI   NBUSER+13,NO        **FORCE PRE-EMPTS TO RE RETURNED             
         MVI   NBSELMOD,NBPROCUN                                                
         MVI   NBESTOPT,C'M'       LOOK UP HOMES VALUES                         
         OI    NBSPLOPT,X'10'      DONT BREAK OUT COPY SPLIT NUMBERS            
         MVC   NBAIO,AIOAREA1                                                   
         MVI   FERN,NOTFOUND                                                    
         XC    NBHUNOPT,NBHUNOPT                                                
         CLI   NBPOSTYP,C'S'                                                    
         BE    GETUNIT2                                                         
         CLI   NBPOSTYP,C'H'                                                    
         BE    GETUNIT2                                                         
         CLI   NBPOSTYP,C'N'                                                    
         BE    GETUNIT2                                                         
GETUNIT1 MVI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         SPACE                                                                  
GETUNIT2 GOTO1 VNETIO,DMCB,NEBLOCKD                                             
         CLI   NBERROR,NBGOOD                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NBMODE,NBPROCUN     TEST FOR UNIT RETURNED                       
         BE    GETUNITX            YES                                          
         CLI   NBMODE,NBREQLST                                                  
         BE    ERROR                                                            
         B     GETUNIT2                                                         
         SPACE                                                                  
GETUNITX L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO SET DEMO LIST FOR SCREEN                                       
*                                                                               
*********************************************************************           
* FOR FIRST TIME OR FORCED DISPLAY, ESTABLISH THE INTERNAL ESTIMATE *           
* HEADER DEMO LIST EXCLUDING HOMES SINCE IT IS ALWAYS DISPLAYED ON  *           
* EACH SCREEN.                                                      *           
*********************************************************************           
DEMLIST  NTR1                                                                   
         TM    MODE,FIRST+DISPLAY  TEST FOR FIRST TIME OR DISPLAY               
         BZ    DEML4               NEITHER                                      
*                                                                               
         MVI   SVLPAGE,0           CLEAR SAVED VALUES                           
         MVI   SVLACT,0                                                         
         MVI   SVNDEMS,0                                                        
         XC    SVDEMOS,SVDEMOS                                                  
         SR    R0,R0               COUNTER                                      
         ZIC   R1,ESTNDEMS         NUMBER OF ESTIMATE HEADER DEMOS              
         LA    R2,ESTDEMS                                                       
         LA    RE,SVDEMOS          RE POINTS TO OUTPUT                          
*                                                                               
DEML1    CLI   1(R2),USERMOD       TEST FOR USER DEMO                           
         BE    *+20                YES-CANNOT BE HOMES                          
         CLI   0(R2),1             CHECK FOR NAD                                
         BH    *+12                IF NAD BYPASS HOMES CHECK                    
         CLI   2(R2),1             TEST FOR HOMES                               
         BE    DEML2               REMOVE FROM LIST SINCE ALWAYS ON SCR         
         MVC   0(3,RE),0(R2)                                                    
         BCTR  R0,0                INCREMENT LIST COUNT                         
         LA    RE,3(RE)            POINT TO NEXT OUTPUT AREA                    
*                                                                               
DEML2    LA    R2,3(R2)            NEXT INPUT ENTRY                             
         BCT   R1,DEML1                                                         
         LPR   R0,R0                                                            
         STC   R0,SVNDEMS                                                       
         SPACE                                                                  
DEML4    CLI   ACTION,CD           TEST FOR CHANGE                              
         BNE   DEML6                                                            
*                                                                               
         SR    R1,R1               BACK UP TO START OF LAST PAGE                
         ICM   R1,1,SVLPAGE                                                     
         BZ    *+6                                                              
         BCTR  R1,0                                                             
         LA    R0,LINES                                                         
         MR    R0,R0               MAX DEMOS BEFORE LAST PAGE                   
*                                                                               
         ZIC   RE,SVNDEMS          DEMOS IN ESTIMATE LIST                       
         SR    RE,R1               NUMBER OF DEMOS REMAINING                    
         LA    RF,LINES                                                         
         CR    RE,RF               NO MORE DEMOS THAN LINES ON SCREEN           
         BNH   *+6                                                              
         LR    RE,RF                                                            
         STC   RE,DEMS             DEMOS ON SCREEN                              
         MH    RE,=H'3'            LENGTH OF DEMO LIST FOR SCREEN               
         BCTR  RE,0                EXECUTE LENGTH                               
         MH    R1,=H'3'            DISPLACEMENT TO START OF LIST                
         LA    R1,SVDEMOS(R1)                                                   
         EX    RE,*+8                                                           
         B     DEMLX                                                            
         MVC   DEMOS(0),0(R1)      SET DEMO LIST FOR SCREEN                     
         SPACE                                                                  
DEML6    CLI   ACTION,DD           TEST FOR ACTION DISPLAY                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R1,SVLPAGE          FORCE TO NEW PAGE                            
         LA    R1,1(R1)                                                         
         STC   R1,SVLPAGE                                                       
         BCTR  R1,0                                                             
         LA    R0,LINES                                                         
         MR    R0,R0               MAXIMUM DEMOS ALREADY DONE                   
*                                                                               
         ZIC   RE,SVNDEMS                                                       
         CR    R1,RE                                                            
         BNL   DEML8               NO DEMOS TO DISPLAY                          
         SR    RE,R1               DEMOS REMAINING                              
         LA    RF,LINES                                                         
         CR    RE,RF                                                            
         BNH   *+10                                                             
         LR    RE,RF               NO MORE DEMOS THAN SCREEN LIMIT              
         MVI   MORESW,YES          ANOTHER PAGE TO COME                         
         STC   RE,DEMS                                                          
         MH    RE,=H'3'                                                         
         BCTR  RE,0                LENGTH TO MOVE                               
         MH    R1,=H'3'                                                         
         LA    R1,SVDEMOS(R1)      SOURCE FOR MOVE                              
         EX    RE,*+8                                                           
         B     DEMLX                                                            
         MVC   DEMOS(0),0(R1)                                                   
         SPACE                                                                  
DEML8    XC    SVAREA,SVAREA       CLEAR SAVE DATA TO FORCE DIS NEXT            
         MVC   BUYMSG(L'NOMORE),NOMORE                                          
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         OI    DEMEVWH+6,X'81'     MODIFY TO ALLOW RE-ENTRY AND XMIT            
         MVI   MODE,0              DIRECTLY OUT TO USER                         
         GOTO1 VEXIT                                                            
         SPACE                                                                  
DEMLX    B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY HEADLINE DATA                                          
*                                                                               
DISHEAD  NTR1                                                                   
         GOTO1 VCALLOV,DMCB,(X'30',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDISPLAY,DMCB       GENERAL DISPLAY                              
         L     RF,VDISPLAY                                                      
         LA    R1,DMCB                                                          
         LA    R3,FLDTAB                                                        
         LA    R2,DEMSHRH                                                       
         SPACE                                                                  
DIS2     CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    DISX                YES-EXIT                                     
         ZIC   R0,0(R3)                                                         
         GOTO1 (RF),(R1),((R0),(R9))                                            
         ZIC   RE,0(R2)                                                         
         SH    RE,=H'9'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),FLD                                                      
         OI    6(R2),X'80'                                                      
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *-10                YES                                          
         LA    R3,1(R3)                                                         
         B     DIS2                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO DISPLAY RESULT FIELD                                           
*                                                                               
DISRES   NTR1                                                                   
         LA    R2,DEMRESH                                                       
         XC    DEMRES,DEMRES                                                    
         OI    6(R2),X'80'                                                      
         LA    R2,8(R2)                                                         
         MVI   BYTE,URES                                                        
         GOTO1 VDISPLAY,DMCB,(BYTE,(R9))                                        
         CLI   FLDH+7,0            TEST FOR OUTPUT                              
         BE    DISRES8             NONE                                         
         MVC   0(7,R2),=C'ACTUAL='                                              
         MVC   7(1,R2),FLD                                                      
         LA    R2,8(R2)                                                         
*--MOVE CABLE LEVEL INFO                                                        
         CLI   NBPOSTYP,C'C'                                                    
         BNE   DISRES6                                                          
         CLI   BUYPROF2+5,C'Y'                                                  
         BE    DISRES6                                                          
         CLI   BUYPROF2+5,C'N'                                                  
         BE    DISRES6                                                          
         CLI   BUYPROF2+5,X'40'                                                 
         BNH   DISRES6                                                          
         MVC   0(1,R2),BUYPROF2+5                                               
DISRES6  LA    R2,2(R2)                                                         
         SPACE                                                                  
DISRES8  MVI   BYTE,UDA                                                         
         GOTO1 (RF),(R1),(BYTE,(R9))                                            
         CLI   FLDH+7,0            TEST FOR OUTPUT                              
         BE    *+10                NO                                           
         MVC   0(11,R2),FLD        YES                                          
*  DISPLAY POSTING TYPE                                                         
         MVC   DEMPTYP(1),NBPOSTYP                                              
*                                                                               
* FIND TCAR AND PLACE ON SCREEN                                                 
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'02',NBAIO),0                       
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    *+6                 YES                                          
         DC    H'0'                NO                                           
         L     RE,12(R1)                                                        
         USING NUSDRD,RE                                                        
         MVC   DEMPTYP+1(1),NUSTCAR                                             
         OI    DEMPTYPH+6,X'80'                                                 
         DROP  RE                                                               
         SPACE                                                                  
DISX     B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO SAVE PACKAGE GUARANTIES AND CHANGE TO 100 PERCENT              
*                                                                               
SAVGUAR  NTR1                                                                   
         CLC   NBACTDAT,=XL2'B32B' SEP11/89                                     
         BL    SAVGUAR8                                                         
         USING NUNGUD,R3                                                        
         XC    PNGUAHLD,PNGUAHLD                                                
****     XC    PGUARHLD,PGUARHLD                                                
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
         EJECT                                                                  
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
***      MVC   NUGUAFAC,PGUARHLD                                                
***      MVC   NBGUAFAC(2),PGUARHLD    RESTORE PACKAGE GUARENTEE                
*                                                                               
         USING NUNDGD,R3                                                        
RESGUAR4 GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'B4',NBAIO),0                       
         CLI   12(R1),0            TEST IF DEMO GUARANTEE FOUND                 
         BNE   RESGUAR8            NO                                           
         L     R3,12(R1)                                                        
*                                                                               
         MVC   NUNDGFAC,DNGUAHLD                                                
***      MVC   NUGUAFAC,DGUARHLD                                                
*                                                                               
RESGUAR8 B     EXXMOD                                                           
         DROP  R3                                                               
         EJECT                                                                  
* SUB-ROUTINE TO INITIALIZE EDIT                                                
*                                                                               
EDINIT   NTR1                                                                   
         GOTO1 VCALLOV,DMCB,(X'35',0),ATWA                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VEDIT,DMCB          GENERAL EDIT                                 
         SPACE                                                                  
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
         XC    UNBLOCK,UNBLOCK                                                  
         ST    R9,UNAGLOB                                                       
         MVC   UNAREC,AIOAREA1                                                  
         MVC   UNALOCAL,AIOAREA4   LOCAL STORAGE FOR EDIT                       
         L     RF,VEDIT                                                         
         LA    R1,DMCB                                                          
         GOTO1 (RF),(R1),(C'I',(R4))                                            
         SPACE                                                                  
         B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT HEADLINE FIELDS                                           
*                                                                               
EDHEAD   NTR1                                                                   
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
         LA    R3,HEADTAB                                                       
         LA    R2,DEMSHRH                                                       
         SPACE                                                                  
ED2      CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    EDX                                                              
         ST    R2,UNFLDH                                                        
         ST    R2,FADDR                                                         
         MVC   UNEDATA,0(R3)                                                    
         GOTO1 VEDIT,DMCB,(C'E',(R4))                                           
         CLI   UNERROR,0                                                        
         BE    *+14                                                             
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
         LA    R3,1(R3)            NEXT DATA TYPE                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         NEXT UNPROTECTED FIELD                       
         BO    *-10                                                             
         B     ED2                                                              
         SPACE                                                                  
EDX      B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO EDIT DEMO PERCENTAGE ADJUSTMENT FIELDS AFTER                   
* LOWER SCREEN HAS BEEN EDITED FOR OVERRIDES.                                   
*                                                                               
ADJED    NTR1                                                                   
         LA    R4,BLOCK                                                         
         USING UNBLOCKD,R4                                                      
         L     RF,VEDIT                                                         
         LA    R1,DMCB                                                          
         LA    R3,ADJTAB                                                        
         LA    R2,DEMIMPH                                                       
         SPACE                                                                  
ADJED2   CLI   0(R3),X'FF'         TEST FOR E-O-L                               
         BE    ADJEDX                                                           
         ST    R2,UNFLDH                                                        
         ST    R2,FADDR                                                         
         MVC   UNEDATA,0(R3)                                                    
         GOTO1 (RF),(R1),(C'E',(R4))                                            
         CLI   UNERROR,0                                                        
         BE    *+14                                                             
         MVC   FERN,UNERROR                                                     
         B     ERROR                                                            
*                                                                               
         LA    R3,1(R3)            NEXT DATA TYPE                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'20'         NEXT UNPROTECTED FIELD                       
         BO    *-10                                                             
         B     ADJED2                                                           
         SPACE                                                                  
ADJEDX   B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY DEMO CATEGORY NAMES                                    
*                                                                               
DCAT     NTR1                                                                   
         LA    R4,DBLOCKA          INIT  DBLOCK                                 
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
*                                                                               
         LA    R2,DEMNAM1H                                                      
         OC    DEMS,DEMS                                                        
         BZ    DCATX                                                            
         ZIC   R0,DEMS                                                          
         LA    R3,DEMOS                                                         
         SPACE                                                                  
DCAT2    MVC   THREE,0(R3)                                                      
         GOTO1 VDEMOCON,DMCB,THREE,(10,WORK),(C'S',DBLOCK),ESTUSNS              
         MVC   8(10,R2),WORK       CATEGORY NAME                                
         XC    WORK(10),WORK                                                    
         CLI   1(R3),USERMOD       TEST FOR USER DEMO                           
         BNE   DCAT4               NO                                           
         ZIC   R1,2(R3)            EXTRACT USER DEMO NAME BY INDEXING           
         MH    R1,=H'7'            INTO NAME LIST WITH CATEGORY                 
         LA    RE,ESTUSNS-7(R1)                                                 
         XC    8(10,R2),8(R2)      CLEAR FIELD                                  
         MVC   8(7,R2),0(RE)                                                    
         SPACE 1                                                                
DCAT4    OI    6(R2),X'80'                                                      
         LA    R2,LINELEN(R2)                                                   
         LA    R3,3(R3)                                                         
         BCT   R0,DCAT2                                                         
         SPACE                                                                  
DCATX    B     EXXMOD                                                           
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO LOOK UP DEMO VALUES                                            
*                                                                               
LOOKDEM  NTR1                                                                   
         MVI   NBACTOPT,YES        RETURN ACTUAL VALUES                         
         MVC   NBADEM,AIOAREA3     IO3 CONTAINS NETWORK DEMO BLOCK              
         L     R4,NBADEM                                                        
         USING NETDEMOD,R4                                                      
         ZIC   R1,ESTNDEMS         PASS ESTIMATE LIST WHICH                     
         MH    R1,=H'3'            INCLUDES HOMES                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDDEMOS(0),ESTDEMS                                               
*        MVC   NDWGTLST,ESTWLST    NOT CURRENTLY SUPPORTING WEIGTHS             
         MVC   NDUSRNMS,ESTUSNS                                                 
         MVC   USERNMS,ESTUSNS                                                  
         GOTO1 VNETVAL,DMCB,NETBLOCK                                            
         MVC   SVUSRUNV(16),NDUSRUNV                                            
         SPACE                                                                  
         SR    R0,R0                                                            
         LA    R1,ESTDEMS                                                       
         CLC   0(3,R1),DEMOS       FIND DISPLACEMENT TO START OF DEMO           
         BE    *+12                LIST                                         
         LA    R1,3(R1)                                                         
         BCT   R0,*-14                                                          
         LPR   R0,R0                                                            
         STC   R0,DISP                                                          
         SPACE                                                                  
LOOK2    MVC   HOMERAT+2(2),NBESTHOM+2  EXTRACT HOMES VALUES FROM               
         MVC   HOMEIMP,NBESTHOM+4       DEMO OUTPUT BLOCK                       
         MVC   HOMARAT+2(2),NBACTHOM+2                                          
         MVC   HOMAIMP,NBACTHOM+4                                               
         MVI   BYTE,NO             SET SWITCH                                   
         SPACE                                                                  
LOOK4    ZIC   R0,DEMS             TRANSFER THE SCREEN DEMO'S VALUES            
         ZIC   R1,DISP             FROM NETWORK DEMO BLOCK TO WORK AREA         
         SR    R2,R2                                                            
         SPACE                                                                  
LOOK5    LR    R3,R1                                                            
         MH    R3,=H'3'                                                         
         LA    R3,ESTDEMS(R3)                                                   
         CLI   1(R3),USERMOD                                                    
         BE    LOOK5A                                                           
         CLI   0(R3),0             CHECK FOR NAD                                
         BNE   LOOK5A                                                           
         CLI   2(R3),1                                                          
         BNE   LOOK5A                                                           
         LA    R1,1(R1)                                                         
         B     LOOK5                                                            
*                                                                               
LOOK5A   LR    RE,R1               DISPLACEMENT INTO OUTPUT BLOCK               
         SLL   RE,3                                                             
         LR    RF,R2                                                            
         SLL   RF,2                FULLWORD DISP INTO WORK AREA LIST            
         CLI   BYTE,YES            TEST FOR ACTUALS                             
         BE    LOOK6                                                            
         LA    RE,NDESTDEM(RE)     INDEX INTO TABLE                             
         LH    R3,0(RE)                                                         
         ST    R3,OLDEVPH(RF)                                                   
         LH    R3,2(RE)                                                         
         ST    R3,OLDERTG(RF)                                                   
         L     R3,4(RE)                                                         
         ST    R3,OLDEIMP(RF)                                                   
         B     LOOK7                                                            
         SPACE                                                                  
LOOK6    LA    RE,NDACTDEM(RE)     INDEX INTO RETURNED VALUES                   
         LH    R3,0(RE)            SLOT ACTUAL VALUES FOR DEMO                  
         ST    R3,OLDAVPH(RF)      INTO WORK AREA                               
         LH    R3,2(RE)                                                         
         ST    R3,OLDARTG(RF)                                                   
         L     R3,4(RE)                                                         
         ST    R3,OLDAIMP(RF)                                                   
         SPACE                                                                  
LOOK7    LA    R1,1(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,LOOK5                                                         
         CLI   BYTE,YES            TEST IF ACTUALS DONE                         
         BE    LOOK8               YES                                          
         MVI   BYTE,YES            NO-GO BACK AND DO THEM                       
         B     LOOK4                                                            
         SPACE 2                                                                
* SAVE UNIVERSES VALUES                                                         
*                                                                               
LOOK8    XC    NDDEMOS,NDDEMOS                                                  
         ZIC   R1,DEMS             TO HOMES                                     
         MH    R1,=H'3'                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NDDEMOS(0),DEMOS                                                 
         LA    R1,NDDEMOS+1(R1)                                                 
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    R1,NDDEMOS          ALTER MODIFIERS IN DEMO LIST TO              
         SR    R0,R0               LOOK UP UNIVERSES                            
LOOK9    CLI   1(R1),63            TEST FOR WEIGHTED DEMO                       
         BNE   *+14                NO                                           
         LPR   R0,R0               YES                                          
         STC   R0,WEIGHT           DISPLACEMENT TO WEIGHTED DEMO                
         B     *+10                                                             
*                                                                               
         BCTR  R0,0                                                             
         MVI   1(R1),C'U'          SET MODIFIER TO UNIVERSE                     
*                                                                               
         LA    R1,3(R1)                                                         
         CLI   0(R1),X'FF'         TEST FOR E-O-L                               
         BNE   LOOK9               NO                                           
*                                                                               
         L     RE,AIOAREA3                                                      
         LA    RE,NDBLKLEN(RE)                                                  
         ST    RE,NDAUBLOK                                                      
         GOTO1 VNETVAL,DMCB,NETBLOCK                                            
         SPACE                                                                  
         L     R3,NDAUBLOK                                                      
         USING NETUNIVD,R3                                                      
         MVC   HOMEUNIV(32),NDUEHOME                                            
*                                                                               
         CLI   WEIGHT,0                                                         
         BE    LOOK12                                                           
         ZIC   R1,WEIGHT           PUT THE WEIGHTED DEMO UNIVERSE               
         SLL   R1,2                VALUE INTO RIGHT WORK AREA POSITION          
         L     R0,NDESTWUN                                                      
         ST    R0,HOMEUNIV(R1)                                                  
         SPACE 1                                                                
LOOK12   MVC   HOMAUNIV(32),NDUAHOME                                            
*                                                                               
         CLI   WEIGHT,0                                                         
         BE    LOOK13                                                           
         ZIC   R1,WEIGHT                                                        
         SLL   R1,2                                                             
         L     R0,NDACTWUN         PLACE WEIGHTED VALUE IN LIST                 
         ST    R0,HOMAUNIV(R1)                                                  
         SPACE                                                                  
* LOOKUP FOR USER DEMO UNIVERSES                                                
*                                                                               
LOOK13   LA    RE,DEMOS            POINT TO FIRST DEMO                          
         SR    R1,R1               INITIALIZE INDEX INTO DEMO VALUES            
         ZIC   R2,DEMS             COUNTER                                      
*                                                                               
LOOK14   CLI   1(RE),USERMOD       TEST FOR USER DEMO                           
         BNE   LOOK15                                                           
         ZIC   RF,2(RE)                                                         
         SLL   RF,2                INDEX TO UNIVERSE VALUE                      
         L     R0,SVUSRUNV-4(RF)                                                
         ST    R0,OLDEUNI(R1)      SLOT INTO POSITION IN UNIV VALUES            
         ST    R0,OLDAUNI(R1)      ALSO USE FOR ACTUAL UNIVERSE                 
*                                                                               
LOOK15   LA    RE,3(RE)            NEXT DEMO ENTRY                              
         LA    R1,4(R1)            INCREASE INDEX                               
         BCT   R2,LOOK14                                                        
         SPACE 1                                                                
LOOKX    B     EXXMOD                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY HOMES VALUES ON SCREEN                                 
*                                                                               
HOMESD   NTR1                                                                   
         LA    R3,HOMTAB           POINT TO TABLE                               
         SPACE                                                                  
HO100    CLI   0(R3),X'FF'         TEST FOR END OF TABLE                        
         BE    HOEXT                                                            
         ZIC   R2,0(R3)            DISPLACEMENT INTO SCREEN LINE                
         LA    R2,DEMHOM(R2)       POINT TO OUTPUT POSITION                     
         ICM   R1,7,1(R3)          DISPLACEMENT TO VALUE                        
         LA    R1,TEMPD(R1)                                                     
         L     R0,0(R1)            VALUE                                        
         TM    4(R3),X'01'         TEST FOR EDIT TO ONE DECIMAL                 
         BO    HO200                                                            
         TM    4(R3),X'02'         TEST FOR HUNDREDS IF TABLE                   
         BZ    HO110                                                            
         TM    4(R3),X'80'         TEST FOR ACTUAL IMPRESSION                   
         BZ    HO105                                                            
         XC    0(7,R2),0(R2)       CLEAR OUT ACTUAL IMPRESSION FIELD            
         XC    OVERSTOR,OVERSTOR                                                
         MVC   OVERSTOR+2(2),HOMIMP1                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'DE',AIOAREA1),(5,OVERSTOR)         
         CLI   12(R1),0            TEST IF OVERRIDE FOUND                       
         BE    HO103               NO                                           
         MVC   OVERSTOR+2(2),HOMIMP2                                            
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'DE',AIOAREA1),(5,OVERSTOR)         
         CLI   12(R1),0            TEST IF OVERRIDE FOUND                       
         BNE   HO105               NO                                           
HO103    MVI   0(R2),STAR          MOVE STAR                                    
         LA    R2,1(R2)                                                         
HO105    CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BE    HO120                                                            
HO110    EDIT  (R0),(5,(R2)),ALIGN=LEFT,ZERO=BLANK                              
         B     HO300                                                            
*                                                                               
HO120    TM    4(R3),X'02'                                                      
         BO    HO140                                                            
         EDIT  (R0),(6,(R2)),1,ALIGN=LEFT,ZERO=BLANK                            
         B     HO300                                                            
*                                                                               
HO140    EDIT  (R0),(7,(R2)),1,ALIGN=LEFT,ZERO=BLANK                            
         B     HO300                                                            
*--CHECK PRECISION FACTOR                                                       
HO200    LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
HO220    CLI   0(RE),C'R'                                                       
         BE    HO240                                                            
         LA    RE,2(RE)                                                         
         BCT   RF,HO220                                                         
         DC    H'0'                                                             
HO240    TM    1(RE),X'82'                                                      
         BO    HO260                                                            
*                                                                               
         EDIT  (R0),(5,(R2)),1,ALIGN=LEFT                                       
         XC    DEMRAT,DEMRAT                                                    
         MVC   DEMRAT(7),0(R2)     MAKE SURE BOTH RTGS AGREE                    
         OI    DEMRATH+6,X'80'                                                  
         B     HO300                                                            
         SPACE                                                                  
HO260    EDIT  (R0),(6,(R2)),2,ALIGN=LEFT                                       
         XC    DEMRAT,DEMRAT                                                    
         MVC   DEMRAT(7),0(R2)     MAKE SURE BOTH RTGS AGREE                    
         OI    DEMRATH+6,X'80'                                                  
         SPACE                                                                  
HO300    LA    R3,L'HOMTAB(R3)     NEXT TABLE ENTRY                             
         B     HO100                                                            
         SPACE                                                                  
HOEXT    OI    DEMHOMH+6,X'80'                                                  
         B     EXXMOD                                                           
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY ACTUAL HOMES RATING                                    
*                                                                               
DISRAT   ST    RE,SAVEREG                                                       
         L     R0,HOMARAT          GET ACTUAL RATING                            
*--CHECK PRECISION FACTOR                                                       
         LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
DISRT40  CLI   0(RE),C'R'                                                       
         BE    DISRT60                                                          
         LA    RE,2(RE)                                                         
         BCT   RF,DISRT40                                                       
         DC    H'0'                                                             
DISRT60  TM    1(RE),X'82'                                                      
         BO    DISRT80                                                          
         EDIT  (R0),(6,DEMARHM),1,ALIGN=LEFT,ZERO=NOBLANK                       
         B     DISRT100                                                         
*                                                                               
DISRT80  EDIT  (R0),(7,DEMARHM),2,ALIGN=LEFT,ZERO=NOBLANK                       
*                                                                               
DISRT100 OI    DEMARHMH+6,X'80'                                                 
         XC    OVERSTOR,OVERSTOR   CHECK NEW FORMAT                             
         MVC   OVERSTOR+2(2),HOMRAT                                             
         GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(X'DE',AIOAREA1),(5,OVERSTOR)         
         CLI   12(R1),0            TEST IF OVERRIDE FOUND                       
         BNE   DISRATX             NO                                           
         MVC   DUB(6),DEMARHM      INSERT A STAR BEFORE VALUE                   
         MVI   DEMARHM,STAR                                                     
         MVC   DEMARHM+1(5),DUB                                                 
         SPACE 1                                                                
DISRATX  L     RE,SAVEREG                                                       
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO DISPLAY DEMO VALUES ON LOWER SCREEN                            
*                                                                               
DISVAL   NTR1                                                                   
         MVI   LINE,0                                                           
         LA    R2,DEMNAM1H         INITIALIZE LINE POINTER                      
         ST    R2,THISLINE                                                      
         SPACE                                                                  
DS100    ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         CLM   R1,1,DEMS           TEST IF ALL DEMOS DISPLAYED                  
         BH    DSEXT               YES                                          
         STC   R1,LINE                                                          
         BCTR  R1,0                                                             
         LR    RE,R1                                                            
         MH    RE,=H'3'                                                         
         LA    RF,DEMOS(RE)                                                     
         MVC   THREE,0(RF)         DEMO FOR THE LINE                            
         SLL   R1,2                DISPLACEMENT INTO VALUE LIST                 
         STH   R1,LDISP                                                         
         LA    R3,VALTAB           R3 POINTS TO VALUE TABLE                     
         USING VALTABD,R3                                                       
         SPACE                                                                  
DS200    CLI   0(R3),X'FF'         TEST FOR E-O-T                               
         BE    DS500                                                            
         ICM   R4,7,VALODISP                                                    
         LA    R4,TEMPD(R4)        POINT R4 TO START OF LIST                    
         AH    R4,LDISP            NOW ADD LIST DISP. TO POINT TO VALUE         
         ZIC   R2,VALFDISP         DISP. TO OUTPUT FIELD HEADER                 
         A     R2,THISLINE                                                      
         L     R0,0(R4)            OUTPUT VALUE                                 
         TM    VALDIS,X'02'                                                     
         BZ    *+12                                                             
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BE    DS220                                                            
         TM    VALDIS,X'04'                                                     
         BO    DS240                                                            
         EDIT  (R0),(6,8(R2)),ALIGN=LEFT,ZERO=BLANK                             
         B     DS300                                                            
*                                                                               
DS220    EDIT  (R0),(8,8(R2)),1,ALIGN=LEFT,ZERO=BLANK                           
         B     DS300                                                            
*                                                                               
*--CHECK PRECISION FACTOR                                                       
DS240    LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
DS245    CLC   VALMOD(1),0(RE)                                                  
         BE    DS250                                                            
         LA    RE,2(RE)                                                         
         BCT   RF,DS245                                                         
         DC    H'0'                                                             
DS250    TM    1(RE),X'82'                                                      
         BO    DS260                                                            
         EDIT  (R0),(7,8(R2)),1,ALIGN=LEFT,ZERO=BLANK                           
         B     DS300                                                            
*                                                                               
DS260    EDIT  (R0),(6,8(R2)),2,ALIGN=LEFT,ZERO=BLANK                           
         SPACE                                                                  
DS300    OI    6(R2),X'80'         XMIT                                         
         CLI   VALOVER,0           TEST IF OVERRIDE POSSIBLE                    
         BE    DS400               NO                                           
         XC    DUB,DUB             CLEAR ELEMENT DATA SEARCH AREA               
         MVC   ELCODE,VALOVER      DEMO OVERRIDE CODE                           
         MVI   BYTE,5              OVERRIDE DATA LENGTH                         
         MVC   DUB+1(3),THREE                                                   
         MVC   DUB+2(1),VALMOD     SEARCH ON MODIFIER/CATEGORY                  
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   DS320                                                            
*        CLI   ELCODE,X'DE'        ACTUAL                                       
*        BNE   DS320                                                            
         CLI   DUB+2,C'T'          IMPRESSION                                   
         BNE   DS320                                                            
         MVI   DUB+2,C'H'          HUNDREDS OVERRIDE                            
DS320    CLI   THREE+1,USERMOD     TEST FOR USER DEMO                           
         BNE   DS340               NO                                           
         MVC   ELCODE,VALUSER      YES-SET USER OVERRIDE CODE/LENGTH            
         MVI   BYTE,8                                                           
         ZIC   R1,THREE+2          DATA IS MODIFIER/USER DEMO NAME              
         MH    R1,=H'7'                                                         
         LA    RE,USERNMS-7(R1)                                                 
         MVC   DUB(1),DUB+2        MOVE MODE INTO DUB                           
         MVC   DUB+1(7),0(RE)                                                   
*                                                                               
DS340    GOTO1 VHELLO,DMCB,(C'G',UNTFILE),(ELCODE,AIOAREA1),(BYTE,DUB)          
         CLI   12(R1),0                                                         
         BNE   DS400                                                            
         L     RE,12(R1)           IF 0 THEN MOVE IN 0 IF OVERRIDE              
         USING NUOVD,RE                                                         
         CLI   THREE+1,USERMOD                                                  
         BNE   *+12                                                             
         ICM   RF,3,12(RE)                                                      
         B     *+8                                                              
         ICM   RF,3,NUOVVAL+2                                                   
         BNZ   *+8                                                              
         MVI   8(R2),C'0'                                                       
         MVC   DUB(7),8(R2)                                                     
         MVI   8(R2),C'*'          INSERT A STAR BEFORE OVERRIDE VALUE          
         MVC   9(5,R2),DUB                                                      
         TM    VALDIS,X'02'                                                     
         BZ    DS400                                                            
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   DS400                                                            
         MVC   9(7,R2),DUB                                                      
         SPACE                                                                  
DS400    LA    R3,VALTABL(R3)      NEXT TABLE ENTRY                             
         B     DS200                                                            
         SPACE                                                                  
DS500    L     R1,THISLINE         END-OF-TABLE, DO NEXT SCREEN LINE            
         LA    R1,LINELEN(R1)                                                   
         ST    R1,THISLINE                                                      
         B     DS100                                                            
         SPACE                                                                  
DSEXT    B     EXXMOD                                                           
         DROP  R3,RE                                                            
         EJECT                                                                  
* SUB-ROUTINE TO EDIT ACTUAL HOMES RATING                                       
*                                                                               
GETHOM   NTR1                                                                   
         LA    R2,DEMARHMH                                                      
         GOTO1 VGETFLD                                                          
         CLI   FLDH+5,0            TEST FOR ANY INPUT                           
         BE    GH100               NO                                           
*                                                                               
         CLI   FLD,C'X'                                                         
         BNE   GH120                                                            
         CLI   FLDH+5,1                                                         
         BNE   GETHOMR                                                          
GH100    L     R0,=F'-1'           RESET OVERRIDE WITH REAL VALUES              
         B     GH800                                                            
*                                                                               
GH120    ZIC   R1,FLDH+5           R1 CONTAINS FIELD LENGTH                     
         CLI   FLD,STAR            STRIP LEADING ASTERISK BEFORE SCAN           
         BNE   GH200                                                            
*                                                                               
         SH    R1,=H'1'                                                         
         BZ    GETHOMR                                                          
         STC   R1,FLDH+5           ADJUST DATA LENGTH FOR STAR                  
         BCTR  R1,0                                                             
         EX    R1,FLDMOVE                                                       
         LA    R1,1(R1)                                                         
         EX    R1,DUBMOVE                                                       
         SPACE 1                                                                
GH200    SR    R0,R0               CLEAR R0 FOR NEW VALUE                       
         LA    R5,FLD              RE POINTS TO START OF FIELD                  
         LR    R2,R1               SAVE TOTAL LENGTH IN R2                      
         SPACE 1                                                                
GH300    CLI   0(R5),PERIOD        TEST FOR DECIMAL POINT                       
         BE    GH400                                                            
         CLI   0(R5),C'0'          VALIDATE FOR NUMERIC DATA                    
         BL    GETHOMR                                                          
         CLI   0(R5),C'9'                                                       
         BH    GETHOMR                                                          
         LA    R5,1(R5)            NEXT BYTE                                    
         BCT   R1,GH300                                                         
         SPACE 1                                                                
*--CHECK PRECISION FACTOR                                                       
GH400    LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
GH450    CLI   0(RE),C'R'                                                       
         BE    GH460                                                            
         LA    RE,2(RE)                                                         
         BCT   RF,GH450                                                         
         DC    H'0'                                                             
GH460    TM    1(RE),X'82'                                                      
         BO    GH700                                                            
*--EDIT DEMO FOR TENTHS PLACE                                                   
         LR    R3,R2               LENGTH OF FIELD                              
         SR    R3,R1               LENGTH OF DATA SO FAR                        
         BZ    GH600               NOTHING BEFORE POINT                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         CH    R0,=H'100'                                                       
         BH    GETHOMR                                                          
         MH    R0,=H'10'           SCALE RATING TO TENTHS                       
         LTR   R1,R1               TEST IF WHOLE FIELD EDITED                   
         BZ    GH800               YES                                          
         SPACE 1                                                                
GH600    CH    R1,=H'2'            ONLY CAN BE POINT AND DIGIT                  
         BNE   GETHOMR                                                          
         CLI   1(R5),C'0'                                                       
         BL    GETHOMR                                                          
         CLI   1(R5),C'9'                                                       
         BH    GETHOMR                                                          
         NI    1(R5),X'0F'         TURN OFF ZONE BITS                           
         ZIC   RF,1(R5)                                                         
         AR    R0,RF                                                            
         B     GH800                                                            
*--NUMBER TO HUNDREDS PLACE EDIT                                                
GH700    LR    R3,R2               LENGTH OF FIELD                              
         SR    R3,R1               LENGTH OF DATA SO FAR                        
         BZ    GH760               NOTHING BEFORE POINT                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         CH    R0,=H'100'                                                       
         BH    GETHOMR                                                          
         MH    R0,=H'100'          SCALE RATING TO HUNDREDS                     
         LTR   R1,R1               TEST IF WHOLE FIELD EDITED                   
         BZ    GH800               YES                                          
         SPACE 1                                                                
GH760    CH    R1,=H'3'            ONLY CAN BE POINT AND 2 DIGITS               
         BH    GETHOMR                                                          
         BCTR  R1,0                                                             
         LR    R3,R1                                                            
         BCTR  R3,0                                                             
         XC    DUB,DUB                                                          
         EX    R3,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R5)                                                      
         CVB   R3,DUB                                                           
         CH    R1,=H'2'            IF ONLY ONE DIGIT AFTER DECIMAL              
         BE    GH770               THEN MULTIPLY THE DIGIT BY TEN               
         MH    R3,=H'10'           TO PUT INTO CORRECT PRECISION                
*                                                                               
GH770    CLI   1(R5),C'0'          VALIDATE FOR NUMERIC DATA                    
         BL    GETHOMR                                                          
         CLI   1(R5),C'9'                                                       
         BH    GETHOMR                                                          
         LA    R5,1(R5)                                                         
         BCT   R1,GH770                                                         
*                                                                               
         AR    R0,R3                                                            
         SPACE 1                                                                
GH800    ST    R0,NEWARHM                                                       
         SPACE 1                                                                
GETHOMX  B     EXXMOD                                                           
         SPACE 1                                                                
GETHOMR  MVI   FERN,INVERR                                                      
         B     ERROR                                                            
         SPACE 2                                                                
FLDMOVE  MVC   DUB(0),FLD+1                                                     
DUBMOVE  MVC   FLD(0),DUB                                                       
         EJECT                                                                  
* SUB-ROUTINE FOR ACTUAL HOMES OVERRIDE                                         
*                                                                               
HOMOVER  ST    RE,SAVEREGE                                                      
         CLC   HOMARAT,NEWARHM     TEST FOR CHANGE                              
         BE    HOMOVERX            NO                                           
*                                                                               
         XC    OVERSTOR,OVERSTOR   DELETE NEW FORMATTED OVERRIDE ELEM           
         MVC   OVERSTOR+2(2),HOMRAT                                             
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'DE',AIOAREA1),(5,OVERSTOR)         
         MVC   OVERSTOR+2(2),HOMIMP1                                            
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'DE',AIOAREA1),(5,OVERSTOR)         
         MVC   OVERSTOR+2(2),HOMIMP2                                            
         GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(X'DE',AIOAREA1),(5,OVERSTOR)         
         CLI   NEWARHM,X'FF'       TEST FOR ZERO OR NO INPUT                    
         BE    HOMOVERX            YES                                          
*                                                                               
         ST    R4,SAVEREG4                                                      
         XC    OVEREL,OVEREL                                                    
         LA    R4,OVEREL                                                        
         USING NUOVD,R4                                                         
         MVI   NUOVEL,X'DE'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVMOD(2),HOMRAT                                                
         MVC   NUOVVAL+2(2),NEWARHM+2                                           
         BAS   RE,SETPRE                                                        
         BAS   RE,PUTEL                                                         
*--SET HOME OVERRIDE IMPRESSION                                                 
         MVC   BYTE,NUOVPRE                                                     
         MVI   THREE,0                                                          
         MVC   THREE+1(2),HOMIMP1                                               
         SR    R2,R2                                                            
         ICM   R2,3,NEWARHM+2                                                   
         L     R1,HOMAUNIV                                                      
         BAS   RE,IMP2                                                          
*                                                                               
         L     RE,NBAIO                                                         
         USING NURECD,RE                                                        
         OI    NUACTWHY,X'08'      DEMO OVERRIDE                                
         L     R4,SAVEREG4                                                      
*                                                                               
HOMOVERX L     RE,SAVEREGE                                                      
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FIND OVERRIDE INPUT VALUES AND TO ADD ELEMENTS                 
* FOR THEM (ADDRESSABILITY TO BUYVALS - R5 - IS LOST)                           
*                                                                               
OVER     NTR1                                                                   
         MVI   LINE,0                                                           
         SPACE                                                                  
OVER2    ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         CLM   R1,1,DEMS                                                        
         BH    OVERX                                                            
         STC   R1,LINE                                                          
         BCTR  R1,0                                                             
         LR    RE,R1                                                            
         MH    RE,=H'3'                                                         
         LA    RF,DEMOS(RE)                                                     
         MVC   THREE,0(RF)         EXTRACT DEMO FROM LIST                       
         SLL   R1,2                                                             
         STH   R1,LDISP            DISPLACEMENT IN VALUE LIST                   
         LA    R3,VALTAB                                                        
         USING VALTABD,R3                                                       
         SPACE                                                                  
OVER4    CLI   0(R3),X'FF'         TEST FOR END OF TABLE                        
         BE    OVER8               YES                                          
         CLI   VALOVER,0           TEST IF OVERRIDE POSSIBLE                    
         BE    OVER7               NO                                           
         TM    VALOVCTL,X'40'      TEST FOR ESTIMATED IMPRESSION                
         BO    OVER7                                                            
         SPACE                                                                  
         ICM   R4,7,VALODISP                                                    
         LA    R4,TEMPD(R4)        POINT TO OLD VALUE LIST                      
         ICM   R5,7,VALNDISP                                                    
         LA    R5,TEMPD(R5)        POINT TO NEW VALUE LIST                      
         AH    R4,LDISP                                                         
         AH    R5,LDISP                                                         
*                                                                               
         CLC   0(4,R5),0(R4)       TEST NEW VS. OLD VALUE                       
         BE    OVER7               SAME                                         
*                                                                               
         CLI   VALOVER,X'DD'       TEST FOR ESTIMATED DEMO                      
         BNE   OVER4A              NO                                           
         L     RE,APACKREC                                                      
         USING NPRECD,RE                                                        
         TM    NPAKSTAT,X'80'      TEST FOR FROZEN PACKAGE                      
         BZ    OVER4A              NO                                           
         LA    R2,DEMNAM1H         FIND THE CORRESPONDING FIELD                 
         ZIC   R1,LINE                                                          
         BCTR  R1,0                                                             
         MH    R1,=Y(LINELEN)      DISPLACEMENT TO FIELD LINE                   
         ZIC   R0,VALFDISP         DISPLACEMENT TO FIELD                        
         AR    R1,R0                                                            
         LA    R2,0(R1,R2)         POINT TO ERROR FIELD                         
         ST    R2,FADDR                                                         
         MVI   FERN,PAKFERR                                                     
         B     ERROR                                                            
*                                                                               
OVER4A   L     R2,0(R5)            GET NEW VALUE                                
         XC    DUB,DUB             ELEMENT DATA FILTER                          
         MVC   ELCODE,VALOVER      OVERRIDE ELEMENT                             
         MVI   BYTE,5              DATA LENGTH                                  
         MVC   DUB+1(3),THREE      MODIFIER/CATEGORY                            
         MVC   DUB+2(1),VALMOD                                                  
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   OVER4B                                                           
         CLI   DUB+2,C'T'          IMPRESSION                                   
         BNE   OVER4B                                                           
         MVI   DUB+2,C'H'          HUNDREDS OVERRIDE                            
OVER4B   CLI   THREE+1,USERMOD     TEST FOR USER DEMO                           
         BNE   OVER5                                                            
         MVC   ELCODE,VALUSER      USER DEMO ELEMENT                            
         MVI   BYTE,8                                                           
         ZIC   R1,THREE+2                                                       
         MH    R1,=H'7'                                                         
         LA    RE,USERNMS-7(R1)                                                 
         MVC   DUB(1),DUB+2        MOVE MODE INTO DUB                           
         MVC   DUB+1(7),0(RE)      DATA IS MODIFIER/USER DEMO NAME              
         SPACE 1                                                                
OVER5    GOTO1 VHELLO,DMCB,(C'D',UNTFILE),(ELCODE,AIOAREA1),(BYTE,DUB)          
         CLI   THREE+1,USERMOD     TEST FOR USER DEMO                           
         BE    OVER5E                                                           
*                                                                               
         CLI   VALMOD,C'T'                                                      
         BE    OVER5B                                                           
         CLI   VALMOD,C'R'                                                      
         BNE   OVER6                                                            
         TM    VALOVCTL,X'80'      TEST FOR ESTIMATED RATING                    
         BNO   OVER5B                                                           
         MVI   DUB+2,C'V'          VPH                                          
         GOTO1 (RF),(R1),,,,                                                    
OVER5B   MVI   DUB+2,C'T'          IMPRESSION                                   
         GOTO1 (RF),(R1),,,,                                                    
         MVI   DUB+2,C'H'          IMPRESSION                                   
         GOTO1 (RF),(R1),,,,                                                    
         MVC   DUB+2(1),VALMOD     RESTORE MODIFIER                             
         B     OVER6                                                            
*--USER DEMOS                                                                   
OVER5E   CLI   VALMOD,C'T'                                                      
         BE    OVER5F                                                           
         CLI   VALMOD,C'R'                                                      
         BNE   OVER6                                                            
         TM    VALOVCTL,X'80'      TEST FOR ESTIMATED RATING                    
         BNO   OVER5F                                                           
         MVI   DUB,C'V'            DELETE ESTIMATED VPH                         
         GOTO1 (RF),(R1),,,,                                                    
OVER5F   MVI   DUB,C'T'            DELETE THE IMPS                              
         GOTO1 (RF),(R1),,,,                                                    
         MVI   DUB,C'H'                                                         
         GOTO1 (RF),(R1),,,,                                                    
         MVC   DUB(1),VALMOD       RESTORE MODIFIER                             
*                                                                               
OVER6    CLI   0(R5),X'FF'         TEST FOR ZERO OR NO INPUT                    
         BE    OVER7               YES-JUST DELETE ANY OVERRIDE EL              
*                                                                               
         ST    R4,SAVEREG4                                                      
         XC    OVEREL,OVEREL                                                    
         LA    R4,OVEREL           BUILD OVERRIDE ELEMENT                       
         CLI   THREE+1,USERMOD     TEST FOR USER DEMO                           
         BE    OVER6B              YES                                          
*                                                                               
         USING NUOVD,R4                                                         
         MVC   NUOVEL,ELCODE                                                    
         MVI   NUOVLEN,12                                                       
         MVC   NUOVZER(4),DUB                                                   
         CLI   NBHUNOPT,C'Y'                                                    
         BNE   OVER6A                                                           
         CLI   NUOVMOD,C'T'        IS IMPRESSIONS                               
         BNE   OVER6A                                                           
         MVI   NUOVMOD,C'H'        CHANGE TO HUNDREDS                           
OVER6A   STCM  R2,15,NUOVVAL                                                    
         BAS   RE,SETPRE                                                        
         MVC   BYTE,NUOVPRE        STORE PRECISION FACTOR IN BYTE               
         B     OVER6C                                                           
*                                                                               
         USING UDOVD,R4                                                         
OVER6B   MVC   UDOVEL,ELCODE                                                    
         MVI   UDOVLEN,14                                                       
         MVC   UDOVMOD(8),DUB      MODIFIER/USER DEMO NAME                      
         STCM  R2,15,UDOVVAL                                                    
*                                                                               
OVER6C   L     R4,SAVEREG4                                                      
         BAS   RE,PUTEL                                                         
         CLI   VALMOD,C'R'         TEST FOR RATING                              
         BNE   OVER6E                                                           
         TM    VALOVCTL,X'80'      TEST FOR ESTIMATED RATING                    
         BZ    *+16                                                             
         BAS   RE,VPH                                                           
         BAS   RE,EIMP             CALCULATE THE ESTIMATE IMP                   
         B     *+8                                                              
         BAS   RE,IMP              CALCULATE THE ACTUAL IMP                     
*                                                                               
OVER6E   L     R4,NBAIO                                                         
         USING NURECD,R4                                                        
         OI    NUACTWHY,X'08'      DEMO OVERRIDE                                
*                                                                               
OVER7    LA    R3,VALTABL(R3)                                                   
         B     OVER4                                                            
         SPACE                                                                  
OVER8    B     OVER2               NEXT LINE                                    
         SPACE                                                                  
OVERX    B     EXXMOD                                                           
         DROP  R3,R4,RE                                                         
         EJECT                                                                  
* SUB-ROUTINE TO CALCULATE ESTIMATED VPH AND TO ADD AN OVERRIDE                 
* ELEMENT FOR IT.                                                               
* AT ENTRY     R2 CONTAINS ESTIMATED RATING VALUE                               
*              THREE CONTAINS DEMO                                              
*              BYTE CONTAINS OVERRIDE RATING PRECISION FACTOR                   
*                                                                               
VPH      NTR1                                                                   
         LA    R4,OLDEUNI          POINT TO ESTIMATED UNIVERSE                  
         AH    R4,LDISP            AND INDEX TO APPROPRIATE VALUE               
         L     R3,0(R4)            UNIVERSE                                     
         L     R1,HOMERAT                                                       
         M     R0,HOMEUNIV                                                      
         TM    BYTE,X'82'          CHECK PRECISION FOR HUNDREDS RATING          
         BO    VPH20               ADJUST THE ROUND                             
*                                                                               
         AH    R1,=H'50'                                                        
         D     R0,=F'100'                                                       
         ST    R1,FULL             HOMES IMPRESSION                             
         B     VPH30                                                            
*                                                                               
VPH20    AH    R1,=H'500'                                                       
         D     R0,=F'1000'                                                      
         ST    R1,FULL             HOMES IMPRESSION                             
*                                                                               
VPH30    MR    R2,R2               RATING X UNIVERSE                            
         M     R2,=F'10000'        SCALE TO 3 DECIMAL PLACES                    
         OC    FULL,FULL           DEFEND AGAINST ZERO DIVISOR                  
         BZ    VPHX                                                             
         D     R2,FULL                                                          
         SR    R2,R2               PREPARE DIVIDEND                             
         TM    BYTE,X'82'          CHECK PRECISION FOR HUNDREDS RATING          
         BO    VPH40               ADJUST THE ROUND                             
*                                                                               
         AH    R3,=H'500'          ROUND BACK DOWN TO INTEGER                   
         D     R2,=F'1000'         PRECISION                                    
         B     VPH50                                                            
*                                                                               
VPH40    A     R3,=F'5000'                                                      
         D     R2,=F'10000'                                                     
         ST    R1,FULL             HOMES IMPRESSION                             
*                                                                               
VPH50    XC    OVEREL,OVEREL                                                    
         LA    R4,OVEREL                                                        
         CLI   THREE+1,USERMOD                                                  
         BE    VPH2                                                             
*                                                                               
         USING NUOVD,R4                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),THREE                                                 
         MVI   NUOVMOD,C'V'                                                     
         STH   R3,NUOVVAL+2                                                     
         STCM  R3,15,NUOVVAL                                                    
         BAS   RE,SETPRE           SET OVERRIDE PRECISION FACTOR                
         B     VPH4                                                             
         SPACE                                                                  
         USING UDOVD,R4                                                         
VPH2     LTR   R3,R3               TEST FOR ZERO VPH                            
         BZ    VPHX                YES-EXIT                                     
         MVI   UDOVEL,X'CD'                                                     
         MVI   UDOVLEN,14                                                       
         MVI   UDOVMOD,C'V'                                                     
         MVC   UDOVNAM,DUB+1       USER DEMO NAME                               
         STCM  R3,15,UDOVVAL                                                    
         SPACE 1                                                                
VPH4     BAS   RE,PUTEL                                                         
         SPACE 1                                                                
VPHX     B     EXXMOD                                                           
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE THE ACTUAL IMP AND TO ADD AN OVERRIDE                
*                                                                               
* AT ENTRY R2 CONTAINS OVERRIDE RATING                                          
* AT ENTRY BYTE CONTAINS OVERRIDE RATING PRECISION FACTOR                       
*                                                                               
IMP      ST    RE,SAVEREG                                                       
         CLI   THREE+1,USERMOD                                                  
         BE    IMPX                                                             
         CLI   THREE+2,1           TEST FOR HOMES                               
         BE    IMPX                NO OVERRIDE-USE FORMULA WHICH IS R*U         
         LA    RF,OLDAUNI                                                       
         AH    RF,LDISP            POINT TO UNIVERSE                            
         L     R1,0(RF)                                                         
IMP2     ST    RE,SAVEREG                                                       
         LR    R0,R2               PUT RATING IN R0                             
         LTR   R1,R1                                                            
         BZ    IMPX                                                             
         MR    R0,R0               RATING*UNIVERSE                              
         TM    BYTE,X'82'          CHECK PRECISION FOR HUNDREDS RATING          
         BO    IMP20               ADJUST THE ROUND                             
*                                                                               
         AH    R1,=H'50'           ROUND THE IMP TO THOUSANDS                   
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   IMP30                                                            
         MH    R1,=H'10'           X 10 FOR DECIMAL POINT                       
         B     IMP30                                                            
*                                                                               
IMP20    AH    R1,=H'500'          ROUND THE IMP TO THOUSANDS                   
         SR    R0,R0                                                            
         D     R0,=F'1000'                                                      
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+8                                                              
         MH    R1,=H'10'           X 10 FOR DECIMAL POINT                       
*                                                                               
IMP30    ST    R4,SAVEREG4                                                      
         XC    OVEREL,OVEREL                                                    
         LA    R4,OVEREL                                                        
         USING NUOVD,R4                                                         
         MVI   NUOVEL,X'DE'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),THREE                                                 
         MVI   NUOVMOD,C'T'                                                     
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+8                                                              
         MVI   NUOVMOD,C'H'                                                     
         STCM  R1,15,NUOVVAL                                                    
         BAS   RE,SETPRE                                                        
         BAS   RE,PUTEL                                                         
         L     R4,SAVEREG4                                                      
*                                                                               
IMPX     L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO CALCULATE THE ESTIMATE IMP AND TO ADD AN OVERRIDE              
*                                                                               
* AT ENTRY R2 CONTAINS OVERRIDE RATING                                          
* AT ENTRY BYTE CONTAINS OVERRIDE RATING PRECISION FACTOR                       
*                                                                               
EIMP     ST    RE,SAVEREG                                                       
         CLI   THREE+1,USERMOD                                                  
         BE    EIMPX                                                            
         CLI   THREE+2,1           TEST FOR HOMES                               
         BE    EIMPX                                                            
         LA    RE,OLDEUNI                                                       
         AH    RE,LDISP            POINT TO UNIVERSE                            
         L     R1,0(RE)                                                         
         LR    R0,R2               PUT RATING IN R0                             
         MR    R0,R0               RATING*UNIVERSE                              
         TM    BYTE,X'82'          CHECK PRECISION FOR HUNDREDS RATING          
         BO    EIMP20              ADJUST THE ROUND                             
*                                                                               
         AH    R1,=H'50'           ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'100'                                                       
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   EIMP30                                                           
         MH    R1,=H'10'           X 10 FOR DECIMAL POINT                       
*                                                                               
EIMP20   AH    R1,=H'500'          ROUND THE IMP TO THOUSANDS                   
         D     R0,=F'1000'                                                      
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+8                                                              
         MH    R1,=H'10'           X 10 FOR DECIMAL POINT                       
*                                                                               
EIMP30   ST    R4,SAVEREG4                                                      
         XC    OVEREL,OVEREL                                                    
         LA    R4,OVEREL                                                        
         USING NUOVD,R4                                                         
         MVI   NUOVEL,X'DD'                                                     
         MVI   NUOVLEN,12                                                       
         MVC   NUOVCAT(3),THREE                                                 
         MVI   NUOVMOD,C'T'                                                     
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BNE   *+8                                                              
         MVI   NUOVMOD,C'H'                                                     
         STCM  R1,15,NUOVVAL                                                    
         BAS   RE,SETPRE                                                        
         BAS   RE,PUTEL                                                         
         L     R4,SAVEREG4                                                      
*                                                                               
EIMPX    L     RE,SAVEREG                                                       
         BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO ADD THE PRECISION FACTOR TO OVERRIDE ELEMENTS                  
* R4 POINTS TO THE ELEMENT AREA                                                 
*                                                                               
SETPRE   NTR1                                                                   
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
         SPACE 2                                                                
* SUB-ROUTINE TO PUT AN OVERRIDE ELEMENT TO RECORD                              
*                                                                               
PUTEL    LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 VHELLO,DMCB,(C'P',UNTFILE),AIOAREA1,OVEREL,0                     
         CLI   12(R1),0            TEST IF OK                                   
         BE    PUTELX              YES                                          
         CLI   12(R1),5            TEST FOR RECORD OVERFLOW                     
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   FERN,TOOLARGE                                                    
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     ERROR                                                            
         SPACE                                                                  
PUTELX   LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FORMAT MESSAGE AND TO SET CURSOR                               
*                                                                               
MSG      ST    RE,SAVEREG                                                       
         MVC   BUYMSG(16),=C'DEMOGRAPHIC DATA'                                  
         TM    MODE,DISPLAY                                                     
         BO    MSG2                                                             
         CLI   ACTION,CD           TEST FOR ACTION CHANGE                       
         BNE   MSG2                                                             
         MVC   BUYMSG+17(8),=C'CHANGED.'                                        
         MVC   BUYMSG+27(17),=C'ENTER NEXT ACTION'                              
         B     MSGX                                                             
         SPACE                                                                  
MSG2     MVC   BUYMSG+17(10),=C'DISPLAYED.'                                     
         LA    RE,BUYMSG+29                                                     
         MVC   0(13,RE),=C'ENTER CHANGES'                                       
         CLI   MORESW,YES          TEST FOR MORE DEMOS                          
         BNE   *+10                                                             
         MVC   14(L'MOREMSG,RE),MOREMSG                                         
         CLI   ACTION,CD           TEST FOR ACTION CHANGE                       
         BE    MSGX                                                             
         MVC   0(17,RE),=C'ENTER NEXT ACTION'                                   
         CLI   MORESW,YES          TEST FOR MORE DEMOS                          
         BNE   *+14                                                             
         MVI   17(RE),C' '                                                      
         MVC   18(L'MOREMSG,RE),MOREMSG                                         
         OI    DEMEVWH+6,X'01'     MAKE FIELD MODIFIED FOR RE-ENTRY             
         SPACE                                                                  
MSGX     LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
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
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFILE'                                                     
NOMORE   DC    C'**NO MORE DEMOS TO BE DISPLAYED - ENTER NEXT ACTION**'         
MOREMSG  DC    C'(MORE DEMOS)'                                                  
HOMES    DC    X'00',C'T',X'01'                                                 
HOMRAT   DC    C'R',X'01'                                                       
HOMIMP1  DC    C'T',X'01'                                                       
HOMIMP2  DC    C'H',X'01'                                                       
         SPACE 2                                                                
* LIST OF DATA TYPES FOR HEADLINE FIELDS                                        
*                                                                               
FLDTAB   DS    0C                                                               
         DC    AL1(USHR)                                                        
         DC    AL1(UHUT)                                                        
         DC    AL1(URAT)                                                        
         DC    AL1(UHAVE)                                                       
         DC    AL1(UHSC)                                                        
         DC    AL1(UHUTADJ)                                                     
         DC    AL1(UUNCD)                                                       
         DC    AL1(UNTI)                                                        
         DC    AL1(UIMP)                                                        
         DC    AL1(UUNPC)                                                       
         DC    AL1(UFEED)                                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
* LIST OF DATA TYPES FOR HEADLINE EDIT                                          
*                                                                               
HEADTAB  DS    0C                                                               
         DC    AL1(USHR)                                                        
         DC    AL1(UHUT)                                                        
         DC    AL1(URAT)                                                        
         DC    AL1(UHAVE)                                                       
         DC    AL1(UHSC)                                                        
         DC    AL1(UHUTADJ)                                                     
         DC    AL1(UUNCD)                                                       
         DC    AL1(UNTI)                                                        
         DC    X'FF'                                                            
         SPACE 2                                                                
* LIST OF DEMO PERCENTAGE ADJUSTMENT FIELDS FOR EDIT                            
*                                                                               
ADJTAB   DS    0C                                                               
         DC    AL1(UIMP)                                                        
         DC    AL1(UUNPC)                                                       
         DC    AL1(UFEED)                                                       
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE TO CONTROL DISPLAY OF HOMES VALUES                                      
*                                                                               
*        BYTE  0   = DISPLACEMENT INTO DEMHOM                                   
*        BYTES 1-3 = DISPLACEMENT OF VALUE IN LOCAL STORAGE                     
*        BYTE  4   = X'01' - EDIT TO ONE DECIMAL PLACE                          
*                                                                               
HOMTAB   DS    0XL5                                                             
         DC    AL1(8),AL3(HOMEIMP-TEMPD),X'02'                                  
         DC    AL1(17),AL3(HOMEUNIV-TEMPD),X'00'                                
         DC    AL1(24),AL3(HOMERAT-TEMPD),X'01'                                 
         DC    AL1(40),AL3(HOMAIMP-TEMPD),X'82'                                 
         DC    AL1(49),AL3(HOMAUNIV-TEMPD),X'00'                                
         DC    X'FF'                                                            
         SPACE 2                                                                
* TABLE FOR DISPLAY AND EDIT OF DEMO VALUES (COVERED BY VALTABD)                
*                                                                               
VALTAB   DS    0XL13                                                            
         DC    AL1(EVWH),AL3(OLDEVPH-TEMPD),AL3(NEWEVPH-TEMPD)                  
         DC    X'DD',X'CD',C'V',X'001000'                                       
*                                                                               
         DC    AL1(EIMPH),AL3(OLDEIMP-TEMPD),AL3(0)                             
         DC    X'DD',X'00',C'T',X'020040'                                       
*                                                                               
         DC    AL1(EUNIH),AL3(OLDEUNI-TEMPD),AL3(0)                             
         DC    X'00',X'00',C'U',X'000000'                                       
*                                                                               
         DC    AL1(ERTGH),AL3(OLDERTG-TEMPD),AL3(NEWERTG-TEMPD)                 
         DC    X'DD',X'CD',C'R',X'050580'                                       
*                                                                               
         DC    AL1(AVWH),AL3(OLDAVPH-TEMPD),AL3(NEWAVPH-TEMPD)                  
         DC    X'DE',X'CE',C'V',X'000000'                                       
*                                                                               
         DC    AL1(AIMPH),AL3(OLDAIMP-TEMPD),AL3(NEWAIMP-TEMPD)                 
         DC    X'DE',X'CE',C'T',X'020200'                                       
*                                                                               
         DC    AL1(AUNIH),AL3(OLDAUNI-TEMPD),AL3(0)                             
         DC    X'00',X'00',C'U',X'000000'                                       
*                                                                               
         DC    AL1(ARTGH),AL3(OLDARTG-TEMPD),AL3(NEWARTG-TEMPD)                 
         DC    X'DE',X'CE',C'R',X'050500'                                       
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*--OVERFLOW ROUTINES                                                            
*                                                                               
         DS    0F                                                               
         DROP  RA,RB                                                            
OVFLRTN  NMOD1 0,**13OV**                                                       
         LA    RA,2048(RB)                                                      
         LA    RA,2048(RA)                                                      
         USING OVFLRTN+4096,RA                                                  
         L     RC,4(R1)                                                         
*                                                                               
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     OVBRANCH(RF)                                                     
*                                                                               
OVBRANCH B     GETVAL                                                           
         B     DISGUAR                                                          
         EJECT                                                                  
* SUB-ROUTINE TO EDIT INPUT VALUES AND PLACE THEM IN WORK AREA                  
*     INPUT   P3=A(VALTAB)                                                      
*                                                                               
GETVAL   MVC   AVALTAB,8(R1)       THE ADDRESS OF VALTAB                        
         MVI   LINE,0                                                           
         LA    R2,DEMNAM1H                                                      
         ST    R2,THISLINE         INITIALIZE POINTER TO SCREEN LINE            
         MVC   NEWEVPH(LINES*4),OLDEVPH INITIALIZE NEW EST. VPHS AND            
         MVC   NEWERTG(LINES*4),OLDERTG RTGS, ONLY 1 OF 2 WILL BE INPUT         
         SPACE                                                                  
GT100    ZIC   R1,LINE                                                          
         LA    R1,1(R1)                                                         
         CLM   R1,1,DEMS           TEST IF ALL DEMOS DONE                       
         BH    GETVX               YES                                          
         STC   R1,LINE                                                          
         BCTR  R1,0                                                             
*                                                                               
         SLL   R1,2                                                             
         STH   R1,LDISP            DISPLACEMENT INTO EACH VALUE LIST            
         L     R3,AVALTAB                                                       
         USING VALTABD,R3                                                       
         SPACE                                                                  
GT200    ST    R3,SAVEREG3                                                      
         CLI   0(R3),X'FF'         TEST FOR E-O-T                               
         BE    GT1000                                                           
         ZIC   R2,VALFDISP                                                      
         A     R2,THISLINE         POINT TO FIELD HEADER                        
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    GT900               YES-NEXT FIELD                               
*                                                                               
         GOTO1 VGETFLD                                                          
         ICM   RE,7,VALNDISP                                                    
         LA    RE,TEMPD(RE)        POINT TO START OF LIST                       
         AH    RE,LDISP            DISPLACEMENT INTO LIST FOR DEMO              
         XC    0(4,RE),0(RE)       ZERO INPUT VALUE FIELD                       
         CLI   FLDH+5,0            TEST FOR NO INPUT                            
         BE    GT900               YES                                          
         CLI   FLD,C'*'            STRIP LEADING ASTERISK BEFORE SCAN           
         BNE   GT300                                                            
         ZIC   R1,FLDH+5                                                        
         SH    R1,=H'1'                                                         
         BZ    GT900               ONLY A STAR IN FIELD                         
         STC   R1,FLDH+5           ADJUSTED FIELD LENGTH                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+1        REMOVE PREFIX                                
         LA    RE,FLD                                                           
         AR    RE,R1                                                            
         MVI   0(RE),C' '          BLANK THE LAST FIELD OUT                     
         SPACE                                                                  
GT300    MVI   RATSW,NO            SET ESTIMATED RATING SWITCH                  
         CLI   FLD,C'R'                                                         
         BNE   GT400                                                            
         MVI   RATSW,YES                                                        
         TM    VALEDIT,X'10'       TEST IF 'R' PREFIX ALLOWED                   
         BZ    GETVERR             NO                                           
         SPACE                                                                  
         CLC   FLD(2),=C'RX'                                                    
         BNE   GT320                                                            
         CLI   FLDH+5,2                                                         
         BNE   GETVERR                                                          
         L     R0,=F'-1'           RESET OVERRIDE WITH REAL VALUES              
         B     GT800                                                            
         SPACE                                                                  
GT320    ZIC   R1,FLDH+5                                                        
         SH    R1,=H'1'                                                         
         BZ    GETVERR             NOTHING BUT 'R' IN FIELD                     
         STC   R1,FLDH+5                                                        
         LR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),FLD+1        REMOVE PREFIX                                
         LA    RE,FLD                                                           
         AR    RE,R1                                                            
         MVI   0(RE),C' '          BLANK THE LAST FIELD OUT                     
*--BUMP VALTAB TO NEXT RATING ENTRY                                             
GT340    CLI   VALMOD,C'R'                                                      
         BE    GT400                                                            
         LA    R3,VALTABL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   GT340                                                            
         DC    H'0'                                                             
         SPACE                                                                  
GT400    CLI   FLD,C'X'                                                         
         BNE   GT420                                                            
         CLI   FLDH+5,1                                                         
         BNE   GETVERR                                                          
         L     R0,=F'-1'           RESET OVERRIDE WITH REAL VALUES              
         B     GT800                                                            
         SPACE                                                                  
*--CHECK PRECISION FACTOR                                                       
GT420    LA    RE,DEMPREC                                                       
         LA    RF,7                                                             
GT430    CLC   VALMOD(1),0(RE)                                                  
         BE    GT440                                                            
         LA    RE,2(RE)                                                         
         BCT   RF,GT430                                                         
         DC    H'0'                                                             
GT440    TM    1(RE),X'82'                                                      
         BO    GT650                                                            
*--TENTH'S PLACE EDIT                                                           
         ZIC   R1,FLDH+5                                                        
         LA    R5,FLD                                                           
         CH    R1,=H'7'                                                         
         BH    GETVERR             NO MORE THAN 5 DIGITS                        
         SPACE                                                                  
GT500    CLI   0(R5),C'.'          TEST FOR DECIMAL POINT                       
         BE    GT600                                                            
         CLI   0(R5),C'0'                                                       
         BL    GETVERR                                                          
         CLI   0(R5),C'9'                                                       
         BH    GETVERR                                                          
         LA    R5,1(R5)            NEXT BYTE                                    
         BCT   R1,GT500                                                         
*                                                                               
         ZIC   R1,FLDH+5           FOUND A WHOLE NUMBER                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         TM    VALEDIT,X'02'       DECIMAL ALLOWED FOR NON ABC/CBS/NBC          
         BZ    *+12                NO                                           
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BE    GT520                                                            
         TM    VALEDIT,X'01'       TEST FOR DECIMAL POINT                       
         BO    *+12                                                             
         CLI   RATSW,YES                                                        
         BNE   *+8                                                              
GT520    MH    R0,=H'10'           SCALE TO TENTHS                              
         B     GT800               SLOT INTO OUTPUT                             
         SPACE                                                                  
GT600    CH    R1,=H'2'            ONLY 1 DIGIT AFTER POINT                     
         BNE   GETVERR                                                          
         CLI   1(R5),C'0'          TEST FOR NUMBER AFTER POINT                  
         BL    GETVERR                                                          
         CLI   1(R5),C'9'                                                       
         BH    GETVERR                                                          
         NI    1(R5),X'0F'         ISOLATE BINARY NUMBER                        
         ZIC   R0,1(R5)                                                         
         TM    VALEDIT,X'02'       DECIMAL ALLOWED FOR NON ABC/CBS/NBC          
         BZ    *+12                NO                                           
         CLI   NBHUNOPT,C'Y'       HUNDREDS OPTION                              
         BE    GT620                                                            
         CLI   RATSW,YES                                                        
         BE    *+12                                                             
         TM    VALEDIT,X'01'       TEST IF DECIMAL POINT ALLOWED                
         BZ    GETVERR             NO                                           
GT620    ZIC   R1,FLDH+5                                                        
         SH    R1,=H'3'            HANDLE THE WHOLE NUMBER                      
         BM    GT800               DECIMAL POINT STARTED FIELD                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
         MH    RE,=H'10'                                                        
         AR    R0,RE                                                            
         B     GT800                                                            
*--HUNDREDS PLACE EDIT                                                          
*                                                                               
GT650    ZIC   R1,FLDH+5                                                        
         LA    R5,FLD                                                           
         CH    R1,=H'8'                                                         
         BH    GETVERR             NO MORE THAN 5 DIGITS                        
GT670    CLI   0(R5),C'.'          TEST FOR DECIMAL POINT                       
         BE    GT680                                                            
         CLI   0(R5),C'0'          VALIDATE FOR NUMERIC DATA                    
         BL    GETVERR                                                          
         CLI   0(R5),C'9'                                                       
         BH    GETVERR                                                          
         LA    R5,1(R5)            NEXT BYTE                                    
         BCT   R1,GT670                                                         
         SPACE 1                                                                
*                                                                               
         ZIC   R1,FLDH+5           FOUND A WHOLE NUMBER                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   R0,DUB                                                           
         MH    R0,=H'100'          SCALE TO HUNDREDS                            
         B     GT800               SLOT INTO OUTPUT                             
         SPACE                                                                  
GT680    CH    R1,=H'3'            ONLY 1 DIGIT AFTER POINT                     
         BH    GETVERR                                                          
         LR    RF,R1               NUMBER OF PLACES AFTER DECIMAL               
         LR    R0,R5               POSITION OF DECIMAL                          
         BCTR  RF,0                                                             
*                                                                               
GT690    CLI   1(R5),C'0'          TEST FOR NUMBER AFTER POINT                  
         BL    GETVERR                                                          
         CLI   1(R5),C'9'                                                       
         BH    GETVERR                                                          
         LA    R5,1(R5)                                                         
         BCT   RF,GT690                                                         
*                                                                               
         LR    R5,R0               SET R5 AT DECIMAL POINT                      
         LR    RE,R1               GET NUMBER AFTER DEC POINT                   
         SH    RE,=H'2'                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,1(0,R5)                                                      
         CVB   R0,DUB                                                           
         CH    RE,=H'1'            CHECK HOW MANY AFTER DEC.                    
         BE    *+8                 IF TWO NO PROBLEM                            
         MH    R0,=H'10'           MULT BY 10 FOR ALIGNMENT                     
*                                                                               
         ZIC   RE,FLDH+5                                                        
         SR    RE,R1               HANDLE THE WHOLE NUMBER                      
         BCTR  RE,0                DECIMAL POINT STARTED FIELD                  
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
         MH    RE,=H'100'                                                       
         AR    R0,RE                                                            
         B     GT800                                                            
*                                                                               
         SPACE                                                                  
GT800    SR    R4,R4               SLOT OUTPUT VALUE                            
         ICM   R4,7,VALNDISP                                                    
         CLI   RATSW,YES                                                        
         BNE   *+8                                                              
         ICM   R4,7,=AL3(NEWERTG-TEMPD)                                         
         LA    R4,TEMPD(R4)                                                     
         AH    R4,LDISP                                                         
         ST    R0,0(R4)                                                         
         SPACE                                                                  
GT900    L     R3,SAVEREG3                                                      
         LA    R3,VALTABL(R3)      NEXT TABLE ENTRY                             
         B     GT200                                                            
         SPACE                                                                  
GT1000   L     R1,THISLINE         UPDATE LINE POINTER                          
         LA    R1,LINELEN(R1)                                                   
         ST    R1,THISLINE                                                      
         B     GT100                                                            
         SPACE                                                                  
GETVX    B     OVEXIT                                                           
         DROP  R3                                                               
         SPACE                                                                  
GETVERR  MVI   FERN,INVERR                                                      
         B     OVERROR                                                          
         EJECT                                                                  
**********************8                                                         
* SUB-ROUTINE TO DISPLAY PACKAGE AND/OR DEMO GUARANTIES                         
*                                                                               
DISGUAR  MVC   FLD,SPACES                                                       
         LA    R2,FLD+11           R2=OUTPUT POINTER                            
         OC    NBNGUFAC,NBNGUFAC   TEST FOR PACKAGE GUARANTEE                   
****     OC    NBGUAFAC,NBGUAFAC   TEST FOR PACKAGE GUARANTEE                   
         BZ    DISGUAR2            NO                                           
         MVC   FLD(11),=C'GUARANTEES-'                                          
         MVC   0(8,R2),=C'PACKAGE='                                             
         LA    R2,8(R2)            UPDATE OUTPUT POINTER                        
         SR    R0,R0                                                            
         ICM   R0,15,NBNGUFAC                                                   
***      ICM   R0,3,NBGUAFAC                                                    
         EDIT  (R0),(8,(R2)),4,ALIGN=LEFT                                       
***      EDIT  (R0),(6,(R2)),2,ALIGN=LEFT                                       
         AR    R2,R0                                                            
         LA    R2,1(R2)                                                         
*                                                                               
DISGUAR2 GOTO1 VHELLO,DMCB,(C'G',OVUNTFL),(X'B4',NBAIO),0                       
         CLI   12(R1),0            TEST IF DEMO GUARANTEE FOUND                 
         BNE   DISGUAR4            NO                                           
         MVC   FLD(11),=C'GUARANTEES-'                                          
         L     R3,12(R1)                                                        
         USING NUNDGD,R3                                                        
         XC    THREE,THREE                                                      
         MVI   THREE+1,C'T'                                                     
         MVC   THREE+2(1),NUNDGDEM+2    CATEGORY                                
***      MVC   THREE+2(1),NUGUACAT CATEGORY                                     
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 VDEMOCON,DMCB,THREE,(10,WORK),(C'S',DBLOCK),ESTUSNS              
         MVC   0(10,R2),WORK       EXTRACT CATEGORY NAME                        
*                                                                               
         LA    R1,9(R2)                                                         
         LA    R0,10                                                            
         CLI   0(R1),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         LA    R2,1(R1)            POINT PAST LAST SIGNIFICANT CHAR.            
         MVI   0(R2),C'='                                                       
         SR    R0,R0                                                            
         ICM   R0,15,NUNDGFAC                                                   
***      ICM   R0,3,NUGUAFAC                                                    
         EDIT  (R0),(8,1(R2)),4,ALIGN=LEFT                                      
*                                                                               
DISGUAR4 MVC   DEMGUAR,FLD                                                      
         B     OVEXIT                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************                                                          
*                                                                               
OVEXIT   XMOD1 1                                                                
         SPACE 2                                                                
* ERROR EXIT                                                                    
*                                                                               
OVERROR  GOTO1 VERROR                                                           
         SPACE 2                                                                
OVUNTFL  DC    CL8'UNTFILE'                                                     
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* DEMO SCREEN                                                                   
*                                                                               
         ORG   BUYLAST                                                          
       ++INCLUDE NEBUYFBD                                                       
         SPACE 2                                                                
* SAVE AREA                                                                     
*                                                                               
         ORG   TWAD+PAGELEN                                                     
SVAREA   DS    0XL80                                                            
SVDATE   DS    XL2                                                              
SVSUB    DS    X                                                                
SVLPAGE  DS    X                   LAST PAGE DISPLAYED                          
SVLACT   DS    X                   LAST ACTION                                  
SVNDEMS  DS    X                                                                
SVDEMOS  DS    XL60                                                             
         SPACE 2                                                                
* SCREEN EQUATES                                                                
*                                                                               
LINELEN  EQU   DEMNAM2H-DEMNAM1H                                                
LINES    EQU   (DEMGUARH-DEMNAM1H)/LINELEN                                      
EVWH     EQU   DEMEVWH-DEMNAM1H    DISP TO ESTIMATED VPH                        
EIMPH    EQU   DEMEIMPH-DEMNAM1H   DISP TO ESTIMATED IMPS                       
EUNIH    EQU   DEMEUNIH-DEMNAM1H   DISP TO ESTIMATED UNIVERSES                  
ERTGH    EQU   DEMERTGH-DEMNAM1H   DISP TO ESTIMATED RATING                     
AVWH     EQU   DEMAVWH-DEMNAM1H    DISP TO ACTUAL VPH                           
AIMPH    EQU   DEMAIMPH-DEMNAM1H   DISP TO ACTUAL IMPS                          
AUNIH    EQU   DEMAUNIH-DEMNAM1H   DISP TO ACTUAL UNIVERSES                     
ARTGH    EQU   DEMARTGH-DEMNAM1H   DISP TO ACTUAL RATING                        
         EJECT                                                                  
* DSECT TO COVER LOCAL WORKING STORAGE                                          
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
SAVEREG  DS    A                                                                
SAVEREG3 DS    A                                                                
SAVEREG4 DS    A                                                                
SAVEREGE DS    A                                                                
AVALTAB  DS    A                   ADDRESS OF VALTAB                            
VDISPLAY DS    V                   V(GENERAL UNIT DISPLAY)                      
VEDIT    DS    V                   V(GENERAL UNIT EDIT)                         
*                                                                               
DATE     DS    XL2                 DATE EXTRACTED FROM ACTION FIELD             
SUB      DS    X                   SUBLINE EXTRACTED FROM ACTION FIELD          
*                                                                               
DEMS     DS    X                   DEMOS FOR SCREEN                             
DEMOS    DS    (LINES)XL3          SCREEN DEMO LIST                             
DISP     DS    X                   DISPLACEMENT INDEX                           
LINE     DS    X                   SCREEN LINE INDEX                            
WEIGHT   DS    X                   WEIGHTED DEMO NUMBER                         
RATSW    DS    C                   RATING OVERRIDE IN ESTIMATED VPH Y/N         
ESTLOOK  DS    C                   FORCE LOOKUP OF ESTIMATED DEMOS (Y)          
MORESW   DS    C                   MORE DEMOS TO COME (Y)                       
*                                                                               
OVEREL   DS    XL20                OVERRIDE ELEMENT AREA                        
USERNMS  DS    CL28                USER DEMO NAMES                              
*                                                                               
THISLINE DS    A                   SCREEN LINE POINTER                          
LDISP    DS    H                   DISPLACEMENT INTO VALUE LIST                 
PNGUAHLD DS    F                   NEW PACKAGE GUARANTEE HOLD AREA              
DNGUAHLD DS    F                   NEW DEMO GUARANTEE HOLD AREA                 
PGUARHLD DS    H                   PACKAGE GUARANTEE HOLD AREA                  
DGUARHLD DS    H                   DEMO GUARANTEE HOLD AREA                     
*                                                                               
* DEMO VALUES LOOKED UP FROM RECORD                                             
*                                                                               
         DS    0F                                                               
OLDEVPH  DS    (LINES)F            OLD (RECORD) ESTIMATED VPH'S                 
HOMEIMP  DS    F                   OLD ESTIMATED HOMES IMPRESSION               
OLDEIMP  DS    (LINES)F            OLD ESTIMATED IMPRESSIONS                    
HOMEUNIV DS    F                   OLD ESTIMATED HOMES UNIVERSE                 
OLDEUNI  DS    (LINES)F            OLD ESTIMATED UNIVERSES                      
HOMERAT  DS    F                   OLD ESTIMATED HOMES RATING                   
OLDERTG  DS    (LINES)F            OLD ESTIMATED RATINGS                        
*                                                                               
OLDAVPH  DS    (LINES)F            OLD (RECORD) ACTUAL VPH'S                    
HOMAIMP  DS    F                   OLD ACTUAL HOMES IMPRESSION                  
OLDAIMP  DS    (LINES)F            OLD ACTUAL IMPRESSIONS                       
HOMAUNIV DS    F                   OLD ACTUAL HOMES UNIVERSE                    
OLDAUNI  DS    (LINES)F            OLD ACTUAL UNIVERSES                         
HOMARAT  DS    F                   OLD ACTUAL HOMES RATING                      
OLDARTG  DS    (LINES)F            OLD ACTUAL RATINGS                           
*                                                                               
* DEMO VALUES INPUT BY USER                                                     
*                                                                               
NEWEVPH  DS    (LINES)F            NEW ESTIMATED VPH LIST                       
NEWERTG  DS    (LINES)F            NEW ESTIMATED RATING LIST                    
NEWAVPH  DS    (LINES)F            NEW ACTUAL VPH LIST                          
NEWAIMP  DS    (LINES)F            NEW ACTUAL IMPRESSIONS                       
NEWARTG  DS    (LINES)F            NEW ACTUAL RATINGS                           
NEWARHM  DS    F                   NEW ACTUAL RATING HOMES                      
*                                                                               
OVERSTOR DS    D                   STORAGE FOR OVERIDE DEMO FILTER              
SVUSRUNV DS    4F                                                               
BLOCK    DS    CL256                                                            
         SPACE 2                                                                
* EQUATES                                                                       
*                                                                               
USERMOD  EQU   33                  USER DEMO MODIFIER CODE                      
PERIOD   EQU   C'.'                                                             
         EJECT                                                                  
* DSECT TO COVER DEMO VALUE TABLE                                               
*                                                                               
VALTABD  DSECT                                                                  
VALFDISP DS    X                   DISPLACEMENT INTO SCREEN LINE                
VALODISP DS    AL3                 DISPLACEMENT INTO OLD VALUE LIST             
VALNDISP DS    AL3                 DISPLACEMENT INTO NEW VALUE LIST             
VALOVER  DS    X                   OVERRIDE ELEMENT CODE                        
VALUSER  DS    X                   USER DEMO OVERRIDE CODE                      
VALMOD   DS    C                   MODIFIER CODE                                
VALDIS   DS    X                   DISPLAY CONTROL BITS                         
*                                  X'01' = OUTPUT HAS ONE DECIMAL PLACE         
*                                  X'02' = CABLE HAS ONE DECIMAL PLACE          
*                                  X'04' = USE PRECISION TABLE FOR EDIT         
VALEDIT  DS    X                   EDIT CONTROL BITS                            
*                                  X'10' = 'R' PREFIX ALLOWED IN FIELD          
*                                  X'01' = VALUE HAS ONE DECIMAL PLACE          
*                                  X'02' = CABLE HAS ONE DECIMAL PLACE          
*                                  X'04' = USE PRECISION TABLE FOR EDIT         
VALOVCTL DS    X                   OVERRIDE CONTROL BITS                        
*                                  X'80' = GENERATE OVERRIDE VPH                
*                                  X'40' = DON'T OVERRIDE EST IMP               
VALTABL  EQU   *-VALTABD                                                        
         SPACE 2                                                                
* NETDEMOD (DSECT COVERING NETWORK DEMO BLOCK)                                  
*                                                                               
         PRINT OFF                                                              
NETDEMOD DSECT                                                                  
       ++INCLUDE NETDEMOD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* NETUNIVD (DSECT COVERING NETWORK UNIVERSE BLOCK)                              
*                                                                               
         PRINT OFF                                                              
NETUNIVD DSECT                                                                  
       ++INCLUDE NETUNIVD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'200NEBUY13S  05/01/02'                                      
         END                                                                    
