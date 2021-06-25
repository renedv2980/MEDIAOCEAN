*          DATA SET SPTRA0B    AT LEVEL 064 AS OF 10/23/12                      
*PHASE T2160BA                                                                  
         TITLE 'T2160B STATION ADDRESS REC DISPLAY, CHANGE, ADD, DELETEC        
                , LIST'                                                         
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - READ CTFILE TWX RECS-OFFLINE LIST & DISPLAY REC            
*             AIO3 - IN LRR RTN, READ STAION MASTER REC                         
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG IN VFTR                                                  
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE                                                       
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
*                                                                               
* LEV 22 PRINT 4TH LINE OF ADDRESS                                              
* LEV 23 START WITH ANY STATION                                                 
* LEV 24 JUN23/86 ADD TWX ANSWERBACK, CLASS OF SERVICE, COMML TYPE              
* LEV 25 JUN26/86 CHA TWX ANSWERBACK, CLASS OF SERVICE, ON LISTS                
* LEV 26 JUL01/86 CK TWX/ANSWERBACK NOT MORE THAN 39 CHAR                       
* LEV 27 JUL01/86 ADD GRAPHNET CODE TO REPLACE TWX/ANSWERBACK                   
* LEV 28 SEP04/86 FIX 4TH LINE OF ADDRESS AGAIN                                 
* LEV 29 SEP11/86 BLANK UNUSED TWA FIELDS FOR OLD STA REC TYPES                 
* LEV 32-34 NOV17/86 ADD CML TYPE FILTER                                        
* LEV 35    DEC04/86 ADD CML TYPE C34 AND PUT CMML TYPES IN ORDER               
* LEV 36    JAN05/87 ADD TWX=YES FILTER                                         
* LEV 37    MAR10/87 FILL IN AGY ALPHA                                          
* LEV 39    SEP04/87 GET TWX, TWX ANSWERBACK, POSSIBLE TELEPHONE                
*                    TELEPHONE NUMBER FROM CONTROL FILE                         
* LEV 40-43 JAN19/88 DIS TWX, TWX ANSWERBACK, POSSIBLE TELEPHONE                
*                     CENTRAL FILE ONLY LIST                                    
* LEV 44-46 JAN27/88 PROTECT INPUT FIELDS FOR DISPLAYING STA FROM               
*                     CENTRAL FILE                                              
* LEV 47-48 FEB04/88 FIX BUG ON PROTECTED FIELDS, LIST CEN ONLINE               
* LEV 49-50 SEP12/88 BACK OUT CENTRAL FILE, ADD FX FOR GRAPH                    
* LEV 52    FEB25/92 ADD CABLE HEAD CODE                              *         
* LEV 53    JUN21/92 FIX LIST FOR CABLE - USE STANET IF PRESENT       *         
* LEV 54    JUN25/92 DROP F1 ELEM FIX, CABLE DOES NOT HAVE F1 ELEMS   *         
* LEV 55    OCT04/94 DEL SERVICE CLASS, DDS TWX CODE AND TWX ANSWERBACK         
*                    ADD AMS TAPE DEADLINE, GROUP CODE                *         
*                    FILTERS 'AMST'->TAPE DEADLINE, 'AMSG'->GROUP CODE*         
*                        AND 'AMS'->DISPLAY TAPE AND OR GROUP         *         
*                        (IF 'AMSG,AMST'->GROUP AND TAPE              *         
*                         GROUP FILTER EG. 'AMSG=GROUP C'             *         
* LEV 56    SEP10/96 EDIT FOR LOW POWER STATIONS                      *         
* LEV 57    AUG31/98 STOP DELETES FOR CABLE STATIONS                  *         
* LEV 58    NOV07/00 FIX EOR AND BAD F1 ELEM                          *         
* LEV 62 SM MAR27/03 ADD DDGENTWA                                     *         
* LEV 63 SM JUL29/04 SOX                                              *         
* LEV 64 MNAS OCT23/12 MORE BANDS                                     *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
T2160B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**STAD**,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
         OI    GENSTAT4,CONFDEL    CONFIRM DELETE                               
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   EXIT                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    DELREC                                                           
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    DELREC                                                           
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
EXIT     XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
         SPACE 3                                                                
VK       DS    0H                                                               
         CLI   ACTNUM,ACTDEL                                                    
         BE    VK01                                                             
         CLI   ACTNUM,ACTREST                                                   
         BNE   VK02                                                             
         SPACE                                                                  
VK01     CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VK02                                                             
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VK02                                                             
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VK02     DS    0H                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         LA    R2,TRASTAH          STATION                                      
         XC    QSTA,QSTA                                                        
         XC    STANET,STANET                                                    
         CLI   5(R2),0                                                          
         BNE   VK10                                                             
         CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BE    VK20                                                             
         B     VK14                                                             
         SPACE                                                                  
VK10     CLI   ACTNUM,ACTLIST      OPTIONAL FOR LIST                            
         BNE   VK18                                                             
         SPACE                                                                  
         ZIC   R1,5(R2)                                                         
         LR    RF,R1                                                            
         LA    RE,8(,R2)                                                        
         SPACE                                                                  
VK14     CLI   0(RE),C'0'                                                       
         BL    VK16                                                             
         LA    RE,1(,RE)                                                        
         BCT   RF,VK14                                                          
         SPACE                                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  STANET(4),DUB                                                    
         B     VK20                                                             
         SPACE                                                                  
VK16     MVC   QSTA,8(R2)                                                       
         B     VK20                                                             
         SPACE                                                                  
VK18     GOTO1 VALISTA                                                          
         SPACE                                                                  
         CLI   8(R2),C'0'          THIS CABLE STATION                           
         BL    *+12                 NO                                          
         CLI   ACTNUM,ACTDEL       NO DELETES FOR CABLE                         
         BE    CABDELER                                                         
         SPACE                                                                  
         CLI   QSTA+4,C' '                                                      
         BH    VK20                                                             
         MVC   QSTA+4(1),QMED                                                   
         SPACE                                                                  
VK20     LA    R2,TRAFLTRH                                                      
         BAS   RE,VFTR             VALIDATE FILTERS                             
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STADDKEY,R4                                                      
         MVC   STAKID,=XL2'0A28'                                                
         MVC   STAKAM,BAGYMD                                                    
         MVC   STAKSTA,QSTA                                                     
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE RECORD ROUTINE                                                       
         SPACE 2                                                                
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VRS01                                                            
         SPACE                                                                  
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VRS01                                                            
         SPACE                                                                  
         GOTO1 VSOXERR                                                          
         SPACE                                                                  
VRS01    DS    0H                                                               
         L     R4,AIO                                                           
         USING STADDKEY,R4                                                      
         MVC   QSTA,STAKSTA                                                     
         MVC   20(2,R4),AGENCY                                                  
         SPACE                                                                  
         CLI   ACTNUM,ACTADD       IF ADD, BYPASS                               
         BE    VR04                                                             
         SPACE                                                                  
         CLI   STAKSTA,X'F0'       BYPASS CABLE STATIONS WITH NO F1 ELM         
         BNL   VR04                                                             
         DROP  R4                                                               
         SPACE                                                                  
         SR    RF,RF                                                            
         ICM   RF,3,13(R4)                                                      
         LA    R1,0(R4,RF)                                                      
         AHI   R1,-4                                                            
         XC    0(4,R1),0(R1)                                                    
         SPACE                                                                  
VR04     MVI   ELCODE,X'10'        ADDRESS PART OF ELEMENT                      
         XC    ELEM,ELEM                                                        
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING STADTAEL,R6                                                      
         SPACE                                                                  
         MVI   STADTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   STADTALN,STADTAX-STADTAEL ELEMENT LENGTH                         
         SPACE                                                                  
         LA    R2,TRAAL1H          ADDRESS LINE 1                               
         SPACE                                                                  
         BAS   RE,VADR             VALIDATE ADDRESS LINE                        
         MVC   STALINE1,WORK                                                    
         SPACE                                                                  
         LA    R2,TRAAL2H          ADDRESS LINE 2                               
         SPACE                                                                  
         BAS   RE,VADR             VALIDATE ADDRESS LINE                        
         MVC   STALINE2,WORK                                                    
         SPACE                                                                  
         LA    R2,TRAAL3H          CITY                                         
         SPACE                                                                  
         BAS   RE,VADR             VALIDATE ADDRESS LINE                        
         MVC   STALINE3,WORK                                                    
         SPACE                                                                  
         LA    R2,TRAAL4H           STATE/ZIP                                   
         XC    STALINE4,STALINE4                                                
         CLI   5(R2),0             IF INPUT LENGTH ZERO                         
         BE    VR10                NOTHING ENTERED                              
         SPACE                                                                  
         BAS   RE,VADR             VALIDATE ADDRESS LINE                        
         MVC   STALINE4,WORK                                                    
         SPACE                                                                  
VR10     LA    R2,TRACMLTH         COMMERCIAL TYPE                              
         XC    STACMLT,STACMLT                                                  
         SPACE                                                                  
         BAS   RE,VTYP                                                          
         MVC   STACMLT,WORK                                                     
         SPACE                                                                  
VR20     LA    R2,TRATWXH          TWX NUMBER                                   
         XC    STATWX,STATWX                                                    
         CLI   5(R2),0             IF INPUT LENGTH ZERO                         
         BE    VR50                  NOTHING ENTERED                            
         GOTO1 ANY                                                              
         CLI   5(R2),20            LENGTH MUST BE 21 OR LESS                    
         BNH   *+12                                                             
         CLI   TRATWX+21,C' '      IF HIGHER THAN BLANK                         
         BH    TWXLENER              PROBLEM                                    
         MVC   STATWX,WORK                                                      
         SPACE                                                                  
VR50     GOTO1 ADDELEM                                                          
         SPACE                                                                  
         B     DR                                                               
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY RECORD                                                                
         SPACE 3                                                                
DR       L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING STADTAEL,R6                                                      
         MVC   WORK(L'TRAAL1),SPACES                                            
         MVC   WORK(L'STALINE1),STALINE1                                        
         CLC   TRAAL1,WORK         ADDRESS LINE1                                
         BE    *+14                                                             
         MVC   TRAAL1,WORK                                                      
         OI    TRAAL1H+6,X'80'                                                  
         MVC   WORK(L'TRAAL2),SPACES                                            
         MVC   WORK(L'STALINE2),STALINE2                                        
         CLC   TRAAL2,WORK         ADDRESS LINE 2                               
         BE    *+14                                                             
         MVC   TRAAL2,WORK                                                      
         OI    TRAAL2H+6,X'80'                                                  
         MVC   WORK(L'TRAAL3),SPACES                                            
         MVC   WORK(L'STALINE3),STALINE3                                        
         CLC   TRAAL3,WORK         CITY                                         
         BE    *+14                                                             
         MVC   TRAAL3,WORK                                                      
         OI    TRAAL3H+6,X'80'                                                  
         SPACE                                                                  
         MVC   WORK(L'TRAAL4),SPACES                                            
         MVC   WORK(L'STALINE4),STALINE4                                        
         CLC   TRAAL4,WORK         STATE                                        
         BE    *+14                                                             
         MVC   TRAAL4,WORK                                                      
         OI    TRAAL4H+6,X'80'                                                  
         SPACE                                                                  
         MVC   WORK(L'TRATWX),SPACES                                            
         MVC   WORK(L'STATWX),STATWX                                            
         CLC   TRATWX,WORK         STATE                                        
         BE    *+14                                                             
         MVC   TRATWX,WORK                                                      
         OI    TRATWXH+6,X'80'                                                  
         SPACE                                                                  
* IF OLD RECORD, DONE DISPLAY *                                                 
         SPACE                                                                  
         CLI   STADTALN,119       THIS AN OLD ELEMENT                           
         BE    DR20                                                             
         SPACE                                                                  
         MVC   WORK(L'TRACMLT),SPACES                                           
         MVC   WORK(L'STACMLT),STACMLT                                          
         CLC   TRACMLT,WORK            COMML TYPE                               
         BE    *+14                                                             
         MVC   TRACMLT,WORK                                                     
         OI    TRACMLTH+6,X'80'                                                 
         SPACE                                                                  
         SPACE                                                                  
         SPACE                                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        AMS DATA ELEM                                
         BAS   RE,GETEL                                                         
         BNE   DR5                 ELEM NOT FOUND, ZERO INTENSITY               
*                                                                               
         USING STAAMSEL,R6                                                      
         MVC   WORK(L'TRAAMSD),SPACES                                           
         MVC   WORK(L'STAAMSTD),STAAMSTD                                        
         CLC   TRAAMSD,WORK             AMS TAPE DEADLINE                       
         BE    *+18                                                             
         MVC   TRAAMSD,WORK                                                     
         NI    TRAAMSDH+6,X'FF'-X'0C' NORMAL INTENSITY                          
         OI    TRAAMSDH+6,X'80'    TRANSMIT                                     
         NI    TRAAMSTH+6,X'FF'-X'0C'                                           
         OI    TRAAMSTH+6,X'80'                                                 
         SPACE                                                                  
         MVC   WORK(L'TRAAMSC),SPACES                                           
         MVC   WORK(L'STAAMSGC),STAAMSGC                                        
         CLC   TRAAMSC,WORK        AMS GROUP CODE                               
         BE    *+18                                                             
         MVC   TRAAMSC,WORK                                                     
         NI    TRAAMSCH+6,X'FF'-X'0C' NORMAL INTENSITY                          
         OI    TRAAMSCH+6,X'80'    TRANSMIT                                     
         NI    TRAAMSGH+6,X'FF'-X'0C'                                           
         OI    TRAAMSGH+6,X'80'                                                 
         B     DR10                                                             
*                                                                               
DR5      OI    TRAAMSTH+6,X'0C'    ZERO INTENSITY - AMS TAPE DEADLINE           
         OI    TRAAMSTH+6,X'80'    TRANSMIT                                     
*                                                                               
         OI    TRAAMSDH+6,X'0C'                                                 
         OI    TRAAMSDH+6,X'80'                                                 
*                                                                               
         OI    TRAAMSGH+6,X'0C'    ZERO INTENSITY - AMS GROUP CODE              
         OI    TRAAMSGH+6,X'80'    TRANSMIT                                     
*                                                                               
         OI    TRAAMSCH+6,X'0C'                                                 
         OI    TRAAMSCH+6,X'80'                                                 
*                                                                               
*                                                                               
DR10     B     EXIT                                                             
         SPACE                                                                  
DR20     OC    TRACMLT,TRACMLT         COMML TYPE                               
         BZ    *+24                                                             
         CLC   TRACMLT,SPACES          COMML TYPE                               
         BE    *+14                                                             
         MVC   TRACMLT,SPACES                                                   
         OI    TRACMLTH+6,X'80'                                                 
         SPACE                                                                  
         OC    TRATWX,TRATWX       TWX                                          
         BZ    *+24                                                             
         CLC   TRATWX,SPACES       TWX                                          
         BE    *+14                                                             
         MVC   TRATWX,SPACES                                                    
         OI    TRATWXH+6,X'80'                                                  
         SPACE                                                                  
         SPACE                                                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
         SPACE 3                                                                
DK       L     R4,AIO                                                           
         USING STADDKEY,R4                                                      
         MVC   WORK(L'TRASTA),SPACES                                            
         MVC   WORK(4),STAKSTA                                                  
         LA    R1,WORK+4                                                        
         CLI   WORK+3,C' '                                                      
         BH    DK10                                                             
         BCTR  R1,0                                                             
DK10     CLI   STAKSTA+4,C'T'                                                   
         BE    DK12                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),STAKSTA+4                                                
         CLI   STAKSTA+4,C'X'                                                   
         BE    DK12                                                             
         MVI   2(R1),C'M'                                                       
         SPACE                                                                  
DK12     DS   0H                                                                
         CLI   WORK,C'0'           THIS CABLE STATION                           
         BL    *+12                 NO                                          
         CLI   ACTNUM,ACTDEL       NO DELETES FOR CABLE                         
         BE    CABDELER                                                         
         SPACE                                                                  
         CLC   TRASTA,WORK                                                      
         BE    DK14                                                             
         MVC   TRASTA,WORK         MOVE IN CALL LETTERS                         
         OI    TRASTAH+6,X'80'     SET ON TRANSMIT BIT                          
DK14     DS    0H                                                               
         MVC   QSTA,STAKSTA                                                     
         MVC   STAPRNT,WORK                                                     
         B     EXIT                                                             
         DROP  R4                                                               
         SPACE 3                                                                
* CHECK FOR DELETE HERE, NOT OKAY FOR CABLE HEAD STATIONS                       
         SPACE                                                                  
DELREC   CLI   TRASTA,C'0'                                                      
         BL    EXIT                                                             
         B     CABDELER                                                         
         EJECT                                                                  
* ONLINE LIST OR OFFLINE REPORT ROUTINE                                         
         SPACE                                                                  
LR       LA    R4,KEY                                                           
         USING STADDKEY,R4                                                      
         OC    KEY(13),KEY         IS KEY ZERO                                  
         BNZ   LR10                NO, GOTO HIGH                                
         LA    R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         ZAP   STATCTR,=P'0'       FORMAT STATION COUNTER                       
         SPACE                                                                  
* BUILD KEY, AND DO READHI                                                      
         SPACE                                                                  
         MVC   STAKID(2),=XL2'0A28'                                             
         MVC   STAKAM,BAGYMD                                                    
         MVC   STAKSTA,QSTA        START WITH VALIDATED STA IF ANY              
         SPACE                                                                  
         OC    STANET,STANET       IS THIS CABLE                                
         BZ    LR10                                                             
         MVC   STAKSTA(4),STANET                                                
         MVI   STAKSTA+4,C'T'                                                   
         SPACE                                                                  
LR10     GOTO1 HIGH                GO DO DATAMGR READ HI                        
         CLC   KEYSAVE(3),KEY      WERE THERE ANY RECS FOR THIS AGENCY          
         BE    LR22                                                             
         SPACE                                                                  
LR16     CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   EXIT                                                             
         MVC   P(21),=CL21'NO STATION RECS FOUND'                               
         GOTO1 SPOOL,DMCB,(R8)     NO RECORDS AT ALL                            
         B     EXIT                                                             
         SPACE                                                                  
LR20     GOTO1 SEQ                 DO READ SEQUENTIAL                           
         SPACE                                                                  
LR22     CLC   KEY(3),KEYSAVE      AT END OF THIS AGENCY/TYPE                   
         BNE   LREND               YES                                          
         MVC   SVKEY(2),=XL2'0A28'                                              
         MVC   SVKEY+2(1),BAGYMD                                                
         CLC   SVKEY(3),KEY                                                     
         BH    LR20                                                             
         BL    EXIT                                                             
LR30     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         CLI   FTRSWAMS,C'Y'                                                    
         BNE   LR40                                                             
         MVI   ELCODE,X'20'        AMS DATA ELEM                                
         BAS   RE,GETEL                                                         
         BNE   LR20                                                             
         SPACE                                                                  
         BAS   RE,FTR               FILTER RECORDS                              
         BNE   LR20                                                             
LR40     LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE ADDRESS ELEMENT                    
         USING STADTAEL,R6                                                      
         SPACE                                                                  
         BAS   RE,FTR                FILTER RECORDS                             
         BNE   LR20                                                             
         SPACE                                                                  
LR50     CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LRL                 GO DO ONLINE LIST                            
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LRR                 GO FORMAT FOR OFFLINE REPORT                 
         DC    H'0'                MUST BE ON/OFFLINE                           
LREND    CLI   MODE,PRINTREP       IF OFFLINE                                   
         BNE   EXIT                                                             
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         MVC   P+9(24),=CL24'TRAFFIC STATIONS PRINTED'                          
         EDIT (P3,STATCTR),(5,P+3)                                              
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         B     EXIT                                                             
         EJECT                                                                  
* FORMAT ONLINE LIST HERE                                                       
         SPACE                                                                  
LRL      LA    R5,LISTAR           ADDRESS OF WORK AREA                         
         MVC   LISTAR,SPACES                                                    
         USING LSTLINE,R5                                                       
         MVC   QSTA,STAKSTA                                                     
         BAS   RE,FSTA             FORMAT STATION                               
         MVC   LSTA,STAPRNT                                                     
         MVC   LLINE1,STALINE1     ADDRESS LINE 1                               
         MVC   LLINE2,STALINE2     CITY                                         
         MVC   LLINE3,STALINE3     STATE                                        
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
         B     LR20                                                             
         SPACE 3                                                                
* FORMAT OFFLINE REPORT HERE                                                    
         SPACE                                                                  
LRR      LA    R5,P                PRINT LINE ADDRESS                           
         MVC   P,SPACES                                                         
         USING PRTLINE,R5                                                       
LRR06    AP    STATCTR,=P'1'       ADD TO CT OF STATIONS PRINTED                
         MVC   QSTA,STAKSTA                                                     
         BAS   RE,FSTA             GO FORMAT STATION                            
         MVC   PSTA,STAPRNT                                                     
         MVC   PALINE1,STALINE1                                                 
         MVC   PALINE2,STALINE2                                                 
         MVC   PALINE3,STALINE3                                                 
         SPACE                                                                  
         MVC   PALINE3+132,STALINE4                                             
         SPACE                                                                  
         CLI   STADTALN,119        THIS AN OLD ELEMENT                          
         BE    LRR40                                                            
         SPACE                                                                  
         MVC   PCMLTYP,STACMLT                                                  
         SPACE                                                                  
         OC    STATWX,STATWX     IF NO TWX NUMBER, GET TWX ID                   
         BZ    LRR20                                                            
         LA    R5,132(,R5)                                                      
         MVC   PTWX-4(4),=C'TWX='                                               
         MVC   PTWX(20),STATWX                                                  
         SPACE                                                                  
LRR20    OC    STATWXAB,STATWXAB  TWX ANSWERBACK PRESENT                        
         BZ    LRR30                                                            
         LA    R5,132(,R5)                                                      
         MVC   PTWX-4(4),=C'ANS='                                               
         MVC   PTWX(20),STATWXAB                                                
         SPACE                                                                  
LRR30    GOTO1 SPOOL,DMCB,(R8)     PRT TWX OR BLK LINE                          
         LA    R5,P                                                             
         MVC   P,SPACES                                                         
         SPACE                                                                  
         LR    R6,R4               POINT TO AIO                                 
         MVI   ELCODE,X'20'        AMS DATA ELEM                                
         BAS   RE,GETEL                                                         
         BNE   LRR40                                                            
         USING STAAMSEL,R6                                                      
         CLC   STAAMSTD,SPACES                                                  
         BNH   LRR38                                                            
         MVC   PAMSD(18),=CL18'AMS TAPE DEADLINE='                              
         MVC   PAMSD+18(50),STAAMSTD                                            
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)     PRT AMS TAPE DEADLINE                        
         LA    R5,P                                                             
         MVC   P,SPACES                                                         
         SPACE                                                                  
LRR38    CLC   STAAMSGC,SPACES                                                  
         BNH   LRR40                                                            
         MVC   PAMSD(15),=CL18'AMS GROUP CODE='                                 
         MVC   PAMSD+15(15),STAAMSGC                                            
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)     PRT AMS GROUP CODE                           
         LA    R5,P                                                             
         MVC   P,SPACES                                                         
         SPACE                                                                  
LRR40    MVI   SPACING,2                                                        
         OC    P2,SPACES                                                        
         OC    P3,SPACES                                                        
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)     PRT TWX OR BLK LINE                          
         SPACE                                                                  
* READ STATION MASTER RECORD *                                                  
         SPACE                                                                  
         MVI   SVKEY,C'0'                                                       
         MVC   SVKEY+1(16),SVKEY                                                
         MVI   SVKEY,C'S'                                                       
         MVC   SVKEY+1(1),QMED                                                  
         MVC   SVKEY+2(5),STAKSTA                                               
         MVC   SVKEY+7(2),AGENCY                                                
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',SVKEY,AIO3                   
         CLI   8(R1),0                                                          
         BE    LR20                                                             
         MVC   P+3(7),STAPRNT                                                   
         MVC   P+12(19),=CL19'NOT ON STATION FILE'                              
         MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     LR20                                                             
         EJECT                                                                  
* VALIDATE FILTER ROUTINES - CMML TYPE *                                        
         SPACE                                                                  
         DS    0H                                                               
VFTR     NTR1                                                                   
         MVI   FTRSWAMS,C'N'       INIT AMS FILTER SWITCH                       
         MVI   AMSG,C'N'                AND GROUP                               
         MVI   AMST,C'N'                AND TAPE                                
         MVI   AMS,C'N'                 BOTH                                    
         XC    FILTERS,FILTERS                                                  
         XC    AMSGLEN,AMSGLEN                                                  
         XC    AMSGSAVE,AMSGSAVE                                                
         CLI   5(R2),0             ANY ENTRY                                    
         BE    EXIT                NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTR06              YES                                          
         CLI   5(R2),4                                                          
         BNH   VFTR02                                                           
         LA    R1,4                                                             
         B     VFTR04                                                           
VFTR02   ZIC   R1,5(R2)                                                         
VFTR04   EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=CL4'HELP'                                               
         BNE   VFTR08                                                           
VFTR06   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(FTRHELPL),FTRHELP                                        
         B     ERREXIT                                                          
VFTR08   GOTO1 SCANNER,DMCB,TRAFLTRH,(5,BLOCK)                                  
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERR             NO                                           
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
VFTR10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         EX    R1,VFTRCLCA         CMML TYPE                                    
         BNE   VFTR20                                                           
         MVC   WORK(12),22(R4)                                                  
         CLC   =C'ALL',22(R4)      IS TYPE=ALL                                  
         BE    VFTR14              YES, SAVE IT                                 
         BAS   RE,VTYPF                                                         
VFTR14   MVC   CTYPFTR,WORK                                                     
         B     VFTR80                                                           
         SPACE                                                                  
VFTR20   EX    R1,VFTRCLCB         TWX                                          
         BNE   VFTR30                                                           
         OI    FTRSW,TWXFTR                                                     
         B     VFTR80                                                           
         SPACE                                                                  
VFTR30   EX    R1,VFTRCLCT         'AMST' FOR TAPE DEADLINE                     
         BNE   VFTR40                                                           
         MVI   FTRSWAMS,C'Y'                                                    
         MVI   AMST,C'Y'           FILTER ON TAPE                               
         CLC   12(4,R4),=CL4'AMS'  IF 'AMS' THEN                                
         BNE   *+12                                                             
         MVI   AMSG,C'Y'                                                        
         MVI   AMS,C'Y'            FILTER ON BOTH GROUP AND TAPE                
         B     VFTR80                                                           
         SPACE                                                                  
VFTR40   EX    R1,VFTRCLCG         'AMSG' GROUP CODE                            
         BNE   VFTR90                                                           
         ZIC   R1,1(R4)            WAS '=GROUP' ENTERED                         
         LTR   R1,R1                                                            
         BZ    VFTR40D             NO                                           
         STC   R1,AMSGLEN          SAVE LENGTH OF GROUP FILTER                  
         BCTR  R1,0                                                             
         EX    R1,MVCAMSG          SAVE GROUP FILTER                            
VFTR40D  MVI   FTRSWAMS,C'Y'                                                    
         MVI   AMSG,C'Y'           FILTER ON GROUP                              
         B     VFTR80                                                           
         SPACE                                                                  
VFTR80   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
         B     EXIT                                                             
VFTR90   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(FTRMSGL),FTRMSG                                          
         B     ERREXIT                                                          
VFTRCLCA CLC   12(0,R4),=CL5'TYPE'                                              
VFTRCLCB CLC   12(0,R4),=CL4'TWX'                                               
VFTRCLCT CLC   12(0,R4),=CL5'AMST'                                              
VFTRCLCG CLC   12(0,R4),=CL5'AMSG'                                              
         SPACE                                                                  
MVCAMSG  MVC   AMSGSAVE(0),22(R4)                                               
         EJECT                                                                  
* FILTER CMMLS FOR LIST FUNCTION                                                
         SPACE                                                                  
         DS    0H                                                               
FTR      NTR1                                                                   
         SPACE                                                                  
         CLI   ELCODE,X'20'                                                     
         BNE   FTR05                                                            
         USING STAAMSEL,R6                                                      
         CLI   AMST,C'Y'                                                        
         BNE   *+22                                                             
         CLC   STAAMSTD,SPACES                                                  
         BNH   FTRTNO                                                           
         CLI   AMS,C'Y'            SHOW IF AMST OR AMSG IS PRESENT              
         BE    FTREQ                                                            
FTR03    CLI   AMSG,C'Y'                                                        
         BNE   FTRGNO                                                           
         CLC   STAAMSGC,SPACES                                                  
         BNH   FTRNO                                                            
         OC    AMSGLEN,AMSGLEN                                                  
         BZ    FTR03D                                                           
         ZIC   R1,AMSGLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   STAAMSGC(0),AMSGSAVE                                             
         BNE   FTRNO                                                            
FTR03D   B     FTREQ                                                            
         SPACE                                                                  
FTR05    OC    FILTERS,FILTERS                                                  
         BZ    FTREQ                                                            
         SPACE                                                                  
         USING STADTAEL,R6                                                      
         CLI   STADTALN,119        OLD ELEM                                     
         BE    FTRNO                                                            
         SPACE                                                                  
         OC    CTYPFTR,CTYPFTR     TYPE FILTER                                  
         BZ    FTR20                                                            
         CLC   =CL3'ALL',CTYPFTR                                                
         BNE   FTR10                                                            
         OC    STACMLT,STACMLT     ANY CMML TYPE                                
         BZ    FTRNO                                                            
         B     FTR20                                                            
         SPACE                                                                  
FTR10    CLC   CTYPFTR,STACMLT    THIS IT                                       
         BNE   FTRNO                                                            
         SPACE                                                                  
FTR20    TM    FTRSW,TWXFTR        ONLY STATIONS WITH TWX                       
         BZ    FTREQ                                                            
         OC    STATWX,STATWX                                                    
         BNZ   FTREQ                                                            
         OC    STATWXAB,STATWXAB                                                
         BNZ   FTREQ                                                            
         OC    STATWXGC,STATWXGC                                                
         BZ    FTRNO                                                            
         SPACE                                                                  
FTREQ    CR    R1,R1               SET COND CODE FILTERED OK                    
         B     EXIT                                                             
         SPACE                                                                  
FTRNO    CR    RB,RD               SET COND CODE NO FILTER                      
         B     EXIT                                                             
         SPACE                                                                  
FTRTNO   CLI   AMS,C'Y'                                                         
         BE    FTR03                                                            
         B     FTRNO                                                            
         SPACE                                                                  
FTRGNO   CLI   AMST,C'Y'                                                        
         BNE   FTRNO                                                            
         B     FTREQ                                                            
                                                                                
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE ADDRESS LINE *                                                       
         SPACE                                                                  
VADR     NTR1                                                                   
         XC    WORK,WORK                                                        
         GOTO1 ANY                 MOVES DATA LEFT JUSTIFIED INTO WORK          
         CLI   5(R2),24            LENGTH MUST BE 24 OR LESS                    
         BNH   *+12                                                             
         CLI   8+24(R2),C' '      IF HIGHER THAN BLANK                          
         BH    LINLENER            PROBLEM                                      
         B     EXIT                                                             
         SPACE 3                                                                
* VALIDATE COMMERCIAL TYPE                                                      
         SPACE                                                                  
         DS    0H                                                               
VTYP     NTR1                                                                   
         XC    WORK,WORK                                                        
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
         GOTO1 ANY                                                              
         B     VTYP04                                                           
         SPACE                                                                  
VTYPF    NTR1                                                                   
VTYP04   CLI   WORK+3,C' '                                                      
         BH    TYPLENER                                                         
         LA    R0,CTYPTBCT                                                      
         LA    R1,CTYPTABL                                                      
VTYP10   CLC   WORK(3),0(R1)                                                    
         BE    EXIT                                                             
         LA    R1,3(,R1)                                                        
         BCT   R0,VTYP10                                                        
         B     VTYPER                                                           
         EJECT                                                                  
* FORMAT STATION FOR PRINTING                                                   
         SPACE                                                                  
FSTA     NTR1                                                                   
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),QSTA                                                  
         LA    R1,STAPRNT+4                                                     
         CLI   STAPRNT+3,C' '                                                   
         BH    FSTA10                                                           
         BCTR  R1,0                                                             
FSTA10   MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),QSTA+4                                                   
         CLI   QSTA+4,C'L'         LOW POWER TV STATION                         
         BE    FSTA20                                                           
         CLI   QSTA+4,C'X'         NETWORK RADIO                                
         BE    FSTA20                                                           
         MVI   2(R1),C'V'                                                       
*MNMB                                                                           
         CLI   QSTA+4,C'D'                                                      
         BE    FSTA20                                                           
*MNMB                                                                           
         CLI   QSTA+4,C'T'                                                      
         BE    FSTA20                                                           
         MVI   2(R1),C'M'                                                       
FSTA20   B     EXIT                                                             
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
TWXLENER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TWXLENMS),TWXLENMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
TYPLENER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TYPLENMS),TYPLENMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
LINLENER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'LINLENMS),LINLENMS                                     
         B     ERREXIT                                                          
         SPACE                                                                  
         LA    R2,TRASTAH                                                       
CABDELER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CABDELMS),CABDELMS                                     
         SPACE                                                                  
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
VTYPER   MVI   ERROR,INVTYPE                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
         SPACE                                                                  
LINLENMS DC    C'** ERROR ** LINE CAN NOT BE MORE THAN 24 CHARACTERS *'         
CABDELMS DC    C'** ERROR ** CABLE STATION CAN''T BE DELETED *'                 
TWXLENMS DC    C'** ERROR ** TWX CAN NOT BE MORE THAN 20 CHARACTERS *'          
TYPLENMS DC    C'** ERROR ** TYPE CAN NOT BE MORE THAN 4 CHARACTERS *'          
TWXABMS  DC    C'** ERROR ** TWX/ANSWERBACK MORE THAN 39 CHARACTERS *'          
FTRMSG   DC    C'* ERROR *'                                                     
FTRHELP  DC    C'VALID FILTERS - TYPE= *'                                       
FTRHELPL EQU   *-FTRHELP                                                        
FTRMSGL  EQU   *-FTRMSG                                                         
         EJECT                                                                  
       ++INCLUDE SPTRCMLTYP                                                     
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H2,3,C'______________'                                           
         SSPEC H5,3,PAGE                                                        
         SSPEC H1,35,C'STATION ADDRESS LIST'                                    
         SSPEC H2,35,C'____________________'                                    
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,73,AGYADD                                                     
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,3,C'STATION'                                                  
         SSPEC H9,3,C'-------'                                                  
         SSPEC H8,13,C'STATION DESCRIPTION.....'                                
         SSPEC H8,38,C'ADDRESS DETAILS'                                         
         SSPEC H9,38,C'---------------------------------------------'           
         SSPEC H9,83,C'----'                                                    
         SSPEC H8,93,C'COMMERCIAL'                                              
         SSPEC H9,96,C'TYPE'                                                    
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
PRTLINE  DSECT                                                                  
         DS    CL2                                                              
PSTA     DS    CL7                                                              
         DS    C                                                                
PFILTYP  DS    C                                                                
         DS    C                                                                
PTWX     DS    0CL20                                                            
PALINE1  DS    CL24                                                             
         DS    C                                                                
PALINE2  DS    CL24                                                             
         DS    C                                                                
PALINE3  DS    CL24                                                             
         DS    CL8                                                              
PCMLTYP  DS    CL4                                                              
         ORG   PSTA+6                                                           
PAMSD    DS    CL68                                                             
         ORG   PSTA+6                                                           
PAMSC    DS    CL30                                                             
LSTLINE  DSECT                                                                  
LSTA     DS    CL7                                                              
         DS    CL1                                                              
LLINE1   DS    CL24                                                             
         DS    C                                                                
LLINE2   DS    CL24                                                             
         DS    C                                                                
LLINE3   DS    CL14                                                             
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAFBD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
STATCTR  DS    PL3                                                              
FILTERS  DS    0CL5                                                             
CTYPFTR  DS    CL4                                                              
FTRSW    DS    XL1                                                              
TWXFTR   EQU   X'80'                                                            
FTRSWAMS DS    CL1                                                              
AMST     DS    CL1                                                              
AMSG     DS    CL1                                                              
AMS      DS    CL1                                                              
AMSGLEN  DS    XL1                                                              
AMSGSAVE DS    CL15                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064SPTRA0B   10/23/12'                                      
         END                                                                    
