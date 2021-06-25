*          DATA SET SPSFM83    AT LEVEL 251 AS OF 11/07/18                      
*PHASE T21783C                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21783  -- DEMOGRAPHICS MENU RECORD MAINTENANCE      *         
*                                   AND LIST                          *         
*  COMMENTS:     MAINTAINS DEMOGRAPHICS MENU RECORDS                  *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SPSFM42 (MAINT) & SPSFM43 (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21783 - DEMOGRAPHICS MENU MAINTENANCE AND LIST'                
T21783   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1783**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         MVC   DMCB+4(4),=X'D9000AD9'   DEMOVAL                                 
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOVAL,DMCB                                                    
*                                                                               
         MVC   DMCB+4(4),=X'D9000AE0'   DEMOCON                                 
         GOTO1 CALLOV,DMCB,0                                                    
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDEMOCON,DMCB                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                  LIST RECORDS                                 
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0X                                                               
*                                                                               
         XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
*                                                                               
         MVC   DMNMEDN,SPACES      CLEAR MEDIA NAME                             
         OI    DMNMEDNH+6,X'80'                                                 
*                                                                               
         LA    R4,SVKEY                                                         
         USING DMNRECD,R4                                                       
         MVC   DMNKTYP,=XL2'0D26'   MOVE RECORD TYPE X'0D26' TO KEY             
*                                                                               
         LA    R2,DMNMEDKH                                                      
         MVI   USEIONUM,3          USING AIO3 FOR TEMPORARY STORAGE...          
         GOTO1 VALIMED             ...FOR THE VALIDATION SUBROUTINES            
         MVC   AIO,AIO1                                                         
         MVC   DMNKAGMD,BAGYMD     MOVE 1 BYTE BINARY AGENCY/MEDIA CODE         
         MVC   DMNMEDN(L'MEDNM),MEDNM   DISPLAY MEDIA NAME                      
         OI    DMNMEDNH+6,X'80'    TRANSMIT                                     
*---------------------------------------------------------------------*         
*        NON DDS TERMINALS MAY UPDATE MEDIA T RECORDS, BUT NOT        *         
*                    MEDIA C AND N RECORDS (CANADA)                   *         
*---------------------------------------------------------------------*         
         CLI   SVAPROF+7,C'C'      IF CANADIAN                                  
         BNE   VK07                 NOPE, SKIP CHECK                            
         CLI   T217FFD+1,C'*'      TEST DDS TERMINAL?                           
         BE    VK07                 YUP, SKIP MEDIA CHECK                       
         CLI   ACTNUM,ACTDIS                                                    
         BE    VK07                                                             
         CLI   ACTNUM,ACTLIST      NOT LIST OR DISP?.....                       
         BE    VK07                                                             
         CLI   ACTNUM,ACTSEL       COULD BE SELECT FROM LIST...                 
         BNE   VK05                 NO?  MUST BE UPDATIVE, CHECK MEDIA          
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BNE   VK07                 NO, SKIP MEDIA CHECK FOR C AND N            
VK05     CLI   QMED,C'C'           IS THE MEDIA 'C'?                            
         BE    ERRNOTDI             YES, ONLY DISPLAY ALLOWED                   
         CLI   QMED,C'N'           IS THE MEDIA 'N'?                            
         BE    ERRNOTDI             YES, ONLY DISPLAY ALLOWED                   
*                                                                               
VK07     LA    R2,DMNMENUH                                                      
         CLI   DMNMENUH+5,0        ANY MENU INPUT?                              
         BNE   VK10                 YES, MOVE THE MENU TO KEY FIELD             
         CLI   ACTNUM,ACTLIST                                                   
         BNE   ERRMIS                                                           
         B     VKX                                                              
VK10     MVC   DMNKCODE,DMNMENU                                                 
         OC    DMNKCODE,SPACES     SPACE IS NEEDED INSTEAD OF NULLS             
*                                                                               
         DROP  R4                                                               
*                                                                               
VKX      XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0X                                                               
         XC    FLAG,FLAG                                                        
*                                                                               
         CLI   SVAPROF+7,C'C'      IF CANADIAN                                  
         BNE   VR10                                                             
         CLI   T217FFD+1,C'*'      TEST DDS TERMINAL?                           
         BE    VR10                                                             
         CLI   QMED,C'C'           IS THE MEDIA 'C'?                            
         BE    ERRNOTDI             YES, ONLY DISPLAY ALLOWED                   
         CLI   QMED,C'N'           IS THE MEDIA 'N'?                            
         BE    ERRNOTDI             YES, ONLY DISPLAY ALLOWED                   
*                                                                               
*  THE ABOVE 4 LINES OF CODE EXISTS FOR THE CASE OF CLIENTS RUNNING             
*   DISPLAY RIGHT BEFORE A CHANGE, SINCE GENCON SKIPS VALKEY IF THE KEY         
*   IS THE SAME                                                                 
*                                                                               
         XC    FLAG,FLAG                                                        
*                                                                               
VR10     MVI   ELCODE,X'01'        THE ACTIVITY DATE ELEMENT                    
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING DMNEL01,R6                                                       
         XC    ELEM,ELEM                                                        
         MVC   DMNEL01(2),=X'0105'   MOVE IN ELEMENT CODE AND LENGTH            
         GOTO1 DATCON,DMCB,(5,0),(3,DUB)                                        
         MVC   2(3,R6),DUB         SET THE ACTIVITY DATE - YYMMDD               
         GOTO1 ADDELEM             ADD ACTIVITY (X'01') ELEMENT                 
         DROP  R6                                                               
*                                                                               
         L     R4,AIO                                                           
         LA    R4,29(R4)                                                        
*                                                                               
         MVI   ELCODE,X'05'                                                     
         GOTO1 REMELEM                                                          
         LA    R2,DMNF1H           R2 POINTS TO FIRST FIELD HEADER              
VR20     LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   COMSEQ,0            COMSCORE SEQUENCE NUMBER                     
*                                                                               
         USING DMNEL05,R5                                                       
VR25     CLI   5(R2),0             CHECK FOR INPUT                              
         BE    VR35                 NO INPUT                                    
         OI    FLAG,X'80'          AT LEAST ONE DEMO EXISTS                     
         BAS   RE,VRTVAL           VALIDATE THE RATING GROUP                    
         XC    ELEM,ELEM                                                        
         MVI   DMNEL05,X'05'       BUILD A DEMO DESCRIPT. X'05' ELEMENT         
         MVI   DMN05LEN,DMN5LEN1   X'05' ELEMENT LENGTH = 12                    
         MVC   DMNRTN,WORK2        DEMO CODE                                    
         MVC   DMNRTG,WORK2+6      7 CHAR NAME                                  
         CLI   WORK2+2,0           COMSCORE DEMO?                               
         BNE   VR26                NO                                           
         LLC   RF,COMSEQ           WE HAVE TO MAINTAIN COMSCORE SEQ             
         AHI   RF,1                BUMP TO NEXT SEQ                             
         STC   RF,COMSEQ           COMSCORE SEQ NUMBER                          
         STC   RF,DMNRTN+1         COMSCORE SEQ NUMBER                          
         MVC   DMNRTGC,WORK2+6     8 CHAR NAME FOR COMSCORE                     
         OC    DMNRTGC,SPACES      SPACE PAD                                    
         MVI   DMN05LEN,DMN5LEN2   X'05' ELEMENT LENGTH = 13                    
*                                                                               
VR26     L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VR27     BAS   RE,NEXTEL                                                        
         BNE   VR30                                                             
         CLC   2(3,R6),DMNRTN                                                   
         BNE   VR27                                                             
         B     ERRDUPE                                                          
*                                                                               
VR30     GOTO1 RECUP,DMCB,(0,AIO),ELEM,0(R4)                                    
VR35     ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT R2 TO THE NEXT FIELD HEADER            
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BE    VRX                  YES, EXIT                                   
         CLI   5(R2),0             IS THERE ANOTHER MENU TO DO?                 
         BE    VR35                 NO - SKIP TO NEXT DEMO                      
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VR25                                                             
         DROP  R5                                                               
*                                                                               
VRX      BAS   RE,CANTV                                                         
         TM    FLAG,X'80'          IS THERE AT LEAST ONE DEMO?                  
         LA    R2,DMNF1H                                                        
         BZ    ERRNODEM             NO                                          
         B     DR                  REDISPLAY RECORD                             
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0X                                                               
*                                                                               
         LA    R0,DMNLAST          LAST FIELD ON SCREEN                         
         LA    R2,DMNF1H           FIRST FIELD HEADER                           
*                                                                               
DR01     TM    1(R2),X'20'         FIELD IS PROTECTED?                          
         BO    DR02                 YES, BUMP TO NEXT FIELD                     
         ZIC   R1,0(R2)            LENGTH OF FIELD + HEADER                     
         SHI   R1,9                MINUS HEADER, AND 1 FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES      BLANK OUT FIELD                              
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
DR02     ZIC   R1,0(R2)            RESTORE LENGTH                               
         AR    R2,R1               NEXT SCREEN FIELD                            
         CR    R2,R0               END OF SCREEN?                               
         BL    DR01                 NO                                          
*---------------------------------------------------------------------*         
*                DR  ---  SCREEN IS NOW CLEARED                       *         
*---------------------------------------------------------------------*         
         MVC   KEY,SVKEY                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'01'        GET THE ACTIVITY DATE ELEMENT                
         BAS   RE,GETEL                                                         
         BE    *+8                                                              
         B     ERRNOACT            ACTIVITY DATE ELEMENT MUST EXIST             
*                                                                               
         MVC   DMNMSG(13),=C'LAST ACTIVITY'                                     
         USING DMNEL01,R6                                                       
         GOTO1 DATCON,DMCB,(3,DMNACDAT),(5,DMNMSG+14)                           
         OI    DMNMSGH+6,X'80'     TRANSMIT                                     
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'        GET THE DEMO DESCRIPTION ELEMENT             
         BAS   RE,GETEL                                                         
         BE    *+8                 R6 POINTS TO 1ST X'05' ELEMENT               
         B     ERRNODES            DEMO DESCRIPTION ELEMENT MUST EXIST          
*                                                                               
DR10     LA    R2,DMNF1H           R2 POINTS TO 1ST SCREEN FIELD HEADER         
         USING DMNEL05,R6                                                       
DR20     MVC   8(7,R2),DMNRTG      PUT OUT DEMO NAME 7 CHARS                    
         CLI   DMNRTN+2,0          COMSCORE DEMO?                               
         BNE   *+10                NO                                           
         MVC   8(8,R2),DMNRTGC     COMSCORE DEMO NAME IS 8 CHARS                
*                                                                               
         CLC   DMNRTN(2),=X'0021'  CHECK FOR USER DEMO                          
         BNE   DR23                                                             
         MVC   8(3,R2),=C'UN/'                                                  
         ZIC   R0,DMNRTN+2         USER DEMO NUMBER                             
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  9(1,R2),DUB                                                      
         MVC   11(7,R2),DMNRTG     USER DEMO NAME                               
*                                                                               
DR23     OI    6(R2),X'80'         TRANSMIT                                     
         ZIC   R0,0(R2)            POINT R2 TO NEXT FIELD HEADER                
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9             IS THIS THE END OF THE SCREEN?               
         BNH   DRX                  YES                                         
         BAS   RE,NEXTEL                                                        
         BE    DR20                THERE ANOTHER 05 ELEMENT                     
         DROP  R6                                                               
*                                                                               
DR30     MVC   KEY(13),SVKEY       REREADS THE RECORD FOR DISK...               
         CLI   ACTNUM,ACTADD       ...ADDRESS, EXCEPT FOR ADD ACTION            
         BE    DRX                  YES, IT IS ADD                              
         GOTO1 READ                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
DRX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
*                                                                               
DK       DS    0X                                                               
         L     R4,AIO                                                           
         USING DMNRECD,R4                                                       
         MVC   AMBYTE,DMNKAGMD     ISOLATE MEDIA CODE                           
         NI    AMBYTE,X'0F'                                                     
         LA    R5,MEDTAB           FIND MEDIA CODE USING MEDIA TABLE            
*                                                                               
DK10     CLC   AMBYTE,1(R5)                                                     
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
         B     ERRBDMED                                                         
DK20     MVC   DMNMEDK,0(R5)                                                    
         OI    DMNMEDKH+6,X'80'                                                 
         MVI   DMNMEDKH+5,1        TRANSMIT MEDIA CODE TO SCREEN                
*                                                                               
***********************************************************************         
*                                                                               
         MVC   DMNMENU,DMNKCODE                                                 
         OI    DMNMENUH+6,X'80'                                                 
         MVI   DMNMENUH+5,4        TRANSMIT DEMO MENU TO SCREEN                 
         OC    DMNMENU,SPACES      NEED SPACES INSTEAD OF NULLS                 
*                                                                               
***********************************************************************         
         DROP  R4                                                               
DKX      B     VK                                                               
         EJECT                                                                  
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0X                                                               
*                                                                               
         OC    KEY(13),KEY         FIRST TIME THROUGH?                          
         BNZ   LR10                 NO                                          
*                                                                               
         MVC   KEY(13),SVKEY       GETTING READY TO DO READ HIGH                
*                                                                               
LR10     GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR20     GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    LR30                                                             
         DC    H'0'                                                             
*                                                                               
LR30     CLC   KEY(3),SVKEY        SAME RECORD TYPE/MEDIA?                      
         BNE   LRX                  NO, NO MORE MENU RECORDS TO LIST            
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R4,AIO                                                           
         USING DMNRECD,R4                                                       
         MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   LSMENU,DMNKCODE     MOVE PRODUCT CODE TO LIST SCREEN             
         DROP  R4                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,5            DEMO DESCRIPTION ELEMENT CODE                
         LA    R5,LSDEMO                                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR45     BAS   RE,NEXTEL                                                        
         BNE   LRNEXT                                                           
         USING DMNEL05,R6                                                       
         MVC   0(7,R5),DMNRTG                                                   
         CLI   DMNRTN+2,0          COMSCORE DEMO?                               
         BNE   *+10                NO                                           
         MVC   0(8,R5),DMNRTGC     YES - CAN BE 8 CHARS                         
*                                                                               
         LA    R5,8(R5)                                                         
         B     LR45                                                             
         DROP  R6                                                               
*                                                                               
LRNEXT   GOTO1 LISTMON                                                          
         B     LR20                NEXT RECORD                                  
LRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
*                                                                               
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                    VRTVAL -- DEMO VALIDATION SUBROUTINE             *         
***********************************************************************         
*                                                                               
VRTVAL   NTR1                                                                   
         XC    WORK2(20),WORK2                                                  
         CLI   8(R2),C'U'          CHK FOR USER DEMOS                           
         BNE   VRTV2                                                            
         CLI   9(R2),C'1'                                                       
         BL    VRTV2                                                            
         CLI   9(R2),C'4'                                                       
         BH    VRTV2                                                            
         CLI   10(R2),C'/'                                                      
         BNE   VRTV2                                                            
         CLI   5(R2),4                                                          
         BL    ERRSMLDM                                                         
         MVC   WORK2(3),=X'002100'                                              
         PACK  DUB,9(1,R2)                                                      
         CVB   R0,DUB                                                           
         STC   R0,WORK2+2                                                       
         ZIC   R5,5(R2)            INPUT LENGTH                                 
         SH    R5,=H'4'            SUBTRACT FOR UN/                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+6(0),11(R2)   MOVE USER DEMO NAME                          
         OC    WORK2+6(7),SPACES                                                
         B     VRTVX                                                            
*                                                                               
VRTV2    CLI   5(R2),8             INPUT TOO LONG                               
         BH    ERRBIGDM                                                         
         L     R5,AIO2                                                          
         USING DBLOCK,R5                                                        
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'                                                   
         MVI   DBSELMED,C'T'                                                    
         LA    R4,256(R5)          BUILD COMSCORE DEMO LIST HERE                
***                                                                             
         L     R1,AIO2                                                          
         AHI   R1,1024                                                          
         USING P5XD,R1                                                          
         MVC   P5XID,=C'P5X '       PARAM 5 EXTENDED BLOCK                      
         LA    RE,CSUTOKEN                                                      
         ST    RE,P5XLICNS          A(32 BYTE COMSCORE LICENSE)                 
         DROP  R1                                                               
         ST    R1,DMCB+16           EXTENDED PARAM5                             
         OI    DMCB+16,X'80'        SO DEMOVAL KNOWS EXTENDED BLOCK             
*                                                                               
         GOTO1 VDEMOVAL,DMCB,(R2),(1,WORK2),(C'S',DBLOCK),0,,(R4)               
*                                                                               
         CLI   4(R1),0                                                          
         BE    ERRINVDM            INVALID DEMO                                 
*                                                                               
         MVC   DBFILE,=C'TP '                                                   
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   VRTV10                                                           
         CLI   QMED,C'T'             TV?                                        
         BNE   VRTV10                                                           
         MVI   DBSELMED,C'C'                                                    
*                                                                               
VRTV10   GOTO1 VDEMOCON,DMCB,(1,WORK2),(2,WORK2+6),(C'S',DBLOCK),0,(R4)         
*                                                                               
VRTVX    XIT1                                                                   
*        DROP  R7                                                               
         DROP  R5                                                               
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
*        CANTV DEMOGRAPHICS MENU                                      *         
***********************************************************************         
*        IF CANADIAN AGENCY ADDS OR MODIFIES A MENU(S) FOR TV - MUST  *         
*        ADD OR MODIFY PRODUCT RECORD FOR MEDIA N(03) AND MEDIA C(08) *         
***********************************************************************         
CANTV    NTR1                                                                   
         USING DMNRECD,R4                                                       
         L     R4,AIO1                                                          
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTX                                                              
         CLI   QMED,C'T'             TV?                                        
         BNE   CTX                                                              
*                                                                               
         XC    KEY,KEY               MEDIA N(03)                                
         MVC   KEY,DMNKEY                                                       
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT10                                                             
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEYSAVE      RECORD DOESN'T EXIST                       
         MVC   KEY(13),KEYSAVE       MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         BRAS  RE,ADREC                                                         
         B     CT20                                                             
*                                                                               
CT10     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         BRAS  RE,PTREC              PUT N(03) RECORD                           
*                                                                               
CT20     XC    KEY,KEY               MEDIA C(08)                                
         MVC   KEY,DMNKEY                                                       
         NI    KEY+2,X'F0'                                                      
         OI    KEY+2,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT30                                                             
         L     R5,AIO1               RECORD DOESN'T EXIST                       
         MVC   0(13,R5),KEYSAVE      MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),KEYSAVE                                                  
         BRAS  RE,ADREC                                                         
         B     CT40                                                             
*                                                                               
CT30     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         BRAS  RE,PTREC              PUT C(08) RECORD                           
CT40     MVC   KEY,DMNKEY            RESTORE KEY                                
         NI    2(R5),X'F0'                                                      
         OI    2(R5),X'01'                                                      
CTX      XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       PUT RECORD                                    *         
***********************************************************************         
PTREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       ADD RECORD                                    *         
***********************************************************************         
ADREC    NTR1                                                                   
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRDUPE  MVC   ERRNUM,=AL2(DUPEDEMO)                                            
         B     SPERREX                                                          
ERRSMLDM MVC   ERRNUM,=AL2(SMALLDEM)                                            
         B     SPERREX                                                          
ERRBIGDM MVC   ERRNUM,=AL2(BIGDEMO)                                             
         B     SPERREX                                                          
ERRINVDM MVC   ERRNUM,=AL2(INVLDDM)                                             
         B     SPERREX                                                          
ERRNOACT MVC   ERRNUM,=AL2(NOACTELM)                                            
         B     SPERREX                                                          
ERRNODES MVC   ERRNUM,=AL2(NODEMDES)                                            
         B     SPERREX                                                          
ERRBDMED MVC   ERRNUM,=AL2(BADMEDIA)                                            
         B     SPERREX                                                          
ERRNODEM MVC   ERRNUM,=AL2(NODEMO)                                              
         B     SPERREX                                                          
ERRNOTDI MVC   ERRNUM,=AL2(NOTDISP)                                             
         B     SPERREX                                                          
ERRNOAUT MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRSJR   MVC   ERRNUM,=AL2(NOTDDS)                                              
         B     SPERREX                                                          
                                                                                
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*                                  SHORT DESP OF ERROR MSGS                     
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
DUPEDEMO EQU   291                 DUPLICATE DEMO                               
NOACTELM EQU   899                 REQUIRED ACTIVITY ELEMENT NOT FOUND          
NODEMDES EQU   904                 REQUIRED DEMO ELEMENT NOT FOUND              
BADMEDIA EQU   905                 BAD MEDIA AND/OR RECORD                      
SMALLDEM EQU   909                 DEMO HAS TOO FEW CHARACTERS                  
BIGDEMO  EQU   910                 DEMO HAS TOO MANY CHARACTERS                 
INVLDDM  EQU   388                 INVALID DEMO                                 
NODEMO   EQU   721                 AT LEAST ONE DEMO NEEDED                     
NOTDISP  EQU   911                 WITH MEDIA C AND N, UPDATIVE ACTIONS         
*                                  ...ARE NOT ALLOWED                           
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
NOTDDS   EQU   11                  THIS PROGRAM FOR DDS TERMINALS ONLY          
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   CANNOT DELETE FROM LIST                      
         MVI   NLISTS,15           CHANGE THE NUMBER OF LINES TO 15             
*                                  ...FOR LIST                                  
SETUPX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM42D          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM43D          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDMN          DEMOGRAPHICS MENU RECORD DSECT               
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMOVALD                                                     
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
VDEMOVAL DS    F                                                                
VDEMOCON DS    F                                                                
*                                                                               
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
AMBYTE   DS    CL1                                                              
SAVESEL  DS    CL1                                                              
SAVEKEY  DS    CL30                                                             
WORK2    DS    CL40                                                             
FLAG     DS    CL1                                                              
*                                  X'80' AT LEAST 1 DEMO EXISTS                 
COMSEQ   DS    XL1                                                              
         EJECT                                                                  
***********************************************************************         
*        LIST LINE DSECT                                              *         
***********************************************************************         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSMENU   DS    CL4                                                              
         DS    CL2                                                              
LSDEMO   DS    CL68                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'251SPSFM83   11/07/18'                                      
         END                                                                    
