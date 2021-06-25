*          DATA SET TAGEN81    AT LEVEL 048 AS OF 05/04/13                      
*PHASE T70281D,*                                                                
         TITLE 'T70281 - RELEASE LETTER REQUESTS'                               
T70281   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70281,R6                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    *+14                                                             
         MVC   SCOHEAD(9),=CL9'Pid Num'                                         
         OI    SCOHEADH+6,X'80'                                                 
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BNE   MX                                                               
         BAS   RE,VK                                                            
         CLI   SELCAST,C'Y'        IF REQUESTING ENTIRE CAST                    
         BE    M02                                                              
         BAS   RE,RLADD            ADD A REQUEST CARD                           
         BAS   RE,ACTVINCO         ADD ACTIVITY EL TO COMMERCIAL REC            
         TWAXC SCOSELH,SCOLSTH,PROT=Y                                           
         B     MX                                                               
*                                                                               
M02      CLI   LISTED,C'Y'         IF A PAGE WAS ALREADY LISTED                 
         BNE   M05                                                              
         BAS   RE,SELECTC          THEN SELECT FROM IT                          
*                                                                               
M05      CLI   LSTPAGE,C'Y'        IF LAST PAGE SELECTED FROM ALREADY           
         BNE   M10                                                              
         EDIT  REQCNT,(3,SCOCNT),ALIGN=LEFT,ZERO=NOBLANK                        
         OI    SCOCNTH+6,X'80'     TRANSMIT REQUEST COUNT                       
         TWAXC SCOSELH,SCOLSTH,PROT=Y   & CLEAR SCREEN                          
         B     MX                  GIVE MESSAGE                                 
*                                                                               
M10      BAS   RE,LISTCAST         LIST CAST                                    
         EDIT  REQCNT,(3,SCOCNT),ALIGN=LEFT,ZERO=NOBLANK                        
         OI    SCOCNTH+6,X'80'     TRANSMIT REQUEST COUNT                       
*                                                                               
         OC    TIQSKEY,TIQSKEY     IF END OF LIST                               
         BNZ   SELINF                                                           
         B     LSTINF              GIVE LAST PAGE MESSAGE                       
*                                                                               
MX       B     REQINF                                                           
         EJECT                                                                  
*                                                                               
VK       NTR1                                                                   
         TM    SCRSTAT,SCRCHG      FIRST TIME NEW SCREEN                        
         BNO   VK05                                                             
         NI    SCOAGYH+4,X'DF'     FORCE VALIDATION OF SCREEN                   
*                                                                               
VK05     MVI   KEYCHG,C'N'         DEFAULT - KEY HASN'T CHANGED                 
         LA    R2,SCOAGYH                                                       
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK10                                                             
         NI    SCOCIDH+4,X'DF'     UNVALIDATE NEXT FIELD                        
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SCOAGYH),SCOAGYNH                     
         MVC   REQAGY,TGAGY                                                     
         L     R4,AIO              GET OFFICE CODE                              
         USING TAAYD,R4                                                         
         MVI   ELCODE,TAAYELQ      FROM AGENCY ELEMENT                          
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   REQOFF,TAAYTPOF                                                  
*                                                                               
VK10     OI    4(R2),X'20'         VALIDATE AGENCY                              
         LA    R2,SCOCIDH                                                       
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK20                                                             
         NI    SCOCODEH+4,X'DF'     UNVALIDATE NEXT FIELD                       
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'28',SCOCIDH),SCOCIDNH                    
         L     R3,AIO              GET INTERNAL # AND SAVE                      
         USING TLCOD,R3                                                         
         GOTO1 HEXOUT,DMCB,TLCOCOM,REQCOM,4,=C'TOG'                             
         MVC   SVCOM,TLCOCOM                                                    
*                                                                               
VK20     OI    4(R2),X'20'         VALIDATE COMMERCIAL                          
         LA    R2,SCOCODEH                                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK30                                                             
         NI    SCOCASTH+4,X'DF'    UNVALIDATE NEXT FIELD                        
         GOTO1 ANY                                                              
         CLI   SCOCODE,C'A'        LETTER CODE MUST BE A - D                    
         BL    INVERR                                                           
         CLI   SCOCODE,C'D'                                                     
         BH    INVERR                                                           
         MVC   REQCODE,SCOCODE                                                  
*                                                                               
VK30     OI    4(R2),X'20'         VALIDATE LETTER CODE                         
         LA    R2,SCOCASTH         ENTIRE CAST ?                                
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK40                                                             
         NI    SCOEFDTH+4,X'DF'     UNVALIDATE NEXT FIELD                       
         GOTO1 ANY                                                              
         MVI   SELCAST,C'N'                                                     
         MVC   REQSSN,ZEROES                                                    
         MVC   REQSEQ,ZEROES                                                    
         MVC   REQCAT,SPACES                                                    
         CLI   SCOCAST,C'N'        OR SELECT CAST MEMBERS                       
         BE    VK40                                                             
         MVI   SELCAST,C'Y'                                                     
         CLI   SCOCAST,C'Y'                                                     
         BNE   INVERR                                                           
*                                                                               
VK40     OI    4(R2),X'20'         VALIDATE LETTER CODE                         
         LA    R2,SCOEFDTH                                                      
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BO    VK50                                                             
         MVI   KEYCHG,C'Y'                                                      
         MVC   REQDATE,ZEROES                                                   
         CLI   5(R2),0             EFFECTIVE DATE REQUIRED FOR C AND D          
****     BNE   VK45                LETTER CODES                                 
****     CLI   SCOCODE,C'B'                                                     
****     BH    ERRMIS                                                           
****     B     VK50                                                             
         BE    ERRMIS                                                           
VK45     DS    0C                                                               
****     CLI   SCOCODE,C'B'        EFFECTIVE DATE NOT ALLOWED FOR               
****     BNH   INVERR              A AND B LETTER CODES                         
         GOTO1 DATVAL,DMCB,(0,SCOEFDT),(X'80',REQDATE)                          
         ICM   R1,15,0(R1)                                                      
         BZ    INVERR                                                           
*                                                                               
VK50     OI    4(R2),X'20'         VALIDATE                                     
         CLI   KEYCHG,C'N'                                                      
         BE    VK60                                                             
         MVI   LISTED,C'N'                                                      
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY TO START OVER             
         XC    REQCNT,REQCNT             REQUEST COUNTER                        
         MVI   LSTPAGE,C'N'                                                     
*                                                                               
VK60     TM    WHEN,X'80'          IF NO INPUT IN PRINT FIELD                   
         BNO   VKX                                                              
         MVI   WHEN,X'08'          THEN SET AS IF DDS REPORT                    
         MVI   TWAWHEN,X'04'                                                    
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LIST CAST AND SET UP REQUEST CARD FOR SEL             
         SPACE 1                                                                
LISTCAST NTR1                                                                   
         TWAXC SCOSELH,SCOLSTH,PROT=Y                                           
         XC    SEQTAB(28),SEQTAB                                                
         BAS   RE,SETSYS           SET UP SYSIO BLOCK                           
         LA    R2,SCOLINEH                                                      
         ST    R2,APLINE                                                        
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         CLI   KEY,X'90'           IF SYSIO STOPPED (NOT FORCED)                
         BE    LC10                                                             
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY TO START OVER             
*                                                                               
LC10     MVI   LISTED,C'Y'                                                      
         MVI   COUNTER,0           AND START OVER                               
*                                                                               
LCX      B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
*        SET UP BASIC SYSIO BLOCK                                               
*                                                                               
         SPACE 1                                                                
SETSYS   NTR1                                                                   
         MVC   TIACOMFC,ACOMFACS                                                
         LA    R1,IOHOOK                                                        
         ST    R1,TIHOOK                                                        
         MVC   TIACCS,TWAACCS      PASS THROUGH LIMIT ACCESS                    
         MVC   TIAUTH,TWAAUTH           AND AUTHORIZATION                       
         MVC   TIUSERID,TWAORIG         AND REQUESTING ID                       
         XC    SVORIG,SVORIG                                                    
         MVC   SVORIG+8(2),TWAORIG                                              
         GOTO1 USERVAL,DMCB,(X'80',SVORIG)                                      
*                                                                               
         MVC   TIFCOM,SVCOM            INTERNAL COMMERCIAL NUMBER               
         MVI   TIREAD,TLCACDQ          CAST RECORDS                             
         B     XIT                                                              
         SPACE 1                                                                
*                                                                               
*        HOOK TO SYSIO                                                          
*                                                                               
         SPACE 1                                                                
         USING LINED,R2                                                         
IOHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      RECORD GOTTEN FROM SYSIO                     
         BNE   IOX                                                              
         MVC   TIQSKEY,TIKEY       SET CONTINUE KEY                             
         CLC   TIUN,=C'AFM'        IF MUSICIAN - STOP                           
         BNE   IO20                                                             
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY TO START OVER             
         B     IOX                                                              
*                                                                               
IO20     CLI   COUNTER,14                                                       
         BNE   IO25                IF END OF PAGE                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'90'           FORCE END TO SYSIO READ                      
         GOTO1 HIGH                                                             
         MVI   COUNTER,0           AND START OVER                               
         B     IOX                                                              
*                                                                               
*NO-OP   CLC   TIUN,=C'ACT'        DO NOT DISPLAY UNION ACT                     
*NO-OP   BE    IOX                                                              
IO25     L     R2,APLINE           A(HEADER OF NEXT LINE TO PRINT ON)           
         LA    R2,8(R2)            PAST HEADER                                  
         USING LINED,R2                                                         
         L     R3,TIAREC                                                        
         USING TLCAD,R3                                                         
         CLI   SCOCODE,C'A'        DOWNGRADE?                                   
         BNE   *+12                                                             
         TM    TLCASORT,X'20'      EXTRAS CANNOT BE DOWNGRADED                  
         BO    IOX                                                              
*                                                                               
         MVC   CASSN,TLCASSN       SSN                                          
*                                                                               
         TM    TGSYSTAT,TASYSPID                                                
         BZ    IO28                                                             
         MVC   CASSN,SPACES                                                     
         GOTO1 SSNPACK,DMCB,TISSN,CASSN    CONVERT SSN TO PID                   
*                                                                               
IO28     MVC   CACAT,TLCACAT       CATEGORY                                     
         MVC   CACAM,TIONOF        ON/OFF CAMERA                                
         ZIC   R1,COUNTER                                                       
         LA    R4,SEQTAB           A(START OF SEQUENCE TABLE)                   
         LTR   R1,R1                                                            
         BZ    IO40                                                             
*                                                                               
IO30     LA    R4,2(R4)            BUMP TO CORRECT ENTRY IN TABLE               
         BCT   R1,IO30                                                          
*                                                                               
IO40     MVC   0(2,R4),TLCASEQ     SET SEQ NUMBER                               
*                                                                               
         MVI   MYNAMEH,41          SET FIELD LENGTH                             
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'88',TLCASSN),MYNAMEH                      
         MVC   CALNAME(32),MYNAME                                               
         DROP  R2                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,TIKEY           RESTORE SYSIO READ SEQ                       
         GOTO1 HIGH                                                             
*                                                                               
         L     R2,APLINE           A(HEADER OF NEXT LINE TO PRINT ON)           
         OI    6(R2),X'80'         TRASNMIT                                     
         ZIC   R1,0(R2)            BUMP PAST LINE FIELD                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP PAST AUTO SKIP FIELD                    
         AR    R2,R1                                                            
         ZIC   R1,0(R2)            BUMP PAST SEL FIELD                          
         AR    R2,R1                                                            
         ST    R2,APLINE           NEXT LINE TO PRINT ON                        
*                                                                               
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
*                                                                               
IOX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SELECT CAST MEMBERS                                   
         SPACE 1                                                                
SELECTC  NTR1                                                                   
         MVI   LISTED,C'N'                                                      
         LA    R2,SCOSELH          START AT FIRST SEL FIELD                     
         LR    R3,R2                                                            
         LA    R4,SEQTAB           GET SEQ NUMBER TOO                           
         LA    R5,SCOLSTH                                                       
*                                                                               
SC10     CR    R2,R5               IF END OF SCREEN - EXIT                      
         BNL   SCX                                                              
         CLI   5(R2),0             ACCEPT ANY INPUT                             
         BE    SC20                                                             
         BAS   RE,ANYSEL           IS THERE'S NOTHING ON THIS LINE TO           
         BNE   SCX                 SELECT FROM - DONE                           
         MVC   8(1,R2),SPACES      CLEAR FIELD FROM INPUT                       
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)            BUMP PAST AUTO SKIP                          
         AR    R3,R1                                                            
         ZIC   R1,0(R3)            BUMP TO DISPLAY LINE                         
         AR    R3,R1                                                            
         LA    R3,8(R3)            PAST HEADER                                  
         USING LINED,R3                                                         
         MVC   REQSSN,CASSN        SET REQUEST INFO OF CAST MEMBER              
         MVC   REQCAT,CACAT                                                     
         MVC   SVSEQ,0(R4)           HEXOUT SEQ NUMBER                          
         GOTO1 HEXOUT,DMCB,SVSEQ,REQSEQ,2,=C'TOG'                               
         BAS   RE,RLADD            ADD A REQUEST                                
         BAS   RE,ACTVINCA         ADD ACTIVITY EL TO CAST REC                  
*                                                                               
SC20     LA    R4,2(R4)            BUMP SEQ TABLE                               
         ZIC   R1,0(R2)            BUMP PAST SEL & DISPLAY LINES                
         AR    R2,R1               & AUTO SKIP                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         LR    R3,R2                                                            
         B     SC10                                                             
*                                                                               
SCX      B     XIT                                                              
         EJECT                                                                  
*              CHECK TO SEE IF ANYTHING ON DISPLAY LINE                         
         SPACE 1                                                                
ANYSEL   NTR1                                                                   
         ZIC   R1,0(R2)            BUMP PAST SEL                                
         AR    R2,R1               & AUTO SKIP                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLC   8(L'SCOLINE,R2),SPACES  IF THERE IS ANYTHING ON DISPLAY          
         BH    YES                        RETURN CC - EQ                        
         B     NO                                                               
         SPACE 3                                                                
*              ROUTINE TO STAMP COMM'L REC WITH ACTIVITY EL                     
         SPACE                                                                  
ACTVINCO NTR1                                                                   
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'B0',SVCOM)  GET REC & RDUPDATE           
         BE    *+6                                                              
         DC    H'0'                   MUST BE THERE, ALREADY READ               
         GOTO1 ACTVIN,DMCB,(X'80',0)  ADD ACTIVITY EL WITH SCREEN CODE          
         GOTO1 PUTREC                 WRITE RECORD BACK                         
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO STAMP CAST REC WITH ACTIVITY EL                       
         SPACE                                                                  
ACTVINCA NTR1                                                                   
         MVC   TGSSN,REQSSN                                                     
         TM    TGSYSTAT,TASYSPID                                                
         BNO   ACTV10                                                           
         GOTO1 SSNUNPK,DMCB,REQPID,TGSSN  CONVERT PID TO SSN                    
         BE    *+6                                                              
         DC    H'00'                                                            
ACTV10   MVC   TGCAT,REQCAT                                                     
         MVC   TGCOM,SVCOM                                                      
         GOTO1 RECVAL,DMCB,TLCACCDQ,(X'B0',SVSEQ)  GET REC & RDUPDATE           
         BE    *+6                                                              
         DC    H'0'                   MUST BE THERE, ALREADY READ               
         GOTO1 ACTVIN,DMCB,(X'80',0)  ADD ACTIVITY EL WITH SCREEN CODE          
         GOTO1 PUTREC                 WRITE RECORD BACK                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET UP REQUEST CARD FOR RELEASE LETTER                
         SPACE 1                                                                
RLADD    NTR1                                                                   
         LA    R3,BLOCK            R3=A(REQUEST RECORD)                         
         USING REQD,R3                                                          
         MVC   REQUEST,RLEQCARD    INITIALIZE REQUEST CARD                      
*                                                                               
         MVC   REQUEST+38(8),REQCOM    INTERNAL COMML NUMBER                    
         MVC   REQUEST+46(1),REQCODE   LETTER CODE                              
         MVC   REQUEST+52(6),REQDATE   EFFECTIVE DATE                           
         MVC   REQUEST+63(9),REQSSN    SS NUMBER/PID                            
*                                                                               
         MVC   REQUEST+72(4),REQSEQ                                             
*                                                                               
         MVC   REQUEST+76(3),REQCAT    CATEGORY                                 
         BAS   RE,ADDREQ           ADD REQUEST TO REQUEST FILE                  
*                                                                               
RLX      B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO ADD A REQUEST TO REQUEST FILE                         
         SPACE 1                                                                
         USING REQD,R3             R3=A(REQUEST RECORD)                         
ADDREQ   NTR1                                                                   
         ZIC   R1,REQCNT           INCREMENT REQUEST COUNT                      
         LA    R1,1(R1)                                                         
         STC   R1,REQCNT                                                        
         XC    REQHDR,REQHDR       CLEAR HEADER                                 
         OI    REQHDR+15,X'01'     SET LINKED REQUEST                           
         SPACE 1                                                                
*                                  * BUILD SORT AREA AT BEG. OF CARD            
         MVC   REQUEST+2(2),TGCTALPH  CONNECT ALPHA USER ID                     
         MVC   REQUEST+4(1),REQOFF    TP OFFICE                                 
         MVC   REQUEST+5(6),REQAGY    AGENCY                                    
         SPACE 1                                                                
         OC    REQOFF,REQOFF                                                    
         BNZ   *+6                                                              
         DC    H'0'                DIE IF NO OFFICE                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMADD '),REQFILE,(R3),(R3)                    
         CLI   8(R1),0                                                          
         BE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
ERRMIS   MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
REQINF   CLI   SELCAST,C'N'        IF REQUESTING ENTIRE CAST                    
         BE    REQINF2             GIVE APPROPRIATE MESSAGE                     
         MVI   LSTPAGE,C'N'                                                     
         MVI   MYMSGNO1,83         NOTHING REQUESTED                            
         CLI   REQCNT,0                                                         
         BE    RECEND                                                           
*                                                                               
REQINF2  XC    REQCNT,REQCNT       CLEAR REQUEST COUNTER                        
         MVI   MYMSGNO1,68         REPORT WILL BE PROCESSED OVERNIGHT           
         B     RECEND                                                           
         SPACE 1                                                                
SELINF   MVI   MYMSGNO1,3          CAST DISPLAYED - SELECT AS DESIRED           
         LA    R2,SCOSELH                                                       
         B     INFEND                                                           
         SPACE 1                                                                
LSTINF   MVI   MYMSGNO1,10         NO MORE CAST TO DISPLAY - SELECT             
         MVI   LSTPAGE,C'Y'                                                     
         LA    R2,SCOSELH                                                       
         B     INFEND                                                           
         SPACE 1                                                                
RECEND   LA    R2,CONRECH                                                       
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
ZEROES   DC    CL9'000000000'                                                   
         SPACE 1                                                                
RLEQCARD DC    CL80'RLXXOAGENCY 0203REL 0301R 0503DDS 0909INTCOMNOL 100X        
               6YYMMDD 1116123456789SEQ CAT*'                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
LINED    DSECT                                                                  
CASSN    DS    CL9                 SS NUMBER                                    
         DS    CL1                                                              
CALNAME  DS    CL16                PERFORMER NAME                               
         DS    CL1                                                              
CAFNAME  DS    CL16                                                             
         DS    CL1                                                              
CACAT    DS    CL3                 CATEGORY                                     
         DS    CL1                                                              
CACAM    DS    CL3                 ON/OFF CAMERA                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR81D                                                       
         EJECT                                                                  
REQAGY   DS    CL6                 REQUEST AGENCY                               
REQOFF   DS    CL1                         OFFICE                               
REQCOM   DS    CL8                         INTERNAL COMML # (HEXOUT)            
REQSSN   DS    CL9                         SSN                                  
         ORG   REQSSN                                                           
REQPID   DS    CL6                         PID                                  
         DS    CL3                                                              
REQSEQ   DS    CL4                         CAST SEQ NUMBER                      
REQCAT   DS    CL3                         CAST CATAGORY                        
REQCODE  DS    CL1                         LETTER CODE                          
REQDATE  DS    CL6                         EFFECTIVE DATE                       
*                                                                               
KEYCHG   DS    CL1                 FLAG - DID KEY CHANGE                        
LSTPAGE  DS    CL1                 SELECTED FROM LAST PAGE ALREADY              
REQCNT   DS    XL1                 COUNT HOW MANY REQUESTS ADDED                
COUNTER  DS    XL1                 COUNT HOW MANY LINES PER PAGE                
SELCAST  DS    CL1                 ENTIRE CAST REQUESTED Y/N                    
LISTED   DS    CL1                 CAST LISTED ALREADY                          
MYNAMEH  DS    XL8                 FAKE HEADER                                  
MYNAME   DS    CL33                                                             
SVORIG   DS    XL10                                                             
SVCOM    DS    XL4                 SAVED INTERNAL COMMERCIAL NUMBER             
SVKEY    DS    CL(L'KEY)                 KEY                                    
SVSEQ    DS    H                   CAST INPUT SEQUENCE NUMBER                   
SVTWASCR DS    X                   SAVED TWASCR                                 
APLINE   DS    A                   A(NEXT PRINT LINE)                           
*                                                                               
SEQTAB   DS    14H                 TABLE OF SEQ NUMBERS                         
*                                                                               
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
*                                                                               
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048TAGEN81   05/04/13'                                      
         END                                                                    
