*          DATA SET TAGEN30    AT LEVEL 005 AS OF 05/01/02                      
*PHASE T70230A                                                                  
         TITLE 'T70230 - ACCESS LIST'                                           
T70230   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70230                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=TWAHOLE                                   
         USING WORKD,R7                                                         
         EJECT                                                                  
* MODE CONTROLLED ROUTINES                                                      
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LR                                                               
         CLI   MODE,VALREC         IF MODE LISTRECS                             
         BNE   ACLX                                                             
         B     LR                  GO TO LR                                     
*                                                                               
ACLX     B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE THE KEY                                                              
*                                                                               
VK       DS    0H                                                               
*                                                                               
         LA    R2,ACLUSERH         VALIDATE USER ID                             
         TM    4(R2),X'20'                                                      
         BO    VK10                                                             
         NI    ACLTYPEH+4,X'DF'                                                 
*                                                                               
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BNE   VK3                                                              
         XC    MYUSER,MYUSER       SET USER ID TO ZEROS                         
         MVC   8(3,R2),=C'ALL'     MOVE 'ALL' INTO FIELD                        
         OI    6(R2),X'80'                                                      
         B     VK7                                                              
*                                                                               
VK3      CLC   =C'ALL',8(R2)       ELSE IF USER ID IS 'ALL'                     
         BNE   VK5                                                              
         XC    MYUSER,MYUSER       SET USER ID TO ZEROS                         
         B     VK7                                                              
*                                                                               
VK5      GOTO1 USERVAL,DMCB,(R2)   ELSE CALL USERVAL                            
         MVC   MYUSER,TGUSER       SAVE USER ID                                 
*                                                                               
VK7      OI    4(R2),X'20'                                                      
*                                                                               
VK10     DS    0H                                                               
*&&DO                                                                           
VK10     LA    R2,ACLTYPEH         VALIDATE TYPE FILTER                         
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         NI    ACLSTRH+4,X'DF'                                                  
*                                                                               
         MVI   TYPEFILT,0          IF NOT ENTERED THEN ZERO                     
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 STAFVAL,DMCB,8(R2),0      VALIDATE IT                            
         BNE   ERRINV                                                           
         GOTO1 (RF),(R1),8(R2),TGCTSTLV  TEST IF USER CAN ACCESS IT             
         BNE   ERRINV                                                           
         MVC   TYPEFILT,8(R2)      SAVE TYPE FILTER                             
VK30     OI    4(R2),X'20'                                                      
*                                                                               
VK50     LA    R2,ACLSTRH          VALIDATE START AT KEY                        
         TM    4(R2),X'20'                                                      
         BO    VK60                                                             
*        NI    ACLFMTH+4,X'DF'                                                  
*                                                                               
         XC    STARTAT,STARTAT     SET TO ZEROS IF NOTHING ENTERED              
         CLI   5(R2),0                                                          
         BE    VK55                                                             
*                                                                               
         ZIC   R3,5(R2)            ELSE EXTRACT KEY FROM FIELD                  
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   STARTAT(0),8(R2)                                                 
         OC    STARTAT,SPACES      PAD WITH SPACES                              
*                                                                               
VK55     OI    4(R2),X'20'                                                      
*                                                                               
VK60     LA    R2,ACLFMTH          VALIDATE FORMAT                              
         TM    4(R2),X'20'                                                      
         BO    VK70                                                             
         NI    ACLOPTSH+4,X'DF'                                                 
*                                                                               
         MVI   RDSEQ,C'A'          DEFAULT TO ALPHA SEQUENCE                    
         OC    MYUSER,MYUSER                                                    
         BZ    *+8                                                              
         MVI   RDSEQ,C'C'          UNLESS ID INPUT, THEN DEF TO CODE            
         CLI   5(R2),0                                                          
         BE    VK65                                                             
         CLI   5(R2),1             ONLY 1 CHAR CAN BE INPUT                     
         BNE   ERRINV                                                           
         CLI   8(R2),C'A'                                                       
         BE    VK65                                                             
         MVI   RDSEQ,C'C'                                                       
         CLI   8(R2),C'C'          CODE SEQUENCE                                
         BE    VK65                                                             
         CLI   8(R2),C'X'          BAD RECORDS ONLY                             
         BNE   ERRINV                                                           
         CLI   TGCTSTTY,TASTTYPP   PROGRAMMERS ONLY                             
         BNE   ERRINV                                                           
*                                                                               
VK65     OI    4(R2),X'20'                                                      
*                                                                               
VK70     LA    R2,ACLOPTSH         VALIDATE OPTIONS                             
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
*                                                                               
         CLI   5(R2),0             NOTHING VALID AT THIS POINT                  
         BNE   ERRINV                                                           
*                                                                               
         OI    4(R2),X'20'                                                      
*&&                                                                             
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
* LIST THE RECORDS                                                              
*                                                                               
LR       LA    R5,LISTAR                                                        
         USING LISTD,R5                                                         
         CLI   LISTCT,0            ARE ANY ITEMS IN THE LIST?                   
         BNH   LR10                                                             
         CLI   MYMODE,1            RESTORING LIST AFTER SELECT?                 
         BE    *+8                                                              
         BAS   RE,TESTSEL          TEST IF ANY ITEMS ARE SELECTED               
         BAS   RE,RESET            IF NOT, SCROLL DOWN LIST                     
*                                                                               
         L     R4,AIO              ADVANCE R4 TO THE NEXT ELEMENT               
         MVI   ELCODE,TAAVELQ      THAT WILL GO ON THE LIST SCREEN              
         BAS   RE,GETEL            WHEN SCROLLING DOWN                          
         BNE   LRX                                                              
         ZIC   R1,ELCOUNT          TOTAL ELEMENTS ALREADY ON LIST               
LR05     BAS   RE,NEXTEL                                                        
         BNE   LR10                IF END OF RECORD, START OVER                 
         BCT   R1,LR05                                                          
         B     LR15                                                             
*                                                                               
LR10     MVI   ELCOUNT,0           ELEMENT COUNTER                              
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0)   GET SYSTEM RECORD                
         BE    *+6                                                              
         DC    H'00'                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAAVELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   LRX                                                              
         USING TAAVD,R4                                                         
*                                                                               
LR15     LA    R2,ACLSELH          POINT R2 TO FIRST LIST LINE ON SCRN          
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
*                                                                               
LR20     MVC   LISTAR,SPACES       CLEAR LIST LINE                              
         MVC   ALLINE,TAAVLINE     LINE ID/ADD                                  
         CLI   TAAVLEN,TAAVLNQ     IF ELEMENT IS OLD LENGTH, CANNOT             
         BNE   LR40                GET STAFF ID OR READ STAFF REC.              
*                                                                               
LR30     MVC   ALSTAFF,TAAVSTAF    NEW LENGTH ELEMENT                           
         MVC   HALF,TGUSER         SAVE CURRENT GLOBAL USER CODE                
         MVC   TGUSER,TAAVUSER     GLOBAL USER CODE FOR RECVAL                  
         BAS   RE,GETSTAFF                                                      
         MVC   TGUSER,HALF         RESTORE USER CODE                            
         BNE   LR60                                                             
*                                                                               
LR40     ZIC   RE,LISTCT           INCREMENT LIST COUNT                         
         AHI   RE,1                                                             
         STC   RE,LISTCT                                                        
*                                                                               
         CLI   LISTCT,LISTMAX      IF END OF PAGE                               
         BNH   LR50                BUT THERE ARE MORE RECS                      
         LA    R2,ACLSELH                                                       
         CLI   MYMODE,1            RESTORING LIST AFTER SELECT?                 
         BE    RESTORED                                                         
         MVI   MYMSGNO1,9          PUT MSG - HIT ENTER FOR NEXT                 
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         B     ERRXIT                                                           
*                                                                               
LR50     MVC   8(75,R2),LISTAR     DISPLAY LIST LINE                            
         MVI   7(R2),75                                                         
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP2                                                         
*                                                                               
LR60     ZIC   R1,ELCOUNT          INCREMENT ELEMENT COUNTER                    
         AHI   R1,1                                                             
         STC   R1,ELCOUNT                                                       
*                                                                               
         MVC   AIO,AIO1                                                         
         MVI   ELCODE,TAAVELQ      USERVAL CHANGES THIS                         
         BAS   RE,NEXTEL           IF NO MORE VIOLATION ELEMENTS, DONE          
         BE    LR20                                                             
*                                                                               
LRX      LA    R2,ACLSELH                                                       
         CLI   MYMODE,1            RESTORING LIST AFTER SELECT?                 
         BE    RESTORED                                                         
         MVI   MYMSGNO1,10         PUT MSG - END OF LIST                        
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         B     ERRXIT                                                           
*                                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
*                                                                               
* GET STAFF INFO FROM THE STAFF RECORD                                          
*                                                                               
GETSTAFF NTR1                                                                   
         USING LISTD,R5                                                         
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A0',ALSTAFF)                              
         BNE   YES                 SKIP IF STAFF RECORD DOESN'T EXIST           
         L     R4,AIO                                                           
         USING TLSTD,R4                                                         
         OC    MYUSER,MYUSER       IF USER ID FILTER SPECIFIED                  
         BZ    *+14                                                             
         CLC   TGUSER,MYUSER     THEN SKIP DIFFERENT USER IDS                   
         BNE   NO                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
         XC    WORK,WORK           MOVE IN EBCDIC USER ID                       
         MVC   WORK+8(2),TGUSER                                                 
         GOTO1 USERVAL,DMCB,(X'A0',WORK)                                        
*&&DO                                                                           
         BE    *+16                                                             
         CLI   ACLFMT,C'X'                                                      
         BNE   NO                                                               
         B     *+12                                                             
         CLI   ACLFMT,C'X'                                                      
         BE    NO                                                               
*&&                                                                             
         MVC   ALUSER,TGUSERID                                                  
         MVC   AIO,AIO2                                                         
         DROP  R4                                                               
*                                                                               
         MVI   ELCODE,TASTELQ      GET STAFF ELEMENT                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TASTD,R4                                                         
*                                                                               
         CLC   TWAORIG,TGUSER      CAN ALWAYS ACCESS YOURSELF                   
         BNE   *+14                                                             
         CLC   TGCTSTAF,TISTAFF                                                 
         BE    GSTF50                                                           
*                                                                               
*&&DO                                                                           
GSTF50   CLI   TYPEFILT,0          IF TYPE FILTER SPECIFIED                     
         BE    *+14                                                             
         CLC   TASTTYPE,TYPEFILT   THEN SKIP DIFFERENT CATEGORIES               
         BNE   NO                                                               
*&&                                                                             
*                                                                               
GSTF50   MVC   ALLAST,TASTLST      MOVE IN LAST NAME                            
         MVC   ALFIRST,TASTFST             FIRST NAME                           
         MVC   ALCAT,TASTTYPE              CATEGORY                             
         GOTO1 STAFVAL,DMCB,TASTTYPE,0                                          
         MVC   ALCATN,TGSTNAME             CATEGORY NAME                        
         DROP  R4,R5                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
* ROUTINE TESTS IF ANY FIELDS ARE SELECTED                                      
*                                                                               
TESTSEL  NTR1                                                                   
         LA    R2,ACLSELH          POINT R2 TO FIRST LIST LINE ON SCRN          
         ZIC   R6,LISTCT                                                        
         B     *+8                                                              
TSEL10   BAS   RE,BUMP2                                                         
         CLI   5(R2),0             CHECK FOR INPUT                              
         BE    TSEL30                                                           
         CLI   8(R2),C'S'          S,C, OR D ARE VALID                          
         BE    TSEL20                                                           
         CLI   8(R2),C'C'                                                       
         BE    TSEL20                                                           
         CLI   8(R2),C'D'                                                       
         BNE   ERRINV                                                           
TSEL20   BAS   RE,UNVIOLAT         IF SELECTED, ERASE VIOLATION ELEMENT         
*                                                                               
TSEL30   BCT   R6,TSEL10                                                        
         B     XIT                                                              
*                                                                               
* ROUTINE ERASES VIOLATION ELEMENT FROM SYSTEM RECORD                           
*                                                                               
UNVIOLAT NTR1                                                                   
         LR    R5,R2               POINT R5 TO THE SELECT FIELD                 
         ZIC   R1,0(R2)            BUMP R5 TO THE LIST LINE                     
         AR    R5,R1                                                            
         AHI   R5,8                ADVANCE PAST FIELD HEADER                    
         USING LISTD,R5                                                         
*                                                                               
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0)                                    
         BE    *+6                 GET SYSTEM RECORD IN AIO AGAIN               
         DC    H'00'                                                            
         L     R4,AIO              LOOK FOR ACCESS VIOLATION ELEMENT            
         MVI   ELCODE,TAAVELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
UNV4     BAS   RE,NEXTEL                                                        
         BNE   NTFOUND                                                          
*                                                                               
         USING TAAVD,R4                                                         
         OC    ALLINE,SPACES                                                    
         CLC   TAAVLINE,ALLINE     MATCH ON LINE ID                             
         BNE   UNV4                                                             
         CLC   TAAVADDR,ALLINE+4   AND ADDRESS                                  
         BNE   UNV4                                                             
*                                                                               
         MVI   TAAVEL,X'FF'        SET TO DELETE ELEMENT                        
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 PUTREC              WRITE BACK SYSTEM RECORD                     
         MVI   MYMODE,1            UPDATE LIST AND SEND RESTORED MSG            
         B     XIT                                                              
*                                                                               
         DROP  R4,R5                                                            
         EJECT                                                                  
         SPACE 1                                                                
*                                                                               
* ROUTINE RESETS SCREEN AFTER USER HITS ENTER                                   
*                                                                               
RESET    NTR1                                                                   
         CLI   MYMODE,1            IF RESTORING LIST AFTER A RECORD IS          
         BNE   RESET10             SELECTED, CALCULATE WHERE TO START           
         ZIC   RE,ELCOUNT          RE-LISTING ELEMENTS FROM THE SYSTEM          
         ZIC   RF,LISTCT           RECORD                                       
         SR    RE,RF                                                            
         STC   RE,ELCOUNT                                                       
RESET10  MVI   LISTCT,0            START COUNTING WITH 0                        
         TWAXC ACLSELH,PROT=Y      CLEAR ALL LIST LINES                         
         GOTO1 RECVAL,DMCB,TLSYCDQ,(X'30',0)                                    
         BE    *+6                  GET SYSTEM RECORD IN AIO AGAIN              
         DC    H'00'                                                            
         B     XIT                                                              
*                                                                               
* ROUTINE BUMPS 2 SCREEN FIELDS                                                 
*                                                                               
BUMP2    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               ADVANCE TO NEXT LIST LINE ON SCREEN          
         BR    RE                                                               
*                                                                               
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERRXIT                                                           
         SPACE 1                                                                
NTFOUND  MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     ERRXIT                                                           
         SPACE 1                                                                
RESTORED MVI   MYMODE,0                                                         
         MVI   MYMSGNO1,12         ACCESS RESTORED                              
         OI    GENSTAT2,USGETTXT                                                
         MVI   8(R2),C' '                                                       
         OI    6(R2),X'80'                                                      
         B     ERRXIT                                                           
         SPACE 1                                                                
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
* APPLICATION STORAGE PUT AT END OF TWA                                         
*                                                                               
WORKD    DSECT                                                                  
RDSEQ    DS    CL1                 READ WITH ACTIVE/PASSIVE POINTER             
MYUSER   DS    XL2                 USER ID FOR FILTERING                        
TYPEFILT DS    C                   STAFF TYPE FOR FILTERING                     
STARTAT  DS    CL16                START AT KEY                                 
PRCOUNT  DS    PL4                 RECORD COUNT FOR REPORTS                     
MYSTAFF  DS    CL8                 STAFF ID                                     
MYMODE   DS    X                   1=UPDATE LIST AFTER SELECT                   
ELCOUNT  DS    X                   ELEMENT COUNT                                
LISTCT   DS    X                   LIST COUNT                                   
LISTMAX  EQU   15                  MAXIMUM NUMBERS OF LINES IN LIST             
         EJECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
LISTD    DSECT                                                                  
ALUSER   DS    CL6                 USER ID                                      
         DS    XL3                                                              
ALSTAFF  DS    CL8                 STAFF ID                                     
         DS    XL3                                                              
ALLAST   DS    CL12                LAST NAME                                    
         DS    XL2                                                              
ALFIRST  DS    CL12                FIRST NAME                                   
         DS    XL3                                                              
ALLINE   DS    CL8                 LINE ID/ADDRESS                              
         DS    XL2                                                              
ALCAT    DS    CL1                 CATEGORY                                     
         DS    XL1                                                              
ALCATN   DS    CL14                CATEGORY NAME                                
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR30D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005TAGEN30   05/01/02'                                      
         END                                                                    
