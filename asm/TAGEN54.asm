*          DATA SET TAGEN54    AT LEVEL 147 AS OF 11/02/16                      
*PHASE T70254B,*                                                                
         TITLE 'T70254 - TRANSFER ADVICE RECORD DETAILS TO PAY SCREEN'          
T70254   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 MYDLNQ,T70254,R6,CLEAR=YES                                       
         LR    R7,RC               R7=A(LOCAL W/S)                              
         USING MYD,R7                                                           
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         LA    R8,TWAHOLE          R8=A(PAY STORAGE)                            
         USING PAYD,R8                                                          
         EJECT                                                                  
*              MAIN CONTROL                                                     
         SPACE 1                                                                
         OC    SVADVDA,SVADVDA     IF HAVE ADVICE RECORD                        
         BZ    MAIN50                                                           
         TM    PAYMODE,DRAFT       IF NOT DRAFT MODE                            
         BO    XIT                                                              
         MVC   SVINVDA,DMDSKADD    SAVE D/A OF INVOICE RECORD                   
         BRAS  RE,GETADV           READ ADVICE RECORD (AIO2)                    
         MVC   AIO,AIO2                                                         
         SPACE 1                                                                
         BRAS  RE,CMPADV           COMPLETE ADVICE                              
         SPACE 1                                                                
         BRAS  RE,MRKADV           MARK ADVICE WITH A= INFO                     
         TM    LCLSTAT3,SPLIT      IF SPLIT ADVICE RECORD                       
         BZ    *+8                                                              
         BAS   RE,CPYSI            COPY TASI ELEMENTS TO INVOICE REC            
         SPACE 1                                                                
         BAS   RE,CPYDETS          COPY ADVICE DETAIL RECORDS TO INV            
         SPACE 1                                                                
         BRAS  RE,RERDINV          RE-READ INVOICE RECORD                       
         B     XIT                                                              
         SPACE 2                                                                
*                                  R2=A(FIELD WITH A=(ADVICE CODE))             
MAIN50   BRAS  RE,PARSE            PARSE INPUT FIELD                            
         BNE   ERRINV                                                           
         SPACE 1                                                                
         BAS   RE,ADVREAD          READ ADVICE RECORD                           
         SPACE 1                                                                
         BRAS  RE,GENEXT           EXTRACT GENERAL DETAILS                      
         SPACE 1                                                                
         BRAS  RE,USEEXT           EXTRACT USE SPECIFIC DETAILS                 
         SPACE 1                                                                
         CLI   ORIGSCR,TADVTYPB                                                 
         BNE   ADVEXT              GIVE ADVICE DETAILS EXTRACTED MSG            
         B     XIT                                                              
         EJECT                                                                  
*              COPY TASI ELEMENTS FROM ADVICE TO INVOICE                        
*              AIO1 = A(INVOICE RECORD)                                         
CPYSI    NTR1                                                                   
         L     R4,AIO1             R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TASIELQ      IF TASID ELEMENTS ON INVOICE                 
         BAS   RE,GETEL                                                         
         BE    CPYSI20             THEY PREVAIL                                 
*                                  ELSE, FIND TASID ELEMENTS ON ADVICE          
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TASIELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CPYSI10  BAS   RE,NEXTEL                                                        
         BNE   CPYSI20                                                          
         MVC   ELEMENT,0(R4)       COPY ELEMENT FROM ADVICE RECORD              
         GOTO1 ADDELEM             TO INVOICE RECORD                            
         B     CPYSI10                                                          
*                                                                               
CPYSI20  L     R4,AIO1             R4=A(INVOICE RECORD)                         
         MVI   ELCODE,TAINELQ                                                   
         USING TAIND,R4                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TAINSTA2,TAINSPRM   SET PRIMARY INVOICE STATUS                   
         B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO COPY ADVICE DETAILS RECORDS TO INVOICE                
         SPACE                                                                  
CPYDETS  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'C0',TYPEV)  EXIT IF NO DETAILS            
         GOTO1 HIGH                               ATTACHED TO ADVICE            
         CLC   KEY(TLCMLEV-TLCMKEY),KEYSAVE                                     
         BNE   CDX                                                              
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TLCMD,R4                                                         
         L     R4,AIO              BUILD INVOICE COMMENT KEY OVER               
         MVI   TLCMTYP,TLCMTINV    KEY OF ADVICE DETAILS RECORD                 
         XC    TLCMVCID(TLCMLEV-TLCMVCID),TLCMVCID                              
         MVC   TLCMINV,TGINV                                                    
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCMKEY),0(R4)                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BE    CD10                 IF INVOICE DOESN'T ALREADY HAVE             
         GOTO1 ADDREC               THIS LEVEL OF COMMENT, ADD IT               
         B     CD20                                                             
CD10     MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC               ELSE OVERWRITE IT                           
         SPACE 1                                                                
CD20     CLI   TLCMLEV,TLCMTPC      IF DETAILS RECORD WAS TP LEVEL              
         BE    CDX                  WE'RE DONE                                  
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCMCDQ,(X'C0',TYPEV)                                
         LA    R4,KEY                                                           
         MVI   TLCMLEV,TLCMTPC      OTHERWISE READ FOR TP LEVEL                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BNE   CDX                                                              
         L     R4,AIO                                                           
         GOTO1 GETREC               BUILD INVOICE COMMENT KEY OVER              
         MVI   TLCMTYP,TLCMTINV     KEY OF ADVICE DETAILS RECORD                
         XC    TLCMVCID(TLCMLEV-TLCMVCID),TLCMVCID                              
         MVC   TLCMINV,TGINV                                                    
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLCMKEY),0(R4)                                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BE    CD30                 IF INVOICE DOESN'T ALREADY HAVE             
         GOTO1 ADDREC               TP LEVEL OF COMMENT, ADD IT                 
         B     CDX                                                              
CD30     MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 PUTREC               ELSE OVERWRITE IT                           
CDX      MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 1                                                                
TYPEV    DC    C'V'                                                             
         EJECT                                                                  
*              ROUTINE TO READ/VALIDATE ADVICE RECORD                           
         SPACE 1                                                                
ADVREAD  NTR1                                                                   
         MVI   ERRDISP,2           SET TO POINT TO ADVICE CODE                  
         XC    SVADVDA,SVADVDA     CLEAR D/A OF ADVICE RECORD                   
         SPACE 1                                                                
         MVC   SVCID,TGCID         SAVE ACTUAL CID                              
         MVC   SVVERS,VERSION      AND VERSION LETTER/NUMBER                    
         SPACE 1                                                                
         LH    R3,DSPEST           IF ESTIMATE FIELD DEFINED                    
         LTR   R3,R3                                                            
         BZ    AR10                                                             
         A     R3,ATWA                                                          
         CLC   =C'C=',8(R3)        LOOK FOR CID OVERRIDE                        
         BNE   AR10                                                             
         MVC   TGCID,10(R3)        AND MOVE OVERRIDE TO GLOBAL                  
         OC    TGCID,SPACES                                                     
         SPACE 1                                                                
AR10     GOTO1 RECVAL,DMCB,TLDVCDQ,(X'20',0)  READ ADVICE RECORD                
         MVC   TGCID,SVCID                                                      
         BNE   ERRXIT                                                           
*                                                                               
         USING TLDRD,R4                                                         
         LA    R4,KEY                                                           
         MVC   SVADVDA2,TLDRDA     SAVE D/A OF ADVICE RECORD                    
         DROP  R4                                                               
*                                                                               
         XC    ATANXEL,ATANXEL                                                  
         L     R4,AIO                                                           
         MVI   ELCODE,TANXELQ      SAVE ADDRESS OF                              
         BAS   RE,GETEL            NETWORK TRANSFER DETAILS ELEMENT             
         BNE   *+8                                                              
         ST    R4,ATANXEL                                                       
         SPACE 1                                                                
AR20     XC    PAYLFT,PAYLFT       INITIALIZE VERSION/LIFT INFORMATION          
         MVI   PAYLFTH+5,0                                                      
         OI    PAYLFTH+6,X'80'                                                  
         MVI   LIFT,0                                                           
         MVI   VERSION,0                                                        
         SPACE 1                                                                
         L     R4,AIO              GET VERSION OR LIFT ELEMENT                  
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   AR100                                                            
         USING TAFND,R4                                                         
         L     R4,TGELEM                                                        
         MVC   PAYLFT(1),TAFNNAME  DISPLAY VERSION OR LIFT LETTER               
         MVC   VERSION,TAFNNAME    SAVE VER/LFT LETTER INTO VERSION             
         MVI   PAYLFTH+5,1                                                      
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      IF ADVICE HAS LIFT ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   AR30                                                             
         MVC   LIFT,VERSION        SAVE LETTER INTO LIFT INSTEAD                
         MVI   VERSION,0           OF VERSION                                   
         B     AR100                                                            
         SPACE 1                                                                
AR30     TM    TGSYSTAT,TASYS3VR   IF WE THINK WE HAVE A VERSION                
         BZ    AR40                AND SYSTEM SET TO HANDLE 3-CHAR              
         EDIT  VERSION,PAYLFT,ALIGN=LEFT   VERSION CODES, DISPLAY               
         STC   R0,PAYLFTH+5                THE 3-CHAR NUMBER                    
         SPACE 1                                                                
         USING TAVRD,R4                                                         
AR40     MVC   AIO,AIO3            THEN GO GET COMMERCIAL RECORD                
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'20',0)                                   
         BNE   AR90                                                             
         SPACE 1                                                                
         L     R4,AIO3                                                          
         SPACE 1                                                                
         TM    TGSYSTAT,TASYS3VR   IF SYSTEM SET TO HANDLE 3-CHAR               
         BZ    AR70                VERSION CODES                                
         CLI   VERSION,26          AND VERSION CODE IS GREATER                  
         BNH   AR70                THAN 26                                      
         SPACE 1                                                                
         USING VINDEXD,RE                                                       
         LA    RE,VERINDEX         FIND RECORD EQUATE FOR THIS                  
AR50     CLI   0(RE),X'FF'         VERSION NUMBER                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLC   VERSION,VINDUPLM                                                 
         BNH   AR60                                                             
         LA    RE,VINDLNQ(RE)                                                   
         B     AR50                                                             
         SPACE 1                                                                
AR60     XC    KEY,KEY              GET COMMERCIAL RECORD FOR                   
         MVC   KEY(L'TLCOKEY),0(R4) THAT VERSION                                
         MVC   KEY+TLCOVER-TLCOD(L'TLCOVER),VINDEQUT                            
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCOKEY),KEYSAVE                                           
         BNE   AR90                                                             
         GOTO1 GETREC                                                           
         DROP  RE                                                               
         SPACE                                                                  
         USING TAVRD,R4                                                         
AR70     MVI   ELCODE,TAVRELQ      MAKE SURE VERSION REALLY EXISTS              
         BAS   RE,GETEL            ON COMMERCIAL RECORD                         
         B     *+8                                                              
AR80     BAS   RE,NEXTEL                                                        
         BNE   AR90                                                             
         CLC   TAVRVERS,VERSION                                                 
         BNE   AR80                                                             
         MVC   PAYLID,TAVRCID       DISPLAY DESCRIPTION                         
         OI    PAYLIDH+6,X'80'                                                  
         MVC   ELTAVR,TAVREL                                                    
         MVC   TCLFTSEC,TAVRSEC     SAVE LENGTH                                 
         MVC   TCVERSEC,TAVRSEC                                                 
         B     AR100                                                            
         DROP  R4                                                               
         SPACE 1                                                                
AR90     MVC   LIFT,VERSION        IF VERSION NOT FOUND ON COMMERCIAL           
         MVI   VERSION,0           SET TO ERROR OUT LATER ON                    
         SPACE 1                                                                
AR100    CLI   SVVERS,0            IF COMMERCIAL REALLY HAS VERSIONS            
         BE    AR105                                                            
         CLI   VERSION,0           BUT ADVICE DOESN'T                           
         BNE   AR105                                                            
         MVC   VERSION,SVVERS      RESTORE VERSION INPUT                        
         EDIT  VERSION,PAYLFT,ALIGN=LEFT                                        
         STC   R0,PAYLFTH+5                                                     
         SPACE 1                                                                
         USING TLDRD,R4                                                         
AR105    LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   TLDRDA,SVADVDA2                                                  
         MVC   AIO,AIO1            RE-READ THE ADVICE RECORD                    
         GOTO1 GETREC                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         TM    PAYMODE,DRAFT                                                    
         BO    AR120                                                            
         SPACE 1                                                                
         USING TAAID,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAAIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   AR120                                                            
         OC    PAYINV,SPACES                                                    
         LA    R3,TAAIINU2                                                      
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BZ    AR110                                                            
         LA    R3,TAAIINUM                                                      
         CLI   TAAILEN,TAAIILNQ                                                 
         BE    AR110                                                            
         CLC   PAYINV,TAAIINUM                                                  
         BL    AIADVERR                                                         
         CLC   PAYINV,TAAIINUX                                                  
         BNH   AR120                                                            
AR110    CLC   PAYINV,0(R3)                                                     
         BNE   AIADVERR                                                         
         DROP  R4                                                               
         SPACE 1                                                                
AR120    L     R4,AIO                                                           
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R4                                                         
*                                                                               
         CLI   TADVTYPE,TADVTYPS   SESSION ADVICES NOT ALLOWED                  
         BE    ERRUSE                                                           
*                                                                               
         MVC   ORIGSCR,TADVTYPE    SAVE ORIGIN SCREEN                           
*                                                                               
         OC    ATANXEL,ATANXEL     IF GENERATED ADVICE                          
         BZ    *+12                                                             
         TM    AGYSTAT3,TAAYSNSD   AND AGENCY ALLOWED TO SEND                   
         BO    *+12                                                             
         TM    TADVSTAT,TADVSRCV   ADVICE MUST HAVE BEEN RECEIVED               
         BZ    ERRRCV                                                           
         SPACE 1                                                                
         TM    TADVSTAT,TADVSPAY   IF PREVIOUSLY PAID VIA A=                    
         BZ    AR130                                                            
         CLI   PFAID,22            TEST OVERRODE ERROR                          
         BNE   ADVPAID                                                          
         SPACE 1                                                                
AR130    MVI   REGOTH,C'N'         SEE IF COMMENT ELEMENT                       
         L     R4,AIO              EXISTS FOR REGULAR USE                       
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPU))                                     
         BNE   *+8                                                              
         MVI   REGOTH,C'Y'                                                      
         SPACE 1                                                                
         MVI   MUSOTH,C'N'         SEE IF COMMENT ELEMENT                       
         L     R4,AIO              EXISTS FOR MUSIC USE                         
         MVI   ELCODE,TACMELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPM))                                     
         BNE   *+8                                                              
         MVI   MUSOTH,C'Y'                                                      
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVUELQ      GET ADVICE USE DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R4,ATAVUEL          SAVE ITS ADDRESS                             
         MVC   TGDATE,TAVUCYCS                                                  
         SPACE 1                                                                
         MVI   TWOUSES,C'N'        INITIALIZE TWO USES VARIABLE                 
         SPACE 1                                                                
         LHI   RE,0                                                             
         CLI   TAVUUSE,0           IF ANY TWO OF USE1                           
         BE    *+8                                                              
         AHI   RE,1                                                             
         CLI   TAVUUSE2,0          USE2                                         
         BE    *+8                                                              
         AHI   RE,1                                                             
         CLI   REGOTH,C'Y'         REGULAR COMMENT                              
         BNE   *+8                                                              
         AHI   RE,1                                                             
         CLI   MUSOTH,C'Y'         OR MUSIC COMMENT PRESENT                     
         BNE   *+8                                                              
         AHI   RE,1                                                             
         CHI   RE,2                                                             
         BNE   *+8                                                              
         MVI   TWOUSES,C'Y'        THEN SET TWO USES ON ADVICE                  
         SPACE 1                                                                
         MVI   SECUSE,C'Y'         IF USE BEING PAID MATCHES                    
         CLC   TGUSEQU,TAVUUSE2    ADVICE'S MUSIC USE                           
         BE    AR160               SET PAYING SECOND USE                        
         SPACE 1                                                                
         MVI   SECUSE,C'N'         IF USE BEING PAID MATCHES                    
         CLC   TGUSEQU,TAVUUSE     ADVICE'S REGULAR USE                         
         BE    AR160               SET NOT PAYING SECOND USE                    
         SPACE 1                                                                
         TM    TGUSSTA3,CBLUSE     IF PAYING CABLE OR SPANISH CABLE             
         BZ    AR140                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAMTELQ      AND CABLE SYSTEMS ON ADVICE                  
         BAS   RE,GETEL                                                         
         L     R4,ATAVUEL                                                       
         BNE   AR140                                                            
         CLI   TAVUUSE,ULCB        OK IF ADVICE IS LOCAL CABLE                  
         BE    AR160                                                            
         SPACE 1                                                                
AR140    CLI   REGOTH,C'Y'         IF REGULAR OTHER COMMENT                     
         BNE   AR150               EXISTS                                       
         CLI   TGUSEQU,UOTH        AND PAYING OTHER USE                         
         BE    AR160               SET NOT PAYING SECOND USE                    
         SPACE 1                                                                
*        TM    TGUSXUNI,AFM        IF REGULAR OTHER COMMENT                     
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BO    AR160               EXISTS AND NOT PAYING OTHER                  
         B     ERRUSE              USE, CANNOT PAY MUSIC USE                    
         SPACE 1                                                                
AR150    MVI   SECUSE,C'Y'         IF ADVICE USE DOESN'T MATCH                  
         CLI   MUSOTH,C'Y'         PAYING USE AT THIS POINT AND NO              
         BNE   ERRUSE              MUSIC OTHER COMMENT EXISTS ERROR             
         SPACE 1                                                                
         CLI   TGUSEQU,UOTH        IF MUSIC OTHER COMMENT EXISTS                
         BE    AR160               AND NOT PAYING OTHER USE, CANNOT             
*        TM    TGUSXUNI,AFM        BE PAYING A REGULAR USE                      
         GOTO1 UNITEST,DMCB,TGUSXUNS,AFM,0,0,0                                  
         BO    ERRUSE                                                           
         SPACE 1                                                                
AR160    MVI   ERRDISP,0           CLEAR ERROR FIELD DISPLACEMENT               
         L     R4,AIO                                                           
         MVI   ELCODE,TASIELQ      IF SUBSIDIARY INVOICE ELEMENTS               
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    LCLSTAT3,SPLIT      SET ON SPLIT INDICATOR                       
         SPACE 1                                                                
         MVI   SECUPGR,C'N'                                                     
         L     R4,AIO                                                           
         MVI   ELCODE,TAUPELQ                                                   
         BAS   RE,GETEL                                                         
         BE    AR170                                                            
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,ATAVUEL          SAVE ITS ADDRESS                             
         CLI   TGUSEQU,UCBL                                                     
         BNE   *+12                                                             
         CLI   TAVUTYPE,UCBLUPG                                                 
         BE    AR170                                                            
         SPACE 1                                                                
         CLI   TGUSEQU,USWS                                                     
         BNE   *+12                                                             
         CLI   TAVUTYPE,USWSUPG                                                 
         BE    AR170                                                            
         SPACE 1                                                                
         CLI   TGUSEQU,USCB                                                     
         BNE   *+12                                                             
         CLI   TAVUTYPE,USCBUPG                                                 
         BE    AR170                                                            
         SPACE 1                                                                
         CLI   TGUSEQU,UWSC                                                     
         BNE   *+12                                                             
         CLI   TAVUTYPE,UWSCUPG                                                 
         BE    AR170                                                            
         DROP  R4                                                               
         SPACE 1                                                                
         CLC   TGDATE,TCTAUHEL+TAUHSTRT-TAUHD                                   
         BNE   AR180                                                            
         L     R4,AIO                                                           
         MVI   ELCODE,TAMTELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   AR180                                                            
AR170    MVI   SECUPGR,C'Y'        SET ADVICE IS AN UPGRADE                     
         SPACE 1                                                                
AR180    LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   SVADVDA,TLDRDA      SAVE D/A OF ADVICE RECORD                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 1                                                                
ERRMIS   MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     ERRXIT                                                           
         SPACE 1                                                                
ERRINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     ERRXIT                                                           
         SPACE 1                                                                
ERRRCV   MVI   ERROR,ERNOTRCV      ADVICE NOT YET RECEIVED                      
         B     ERRXIT                                                           
         SPACE 1                                                                
ADVPAID  MVI   ERROR,ERADVPAY      ADVICE HAS ALREADY BEEN PAID                 
         B     ERRXIT                                                           
         SPACE 1                                                                
ERRUSE   MVI   ERROR,ERADVUSE      ADVICE NOT FOR USE BEING PAID                
         B     ERRXIT                                                           
         SPACE 1                                                                
ADVEXT   MVI   MYMSGNO1,84         ADVICE DETAILS EXTRACTED                     
         B     INFXIT                                                           
         SPACE 1                                                                
AIADVERR MVC   MYMSGNO,=Y(ERRAIADV) INV HAS DIFF ADVICE ASSIGNED TO IT          
         B     NTHEEND                                                          
         SPACE 1                                                                
NTHEEND  OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     ERRXIT                                                           
         SPACE 1                                                                
INFXIT   OI    GENSTAT2,USGETTXT                                                
ERRXIT   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              TABLE TO DETERMINE WHICH COMMERCIAL RECORD THE                   
*              VERSION CODE IS ON                                               
         SPACE 1                                                                
VERINDEX DC    X'1A',AL1(TLCOV026)                                              
         DC    X'78',AL1(TLCOV120)                                              
         DC    X'D2',AL1(TLCOV210)                                              
         DC    X'FA',AL1(TLCOV250)                                              
         DC    X'FF'                                                            
         SPACE 3                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE EXTRACTS GENERAL PAYMENT DETAILS                         
         SPACE 1                                                                
GENEXT   NTR1  BASE=*,LABEL=*                                                   
         LH    R3,DSPAUTH          AUTH/PO                                      
         LTR   R3,R3                                                            
         BZ    GEN10                                                            
         A     R3,ATWA                                                          
         GOTO1 CHAROUT,DMCB,TANUELQ,(R3),TANUTAUT                               
         NI    4(R3),X'DF'         FORCE RE-VALIDATION                          
         SPACE 1                                                                
GEN10    LH    R3,DSPEST           ESTIMATE                                     
         LTR   R3,R3                                                            
         BZ    GEN20                                                            
         A     R3,ATWA                                                          
         GOTO1 CHAROUT,DMCB,TANUELQ,(R3),TANUTEST                               
         NI    4(R3),X'DF'         FORCE RE-VALIDATION OF FIELD                 
         SPACE 1                                                                
         TM    LCLSTAT3,SPLIT      IF SPLIT INVOICE                             
         BZ    GEN20                                                            
         MVC   8(16,R3),=CL16'SPLIT' INDICATE SO                                
         OI    6(R3),X'80'                                                      
         SPACE 1                                                                
GEN20    L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         SPACE 1                                                                
         LH    R3,DSPPD            ESTIMATE PERIOD                              
         LTR   R3,R3                                                            
         BZ    GEN40                                                            
         A     R3,ATWA                                                          
         OI    6(R3),X'80'                                                      
         NI    4(R3),X'DF'         FORCE RE-VALIDATION                          
         EDIT  (1,TAVUESPD),(2,8(R3)),ZERO=BLANK,ALIGN=LEFT                     
         SPACE 1                                                                
GEN40    LH    R3,DSPCYC           CYCLE DATES                                  
         LTR   R3,R3                                                            
         JZ    XIT                                                              
         A     R3,ATWA                                                          
         OI    6(R3),X'80'                                                      
         NI    4(R3),X'DF'         FORCE RE-VALIDATION                          
         SPACE 1                                                                
         LA    R5,TAVUCYC         DEFAULT CYCLE DATE TO REGULAR                 
         SPACE 1                                                                
         CLI   SECUSE,C'Y'        IF SECOND USE BEING PAID                      
         BNE   GEN40B                                                           
         CLI   TAVULEN,27         AND NOT OLD ADVICE                            
         BL    GEN40B                                                           
         OC    TAVUMCYS,TAVUMCYS  AND MUSIC CYCLE DATES PRESENT                 
         BZ    GEN40B                                                           
         LA    R5,TAVUMCYS        USE MUSIC CYLE DATES                          
         SPACE 1                                                                
GEN40B   OC    0(L'TAVUCYC,R5),0(R5)  IF NO CYCLE DATES                         
         JZ    XIT                    SKIP THIS DISPLAY                         
         SPACE 1                                                                
         OC    3(L'TAVUCYCE,R5),3(R5) MAKE DIFFERENT DATCON CALL                
         BZ    GEN40C                 IF CYCLE END DATE NOT PRESENT             
         CLI   TAVULEN,27                                                       
         BE    GEN40C                                                           
         GOTO1 DATCON,DMCB,(X'11',0(R5)),(8,8(R3))                              
         MVI   5(R3),17                                                         
         J     XIT                                                              
GEN40C   GOTO1 DATCON,DMCB,(1,0(R5)),(8,8(R3))                                  
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO READ ADVICE RECORD INTO AIO2                          
         SPACE                                                                  
GETADV   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   TLDRDA,SVADVDA      ADVICE DISK ADDRESS                          
         MVC   AIO,AIO2            SET AIO FOR ADVICE                           
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TLDVD,RE                                                         
         L     RE,AIO              IF CORRECT ADVICE NUMBER IS NOT              
         CLC   TLDVADV,TGADV       AT THE SAVED ADVICE ADDRESS                  
         BE    *+6                 TRAP IT!                                     
         DC    H'00'                                                            
         DROP  RE                                                               
         SPACE 1                                                                
         GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE POINTERS                        
         MVC   AIO,AIO1            RESET AIO (INVOICE RECORD)                   
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE EXTRACTS AND DISPLAYS USE SPECIFIC DETAILS               
         SPACE 1                                                                
USEEXT   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,DVUSES           R2=A(USE SPECIFIC TABLE)                     
         USING DVUSESD,R2                                                       
*                                                                               
USEEXT5  CLI   0(R2),X'FF'                                                      
         BE    USEEXTX                                                          
         CLC   TGUSEQU,DVUSE       MATCH ON USE TYPE                            
         BE    *+12                                                             
         LA    R2,DVUSELNQ(R2)                                                  
         B     USEEXT5                                                          
         XR    RF,RF                                                            
         ICM   RF,3,DVUSERTN       GO TO SPECIFIC USE ROUTINE                   
         BZ    USEEXTX                                                          
         AR    RF,RB                                                            
         LA    RE,USEEXTX                                                       
         NTR1                                                                   
         BR    RF                  TO DISPLAY BOTTOM HALF OF PAY SCREEN         
*                                                                               
USEEXTX  J     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT CLA DETAILS                                   
         SPACE 1                                                                
CLAEXT   DS    0H                                                               
         LA    R3,CLAFRSTH         R3=A(1ST SET OF CLA SCREEN DETAILS)          
         USING CLAD,R3                                                          
         TWAXC (R3)                TRANSMIT ALL                                 
         SPACE 1                                                                
         LA    R0,20               R0=MAX N'USES ON PAY SCREEN                  
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TANPELQ      GET CLA PROGRAM DETAILS ELEMENTS             
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CLA10    BRAS  RE,NEXTEL                                                        
         BNE   CLAX                                                             
         USING TANPD,R4            R4=A(CLA PROGRAM DETAILS EL.)                
         SPACE 1                                                                
         OC    STUSE,STUSE         IF STARTING USE NUMBER DEFINED               
         BZ    *+14                                                             
         CLC   TANPUSEN,STUSE      INSURE WE'VE REACHED IT                      
         BL    CLA10                                                            
         SPACE 1                                                                
         OC    ENDUSE,ENDUSE       IF ENDING USE NUMBER DEFINED                 
         BZ    *+14                                                             
         CLC   TANPUSEN,ENDUSE     THEN SKIP THOSE AFTER IT                     
         BH    CLA10                                                            
         SPACE 1                                                                
         CLC   TANPDATE,=C'TBA'    USE DATE                                     
         BNE   CLA20                                                            
         OC    ACURFORC,ACURFORC   IF NONE THEN IF HAVEN'T DONE IT YET          
         BNZ   CLA30                                                            
         LA    R1,CLADATEH         SET TO PLACE CURSOR HERE                     
         ST    R1,ACURFORC                                                      
         B     CLA30                                                            
         SPACE 1                                                                
CLA20    GOTO1 DATCON,DMCB,(1,TANPDATE),(8,CLADATE)                             
         SPACE 1                                                                
CLA30    MVC   CLAPROG,TANPPNME    PROGRAM NAME                                 
         SPACE 1                                                                
         MVC   CLALIFT,TANPLFT     LIFT                                         
         SPACE 1                                                                
         MVC   CLANWK,TANPNWK      NETWORK                                      
         SPACE 1                                                                
         LA    R3,CLANEXT          BUMP TO NEXT SET OF CLA DETAILS              
         BCT   R0,CLA10                                                         
         SPACE 1                                                                
CLAX     B     USEEXTX                                                          
         EJECT                                                                  
*              ROUTINE TO EXTRACT WSP DETAILS                                   
         SPACE 1                                                                
WSPEXT   DS    0H                                                               
         LA    R3,WSP8WH           R3=A(THIS LINE)                              
         TWAXC (R3)                TRANSMIT ALL                                 
         SPACE 1                                                                
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         SPACE 1                                                                
         CLI   SECUSE,C'Y'         CALL USEVAL TO GET WEEKS FOR                 
         BE    WSPE20              ADVICE'S TYPE                                
         GOTO1 USEVAL,DMCB,(X'80',TAVUUSE),(X'80',TAVUTYPE)                     
         B     WSPE30                                                           
WSPE20   GOTO1 USEVAL,DMCB,(X'80',TAVUUSE2),(X'80',TAVUTYP2)                    
WSPE30   LA    R2,WSP8WH                                                        
         BRAS  RE,CHK8WK           SET Y IF 8 WEEKS                             
         LA    R2,WSP13WH                                                       
         BAS   RE,CHK13WK          SET Y IF 13 WEEKS                            
         GOTO1 USEVAL,DMCB,(X'40',CONREC)    RESET USEVAL VARIABLES             
         SPACE 1                                                                
WSPE40   LA    R2,WSPFRSTH       FILL IN TMKT/RMKT FIELDS                       
         BRAS  RE,POPNELIM       AND CALCULATE UNITS                            
         BE    WSPE50                                                           
         SPACE 1                                                                
         EDIT  TAVUUNT,(4,WSPUNIT),ALIGN=LEFT,ZERO=NOBLANK                      
         SPACE 1                                                                
WSPE50   CLI   SECUPGR,C'Y'                                                     
         BNE   WSPE60                                                           
         CLC   TAVUCYCS,TCTAUHEL+TAUHSTRT-TAUHD                                 
         BNE   WSPE60                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   WSPE60                                                           
         CLI   VERSION,0                                                        
         BNE   WSPE70                                                           
         L     R4,ATAVUEL                                                       
         MVC   TAVUMAJ,TCTAUHEL+TAUHMAJ-TAUHD                                   
         SPACE 1                                                                
WSPE60   L     R4,ATAVUEL                                                       
         TM    TAVUMAJ,NY          SET MAJORS                                   
         BZ    *+12                                                             
         MVI   WSPNY,C'Y'                                                       
         MVI   WSPNYH+5,1                                                       
         TM    TAVUMAJ,LA                                                       
         BZ    *+12                                                             
         MVI   WSPLA,C'Y'                                                       
         MVI   WSPLAH+5,1                                                       
         TM    TAVUMAJ,CHI                                                      
         BZ    *+12                                                             
         MVI   WSPCHI,C'Y'                                                      
         MVI   WSPCHIH+5,1                                                      
         DROP  R4                                                               
         SPACE 1                                                                
WSPE70   CLI   SECUPGR,C'Y'        IF ADVICE IS UPGRADE (PREVIOUS INFO)         
         BNE   USEEXTX                                                          
         MVI   WSPUPGR,C'Y'        SET UPGRADE INDICATOR                        
         MVI   WSPUPGRH+5,1                                                     
         B     USEEXTX                                                          
         EJECT                                                                  
*              MUSIC EXTENTION SCREENS                                          
         SPACE 2                                                                
FMUEXT   DS    0H                                                               
         LA    R3,FMU8WH           R3=A(THIS LINE)                              
         TWAXC (R3)                TRANSMIT ALL                                 
         GOTOR DISPTYP,WORK,('UFMU8W',FMU8W),('UFMU13W',FMU13W),       X        
               ('UFMU6M',FMU6M),(X'FF',0)                                       
         B     USEEXTX                                                          
         SPACE 2                                                                
FGMEXT   DS    0H                                                               
         LA    R3,FGMUK12H                                                      
         TWAXC (R3)                                                             
         GOTOR DISPTYP,WORK,('UFGMEU12',FGMUK12),                      +        
               ('UFGMEU24',FGMUK24),('UFGMNE12',FGMNE12),              +        
               ('UFGMNE24',FGMNE24),('UFGMWO12',FGMWO12),              +        
               ('UFGMWO24',FGMWO24),(X'FF',0)                                   
         B     USEEXTX                                                          
         SPACE 2                                                                
NBMEXT   DS    0H                                                               
         LA    R3,NBM8WKH                                                       
         TWAXC (R3)                                                             
         GOTOR DISPTYP,WORK,('UNBM1YR',NBM1YR),('UNBM2YR',NBM2YR),     +        
               ('UNBM8WK',NBM8WK),(X'FF',0)                                     
         B     USEEXTX                                                          
         SPACE 2                                                                
NIMEXT   DS    0H                                                               
         LA    R3,MVM8WKH                                                       
         TWAXC (R3)                                                             
         GOTOR DISPTYP,WORK,('UNIM8WK',MVM8WK),('UNIM6MO',MVM6MO),     +        
               ('UNIM1YR',MVM1YR),(X'FF',0)                                     
         B     USEEXTX                                                          
         SPACE 2                                                                
MUSEXT   DS    0H                                                               
         BRAS  RE,MUSTEXTN                                                      
         B     USEEXTX                                                          
         SPACE 2                                                                
MBOEXT   DS    0H                                                               
         BRAS  RE,MBOTEXTN                                                      
         B     USEEXTX                                                          
         SPACE 2                                                                
         EJECT                                                                  
*              CABLE EXTENTION SCREENS                                          
         SPACE 2                                                                
CBLEXT   DS    0H                                                               
         BRAS  RE,CBLEXTN                                                       
         B     USEEXTX                                                          
         SPACE 2                                                                
LCBEXT   DS    0H                                                               
         GOTO1 FLDVAL,DMCB,(X'02',LCBL50H),(X'80',999)                          
         SPACE 1                                                                
         GOTOR DISPTYP,WORK,('ULCB50',LCB50),('ULCB100',LCB100),       X        
               ('ULCB150',LCB150),('ULCB200',LCB200),                  X        
               ('ULCB250',LCB250),('ULCB500',LCB500),                  X        
               ('ULCB750',LCB750),('ULCB1M',LCB1MIL),                  X        
               ('ULCBMAX',LCBMAX),('ULCBU12',LCB100),                  X        
               ('ULCBU13',LCB150),('ULCBU23',LCB150),                  X        
               ('ULCBU14',LCB200),('ULCBU24',LCB200),                  X        
               ('ULCBU34',LCB200),('ULCBU15',LCB250),                  X        
               ('ULCBU25',LCB250),('ULCBU35',LCB250),                  X        
               ('ULCBU45',LCB250),('ULCBU45',LCB250),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('ULCBU16',LCB500),('ULCBU26',LCB500),     X        
               ('ULCBU36',LCB500),('ULCBU46',LCB500),                  X        
               ('ULCBU56',LCB500),('ULCBU17',LCB750),                  X        
               ('ULCBU27',LCB750),('ULCBU37',LCB750),                  X        
               ('ULCBU47',LCB750),('ULCBU57',LCB750),                  X        
               ('ULCBU67',LCB750),('ULCBU18',LCB1MIL),                 X        
               ('ULCBU28',LCB1MIL),('ULCBU38',LCB1MIL),                X        
               ('ULCBU48',LCB1MIL),('ULCBU58',LCB1MIL),                X        
               ('ULCBU68',LCB1MIL),('ULCBU78',LCB1MIL),(X'FF',0)                
         GOTOR DISPTYP,WORK,('ULCBU19',LCBMAX),('ULCBU29',LCBMAX),     X        
               ('ULCBU39',LCBMAX),('ULCBU49',LCBMAX),                  X        
               ('ULCBU59',LCBMAX),('ULCBU69',LCBMAX),                  X        
               ('ULCBU79',LCBMAX),('ULCBU89',LCBMAX),                  X        
               ('ULCBUMAX',LCBMAX),(X'FF',0)                                    
         CLI   SECUPGR,C'Y'                                                     
         BNE   USEEXTX                                                          
         MVI   LCBUPGR,C'Y'                                                     
         B     USEEXTX                                                          
         SPACE 2                                                                
CABEXT   DS    0H                                                               
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         CLI   TAVUTYPE,0          IF NOT AN UPGRADE                            
         BNE   CABEXT5                                                          
         GOTO1 USEVAL,DMCB,(X'80',TAVUUSE),(X'80',TAVUTYPE)                     
         LA    R2,CAB4WH                                                        
         BAS   RE,CHK4WK           SET Y IF 4 WEEKS                             
         LA    R2,CAB13WH                                                       
         BAS   RE,CHK13WK          SET Y IF 13 WEEKS                            
         LA    R2,CAB52W                                                        
         BRAS  RE,CHK52WK          SET Y IF 52 WEEKS                            
         GOTO1 USEVAL,DMCB,(X'40',CONREC)    RESET USEVAL VARIABLES             
         B     USEEXTX                                                          
*                                                                               
CABEXT5  GOTOR DISPTYP,WORK,('UCABU413',CAB413),('UCABU13',CAB1352),   X        
               ('UCABU452',CAB452),(X'FF',0)                                    
         B     USEEXTX                                                          
         DROP  R4                                                               
         SPACE 2                                                                
LOCEXT   DS    0H                                                               
         LA    R3,LOCBH            R3=A(THIS LINE)                              
         TWAXC (R3)                TRANSMIT ALL                                 
         GOTOR DISPTYP,WORK,('ULOCB',LOCB),('ULOCBNY',LOCBNY),         X        
               ('ULOCC',LOCC),(X'FF',0)                                         
         B     USEEXTX                                                          
         EJECT                                                                  
*              DEALER EXTENTION SCREEN                                          
         SPACE 2                                                                
DLREXT   DS    0H                                                               
***      LA    R3,DLRAH            R3=A(THIS LINE)                              
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         LA    R3,DLR8WH           R3=A(THIS LINE)                              
         TWAXC (R3)                TRANSMIT ALL                                 
         LA    R2,DLR8WH                                                        
         BRAS  RE,CHK8WK           SET Y IF 8 WEEKS                             
         CLI   8(R2),C'Y'                                                       
         BE    DLREXT8                                                          
         LA    R2,DLR6MH                                                        
         MVI   8(R2),C'Y'                                                       
         MVI   5(R2),1                                                          
         GOTOR DISPTYP,WORK,('UDLRA',DLRA),('UDLRANY',DLRANY),         X        
               ('UDLRB',DLRB),('UDLRBNY',DLRBNY),(X'FF',0)                      
         B     USEEXTX                                                          
DLREXT8  GOTOR DISPTYP,WORK,('UDLRA8',DLRA),('UDLRANY8',DLRANY),       X        
               ('UDLRB8',DLRB),('UDLRBNY8',DLRBNY),(X'FF',0)                    
         B     USEEXTX                                                          
         EJECT                                                                  
*              FOREIGN SCREEN                                                   
         SPACE 2                                                                
FGREXTR  DS    0H                                                               
         LA    R3,FGRUKH                                                        
         TWAXC (R3)                                                             
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         CLI   TAVUTYPE,UFGRMAJ                                                 
         BE    FGREXT10                                                         
         CLI   TAVUTYPE,UFGREXT                                                 
         BE    FGREXT10                                                         
         GOTOR DISPTYP,WORK,('UFGRUK',FGRUK),('UFGREUR',FGREUR),       +        
               ('UFGRJAP',FGRJAP),('UFGRAP',FGRAP),                    +        
               ('UFGRWOR',FGRWOR),('UFGRWIDE',FGRWIDE),                +        
               ('UFGRRAD',FGRRAD),(X'FF',0)                                     
         B     USEEXTX                                                          
FGREXT10 TM    TAVUMAJ,UFGRRAD                                                  
         BZ    *+8                                                              
         MVI   FGRRAD,C'Y'                                                      
         CLI   TAVUMAJ,WWIDE                                                    
         BE    *+12                                                             
         CLI   TAVUMAJ,WWIDE+UFGRRAD                                            
         BNE   FGREXT20                                                         
         MVI   FGRWIDE,C'Y'                                                     
         B     USEEXTX                                                          
FGREXT20 TM    TAVUMAJ,UK                                                       
         BZ    *+8                                                              
         MVI   FGRUK,C'Y'                                                       
         TM    TAVUMAJ,EUR                                                      
         BZ    *+8                                                              
         MVI   FGREUR,C'Y'                                                      
         TM    TAVUMAJ,JAP                                                      
         BZ    *+8                                                              
         MVI   FGRJAP,C'Y'                                                      
         TM    TAVUMAJ,AP                                                       
         BZ    *+8                                                              
         MVI   FGRAP,C'Y'                                                       
         TM    TAVUMAJ,REST                                                     
         BZ    *+8                                                              
         MVI   FGRWOR,C'Y'                                                      
         CLI   TAVUTYPE,UFGREXT                                                 
         BNE   USEEXTX                                                          
         MVI   FGREXT,C'Y'                                                      
         B     USEEXTX                                                          
         EJECT                                                                  
*              INDUSTRIAL SCREEN                                                
INREXT   DS    0H                                                               
         LA    R3,INR30DH                                                       
         TWAXC (R3)                                                             
         GOTOR DISPTYP,WORK,('UINR30D',INR30D),('UINRUNL',INRUNL),     +        
               ('USIN30D',INR30D),('USINUNL',INRUNL),(X'FF',0)                  
         B     USEEXTX                                                          
         EJECT                                                                  
*              RADIO NETWORK SCREEN                                             
RNTEXT   DS    0H                                                               
         LA    R3,RNT1WH                                                        
         TWAXC (R3)                                                             
         GOTOR DISPTYP,WORK,('URNT1W',RNT1W),                          +        
               ('URNT4W',RNT4W),('URNT8W',RNT8W),                      +        
               ('URNT13W',RNT13W),('URNT26U',RNT26U),                  +        
               ('URNT39U',RNT39U),('URNTAB',RNTAB),(X'FF',0)                    
         B     USEEXTX                                                          
         EJECT                                                                  
*              REGIONAL NETWORK SCREEN                                          
RRNEXT   DS    0H                                                               
         LA    R3,RRNNONEH                                                      
         TWAXC (R3)                                                             
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         CLI   TAVUMAJ,0                                                        
         BNE   *+8                                                              
         MVI   RRNNONE,C'Y'                                                     
         TM    TAVUMAJ,NY                                                       
         BZ    *+8                                                              
         MVI   RRNNY,C'Y'                                                       
         TM    TAVUMAJ,LA                                                       
         BZ    *+8                                                              
         MVI   RRNLA,C'Y'                                                       
         TM    TAVUMAJ,CHI                                                      
         BZ    USEEXTX                                                          
         MVI   RRNCHI,C'Y'                                                      
         B     USEEXTX                                                          
         DROP  R4                                                               
         EJECT                                                                  
*              ADDENDUM COMBINED/WILDSPOT SCREEN                                
         SPACE 1                                                                
ADCEXT   DS    0H                                                               
         LA    R3,ADCUNITH                                                      
         TWAXC (R3)                                                             
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         EDIT  TAVUUNT,(4,ADCUNIT),ALIGN=LEFT,ZERO=NOBLANK                      
         SPACE 1                                                                
         LA    R3,ELTACO                                                        
         USING TACOD,R3                                                         
         CLC   TACOADST,=C'TX'                                                  
         BE    USEEXTX                                                          
         SPACE 1                                                                
         CLI   TAVUTYPE,UADC3D                                                  
         BNE   ADCEXT10                                                         
         MVI   ADCTTY1,C'Y'                                                     
         B     USEEXTX                                                          
         SPACE 1                                                                
ADCEXT10 CLI   TAVUTYPE,UADC1W                                                  
         BNE   ADCEXT20                                                         
         CLC   TACOADST,=C'KS'                                                  
         BNE   *+12                                                             
         MVI   ADCTTY1,C'Y'                                                     
         B     USEEXTX                                                          
         MVI   ADCTTY2,C'Y'                                                     
         B     USEEXTX                                                          
         SPACE 1                                                                
ADCEXT20 CLI   TAVUTYPE,UADC4W                                                  
         BNE   ADCEXT30                                                         
         MVI   ADCTTY3,C'Y'                                                     
         B     USEEXTX                                                          
         SPACE 1                                                                
ADCEXT30 CLI   TAVUTYPE,UADC13W                                                 
         BNE   ADCEXT40                                                         
         CLC   TACOADST,=C'KS'                                                  
         BNE   *+12                                                             
         MVI   ADCTTY3,C'Y'                                                     
         B     USEEXTX                                                          
         MVI   ADCTTY4,C'Y'                                                     
         B     USEEXTX                                                          
         SPACE 1                                                                
ADCEXT40 CLI   TAVUTYPE,UADC31D                                                 
         BNE   USEEXTX                                                          
         MVI   ADCTTY2,C'Y'                                                     
         B     USEEXTX                                                          
         DROP  R3,R4                                                            
         EJECT                                                                  
*              ADDENDUM WILDSPOT                                                
         SPACE 1                                                                
ADWEXT   DS    0H                                                               
         BRAS  RE,ADWEXTN                                                       
         B     USEEXTX                                                          
         EJECT                                                                  
ACBEXT   DS    0H                                                               
         GOTO1 FLDVAL,DMCB,(X'02',LCBL50H),(X'80',999)                          
         SPACE 1                                                                
         GOTOR ADDCBL                                                           
         SPACE 1                                                                
         CLI   SECUPGR,C'Y'                                                     
         BNE   USEEXTX                                                          
         MVI   LCBUPGR,C'Y'                                                     
         B     USEEXTX                                                          
         SPACE 2                                                                
*              SPANISH WILDSPOT SCREEN                                          
         SPACE 1                                                                
SWSEXT   DS    0H                                                               
         LA    R3,SWSUNITH                                                      
         TWAXC (R3)                                                             
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         EDIT  TAVUUNT,(4,SWSUNIT),ALIGN=LEFT,ZERO=NOBLANK                      
         CLI   SECUPGR,C'Y'        IF ADVICE IS UPGRADE (PREVIOUS INFO)         
         BNE   *+8                                                              
         MVI   SWSUPGR,C'Y'        SET UPGRADE INDICATOR                        
         B     USEEXTX                                                          
         DROP  R4                                                               
         EJECT                                                                  
*              SPANISH NETWORK/WILDSPOT COMBINED SCREEN                         
         SPACE 1                                                                
SNWEXT   DS    0H                                                               
         LA    R3,SNWUNITH                                                      
         TWAXC (R3)                                                             
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         EDIT  TAVUUNT,(4,SNWUNIT),ALIGN=LEFT,ZERO=NOBLANK                      
         CLI   TAVUTYPE,USNWUN                                                  
         BNE   *+8                                                              
         MVI   SNWUPGN,C'Y'                                                     
         CLI   TAVUTYPE,USNWUW                                                  
         BNE   *+8                                                              
         MVI   SNWUPGW,C'Y'                                                     
         CLI   TAVUTYPE,USNWUC                                                  
         BNE   USEEXTX                                                          
         MVI   SNWUPGC,C'Y'                                                     
         B     USEEXTX                                                          
         DROP  R4                                                               
         SPACE 1                                                                
SFREXT   DS    0H                                                               
         LA    R3,SFRSFAH          R3=A(THIS LINE)                              
         TWAXC (R3)                TRANSMIT ALL                                 
         GOTOR DISPTYP,WORK,('USFRA',SFRSFA),('USFRB',SFRSFB),         X        
               ('USFRC',SFRSFC),(X'FF',0)                                       
         B     USEEXTX                                                          
         EJECT                                                                  
*              ITN SCREEN                                                       
         SPACE 1                                                                
ITNEXT   DS    0H                                                               
         LA    R3,ITNAUTOH                                                      
         TWAXC (R3)                                                             
         SPACE                                                                  
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         SPACE                                                                  
         OC    TAVUUSES,TAVUUSES   IF NO ITN USES SPECIFIED                     
         BNZ   ITNEXT10                                                         
         MVI   ITNAUTO,C'X'        CHECK OFF AUTO                               
         B     USEEXTX                                                          
         SPACE                                                                  
ITNEXT10 EDIT  TAVUUNT,ITNEUSE,ALIGN=LEFT                                       
         EDIT  TAVUUSES,ITNNUSE,ALIGN=LEFT                                      
         B     USEEXTX                                                          
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO EXTRACT INTERNET/NEW MEDIA DETAILS                    
         SPACE 1                                                                
INMEXT   DS    0H                                                               
         LA    R3,IRNFRSTH                                                      
         TWAXC (R3)                TRANSMIT ALL FIELDS                          
         B     MOV10                                                            
         SPACE 2                                                                
*              ROUTINE TO EXTRACT NEW INTERNET/NEWMEDIA DETAILS                 
         SPACE 1                                                                
INUEXT   DS    0H                                                               
         LA    R3,INUI4WH                                                       
         TWAXC (R3)                TRANSMIT ALL FIELDS                          
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         GOTOR DISPTYP,WORK,('UINUI4W',INUI4W),('UINUR4W',INUI4W),     X        
               ('UINUI8W',INUI8W),('UINUR8W',INUI8W),                  X        
               ('UINUI1Y',INUI1Y),('UINUR1Y',INUI1Y),                  X        
               ('UINUEXT',INUE1Y),('UINUREX',INUE1Y),,(X'FF',0)                 
         MVI   INURADH+5,0                                                      
         OI    INURADH+6,X'80'                                                  
         MVI   INURAD,0                                                         
         CLI   TAVUTYPE,UINUI4W                                                 
         BE    INVEXT10                                                         
         CLI   TAVUTYPE,UINUR8W                                                 
         BL    INVEXT10                                                         
         MVI   INURADH+5,1                                                      
         MVI   INURAD,C'X'                                                      
         DROP  R4                                                               
         SPACE 1                                                                
INVEXT10 LA    R3,INUFRSTH                                                      
         B     MOV10                                                            
         SPACE 2                                                                
*              ROUTINE TO EXTRACT MOVE TO INTERNET/NEWMEDIA DETAILS             
         SPACE 1                                                                
MOVEXT   DS    0H                                                               
         LA    R3,MOVI4WH                                                       
         TWAXC (R3)                TRANSMIT ALL FIELDS                          
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         GOTOR DISPTYP,WORK,('UMVII4W',MOVI4W),('UMVII8W',MOVI8W),     X        
               ('UMVII1Y',MOVI1Y),('UMVIEXT',MOVE1Y),(X'FF',0)                  
         DROP  R4                                                               
         SPACE 1                                                                
         LA    R3,MOVFRSTH                                                      
         SPACE 1                                                                
         USING TAMDD,R4                                                         
MOV10    L     R4,AIO                                                           
         MVI   ELCODE,TAMDELQ      GET INTERNET/NEWMEDIA ELEMENTS               
         BRAS  RE,GETEL            AND COPY TO SCREEN                           
         B     *+8                                                              
MOV20    BRAS  RE,NEXTEL                                                        
         BNE   USEEXTX                                                          
         SPACE 1                                                                
         ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
         SPACE 1                                                                
         MVC   8(L'TAMDCODE,R3),TAMDCODE                                        
         SPACE 1                                                                
         ZIC   RE,0(R3)                                                         
         AR    R3,RE                                                            
         B     MOV20                                                            
         SPACE 1                                                                
         EJECT                                                                  
*              UNITS/UPGRADE SCREEN                                             
         SPACE 1                                                                
UUEXT    DS    0H                                                               
         LA    R3,SWSUNITH                                                      
         TWAXC (R3)                                                             
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         EDIT  TAVUUNT,(4,SWSUNIT),ALIGN=LEFT,ZERO=NOBLANK                      
         CLI   SECUPGR,C'Y'        IF ADVICE IS UPGRADE (PREVIOUS INFO)         
         BNE   *+8                                                              
         MVI   SWSUPGR,C'Y'        SET UPGRADE INDICATOR                        
         B     USEEXTX                                                          
         DROP  R4                                                               
         EJECT                                                                  
         USING TAVUD,R4            R4=A(ADVICE USE DETAILS)                     
*                                  R2=A(SCREEN FIELD)                           
         SPACE 2                                                                
*              ROUTINE TO SET Y IF 4 WEEKS                                      
         SPACE 1                                                                
CHK4WK   DS    0H                                                               
         LA    R1,TAVUWKS          DEFAULT TO REGULAR TAVUWKS                   
         SPACE 1                                                                
         CLI   SECUSE,C'N'         IF PAYING SECOND USE                         
         BE    CHK4WK05                                                         
         OC    TAVUMWKS,TAVUMWKS   AND MUSIC WEEKS IS SET                       
         BZ    CHK4WK05                                                         
         LA    R1,TAVUMWKS         RESET TO TAVUMWKS                            
         SPACE 1                                                                
CHK4WK05 CLC   =C'4 ',0(R1)        DETERMINE CYCLE LENGTH                       
         BE    *+12                BY WHAT IS IN TAVUWKS                        
         CLC   =C'04',0(R1)                                                     
         BNER  RE                                                               
         MVI   8(R2),C'Y'                                                       
         MVI   5(R2),1                                                          
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET Y IF 13 WEEKS                                     
         SPACE 1                                                                
CHK13WK  DS    0H                                                               
         LA    R1,TAVUWKS          DEFAULT TO REGULAR TAVUWKS                   
         CLI   SECUSE,C'N'         IF PAYING SECOND USE                         
         BE    CHK13W05                                                         
         OC    TAVUMWKS,TAVUMWKS   AND MUSIC WEEKS IS SET                       
         BZ    CHK13W05                                                         
         LA    R1,TAVUMWKS         RESET TO TAVUMWKS                            
         SPACE 1                                                                
CHK13W05 CLC   =C'13',0(R1)                                                     
         BNER  RE                                                               
         MVI   8(R2),C'Y'                                                       
         MVI   5(R2),1                                                          
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
*              TABLE TO DISPLAY SPECIFIC USE INFORMATION                        
         SPACE 1                                                                
DVUSES   DS    0XL3                                                             
         DC    AL1(UWSP),AL2(WSPEXT-USEEXT)                                     
         DC    AL1(UCBL),AL2(CBLEXT-USEEXT)                                     
         DC    AL1(ULCB),AL2(CBLEXT-USEEXT)                                     
         DC    AL1(ULOC),AL2(LOCEXT-USEEXT)                                     
         DC    AL1(UDLR),AL2(DLREXT-USEEXT)                                     
         DC    AL1(UFGR),AL2(FGREXTR-USEEXT)                                    
         DC    AL1(UINR),AL2(INREXT-USEEXT)                                     
         DC    AL1(URNT),AL2(RNTEXT-USEEXT)                                     
         DC    AL1(URRN),AL2(RRNEXT-USEEXT)                                     
         DC    AL1(UADC),AL2(ADCEXT-USEEXT)                                     
         DC    AL1(UADW),AL2(ADWEXT-USEEXT)                                     
         DC    AL1(UACB),AL2(ACBEXT-USEEXT)                                     
         DC    AL1(USWS),AL2(CBLEXT-USEEXT)                                     
         DC    AL1(USNW),AL2(SNWEXT-USEEXT)                                     
         DC    AL1(USCB),AL2(CBLEXT-USEEXT)                                     
         DC    AL1(USIN),AL2(INREXT-USEEXT)                                     
         DC    AL1(USFR),AL2(SFREXT-USEEXT)                                     
         DC    AL1(UNET),AL2(UUEXT-USEEXT)                                      
         DC    AL1(UWSC),AL2(CBLEXT-USEEXT)                                     
         DC    AL1(UWSM),AL2(UUEXT-USEEXT)                                      
         DC    AL1(UCLA),AL2(CLAEXT-USEEXT)                                     
         DC    AL1(ULNA),AL2(CLAEXT-USEEXT)                                     
         DC    AL1(ULNN),AL2(CLAEXT-USEEXT)                                     
         DC    AL1(ULNC),AL2(CLAEXT-USEEXT)                                     
         DC    AL1(ULNF),AL2(CLAEXT-USEEXT)                                     
         DC    AL1(UPAX),AL2(CLAEXT-USEEXT)                                     
         DC    AL1(UCAB),AL2(CABEXT-USEEXT)                                     
         DC    AL1(UITN),AL2(ITNEXT-USEEXT)                                     
         DC    AL1(UIRN),AL2(INMEXT-USEEXT)                                     
         DC    AL1(UINU),AL2(INUEXT-USEEXT)                                     
         DC    AL1(USIR),AL2(INMEXT-USEEXT)                                     
         DC    AL1(USIU),AL2(INUEXT-USEEXT)                                     
         DC    AL1(UNMR),AL2(INMEXT-USEEXT)                                     
         DC    AL1(UNMU),AL2(INUEXT-USEEXT)                                     
         DC    AL1(USNM),AL2(INMEXT-USEEXT)                                     
         DC    AL1(USNU),AL2(INUEXT-USEEXT)                                     
         DC    AL1(UMVI),AL2(MOVEXT-USEEXT)                                     
         DC    AL1(USMI),AL2(MOVEXT-USEEXT)                                     
         DC    AL1(UMVN),AL2(MOVEXT-USEEXT)                                     
         DC    AL1(USMN),AL2(MOVEXT-USEEXT)                                     
         DC    AL1(UMUS),AL2(MUSEXT-USEEXT)                                     
         DC    AL1(UFMU),AL2(FMUEXT-USEEXT)                                     
         DC    AL1(UFGM),AL2(FGMEXT-USEEXT)                                     
         DC    AL1(UNBM),AL2(NBMEXT-USEEXT)                                     
         DC    AL1(UNIM),AL2(NIMEXT-USEEXT)                                     
         DC    AL1(UMBO),AL2(MBOEXT-USEEXT)                                     
         DC    X'FF'                                                            
                                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              MUSIC EXTENSION SCREEN                                           
         SPACE 1                                                                
MUSTEXTN NTR1  BASE=*,LABEL=*                                                   
         LA    R3,MUSREUH          R3=A(THIS LINE)                              
         TWAXC (R3)                TRANSMIT ALL                                 
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         CLI   TAVUTYP2,UMUSDSH                                                 
         BNE   MUSEXT10                                                         
         MVI   MUSLTS,C'Y'                                                      
         MVI   MUSDUB,C'Y'                                                      
         MVI   MUS13W,C'Y'                                                      
         J     XIT                                                              
MUSEXT10 CLI   TAVUTYP2,UMUSDSH6                                                
         BNE   MUSEXT20                                                         
         MVI   MUSLTS,C'Y'                                                      
         MVI   MUSDUB,C'Y'                                                      
         MVI   MUS6M,C'Y'                                                       
         J     XIT                                                              
MUSEXT20 CLI   TAVUTYP2,UMUSDSH8                                                
         BNE   MUSEXT30                                                         
         MVI   MUSLTS,C'Y'                                                      
         MVI   MUSDUB,C'Y'                                                      
         MVI   MUS8W,C'Y'                                                       
         J     XIT                                                              
MUSEXT30 CLI   TAVUTYP2,UMUSDSH1                                                
         BNE   MUSEXT40                                                         
         MVI   MUSLTS,C'Y'                                                      
         MVI   MUSDUB,C'Y'                                                      
         MVI   MUS1Y,C'Y'                                                       
         J     XIT                                                              
MUSEXT40 CLI   TAVUTYP2,UMUS2DS                                                 
         BNE   MUSEXT50                                                         
         MVI   MUSLTS2,C'Y'                                                     
         MVI   MUSDUB,C'Y'                                                      
         MVI   MUS13W,C'Y'                                                      
         J     XIT                                                              
MUSEXT50 GOTOR DISPTYP,WORK,('UMUSDUB',MUS13W),('UMUSDUB8',MUS8W),     X        
               ('UMUS13W',MUS13W),('UMUS8W',MUS8W),('UMUSNEW',MUS13W), X        
               ('UMUSNEW8',MUS8W),('UMUSDUB6',MUS6M),                  X        
               ('UMUSDUB1',MUS1Y),(X'FF',0)                                     
                                                                                
         GOTOR DISPTYP,WORK,('UMUSDUB',MUSDUB),('UMUSDUB8',MUSDUB),    X        
               ('UMUS13W',MUSREU),('UMUS8W',MUSREU),('UMUSNEW',MUSNEW),X        
               ('UMUSNEW8',MUSNEW),('UMUSDUB6',MUSDUB),                X        
               ('UMUSDUB1',MUSDUB),(X'FF',0)                                    
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ALL MEDIA BUYOUT EXTENSION SCREEN                                
         SPACE 1                                                                
MBOTEXTN NTR1  BASE=*,LABEL=*                                                   
         LA    R3,MBOINIH                                                       
         TWAXC (R3)                                                             
         GOTOR DISPTYP,WORK,('UMBOI1Y',MBOINI),('UMBOR1Y',MBOREU),     +        
               (X'FF',0)                                                        
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              CABLE EXTENTION SCREENS                                          
         SPACE 2                                                                
CBLEXTN  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,CBLUNITH         R3=A(THIS LINE)                              
         TWAXC (R3)                TRANSMIT ALL                                 
         SPACE 1                                                                
         LA    R2,CBLFRSTH       FILL IN TMKT/RMKT FIELDS                       
         BRAS  RE,POPNELIM       AND CALCULATE UNITS                            
         BE    CBL60                                                            
         SPACE 1                                                                
         USING TAVUD,R4                                                         
CBL50    L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         BRAS  RE,LCBOVR                                                        
         BE    CBL60                                                            
         EDIT  TAVUUNT,(4,CBLUNIT),ALIGN=LEFT,ZERO=NOBLANK                      
         DROP  R4                                                               
         SPACE 1                                                                
CBL60    CLI   SECUPGR,C'Y'        IF ADVICE IS UPGRADE (PREVIOUS INFO)         
         JNE   XIT                                                              
         MVI   CBLUPGR,C'Y'        SET UPGRADE INDICATOR                        
         MVI   CBLUPGRH+5,1                                                     
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ADDENDUM WILDSPOT EXTENSION SCREENS                              
         SPACE 2                                                                
ADWEXTN  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ADWUNITH                                                      
         TWAXC (R3)                                                             
         SPACE 1                                                                
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
         SPACE 1                                                                
ADWEXT05 LA    R3,ELTACO                                                        
         USING TACOD,R3                                                         
         CLC   TACOADST,=C'TX'                                                  
         BE    ADWEXT30                                                         
         SPACE 1                                                                
         CLC   TACOADST,=C'KS'                                                  
         BE    ADWEXT10                                                         
         CLC   TACOADST,=C'NW'                                                  
         BE    ADWEXT15                                                         
         GOTOR DISPTYP,WORK,('UADW3D',ADWTTY1),('UADWU3D',ADWTTY1),    +        
               ('UADW1W',ADWTTY2),('UADWU1W',ADWTTY2),                 +        
               ('UADW4W',ADWTTY3),('UADWU4W',ADWTTY3),                 +        
               ('UADW13W',ADWTTY4),('UADWU13W',ADWTTY4),(X'FF',0)               
         B     ADWEXT20                                                         
ADWEXT10 GOTOR DISPTYP,WORK,('UADW1W',ADWTTY1),('UADWU1W',ADWTTY1),    +        
               ('UADW13W',ADWTTY3),('UADWU13W',ADWTTY3),               +        
               ('UADW31D',ADWTTY2),('UADWU31D',ADWTTY2),(X'FF',0)               
         B     ADWEXT20                                                         
ADWEXT15 GOTOR DISPTYP,WORK,('UADW2W',ADWTTY1),('UADWU2W',ADWTTY1),    +        
               ('UADW13W',ADWTTY2),('UADWU13W',ADWTTY2),(X'FF',0)               
         B     ADWEXT20                                                         
ADWEXT20 CLI   SECUPGR,C'Y'                                                     
         BNE   ADWEXT30                                                         
         MVI   ADWUPGR,C'Y'                                                     
         MVI   ADWUPGRH+5,1                                                     
         DROP  R3,R4                                                            
         SPACE 1                                                                
ADWEXT30 LA    R2,ADWFRSTH       FILL IN TMKT/RMKT FIELDS                       
         BRAS  RE,POPNELIM       AND CALCULATE UNITS                            
         JE    XIT                                                              
         SPACE 1                                                                
         USING TAVUD,R4                                                         
ADWEXT80 L     R4,ATAVUEL                                                       
         EDIT  TAVUUNT,(4,ADWUNIT),ALIGN=LEFT                                   
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO POPULATE CNET/MKT/CSYS SCREEN FIELDS                  
*              WITH CNET/MKT/CSYS CODES FROM TAMTD ELEMENTS                     
*              THEN ELIMINATE ONES ALREADY PAID IN THIS CYCLE                   
*              ON ENTRY ... R2=A(FIRST CNET/MKT/CSYS FIELD)                     
         SPACE 1                                                                
POPNELIM NTR1  BASE=*,LABEL=*                                                   
         NI    PAYSTAT1,X'FF'-ELIMINTD                                          
         MVI   INVASSGN,C'N'                                                    
         SPACE 1                                                                
         USING TADVD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TADVELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         CLI   TADVTYPE,TADVTYPB                                                
         JNE   NO                                                               
         DROP  R4                                                               
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAAIELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   INVASSGN,C'Y'                                                    
         SPACE 1                                                                
         USING TAVUD,R4                                                         
         L     R4,AIO           SAVE ADVICE CYCLE START DATE                    
         MVI   ELCODE,TAVUELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   TGDATE,TAVUCYCS                                                  
         MVC   CYCEND,TAVUCYCE                                                  
         DROP  R4                                                               
         SPACE 1                                                                
         USING TAMTD,R4                                                         
PNE04    L     R4,AIO            EXIT IF NO TAMTD ELEMENTS                      
         MVI   ELCODE,TAMTELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   PNE170                                                           
         SPACE 1                                                                
         LA    R0,MAXMKT         28 CNET/MKT/CSYS SCREEN FIELDS                 
         SPACE 1                                                                
         MVI   TGMTTYPE,TANPNET  PREPARE TO READ CNET RECORDS                   
         TM    TGUSSTA3,CBLUSE   IF USE IS CABLE OR SPANISH CABLE               
         BNZ   PNE20                                                            
         SPACE 1                                                                
         MVI   TGMTTYPE,C'S'     CSYS RECORDS                                   
         CLI   TGUSEQU,ULCB      IF USE IS LOCAL CABLE                          
         BE    PNE20                                                            
         SPACE 1                                                                
         MVI   TGMTTYPE,C'T'     ELSE, READ TMKT RECORDS                        
         CLI   TGMEEQU,RADIO     OR RMKTS RECORDS BASED ON MEDIA                
         BNE   PNE20                                                            
         MVI   TGMTTYPE,C'R'                                                    
         B     PNE20                                                            
         SPACE 1                                                                
PNE10    BRAS  RE,NEXTEL                                                        
         BNE   PNE170                                                           
         SPACE 1                                                                
PNE20    STH   R0,FLDCNTR                                                       
         SPACE 1                                                                
         OC    STCODE,STCODE     IF NOT STARTING WITH FIRST                     
         BZ    PNE30             CNET/MKT/CSYS, FIND STARTING CODE              
         XC    DUB,DUB                                                          
         MVC   DUB(L'TAMTCODE),TAMTCODE                                         
         OC    DUB(L'TAMTCODE),SPACES                                           
         CLC   DUB(L'TAMTCODE),STCODE                                           
         BL    PNE10                                                            
         SPACE 1                                                                
PNE30    LA    RE,CBLFRSTH       RE=A(FIRST CNET/MKT/CSYS HEADER)               
         CLI   TWASCR,SCR5F                                                     
         BE    PNE40                                                            
         LA    RE,WSPFRSTH                                                      
         CLI   TWASCR,SCR55                                                     
         BE    PNE40                                                            
         LA    RE,ADWFRSTH                                                      
PNE40    CR    R2,RE             IF CURRENT CNET/MKT/CSYS ALREADY               
         BNH   PNE50             ON SCREEN, DON'T ENTER IT AGAIN                
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         CLC   TAMTCODE,8(RE)                                                   
         BE    PNE160                                                           
         ZIC   RF,0(RE)                                                         
         AR    RE,RF                                                            
         B     PNE40                                                            
         SPACE 1                                                                
PNE50    XC    TGMTINTC,TGMTINTC                                                
         LHI   R0,TLMTCDQ                                                       
         BRAS  RE,USEALPH        GET INTERNAL CODE FOR CNET/MKT/CSYS            
         BNE   *+8               IN THE ELEMENT                                 
         LHI   R0,TLMTALDQ                                                      
         GOTO1 RECVAL,DMCB,(R0),(X'80',TAMTCODE)                                
         BE    PNE60                                                            
         CLI   TGMTTYPE,TANPNET                                                 
         BNE   PNE60                                                            
         MVI   TGMTTYPE,C'S'                                                    
         GOTO1 RECVAL,DMCB,(R0),(X'80',TAMTCODE)                                
         MVI   TGMTTYPE,TANPNET                                                 
         SPACE 1                                                                
PNE60    OC    TGMTINTC,TGMTINTC IF CNET/MKT/CSYS EXISTS                        
         BZ    PNE140                                                           
         SPACE 1                                                                
         MVI   TGBYTE,UHCUR      SET TRIED CURRENT AGENCY/COMM'L                
         SPACE 1                                                                
         USING TLUHPD,R3                                                        
         LA    R3,KEY            READ ALL USAGE HISTORY KEYS                    
         XC    TLUHPKEY,TLUHPKEY FOR THIS CNET/MKT/CSYS ON THIS                 
         MVI   TLUHPCD,TLUHMTDQ  COMMERCIAL                                     
         MVC   TLUHMINM,TGMTINTC                                                
         MVC   TLUHMCOM,TGCOM                                                   
PNE70    GOTO1 HIGH                                                             
         B     PNE90                                                            
PNE80    GOTO1 SEQ                                                              
PNE90    CLC   KEY(TLUHMSEQ-TLUHPCD),KEYSAVE                                    
         BE    PNE100                                                           
         SPACE 1                                                                
         USING TAOCD,RF                                                         
         CLI   TGBYTE,UHPRE      IF NONE FOUND, SEARCH PREVIOUS                 
         BE    PNE140            AGENCY/COMM'L                                  
         MVI   TGBYTE,UHPRE      IF WE HAVE IT                                  
         OC    ELTAOC,ELTAOC                                                    
         BZ    PNE140                                                           
         LA    RF,ELTAOC                                                        
         TM    TAOCSTAT,TAOCSFRO                                                
         BZ    PNE140                                                           
         XC    KEY,KEY                                                          
         MVI   TLUHPCD,TLUHMTDQ                                                 
         MVC   TLUHMINM,TGMTINTC                                                
         MVC   TLUHMCOM,TAOCCOM                                                 
         B     PNE70                                                            
         DROP  RF                                                               
         SPACE 1                                                                
PNE100   CLC   TAMTCYCS,TLUHMC1D IF USE DATE FITS WITHIN AN EXISTING            
         BL    PNE80             CYCLE FOR THIS CNET/MKT/CSYS ...               
         CLC   TAMTCYCS,TLUHMCYE                                                
         BH    PNE80                                                            
         SPACE 1                                                                
PNE120   CLI   VERSION,0         ... AND THIS IS A NON-VERSION PAYMENT          
         BNE   PNE130                                                           
         TM    TLUHMSTA,TLUHMCRD DISPLAY IF THE PREVIOUS IS A CREDIT            
         BZ    PNE160            ELSE, DON'T DISPLAY IT                         
         B     PNE140                                                           
         SPACE 1                                                                
PNE130   CLC   TLUHMC1D,TAMTCYCS ... AND THIS IS A VERSION PAYMENT              
         BNH   PNE135            DISPLAY IF NOT EARLIER THAN EXISTING           
         CLC   TLUHMC1D,CYCEND   OR IF ADVICE'S CYCLE IS EARLIER THAN           
         BH    PNE160            EXISTING, GO DISPLAY                           
PNE135   CLC   TLUHMVER,VERSION                                                 
         BE    PNE160                                                           
         DROP  R3                                                               
         SPACE 1                                                                
PNE140   ZIC   RE,0(R2)          DISPLAY DATE                                   
         AR    R2,RE                                                            
         GOTO1 DATCON,DMCB,(1,TAMTCYCS),(8,8(R2))                               
         MVI   5(R2),8                                                          
         SPACE 1                                                                
         ZIC   RE,0(R2)          DISPLAY CODE                                   
         AR    R2,RE                                                            
         MVC   8(L'TAMTCODE,R2),TAMTCODE                                        
         MVI   5(R2),L'TAMTCODE                                                 
         SPACE 1                                                                
         USING TAVUD,RE                                                         
         CLI   TWASCR,SCR55                                                     
         BNE   PNE150                                                           
         L     R4,ATAVUEL                                                       
         CLC   TAMTCODE,=X'D5E800000000'                                        
         BNE   *+8                                                              
         OI    TAVUMAJ,NY                                                       
         CLC   TAMTCODE,=X'D3C100000000'                                        
         BNE   *+8                                                              
         OI    TAVUMAJ,LA                                                       
         CLC   TAMTCODE,=X'C3C8C9000000'                                        
         BNE   *+8                                                              
         OI    TAVUMAJ,CHI                                                      
         DROP  RE                                                               
         SPACE 1                                                                
PNE150   ZIC   RE,0(R2)          BUMP TO NEXT FIELD COMBO                       
         AR    R2,RE                                                            
         SPACE 1                                                                
         LH    R0,FLDCNTR                                                       
         BCT   R0,PNE10          AND GO GET NEXT ELEMENT                        
         B     PNE170                                                           
         DROP  R4                                                               
         SPACE 1                                                                
PNE160   OI    PAYSTAT1,ELIMINTD                                                
         LH    R0,FLDCNTR                                                       
         B     PNE10                                                            
         SPACE 1                                                                
PNE170   TM    PAYMODE,DRAFT     IF NOT MAKING A DRAFT PAYMENT                  
         JO    YES                                                              
         BRAS  RE,SETFAL                                                        
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         CLI   5(R2),0           AND ALL CNET/MKT/CSYS WERE DUPLICATES          
         JNE   YES                                                              
         SPACE 1                                                                
         MVC   MYMSGNO,=H'273'                                                  
         CLI   INVASSGN,C'N'                                                    
         BE    PNE175                                                           
         SPACE 1                                                                
         CLI   PFAID,20                                                         
         BE    PNE190                                                           
         MVC   MYMSGNO,=H'258'   GIVE OPTION TO AUTOMATICALLY                   
PNE175   OI    LCLSTAT8,DVDELPND                                                
         MVI   BLOCK,0           DELETE INVOICE/COMPLETE ADVICE                 
         MVI   MYMTYP,GTMINF     BY HITTING PF20                                
         LH    RE,DSPCYC                                                        
         LTR   RE,RE                                                            
         BZ    *+10                                                             
         AR    RE,RA                                                            
         NI    4(RE),X'FF'-X'20'                                                
         OI    GENSTAT2,USGETTXT                                                
         LH    R2,DSPAUTH                                                       
         LTR   R2,R2                                                            
         JZ    ERRXIT                                                           
         AR    R2,RA                                                            
         MVC   8(2,R2),=C'A='                                                   
         MVC   10(L'TGADV,R2),TGADV                                             
         MVI   5(R2),L'TGADV+2                                                  
         OI    6(R2),X'80'                                                      
         J     ERRXIT                                                           
         SPACE 1                                                                
PNE190   NI    LCLSTAT8,ALL-DVDELPND                                            
         BRAS  RE,DELINV         IF PF20 WAS HIT, GO DELETE INVOICE             
         BRAS  RE,DELICMT        ADD INVOICE COMMENT                            
         BRAS  RE,GETADV                                                        
         BRAS  RE,CMPADV         AND ATTEMPT TO COMPLETE ADVICE                 
         BRAS  RE,MRKADV2                                                       
         DC    H'00'                                                            
         SPACE 2                                                                
*              ROUTINE TO SET FIRST AND LAST MKT/CNET/CSYS                      
*              SCREEN FIELDS AND AUTOMATIC SCREEN FIELD                         
*              ON EXIT ... R2=A(FIRST FIELD)                                    
*                          R3=A(LAST FIELD)                                     
*                          RF=A(AUTOMATIC FIELD)                                
*                                                                               
SETFAL   LA    RF,CONTAGH                                                       
         LA    R2,CBLFRSTH-CBLTAGH(RF)                                          
         LA    R3,CBLLSTH-CBLTAGH(RF)                                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         CLI   TWASCR,SCR5F                                                     
         BER   RE                                                               
         SPACE 1                                                                
         LA    RF,CONTAGH                                                       
         LA    R2,WSPFRSTH-WSPTAGH(RF)                                          
         LA    R3,WSPLSTH-WSPTAGH(RF)                                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         CLI   TWASCR,SCR55                                                     
         BER   RE                                                               
         SPACE 1                                                                
         LA    RF,CONTAGH                                                       
         LA    R2,ADWFRSTH-ADWTAGH(RF)                                          
         LA    R3,ADWLSTH-ADWTAGH(RF)                                           
         ZIC   RF,0(R3)                                                         
         AR    R3,RF                                                            
         BR    RE                                                               
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE TO SET Y IF 52 WEEKS                                     
         SPACE 1                                                                
         USING TAVUD,R4                                                         
CHK52WK  NTR1  BASE=*,LABEL=*                                                   
         CLI   TAVULEN,27          IF NOT INTERMEDIATE ADVICE                   
         BE    CHK52W10                                                         
         LA    R1,TAVUWKS          DEFAULT TO REGULAR TAVUWKS                   
         CLI   SECUSE,C'N'         IF PAYING SECOND USE                         
         BE    CHK52W05                                                         
         OC    TAVUMWKS,TAVUMWKS   AND MUSIC WEEKS IS SET                       
         BZ    CHK52W05                                                         
         LA    R1,TAVUMWKS         RESET TO TAVUMWKS                            
         SPACE 1                                                                
CHK52W05 CLC   =C'52',0(R1)                                                     
         B     *+8                                                              
CHK52W10 CLI   TGUSWKS,52                                                       
         JNE   XIT                                                              
         MVI   0(R2),C'Y'                                                       
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO HANDLE ADDENDUM CABLE                                 
         SPACE 1                                                                
ADDCBL   NTR1  BASE=*,LABEL=*                                                   
         GOTOR DISPTYP,WORK,('UACB50',LCB50),('UACB100',LCB100),       X        
               ('UACB150',LCB150),('UACB200',LCB200),                  X        
               ('UACB250',LCB250),('UACB500',LCB500),                  X        
               ('UACB750',LCB750),('UACB1M',LCB1MIL),                  X        
               ('UACBM50',LCB50),('UACBM50',LCBMAX),                   X        
               ('UACBM100',LCB100),('UACBM100',LCBMAX),                X        
               ('UACBM150',LCB150),('UACBM150',LCBMAX),                X        
               ('UACBM200',LCB200),('UACBM200',LCBMAX),                X        
               ('UACBM250',LCB250),('UACBM250',LCBMAX),                X        
               ('UACBM500',LCB500),('UACBM500',LCBMAX),(X'FF',0)                
         GOTOR DISPTYP,WORK,('UACBM750',LCB750),('UACBM750',LCBMAX),   X        
               ('UACBM1M',LCB1MIL),('UACBM1M',LCBMAX),                 X        
               ('UACBU12',LCB100),('UACBU13',LCB150),                  X        
               ('UACBU14',LCB200),('UACBU15',LCB250),                  X        
               ('UACBU15',LCB250),('UACBU16',LCB500),                  X        
               ('UACBU17',LCB750),('UACBU18',LCB1MIL),                 X        
               ('UACBU19',LCB50),('UACBU19',LCBMAX),                   X        
               ('UACBU20',LCB100),('UACBU20',LCBMAX),                  X        
               ('UACBU21',LCB150),('UACBU21',LCBMAX),                  X        
               ('UACBU22',LCB200),('UACBU22',LCBMAX),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('UACBU23',LCB250),('UACBU23',LCBMAX),     X        
               ('UACBU24',LCB500),('UACBU24',LCBMAX),                  X        
               ('UACBU25',LCB750),('UACBU25',LCBMAX),                  X        
               ('UACBU26',LCB1MIL),('UACBU26',LCBMAX),                 X        
               ('UACBU27',LCB150),('UACBU28',LCB200),                  X        
               ('UACBU29',LCB250),('UACBU30',LCB500),                  X        
               ('UACBU31',LCB750),('UACBU32',LCB1MIL),                 X        
               ('UACBU33',LCB50),('UACBU33',LCBMAX),                   X        
               ('UACBU34',LCB100),('UACBU34',LCBMAX),                  X        
               ('UACBU35',LCB150),('UACBU35',LCBMAX),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('UACBU36',LCB200),('UACBU36',LCBMAX),     X        
               ('UACBU37',LCB250),('UACBU37',LCBMAX),                  X        
               ('UACBU38',LCB500),('UACBU38',LCBMAX),                  X        
               ('UACBU39',LCB750),('UACBU39',LCBMAX),                  X        
               ('UACBU40',LCB1MIL),('UACBU40',LCBMAX),                 X        
               ('UACBU41',LCB200),('UACBU43',LCB250),                  X        
               ('UACBU43',LCB500),('UACBU44',LCB750),                  X        
               ('UACBU45',LCB1MIL),                                    X        
               ('UACBU46',LCB50),('UACBU46',LCBMAX),                   X        
               ('UACBU47',LCB100),('UACBU47',LCBMAX),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('UACBU48',LCB150),('UACBU48',LCBMAX),     X        
               ('UACBU49',LCB200),('UACBU49',LCBMAX),                  X        
               ('UACBU50',LCB250),('UACBU50',LCBMAX),                  X        
               ('UACBU51',LCB500),('UACBU51',LCBMAX),                  X        
               ('UACBU52',LCB750),('UACBU52',LCBMAX),                  X        
               ('UACBU53',LCB1MIL),('UACBU53',LCBMAX),                 X        
               ('UACBU54',LCB250),('UACBU55',LCB500),                  X        
               ('UACBU56',LCB750),('UACBU57',LCB1MIL),                 X        
               ('UACBU58',LCB50),('UACBU58',LCBMAX),                   X        
               ('UACBU59',LCB100),('UACBU59',LCBMAX),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('UACBU60',LCB150),('UACBU60',LCBMAX),     X        
               ('UACBU61',LCB200),('UACBU61',LCBMAX),                  X        
               ('UACBU62',LCB250),('UACBU62',LCBMAX),                  X        
               ('UACBU63',LCB500),('UACBU63',LCBMAX),                  X        
               ('UACBU64',LCB750),('UACBU64',LCBMAX),                  X        
               ('UACBU65',LCB1MIL),('UACBU65',LCBMAX),                 X        
               ('UACBU66',LCB500),('UACBU67',LCB750),                  X        
               ('UACBU68',LCB1MIL),                                    X        
               ('UACBU69',LCB50),('UACBU69',LCBMAX),                   X        
               ('UACBU70',LCB100),('UACBU70',LCBMAX),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('UACBU71',LCB150),('UACBU71',LCBMAX),     X        
               ('UACBU72',LCB200),('UACBU72',LCBMAX),                  X        
               ('UACBU73',LCB250),('UACBU73',LCBMAX),                  X        
               ('UACBU74',LCB500),('UACBU74',LCBMAX),                  X        
               ('UACBU75',LCB750),('UACBU75',LCBMAX),                  X        
               ('UACBU76',LCB1MIL),('UACBU76',LCBMAX),                 X        
               ('UACBU77',LCB750),('UACBU78',LCB1MIL),                 X        
               ('UACBU79',LCB50),('UACBU79',LCBMAX),                   X        
               ('UACBU80',LCB100),('UACBU80',LCBMAX),                  X        
               ('UACBU81',LCB150),('UACBU81',LCBMAX),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('UACBU82',LCB200),('UACBU82',LCBMAX),     X        
               ('UACBU83',LCB250),('UACBU83',LCBMAX),                  X        
               ('UACBU84',LCB500),('UACBU84',LCBMAX),                  X        
               ('UACBU85',LCB750),('UACBU85',LCBMAX),                  X        
               ('UACBU86',LCB1MIL),('UACBU86',LCBMAX),                 X        
               ('UACBU87',LCB1MIL),                                    X        
               ('UACBU88',LCB50),('UACBU88',LCBMAX),                   X        
               ('UACBU89',LCB100),('UACBU89',LCBMAX),                  X        
               ('UACBU90',LCB150),('UACBU90',LCBMAX),                  X        
               ('UACBU91',LCB200),('UACBU91',LCBMAX),(X'FF',0)                  
         GOTOR DISPTYP,WORK,('UACBU92',LCB250),('UACBU92',LCBMAX),     X        
               ('UACBU93',LCB500),('UACBU93',LCBMAX),                  X        
               ('UACBU94',LCB750),('UACBU94',LCBMAX),                  X        
               ('UACBU95',LCB1MIL),('UACBU95',LCBMAX),                 X        
               ('UACBU96',LCB50),('UACBU96',LCBMAX),                   X        
               ('UACBU97',LCB100),('UACBU97',LCBMAX),                  X        
               ('UACBU98',LCB150),('UACBU98',LCBMAX),                  X        
               ('UACBU99',LCB200),('UACBU99',LCBMAX),                  X        
               ('UACBU100',LCB250),('UACBU100',LCBMAX),                X        
               ('UACBU101',LCB500),('UACBU101',LCBMAX),(X'FF',0)                
         GOTOR DISPTYP,WORK,('UACBU102',LCB750),('UACBU102',LCBMAX),   X        
               ('UACBU103',LCB1MIL),('UACBU103',LCBMAX),               X        
               ('UACBU104',LCB100),('UACBU104',LCBMAX),                X        
               ('UACBU105',LCB150),('UACBU105',LCBMAX),                X        
               ('UACBU106',LCB200),('UACBU106',LCBMAX),                X        
               ('UACBU107',LCB250),('UACBU107',LCBMAX),                X        
               ('UACBU108',LCB500),('UACBU108',LCBMAX),                X        
               ('UACBU109',LCB750),('UACBU109',LCBMAX),                X        
               ('UACBU110',LCB1MIL),('UACBU110',LCBMAX),               X        
               ('UACBU111',LCB150),('UACBU111',LCBMAX),(X'FF',0)                
         GOTOR DISPTYP,WORK,('UACBU112',LCB200),('UACBU112',LCBMAX),   X        
               ('UACBU113',LCB250),('UACBU113',LCBMAX),                X        
               ('UACBU114',LCB500),('UACBU114',LCBMAX),                X        
               ('UACBU115',LCB750),('UACBU115',LCBMAX),                X        
               ('UACBU116',LCB1MIL),('UACBU116',LCBMAX),               X        
               ('UACBU117',LCB200),('UACBU117',LCBMAX),                X        
               ('UACBU118',LCB250),('UACBU118',LCBMAX),                X        
               ('UACBU119',LCB500),('UACBU119',LCBMAX),                X        
               ('UACBU120',LCB750),('UACBU120',LCBMAX),                X        
               ('UACBU121',LCB1MIL),('UACBU121',LCBMAX),(X'FF',0)               
         GOTOR DISPTYP,WORK,('UACBU122',LCB250),('UACBU122',LCBMAX),   X        
               ('UACBU123',LCB500),('UACBU123',LCBMAX),                X        
               ('UACBU124',LCB750),('UACBU124',LCBMAX),                X        
               ('UACBU125',LCB1MIL),('UACBU125',LCBMAX),               X        
               ('UACBU126',LCB500),('UACBU126',LCBMAX),                X        
               ('UACBU127',LCB750),('UACBU127',LCBMAX),                X        
               ('UACBU128',LCB1MIL),('UACBU128',LCBMAX),               X        
               ('UACBU129',LCB750),('UACBU129',LCBMAX),                X        
               ('UACBU130',LCB1MIL),('UACBU130',LCBMAX),               X        
               ('UACBU131',LCB1MIL),('UACBU131',LCBMAX),(X'FF',0)               
         J     XIT                                                              
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO PUT Y IN CORRECT SCREEN FIELD                         
*                                  NTRY P1 BYTE 0 - TYPE                        
*                                          0-4    - A(FIELD)                    
         SPACE 1                                                                
DISPTYP  NTR1  BASE=*,LABEL=*                                                   
         L     R4,ATAVUEL          R4=A(ADVICE USE DETAILS ELEMENT)             
         USING TAVUD,R4                                                         
*                                                                               
DISPTYP5 CLI   0(R1),X'FF'         TEST END OF LIST                             
         JE    XIT                                                              
         CLI   SECUSE,C'Y'         IF PAYING FIRST USE                          
         BE    *+18                                                             
         CLC   TAVUTYPE,0(R1)      MATCH ON FIRST TYPE                          
         BE    DISPTYP8                                                         
         B     *+14                                                             
         CLC   TAVUTYP2,0(R1)      ELSE MATCH ON SECOND TYPE                    
         BE    DISPTYP8                                                         
DISPTYP7 LA    R1,4(R1)            BUMP TO NEXT PARAMETER                       
         B     DISPTYP5                                                         
*                                                                               
DISPTYP8 L     R2,0(R1)                                                         
         MVI   0(R2),C'Y'          DISPLAY A 'Y' HERE                           
         LR    RE,R2                                                            
         AHI   RE,-3                                                            
         MVI   0(RE),1                                                          
         B     DISPTYP7                                                         
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DELETE INVOICE                                        
         SPACE 1                                                                
DELINV   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO3            AIO3=A(INVOICE RECORD)                       
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'B4',0)                                    
         BE    *+6                                                              
         DC    H'00'                                                            
*        XC    KEY,KEY                                                          
*        MVC   KEY(L'TLINKEY),0(R4)                                             
*        MVI   RDUPDATE,C'Y'                                                    
*        GOTO1 HIGH                                                             
*        CLC   KEY(L'TLINKEY),KEYSAVE                                           
*        BE    *+6                                                              
*        DC    H'00'                                                            
*        MVI   RDUPDATE,C'Y'                                                    
*        GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         SPACE 1                                                                
         USING TLRCD,R4                                                         
         L     R4,AIO3             REREAD INVOICE FOR UPDATE                    
         OI    TLRCSTAT,TLINSDEL   DELETE INVOICE AND PUT IT                    
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         USING TLDRD,R4                                                         
         LA    R4,KEY                                                           
         OI    TLDRSTAT,TLINSDEL   DELETE PRIMARY KEY                           
         GOTO1 WRITE               AND ALL PASSIVE POINTERS                     
         GOTO1 ADDPTRS,DMCB,PTRBLK                                              
         J     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD INVOICE DELETE COMMENT RECORD                     
         SPACE 1                                                                
DELICMT  NTR1  BASE=*,LABEL=*                                                   
         MVI   TGBYTE,C'N'         TGBYTE INDICATES IF COMMENT EXISTS           
         SPACE 1                                                                
         USING TLCMD,R4                                                         
         LA    R4,KEY                                                           
         XC    KEY,KEY             SEE IF COMMENT RECORD EXISTS                 
         MVI   TLCMCD,TLCMCDQ      FOR THIS INVOICE                             
         MVC   TLCMAGY,TGAGY                                                    
         MVI   TLCMTYP,TLCMTINV                                                 
         MVC   TLCMINV,TGINV                                                    
         MVI   TLCMLEV,TLCMTPC                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLCMKEY),KEYSAVE                                           
         BNE   DIC10                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         DROP  R4                                                               
         SPACE 1                                                                
         MVI   TGBYTE,C'Y'                                                      
         B     DIC20                                                            
         SPACE 1                                                                
DIC10    L     R4,AIO              IF COMMENT NOT FOUND, INITIALIZE REC         
         XC    0(255,R4),0(R4)                                                  
         MVC   0(L'TLCMKEY,R4),KEYSAVE                                          
         SPACE 1                                                                
         USING TAXCD,R4                                                         
DIC20    LA    R4,ELEMENT           ADD ELEMENT WITH DELETE REASON              
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAXCEL,TAXCELQ                                                   
         MVI   TAXCLEN,L'DELREAS+L'DELREAS2+4                                   
         MVI   TAXCSEQ,1                                                        
         MVC   TAXCCMNT(L'DELREAS),DELREAS                                      
         MVC   TAXCCMNT+L'DELREAS(L'DELREAS2),DELREAS2                          
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         CLI   TGBYTE,C'N'         IF COMMENT RECORD DOESN'T ALREADY            
         BNE   DIC30               EXIST, ADD IT                                
         GOTO1 ADDREC                                                           
         SPACE 1                                                                
DIC30    CLI   TGBYTE,C'Y'         IF COMMENT RECORD DOES ALREADY               
         JNE   XIT                 EXIST, PUT IT                                
         GOTO1 PUTREC                                                           
         J     XIT                                                              
         SPACE 1                                                                
DELREAS  DC    C'DELETE REASON: ALL CODES PREVIOUSLY PAID '                     
DELREAS2 DC    C'- AUTO DELETE BY PAY'                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO MARK ADVICE RECORD WITH A= DONE                       
         SPACE                                                                  
MRKADV2  NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
*                                                                               
         L     R4,AIO              R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TADVELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R4            R4=A(ADVICE DETAILS ELEMENT)                 
*                                                                               
         CLI   COMPSTAT,0          IF ADVICE CAN BE MARKED COMPLETE             
         BH    MADV210                                                          
         BRAS  RE,DELOLDAC         IF NEW ACTIVITY WON'T FIT, DEL OLD           
         OI    TADVSTAT,TADVSCMP   SET COMPLETE STATUS                          
         MVC   BYTE,TWASCR         AND ADD COMPLETED ACTIVITY ELEMENT           
         MVI   TWASCR,X'7A'                                                     
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         MVC   TWASCR,BYTE                                                      
*                                                                               
MADV210  GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'20',PTRBLK)  MAINTAIN PASSIVE PTRS               
         DROP  R4                                                               
*                                                                               
         NI    CLAINVH+4,X'FF'-X'20'                                            
*                                                                               
         MVC   MYMSGNO,=H'259'     GIVE MESSAGE INVOICE WAS DELETED             
         CLI   COMPSTAT,0                                                       
         BH    *+10                OR                                           
         MVC   MYMSGNO,=H'260'     INVOICE WAS DEL'D AND ADVICE COMP'ED         
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMINF                                                    
         OI    GENSTAT2,USGETTXT                                                
         J     ERRXIT                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DETERMINE IF ADVICE SHOULD BE MARKED COMP             
         SPACE 1                                                                
CMPADV   NTR1  BASE=*,LABEL=*                                                   
         MVI   COMPSTAT,0          INITIALIZE THE COMPLETED STATUS              
         SPACE 1                                                                
         USING TAAID,R4                                                         
         L     R4,AIO2             R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TAAIELQ                                                   
         BRAS  RE,GETEL            GET INVOICE/ADVICE ASSIGN ELEMENT            
         JNE   XIT                                                              
         SPACE 1                                                                
         LA    R3,TAAIINUM         IF REUSE INVOICE EXISTS                      
         OC    TAAIINUM,TAAIINUM   R3=(FIRST REUSE INVOICE)                     
         BNZ   CAR10               ELSE, R3=A(MUSIC INVOICE)                    
         LA    R3,TAAIINU2                                                      
         SPACE 1                                                                
CAR10    PACK  DUB,2(4,R3)                                                      
         CVB   R1,DUB                                                           
         ST    R1,INV1             INV1=FIRST INVOICE ON ADVICE                 
         MVC   INVHEAD,0(R3)                                                    
         SPACE 1                                                                
         LA    R3,TAAIINU2         IF MUSIC INVOICE                             
         OC    TAAIINU2,TAAIINU2   R3=A(MUSIC INVOICE)                          
         BNZ   CAR160                                                           
         SPACE 1                                                                
         CLI   TAAILEN,TAAIILNQ    IF NOT A MUSIC INVOICE                       
         BE    CADV20              BUT MULTIPLE REUSE INVOICES                  
         LA    R3,TAAIINUX         R3=A(LAST REUSE INVOICE)                     
         OC    TAAIINUX,TAAIINUX                                                
         BNZ   CAR160                                                           
         SPACE 1                                                                
CADV20   LA    R3,TAAIINUM         ELSE, R3=A(SOLE REUSE INVOICE)               
         SPACE 1                                                                
CAR160   PACK  DUB,2(4,R3)                                                      
         CVB   R1,DUB                                                           
         ST    R1,INV2             INV2=LAST INVOICE ON ADVICE                  
         SPACE 1                                                                
         MVC   INV,INV1            SET TO PROCESS NOW FROM FIRST INV            
         MVC   AIO,AIO2                                                         
         SPACE 1                                                                
CADV40   CLC   INV,INV2            MOVE PRINTABLE INV FORM INTO CURRINV         
         BH    CADV80                                                           
         MVC   CURRINV(L'INVHEAD),INVHEAD                                       
         EDIT  INV,(4,CURRINV+2),ALIGN=RIGHT                                    
         OC    CURRINV+2(4),=4X'F0'                                             
         SPACE 1                                                                
         ZIC   RE,COMPSTAT         ASSUME INVOICE WILL HOLD UP                  
         AHI   RE,1                COMPLETION PROCESS                           
         STC   RE,COMPSTAT                                                      
         SPACE 1                                                                
         GOTO1 TINVCON,DMCB,CURRINV,CNVINV,DATCON                               
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         XC    CNVINV,=6X'FF'                                                   
         SPACE 1                                                                
         USING TLIND,RE                                                         
         LA    RE,KEY              READ FOR INVOICE                             
         XC    KEY,KEY             IF NOT FOUND, INVOICE WILL NOT               
         MVI   TLINCD,TLINCDQ      HOLD UP COMPLETION PROCESS                   
         MVC   TLINAGY,TGAGY                                                    
         MVC   TLININV,CNVINV                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLINKEY),KEYSAVE                                           
         BNE   CADV60                                                           
         TM    KEY+L'TLINKEY,TLINSDEL                                           
         BNZ   CADV60                                                           
         DROP  RE                                                               
         SPACE 1                                                                
         GOTO1 GETREC              ELSE, GET INVOICE                            
         SPACE 1                                                                
         USING TAIND,R4                                                         
CADV50   L     R4,AIO2             IF INVOICE HAS BEEN PAID,                    
         MVI   ELCODE,TAINELQ      IT WILL NOT HOLD UP COMPLETION               
         BRAS  RE,GETEL                                                         
         BNE   CADV70                                                           
         OC    TAINPINF,TAINPINF                                                
         BZ    CADV70                                                           
         DROP  R4                                                               
         SPACE 1                                                                
CADV60   ZIC   RE,COMPSTAT         SET INVOICE AS NOT HOLDING UP                
         AHI   RE,-1               COMPLETION PROCESS                           
         STC   RE,COMPSTAT                                                      
         SPACE 1                                                                
CADV70   L     RE,INV              BUMP TO NEXT INVOICE                         
         AHI   RE,1                                                             
         ST    RE,INV                                                           
         B     CADV40                                                           
         SPACE 1                                                                
CADV80   BRAS  RE,GETADV                                                        
         J     XIT                                                              
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DETERMINE IF ALPHA CODE SHOULD BE USED TO             
*              READ FOR TMKT RECORDS                                            
         SPACE 1                                                                
USEALPH  NTR1  BASE=*,LABEL=*                                                   
         TM    TGMEEQU,LIKETV                                                   
         JZ    NO                                                               
         TM    TGUSSTA3,CBLUSE                                                  
         JNZ   NO                                                               
         CLI   TGUSEQU,ULCB                                                     
         JNE   YES                                                              
         J     NO                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY # OF SUBSCRIBERS FROM LCB USE TYPE            
         SPACE 1                                                                
         USING TAVUD,R4                                                         
LCBOVR   NTR1  BASE=*,LABEL=*                                                   
         CLI   TAVUUSE,ULCB                                                     
         JNE   NO                                                               
         SPACE 1                                                                
         LA    RE,SUBSTAB                                                       
LCBO10   CLI   0(RE),X'FF'                                                      
         JE    NO                                                               
         CLC   TAVUTYPE,7(RE)                                                   
         JE    LCBO20                                                           
         LA    RE,L'SUBSTAB(RE)                                                 
         J     LCBO10                                                           
         SPACE 1                                                                
LCBO20   MVC   CBLUNIT,0(RE)                                                    
         J     YES                                                              
         SPACE 2                                                                
SUBSTAB  DC    0XL8                                                             
         DC    C'50000  ',AL1(ULCB50)                                           
         DC    C'50000  ',AL1(ULCBU50)                                          
         DC    C'100000 ',AL1(ULCB100)                                          
         DC    C'100000 ',AL1(ULCBU100)                                         
         DC    C'100000 ',AL1(ULCBU12)                                          
         DC    C'150000 ',AL1(ULCB150)                                          
         DC    C'150000 ',AL1(ULCBU150)                                         
         DC    C'150000 ',AL1(ULCBU13)                                          
         DC    C'150000 ',AL1(ULCBU23)                                          
         DC    C'200000 ',AL1(ULCB200)                                          
         DC    C'200000 ',AL1(ULCBU200)                                         
         DC    C'200000 ',AL1(ULCBU14)                                          
         DC    C'200000 ',AL1(ULCBU24)                                          
         DC    C'200000 ',AL1(ULCBU34)                                          
         DC    C'250000 ',AL1(ULCB250)                                          
         DC    C'250000 ',AL1(ULCBU250)                                         
         DC    C'250000 ',AL1(ULCBU15)                                          
         DC    C'250000 ',AL1(ULCBU25)                                          
         DC    C'250000 ',AL1(ULCBU35)                                          
         DC    C'250000 ',AL1(ULCBU45)                                          
         DC    C'500000 ',AL1(ULCB500)                                          
         DC    C'500000 ',AL1(ULCBU500)                                         
         DC    C'500000 ',AL1(ULCBU16)                                          
         DC    C'500000 ',AL1(ULCBU26)                                          
         DC    C'500000 ',AL1(ULCBU36)                                          
         DC    C'500000 ',AL1(ULCBU46)                                          
         DC    C'500000 ',AL1(ULCBU56)                                          
         DC    C'750000 ',AL1(ULCB750)                                          
         DC    C'750000 ',AL1(ULCBU750)                                         
         DC    C'750000 ',AL1(ULCBU17)                                          
         DC    C'750000 ',AL1(ULCBU27)                                          
         DC    C'750000 ',AL1(ULCBU37)                                          
         DC    C'750000 ',AL1(ULCBU47)                                          
         DC    C'750000 ',AL1(ULCBU57)                                          
         DC    C'750000 ',AL1(ULCBU67)                                          
         DC    C'1000000',AL1(ULCB1M)                                           
         DC    C'1000000',AL1(ULCBU1M)                                          
         DC    C'1000000',AL1(ULCBU18)                                          
         DC    C'1000000',AL1(ULCBU28)                                          
         DC    C'1000000',AL1(ULCBU38)                                          
         DC    C'1000000',AL1(ULCBU48)                                          
         DC    C'1000000',AL1(ULCBU58)                                          
         DC    C'1000000',AL1(ULCBU68)                                          
         DC    C'1000000',AL1(ULCBU78)                                          
         DC    C'2000000',AL1(ULCBMAX)                                          
         DC    C'2000000',AL1(ULCBU19)                                          
         DC    C'2000000',AL1(ULCBU29)                                          
         DC    C'2000000',AL1(ULCBU39)                                          
         DC    C'2000000',AL1(ULCBU49)                                          
         DC    C'2000000',AL1(ULCBU59)                                          
         DC    C'2000000',AL1(ULCBU69)                                          
         DC    C'2000000',AL1(ULCBU79)                                          
         DC    C'2000000',AL1(ULCBU89)                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO CHECK IF NEW ACTIVITY ELEMENT WILL FIT                
*              ON ADVICE RECORD, IF NOT DELETE THE OLD ONE                      
*                                                                               
DELOLDAC NTR1  BASE=*,LABEL=*                                                   
         USING TLRCD,RE                                                         
         L     RE,AIO                                                           
         CLC   TLRCLEN,=H'1970'                                                 
         JL    XIT                                                              
         MVI   ELCODE,TAACELQ                                                   
         GOTO1 REMELEM                                                          
         J     XIT                                                              
         DROP  RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO MARK ADVICE RECORD WITH A= DONE                       
                                                                                
MRKADV   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         L     R4,AIO              R4=A(ADVICE RECORD)                          
         MVI   ELCODE,TADVELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R4            R4=A(ADVICE DETAILS ELEMENT)                 
                                                                                
         OI    TADVSTAT,TADVSPAY   SET PAID STATUS                              
         MVC   TADVPDTE,TGTODAY1   SET ADVICE PAY DATE                          
         TIME  DEC                                                              
         STCM  R0,14,TADVPTIM      SET ADVICE PAY TIME                          
                                                                                
         CLI   COMPSTAT,1          IF ADVICE CAN BE MARKED COMPLETE             
         JH    MADV10                                                           
         BRAS  RE,DELOLDAC         IF NEW ACTIVITY WON'T FIT, DEL OLD           
         OI    TADVSTAT,TADVSCMP   SET COMPLETE STATUS                          
         MVC   BYTE,TWASCR         AND ADD COMPLETED ACTIVITY ELEMENT           
         MVI   TWASCR,X'7A'                                                     
         GOTO1 ACTVIN,DMCB,(X'80',0)                                            
         MVC   TWASCR,BYTE                                                      
                                                                                
MADV10   GOTO1 PUTREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'20',PTRBLK)  MAINTAIN PASSIVE PTRS               
         MVC   AIO,AIO1            RESET AIO TO INVOICE RECORD                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO RE-READ INVOICE RECORD                                
                                                                                
RERDINV  NTR1  BASE=*,LABEL=*                                                   
         TM    TGCTSTST,TGCTSCLI   IF NOT CLIENT                                
         JO    XIT                                                              
         LA    RE,WRKIO            REREAD INVOICE RECORD                        
         ST    RE,AIO                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLDRD,R4                                                         
         MVC   TLDRDA,SVINVDA                                                   
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            RE-SET AIO (INVOICE RECORD)                  
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET Y IF 8 WEEKS                                      
         SPACE 1                                                                
         USING TAVUD,R4                                                         
CHK8WK   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TAVUWKS          DEFAULT TO REGULAR TAVUWKS                   
         CLI   SECUSE,C'N'         IF PAYING SECOND USE                         
         JE    CHK8WK05                                                         
         OC    TAVUMWKS,TAVUMWKS   AND MUSIC WEEKS IS SET                       
         JZ    CHK8WK05                                                         
         LA    R1,TAVUMWKS         RESET TO TAVUMWKS                            
         SPACE 1                                                                
CHK8WK05 CLC   =C'8 ',0(R1)                                                     
         JE    CHK8WK20                                                         
         CLC   =C'08',0(R1)                                                     
         JNE   XIT                                                              
CHK8WK20 MVI   8(R2),C'Y'                                                       
         MVI   5(R2),1                                                          
         J     XIT                                                              
         DROP  R4                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
PARSE    NTR1  BASE=*,LABEL=*                                                   
         USING SCAND,R3                                                         
         LA    R3,BLOCK            SET TO SCAN THE FIELD                        
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3)),C',=,-'                           
         CLI   4(R1),0                                                          
         JE    PARNO                                                            
         CLI   4(R1),2             MAX OF 2 ENTRIES (ADVICE + USE NOS.)         
         JH    PARNO                                                            
                                                                                
         MVC   ERRDISP,SCDISP2                                                  
         CLI   SCLEN2,0            1ST ENTRY IS A=ADVICE (NO RHS)               
         JNE   PARNO                                                            
         MVC   TGADV,SCDATA1+2     SAVE ADVICE RECORD CODE                      
                                                                                
         CLI   4(R1),1             IF ONLY ONE ENTRY THEN DONE                  
         JE    XIT                                                              
         LA    R3,SCANNEXT         ELSE BUMP TO USE NUMBERS                     
                                                                                
         MVC   ERRDISP,SCDISP1                                                  
         TM    TGUSSTA3,NWKUSE     IF NETWORK USE                               
         JZ    PAR10                                                            
         TM    SCVAL1,X'80'        STARTING USE NUMBER MUST BE NUMERIC          
         JZ    PARNO                                                            
         OC    SCBIN1+1(3),SCBIN1+1  AND MUST BE GT ZERO                        
         JZ    PARNO                                                            
         MVC   STUSE,SCBIN1+2      SAVE STARTING USE NUMBER                     
                                                                                
         CLI   SCLEN2,0            OK TO NOT HAVE RHS                           
         JE    XIT                                                              
         MVC   ERRDISP,SCDISP2                                                  
         TM    SCVAL2,X'80'        ENDING USE NUMBER MUST BE NUMERIC            
         JZ    PARNO                                                            
         OC    SCBIN2+1(3),SCBIN2+1  AND MUST BE GT ZERO                        
         JZ    PARNO                                                            
         CLC   SCBIN2+1(3),SCBIN1+1  END MUST BE GE START                       
         JL    PARNO                                                            
         LH    R1,STUSE            TEST FOR MAXIMUM RANGE                       
         LA    R1,20-1(R1)                                                      
         CH    R1,SCBIN2+2                                                      
         JL    PARNO                                                            
         MVC   ENDUSE,SCBIN2+2     SAVE ENDING USE NUMBER                       
         J     XIT                                                              
                                                                                
PAR10    TM    TGUSSTA3,USEMTAB    IF WILDSPOT OR CABLE                         
         JZ    PARNO                                                            
PAR20    MVC   STCODE,SCDATA1      SAVE STARTING CODE                           
         J     XIT                                                              
                                                                                
PARYES   CR    RB,RB                                                            
         J     *+6                                                              
PARNO    LTR   RB,RB                                                            
         J     XIT                                                              
         DROP  R3                                                               
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER DVUSES TABLE                                      
         SPACE 1                                                                
DVUSESD  DSECT                                                                  
DVUSE    DS    XL1                 USE EQUATE                                   
DVUSERTN DS    AL2                 DISPLACEMENT TO USE ROUTINE                  
DVUSELNQ EQU   *-DVUSE                                                          
         SPACE 2                                                                
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
MYD      DSECT                                                                  
SVCID    DS    CL(L'TGCID)         SAVED COMMERCIAL ID NUMBER                   
STUSE    DS    H                   STARTING USE NUMBER                          
ENDUSE   DS    H                   ENDING USE NUMBER                            
STCODE   DS    CL(L'TAMTCODE)      STARTING CODE                                
ATAVUEL  DS    A                   A(ADVICE USE DETAILS EL.)                    
ATANXEL  DS    A                   A(NETWORK XFER DETAILS EL.)                  
REGOTH   DS    CL1                 Y=REGULAR OTHER COMMENT EXISTS               
MUSOTH   DS    CL1                 Y=MUSIC OTHER COMMENT EXISTS                 
TWOUSES  DS    CL1                 Y=TWO USES EXIST ON ADVICE                   
SECUSE   DS    CL1                 Y=PAYING SECOND USE ON ADVICE                
SECUPGR  DS    CL1                 Y=ADVICE IS AN UPGRADE                       
INVASSGN DS    XL1                 Y=ADVICE HAS INVOICES ASSIGNED               
SVINVDA  DS    XL4                 SAVED INVOICE DISK ADDRESS                   
SVVERS   DS    C                   SAVED VERSION                                
SVADVDA2 DS    XL4                 TEMPORARY SAVED ADVICE DISK ADDRESS          
*                                                                               
FLDCNTR  DS    H                   FIELD COUNTER                                
*                                                                               
CYCEND   DS    XL3                                                              
*                                                                               
INVHEAD  DS    CL2                                                              
INV1     DS    F                                                                
INV2     DS    F                                                                
INV      DS    F                                                                
CNVINV   DS    XL6                                                              
COMPSTAT DS    X                                                                
CURRINV  DS    CL6                                                              
*                                                                               
ORIGSCR  DS    CL1                                                              
*                                                                               
PTRBLK   DS    CL(10*L'TLDRREC+1)   POINTER BLOCK                               
WRKIO    DS    CL4000              WORKING IOAREA                               
MYDLNQ   EQU   *-MYD                                                            
         EJECT                                                                  
       ++INCLUDE TAPYS78D                                                       
         EJECT                                                                  
       ++INCLUDE TAGENPAYD                                                      
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
* TAPYS51D                                                                      
* TAPYS5BD                                                                      
* DDGENTWA    *** MUST FOLLOW A SCREEN ***                                      
* TAGENWORKD                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS11D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS12D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS1AD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS1DD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS21D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5BD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5CD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5FD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS51D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS53D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS54D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS55D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS56D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS57D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS59D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS5ED                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS60D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS61D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS62D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS63D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS66D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS6DD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS67D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS71D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS76D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS77D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TAPYS79D                                                       
         SPACE 1                                                                
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'147TAGEN54   11/02/16'                                      
         END                                                                    
