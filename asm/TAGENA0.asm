*          DATA SET TAGENA0    AT LEVEL 067 AS OF 02/02/10                      
*PHASE T702A0A,*                                                                
         TITLE 'T702A0 - INVOICE RELEASE'                                       
T702A0   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A0                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=WORKING STORAGE                           
         USING WORKD,R7                                                         
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BNE   IR10                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
IR10     CLI   MODE,VALREC                                                      
         BNE   XIT                                                              
         TM    LCLSTAT,START       TEST (RE)STARTING LIST                       
         BO    IR100                                                            
         CLI   PFAID,8             ELSE IF PF8 PRESSED                          
         BNE   IR20                                                             
         XC    TIKEY,TIKEY         SET TO START AT BEGINNING                    
         B     IR100                                                            
         SPACE 1                                                                
IR20     BAS   RE,PROCSEL          PROCESS IF ANY SEL FIELDS CHANGED            
         BE    PFRELEAS            DISPLAY TOTAL & HIT PFKEY TO RELEASE         
         SPACE 1                                                                
         TM    LCLSTAT,PFTOREL     TEST PF13 TO RELEASE BIT ON                  
         BZ    IR100                                                            
         CLI   PFAID,13            TEST THEY HIT PF13                           
         BNE   PFRELEAS            NO - REDISPLAY MESSAGE                       
         SPACE                                                                  
         BAS   RE,PROCREL          YES - RELEASE SELECTED RECORDS               
         B     RELEASED            DISPLAY RELEASED MESSAGE                     
         SPACE                                                                  
IR100    BAS   RE,INITSCRN         INIT SCREEN FOR NEW PAGE                     
         SPACE 1                                                                
         MVI   LCLSTAT,0           CLEAR STATUS FOR NEW PAGE                    
         BAS   RE,CLRTAB           CLEAR SCREEN TABLE                           
         LA    R2,INRFSTH                                                       
         ST    R2,ATHSLINE         SET A(THIS LINE) TO FIRST LINE               
         LA    R2,SCRTAB                                                        
         ST    R2,ATHSTAB          SET A(THIS SCREEN TABLE ENTRY)               
         SPACE 1                                                                
         LA    R2,HOOK             SET HOOK TO SYSIO                            
         ST    R2,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         SPACE 1                                                                
         MVC   TIQSKEY,TIKEY          CLEAR/SET KEY FOR FIRST/NEXT PAGE         
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  OFF TO SYSIO TO READ RECORDS              
         SPACE 1                                                                
         OI    LCLSTAT,FINISHED       SET FINISHED LIST                         
         B     SELFIRST               SELECT OR ENTER FOR FIRST                 
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    SCRSTAT,SCRCHG      TEST NOT FIRST TIME FOR SCREEN               
         BO    VK5                                                              
         TM    INRAGYH+4,X'20'     IF AGENCY NOT PREV VALIDATED                 
         BO    VKX                                                              
VK5      GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',INRAGYH),INRAGYNH VALIDATE IT         
         MVC   TIFAGY,TGAGY        SET AGENCY FILTER                            
         SPACE 1                                                                
         MVI   TIREAD,TLINBCDQ     RECORD TO READ FOR SYSIO                     
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         OI    TIQFLAGS,TIQFDIR    SET WANT DIRECTORY HOOKS                     
         XC    TIKEY,TIKEY         CLEAR TO START LIST FROM BEGINNING           
         OI    LCLSTAT,START       SET FLAG TO (RE)START SYSIO                  
         SPACE 1                                                                
VKX      B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE INITIALIZES THE SCREEN TO DISPLAY A NEW PAGE             
         SPACE                                                                  
INITSCRN NTR1                                                                   
         TWAXC INRSELH,INRLSTH,PROT=Y  CLEAR SCREEN                             
         SPACE 1                                                                
         LA    R2,INRSELH                                                       
INITS5   NI    1(R2),X'DF'         MAKE ALL SEL FLDS UNPROTECTED                
         BAS   RE,BUMP3                                                         
         BE    INITS5                                                           
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'20',INRSELH),(X'80',INRLSTH)  SET VALID           
         B     XIT                                                              
         SPACE                                                                  
CLRTAB   NTR1                                                                   
         LA    RE,MXLINES                                                       
         LA    R1,SCRTAB                                                        
         USING SCRTABD,R1                                                       
CLRTAB5  XC    0(SCRTNXT-SCRTABD,R1),0(R1)                                      
         LA    R1,SCRTNXT                                                       
         BCT   RE,CLRTAB5                                                       
         B     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE PROCESSES SELECT FIELDS                                  
*              RETURNS CC NOT EQUAL IF NO SELECT FIELDS CHANGED                 
*              ELSE RETURNS CC EQUAL IF AT LEAST ONE SELECTED, AND              
*              CALCULATES INVOICE TOTAL OF ALL SELECTED RECORDS                 
PROCSEL  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',INRSELH),(X'80',INRLSTH)  TEST VALID          
         BE    NO                  ALL SEL FLDS VALID, NO PROCESSING            
         SPACE                                                                  
         NI    LCLSTAT,ALL-PFTOREL-SELECTED  SET NOT SELECTED YET               
         XC    TOTAL,TOTAL                   CLEAR TOTAL                        
         SPACE                                                                  
         USING SCRTABD,R5                                                       
         LA    R2,INRSELH          R2=A(SEL FIELD HEADER)                       
         LA    R5,SCRTAB           R5=A(SCREEN TABLE ENTRY)                     
         SPACE                                                                  
PROCS5   TM    1(R2),X'20'         TEST UNPROTECTED                             
         BO    PROCS10                                                          
         CLI   5(R2),0             TEST INPUT IN FIELD, ANYTHING IS OK          
         BE    PROCS10                                                          
         OC    SCRTINV,SCRTINV     TEST SOMETHING TO SELECT                     
         BZ    PROCS50             IF NOT, PAST LAST RECORD                     
         OI    LCLSTAT,SELECTED    ELSE, SET SOMETHING SELECTED                 
         ICM   R1,15,SCRTTOT                                                    
         A     R1,TOTAL            ADD TO TOTAL FOR SELECTED                    
         ST    R1,TOTAL                                                         
         SPACE                                                                  
PROCS10  BAS   RE,BUMP3            BUMP TO NEXT SEL FIELD                       
         BNE   PROCS50             FINISHED IF PAST LAST SEL LINE               
         LA    R5,SCRTNXT          BUMP TO NEXT SCREEN TABLE ENTRY              
         B     PROCS5                                                           
         SPACE                                                                  
PROCS50  TM    LCLSTAT,SELECTED    IF NONE SELECTED                             
         BZ    NO                  RETURN CC NEQ TO DISPLAY NEXT PAGE           
         DROP  R5                                                               
*                                  BUILD SUBST. BLOCK FOR GETTXT                
         OC    TOTAL,TOTAL         TEST TOTAL = 0                               
         BNZ   PROCS65                                                          
         MVI   SUBBLOCK,2          SET LENGTH OF ENTRY = 2                      
         MVI   SUBBLOCK+1,C'0'                                                  
         MVI   SUBBLOCK+2,0        MARK END OF ENTRY                            
         B     PROCS70                                                          
         SPACE                                                                  
PROCS65  LA    R2,SUBBLOCK+1                                                    
         EDIT  TOTAL,(15,(R2)),2,ALIGN=LEFT,FLOAT=-                             
         LR    R1,R0                                                            
         LA    R1,1(R1)                                                         
         STC   R1,SUBBLOCK         LENGTH OF ENTRY = L'TOTAL + 1                
         AR    R2,R0                                                            
         MVI   0(R2),0             MARK END OF ENTRY                            
         SPACE                                                                  
PROCS70  GOTO1 FLDVAL,DMCB,(X'20',INRSELH),(X'80',INRLSTH)  SET VALID           
         OI    LCLSTAT,PFTOREL     SET PF13 TO RELEASE                          
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE RELEASES SELECTED RECORDS                                
         SPACE                                                                  
PROCREL  NTR1                                                                   
         LA    R2,INRSELH          R2=A(SEL FIELD HEADER)                       
         LA    R5,SCRTAB           R5=A(SCREEN TABLE ENTRY)                     
         USING SCRTABD,R5                                                       
         SPACE                                                                  
PROCR5   TM    1(R2),X'20'         TEST UNPROTECTED                             
         BO    PROCR10                                                          
         CLI   5(R2),0             TEST INPUT IN FIELD                          
         BE    PROCR10                                                          
         OC    SCRTINV,SCRTINV     TEST SOMETHING TO RELEASE                    
         BZ    PROCRX              IF NOT, PAST LAST RECORD                     
*                                                                               
         USING TLINPD,R3                                                        
         LA    R3,KEY              R3=A(KEY)                                    
         XC    TLINPKEY,TLINPKEY   BUILD KEY FOR STATUS PTR                     
         MVI   TLINPCD,TLINBCDQ                                                 
         MVC   TLINBAGY,TGAGY                                                   
         MVC   TLINBINV,SCRTINV    MOVE INVOICE TO KEY                          
         MVI   RDUPDATE,C'Y'       READ DIRECTORY FOR UPDATE                    
         GOTO1 HIGH                                                             
         CLC   TLINPKEY,KEYSAVE    COMPARE W/O STATUS BYTES                     
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         NI    TLINBST2,ALL-TAINSHLD  TURN OFF HOLD BIT                         
         GOTO1 WRITE                  WRITE BACK CHANGE                         
         BAS   RE,RECREL              RELEASE RECORD                            
*                                                                               
         LA    RF,MAXSI                                                         
         LA    R1,SCRTSINV         IF ANY SUBSIDIARY INVOICES                   
PROCR6   OC    0(L'SCRTSINV,R1),0(R1)                                           
         BZ    PROCR8                                                           
         BAS   RE,SETSKEY          SET SUBISIDARY KEY FOR GETREC                
         BAS   RE,RECREL           RELEASE THEM TOO                             
         LA    R1,L'SCRTSINV(R1)                                                
         BCT   RF,PROCR6                                                        
         SPACE                                                                  
PROCR8   MVI   8(R2),C'*'          REPLACE SEL WITH *                           
         OI    1(R2),X'20'         SET PROTECTED                                
         OI    6(R2),X'80'         MUST TRANSMIT                                
         SPACE                                                                  
PROCR10  BAS   RE,BUMP3            BUMP TO NEXT SEL FIELD                       
         BNE   PROCRX              FINISHED IF PAST LAST SEL LINE               
         LA    R5,SCRTNXT          BUMP TO NEXT SCREEN TABLE ENTRY              
         B     PROCR5                                                           
PROCRX   NI    LCLSTAT,ALL-PFTOREL TURN OFF PF13 TO RELEASE BIT                 
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              SET SUBSIDIARY INVOICE KEY                                       
*                                  R1=A(SUBSIDIARY INVOICE)                     
         SPACE                                                                  
SETSKEY  NTR1                                                                   
         XC    KEY,KEY             BUILD SUBSIDIARY INVOICE KEY                 
         LA    R3,KEY                                                           
         USING TLIND,R3                                                         
         MVI   TLINCD,TLINCDQ                                                   
         MVC   TLINAGY,TGAGY                                                    
         MVC   TLININV,0(R1)       MOVE INVOICE TO KEY                          
         GOTO1 HIGH                                                             
         CLC   TLINKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         SPACE 2                                                                
*              MARK INVOICE RECORD RELEASED                                     
         SPACE                                                                  
RECREL   NTR1                                                                   
         MVI   RDUPDATE,C'Y'       READ RECORD FOR UPDATE                       
         GOTO1 GETREC                                                           
         GOTO1 SAVPTRS,DMCB,INVPTRS SAVE CURRENT PASSIVE POINTERS               
*                                                                               
         USING TAIND,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
*                                                                               
         NI    TAINSTAT,ALL-TAINSHLD  TURN OFF HOLD BIT                         
         OI    TAINSTA2,TAINSHLR      TURN ON HOLD RELEASED BIT                 
         GOTO1 ACTVIN,DMCB,(X'80',0)  SAVE WHO RELEASED AND WHEN                
         GOTO1 PUTREC                 WRITE BACK CHANGED RECORD                 
         GOTO1 ADDPTRS,DMCB,INVPTRS   UPDATE PASSIVE POINTERS                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUMPS R2 BY 3 FIELDS                                     
*              RETURNS CC EQUAL IF NOT PAST LAST SELECT LINE,                   
*              ELSE RETURNS CC NOT EQUAL                                        
         SPACE                                                                  
BUMP3    NTR1                                                                   
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    BMPNO               RETURN CC NOT EQUAL IF END OF SCREEN         
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    BMPNO               RETURN CC NOT EQUAL IF END OF SCREEN         
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BE    BMPNO               RETURN CC NOT EQUAL IF END OF SCREEN         
         LA    R3,INRLSTH                                                       
         CR    R2,R3                                                            
         BH    BMPNO               OR IF PAST LAST SEL LINE                     
         XR    RC,RC               ELSE RETURN CC EQUAL                         
BMPNO    LTR   RC,RC                                                            
         XIT1  REGS=(R2)           ALWAYS RETURN R2                             
         EJECT                                                                  
*              ROUTINE TO PROCESS RECORDS FROM SYSIO ON SCREEN                  
         SPACE 1                                                                
HOOK     NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESS DIRECTORY                            
         BNE   HK10                                                             
         TM    TIKEYST+1,TAINSHLD  TEST HOLD INVOICE                            
         BO    YES                 SET CC FOR SYSIO                             
         B     NO                                                               
         SPACE                                                                  
HK10     CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         L     R2,ATHSLINE         R2=A(THIS DISPLAY LINE HEADER)               
         LA    RF,INRLSTH                                                       
         CR    R2,RF               TEST WHETHER WE'RE PAST END                  
         BH    SELNEXT             YES - RETURN TO USER                         
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING TLIND,R3                                                         
         USING SCRTABD,R5                                                       
         L     R3,TIAREC           R3=A(RECORD)                                 
         LA    R2,8(R2)            BUMP R2 PAST HEADER                          
         L     R5,ATHSTAB          R5=A(SCREEN TABLE ENTRY)                     
         SPACE                                                                  
         USING TACOD,R4                                                         
         LR    R4,R3                                                            
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS ELEMENT               
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   LINCID,TACOCID      COMMERCIAL ID                                
         SPACE                                                                  
         BAS   RE,SETSUB           SET SUBSIDIARY INVOICES IN TABLE             
         SPACE                                                                  
         LR    R4,R3                                                            
         MVI   ELCODE,TAINELQ      GET INVOICE STATUS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         SPACE                                                                  
         USING TAIND,R4                                                         
         GOTO1 DATCON,DMCB,(1,TAINBDTE),(8,LINBILL) BILL DATE                   
         SPACE                                                                  
         MVC   SCRTINV,TLININV     SAVE INVOICE NUMBER                          
         MVC   DUB,TLININV                                                      
         XC    DUB(6),=6X'FF'      UNCOMPLEMENT FOR DISPLAY                     
         GOTO1 TINVCON,DMCB,DUB,LININV,DATCON CONVERT INVOICE NUMBER            
         SPACE                                                                  
         TM    TAINSTA2,TAINSHLP   IF HOLD BILLS NOT PRINTED                    
         BO    HK20                                                             
         LR    R4,R3                                                            
         MVI   ELCODE,TAEUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    HK19                                                             
         LR    R4,R3                                                            
         MVI   ELCODE,TAPDELQ      GET PAY DETAILS ELEMENT                      
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TAPDD,R4                                                         
         SPACE                                                                  
HK19     L     R3,TAPDGRS          SET R3=GROSS                                 
         A     R3,TAPDPNH                 + P&H=INVOICE TOTAL                   
         B     HK22                                                             
         SPACE                                                                  
HK20     LR    R4,R3                                                            
         MVI   ELCODE,TABDELQ3                                                  
         BAS   RE,GETEL                                                         
         BE    HK21                                                             
         LR    R4,R3                                                            
         MVI   ELCODE,TABDELQ      GET BILL DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING TABDD,R4                                                         
HK21     L     R3,TABDTOT          SET R3=BILL TOTAL                            
         SPACE                                                                  
HK22     STCM  R3,15,SCRTTOT                 SAVE INVOICE TOTAL                 
         EDIT  (R3),(12,LINTOT),2,MINUS=YES  EDIT TO SCREEN                     
         SPACE                                                                  
         L     R2,ATHSLINE         RESET R2=A(THIS DISPLAY LINE HEADER)         
         BAS   RE,BUMP3            BUMP TO NEXT DETAIL LINE                     
         ST    R2,ATHSLINE         SAVE IT FOR NEXT TIME IN                     
         LA    R5,SCRTNXT          BUMP TO NEXT SCREEN TABLE ENTRY              
         ST    R5,ATHSTAB          SAVE IT FOR NEXT TIME                        
         B     XIT                                                              
         EJECT                                                                  
*              SET SUBSIDIARY INVOICES IN TABLE                                 
         SPACE                                                                  
SETSUB   NTR1                                                                   
         LA    R1,SCRTSINV         SUBSIDIARY INVOICES                          
         LR    R4,R3                                                            
         MVI   ELCODE,TASIELQ      GET SUBSIDIARY INVOICE ELEMENT               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
SETSUB10 BAS   RE,NEXTEL                                                        
         BNE   SETSUBX                                                          
         USING TASID,R4                                                         
         OC    TASIINV,TASIINV                                                  
         BZ    SETSUB10                                                         
         MVC   0(L'TASIINV,R1),TASIINV                                          
         XC    0(L'TASIINV,R1),=6X'FF'                                          
         LA    R1,L'SCRTSINV(R1)                                                
         B     SETSUB10                                                         
SETSUBX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
RELEASED MVI   MYMSGNO1,59         INVOICES RELEASED-HIT ENTER FOR NEXT         
         TM    LCLSTAT,FINISHED    IF THIS IS LAST PAGE                         
         BZ    *+8                                                              
         MVI   MYMSGNO1,60         INVOICES RELEASED-HIT ENTER FOR FRST         
         B     *+8                                                              
PFRELEAS MVI   MYMSGNO1,58         DISPLAY TOTAL - HIT PF13 TO RELEASE          
         MVC   BLOCK(L'SUBBLOCK),SUBBLOCK  MOVE SUBST. BLOCK FOR GETTXT         
         B     DTLEND                                                           
         SPACE 1                                                                
SELFIRST MVI   MYMSGNO1,10         SELECT OR HIT ENTER FOR FIRST                
         B     *+8                                                              
SELNEXT  MVI   MYMSGNO1,9          SELECT OR HIT ENTER FOR NEXT                 
         MVI   MYMSYS,X'FF'                                                     
         B     DTLEND                                                           
         SPACE 1                                                                
DTLEND   LA    R2,INRSELH                                                       
         OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
MAXSI    EQU   10                  MAX NUMBER OF SUBSIDIARY INVOICES            
MXLINES  EQU   16                  MAX LINES ON SCREEN                          
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER SCREEN TABLE                                      
         SPACE 2                                                                
SCRTABD  DSECT                                                                  
SCRTINV  DS    CL6                 INTERNAL INVOICE NUMBER                      
SCRTTOT  DS    CL4                 INVOICE TOTAL                                
SCRTSINV DS    (MAXSI)CL6          SUBSIDIARY INVOICE NUMBERS                   
SCRTNXT  EQU   *                   NEXT ENTRY                                   
         SPACE 3                                                                
*              DSECT TO COVER SCREEN DISPLAY LINES                              
         SPACE 2                                                                
LINED    DSECT                                                                  
LININV   DS    CL6                 INVOICE                                      
         DS    CL2                                                              
LINBILL  DS    CL8                 BILL DATE                                    
         DS    CL3                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
         DS    CL3                                                              
LINTOT   DS    CL12                INVOICE TOTAL                                
         EJECT                                                                  
*              DSECT TO COVER WORKING STORAGE                                   
         SPACE 1                                                                
WORKD    DSECT                                                                  
ATHSLINE DS    A                   A(THIS LINE) BEING DISPLAYED                 
ATHSTAB  DS    A                   A(THIS SCREEN TABLE ENTRY)                   
TOTAL    DS    F                   INVOICE TOTAL OF ALL SELECTED RECS           
TEMP     DS    CL6                 SAVED SUBSTITUTION BLOCK FOR GETTXT          
SUBBLOCK DS    CL17                SAVED SUBSTITUTION BLOCK FOR GETTXT          
LCLSTAT  DS    XL1                 LOCAL STATUS BYTE FOR PAGE DISPLAYED         
PFTOREL  EQU   X'80'               RELEASE IF PFKEY HIT                         
START    EQU   X'40'               RE(START) SYSIO FROM BEGINNING               
FINISHED EQU   X'20'               FINISHED DISPLAYING LIST                     
SELECTED EQU   X'10'               THERE'S SOMETHING SELECTED                   
SCRTAB   DS    (MXLINES)CL(SCRTNXT-SCRTABD)   SCREEN TABLE                      
INVPTRS  DS    CL((7*L'TLDRREC)+1) INVOICE POINTER BLOCK                        
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA0D                                                       
         EJECT                                                                  
         SPACE 1                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067TAGENA0   02/02/10'                                      
         END                                                                    
