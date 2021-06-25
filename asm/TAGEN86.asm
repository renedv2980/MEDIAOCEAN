*          DATA SET TAGEN86    AT LEVEL 012 AS OF 07/20/12                      
*PHASE T70286C,*                                                                
         TITLE 'T70286 - GUARANTEE LIST'                                        
T70286   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70286                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         MVC   SGUHEAD,NEWHEADS                                                 
         OI    SGUHEADH+6,X'80'                                                 
         GOTO1 INITIAL,DMCB,NPFTAB                                              
         SPACE 1                                                                
         MVC   SGUSHED(7),=C'Pid Num'                                           
         OI    SGUSHEDH+6,X'80'                                                 
         SPACE 1                                                                
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   GUA10                                                            
         TWAXC SGUSELH,SGULINH,PROT=Y   CLEAR SCREEN                            
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         B     GUA20                                                            
         SPACE 1                                                                
GUA10    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         MVI   LISTNUM,0           INSURE LISTNUM IS CLEARED                    
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
         SPACE 1                                                                
GUA20    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    SGUSSNH+4,X'20'                                                  
         BO    VK3                                                              
         LA    R2,SGUSSNH                                                       
         CLI   SGUSSNH+5,0                                                      
         BE    VK1A                                                             
         CLI   SGUSSNH+5,9         SSN ENTERED?                                 
         BE    VK1                                                              
         CLI   SGUSSNH+5,6         PID ENTERED?                                 
         BNE   INVERR                                                           
         MVC   TGPID,SGUSSN                                                     
VK1A     OC    TGPID,TGPID                                                      
         BZ    MISSERR                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK1                                                              
         MVC   SGUSSN,TGSSN                                                     
         MVI   SGUSSNH+5,9                                                      
VK1      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SGUSSNH),SGUSSNNH  S/S NUMBER         
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SGUSSN,SPACES                                                    
         MVC   SGUSSN(L'TGPID),TGPID                                            
         MVI   SGUSSNH+5,L'TGPID                                                
         OI    SGUSSNH+6,X'80'                                                  
*                                                                               
VK2      MVC   TIFSSN,TGSSN                                                     
         NI    SGUCODEH+4,X'DF'                                                 
         SPACE 1                                                                
VK3      LA    R2,SGUCODEH         START AT SPECIFIC GUARANTEE                  
         TM    4(R2),X'20'                                                      
         BO    VK5                                                              
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    VK4                                                              
         MVC   TIQSTART(4),8(R2)                                                
         SPACE 1                                                                
VK4      OI    4(R2),X'20'         VALIDATED                                    
         NI    SGUOPTSH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         SPACE 1                                                                
VK5      LA    R2,SGUOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VK7                                                              
         NI    SGUAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         MVI   OPTALL,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    VK7                                                              
         LA    R3,BLOCK            SET FOR SCANNER                              
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3)                                           
         ZIC   R0,4(R1)                                                         
         LTR   R0,0                INVALID INPUT                                
         BZ    INVERR                                                           
         SPACE 1                                                                
VK6      CLC   SCDATA1(3),=C'ALL'                                               
         BNE   INVERR                                                           
         MVI   OPTALL,C'Y'                                                      
         LA    R3,SCANNEXT                                                      
         BCT   R0,VK6                                                           
         SPACE 1                                                                
VK7      OI    4(R2),X'20'         VALIDATED                                    
         LA    R2,SGUAGYH          OPTIONAL AGENCY FILTER                       
         TM    4(R2),X'20'                                                      
         BO    VK8                                                              
         NI    SGUCLIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    FLTAGY,FLTAGY                                                    
         XC    SGUAGYN,SGUAGYN     PRE-CLEAR NAME                               
         OI    SGUAGYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    VK8                                                              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),SGUAGYNH  AGENCY                
         MVC   FLTAGY,TGAGY                                                     
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
VK8      OI    4(R2),X'20'         VALIDATED                                    
         TM    SGUCLIH+4,X'20'                                                  
         BO    VK12                                                             
         XC    FLTCLI,FLTCLI                                                    
         XC    SGUCLIN,SGUCLIN     PRE-CLEAR NAME                               
         OI    SGUCLINH+6,X'80'                                                 
         CLI   SGUCLIH+5,0                                                      
         BE    VK10                                                             
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         LA    R2,SGUCLIH          OPTIONAL CLIENT FILTER                       
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),SGUCLINH  CLIENT                
         MVC   FLTCLI,TGCLI                                                     
         SPACE 1                                                                
VK10     OI    4(R2),X'20'                                                      
         BAS   RE,INIT             RE-INITIALIZE LIST                           
VK12     XC    FLTGCNT,FLTGCNT                                                  
         LA    R2,SGUGCONH         OPTIONAL GCON FILTER                         
         CLI   5(R2),0                                                          
         JE    VKX                                                              
         CLI   5(R2),L'SGUGCON                                                  
         JNE   INVERR                                                           
         CLC   8(2,R2),=C'GC'                                                   
         JNE   INVERR                                                           
*                                                                               
         ZIC   R1,5(R2)                                                         
         SHI   R1,3                2 FOR 'GC' AND 1 FOR EXPACK                  
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,10(0,R2)                                                     
         CVB   R1,DUB                                                           
         LNR   R1,R1               COMPLEMENT GRT CONTRACT CODE                 
         STCM  R1,15,FLTGCNT                                                    
*                                                                               
         USING TLGCD,R3                                                         
         LA    R3,KEY                                                           
         XC    TLGCKEY,TLGCKEY                                                  
         MVI   TLGCCD,TLGCCDQ                                                   
         MVC   TLGCSSN,TGSSN                                                    
         MVC   TLGCGCNT,FLTGCNT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLGCKEY),KEYSAVE                                           
         JNE   GCONERR                                                          
*                                                                               
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         XC    TIQSKEY,TIQSKEY                                                  
         XC    PREVDISP,PREVDISP         & PREVIOUS DISPLACEMENT                
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLGUCDQ                                                   
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         SPACE 1                                                                
         MVI   NLISTS,16           IN ORDER TO GET CONTROL                      
         GOTO1 SETLSTK,DMCB,(TIREAD,TIQSKEY)  SET KEY OURSELVES                 
         SPACE 1                                                                
         OC    KEY,KEY             IF WE'RE IN MIDDLE OF LIST                   
         BZ    LR10                                                             
         CLC   DMDSKADD,VERYFRST   AND CURRENT KEY IS VERY FIRST                
         BNE   LR10                                                             
         CLI   LISTSW,C'N'         OR NO PFKEY PRESSED                          
         BNE   LR10                                                             
         TM    MYSTAT,NEXTPEND     AND WE COULDN'T FIT PREV. RECORD             
         BZ    LR10                                                             
         GOTO1 SEQ                 NEED TO START WITH NEXT RECORD               
         MVC   TIQSKEY,KEY                                                      
LR10     NI    MYSTAT,X'FF'-NEXTPEND                                            
         SPACE 1                                                                
         OI    TIQFLAG2,TIQFNLIM                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         SPACE 1                                                                
         MVI   NLISTS,15           BACK AFTER 1 FULL PAGE                       
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY                           
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(5,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(17,R1),=C'GUARANTEE RECORDS'                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         CLI   LISTNUM,15          END OF 1 PAGE                                
         BE    ENDPAGE                                                          
         BRAS  RE,CHKACC           SKIP IF STAFF DOES NOT HAVE                  
         BNE   XIT                 ACCESS                                       
         BAS   RE,COUNTGC          ENSURE ENOUGH ROOM ON PAGE                   
         BE    LRH05                                                            
         OI    MYSTAT,NEXTPEND     SET ANOTHER RECORD PENDING                   
         L     R2,ATHISLST                                                      
         MVC   8(L'LTMORE,R2),LTMORE     OUTPUT MESSAGE                         
         OI    6(R2),X'80'                                                      
         B     ENDPAGE                                                          
         SPACE 1                                                                
LRH05    MVC   LINGUA,TIGUA        GUARANTEE CODE                               
         SPACE 1                                                                
         MVC   NLINAGY,TIAGY       AGENCY                                       
         SPACE 1                                                                
         MVC   LINCLI,TICLI        CLIENT                                       
         SPACE 1                                                                
         BRAS  RE,MULTAYCL         DENOTE MULTIPLE AGENCIES/CLIENTS             
         SPACE 1                                                                
         USING TAGUD,R4            R4=A(GUARANTEE DETAILS EL.)                  
         MVI   ELCODE,TAGUELQ      LOOK FOR GUARANTEE DETAILS EL.               
         L     R4,TIAREC                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
*                                                                               
         OC    FLTGCNT,FLTGCNT     GCON FILTER?                                 
         JZ    *+14                                                             
         CLC   FLTGCNT,TAGUGCNT                                                 
         JNE   LRHX                                                             
*                                                                               
         MVI   TYPE6,C'N'                                                       
         OC    TAGUCOM,TAGUCOM                                                  
         BNZ   LRH10                                                            
         GOTO1 DATCON,DMCB,(X'11',TAGUPD),(8,NLINPD) PERIOD                     
         SPACE 1                                                                
LRH10    MVI   LINDBAL,C'Y'                                                     
         TM    TAGUSTAT,TAGUSDES   TEST DESCENDING BALANCE                      
         BO    *+8                                                              
         MVI   LINDBAL,C'N'                                                     
         SPACE 1                                                                
         MVI   LINPNH,C'Y'                                                      
         TM    TAGUSTAT,TAGUSPNH   TEST PAYING P&H                              
         BO    *+8                                                              
         MVI   LINPNH,C'N'                                                      
         SPACE 1                                                                
         MVI   LINOVER,C'Y'                                                     
         TM    TAGUSTAT,TAGUSOVR   TEST PAYING OVERAGE                          
         BO    *+8                                                              
         MVI   LINOVER,C'N'                                                     
         SPACE 1                                                                
         OC    TAGUCOM,TAGUCOM     IF WE HAVE A COMMERCIAL                      
         BNZ   LRH20                                                            
         EDIT  (4,TAGUAMT),(12,LINAMT),2,MINUS=YES  GUARANTEE AMOUNT            
         EDIT  (4,TAGUBAL),(12,LINBAL),2,MINUS=YES  BALANCE                     
         B     LRH40                                                            
         SPACE 1                                                                
LRH20    MVI   LINT6,C'Y'          TYPE 6                                       
         MVI   TYPE6,C'Y'                                                       
         MVC   TGCOM,TAGUCOM       MOVE TO GLOBAL                               
         SPACE 1                                                                
LRH30    BAS   RE,GETGC            IF TAGCELQ THEN SHOW PERIOD                  
         BE    LRH40                                                            
         MVI   TYPE6,C'N'                                                       
         CLI   LINT6,C'Y'                                                       
         BNE   LRHX                                                             
         SPACE 1                                                                
LRH40    CLI   MODE,PRINTREP                                                    
         BNE   LRH70                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     LRH80                                                            
         SPACE 1                                                                
LRH70    L     R2,ATHISLST         MUST MOVE DIRECTLY TO SCREEN                 
         LA    R2,8(R2)            BUMP PAST HEADER                             
         MVC   LINGUA2,TIGUA       GUARANTEE CODE                               
         SH    R2,=H'8'                                                         
         SPACE 1                                                                
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         MVC   MYKEY,TIKEY         KEY OF LAST RECORD DISPLAYED                 
         LA    R2,LINNEXT          BUMP TO NEXT LIST FIELD                      
         ST    R2,ATHISLST                                                      
         LA    R2,LISTAR                                                        
         MVC   LISTAR,SPACES                                                    
         SPACE 1                                                                
LRH80    CLI   TYPE6,C'Y'          IF TYPE 6                                    
         BNE   LRHX                                                             
         CLI   OPTALL,C'Y'         & OPTION = ALL                               
         BNE   LRHX                                                             
         CLI   LISTNUM,15          AND HAVEN'T REACHED END OF PAGE              
         BL    LRH30               THEN LOOK FOR MORE GC ELS.                   
         SPACE 1                                                                
LRHX     XC    PREVDISP,PREVDISP                                                
         B     XIT                                                              
         EJECT                                                                  
*        COUNT NUMBER OF GC ELEMENTS TO DISPLAY                                 
*        & ENSURE THERE IS ENOUGH ROOM ON PAGE                                  
         SPACE 1                                                                
COUNTGC  NTR1                                                                   
         CLI   OPTALL,C'Y'         IF OPTION ALL THEN                           
         BNE   YES                                                              
         MVI   ELCODE,TAGCELQ      LOOK FOR GUARANTEE CYCLE EL.                 
         L     R4,TIAREC                                                        
         XR    R3,R3               COUNTER                                      
         BAS   RE,GETEL                                                         
         BNE   YES                                                              
*                                                                               
CGC10    LA    R3,1(R3)            INCREMENT COUNTER                            
         BAS   RE,NEXTEL                                                        
         BE    CGC10                                                            
*                                                                               
         ZIC   R1,LISTNUM          THIS LINE NUMBER MUST BE LESS THAN           
         AR    R1,R3               + LINES FOR THIS RECORD                      
         CH    R1,=H'15'           MUST BE LESS THAN TOTAL # OF LINES           
         BNH   YES                                                              
         XC    PREVDISP,PREVDISP   START FROM 1ST ELEMENT                       
         CH    R3,=H'15'           IF THIS PERSON HAS MORE THAN 15              
         BNH   NO                                                               
         CLI   LISTNUM,0           AND AT START OF NEW PAGE                     
         BH    NO                                                               
         B     YES                 THEN DISPLAY AS MUCH AS POSSIBLE NOW         
         EJECT                                                                  
*        GET TAGC ELEMENT FOR TYPE 6                                            
         SPACE 2                                                                
GETGC    NTR1                                                                   
         MVI   ELCODE,TAGCELQ      LOOK FOR GUARANTEE CYCLE EL.                 
         L     R4,TIAREC                                                        
         XR    R3,R3               R3=A(PREV EL.)                               
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         B     GGC20                                                            
         SPACE 1                                                                
GGC10    LR    R3,R4                                                            
         BAS   RE,NEXTEL           FIND LAST ELEMENT IN RECORD                  
         BNE   GGC30                                                            
         SPACE 1                                                                
GGC20    OC    PREVDISP,PREVDISP   1ST ELEMENT TO SHOW                          
         BZ    GGC10                                                            
         LR    R1,R4                                                            
         S     R1,TIAREC                                                        
         CH    R1,PREVDISP                                                      
         BNE   GGC10                                                            
         LTR   R3,R3               IS THERE A PREV. ELEMENT TO DISPLAY          
         BZ    NO                  NOPE                                         
         SPACE 1                                                                
         USING TAGCD,R3            R3=A(GUARANTEE CYCLE EL.)                    
GGC30    GOTO1 DATCON,DMCB,(X'11',TAGCPD),(8,NLINPD) PERIOD                     
         EDIT  (4,TAGCAMT),(12,LINAMT),2,MINUS=YES  GUARANTEE AMOUNT            
         EDIT  (4,TAGCBAL),(12,LINBAL),2,MINUS=YES  BALANCE                     
         SPACE 1                                                                
         CLI   OPTALL,C'Y'         IF OPTION ALL THEN                           
         BNE   GGCX                                                             
         S     R3,TIAREC                                                        
         STH   R3,PREVDISP         SAVE LAST DISPLACEMENT DISPLAYED             
         SPACE 1                                                                
GGCX     B     YES                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
MISSERR  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
RECNTFND MVI   ERROR,NOTFOUND                                                   
         B     THEEND                                                           
         SPACE 1                                                                
GCONERR  MVC   MYMSGNO,=Y(ERMUS522)  Contract code does not exist               
         J     VRENDEXT                                                         
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     VRENDEXT                                                         
                                                                                
VRENDEXT MVI   MYMTYP,C'E'                                                      
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
         SPACE 1                                                                
ENDPAGE  MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SGUSELH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
LTMORE   DC    C'(More on next page)'                                           
NEWHEADS DC    CL(L'SGUHEAD)'Sel Code C Agency  Client  Period         +        
                  D P O     Amount     Balance'                                 
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'GT',CL8'GTRACK  ',CL8'REPORT  '                              
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYCUR,L'LINGUA2-1),AL2(LINGUA2-LINED)                     
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'NLINAGY-1),AL2(NLINAGY-LINED)                     
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LINPD-1),AL2(LINPD-LINED)                         
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'GC',CL8'GCAST   ',CL8'LIST    '                              
PF14     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYCUR,L'LINGUA2-1),AL2(LINGUA2-LINED)                     
PF14X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
NPFTAB   DS    0C                                                               
         DC    AL1(NPF13X-*,13,0,(NPF13X-NPF13)/KEYLNQ,0)                       
         DC    CL3'GT',CL8'GTRACK  ',CL8'REPORT  '                              
NPF13    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYCUR,L'LINGUA2-1),AL2(LINGUA2-LINED)                     
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'NLINAGY-1),AL2(NLINAGY-LINED)                     
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LINPD-1),AL2(LINPD-LINED)                         
NPF13X   EQU   *                                                                
         DC    AL1(NPF14X-*,14,0,(NPF14X-NPF14)/KEYLNQ,0)                       
         DC    CL3'GC',CL8'GCAST   ',CL8'LIST    '                              
NPF14    DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYCUR,L'LINGUA2-1),AL2(LINGUA2-LINED)                     
NPF14X   EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'   ',CL8'GCON    ',CL8'LIST    '                             
PF15     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
*        DC    AL1(KEYTYTWA,L'SGUGCON-1),AL2(SGUGCON-T702FFD)                   
*        DC    AL1(KEYTYCUR,L'LINPD-1),AL2(LINPD-LINED)                         
PF15X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HEXFFS   DC    4X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REQUESTOR                                                  
         SSPEC H2,56,REPORT                                                     
         SSPEC H2,73,PAGE                                                       
         SPACE 1                                                                
         SSPEC H1,32,C'GUARANTEE LIST'                                          
         SSPEC H2,32,C'--------------'                                          
         SPACE 1                                                                
         SSPEC H4,2,C'CODE CY AGENCY CLIENT PERIOD'                             
         SSPEC H5,2,C'---- -- ------ ------ ------'                             
         SSPEC H4,42,C'DB P O     AMOUNT     BALANCE'                           
         SSPEC H5,42,C'-- - -     ------     -------'                           
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO CHECK IF STAFF HAS ACCESS TO GUARANTEE                      
***********************************************************************         
                                                                                
CHKACC   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
*                                                                               
         USING TAGUD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAGUELQ      GET GUARANTEE DETAILS ELEMENT                
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         OC    TAGUAGY(L'TAGUAGY+L'TAGUCLI),TAGUAGY                             
         JNZ   CACC10                                                           
         TM    TGCTSTST,TGCTSCLI                                                
         JO    CACCNO                                                           
*                                                                               
CACC10   GOTO1 CHKLIM,DMCB,TAGUAGY,TAGUCLI                                      
         JE    CACCYES                                                          
         DROP  R4                                                               
*                                                                               
         USING TAVAD,R4                                                         
CACC20   L     R4,TIAREC           ELSE, READ ALL AGENCY/CLIENT                 
         MVI   ELCODE,TAVAELQ      ELEMENTS                                     
         BAS   RE,MYGETEL                                                       
         J     *+8                                                              
CACC30   BAS   RE,MYNEXTEL                                                      
         JNE   CACCNO                                                           
*                                                                               
         XR    RE,RE                                                            
         ZIC   RF,TAVALEN          CALCULATE NUMBER OF CLIENTS                  
         SHI   RF,TAVALNQ          IN ELEMENT                                   
         LTR   RF,RF                                                            
         JNZ   CACC40                                                           
         LHI   RF,TAVALNQ                                                       
CACC40   D     RE,=A(L'TAVACLI)                                                 
*                                                                               
         LR    R2,RF               R2=(NUMBER OF CLIENTS IN ELEMENT)            
         LA    R5,TAVACLI          RE=A(CURRENT CLIENT IN ELEMENT)              
*                                                                               
CACC50   XC    TGCLI,TGCLI                                                      
         GOTO1 CHKLIM,DMCB,TAVAAGY,TGCLI                                        
         JNE   CACC30                                                           
         CLI   TAVALEN,TAVALNQ                                                  
         JE    CACCYES                                                          
*                                                                               
         GOTO1 CHKLIM,DMCB,TAVAAGY,(R5)                                         
         JE    CACCYES                                                          
*                                                                               
CACC60   LA    R5,L'TAVACLI(R5)                                                 
         BCT   R2,CACC50           BUMP TO NEXT CLIENT IN ELEMENT               
         J     CACC30                                                           
         DROP  R3,R4                                                            
*                                                                               
CACCYES  XR    R3,R3                                                            
CACCNO   XC    KEY,KEY                                                          
         MVC   KEY(L'TLGUKEY),TIKEY                                             
         GOTO1 HIGH                                                             
         MVC   AIO,TIAREC                                                       
         LTR   R3,R3                                                            
         J     XIT                                                              
*                                                                               
***********************************************************************         
*        ROUTINE GETS FIRST/NEXT ELEMENT                              *         
***********************************************************************         
                                                                                
MYGETEL  LA    R4,TLRCELEM-TLRCD(R4)                                            
CHKELCD  CLI   0(R4),0                                                          
         JE    MYELNO                                                           
         CLC   ELCODE,0(R4)                                                     
         BER   RE                                                               
MYNEXTEL ZIC   RF,1(R4)                                                         
         AR    R4,RF                                                            
         J     CHKELCD                                                          
*                                                                               
MYELNO   CR    R4,RC                                                            
         BR    RE                                                               
*                                                                               
***********************************************************************         
*        ROUTINE CHECKS IF STAFF HAS ACCESS TO AGENCY/CLIENT          *         
*        ON ENTRY ... P1=A(GUARANTEE AGENCY)                          *         
*                     P2=A(GUARANTEE CLIENT)                          *         
***********************************************************************         
                                                                                
CHKLIM   NTR1                                                                   
         L     R2,0(R1)            R2=A(GUARANTEE AGENCY)                       
         L     R3,4(R1)            R3=A(GUARANTEE CLIENT)                       
*                                                                               
         OC    FLTAGY,FLTAGY       IF FILTERING ON AGENCY                       
         JZ    CLIM10                                                           
         CLC   FLTAGY,0(R2)        AGENCY MUST MATCH IT                         
         JNE   NO                                                               
*                                                                               
CLIM10   OC    FLTCLI,FLTCLI       IF FILTERING ON CLIENT                       
         JZ    CLIM20                                                           
         OC    0(L'FLTCLI,R3),0(R3)                                             
         JZ    CLIM20                                                           
         CLC   FLTCLI,0(R3)        CLIENT MUST MATCH IT                         
         JNE   NO                                                               
*                                                                               
         USING TLSTD,R4                                                         
CLIM20   LA    R4,KEY              FIRST LOOK FOR STAFF WITHOUT                 
         XC    KEY,KEY             A USER-ID                                    
         MVI   TLSTCD,TLSTCDQ                                                   
         MVC   TLSTSTAF,TGCTSTAF                                                
         GOTO1 HIGH                                                             
         CLC   KEY(TLSSPR3-TLSTD),KEYSAVE                                       
         JE    CLIM30                                                           
*                                                                               
         MVC   KEY,KEYSAVE         IF STAFF RECORD IS NOT FOUND                 
         MVC   TLSTUSER,TWAORIG    DO NOT ALLOW ACCESS                          
         GOTO1 HIGH                                                             
         CLC   KEY(TLSSPR3-TLSTD),KEYSAVE                                       
         JNE   NO                                                               
*                                                                               
CLIM30   GOTO1 GETREC                                                           
         DROP  R4                                                               
*                                                                               
         L     R4,AIO              READ AGENCY/CLIENT LIMIT ELEMENTS            
         MVI   ELCODE,TAVAELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    CLIM60              IF NONE EXIST                                
         J     YES                 STAFF HAS ACCESS TO THE AGY/CLI              
*                                                                               
CLIM40   GOTO1 SEQ                                                              
         CLC   KEY(TLSSPR3-TLSTD),KEYSAVE                                       
         JNE   NO                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING TAVAD,R4                                                         
         L     R6,AIO              READ AGENCY/CLIENT LIMIT ELEMENTS            
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
CLIM50   BRAS  RE,NEXTEL                                                        
         JNE   CLIM40                                                           
*                                                                               
CLIM60   OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         JZ    YES                 STAFF HAS ACCESS TO AGY/CLI                  
*                                                                               
         CLC   TAVAAGY,0(R2)       IF AGENCY IS FOUND IN STAFF LIMITS           
         JNE   CLIM50              AND NO CLIENT LIMITS ARE DEFINED             
         CLI   TAVALEN,TAVALNQ     STAFF HAS ACCESS TO AGY/CLI                  
         JE    YES                                                              
         OC    0(L'TAVACLI,R3),0(R3)                                            
         JZ    YES                                                              
*                                                                               
         ZIC   RE,TAVALEN          IF AGENCY AND CLIENT ARE FOUND IN            
         SHI   RE,TAVALNQ          STAFF LIMITS                                 
         LA    RF,TAVACLI          STAFF HAS ACCESS TO AGY/CLI                  
CLIM70   CLC   0(L'TAVACLI,R3),0(RF)                                            
         JE    YES                                                              
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         JNZ   CLIM70                                                           
         J     CLIM50                                                           
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TO DENOTE MULTIPLE AGENCIES/CLIENTS ON GUARANTEE               
*        ON ENTRY ... R2=A(OUTPUT AREA)                                         
***********************************************************************         
                                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
MULTAYCL NTR1  BASE=*,LABEL=*                                                   
         USING TAVAD,R4                                                         
         L     R4,TIAREC                                                        
         MVI   ELCODE,TAVAELQ      READ ALL VALID AGENCY/CLIENT                 
         BRAS  RE,GETEL            ELEMENTS                                     
         J     MAC20                                                            
MAC10    BRAS  RE,NEXTEL                                                        
MAC20    JNE   MAC50                                                            
*                                                                               
         CLC   TAVAAGY,TIAGY       IF MORE THAN ONE AGENCY FOUND                
         JE    MAC30                                                            
         MVI   NLINAGYX,C'*'       DENOTE WITH AN *                             
*                                                                               
MAC30    OC    TICLI,TICLI         IF MORE THAN ONE CLIENT FOUND                
         JZ    MAC10                                                            
         CLI   TAVALEN,TAVALNQ+L'TAVACLI                                        
         JNE   MAC40                                                            
         CLC   TAVACLI,TICLI                                                    
         JE    MAC10                                                            
MAC40    MVI   LINCLIX,C'*'        DENOTE WITH A *                              
         J     MAC10                                                            
         DROP  R4                                                               
*                                                                               
MAC50    GOTO1 SQUASHER,DMCB,NLINAGY,L'NLINAGY+L'NLINAGYX                       
         GOTO1 SQUASHER,DMCB,LINCLI,L'LINCLI+L'LINCLIX                          
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
LINDATA  DS    CL70                                                             
         ORG   LINED                                                            
LINGUA   DS    CL4                                                              
         DS    CL1                                                              
LINT6    DS    CL1                                                              
         DS    CL1                                                              
NLINAGY  DS    CL6                                                              
NLINAGYX DS    CL1                                                              
         ORG   NLINAGY                                                          
         DS    CL1                                                              
LINAGY   DS    CL6                                                              
         DS    CL1                                                              
LINCLI   DS    CL6                                                              
LINCLIX  DS    CL1                                                              
LINPD    DS    CL17                                                             
         ORG   LINPD                                                            
         DS    CL1                                                              
NLINPD   DS    CL17                                                             
         DS    CL1                                                              
LINDBAL  DS    CL1                                                              
         DS    CL1                                                              
LINPNH   DS    CL1                                                              
         DS    CL1                                                              
LINOVER  DS    CL1                                                              
LINAMT   DS    CL12                                                             
LINBAL   DS    CL12                                                             
         ORG                                                                    
         DS    CL8                                                              
LINGUA2  DS    CL4                                                              
         DS    CL8                 SELECT HEADER                                
         DS    CL3                 SELECT                                       
         DS    CL8                 SELECT EXTENDED HEADER                       
         DS    CL8                 HEADER FOR LIST LINE                         
LINNEXT  EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR86D                                                       
         SPACE 3                                                                
SVADD    DS    F                   PREVIOUS ELEMENT ADDRESS                     
PREVDISP DS    H                   PREVIOUS START DISPLACEMENT                  
COUNTER  DS    PL4                 RECORD COUNTER                               
OPTALL   DS    CL1                 OPTION ALL                                   
TYPE6    DS    CL1                 FLAG THIS IS TYPE 6                          
MYSTAT   DS    XL1                                                              
NEXTPEND EQU   X'80'               ANOTHER RECORD PENDING                       
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
MYKEY    DS    CL(L'KEY)           SAVED KEY                                    
FLTAGY   DS    CL(L'TIFAGY)        AGENCY FILTER                                
FLTCLI   DS    CL(L'TIFCLI)        CLIENT FILTER                                
FLTGCNT  DS    CL(L'TAGUGCNT)      GCON FILTER                                  
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012TAGEN86   07/20/12'                                      
         END                                                                    
