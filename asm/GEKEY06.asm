*          DATA SET GEKEY06    AT LEVEL 020 AS OF 07/29/03                      
*PHASE TF0506A                                                                  
         TITLE 'TF0506 - PFM OVERLAY FOR ACCPAK SYSTEM'                         
TF0506   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TF0506,R7,RR=R3                                                
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         MVC   LKEY,=Y(L'ACCKEY) DETAILS OF DIRECTORY AND KEY                   
         MVC   LSTATUS,=Y(L'ACCRSTA)                                            
         MVC   DATADISP,=Y(ACCRFST-ACCRECD)                                     
         MVC   SYSFIL,=C'ACCMST  '                                              
         MVC   SYSDIR,=C'ACCDIR  '                                              
         MVI   GETMSYS,6           USE GETMSG FOR SYSTEM 06                     
         MVC   RCPROG(2),=C'AC'    PREFIX FOR REPORT NUMBER                     
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                 *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,CONRCRDH         VALIDATE RECORD FIELD                        
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                R1=L'INPUT-1                                 
         SPACE 1                                                                
         LA    RF,WORK             MAY ASK FOR DISK ADDRESS                     
         CLC   =C'A-',0(RF)                                                     
         BNE   *+16                                                             
         LA    RF,2(RF)            RECORD IS AT +2                              
         SH    R1,=H'2'                                                         
         BM    FLDINV                                                           
         SPACE 1                                                                
         LA    R4,RECTABLE         R4=A(TABLE OF VALID RECORD TYPES)            
         SPACE 1                                                                
         CLI   8(R2),C'?'          USER ASKED FOR HELP?                         
         BNE   VR10                                                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         OI    STATFLAG,X'01'      HELP INVOKED                                 
         USING CONHEADH-64,RA                                                   
         XC    CONRCRD,CONRCRD     NULL THE DATA                                
         OI    CONRCRDH+6,X'80'    TRANSMIT THE FIELD                           
         GOTO1 CLRSCN,DMCB,CONP0H  CLEAR THE SCREEN                             
         GOTO1 DISPHELP,DMCB,RECTABLE  PRINT HELP TABLE                         
         B     XIT                                                              
         SPACE 1                                                                
VR10     CLI   0(R4),X'FF'                                                      
         BE    FLDINV                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),0(R4)       MATCH AGAINST INPUT                          
         BE    VR20                                                             
         LA    R4,L'RECTABLE(R4)                                                
         B     VR10                                                             
         SPACE 1                                                                
VR20     DS    0H                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         TM    STATFLAG,X'01'      WAS HELP INVOKED??                           
         BZ    VR25                NO                                           
         USING CONHEADH-64,RA                                                   
         GOTO1 CLRSCN,DMCB,CONP0H  YES, CLEAR THE SCREEN                        
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         NI    STATFLAG,X'FF'-X'01'  HELP NOT INVOKED ANYMORE                   
         USING CONHEADH-64,RA                                                   
         SPACE 1                                                                
VR25     L     R4,8(R4)                                                         
         A     R4,RELO             R4=A(FIELD TABLE ENTRY FOR THIS REC)         
         USING FLDD,R4                                                          
         LR    R3,R4               (SAVE IT)                                    
         SPACE 1                                                                
         LA    R2,CONP0H           POINT TO 1ST TAG FIELD                       
VR30     CLI   0(R4),X'FF'         TEST NO MORE FIELDS                          
         BE    VR50                                                             
         CLI   FLDSTAT,0           SKIP DUMMY ENTRIES                           
         BE    VR40                                                             
         MVC   8(L'FLDTAG,R2),FLDTAG  DISPLAY TAG                               
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP             BUMP TO INPUT FIELD                          
         NI    1(R2),X'FF'-X'20'   UNPROTECT IT                                 
         BAS   RE,BUMP             BUMP TO NEXT PROTECTED FIELD                 
         BE    VR60                                                             
         SPACE 1                                                                
VR40     BAS   RE,BUMPTBL          AND TO NEXT FIELD IN KEY                     
         B     VR30                                                             
         SPACE 1                                                                
VR50     XC    8(L'CONP0,R2),8(R2)  CLEAR REMAINING FIELDS                      
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP             BUMP TO INPUT FIELD                          
         XC    8(L'CONI0,R2),8(R2) CLEAR AND                                    
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP             BUMP TO NEXT PROTECTED FIELD                 
         BNE   VR50                                                             
         SPACE 1                                                                
VR60     OC    CONP0,CONP0         TEST ANY FIELDS TO VALIDATE                  
         BZ    VR90                                                             
         LA    R2,CONI0H           POINT TO 1ST UNPROTECTED FIELD               
VR70     CLI   5(R2),0             IS THERE ANY INPUT ALREADY                   
         BNE   VR90                SO GO VALIDATE                               
         BAS   RE,BUMP2            BUMP TO NEXT UNPROTECTED FIELD               
         BNE   VR70                                                             
         B     PLSENTER            E-O-S - GIVE USER A CHANCE                   
         SPACE 1                                                                
VR90     DS    0H                  SCREEN ALREADY BUILT - GO VALIDATE           
         MVC   PFMKEY,SPACES       INITIALIZE KEY TO SPACES                     
         MVC   PFMKEY(2),=C'K,'                                                 
         LA    RE,PFMKEY+2                                                      
         CLI   0(R4),X'FF'         TEST FOR X'FF' ENTRY                         
         BNE   VR90B                                                            
         TM    FLDRSTAT,FLD00S     INIT TO X'00'?                               
         BZ    VR90A                                                            
         XC    PFMKEY,PFMKEY                                                    
         MVC   PFMKEY(2),=C'K,'                                                 
VR90A    OC    FLDRTYPE,FLDRTYPE   IS THERE A RECORD TYPE?                      
         BZ    VR90B                                                            
         MVC   0(L'FLDRTYPE,RE),FLDRTYPE                                        
         LA    RE,L'FLDRTYPE(RE)                                                
         OC    FLDRTYPE,FLDRSUBT   IS THERE A SUB RECORD TYPE?                  
         BZ    VR90B                                                            
         MVC   0(L'FLDRSUBT,RE),FLDRSUBT                                        
         LA    RE,L'FLDRSUBT(RE)                                                
VR90B    LR    R4,R3               R4=A(FIELD TABLE ENTRY FOR THIS REC)         
         LR    R3,RE               R3=SET TO START IN PFMKEY OF INPUT           
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELDS                                                 *         
***********************************************************************         
*                                                                               
         USING FLDD,R4             R4=A(FIELD TABLE ENTRIES)                    
*                                                                               
         MVI   LASTTYPE,C'X'       SET LAST TYPE IS HEX                         
*                                                                               
         LA    R2,CONI0H           R2=A(FIRST INPUT FIELD)                      
*                                                                               
VK10     CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    VK90                                                             
*                                                                               
         BAS   RE,TYPESET          SET TYPE - HANDLE TYPE CHANGE                
*                                                                               
         BAS   RE,VALFLD           VALIDATE THIS FIELD - SET WORK               
*                                                                               
         SR    R5,R5                                                            
         IC    R5,FLDLEN           R5=L'FIELD IN KEY                            
*                                                                               
*        BAS   RE,MODIFY           MAY BE ABLE TO MODIFY OUTPUT                 
*                                                                               
         CLI   THISTYPE,C'C'       IF CURRENT TYPE IS CHARACTER                 
         BNE   VK60                                                             
         BCTR  R5,0                SET FOR EXECUTED MOVE                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK        MOVE TO KEY                                  
         LA    R3,1(R5,R3)         BUMP TO NEXT POSITION IN KEY                 
         B     VK70                                                             
*                                                                               
VK60     GOTO1 HEXOUT,DMCB,WORK,(R3),(R5),0  MOVE HEX DATA TO KEY               
         SLL   R5,1                TAKES TWICE AS MUCH ROOM TO DISPLAY          
         AR    R3,R5               BUMP TO NEXT POSITION IN KEY                 
*                                                                               
VK70     CLI   FLDSTAT,0           IF NOT DUMMY ENTRY                           
         BE    *+8                                                              
         BAS   RE,BUMP2            BUMP TO NEXT UNPROTECTED FIELD               
*                                                                               
         BAS   RE,BUMPTBL          BUMP TO NEXT FIELD IN KEY                    
         B     VK10                                                             
*                                                                               
VK90     CLI   LASTTYPE,C'C'                                                    
         BNE   *+8                                                              
         MVI   0(R3),C')'          DONE - END LAST TYPE IF CHAR                 
*                                                                               
*                                                                               
         CLC   =C'A-',CONRCRD      IF NOT ASKING FOR D/A                        
         BNE   VK100               PASS BACK KEY                                
*                                                                               
         OC    PFMKEY,SPACES       ELSE DECODE AND MOVE TO KEY                  
         GOTO1 VDECODE,DMCB,PFMKEY+2,KEY,0                                      
         CLI   8(R1),X'FF'                                                      
         BE    VK100               INVALID, SO PASS BACK TO PFM                 
*                                                                               
         MVC   FILENAME,=CL8'ACCDIR' SET TO READ ACCDIR                         
*                                                                               
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         LA    R3,KEY                                                           
         USING ACCRECD,R3                                                       
         CLC   ACCKEY,KEYSAVE     ONLY IF WE HAVE EXACT MATCH                   
         BNE   VK100                                                            
         MVC   DISKA,ACCKDA        PASS BACK DISK ADDRESS                       
         XC    PFMKEY,PFMKEY                                                    
         MVC   PFMKEY(2),=C'A,'              BUILD NEW PFM KEY - A,D/A          
         GOTO1 HEXOUT,DMCB,DISKA,PFMKEY+2,4 DISPLAY D/A                         
*                                                                               
VK100    LR    R2,RA                                                            
         USING PFMSAVED,R2                                                      
         MVC   DISKADDR(L'DISKA+L'PFMKEY),DISKA PASS KEY BACK TO PFM            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS CURRENT FIELD TYPE AND HANDLES TYPE CHANGES         
*                                                                               
*                                  R3=A(CURRENT POSITION IN KEY)                
         USING FLDD,R4                R4=A(FIELD TABLE ENTRY)                   
TYPESET  DS    0H                                                               
         MVI   THISTYPE,C'X'          ASSUME HEX                                
*                                                                               
         TM    FLDSTAT,FLDCHR+FLDNUM  CHARACTER/NUMERIC                         
         BZ    TSET10                                                           
         TM    FLDSTAT,FLDCOMP        AS LONG AS THEY'RE NOT COMP.              
         BO    TSET10                                                           
*        CLI   5(R2),0                AND AS LONG AS THERE'S INPUT              
*        BE    TSET10                                                           
         MVI   THISTYPE,C'C'          ARE TYPE CHAR                             
*                                                                               
TSET10   CLC   THISTYPE,LASTTYPE      IF TYPE HAS CHANGED                       
         BE    TSETX                                                            
*                                                                               
         CLI   THISTYPE,C'C'          IF CHAR OPEN (                            
         BNE   *+8                                                              
         MVI   0(R3),C'('             OPEN NEW TYPE                             
         CLI   LASTTYPE,C'C'          IF CHAR CLOSE )                           
         BNE   TSET12                                                           
         MVI   0(R3),C')'             OPEN NEW TYPE                             
*                                                                               
TSET12   LA    R3,1(R3)            BUMP TO NEXT POSITION IN KEY                 
*                                                                               
TSETX    MVC   LASTTYPE,THISTYPE   MOVE THIS TYPE TO LAST                       
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE VALIDATES INPUT AND SETS IN WORK                         
*                                                                               
         USING FLDD,R4             R4=A(FIELD TABLE ENTRY)                      
VALFLD   NTR1                                                                   
         XC    WORK,WORK           PRE-CLEAR OUTPUT AREA - ASSUME HEX           
*                                                                               
         TM    FLDSTAT,FLDNUM      NUMERIC                                      
         BZ    *+14                                                             
         MVI   WORK,C'0'                                                        
         MVC   WORK+1(L'WORK-1),WORK                                            
         TM    FLDSTAT2,FLD40S                                                  
         BZ    *+10                                                             
         MVC   WORK,SPACES                                                      
*                                                                               
         CLI   THISTYPE,C'X'       OK IF HEX                                    
         BE    VFLD05                                                           
*                                                                               
         TM    FLDSTAT,FLDCHR      CHARACTER                                    
         BZ    *+10                                                             
         MVC   WORK,SPACES                                                      
*                                                                               
VFLD05   CLI   FLDSTAT,0           DUMMY ENTRY - NO FIELD, SO SKIP              
         BE    VFLDX                                                            
         CLI   5(R2),0             NO INPUT OK                                  
         BE    VFLDX                                                            
         ZIC   R5,5(R2)            R5=L'INPUT                                   
*                                                                               
         LA    R3,WORK             R3=A(OUTPUT AREA)                            
         TM    FLDSTAT,FLDNUM+FLDHEX  NUMERIC AND HEX ARE RIGHT-ALIGNED         
         BZ    VFLD10                                                           
         TM    FLDSTAT,FLDLEFT     UNLESS SPECIFIED OTHERWISE                   
         BO    VFLD10                                                           
         ZIC   R1,FLDLEN                                                        
         LR    RF,R5                                                            
         TM    FLDSTAT,FLDHEX      L'HEX INPUT IS REALLY HALF                   
         BZ    *+8                                                              
         SRL   RF,1                                                             
         SR    R1,RF                                                            
         BNM   *+6                                                              
         SR    R1,R1                                                            
         LA    R3,WORK(R1)         R3=A(RIGHT-ALIGNED OUTPUT AREA)              
*                                                                               
VFLD10   TM    FLDSTAT,FLDRTN      TEST WE HAVE VALIDATION ROUTINE              
         BZ    VFLD20                                                           
         L     RF,FLDAVAL          GET A(VALIDATION ROUTINE)                    
         A     RF,RELO                                                          
         BASR  RE,RF               VALIDATE FIELD - RETURN IN WORK              
         B     VFLD90                                                           
*                                                                               
VFLD20   TM    FLDSTAT,FLDNUM+FLDBIN  NUMERIC/BINARY DATA                       
         BZ    VFLD30                                                           
         TM    4(R2),X'08'         TEST VALID NUMERIC DATA                      
         BZ    FLDINV                                                           
*                                                                               
VFLD30   TM    FLDSTAT,FLDCHR+FLDNUM  CHARACTER OR NUMERIC DATA                 
         BZ    VFLD40                                                           
         BCTR  R5,0                SET FOR EXECUTED MOVE                        
         EX    R5,*+8                                                           
         B     VFLD90                                                           
         MVC   0(0,R3),8(R2)                                                    
*                                                                               
VFLD40   TM    FLDSTAT,FLDBIN      BINARY DATA                                  
         BZ    VFLD50                                                           
         BCTR  R5,0                SET FOR EXECUTED PACK                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RF,DUB                                                           
         ST    RF,DUB              FULLWORD IN DUB                              
         LA    RF,4                                                             
         ZIC   R1,FLDLEN           R1=L'FIELD IN KEY                            
         SR    RF,R1               RF=DISPL. INTO DUB OF START OF DATA          
         LA    RF,DUB(RF)          RF=A(DATA)                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     VFLD90                                                           
         MVC   0(0,R3),0(RF)                                                    
*                                                                               
VFLD50   TM    FLDSTAT,FLDHEX      HEX DATA                                     
         BZ    VFLD60                                                           
         GOTO1 HEXIN,DMCB,8(R2),(R3),(R5)                                       
         OC    12(4,R1),12(R1)                                                  
         BZ    FLDINV                                                           
         B     VFLD90                                                           
*                                                                               
VFLD60   TM    FLDSTAT,FLDDTE      DATES                                        
         BO    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,VALDTE                                                        
*                                                                               
VFLD90   TM    FLDSTAT,FLDCOMP     TEST NEED TO COMPLEMENT                      
         BZ    *+10                                                             
         XC    WORK,HEXFFS                                                      
*                                                                               
VFLDX    B     XIT                                                              
         EJECT                                                                  
*              SPECIAL VALIDATION ROUTINES                                      
*                                                                               
*                                  R2=A(INPUT FIELD HEADER)                     
         SPACE 3                                                                
VALDTE   NTR1                                                                   
         GOTO1 DATVAL,DMCB,(0,8(R2)),DUB  VALIDATE FOR YMD                      
         CLI   3(R1),0                                                          
         BNE   VDTEX                                                            
         SPACE 1                                                                
         GOTO1 (RF),(R1),(1,8(R2)),DUB    ELSE VALIDATE FOR YM                  
         CLI   3(R1),0                                                          
         BE    FLDINV                                                           
         MVC   DUB(2),=C'91'              OK, SO SET THIS YEAR                  
         SPACE 1                                                                
VDTEX    GOTO1 DATCON,DMCB,(0,DUB),(1,WORK)  RETURN IN PWOS FORMAT              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETECTS WHETHER IT'S MORE EFFICIENT TO MODIFY            
*                                                                               
*                                  R3=A(CURRENT POSITION IN KEY)                
         USING FLDD,R4             R4=A(FIELD TABLE ENTRY)                      
*                                  R5=L'FIELD IN KEY                            
MODIFY   DS    0H                                                               
         CLI   THISTYPE,C'C'       IF THIS TYPE IS CHARACTER                    
         BNE   *+16                                                             
         CLI   FLDLEN,7            THEN DON'T BOTHER IF < 7                     
         BL    MODX                                                             
         B     MOD10                                                            
         CLI   FLDLEN,4            ELSE DON'T BOTHER IF < 4                     
         BL    MODX                                                             
*                                                                               
MOD10    CLI   FLDSTAT,0           IF DUMMY ENTRY, MODIFY                       
         BE    MOD20                                                            
         CLI   5(R2),0             IF NO INPUT, MODIFY                          
         BE    MOD20                                                            
*                                                                               
         B     MODX                DON'T BOTHER CHANGING                        
*                                                                               
MOD20    BCTR  R3,0                SLIDE BACK ONE                               
         CLI   0(R3),C''''         IF WE WERE STARTING NEW TYPE                 
         BNE   *+10                                                             
         BCTR  R3,0                SLIDE BACK ONE MORE                          
         B     *+12                                                             
         MVI   1(R3),C''''         ELSE END LAST TYPE                           
         LA    R3,2(R3)                                                         
*                                                                               
         EDIT  FLDLEN,(2,(R3)),ALIGN=LEFT,WRK=BLOCK  DISPLAY LENGTH             
         AR    R3,R0                                                            
         MVC   0(1,R3),THISTYPE                      THEN TYPE                  
         MVI   1(R3),C''''                                                      
         LA    R3,2(R3)                                                         
*                                                                               
         LA    R5,1                MODIFY L'FIELD - ASSUME 1                    
         CLI   FLDSTAT,0           IF NOT DUMMY ENTRY                           
         BE    *+16                                                             
         CLI   5(R2),0             AND IF THERE'S INPUT                         
         BE    *+8                                                              
         IC    R5,5(R2)            USE L'INPUT                                  
*                                                                               
         MVI   LASTTYPE,C' '       NEXT TIME SHOULD TREAT AS NEW TYPE           
*                                                                               
MODX     BR    RE                                                               
         EJECT                                                                  
*              GENERAL SUBSIDIARY ROUTINES                                      
         SPACE 3                                                                
BUMP2    ZIC   R1,0(R2)            BUMP TWO FIELDS                              
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
BUMP     ZIC   R1,0(R2)            BUMP ONE FIELD                               
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         BR    RE                                                               
         SPACE 3                                                                
         USING FLDD,R4                                                          
BUMPTBL  LA    R1,FLDLNQ           BUMP TO NEXT ENTRY IN FIELD TABLE            
         CLI   FLDSTAT,0                                                        
         BE    BMPT2                                                            
         LA    R1,L'FLDTAG(R1)                                                  
         TM    FLDSTAT,FLDRTN                                                   
         BZ    BMPT2                                                            
         LA    R1,4(R1)                                                         
BMPT2    AR    R4,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
*              ERROR, EXIT ROUTINES                                             
         SPACE 2                                                                
FLDMISS  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
FLDINV   MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
PLSENTER XC    GETTXTCB,GETTXTCB   DEFINE CONTROL BLOCK                         
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMTYP,GTMINF       SET INFORMATION TYPE                         
         MVI   GTMSGNO+1,2         AND MESSAGE NUMBER                           
         MVI   GTMSYS,X'FF'        AND MESSAGE SYSTEM                           
         OI    GENSTAT2,USGETTXT   CALL FOR GETTXT                              
         LA    R2,CONI0H           R2=A(FIRST INPUT FIELD)                      
         B     ERREXIT                                                          
*                                                                               
ERREXIT  GOTO1 ERREX                                                            
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
RELO     DS    F                                                                
HEXFFS   DC    (L'WORK)X'FF'                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              TABLE OF VALID RECORD CODES                                      
         SPACE 1                                                                
RECTABLE DS    0CL12                                                            
         DC    C'ACT     ',AL4(RECACT)                                          
         DC    C'CON     ',AL4(RECCON)                                          
         DC    C'CONH    ',AL4(RECCONH)                                         
         DC    C'TRAN    ',AL4(RECTRAN)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLES TO COVER FIELDS FOR EACH RECORD                           
         SPACE 2                                                                
RECACT   DC    AL1(1,FLDHEX,0,0),CL8'Company'                                   
         DC    AL1(1,FLDCHR,0,0),CL8'Unit'                                      
         DC    AL1(1,FLDCHR,0,0),CL8'Ledger'                                    
         DC    AL1(12,FLDCHR,0,0),CL8'Account'                                  
         DC    X'FF',AL1(0),AL1(0),AL1(FLD40S)                                  
         SPACE 2                                                                
RECCON   DC    AL1(1,FLDHEX,0,0),CL8'Company'                                   
         DC    AL1(1,FLDCHR,0,0),CL8'Unit'                                      
         DC    AL1(1,FLDCHR,0,0),CL8'Ledger'                                    
         DC    AL1(12,FLDCHR,0,0),CL8'Account'                                  
         DC    AL1(2,FLDCHR,0,0),CL8'Off/Wc'                                    
         DC    AL1(1,FLDHEX,0,0),CL8'Company'                                   
         DC    AL1(1,FLDCHR,0,0),CL8'Unit'                                      
         DC    AL1(1,FLDCHR,0,0),CL8'Ledger'                                    
         DC    AL1(12,FLDCHR,0,0),CL8'CAccount'                                 
         DC    AL1(3,0,FLD40S,0)                                                
         DC    AL1(4,FLDCHR,0,0),CL8'BucketTy'                                  
         DC    X'FF',AL1(0),AL1(0),AL1(FLD40S)                                  
         SPACE 2                                                                
RECCONH  DC    AL1(1,FLDHEX,0,0),CL8'Company'                                   
         DC    AL1(1,FLDCHR,0,0),CL8'Unit'                                      
         DC    AL1(1,FLDCHR,0,0),CL8'Ledger'                                    
         DC    AL1(12,FLDCHR,0,0),CL8'Account'                                  
         DC    AL1(2,FLDCHR,0,0),CL8'Off/Wc'                                    
         DC    AL1(1,FLDHEX,0,0),CL8'Company'                                   
         DC    AL1(1,FLDCHR,0,0),CL8'Unit'                                      
         DC    AL1(1,FLDCHR,0,0),CL8'Ledger'                                    
         DC    AL1(12,FLDCHR,0,0),CL8'CAccount'                                 
         DC    AL1(4,0,FLD40S,0)                                                
         DC    AL1(6,0,FLD00S,0)                                                
         DC    X'FF',AL1(0),AL1(0),AL1(FLD40S)                                  
         SPACE 2                                                                
RECTRAN  DC    AL1(1,FLDHEX,0,0),CL8'Company'                                   
         DC    AL1(1,FLDCHR,0,0),CL8'Unit'                                      
         DC    AL1(1,FLDCHR,0,0),CL8'Ledger'                                    
         DC    AL1(12,FLDCHR,0,0),CL8'Account'                                  
         DC    AL1(2,FLDCHR,0,0),CL8'Off/Wc'                                    
         DC    AL1(1,FLDHEX,0,0),CL8'Company'                                   
         DC    AL1(1,FLDCHR,0,0),CL8'Unit'                                      
         DC    AL1(1,FLDCHR,0,0),CL8'Ledger'                                    
         DC    AL1(12,FLDCHR,0,0),CL8'CAccount'                                 
         DC    AL1(3,FLDDTE,0,0),CL8'Date'                                      
         DC    AL1(6,FLDCHR,0,0),CL8'Ref'                                       
         DC    AL1(1,FLDHEX,0,0),CL8'Seq'                                       
         DC    X'FF',AL1(0),AL1(0),AL1(FLD40S)                                  
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER FIELD TABLES                                      
         SPACE 2                                                                
FLDD     DSECT                                                                  
FLDLEN   DS    XL1                 L'FIELD                                      
FLDSTAT  DS    XL1                 STATUS                                       
FLDCHR   EQU   X'80'               CHARACTER FIELD (SPACES PADDED)              
FLDNUM   EQU   X'40'               NUMERIC FIELD (C'0' PADDED)                  
FLDHEX   EQU   X'20'               HEX FIELD (HEXIN - X'00' PADDED)             
FLDBIN   EQU   X'10'               HEX FIELD (HEXIN - X'00' PADDED)             
FLDDTE   EQU   X'08'               DATE FIELD (PWOS - X'00' PADDED)             
FLDRTN   EQU   X'04'               A(VALIDATION ROUTINE) DEFINED                
FLDCOMP  EQU   X'02'               COMPLEMENT THIS FIELD                        
FLDLEFT  EQU   X'01'               DON'T RIGHT-ALIGN HEX/NUM FIELD              
FLDSTAT2 DS    XL1                 STATUS 2                                     
FLD00S   EQU   X'80'               CLEAR FIELD TO X'00' S                       
FLD40S   EQU   X'40'               CLEAR FIELD TO X'40' S                       
         DS    XL1                 N/D                                          
         ORG   FLDLEN                                                           
FLDFF    DS    XL1                 FF INDICATES LAST ENTRY                      
FLDRTYPE DS    XL1                 RECORD TYPE                                  
FLDRSUBT DS    XL1                 SUB RECORD TYPE                              
FLDRSTAT DS    XL1                 RECORD STATUS(SAME AS STAT2 ABOVE)           
FLDLNQ   EQU   *-FLDD                                                           
FLDTAG   DS    CL8                 TAG FOR SCREEN (IF FLDSTAT/=0)               
FLDAVAL  DS    AL4                 A(VALIDATION ROUTINE) IF FLDRTN SET          
         EJECT                                                                  
       ++INCLUDE GEKEYWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
         SPACE 2                                                                
         ORG   SYSSPARE                                                         
DISKA    DS    XL4                                                              
PFMKEY   DS    CL60                                                             
THISTYPE DS    CL1                                                              
LASTTYPE DS    CL1                                                              
         EJECT                                                                  
       ++INCLUDE GEKEYFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
       ++INCLUDE GEPFMSAVE         (OUR MAINTENANCE SCREEN OVERLAY)             
* FAFACTS                                                                       
* FALANG                                                                        
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020GEKEY06   07/29/03'                                      
         END                                                                    
