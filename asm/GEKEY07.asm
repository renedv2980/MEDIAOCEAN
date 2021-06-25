*          DATA SET GEKEY07    AT LEVEL 074 AS OF 01/13/16                      
*PHASE TF0507B                                                                  
*INCLUDE TINVCON                                                                
         TITLE 'TF0507 - PFM OVERLAY FOR TALENT SYSTEM'                         
TF0507   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TF0507,R7,RR=R3                                                
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
******************************************************************              
****                                                                            
****     LR    RF,RA                             USING PFM DSECT                
****     USING PFMSAVED,RF                                                      
****     GOTO1 HEXOUT,DMCB,FILETYPE,13(R2),1,0  DISPLAY FILETYPE                
****     DROP  RF                                                               
******************************************************************              
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
         LR    R4,R3               R4=A(FIELD TABLE ENTRY FOR THIS REC)         
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELDS                                                 *         
***********************************************************************         
*                                                                               
         USING FLDD,R4             R4=A(FIELD TABLE ENTRIES)                    
*                                                                               
         XC    INTKEY,INTKEY       INITIALIZE KEY                               
         MVC   INTKEY(4),=C'K,X'''                                              
         LA    R3,INTKEY+6         SET TO START AFTER RECORD CODE               
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
         ZIC   R5,FLDLEN           R5=L'FIELD IN KEY                            
*                                                                               
         BAS   RE,MODIFY           MAY BE ABLE TO MODIFY OUTPUT                 
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
VK90     MVI   0(R3),C''''         DONE - END LAST TYPE                         
*                                                                               
         GOTO1 HEXOUT,DMCB,1(R4),INTKEY+4,1  NOW SET RECORD TYPE                
*                                                                               
         CLC   =C'A-',CONRCRD      IF NOT ASKING FOR D/A                        
         BNE   VK100               PASS BACK KEY                                
*                                                                               
         OC    INTKEY,SPACES       ELSE DECODE AND MOVE TO KEY                  
         GOTO1 VDECODE,DMCB,INTKEY+2,KEY,0                                      
         CLI   8(R1),X'FF'                                                      
         BE    VK100               INVALID, SO PASS BACK TO PFM                 
*                                                                               
         MVC   FILENAME,=CL8'TALDIR' SET TO READ TALDIR                         
         CLC   =C'CK',CONRCRD        IF RECORD TYPE IS CHECK                    
         BE    *+14                                                             
         CLC   =C'A-CK',CONRCRD                                                 
         BNE   *+10                                                             
         MVC   FILENAME,=CL8'CHKDIR' SET TO READ CHKDIR                         
*                                                                               
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         CLC   TLDRKEY,KEYSAVE     ONLY IF WE HAVE EXACT MATCH                  
         BNE   VK100                                                            
         MVC   DISKA,TLDRDA        PASS BACK DISK ADDRESS                       
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(2),=C'A,'              BUILD NEW PFM KEY - A,D/A          
         GOTO1 HEXOUT,DMCB,DISKA,INTKEY+2,4  DISPLAY D/A                        
*                                                                               
VK100    LR    R2,RA                                                            
         USING PFMSAVED,R2                                                      
         MVC   DISKADDR(L'DISKA+L'INTKEY),DISKA  PASS KEY BACK TO PFM           
         MVC   XKEY,INTKEY         SET TRANSFER KEY                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS CURRENT FIELD TYPE AND HANDLES TYPE CHANGES         
*                                                                               
*                                  R3=A(CURRENT POSITION IN KEY)                
         USING FLDD,R4             R4=A(FIELD TABLE ENTRY)                      
TYPESET  DS    0H                                                               
         MVI   THISTYPE,C'X'       ASSUME HEX                                   
*                                                                               
         TM    FLDSTAT,FLDCHR+FLDNUM  CHARACTER/NUMERIC                         
         BZ    TSET10                                                           
         TM    FLDSTAT,FLDCOMP        AS LONG AS THEY'RE NOT COMP.              
         BO    TSET10                                                           
         CLI   5(R2),0                AND AS LONG AS THERE'S INPUT              
         BE    TSET10                                                           
         MVI   THISTYPE,C'C'          ARE TYPE CHAR                             
*                                                                               
TSET10   CLC   THISTYPE,LASTTYPE   IF TYPE HAS CHANGED                          
         BE    TSETX                                                            
*                                                                               
         MVI   0(R3),C''''         END PREVIOUS                                 
         MVC   1(1,R3),THISTYPE    SET NEW TYPE                                 
         MVI   2(R3),C''''         START NEW                                    
*                                                                               
         LA    R3,3(R3)            BUMP TO NEXT POSITION IN KEY                 
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
VALINV   NTR1                                                                   
         GOTO1 =V(TINVCON),DMCB,8(R2),WORK,DATCON,RR=RELO  INVOICE NO.          
         CLI   0(R1),X'FF'                                                      
         BE    FLDINV                                                           
         B     XIT                                                              
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
         DC    C'AG      ',AL4(RECAG)                                           
         DC    C'AGN     ',AL4(RECAGN)                                          
         DC    C'AK      ',AL4(RECAK)                                           
         DC    C'AN      ',AL4(RECAN)                                           
         DC    C'ANN     ',AL4(RECANN)                                          
         DC    C'AR      ',AL4(RECAR)                                           
         DC    C'AT      ',AL4(RECAT)                                           
         DC    C'AY      ',AL4(RECAY)                                           
         DC    C'AYG     ',AL4(RECAYG)                                          
         DC    C'AYN     ',AL4(RECAYN)                                          
         DC    C'BA      ',AL4(RECBA)                                           
         DC    C'CA      ',AL4(RECCA)                                           
         DC    C'CAA     ',AL4(RECCAA)                                          
         DC    C'CAC     ',AL4(RECCAC)                                          
         DC    C'CAG     ',AL4(RECCAG)                                          
         DC    C'CAH     ',AL4(RECCAH)                                          
         DC    C'CAHCO   ',AL4(RECCAHCO)                                        
         DC    C'CG      ',AL4(RECCG)                                           
         DC    C'CGN     ',AL4(RECCGN)                                          
         DC    C'CK      ',AL4(RECCK)                                           
         DC    C'CKB     ',AL4(RECCKB)                                          
         DC    C'CKC     ',AL4(RECCKC)                                          
         DC    C'CKD     ',AL4(RECCKD)                                          
         DC    C'CKE     ',AL4(RECCKE)                                          
         DC    C'CKH     ',AL4(RECCKH)                                          
         DC    C'CKL     ',AL4(RECCKL)                                          
         DC    C'CKY     ',AL4(RECCKY)                                          
         DC    C'CL      ',AL4(RECCL)                                           
         DC    C'CLG     ',AL4(RECCLG)                                          
         DC    C'CLN     ',AL4(RECCLN)                                          
         DC    C'CM      ',AL4(RECCM)                                           
         DC    C'CN      ',AL4(RECCN)                                           
         DC    C'CO      ',AL4(RECCO)                                           
         DC    C'COA     ',AL4(RECCOA)                                          
         DC    C'COC     ',AL4(RECCOC)                                          
         DC    C'COI     ',AL4(RECCOI)                                          
         DC    C'COK     ',AL4(RECCOK)                                          
         DC    C'CON     ',AL4(RECCON)                                          
         DC    C'COM     ',AL4(RECCOM)                                          
         DC    C'COP     ',AL4(RECCOP)                                          
         DC    C'COG     ',AL4(RECCOG)                                          
         DC    C'COL     ',AL4(RECCOL)                                          
         DC    C'CT      ',AL4(RECCT)                                           
         DC    C'DC      ',AL4(RECDC)                                           
         DC    C'DL      ',AL4(RECDL)                                           
         DC    C'DT      ',AL4(RECDT)                                           
         DC    C'DU      ',AL4(RECDU)                                           
         DC    C'DV      ',AL4(RECDV)                                           
         DC    C'DVO     ',AL4(RECDVO)                                          
         DC    C'EC      ',AL4(RECEC)                                           
         DC    C'ECC     ',AL4(RECECC)                                          
         DC    C'ED      ',AL4(RECED)                                           
         DC    C'EM      ',AL4(RECEM)                                           
         DC    C'EP      ',AL4(RECEP)                                           
         DC    C'ES      ',AL4(RECES)                                           
         DC    C'EX      ',AL4(RECEX)                                           
         DC    C'FT      ',AL4(RECFT)                                           
         DC    C'GL      ',AL4(RECGL)                                           
         DC    C'GT      ',AL4(RECGT)                                           
         DC    C'GU      ',AL4(RECGU)                                           
         DC    C'HC      ',AL4(RECHC)                                           
         DC    C'IF      ',AL4(RECIF)                                           
         DC    C'IN      ',AL4(RECIN)                                           
         DC    C'INB     ',AL4(RECINB)                                          
         DC    C'INC     ',AL4(RECINC)                                          
         DC    C'IND     ',AL4(RECIND)                                          
         DC    C'INH     ',AL4(RECINH)                                          
         DC    C'INK     ',AL4(RECINK)                                          
         DC    C'JB      ',AL4(RECJB)                                           
         DC    C'LN      ',AL4(RECLN)                                           
         DC    C'LNR     ',AL4(RECLNR)                                          
         DC    C'LO      ',AL4(RECLO)                                           
         DC    C'MT      ',AL4(RECMT)                                           
         DC    C'MU      ',AL4(RECMU)                                           
         DC    C'NX      ',AL4(RECNX)                                           
         DC    C'NXN     ',AL4(RECNXN)                                          
         DC    C'OF      ',AL4(RECOF)                                           
         DC    C'OG      ',AL4(RECOG)                                           
         DC    C'OGN     ',AL4(RECOGN)                                          
         DC    C'PR      ',AL4(RECPR)                                           
         DC    C'PRG     ',AL4(RECPRG)                                          
         DC    C'PRN     ',AL4(RECPRN)                                          
         DC    C'PG      ',AL4(RECPG)                                           
         DC    C'PGN     ',AL4(RECPGN)                                          
         DC    C'PT      ',AL4(RECPT)                                           
         DC    C'PTN     ',AL4(RECPTN)                                          
         DC    C'SC      ',AL4(RECSC)                                           
         DC    C'SE      ',AL4(RECSE)                                           
         DC    C'SG      ',AL4(RECSG)                                           
         DC    C'SS      ',AL4(RECSS)                                           
         DC    C'ST      ',AL4(RECST)                                           
         DC    C'STN     ',AL4(RECSTN)                                          
         DC    C'SY      ',AL4(RECSY)                                           
         DC    C'UH      ',AL4(RECUH)                                           
         DC    C'US      ',AL4(RECUS)                                           
         DC    C'W2      ',AL4(RECW2)                                           
         DC    C'W4      ',AL4(RECW4)                                           
         DC    C'W4C     ',AL4(RECW4C)                                          
         DC    C'W4N     ',AL4(RECW4N)                                          
         DC    C'W4O     ',AL4(RECW4O)                                          
         DC    X'FF'                                                            
         EJECT                                                                  
*              TABLES TO COVER FIELDS FOR EACH RECORD                           
         SPACE 2                                                                
RECAG    DC    AL1(23,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agy Group'                                   
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLAGCDQ)                                               
*                                                                               
RECAGN   DC    AL1(11,0)                                                        
         DC    AL1(16,FLDCHR),CL10'Agg Name'                                    
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLAGNCDQ)                                              
*                                                                               
RECAK    DC    AL1(03,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(08,FLDCHR),CL10'Network id'                                  
         DC    AL1(03,FLDCHR),CL10'Client'                                      
         DC    AL1(03,FLDCHR),CL10'Product'                                     
         DC    AL1(01,FLDCHR),CL10'Media'                                       
         DC    AL1(03,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    X'FF',AL1(TLAKCDQ)                                               
*                                                                               
RECAN    DC    AL1(25,0)                                                        
         DC    AL1(04,FLDCHR),CL10'Agent'                                       
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLANCDQ)                                               
*                                                                               
RECANN   DC    AL1(11,0)                                                        
         DC    AL1(16,FLDCHR),CL10'Agt Name'                                    
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLANNCDQ)                                              
*                                                                               
RECAR    DC    AL1(25,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Area'                                        
         DC    AL1(03,0)                                                        
         DC    X'FF',AL1(TLARCDQ)                                               
*                                                                               
RECAT    DC    AL1(15,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(02,FLDCHR),CL10'Att Code'                                    
         DC    AL1(06,0)                                                        
         DC    X'FF',AL1(TLATCDQ)                                               
*                                                                               
RECAY    DC    AL1(21,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(04,0)                                                        
         DC    X'FF',AL1(TLAYCDQ)                                               
*                                                                               
RECAYG   DC    AL1(15,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agy Group'                                   
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(04,0)                                                        
         DC    X'FF',AL1(TLAYGCDQ)                                              
*                                                                               
RECAYN   DC    AL1(11,0)                                                        
         DC    AL1(16,FLDCHR),CL10'Agy Name'                                    
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLAYNCDQ)                                              
*                                                                               
RECBA    DC    AL1(23,0)                                                        
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Date'                                
         DC    AL1(01,FLDCHR),CL10'Currency'                                    
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLBACDQ)                                               
*                                                                               
RECCA    DC    AL1(07,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(06,FLDHEX),CL10'Cast Sort'                                   
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLCACDQ)                                               
*                                                                               
RECCAA   DC    AL1(01,0)                                                        
         DC    AL1(04,FLDCHR),CL10'Agent'                                       
         DC    AL1(02,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(06,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(02,FLDHEX),CL10'Cast Seq'                                    
         DC    X'FF',AL1(TLCAACDQ)                                              
*                                                                               
RECCAC   DC    AL1(07,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(06,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(02,FLDHEX),CL10'Cast Seq'                                    
         DC    X'FF',AL1(TLCACCDQ)                                              
*                                                                               
RECCAG   DC    AL1(09,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(04,FLDCHR),CL10'Guarantee'                                   
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(02,FLDHEX),CL10'Cast Seq'                                    
         DC    X'FF',AL1(TLCAGCDQ)                                              
*                                                                               
RECCAH   DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(06,FLDHEX),CL10'Cast Sort'                                   
         DC    AL1(03,FLDDTE),CL10'Applc Date'                                  
         DC    AL1(06,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDDTE),CL10'Cast FFC'                                    
         DC    X'FF',AL1(TLCAHCDQ)                                              
*                                                                               
RECCAHCO DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(06,0)                                                        
         DC    AL1(03,FLDDTE),CL10'Comml FFC'                                   
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(06,FLDCHR),CL10'Product'                                     
         DC    X'FF',AL1(TLCAHCDQ)                                              
*                                                                               
RECCG    DC    AL1(23,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Cli Group'                                   
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLCGCDQ)                                               
*                                                                               
RECCGN   DC    AL1(11,0)                                                        
         DC    AL1(16,FLDCHR),CL10'Clg Name'                                    
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLCGNCDQ)                                              
*                                                                               
RECCK    DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    AL1(06,FLDHEX),CL10'Cast Sort'                                   
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLCKCDQ)                                               
*                                                                               
RECCKB   DC    AL1(01,FLDCHR),CL10'Currency'                                    
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Bill Date'                           
         DC    AL1(04,FLDHEX+FLDCOMP),CL10'Bill Time'                           
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(03,FLDHEX),CL10'Inv#+2(3)'                                   
         DC    AL1(02,FLDHEX),CL10'Cast Seq'                                    
         DC    X'FF',AL1(TLCKBCDQ)                                              
*                                                                               
RECCKC   DC    AL1(16,0)                                                        
         DC    AL1(08,FLDNUM+FLDCOMP),CL10'Check Num'                           
         DC    AL1(06,FLDCHR),CL10'Bank'                                        
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLCKCCDQ)                                              
*                                                                               
RECCKD   DC    AL1(09,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(06,FLDCHR),CL10'Reference'                                   
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Check Date'                          
         DC    AL1(04,FLDHEX+FLDCOMP),CL10'Sequence'                            
         DC    X'FF',AL1(TLCKDCDQ)                                              
*                                                                               
RECCKE   DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(01,FLDCHR),CL10'Currency'                                    
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Check Date'                          
         DC    AL1(04,FLDHEX+FLDCOMP),CL10'Sequence'                            
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(04,FLDCHR),CL10'Agent'                                       
         DC    AL1(01,0)                                                        
         DC    X'FF',AL1(TLCKECDQ)                                              
*                                                                               
RECCKH   DC    AL1(06,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(06,FLDRTN+FLDCOMP),CL10'Invoice',AL4(VALINV)                 
         DC    AL1(02,FLDHEX),CL10'Cast Seq'                                    
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLCKHCDQ)                                              
*                                                                               
*                                                                               
RECCKL   DC    AL1(09,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(06,FLDCHR),CL10'Reference'                                   
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Check Date'                          
         DC    AL1(04,FLDHEX+FLDCOMP),CL10'Sequence'                            
         DC    X'FF',AL1(TLCKLCDQ)                                              
*                                                                               
RECCKY   DC    AL1(04,FLDCHR),CL10'Year'                                        
         DC    AL1(01,FLDCHR),CL10'Currency'                                    
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDCHR),CL10'Tax Unit'                                    
         DC    AL1(01,0)                                                        
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Check Date'                          
         DC    AL1(04,FLDHEX+FLDCOMP),CL10'Check Seq'                           
         DC    AL1(01,FLDCHR),CL10'W4 Type'                                     
         DC    AL1(01,FLDHEX),CL10'Tacwstat'                                    
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLCKYCDQ)                                              
*                                                                               
RECCL    DC    AL1(15,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLCLCDQ)                                               
*                                                                               
RECCLG   DC    AL1(07,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Cli Group'                                   
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLCLGCDQ)                                              
*                                                                               
RECCLN   DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(16,FLDCHR),CL10'Cli Name'                                    
         DC    AL1(02,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLCLNCDQ)                                              
*                                                                               
RECCM    DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(01,FLDCHR),CL10'Cmnt Type'                                   
         DC    AL1(18,FLDCHR),CL10'Data'                                        
         DC    AL1(01,FLDHEX),CL10'Cmnt Level'                                  
         DC    AL1(04,0)                                                        
         DC    X'FF',AL1(TLCMCDQ)                                               
*                                                                               
RECCN    DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(03,0)                                                        
         DC    AL1(12,FLDCHR),CL10'Cont ID'                                     
         DC    AL1(03,0)                                                        
         DC    AL1(03,FLDDTE),CL10'Start Date'                                  
         DC    AL1(03,FLDDTE),CL10'End Date'                                    
         DC    X'FF',AL1(TLCNCDQ)                                               
*                                                                               
RECCO    DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(06,FLDCHR),CL10'Product'                                     
         DC    AL1(08,FLDCHR),CL10'Comml ID'                                    
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    X'FF',AL1(TLCOCDQ)                                               
*                                                                               
RECCOA   DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(12,FLDCHR),CL10'Contract #'                                  
         DC    AL1(12,FLDCHR),CL10'Comml ID'                                    
         DC    X'FF',AL1(TLCOACDQ)                                              
*                                                                               
RECCOC   DC    AL1(25,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLCOCCDQ)                                              
*                                                                               
RECCOI   DC    AL1(03,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(12,FLDCHR),CL10'Com/Ver ID'                                  
         DC    AL1(08,0)                                                        
         DC    X'FF',AL1(TLCOICDQ)                                              
*                                                                               
RECCOK   DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(12,FLDCHR),CL10'Aka CID'                                     
         DC    AL1(12,FLDCHR),CL10'Comml ID'                                    
         DC    X'FF',AL1(TLCOKCDQ)                                              
*                                                                               
RECCOM   DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(08,FLDCHR),CL10'Music'                                       
         DC    AL1(01,0)                                                        
         DC    AL1(12,FLDCHR),CL10'Comml ID'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLCOMCDQ)                                              
*                                                                               
RECCON   DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(20,FLDCHR),CL10'Comml Name'                                  
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLCONCDQ)                                              
*                                                                               
RECCOP   DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(06,FLDCHR),CL10'Product'                                     
         DC    AL1(08,FLDCHR),CL10'Comml ID'                                    
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    X'FF',AL1(TLCOPCDQ)                                              
*                                                                               
RECCOG   DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Client Grp'                                  
         DC    AL1(12,FLDCHR),CL10'Comml ID'                                    
         DC    X'FF',AL1(TLCOGCDQ)                                              
*                                                                               
RECCOL   DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Cli Grp'                                     
         DC    AL1(20,FLDCHR),CL10'Comml Name'                                  
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    X'FF',AL1(TLCOLCDQ)                                              
*                                                                               
RECCT    DC    AL1(07,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(10,0)                                                        
         DC    X'FF',AL1(TLCTCDQ)                                               
*                                                                               
RECDC    DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(12,FLDCHR),CL10'Comml ID'                                    
         DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Advice'                                      
         DC    AL1(03,0)                                                        
         DC    X'FF',AL1(TLDCCDQ)                                               
*                                                                               
RECDL    DC    AL1(15,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLDLCDQ)                                               
*                                                                               
RECDT    DC    AL1(09,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(06,FLDCHR),CL10'Reference'                                   
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Date'                                
         DC    AL1(04,FLDHEX+FLDCOMP),CL10'Sequence'                            
         DC    X'FF',AL1(TLDTCDQ)                                               
*                                                                               
RECDU    DC    AL1(07,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Reference'                                   
         DC    AL1(02,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLDUCDQ)                                               
*                                                                               
RECDV    DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(12,FLDCHR),CL10'Comml ID'                                    
         DC    AL1(01,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Advice'                                      
         DC    AL1(03,0)                                                        
         DC    X'FF',AL1(TLDVCDQ)                                               
*                                                                               
RECDVO   DC    AL1(01,FLDCHR),CL10'Office'                                      
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(01,0)                                                        
         DC    AL1(03,FLDDTE),CL10'Send Date'                                   
         DC    AL1(01,0)                                                        
         DC    AL1(12,FLDCHR),CL10'Comml ID'                                    
         DC    AL1(06,FLDCHR),CL10'Advice'                                      
         DC    AL1(01,0)                                                        
         DC    X'FF',AL1(TLDVOCDQ)                                              
*                                                                               
RECEC    DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(05,FLDCHR+FLDCOMP),CL10'Episode #'                           
         DC    AL1(01,0)                                                        
         DC    AL1(01,FLDHEX),CL10'Ecast Sort'                                  
         DC    AL1(02,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    X'FF',AL1(TLECCDQ)                                               
*                                                                               
RECECC   DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(05,FLDCHR+FLDCOMP),CL10'Episode #'                           
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(04,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Category'                                    
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    X'FF',AL1(TLECCCDQ)                                              
*                                                                               
RECED    DC    AL1(16,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(01,FLDHEX+FLDCOMP),CL10'Year'                                
         DC    AL1(06,FLDCHR),CL10'Edit Type'                                   
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLEDCDQ)                                               
*                                                                               
RECEM    DC    AL1(26,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLEMCDQ)                                               
*                                                                               
RECEP    DC    AL1(18,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(05,FLDCHR+FLDCOMP),CL10'Episode #'                           
         DC    AL1(2,0)                                                         
         DC    X'FF',AL1(TLEPCDQ)                                               
*                                                                               
RECES    DC    AL1(04,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(20,FLDCHR),CL10'Estimate'                                    
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLESCDQ)                                               
*                                                                               
RECEX    DC    AL1(26,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLEXCDQ)                                               
*                                                                               
RECFT    DC    AL1(02,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(02,FLDHEX),CL10'Cast Seq'                                    
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Start Date'                          
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'End Date'                            
         DC    AL1(02,FLDHEX),CL10'Track No.'                                   
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    X'FF',AL1(TLFTCDQ)                                               
*                                                                               
RECGL    DC    AL1(20,0)                                                        
         DC    AL1(01,FLDCHR),CL10'List Type'                                   
         DC    AL1(01,0)                                                        
         DC    AL1(08,FLDCHR),CL10'List Code'                                   
         DC    AL1(01,0)                                                        
         DC    X'FF',AL1(TLGLCDQ)                                               
*                                                                               
RECGT    DC    AL1(01,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,0)                                                        
         DC    AL1(04,FLDCHR),CL10'Guarantee'                                   
         DC    AL1(02,0)                                                        
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Start Date'                          
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'End Date'                            
         DC    AL1(02,0)                                                        
         DC    AL1(02,FLDHEX),CL10'Track No.'                                   
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLGTCDQ)                                               
*                                                                               
RECGU    DC    AL1(14,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    AL1(04,FLDCHR+FLDCOMP),CL10'Guarantee'                           
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLGUCDQ)                                               
*                                                                               
RECHC    DC    AL1(15,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDRTN+FLDCOMP),CL10'Invoice',AL4(VALINV)                 
         DC    AL1(01,0)                                                        
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLHCCDQ)                                               
*                                                                               
RECIF    DC    AL1(17,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(03,FLDCHR),CL10'Prod Cli'                                    
         DC    AL1(05,0)                                                        
         DC    X'FF',AL1(TLIFCDQ)                                               
*                                                                               
RECIN    DC    AL1(13,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDRTN+FLDCOMP),CL10'Invoice',AL4(VALINV)                 
         DC    AL1(04,0)                                                        
         DC    X'FF',AL1(TLINCDQ)                                               
*                                                                               
RECINB   DC    AL1(15,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDRTN+FLDCOMP),CL10'Invoice',AL4(VALINV)                 
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLINBCDQ)                                              
*                                                                               
RECINC   DC    AL1(15,0)                                                        
         DC    AL1(03,FLDDTE),CL10'Due Date'                                    
         DC    AL1(01,FLDHEX),CL10'Sort Code'                                   
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    X'FF',AL1(TLINCCDQ)                                              
*                                                                               
RECIND   DC    AL1(03,FLDDTE),CL10'Bill Date'                                   
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    AL1(01,FLDCHR),CL10'Currency'                                    
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(01,FLDCHR),CL10'Office'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(05,0)                                                        
         DC    X'FF',AL1(TLINDCDQ)                                              
*                                                                               
RECINH   DC    AL1(15,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDRTN+FLDCOMP),CL10'Invoice',AL4(VALINV)                 
         DC    AL1(01,0)                                                        
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLINHCDQ)                                              
*                                                                               
RECINK   DC    AL1(03,FLDDTE),CL10'Check Date'                                  
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    AL1(01,FLDCHR),CL10'Currency'                                    
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(01,FLDCHR),CL10'Office'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(05,0)                                                        
         DC    X'FF',AL1(TLINKCDQ)                                              
*                                                                               
RECJB    DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Date'                                
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(06,FLDCHR),CL10'Product'                                     
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLJBCDQ)                                               
*                                                                               
RECLN    DC    AL1(12,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Lien'                                        
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLLNCDQ)                                               
*                                                                               
RECLNR   DC    AL1(09,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    AL1(01,FLDBIN),CL10'Rank'                                        
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Lien'                                        
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLLNRCDQ)                                              
*                                                                               
RECLO    DC    AL1(22,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Union'                                       
         DC    AL1(01,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Local'                                       
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLLOCDQ)                                               
*                                                                               
RECMU    DC    AL1(14,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(01,0)                                                        
         DC    AL1(08,FLDCHR),CL10'Music'                                       
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLMUCDQ)                                               
*                                                                               
RECMT    DC    AL1(14,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(01,0)                                                        
         DC    AL1(02,FLDBIN),CL10'Market'                                      
         DC    AL1(08,0)                                                        
         DC    X'FF',AL1(TLMTCDQ)                                               
*                                                                               
RECNX    DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(08,FLDCHR),CL10'Network id'                                  
         DC    AL1(03,FLDCHR),CL10'Client'                                      
         DC    AL1(03,FLDCHR),CL10'Product'                                     
         DC    AL1(01,FLDCHR),CL10'Media'                                       
         DC    AL1(03,FLDCHR),CL10'Use'                                         
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Date'                                
         DC    AL1(02,FLDBIN),CL10'User ID'                                     
         DC    AL1(01,FLDHEX),CL10'Chg Code'                                    
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLNXCDQ)                                               
*                                                                               
RECNXN   DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(16,FLDCHR),CL10'NID Name'                                    
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Date'                                
         DC    AL1(02,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLNXNCDQ)                                              
*                                                                               
RECOF    DC    AL1(30,0)                                                        
         DC    AL1(01,FLDCHR),CL10'Office'                                      
         DC    X'FF',AL1(TLOFCDQ)                                               
*                                                                               
RECOG    DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(06,FLDCHR),CL10'Product'                                     
         DC    AL1(12,FLDCHR),CL10'Comml Grp'                                   
         DC    AL1(01,0)                                                        
         DC    X'FF',AL1(TLOGCDQ)                                               
*                                                                               
RECOGN   DC    AL1(05,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(16,FLDCHR),CL10'Comml Name'                                  
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLOGNCDQ)                                              
*                                                                               
RECPG    DC    AL1(11,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(06,FLDCHR),CL10'Prd Group'                                   
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLPGCDQ)                                               
*                                                                               
RECPGN   DC    AL1(05,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(16,FLDCHR),CL10'Prg Name'                                    
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLPGNCDQ)                                              
*                                                                               
RECPT    DC    AL1(17,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Prd Type'                                    
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLPTCDQ)                                               
*                                                                               
RECPTN   DC    AL1(05,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(16,FLDCHR),CL10'PrdTyp Nam'                                  
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLPTNCDQ)                                              
*                                                                               
RECPR    DC    AL1(07,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Product'                                     
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLPRCDQ)                                               
*                                                                               
RECPRG   DC    AL1(07,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(06,FLDCHR),CL10'Prd Group'                                   
         DC    AL1(06,FLDCHR),CL10'Product'                                     
         DC    X'FF',AL1(TLPRGCDQ)                                              
*                                                                               
RECPRN   DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(06,FLDCHR),CL10'Client'                                      
         DC    AL1(15,FLDCHR),CL10'Prd Name'                                    
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLPRNCDQ)                                              
*                                                                               
RECSC    DC    AL1(11,0)                                                        
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDRTN),CL10'Invoice',AL4(VALINV)                         
         DC    AL1(01,0)                                                        
         DC    AL1(01,FLDHEX),CL10'Page'                                        
         DC    AL1(01,0)                                                        
         DC    AL1(01,FLDHEX),CL10'Screen'                                      
         DC    AL1(01,0)                                                        
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLSCCDQ)                                               
*                                                                               
RECSE    DC    AL1(14,0)                                                        
         DC    AL1(01,FLDHEX),CL10'Program'                                     
         DC    AL1(08,FLDCHR),CL10'Record'                                      
         DC    AL1(08,FLDCHR),CL10'Action'                                      
         DC    X'FF',AL1(TLSECDQ)                                               
*                                                                               
RECSG    DC    AL1(14,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    AL1(04,FLDCHR+FLDCOMP),CL10'Guarantee'                           
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLSGCDQ)                                               
*                                                                               
RECSS    DC    AL1(03,0)                                                        
         DC    AL1(01,FLDCHR),CL10'Media'                                       
         DC    AL1(06,FLDCHR),CL10'Agency'                                      
         DC    AL1(20,FLDCHR),CL10'Estimate'                                    
         DC    AL1(01,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLSSCDQ)                                               
*                                                                               
RECST    DC    AL1(17,0)                                                        
         DC    AL1(02,FLDBIN),CL10'User ID'                                     
         DC    AL1(02,0)                                                        
         DC    AL1(08,FLDCHR),CL10'Staff'                                       
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLSTCDQ)                                               
*                                                                               
RECSTN   DC    AL1(03,0)                                                        
         DC    AL1(12,FLDCHR),CL10'Last Name'                                   
         DC    AL1(12,FLDCHR),CL10'First Name'                                  
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLSTNCDQ)                                              
*                                                                               
RECSY    DC    AL1(31,0)                                                        
         DC    X'FF',AL1(TLSYCDQ)                                               
*                                                                               
RECUH    DC    AL1(12,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Int Comml'                                   
         DC    AL1(02,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Use Code'                                    
         DC    AL1(02,0)                                                        
         DC    AL1(06,FLDRTN+FLDCOMP),CL10'Invoice',AL4(VALINV)                 
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLUHCDQ)                                               
*                                                                               
RECUS    DC    AL1(25,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Use'                                         
         DC    AL1(03,0)                                                        
         DC    X'FF',AL1(TLUSCDQ)                                               
*                                                                               
RECW2    DC    AL1(11,0)                                                        
         DC    AL1(04,FLDCHR),CL10'Year'                                        
         DC    AL1(01,FLDCHR),CL10'Currency'                                    
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(03,FLDDTE+FLDCOMP),CL10'Change Dte'                          
         DC    X'FF',AL1(TLW2CDQ)                                               
*                                                                               
RECW4    DC    AL1(20,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLW4CDQ)                                               
*                                                                               
RECW4C   DC    AL1(09,0)                                                        
         DC    AL1(09,FLDCHR),CL10'Corp. ID'                                    
         DC    AL1(02,0)                                                        
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLW4CCDQ)                                              
*                                                                               
RECW4N   DC    AL1(02,0)                                                        
         DC    AL1(16,FLDCHR),CL10'Last Name'                                   
         DC    AL1(08,FLDCHR),CL10'First Name'                                  
         DC    AL1(01,0)                                                        
         DC    AL1(04,FLDHEX),CL10'Sequence'                                    
         DC    X'FF',AL1(TLW4NCDQ)                                              
*                                                                               
RECW4O   DC    AL1(17,0)                                                        
         DC    AL1(03,FLDCHR),CL10'Employer'                                    
         DC    AL1(09,FLDCHR),CL10'S/S Number'                                  
         DC    AL1(02,0)                                                        
         DC    X'FF',AL1(TLW4OCDQ)                                              
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
FLDLNQ   EQU   *-FLDD                                                           
FLDTAG   DS    CL10                TAG FOR SCREEN (IF FLDSTAT/=0)               
FLDAVAL  DS    AL4                 A(VALIDATION ROUTINE) IF FLDRTN SET          
         EJECT                                                                  
       ++INCLUDE GEKEYWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
         SPACE 2                                                                
         ORG   SYSSPARE                                                         
DISKA    DS    XL4                                                              
INTKEY   DS    CL60                                                             
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
* TAGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074GEKEY07   01/13/16'                                      
         END                                                                    
