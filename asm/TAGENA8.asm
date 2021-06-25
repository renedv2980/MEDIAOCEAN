*          DATA SET TAGENA8    AT LEVEL 011 AS OF 05/01/02                      
*PHASE T702A8A                                                                  
         TITLE 'T702A8 - PMUSIC RECORD MAINTENANCE'                             
T702A8   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702A8                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROL                                                     
*                                                                               
         GOTO1 INITIAL,DMCB,PFTBL  INITIALIZE                                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,BLDREC                                                        
         B     XIT                                                              
*                                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   *+12                                                             
         BAS   RE,DELCHECK                                                      
         B     XIT                                                              
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    MN10                                                             
         CLI   MODE,XRECADD                                                     
         BE    MN10                                                             
         CLI   MODE,XRECPUT                                                     
         BNE   XIT                                                              
*                                                                               
MN10     BAS   RE,DISPLAY                                                       
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE KEY                                                     
*                                                                               
VKEY     NTR1                                                                   
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SMUAGYH),SMUAGYNH                     
         GOTO1 RECVAL,DMCB,TLMUCDQ,(X'40',SMUMUSH)                              
*                                                                               
         B     XIT                                                              
         SPACE 3                                                                
*              DISPLAY KEY                                                      
*                                                                               
DKEY     NTR1                                                                   
*                                                                               
         MVC   AIO,AIO2            USE ALTERNATE IO AREA                        
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SMUAGYH),SMUAGYNH                     
*                                                                               
         MVC   AIO,AIO1            RESTORE IO AREA                              
         MVC   KEY,SVKEY           RESTORE KEY                                  
*                                                                               
         L     R3,AIO              R3=A(RECORD)                                 
         USING TLMUD,R3                                                         
         MVC   SMUMUS,TLMUMUS      MUSIC NUMBER                                 
         OI    SMUMUSH+6,X'80'                                                  
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TESTS WHETHER DELETE ALLOWED                             
*                                                                               
DELCHECK NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2                                                         
*                                                                               
         USING TLCOPD,R3           CAN'T DELETE MUSIC RECORD IF. . .            
         LA    R3,KEY              . . . THERE ARE COMMERCIALS FOR IT           
         XC    KEY,KEY                                                          
         MVI   TLCOPCD,TLCOMCDQ    RECORD CODE                                  
         MVC   TLCOMAGY,TGAGY      AGENCY                                       
         MVC   TLCOMMUS,TGMUS      MUSIC CODE                                   
         ZIC   R4,DMINBTS          SAVE DMINBTS                                 
         NI    DMINBTS,X'F7'       ENSURE NOT READING DELETED                   
         GOTO1 HIGH                                                             
         STC   R4,DMINBTS          RESTORE DMINBTS                              
         CLC   TLCOPKEY(TLCOMCID-TLCOPD),KEYSAVE                                
         BNE   *+12                                                             
         MVI   ERROR,ERINVDEL      DELETE NOT ALLOWED                           
         B     TRAPERR                                                          
*                                                                               
         MVC   KEY,SVKEY           RESTORE SAVED KEY                            
         MVC   AIO,AIO1                                                         
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE BUILDS RECORD                                            
*                                                                               
BLDREC   NTR1                                                                   
*                                                                               
         MVI   ERROR,INVALID       ONLY ERROR POSSIBLE                          
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
* COMPOSITION ELEMENT                                                           
*                                                                               
         GOTO1 NAMIN,DMCB,(2,TANAELQ),SMUNAMEH  COMPOSITION ELEMENT             
*                                                                               
         MVI   ELCODE,TAMUELQ                                                   
         GOTO1 REMELEM             REMOVE ALL MUSIC ELEMENTS                    
*                                                                               
         LA    R3,ELEM                                                          
         USING TAMUD,R3                                                         
*                                                                               
* PUBLISHER 1 ELEMENT                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   TAMUEL,TAMUELQ                                                   
         LA    R0,TAMULNQ          LENGTH OF ELEMENT WITHOUT NAME               
         LA    R2,SMUPUB1H         PUBLISHER 1 NAME                             
         GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         AR    R0,R1                                                            
         STC   R0,TAMULEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAMUNAME(0),8(R2)                                                
         MVI   TAMUTYPE,TAMUTP1                                                 
*                                                                               
         LA    R2,SMULC1H          LICENSER 1                                   
         GOTO1 ANY                                                              
         GOTO1 LICVAL,DMCB,(X'80',SMULC1)                                       
         BNE   TRAPERR                                                          
         MVC   TAMULIC,TGLCCDE                                                  
*                                                                               
         GOTO1 ADDELEM             ADD THE ELEMENT                              
*                                                                               
* PUBLISHER 2 ELEMENT                                                           
*                                                                               
         LA    R2,SMUPUB2H         PUBLISHER 2 NAME                             
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1                                                            
         BNZ   *+20                                                             
         LA    R2,SMULC2H          IF THERE'S NO PUBLISHER 2. . .               
         CLI   5(R2),0             . . . THEN LICENSER IS DISALLOWED            
         BE    BLD20                                                            
         B     TRAPERR                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   TAMUEL,TAMUELQ                                                   
         LA    R0,TAMULNQ          LENGTH OF ELEMENT WITHOUT NAME               
         AR    R0,R1                                                            
         STC   R0,TAMULEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAMUNAME(0),8(R2)                                                
         MVI   TAMUTYPE,TAMUTP2                                                 
*                                                                               
         LA    R2,SMULC2H          LICENSER 2                                   
         CLI   5(R2),0                                                          
         BE    BLD10                                                            
         GOTO1 LICVAL,DMCB,(X'80',SMULC2)                                       
         BNE   TRAPERR                                                          
         MVC   TAMULIC,TGLCCDE                                                  
*                                                                               
BLD10    GOTO1 ADDELEM             ADD THE ELEMENT                              
*                                                                               
* COMPOSER ELEMENT                                                              
*                                                                               
BLD20    XC    ELEM,ELEM                                                        
         MVI   TAMUEL,TAMUELQ                                                   
         LA    R2,SMUCOMPH         COMPOSER NAME                                
         GOTO1 ANY                                                              
         LA    RF,TAMUNAME         NAME FIELD                                   
         LA    R0,SMUCOMLH         A(LAST NAME FIELD)                           
         SR    R1,R1                                                            
*                                                                               
BLD30    IC    R1,0(R2)                                                         
         CLI   5(R2),0                                                          
         BE    BLD40                                                            
         LR    RE,R1                                                            
         SH    RE,=H'17'           16 FOR HEADERS, 1 FOR EX                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),SPACES                                                   
         LA    RF,1(RE,RF)                                                      
*                                                                               
BLD40    AR    R2,R1               BUMP TO NEXT TWA FIELD                       
         CR    R2,R0                                                            
         BH    BLD50                                                            
         IC    R1,0(R2)                                                         
         LA    RE,0(R1,R2)                                                      
         SH    RE,=H'8'            R1 = A(FIELD ID NUMBER)                      
         CLI   0(RE),87            COMPOSER NAME FIELD?                         
         BNE   BLD40               NO -- IT'S THE LICENSER FIELD                
         B     BLD30                                                            
*                                                                               
BLD50    BCTR  RF,0                BACK UP TO NON-BLANK CHARACTER               
         CLI   0(RF),C' '                                                       
         BNH   BLD50                                                            
         LA    RF,1(RF)                                                         
         LA    RE,ELEM                                                          
         SR    RF,RE                                                            
         STC   RF,TAMULEN          TOTAL ELEMENT LENGTH                         
*                                                                               
         MVI   TAMUTYPE,TAMUTCOM   TYPE COMPOSER                                
*                                                                               
         LA    R2,SMULC3H          COMPOSER LICENSER                            
         CLI   5(R2),0                                                          
         BE    BLD60                                                            
         GOTO1 LICVAL,DMCB,(X'80',SMULC3)                                       
         BNE   TRAPERR                                                          
         MVC   TAMULIC,TGLCCDE                                                  
*                                                                               
BLD60    GOTO1 ADDELEM             ADD THE ELEMENT                              
*                                                                               
* AUTHOR ELEMENT                                                                
*                                                                               
         LA    R2,SMUAUTHH         AUTHOR NAME                                  
         XC    ELEM,ELEM                                                        
         MVI   TAMUEL,TAMUELQ                                                   
         LA    RF,TAMUNAME         NAME FIELD                                   
         LA    R0,SMUAUTLH         A(LAST NAME FIELD)                           
         SR    R1,R1                                                            
*                                                                               
BLD70    IC    R1,0(R2)                                                         
         CLI   5(R2),0                                                          
         BE    BLD80                                                            
         LR    RE,R1                                                            
         SH    RE,=H'17'           16 FOR HEADERS, 1 FOR EX                     
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),8(R2)                                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,RF),SPACES                                                   
         LA    RF,1(RE,RF)                                                      
*                                                                               
BLD80    AR    R2,R1               BUMP TO NEXT TWA FIELD                       
         CR    R2,R0                                                            
         BH    BLD90                                                            
         IC    R1,0(R2)                                                         
         LA    RE,0(R1,R2)                                                      
         SH    RE,=H'8'            R1 = A(FIELD ID NUMBER)                      
         CLI   0(RE),89            AUTHOR NAME FIELD?                           
         BNE   BLD80               NO -- IT'S THE LICENSER FIELD                
         B     BLD70                                                            
*                                                                               
BLD90    LA    R0,TAMUNAME                                                      
         CR    R0,RF               WAS AN AUTHOR NAME GIVEN?                    
         BNE   BLD100              YES                                          
         LA    R2,SMULC4H          IF THERE'S NO AUTHOR. . .                    
         CLI   5(R2),0             . . . THEN LICENSER IS DISALLOWED            
         BE    BLD120                                                           
         B     TRAPERR                                                          
*                                                                               
BLD100   BCTR  RF,0                BACK UP TO NON-BLANK CHARACTER               
         CLI   0(RF),C' '                                                       
         BNH   BLD90                                                            
         LA    RF,1(RF)                                                         
         LA    RE,ELEM                                                          
         SR    RF,RE                                                            
         STC   RF,TAMULEN          TOTAL ELEMENT LENGTH                         
*                                                                               
         MVI   TAMUTYPE,TAMUTAUT   TYPE AUTHOR                                  
*                                                                               
         LA    R2,SMULC4H          AUTHOR LICENSER                              
         CLI   5(R2),0                                                          
         BE    BLD110                                                           
         GOTO1 LICVAL,DMCB,(X'80',SMULC4)                                       
         BNE   TRAPERR                                                          
         MVC   TAMULIC,TGLCCDE                                                  
*                                                                               
BLD110   GOTO1 ADDELEM             ADD THE ELEMENT                              
*                                                                               
* CLIENT ELEMENT                                                                
*                                                                               
BLD120   GOTO1 NAMIN,DMCB,TAFNELQ,SMUCLIH,TAFNTCLI  CLIENT NAME                 
*                                                                               
* PRODUCT ELEMENT                                                               
*                                                                               
         GOTO1 NAMIN,DMCB,TAFNELQ,(X'80',SMUPRDH),TAFNTPRD PRODUCT NAME         
*                                                                               
* ACTIVITY ELEMENT                                                              
*                                                                               
         GOTO1 ACTVIN,DMCB,0       ADD ACTIVITY ELEMENT                         
*                                                                               
         MVC   KEY,SVKEY           RESTORE OFFICE KEY                           
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE DISPLAYS RECORD                                          
*                                                                               
DISPLAY  NTR1                                                                   
*                                                                               
         TWAXC SMUNAMEH                                                         
         XC    SMULC1N,SMULC1N     CLEAR LICENSER NAME FIELDS AS WELL           
         OI    SMULC1NH+6,X'80'                                                 
         XC    SMULC2N,SMULC2N                                                  
         OI    SMULC2NH+6,X'80'                                                 
         XC    SMULC3N,SMULC3N                                                  
         OI    SMULC3NH+6,X'80'                                                 
         XC    SMULC4N,SMULC4N                                                  
         OI    SMULC4NH+6,X'80'                                                 
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
*                                                                               
* COMPOSITION ELEMENT                                                           
*                                                                               
         GOTO1 CHAROUT,DMCB,TANAELQ,(2,SMUNAMEH)                                
*                                                                               
* PUBLISHER 1 ELEMENT                                                           
*                                                                               
         L     R3,AIO                                                           
         USING TAMUD,R3                                                         
         MVI   ELCODE,TAMUELQ      LOOK FOR PUBLISHER 1 ELEMENT                 
         GOTO1 GETL,DMCB,(1,=AL1(TAMUTP1))                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,TGELEM                                                        
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SMUPUB1(0),TAMUNAME PUBLISHER 1 NAME                             
         GOTO1 LICVAL,DMCB,(X'80',TAMULIC)                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SMULC1,TGLCCDE      LICENSER                                     
         MVC   SMULC1N,TGLCNAME                                                 
*                                                                               
* PUBLISHER 2 ELEMENT                                                           
*                                                                               
         L     R3,AIO              LOOK FOR PUBLISHER 2 ELEMENT                 
         GOTO1 GETL,DMCB,(1,=AL1(TAMUTP2))                                      
         BNE   DR10                                                             
         L     R3,TGELEM                                                        
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = L'NAME                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SMUPUB2(0),TAMUNAME PUBLISHER 2 NAME                             
         GOTO1 LICVAL,DMCB,(X'80',TAMULIC)                                      
         BNE   DR10                                                             
         MVC   SMULC2,TGLCCDE      LICENSER                                     
         MVC   SMULC2N,TGLCNAME                                                 
*                                                                               
* COMPOSER ELEMENT                                                              
*                                                                               
DR10     L     R3,AIO              LOOK FOR COMPOSER ELEMENT                    
         GOTO1 GETL,DMCB,(1,=AL1(TAMUTCOM))                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,TGELEM                                                        
         GOTO1 LICVAL,DMCB,(X'80',TAMULIC)                                      
         BNE   *+16                                                             
         MVC   SMULC3,TGLCCDE      LICENSER                                     
         MVC   SMULC3N,TGLCNAME                                                 
*                                                                               
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = LENGTH OF ALL NAMES                     
         SR    R0,R0                                                            
         D     R0,=A(L'SMUCOMP)                                                 
         LA    R2,SMUCOMPH                                                      
         LA    R3,TAMUNAME                                                      
         LTR   R1,R1               R1 = NUMBER OF FULL LINES                    
         BZ    DR40                                                             
*                                                                               
DR20     MVC   8(L'SMUCOMP,R2),0(R3)  COMPOSER NAME                             
DR30     LA    RF,SMUCOMLH         A(LAST NAME FIELD)                           
         CR    R2,RF                                                            
         BH    DR45                NO MORE COMPOSER LINES                       
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT TWA FIELD                       
         IC    RF,0(R2)                                                         
         LA    RE,0(RF,R2)                                                      
         SH    RE,=H'8'            R1 = A(FIELD ID NUMBER)                      
         CLI   0(RE),87            COMPOSER NAME FIELD?                         
         BNE   DR30                NO -- IT'S THE LICENSER FIELD                
         LA    R3,L'SMUCOMP(R3)                                                 
         BCT   R1,DR20                                                          
*                                                                               
DR40     LTR   R1,R0               R1 = NUMBER OF CHARACTERS REMAINING          
         BZ    DR45                NOTHING LEFT                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)                                                    
*                                                                               
* AUTHOR ELEMENT                                                                
*                                                                               
DR45     L     R3,AIO              LOOK FOR AUTHOR ELEMENT                      
         GOTO1 GETL,DMCB,(1,=AL1(TAMUTAUT))                                     
         BNE   DR80                                                             
         L     R3,TGELEM                                                        
         GOTO1 LICVAL,DMCB,(X'80',TAMULIC)                                      
         BNE   *+16                                                             
         MVC   SMULC4,TGLCCDE      LICENSER                                     
         MVC   SMULC4N,TGLCNAME                                                 
*                                                                               
         ZIC   R1,TAMULEN          ELEMENT LENGTH                               
         SH    R1,=Y(TAMULNQ)      R1 = LENGTH OF ALL NAMES                     
         SR    R0,R0                                                            
         D     R0,=A(L'SMUAUTH)                                                 
         LA    R2,SMUAUTHH                                                      
         LA    R3,TAMUNAME                                                      
         LTR   R1,R1               R1 = NUMBER OF FULL LINES                    
         BZ    DR70                                                             
*                                                                               
DR50     MVC   8(L'SMUAUTH,R2),0(R3)  AUTHOR NAME                               
DR60     LA    RF,SMUAUTLH         A(LAST NAME FIELD)                           
         CR    R2,RF                                                            
         BH    DR80                NO MORE AUTHOR LINES                         
         ZIC   RF,0(R2)                                                         
         AR    R2,RF               BUMP TO NEXT TWA FIELD                       
         IC    RF,0(R2)                                                         
         LA    RE,0(RF,R2)                                                      
         SH    RE,=H'8'            R1 = A(FIELD ID NUMBER)                      
         CLI   0(RE),89            AUTHOR NAME FIELD?                           
         BNE   DR60                NO -- IT'S THE LICENSER FIELD                
         LA    R3,L'SMUAUTH(R3)                                                 
         BCT   R1,DR50                                                          
*                                                                               
DR70     LTR   R1,R0               R1 = NUMBER OF CHARACTERS REMAINING          
         BZ    DR80                NOTHING LEFT                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),0(R3)                                                    
*                                                                               
* CLIENT ELEMENT                                                                
*                                                                               
DR80     GOTO1 CHAROUT,DMCB,TAFNELQ,SMUCLIH,TAFNTCLI  CLIENT NAME               
*                                                                               
* PRODUCT ELEMENT                                                               
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SMUPRDH,TAFNTPRD  PRODUCT NAME              
*                                                                               
* ACTIVITY ELEMENT                                                              
*                                                                               
         GOTO1 ACTVOUT,DMCB,SMULCHGH                                            
*                                                                               
         MVC   KEY,SVKEY           RESTORE OFFICE KEY                           
*                                                                               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
TRAPERR  GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
PFTBL    DS    0C                  PF KEYS TABLE                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3' ',CL8'PMU',CL8'LIST'                                        
PF13     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3' ',CL8'COM',CL8'LIST'                                        
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGMUS-1),AL2(TGMUS-TGD)                           
PF14X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRA8D                                                       
         SPACE 3                                                                
SVKEY    DS    CL38                SAVED KEY WITH ADDRESS                       
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011TAGENA8   05/01/02'                                      
         END                                                                    
