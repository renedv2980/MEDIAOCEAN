*          DATA SET TAGENEC    AT LEVEL 003 AS OF 03/12/03                      
*PHASE T702ECA,*                                                                
         TITLE 'T702EB - CHECK PLIST'                                           
T702EC   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702EC                                                         
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
         SPACE                                                                  
         MVI   ACTNUM,ACTLIST                                                   
         MVC   KEY,LASTKEY                                                      
         SPACE                                                                  
         BAS   RE,SELCHK                                                        
         SPACE                                                                  
         MVI   THISLSEL,PULLL2M                                                 
         GOTO1 INITIAL,DMCB,PFTAB                                               
         MVI   THISLSEL,0                                                       
         SPACE                                                                  
         CLI   MODE,VALKEY         FIRST TIME IN, VALIDATE KEY                  
         BNE   XIT                                                              
         BAS   RE,INIT                                                          
         SPACE 3                                                                
         CLI   MODE,LISTRECS       IF MODE IS LISTRECS                          
         BNE   XIT                                                              
         BAS   RE,LSTREC                                                        
         EJECT                                                                  
*              ROUTINE MAKES SURE NOTHING BESIDES S IS INPUTTED                 
*              IN SELECT FIELD                                                  
         SPACE                                                                  
SELCHK   NTR1                                                                   
         LA    R2,CKPSELH        R2=A(FIRST SELECT FIELD)                       
         LA    R3,CKPLSTH        R3=A(LAST SELECT FIELD)                        
         SPACE                                                                  
SCHK10   CLI   5(R2),1           INPUT MUST BE => 1                             
         BH    FLDINV                                                           
         OC    8(3,R2),SPACES                                                   
         CLI   8(R2),C'S'        INPUT MUST BE AN S                             
         BE    XIT                                                              
         CLI   8(R2),C' '        OR A SPACE                                     
         BNE   FLDINV                                                           
         SPACE                                                                  
SCHK20   ZIC   RE,0(R2)          BUMP PAST THIS SELECT FIELD                    
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP PAST LINE FIELD                           
         AR    R2,RE                                                            
         ZIC   RE,0(R2)          BUMP PAST ADDRESS FIELD                        
         AR    R2,RE                                                            
         SPACE                                                                  
         CR    R2,R3             IF NOT AT END OF SCREEN                        
         BL    SCHK10            CHECK NEXT SELECT FIELD                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND SETS UP KEY                     
         SPACE                                                                  
INIT     NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',CKPPDTH),(X'80',CKPSSNH)                      
         BE    INITX                                                            
         SPACE                                                                  
         CLI   CKPPDTH+5,0         ONLY ONE OF THE LIST BY FIELDS               
         BE    INIT20              CAN CONTAIN INPUT                            
         LA    R2,CKPCDTH                                                       
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         B     INIT30                                                           
         SPACE                                                                  
INIT20   MVI   READBY,TLCKCPSQ     IF NEITHER FIELD IS INPUT                    
         CLI   CKPCDTH+5,0         DEFAULT TO STOP DATE                         
         BNE   INIT30                                                           
         MVI   CKPPDT,C'X'                                                      
         OI    CKPPDTH+6,X'80'                                                  
         MVI   CKPPDTH+5,1                                                      
         B     INIT40                                                           
         SPACE                                                                  
INIT30   MVI   READBY,TLCKCPSQ                                                  
         CLI   CKPPDTH+5,0                                                      
         BNE   INIT40                                                           
         MVI   READBY,TLCKCPCQ                                                  
         SPACE                                                                  
INIT40   MVI   READONLY,0          ONLY ONE OF THE READ ONLY                    
         CLI   CKPALLH+5,0         FIELDS CAN CONTAIN INPUT                     
         BE    *+8                                                              
         MVI   READONLY,C'A'                                                    
         SPACE                                                                  
         LA    R2,CKPVOIDH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   READONLY,0                                                       
         BNE   FLDINV                                                           
         OI    READONLY,TAKLSVOD                                                
         SPACE                                                                  
         LA    R2,CKPHOLDH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   READONLY,0                                                       
         BNE   FLDINV                                                           
         OI    READONLY,TAKLSHLD                                                
         SPACE                                                                  
         LA    R2,CKPWIREH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   READONLY,0                                                       
         BNE   FLDINV                                                           
         OI    READONLY,TAKLSWIR                                                
         SPACE                                                                  
         LA    R2,CKPGRTH                                                       
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   READONLY,0                                                       
         BNE   FLDINV                                                           
         OI    READONLY,TALKSG50                                                
         SPACE                                                                  
         LA    R2,CKPOVNTH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   READONLY,0                                                       
         BNE   FLDINV                                                           
         OI    READONLY,TAKLSOVN                                                
         SPACE                                                                  
         LA    R2,CKPDRFTH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   READONLY,0                                                       
         BNE   FLDINV                                                           
         OI    READONLY,TAKLSDRF                                                
         SPACE                                                                  
         LA    R2,CKPOTHRH                                                      
         CLI   5(R2),0                                                          
         BE    *+16                                                             
         CLI   READONLY,0                                                       
         BNE   FLDINV                                                           
         OI    READONLY,TAKLSOTH                                                
         SPACE                                                                  
         CLI   READONLY,0          IF NO READ ONLY INPUT DEFAULT                
         BNE   INIT50              TO ALL                                       
         MVI   CKPALL,C'X'                                                      
         OI    CKPALLH+6,X'80'                                                  
         MVI   CKPALLH+5,1                                                      
         MVI   READONLY,C'A'                                                    
         SPACE                                                                  
INIT50   MVI   ONLYWHEN,0                                                       
         CLI   CKPLOOVH+5,0                                                     
         BE    *+8                                                              
         MVI   ONLYWHEN,TAKLSCOV                                                
         LA    R2,CKPLOURH                                                      
         CLI   5(R2),0                                                          
         BE    INIT60                                                           
         CLI   ONLYWHEN,0                                                       
         BNE   FLDINV                                                           
         MVI   ONLYWHEN,TAKLSCUR                                                
         SPACE                                                                  
INIT60   XC    ONLYSSN,ONLYSSN     DEFAULT W4 FILTER TO NOTHING                 
         LA    R2,CKPSSNH                                                       
         CLI   5(R2),0             IF INPUTTED, W4 MUST EXIST                   
         BE    INIT70                                                           
         GOTO1 RECVAL,DMCB,TLW4CDQ,(R2)                                         
         MVC   ONLYSSN,TGSSN                                                    
         SPACE                                                                  
INIT70   GOTO1 FLDVAL,DMCB,(X'20',CKPPDTH),(X'80',CKPSSNH)                      
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   LISTNUM,0                                                        
         SPACE                                                                  
INITX    MVI   MODE,LISTRECS                                                    
         B     XIT                                                              
         EJECT                                                                  
LSTREC   NTR1                                                                   
         MVI   NLISTS,16           GET CONTROL BACK AT END OF PAGE              
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         TWAXC CKPSELH,CKPLSTH,PROT=Y                                           
         SPACE                                                                  
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         SPACE                                                                  
         LA    RE,CKPDETH                                                       
         ST    RE,ATHISLST                                                      
         SPACE                                                                  
         OC    KEY,KEY             IF FIRST TIME IN                             
         BNZ   LSTR10                                                           
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLCKPD,R3                                                        
         MVC   TLCKPCD,READBY      BUILD KEY                                    
         SPACE                                                                  
LSTR10   GOTO1 HIGH                                                             
         B     LSTR30                                                           
LSTR20   GOTO1 SEQ                 GET NEXT RECORD                              
         SPACE                                                                  
LSTR30   CLC   KEY(1),KEYSAVE      TEST STILL HAVE GOOD KEY                     
         BNE   LSTR40                                                           
         CLI   READONLY,C'A'                                                    
         BE    LSTR35                                                           
         CLC   TLCKCPSR,READONLY                                                
         BNE   LSTR20                                                           
         SPACE                                                                  
LSTR35   GOTO1 GETREC              GET RECORD                                   
         SPACE                                                                  
         CLI   ONLYWHEN,0                                                       
         BE    LSTR36                                                           
         USING TAKLD,R4                                                         
         L     R4,AIO              IF URGENT/OVERNIGHT FILTER                   
         MVI   ELCODE,TAKLELQ      INPUTTED MAKE SURE RECORD                    
         BAS   RE,GETEL            MATCHES IT                                   
         BNE   LSTR36                                                           
         MVC   BYTE,ONLYWHEN                                                    
         NC    BYTE,TAKLSTA2                                                    
         BZ    LSTR20                                                           
         DROP  R4                                                               
         SPACE                                                                  
LSTR36   OC    ONLYSSN,ONLYSSN                                                  
         BZ    LSTR39                                                           
         USING TLCKD,R4                                                         
         L     R4,AIO              IF SOCIAL SECURITY FILTER                    
         CLC   TLCKSSN,ONLYSSN     INPUTTED MAKE SURE RECORD                    
         BNE   LSTR20              MATCHES IT                                   
         DROP  R4                                                               
         SPACE                                                                  
LSTR39   BAS   RE,DISPLAY                                                       
         B     LSTR20                                                           
         SPACE                                                                  
LSTR40   MVI   NLISTS,16           RESET AT END OF LIST                         
         B     ENDLIST                                                          
         EJECT                                                                  
*                                                                               
*              PROCESS RECORD IN AIO                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
DISPLAY  NTR1                                                                   
         USING TLCKD,R4                                                         
         L     R4,AIO                                                           
         MVC   LISSSN,TLCKSSN      DISPLAY SOCIAL SECURITY NUMBER               
         DROP  R4                                                               
         SPACE                                                                  
         USING TACDD,R4                                                         
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   DISP10              DISPLAY CHECK NUMBER                         
         MVC   LISCHK(3),=C'N/A'                                                
         OC    TACDCHK,TACDCHK                                                  
         BZ    DISP10                                                           
         MVC   LISCHK,TACDCHK                                                   
         DROP  R4                                                               
         SPACE                                                                  
         USING TAKLD,R4                                                         
DISP10   L     R4,AIO                                                           
         MVI   ELCODE,TAKLELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         GOTO1 DATCON,DMCB,(1,TAKLRQDT),(8,LISSDTE)                             
         MVC   LISREAS,=CL18'GUARANTEE OVER 50K'                                
         TM    TAKLSTAT,TALKSG50                                                
         BO    DISP20                                                           
         MVC   LISREAS,=CL18'VOID'                                              
         TM    TAKLSTAT,TAKLSVOD                                                
         BO    DISP20                                                           
         MVC   LISREAS,=CL18'HOLD'                                              
         TM    TAKLSTAT,TAKLSHLD                                                
         BO    DISP20                                                           
         MVC   LISREAS,=CL18'WIRE'                                              
         TM    TAKLSTAT,TAKLSWIR                                                
         BO    DISP20                                                           
         MVC   LISREAS,=CL18'DRAFT'                                             
         TM    TAKLSTAT,TAKLSDRF                                                
         BO    DISP20                                                           
         MVC   LISREAS,=CL18'OVERNIGHT'                                         
         TM    TAKLSTAT,TAKLSOVN                                                
         BO    DISP20                                                           
         MVC   LISREAS,=CL18'OTHER'                                             
         TM    TAKLSTAT,TAKLSOTH                                                
         BO    DISP20                                                           
         DC    H'00'                                                            
         DROP  R4                                                               
         SPACE                                                                  
         USING TACDD,R4                                                         
DISP20   L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP30                                                           
         GOTO1 DATCON,DMCB,(1,TACDDTE),(8,LISCDTE)                              
         DROP  R4                                                               
         SPACE                                                                  
         USING TLDRD,R3                                                         
DISP30   LA    R3,KEY                                                           
         GOTO1 HEXOUT,DMCB,TLDRDA,DSKADD,L'TLDRDA                               
         MVC   DMDSKADD,TLDRDA     PASS DISK ADDRESS TO LISTMON                 
         CLI   LISTNUM,15                                                       
         BE    ENDPAGE             GET OUT IF ALREADY FILLED PAGE               
         GOTO1 LISTMON             CALL LISTMON                                 
         L     R2,ATHISLST                                                      
         MVC   8(L'DSKADD,R2),DSKADD                                            
         MVI   5(R2),L'DSKADD                                                   
         OI    6(R2),X'80'                                                      
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ZIC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         ST    R2,ATHISLST                                                      
DISPX    MVC   LISTAR,SPACES                                                    
         B     XIT                                                              
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
FLDMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE                                                                  
ENDPAGE  MVI   MYMSGNO1,246        GIVE END OF PAGE MESSAGE                     
         MVC   LASTKEY,KEY                                                      
         MVI   LISTNUM,0                                                        
         B     INFOMS                                                           
         SPACE                                                                  
ENDLIST  MVI   MYMSGNO1,109        GIVE END OF LIST MESSAGE                     
         XC    LASTKEY,LASTKEY                                                  
         NI    CKPPDTH+4,X'FF'-X'20'                                            
         MVI   LISTNUM,0                                                        
         B     INFOMS                                                           
         SPACE                                                                  
INFOMS   LA    R2,CKPSELH                                                       
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
         SPACE                                                                  
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
PFTAB    DS    0C                                                               
         DC    AL1(PF23X-*,23,PFTINT+PFTCPROG,(PF23X-PF23)/KEYLNQ,0)            
         DC    CL3'S',CL8'CHECK   ',CL8'PULL   '                                
PF23     DC    AL1(KEYTYCUR,L'LISCDA-1),AL2(LISCDA-LISTD)                       
PF23X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR SCREEN                                                 
         SPACE 1                                                                
LISTD    DSECT                                                                  
LISCHK   DS    CL8                                                              
         DS    CL2                                                              
LISSDTE  DS    CL8                                                              
         DS    CL3                                                              
LISSSN   DS    CL9                                                              
         DS    CL2                                                              
LISCDTE  DS    CL8                                                              
         DS    CL5                                                              
LISREAS  DS    CL18                                                             
         DS    CL2                                                              
         DS    XL8                                                              
LISCDA   DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRECD                                                       
         SPACE                                                                  
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
READBY   DS    X                                                                
READONLY DS    X                                                                
ONLYSSN  DS    XL9                                                              
ONLYWHEN DS    C                                                                
LASTKEY  DS    XL32                                                             
DSKADD   DS    CL8                                                              
         EJECT                                                                  
* DDGENTWA                                                                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003TAGENEC   03/12/03'                                      
         END                                                                    
