*          DATA SET TAGENEB    AT LEVEL 004 AS OF 06/21/07                      
*PHASE T702EBA,*                                                                
         TITLE 'T702EB - CHECK SLIST'                                           
T702EB   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702EB                                                         
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
         MVI   THISLSEL,STOPL2M                                                 
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
         LA    R2,CKSSELH        R2=A(FIRST SELECT FIELD)                       
         LA    R3,CKSLSTH        R3=A(LAST SELECT FIELD)                        
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
         SPACE                                                                  
         CR    R2,R3             IF NOT AT END OF SCREEN                        
         BL    SCHK10            CHECK NEXT SELECT FIELD                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELDS AND SETS UP KEY                     
         SPACE                                                                  
INIT     NTR1                                                                   
         TM    CKSSDTH+4,X'20'     IF BOTH READ BY STOP DATE AND                
         BNO   INIT10              READ BY CHECK DATE ALREADY                   
         TM    CKSCDTH+4,X'20'     VALIDATED, SKIP AHEAD                        
         BO    INIT50                                                           
         SPACE                                                                  
INIT10   CLI   CKSSDTH+5,0         ONLY ONE FIELD CAN CONTAIN                   
         BE    INIT20              INPUT                                        
         LA    R2,CKSCDTH                                                       
         CLI   5(R2),0                                                          
         BNE   FLDINV                                                           
         B     INIT30                                                           
         SPACE                                                                  
INIT20   MVI   READBY,TLCKCSSQ     IF NEITHER FIELD IS INPUT                    
         CLI   CKSCDTH+5,0         DEFAULT TO STOP DATE                         
         BNE   INIT30                                                           
         MVI   CKSSDT,C'X'                                                      
         OI    CKSSDTH+6,X'80'                                                  
         MVI   CKSSDTH+5,1                                                      
         B     INIT40                                                           
         SPACE                                                                  
INIT30   MVI   READBY,TLCKCSSQ                                                  
         CLI   CKSSDTH+5,0                                                      
         BNE   INIT40                                                           
         MVI   READBY,TLCKCSCQ                                                  
         SPACE                                                                  
INIT40   OI    CKSSDTH+4,X'20'                                                  
         OI    CKSCDTH+4,X'20'                                                  
         XC    KEY,KEY                                                          
         MVI   LISTNUM,0                                                        
         SPACE                                                                  
INIT50   TM    CKSCCKH+4,X'20'     IF READ ONLY CANCELLED STOPS                 
         BO    INIT100             ALREADY VALIDATED, SKIP AHEAD                
         SPACE                                                                  
         MVI   READONLY,C'N'       DEFAULT TO READ ONLY STOPPED                 
         CLI   CKSCCKH+5,0         CHECKS                                       
         BE    *+8                 SET READ ONLY AS CANCELLED                   
         MVI   READONLY,C'Y'       STOPS IF REQUESTED                           
         SPACE                                                                  
         OI    CKSCCKH+4,X'20'                                                  
         XC    KEY,KEY                                                          
         MVI   LISTNUM,0                                                        
         SPACE                                                                  
INIT100  TM    CKSWTSH+4,X'20'     IF BOTH READ W/STOP PLACED AND               
         BZ    INIT110             W/O STOP PLACED VALIDATED,                   
         TM    CKSWOSH+4,X'20'     VALIDATED, SKIP AHEAD                        
         BO    INITX                                                            
         SPACE                                                                  
INIT110  MVI   READSTPL,0                                                       
         CLI   CKSWTSH+5,0         ONLY ONE FIELD CAN CONTAIN                   
         BE    INIT120             INPUT                                        
         LA    R2,CKSWOSH                                                       
         CLI   5(R2),0                                                          
         BNE   FLDINV              IF W/STOP PLACED CHECKED                     
         MVI   READSTPL,C'W'       INDICATE IN READSTPL                         
         B     INIT130                                                          
         SPACE                                                                  
INIT120  CLI   CKSWOSH+5,0                                                      
         BE    INIT130             IF W/O STOP PLACED CHECKED                   
         MVI   READSTPL,C'O'       INDICATE IN READSTPL                         
         SPACE                                                                  
INIT130  OI    CKSWTSH+4,X'20'                                                  
         OI    CKSWOSH+4,X'20'                                                  
         XC    KEY,KEY                                                          
         MVI   LISTNUM,0                                                        
INITX    MVI   MODE,LISTRECS                                                    
         B     XIT                                                              
         EJECT                                                                  
LSTREC   NTR1                                                                   
         MVI   NLISTS,17           GET CONTROL BACK AT END OF PAGE              
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         LA    R2,LISTAR                                                        
         TWAXC CKSSELH,CKSLSTH,PROT=Y                                           
         SPACE                                                                  
         MVC   SYSDIR,=CL8'CHKDIR'                                              
         MVC   SYSFIL,=CL8'CHKFIL'                                              
         SPACE                                                                  
         LA    RE,CKSDETH                                                       
         ST    RE,ATHISLST                                                      
         SPACE                                                                  
         OC    KEY,KEY             IF FIRST TIME IN                             
         BNZ   LSTR10                                                           
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLCKPD,R3                                                        
         MVC   TLCKPCD,READBY      BUILD KEY                                    
         MVC   TLCKCSSX,READONLY                                                
         SPACE                                                                  
LSTR10   GOTO1 HIGH                                                             
         B     LSTR30                                                           
LSTR20   GOTO1 SEQ                 GET NEXT RECORD                              
         SPACE                                                                  
LSTR30   CLC   KEY(2),KEYSAVE      TEST STILL HAVE GOOD KEY                     
         BNE   LSTR40                                                           
         GOTO1 CATCHIOS                                                         
         SPACE                                                                  
         GOTO1 GETREC              GET RECORD                                   
         SPACE                                                                  
         CLI   READSTPL,0          IF FILTERING ON CHECK HAVING                 
         BE    LSTR35              OR NOT HAVING STOP PLACED                    
         USING TAKPD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAKPELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'               IF STOP HAS BEEN PLACED                      
         OC    TAKPSPDT,TAKPSPDT   AND WANT CHECKS WITH STOP                    
         BZ    *+16                PLACED, CONTINUE                             
         CLI   READSTPL,C'W'                                                    
         BE    LSTR35              IF STOP HAS NOT BEEN PLACED                  
         B     LSTR20              AND WANT CHECKS WITHOUT                      
         CLI   READSTPL,C'O'       STOP PLACED, CONTINUE                        
         BNE   LSTR20                                                           
         DROP  R4                  OTHERWISE, GET NEXT CHECK                    
         SPACE                                                                  
LSTR35   BAS   RE,DISPLAY                                                       
         B     LSTR20                                                           
         SPACE                                                                  
LSTR40   MVI   NLISTS,17           RESET AT END OF LIST                         
         B     ENDLIST                                                          
         EJECT                                                                  
*                                                                               
*              PROCESS RECORD IN AIO                                            
         SPACE                                                                  
         USING LISTD,R2                                                         
DISPLAY  NTR1                                                                   
         USING TACDD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TACDELQ      GET CHECK DETAILS ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   DISP10              DISPLAY CHECK NUMBER                         
         MVC   LISCHK,TACDCHK                                                   
         DROP  R4                                                               
         SPACE                                                                  
         USING TAKPD,R4                                                         
DISP10   L     R4,AIO                                                           
         MVI   ELCODE,TAKPELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         GOTO1 DATCON,DMCB,(1,TAKPSRDT),(8,LISSDTE)                             
         SPACE                                                                  
         OC    TAKPSPDT,TAKPSPDT                                                
         BZ    DISP15                                                           
         GOTO1 DATCON,DMCB,(1,TAKPSPDT),(8,LISSPDT)                             
         SPACE                                                                  
DISP15   OC    TAKPRECD,TAKPRECD                                                
         BZ    DISP20                                                           
         GOTO1 DATCON,DMCB,(1,TAKPRECD),(8,LISREDT)                             
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
         MVC   DMDSKADD,TLDRDA     PASS DISK ADDRESS TO LISTMON                 
         CLI   LISTNUM,15                                                       
         BE    ENDPAGE             GET OUT IF ALREADY FILLED PAGE               
         GOTO1 LISTMON             CALL LISTMON                                 
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
         NI    CKSSDTH+4,X'FF'-X'20'                                            
         MVI   LISTNUM,0                                                        
         B     INFOMS                                                           
         SPACE                                                                  
INFOMS   LA    R2,CKSSELH                                                       
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
         DC    CL3'S',CL8'CHECK   ',CL8'STOP   '                                
PF23     DC    AL1(KEYTYCUR,L'LISCHK-1),AL2(LISCHK-LISTD)                       
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
         DS    CL6                                                              
LISSDTE  DS    CL8                                                              
         DS    CL10                                                             
LISCDTE  DS    CL8                                                              
         DS    CL7                                                              
LISSPDT  DS    CL8                                                              
         DS    CL6                                                              
LISREDT  DS    CL8                                                              
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCREBD                                                       
         SPACE                                                                  
COUNTER  DS    PL4                 COUNTER OF NUM OF OUTPUT LINES               
READBY   DS    X                                                                
READONLY DS    X                                                                
READSTPL DS    X                                                                
LASTKEY  DS    XL32                                                             
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
**PAN#1  DC    CL21'004TAGENEB   06/21/07'                                      
         END                                                                    
