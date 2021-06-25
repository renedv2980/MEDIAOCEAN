*          DATA SET TAGEND7    AT LEVEL 041 AS OF 07/20/12                      
*PHASE T702D7C,*                                                                
         TITLE 'T702D7 - ALIAS MAINTENANCE'                                     
T702D7   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D7                                                         
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
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY FOR SELECT                       
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     MAINX                                                            
*                                                                               
         CLI   MODE,DISPREC        IF DISPLAY RECORD                            
         BE    MAIN15                                                           
         CLI   MODE,XRECADD        OR RECORD ADDED                              
         BNE   *+12                                                             
         BAS   RE,CHKSTAT          MAKE SURE STATUS IN KEY OKAY                 
         B     MAIN15                                                           
         CLI   MODE,XRECDEL        OR DELETED                                   
         BE    MAIN15                                                           
         CLI   MODE,XRECREST       OR RESTORED                                  
         BNE   MAIN20                                                           
MAIN15   BAS   RE,DISPLAY          (RE-)DISPLAY THE RECORD                      
         B     MAINX                                                            
         SPACE 2                                                                
MAIN20   CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   *+12                                                             
         BAS   RE,BLDREC           BUILD IT                                     
         B     MAINX                                                            
         SPACE 2                                                                
         CLI   MODE,RECDEL         DELETE RECORD                                
         BNE   MAINX                                                            
         BAS   RE,CHKDEL           CHECK OKAY TO DELETE                         
*                                                                               
MAINX    B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE THE KEY                                                 
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SAKAGYH          R2=A(AGENCY FIELD)                           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),SAKAGYNH                        
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
         LA    R2,SAKNCLIH         R2=A(NETWORK/SPOT CLIENT FIELD)              
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK10                                                             
         OC    TGNCLI,TGNCLI       AND GLOBAL CLIENT                            
         BZ    VK10                                                             
         MVC   SAKNCLI,TGNCLI      USE IT                                       
         OI    6(R2),X'80'                                                      
         B     VK15                                                             
VK10     GOTO1 ANY                                                              
         MVC   TGNCLI,WORK                                                      
*                                                                               
VK15     LA    R2,SAKNPRDH         R2=A(NETWORK/SPOT PRODUCT FIELD)             
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK20                                                             
         OC    TGNPRD,TGNPRD       AND GLOBAL PRODUCT                           
         BZ    VK20                                                             
         MVC   SAKNPRD,TGNPRD      USE IT                                       
         OI    6(R2),X'80'                                                      
         B     VK25                                                             
VK20     GOTO1 ANY                                                              
         MVC   TGNPRD,WORK                                                      
*                                                                               
VK25     LA    R2,SAKMEDH          R2=A(MEDIA FIELD)                            
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK28                                                             
         OC    TGMENAME,TGMENAME   AND GLOBAL MEDIA                             
         BZ    VK28                                                             
         MVC   SAKMED,TGMENAME     USE IT                                       
         OI    6(R2),X'80'                                                      
         B     VK30                                                             
VK28     GOTO1 ANY                 ELSE, REQUIRE INPUT                          
         GOTO1 MEDVAL,DMCB,WORK                                                 
         BNE   FLDINV                                                           
*                                                                               
VK30     LA    R2,SAKNIDH          R2=A(NETWORK/SPOT COMML ID FIELD)            
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VK40                                                             
         OC    TGADID,TGADID       AND WE HAVE GLOBAL AD-ID                     
         BZ    *+14                                                             
         MVC   SAKNID,TGADID       USE IT                                       
         B     VK35                                                             
         OC    TGNID,TGNID         ELSE IF WE HAVE GLOBAL NETWORK ID            
         BZ    VK40                                                             
         MVC   SAKNID(L'TGNID),TGNID    USE IT                                  
         MVC   TGADID(L'TGNID),TGNID                                            
VK35     OI    6(R2),X'80'                                                      
         B     VK50                                                             
VK40     GOTO1 ANY                                                              
         MVC   TGADID,WORK                                                      
*                                                                               
VK50     CLI   ACTNUM,ACTREST                                                   
         BNE   *+8                                                              
         OI    DMINBTS,X'08'                                                    
         GOTO1 RECVAL,DMCB,TLAKCDQ,0                                            
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(TLAKCOM-TLAKKEY),KEYSAVE                                     
         BNE   VK60                                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    ERRECXIT            KEY DUPLICATE                                
         B     XIT                                                              
*                                                                               
VK60     CLI   ACTNUM,ACTADD                                                    
         BNE   ERRNTFND            RECORD NOT FOUND                             
         LA    R2,SAKTCIDH         R2=A(TALENT CID FIELD)                       
         NI    4(R2),X'DF'         ALWAYS FORCE RE-VALIDATION                   
         GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLCOICDQ,(R2)                                        
         LA    R4,KEY                                                           
         USING TLCOPD,R4                                                        
         MVC   TGCOM,TLCOICOM                                                   
*                                                                               
         GOTO1 RECVAL,DMCB,TLAKCDQ,(X'40',0)                                    
         LA    R4,KEY              BUILD ALIAS KEY                              
         USING TLAKD,R4                                                         
         MVC   TLAKCOM,TGCOM       SET INTERNAL COMMERCIAL # IN KEY             
*                                                                               
         LA    R2,SAKPLFTH         R2=A(PAY LIFT FIELD)                         
         GOTO1 ANY                                                              
         CLI   WORK,C'N'           Y/N INPUT REQUIRED                           
         BE    VKX                                                              
         CLI   WORK,C'Y'           IF ALIAS IS A LIFT ID                        
         BNE   FLDINV                                                           
         OI    KEY+TLDRSTAT-TLDRD,TLAKSLFT    SET IN KEY                        
VKX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              DISPLAY THE KEY                                                  
         SPACE 1                                                                
DKEY     NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         L     R6,AIO              R6=A(ALIAS RECORD)                           
         USING TLAKD,R6                                                         
*                                                                               
         MVC   SAKAGY,TGAGY        AGENCY                                       
         OI    SAKAGYH+6,X'80'                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SAKAGYH),SAKAGYNH                     
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   SAKNCLI,TLAKNCLI    NETWORK/SPOT CLIENT                          
         OI    SAKNCLIH+6,X'80'                                                 
         MVC   SAKNPRD,TLAKNPRD    NETWORK/SPOT PRODUCT                         
         OI    SAKNPRDH+6,X'80'                                                 
         MVC   SAKMED,TLAKMED      NETWORK/SPOT MEDIA                           
         OI    SAKMEDH+6,X'80'                                                  
         MVC   SAKNID,TLAKADID     NETWORK/SPOT AD-ID                           
         OI    SAKNIDH+6,X'80'                                                  
         MVC   KEY,SVKEY                                                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         L     R6,AIO              R6=A(ALIAS RECORD)                           
         USING TLAKD,R6                                                         
*                                                                               
         NI    TLAKSTAT,X'FF'-TLAKSLFT                                          
         CLI   SAKPLFT,C'Y'        IF PAYING LIFT                               
         BNE   *+8                                                              
         OI    TLAKSTAT,TLAKSLFT   INDICATE SO                                  
*                                                                               
         GOTO1 ACTVIN,DMCB,0       LAST CHANGED                                 
*                                                                               
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO MAKE SURE STATUS IN KEY MATCHES                       
*              STATUS IN RECORD                                                 
         SPACE 1                                                                
CHKSTAT  NTR1                                                                   
         L     R6,AIO              R6=A(ALIAS RECORD)                           
         USING TLAKD,R6                                                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLAKKEY),TLAKKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLAKKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+TLDRSTAT-TLDRD(1),TLAKSTAT                                   
         GOTO1 WRITE                                                            
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK NO ACTIVELY MATCHED HOLD RECORDS                
*              WITH ALIAS CONNECTION                                            
         SPACE 1                                                                
CHKDEL   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE THE KEY                                 
*                                  READ HIGH FOR HOLD RECORDS                   
         XC    TGUSCDE,TGUSCDE     NOT INCLUDING USE                            
         XC    TGDATE,TGDATE           DATE ADDED                               
         XC    TGUSER,TGUSER           WORKER FILE ID                           
         MVI   TGTYPE,0                OR CHANGE CODE                           
         GOTO1 RECVAL,DMCB,TLNXCDQ,0                                            
         LA    R4,KEY                                                           
         USING TLNXD,R4                                                         
         B     CHKDEL10                                                         
*                                                                               
CHKDEL5  GOTO1 SEQ                                                              
CHKDEL10 CLC   KEY(TLNXUSE-TLNXKEY),KEYSAVE                                     
         BNE   CHKDELX                                                          
         CLI   TLNXSEQ,0                            SKIP NON-BASE RECS          
         BNE   CHKDEL5                                                          
         CLI   KEY+TLDRSTAT-TLDRD,0                 SKIP UNMATCHED RECS         
         BE    CHKDEL5                                                          
         TM    KEY+TLDRSTAT-TLDRD,TLNXSDEL+TLNXSUSD SKIP DELETED/USED           
         BNZ   CHKDEL5                                                          
         BAS   RE,DISPLAY                                                       
         B     NODELETE            CAN'T DELETE - RECORD FOUND                  
*                                                                               
CHKDELX  MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         L     R6,AIO              R6=A(ALIAS RECORD)                           
         USING TLAKD,R6                                                         
*                                                                               
         MVC   AIO,AIO2                                                         
         XC    SAKTCID,SAKTCID                                                  
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TLAKCOM)                             
         BNE   DISP10                                                           
         L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         USING TACOD,R4                                                         
         MVC   SAKTCID,TACOCID     DISPLAY TALENT COMMERCIAL ID                 
         GOTO1 CHAROUT,DMCB,TANAELQ,SAKTCINH      AND TITLE                     
*                                                                               
DISP10   OI    SAKTCIDH+6,X'80'                                                 
         MVC   AIO,AIO1                                                         
*                                                                               
         MVI   SAKPLFT,C'N'                                                     
         TM    TLAKSTAT,TLAKSLFT                                                
         BZ    *+8                                                              
         MVI   SAKPLFT,C'Y'        ALIAS LIFT                                   
         OI    SAKPLFTH+6,X'80'                                                 
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,SAKCLINH,TAFNTCLI                           
         GOTO1 (RF),(R1),TAFNELQ,SAKPRDNH,TAFNTPRD                              
         GOTO1 (RF),(R1),TAFNELQ,SAKNIDNH,TAFNTTTL                              
*                                                                               
         GOTO1 ACTVOUT,DMCB,SAKACTVH LAST CHANGED                               
*                                                                               
         MVC   KEY,SVKEY           RESET KEY                                    
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
*                                                                               
ERRECXIT MVI   ERROR,RECEXIST      RECORD EXISTS                                
         B     THEEND                                                           
*                                                                               
ERRNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
*                                                                               
NODELETE MVC   MYMSGNO,=Y(ERALDEL) CANNOT DELETE - MATCHED HOLD RECORDS         
         B     ERREND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   OI    GENSTAT2,USGETTXT                                                
         MVI   MYMTYP,GTMERR                                                    
         MVI   BLOCK,0                                                          
         B     THEEND                                                           
         SPACE 2                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
PFTAB    DS    0C                  PF KEYS TABLE                                
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3' ',CL8'ALIAS   ',CL8'LIST    '                               
PF13X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD7D                                                       
         ORG   SAKWORK                                                          
SVKEY    DS    CL(L'TLDRREC)       SAVE KEY                                     
         EJECT                                                                  
* DDGENTWA                                                                      
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'041TAGEND7   07/20/12'                                      
         END                                                                    
