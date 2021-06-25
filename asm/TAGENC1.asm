*          DATA SET TAGENC1    AT LEVEL 026 AS OF 07/20/12                      
*PHASE T702C1C                                                                  
*INCLUDE DLFLD                                                                  
         TITLE 'T702C1 - SESSION ESTIMATING'                                    
T702C1   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702C1,RR=R2                                                   
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=PROGRAM SAVED STORAGE                     
         USING LOCALD,R7                                                        
         ST    R2,RELO                                                          
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
*                                                                               
         MVC   AMASTD,TWAMASTC                                                  
*                                                                               
         L     R1,TWADCONS                                                      
         USING TWADCOND,R1                                                      
         MVC   ALOGOC,TC$TLOGOC                                                 
*                                                                               
         L     R1,AMASTD                                                        
         USING MASTD,R1                                                         
         MVC   ALOGO,MCVLOGO       SAVE A(LOGO)                                 
         MVC   AREMOT,MCVREMOT     SAVE A(REMOTE)                               
         DROP  R1                                                               
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     TVX                                                              
         SPACE 1                                                                
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DKEY                                                          
         B     TVX                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         IF VALIDATE RECORD                           
         BNE   *+12                                                             
         BAS   RE,BLDREC           BUILD THE RECORD                             
         B     TVX                                                              
         SPACE 1                                                                
         CLI   MODE,XRECADD        IF RECORD ADDED                              
         BNE   TV30                                                             
         MVC   CONACT(6),=C'CHANGE' SWITCH ACTION TO CHANGE                     
         OI    CONACTH+6,X'80'     FOR NEXT TIME IN                             
         B     TV40                GO REDISPLAY                                 
         SPACE 1                                                                
TV30     CLI   MODE,XRECPUT        IF RECORD CHANGED                            
         BE    TV40                                                             
         CLI   MODE,XRECREST       OR RECORD RESTORED                           
         BNE   TV50                                                             
TV40     BAS   RE,DISPLAY          RE-DISPLAY THE RECORD                        
         B     TVX                                                              
         SPACE 1                                                                
TV50     CLI   MODE,DISPREC        IF MODE IS DISPLAY                           
         BNE   *+12                                                             
         BAS   RE,DISPLAY          DISPLAY THE RECORD                           
         B     TVX                                                              
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF MODE IS PRINT REPORT                      
         BNE   TVX                                                              
         GOTO1 RECVAL,DMCB,TLSSCDQ,(X'20',SSSESTH)  READ THE RECORD             
         BAS   RE,DISPLAY          DISPLAY HEADER DETAILS                       
         CLI   ACTNUM,ACTDWN       IF ACTION DOWNLOAD                           
         BNE   TVX                                                              
         BAS   RE,PREPD            RUN DOWNLOADABLE REPORT                      
         B     TVZ                                                              
*                                                                               
TVX      BAS   RE,GOSYSEST         GIVE TASYSEST A SHOT                         
*                                                                               
         CLI   ACTNUM,ACTREP       IF WILL GENERATE REPORT                      
         BNE   TVZ                                                              
         OI    SSSESTH+6,X'81'     MAKE KEY FIELD MODIFIED FOR NEXT             
*                                  (NECESSARY TO GET VALKEY/DISPREC IF          
*                                   CHANGE TO ACTION CHANGE NEXT TIME.)         
         SPACE 1                                                                
TVY      TM    WHEN,X'40'          IF RUNNING A 'NOW' REPORT                    
         BZ    *+16                                                             
         XC    CONSERV,CONSERV     SET FOR AUTO $DQU                            
         MVC   CONSERV(4),=C'$DQU'                                              
TVZ      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    SCRSTAT,SCRCHG+RECCHG  IF SCREEN/RECORD CHANGED                  
         BZ    *+8                                                              
         NI    SSSAGYH+4,X'DF'     SET AGENCY FIELD CHANGED                     
         SPACE 1                                                                
         MVI   TGTYPE,TLSSTYPR     SET TYPE = RADIO                             
         CLI   RECNUM,ET                                                        
         BNE   *+8                                                              
         MVI   TGTYPE,TLSSTYPT     ELSE SET TYPE = TV                           
         SPACE 1                                                                
         TM    SSSAGYH+4,X'20'     IF AGENCY CHANGED                            
         BO    VK10                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SSSAGYH),SSSAGYNH  AGENCY             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE 1                                                                
VK10     LA    R2,SSSESTH                                                       
         TM    4(R2),X'20'         IF ESTIMATE CODE CHANGED                     
         BO    *+8                                                              
         OI    TESTAT,TESTINIT     SET TO REINITIALIZE SCREEN                   
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF ADDING                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTREST      OR RESTORING                                 
         BNE   VK20                                                             
         GOTO1 RECVAL,DMCB,TLSSCDQ,(X'24',(R2))                                 
         BE    ONFILE              ENSURE NOT ON FILE ALREADY                   
         CLI   ERROR,MISSING       IF ERROR IS MISSING INPUT                    
         BE    THEEND              THEN GIVE IT NOW                             
         MVC   KEY,KEYSAVE         ELSE RESTORE KEY                             
         B     VKX                 AND LET GENCON HANDLE                        
         SPACE 1                                                                
VK20     GOTO1 RECVAL,DMCB,TLSSCDQ,(X'20',(R2))  ELSE GET THE RECORD            
         SPACE 1                                                                
VKX      OI    4(R2),X'20'         SET VALIDATED                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY THE KEY                                       
         SPACE 1                                                                
DKEY     NTR1                                                                   
         CLC   SVKEY,KEY           IF KEY CHANGED                               
         BE    *+8                                                              
         OI    TESTAT,TESTINIT     SET TO RE-INITIALIZE                         
         SPACE 1                                                                
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            AND I/O AREA                                 
         SPACE 1                                                                
         L     R4,AIO1             R4=A(ESTIMATE RECORD)                        
         USING TLSSD,R4                                                         
         SPACE 1                                                                
         MVC   SSSAGY,TLSSAGY      AGENCY                                       
         MVI   SSSAGYH+5,L'TLSSAGY                                              
         OI    SSSAGYH+4,X'20'                                                  
         OI    SSSAGYH+6,X'80'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',SSSAGYH),SSSAGYNH  AGY NAME           
         SPACE 1                                                                
         MVC   SSSEST,TLSSEST      ESTIMATE                                     
         OI    SSSESTH+4,X'20'                                                  
         OI    SSSESTH+6,X'80'                                                  
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND I/O AREA                                 
         B     XIT                                                              
         EJECT                                                                  
*              BUILD THE RECORD                                                 
         SPACE 1                                                                
BLDREC   NTR1                                                                   
         MVC   SVKEY,KEY           SAVE KEY                                     
         SPACE 1                                                                
         GOTO1 NAMIN,DMCB,TANAELQ,(X'80',SSSESTNH)  ESTIMATE NAME               
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLCLCDQ,SSSCLIH          CLIENT                      
         SPACE 1                                                                
         MVI   ELCODE,TACTELQ      REMOVE EXISTING CLIENT ELEMENT               
         GOTO1 REMELEM                                                          
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     ADD NEW CLIENT ELEMENT                       
         LA    R4,ELEMENT                                                       
         USING TACTD,R4                                                         
         MVI   TACTEL,TACTELQ                                                   
         MVI   TACTLEN,TACTLNQ                                                  
         MVC   TACTCLI,TGCLI                                                    
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
         MVI   ELCODE,TAPRELQ      REMOVE EXISTING PRODUCT ELEMENT              
         GOTO1 REMELEM                                                          
         CLI   SSSPRDH+5,0         VALIDATE PRODUCT (OPTIONAL)                  
         BE    BLDRX                                                            
         NI    SSSPRDH+4,X'DF'     INSURE WE VALIDATE                           
         GOTO1 RECVAL,DMCB,TLPRCDQ,SSSPRDH                                      
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     ADD NEW PRODUCT ELEMENT                      
         LA    R4,ELEMENT                                                       
         USING TAPRD,R4                                                         
         MVI   TAPREL,TAPRELQ                                                   
         MVI   TAPRLEN,TAPRLNQ                                                  
         MVC   TAPRPRD,TGPRD                                                    
         GOTO1 ADDELEM                                                          
         SPACE 1                                                                
BLDRX    GOTO1 ACTVIN,DMCB,0       UPDATE LAST ACTIVITY                         
         SPACE 1                                                                
         MVC   KEY,SVKEY           RESTORE KEY                                  
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY THE RECORD                                               
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         TWAXC SSSESTNH            CLEAR SCREEN                                 
         XC    SSSCLIN,SSSCLIN                                                  
         OI    SSSCLINH+6,X'80'                                                 
         XC    SSSPRDN,SSSPRDN                                                  
         OI    SSSPRDNH+6,X'80'                                                 
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,SSSESTNH  ESTIMATE NAME                     
         SPACE 1                                                                
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVC   AIO,AIO2            SET ALT. I/O AREA FOR READS                  
         SPACE 1                                                                
         MVI   ELCODE,TACTELQ      GET CLIENT CODE                              
         L     R4,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   DISP20                                                           
         USING TACTD,R4                                                         
         MVC   SSSCLI,TACTCLI                                                   
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',SSSCLI),SSSCLINH                      
         SPACE 1                                                                
DISP20   MVI   ELCODE,TAPRELQ      GET PRODUCT CODE                             
         L     R4,AIO1                                                          
         BAS   RE,GETEL                                                         
         BNE   DISP40                                                           
         USING TAPRD,R4                                                         
         MVC   SSSPRD,TAPRPRD                                                   
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'88',SSSPRD),SSSPRDNH                      
         SPACE 1                                                                
DISP40   MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO1            AND I/O AREA FOR ESTIMATE RECORD             
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,SSSLCHGH  LAST CHANGED INFO                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE PASSES CONTROL TO TASYSEST OVERLAY                       
         SPACE 1                                                                
GOSYSEST NTR1                                                                   
         LA    R1,SSSAGYH          A(FIRST KEY FIELD)                           
         ST    R1,AFRSTKEY                                                      
         ST    RC,TEAGEND          A(GEND)                                      
         LA    R1,TAWORK                                                        
         ST    R1,TEAWORKD         A(AREA FOR TALENT SYSTEM WORK AREAS)         
         LA    R1,SSSTAGH                                                       
         ST    R1,TEAOVLYH         A(BEGINNING OF VARIABLE SCREEN)              
         LA    R1,SSSOPTSH                                                      
         ST    R1,TEAOPTSH         A(OPTIONS FIELD)                             
         MVC   TEAIO,AIO           A(I/O AREA FOR ESTIMATE REC.)                
         MVC   TEAS2ACC,TGAS2ACC   A(STAFF2 AGENCY/CLIENT COMBINATIONS)         
         SPACE 1                                                                
         MVI   TEECELCD,TAECELQ    ESTIMATE CAST DETAILS EL. CODE               
         MVI   TEETELCD,TAETELQ    ESTIMATE TOTALS ELEMENT CODE                 
         MVI   TEEOELCD,TAEOELQ    ESTIMATE OPTIONS ELEMENT CODE                
         MVI   TEXCELCD,TAXCELQ    EXTENDED COMMENT ELEMENT CODE                
         MVI   TEWCELCD,TAWCELQ    WORK-CODE SUMMARY ELEMENT CODE               
         MVC   TEMEDIA,TGTYPE      MEDIA                                        
         SPACE 1                                                                
         MVI   TEPFON,PFON         PFKEY TO ADD ON-CAMERA LINE                  
         MVI   TEPFOFF,PFOFF                    OFF-CAMERA                      
         MVI   TEPFSING,PFSING                  SINGER                          
         MVI   TEPFEXT,PFEXT                    EXTRA                           
         MVI   TEPFRAD,PFRAD                    RADIO                           
         MVI   TEPFRSNG,PFRSNG                  RADIO SINGER                    
         MVI   TEPFMUS,PFMUS                    MUSICIAN                        
         MVI   TEPFMISC,PFMISC                  MISCELLANEOUS                   
         MVI   TEPFCMNT,PFCMNT                  COMMENT                         
         MVI   TEPFREP,PFREP       PFKEY TO GENERATE REPORT                     
         SPACE 1                                                                
         MVC   TETALAGY,TGAGY      SET TALENT AGENCY                            
         SPACE 1                                                                
         XC    TETALCLI,TETALCLI                                                
         MVI   ELCODE,TACTELQ      GET CLIENT ELEMENT                           
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         USING TACTD,R4                                                         
         MVC   TETALCLI,TACTCLI    SET TALENT CLIENT                            
         SPACE 1                                                                
         GOTO1 TASYSEST,TEBLOCK    OFF TO TASYSEST                              
         SPACE 1                                                                
         CLI   MODE,VALREC         IF MODE IS VALIDATE RECORD                   
         BNE   GOX                                                              
         CLI   ACTNUM,ACTADD       AND ACTION IS NOT ADD                        
         BE    GOX                                                              
         TM    TESTAT,TESTRDTL     THEN IF SYSEST READ A TALENT REC.            
         BZ    GOX                                                              
         MVC   AIO,AIO2            NEED TO RE-GET EST REC BEFORE PUTREC         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
GOX      B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ONFILE   MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     THEEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,PFON,0,0,PFTRETRN)                                   
         DC    CL3' ',CL8'        ',CL8'        '                               
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,PFOFF,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8'        ',CL8'        '                               
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,PFSING,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8'        ',CL8'        '                               
PF15X    EQU   *                                                                
         DC    AL1(PF16X-*,PFEXT,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8'        ',CL8'        '                               
PF16X    EQU   *                                                                
         DC    AL1(PF17X-*,PFMUS,0,0,PFTRETRN)                                  
         DC    CL3' ',CL8'        ',CL8'        '                               
PF17X    EQU   *                                                                
         DC    AL1(PF19X-*,PFCMNT,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8'        ',CL8'        '                               
PF19X    EQU   *                                                                
         DC    AL1(PF20X-*,PFMISC,0,0,PFTRETRN)                                 
         DC    CL3' ',CL8'        ',CL8'        '                               
PF20X    EQU   *                                                                
         DC    AL1(PF24X-*,PFREP,0,0,PFTSETPN)                                  
         DC    CL3' ',CL8'        ',CL8'REPORT  '                               
PF24X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
PFON     EQU   13                  PFKEY TO ADD ON-CAMERA LINE                  
PFOFF    EQU   14                               OFF-CAMERA                      
PFSING   EQU   15                               SINGER                          
PFEXT    EQU   16                               EXTRA                           
PFRAD    EQU   13                               RADIO                           
PFRSNG   EQU   14                               RADIO SINGER                    
PFMUS    EQU   17                               MUSICIAN                        
PFMISC   EQU   19                               MISCELLANEOUS                   
PFCMNT   EQU   20                               COMMENT                         
PFREP    EQU   24                  PFKEY TO GENERATE REPORT                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================          
*              ROUTINE TO GENERATE REPORTS                                      
*=====================================================================          
PREPD    NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R5                                                         
*                                                                               
*        MVC   P(20),=CL20'TESTING PROGRAM'                                     
*        GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R5,DLBLOCK          R5=A(DOWNLOAD BLOCK)                         
         BAS   RE,NEWPRTQ                                                       
         GOTO1 REQTWA,DMCB,(3,ATWA),,VPRINT,(C'B',ABOX)                         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,INITDWN          INITIALIZE DLBLOCK FOR DOWNLOAD              
*                                                                               
         MVI   DMCB,0                                                           
         BAS   RE,PRTHEAD          HEADLINES                                    
*                                                                               
         MVI   TEMPDETL,C' '                                                    
         MVC   TEMPDETL+1(256),TEMPDETL                                         
         MVC   TEMPDETL+256(DWCASTLN-256),SPACES                                
         LA    R6,TEMPDETL                                                      
         USING DWCASTD,R6                                                       
*                                                                               
         L     R3,AIO                                                           
         USING TLSSD,R3                                                         
         MVC   DWCAGY,TLSSAGY                                                   
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   TEMPNAMH,44         PUT LENGTH IN FAKE FIELD HEADER              
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'88',TLSSAGY),TEMPNAMH                     
         OC    TEMPNAME,SPACES                                                  
         MVC   DWCAGYN,TEMPNAME                                                 
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,TACTELQ                                                   
         BRAS  RE,GETEL            GET CLIENT ELEMENT                           
         BNE   PREP10                                                           
         USING TACTD,R4                                                         
         MVC   DWCCLI,TACTCLI      CLIENT CODE                                  
*                                                                               
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'88',TACTCLI),TEMPNAMH                     
         OC    TEMPNAME,SPACES                                                  
         MVC   DWCCLIN,TEMPNAME                                                 
         DROP  R4                                                               
*                                                                               
PREP10   L     R4,AIO1                                                          
         MVI   ELCODE,TAPRELQ                                                   
         BRAS  RE,GETEL            GET PRODUCT ELEMENT                          
         BNE   PREP20                                                           
         USING TAPRD,R4                                                         
         MVC   DWCPRD,TAPRPRD      PRODUCT CODE                                 
*                                                                               
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'88',TAPRPRD),TEMPNAMH                     
         OC    TEMPNAME,SPACES                                                  
         MVC   DWCPRDN,TEMPNAME                                                 
         DROP  R4                                                               
*                                                                               
PREP20   L     R3,AIO1                                                          
         MVC   DWCEST,TLSSEST       ESTIMATE, DATE AND NAME                     
         GOTO1 DATCON,DMCB,(5,0),(11,DWCESTD)                                   
         GOTO1 RECVAL,DMCB,TLSSCDQ,(X'88',TLSSEST),TEMPNAMH                     
         OC    TEMPNAME,SPACES                                                  
         MVC   DWCESTN,TEMPNAME     ESTIMATE NAME                               
*                                                                               
         L     R4,AIO1                                                          
         MVI   ELCODE,TAECELQ                                                   
         BRAS  RE,GETEL            GET CAST ELEMENT                             
         B     *+8                                                              
PREPD30  BRAS  RE,NEXTEL           GET CAST ELEMENT                             
         BNE   PREPD90X                                                         
         USING TAECD,R4                                                         
*                                                                               
         MVC   DWCNUM(DWCASTL2),SPACES                                          
*                                                                               
         EDIT  TAECNET,(10,DWCNAMT),2,ZERO=BLANK,ALIGN=LEFT,FLOAT=-             
         EDIT  TAECPNH,(9,DWCPNH),2,ZERO=BLANK,ALIGN=LEFT,FLOAT=-               
         EDIT  TAECHNW,(7,DWCHNW),2,ZERO=BLANK,ALIGN=LEFT,FLOAT=-               
         EDIT  TAECTAX,(9,DWCTAX),2,ZERO=BLANK,ALIGN=LEFT,FLOAT=-               
         EDIT  TAECHND,(9,DWCHANDL),2,ZERO=BLANK,ALIGN=LEFT,FLOAT=-             
         EDIT  TAECEMSF,(9,DWCEMSF),2,ZERO=BLANK,ALIGN=LEFT,FLOAT=-             
         SR    RF,RF                                                            
         IC    RF,TAECLEN                                                       
         SH    RF,=Y(TAECLNQ)                                                   
         BZ    PREPD33                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DWCCOMMT(0),TAECCMNT                                             
*                                                                               
PREPD33  CLI   TAECTYPE,TAECTYPM   MISC                                         
         BNE   PREPD35                                                          
         MVC   DWCNAME(L'TAECDESC),TAECDESC                                     
         B     PREPD90                                                          
*                                                                               
PREPD35  EDIT  TAECNUM,(3,DWCNUM),ZERO=BLANK,ALIGN=LEFT                         
         MVC   DWCNAME,=CL30'Principal'                                         
         MVC   DWCCAT,=CL3'P  '                                                 
         CLI   TAECTYPE,TAECTYPO                                                
         BE    PREPD39X                                                         
         CLI   TAECTYPE,TAECTYPF                                                
         BE    PREPD39X                                                         
         CLI   TAECTYPE,TAECTYPR                                                
         BNE   PREPD35A                                                         
         MVC   DWCCAT,=CL3'ANN'                                                 
         B     PREPD39X                                                         
*                                                                               
PREPD35A LA    R2,CATTABLS                                                      
PREPD35B CLI   0(R2),X'FF'                                                      
         BE    PREPD39                                                          
         CLC   TAECCAT,0(R2)                                                    
         BE    PREPD38                                                          
         LA    R2,33(R2)                                                        
         B     PREPD35B                                                         
*                                                                               
PREPD38  MVC   DWCNAME,3(R2)                                                    
*                                                                               
PREPD39  MVC   DWCCAT,TAECCAT                                                   
PREPD39X OC    DWCCAT,SPACES                                                    
         EDIT  TAECSP,(2,DWCSPOT),ZERO=BLANK,ALIGN=LEFT                         
         EDIT  TAECDAY,(2,DWCDAY),ZERO=BLANK,ALIGN=LEFT                         
         EDIT  TAECOT,(2,DWCOVER),ZERO=BLANK,ALIGN=LEFT                         
         EDIT  TAECDT,(2,DWCDBLE),ZERO=BLANK,ALIGN=LEFT                         
         EDIT  TAECTAG,(3,DWCTAGS),ZERO=BLANK,ALIGN=LEFT                        
         EDIT  TAECTRV,(5,DWCTRVL),ZERO=BLANK,ALIGN=LEFT                        
         EDIT  TAECDBL,(1,DWCDBLE),ZERO=BLANK,ALIGN=LEFT                        
         EDIT  TAECOV1,(6,DWCOVSC),2,ZERO=BLANK,ALIGN=LEFT                      
         EDIT  TAECCART,(10,DWCCARTG),2,ZERO=BLANK,ALIGN=LEFT                   
*                                                                               
         CLI   TAECTYPE,TAECTYPU   MUSICIANS                                    
         BNE   PREPD40                                                          
         EDIT  TAECHRM,(5,DWCHRMN),2,ZERO=BLANK,ALIGN=LEFT                      
*                                                                               
PREPD40  CLI   TAECTYPE,TAECTYPO                                                
         BNE   PREPD70                                                          
         EDIT  TAECPDW,(5,DWCPDWD),ZERO=BLANK                                   
*                                                                               
PREPD70  TM    TAECSTAT,TAECSONC   ON CAMERA?                                   
         BO    PREPD79                                                          
         CLI   TAECTYPE,TAECTYPO                                                
         BNE   PREPD80                                                          
PREPD79  MVI   DWCCAMRA,C'Y'                                                    
*                                                                               
PREPD80  TM    TAECSTAT,TAECSDEM   DEMO COMMERCIAL                              
         BZ    *+8                                                              
         MVI   DWCDEMO,C'Y'                                                     
*                                                                               
         TM    TAECSTAT,TAECSNON   NON-UNION                                    
         BZ    *+8                                                              
         MVI   DWCNONUN,C'Y'                                                    
*                                                                               
PREPD90  BAS   RE,MOVEDWN          MOVE DOWNLOAD BLOCK                          
         B     PREPD30                                                          
*                                                                               
PREPD90X DS    0H                                                               
*                                                                               
         MVI   DLCBACT,DLCBEOR     MARK END OF REPORT                           
         GOTO1 =V(DLFLD),DLCBD,RR=RELO                                          
*                                                                               
PREPDX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
*              ROUTINE TO SET UP SECOND REPORT ON QUEUE                         
*======================================================================         
*                                                                               
NEWPRTQ  NTR1                                                                   
*                                                                               
         L     R2,ALOGOC                                                        
         USING LOGOD,R2                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 ALOGO,DMCB,(R2)     END REPORT LOGOS                             
*                                                                               
         TM    WHEN,X'20'          SOON?                                        
         BO    NPQ010                                                           
         GOTO1 VPRINT,DMCB,=C'CLOSE'                                            
*                                                                               
NPQ010   XC    SPECS,SPECS         CLEAR SPECS                                  
         XC    HEADHOOK,HEADHOOK   AND HEADLINE HOOK                            
*                                                                               
         L     R2,ABOX                                                          
         USING BOXD,R2                                                          
         LA    RE,132                                                           
         ST    RE,BOXWIDTH         SET LENGTH OF PRINT LINE                     
*                                                                               
         L     RF,AMASTD                                                        
         USING MASTD,RF                                                         
         L     R2,AREMOT                                                        
         USING REMOTED,R2                                                       
         TM    WHEN,X'20'          SOON?                                        
         BZ    NPQ050                                                           
         XC    MCREMPQK,MCREMPQK                                                
         B     NPQ060                                                           
*                                                                               
NPQ050   MVC   REMOTABF,MCVPQBUF                                                
         MVC   REMOTADM,MCVDMGR                                                 
         MVC   REMOTAOP,MCVPQOPN                                                
         MVC   REMOTDST,MCDESTID                                                
         XC    MCALTREF,MCALTREF                                                
         MVI   REMOTCPY,C'1'                                                    
         MVI   REMOTCLS,C'Q'                                                    
         MVC   REMOTJID,=C'TED'                                                 
NPQ060   MVC   REMOTKEY(11),SPACES                                              
         MVC   REMOTSYS(6),=C'ETDATA'                                           
         MVC   REMOTFRM(4),=C'DATA'                                             
*                                                                               
         B     PREPDX                                                           
         DROP  R2,RF                                                            
         EJECT                                                                  
*              INITIALIZE DOWNLOAD BLOCK                                        
         SPACE 1                                                                
INITDWN  NTR1                                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         XC    DLCBD(DLCBXLX),DLCBD                                             
*                                                                               
         MVI   DLCBACT,DLCBINIT    INITIALIZE FOR DOWNLOAD                      
         LA    R1,SPLATDWN         A(HOOK ROUTINE FOR PRINTING)                 
         ST    R1,DLCBAPR                                                       
         LA    R1,P                A(PRINT LINE)                                
         ST    R1,DLCBAPL                                                       
         MVC   DLCBAED,EDITOR                                                   
         MVC   DLCXMAXL,=Y(L'P)    MAX LENGTH OF PRINT LINE                     
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      TEXT DELIMITER ALTERNATE                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON FOR END OF LINE                   
         MVI   DLCXEORC,C':'       END OF REPORT                                
         GOTO1 =V(DLFLD),DLCBD,RR=RELO                                          
         B     PREPDX                                                           
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              USER SUPPLIED PRINT ROUTINE - DOWNLOAD                           
         SPACE 1                                                                
SPLATDWN NTR1                                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   LINE,1                                                           
         B     PREPDX                                                           
         DROP  R8                                                               
         EJECT                                                                  
*              MOVE FIELDS FOR DOWNLOAD                                         
MOVEDWN  NTR1                                                                   
         LA    R3,TEMPDETL                                                      
         LA    R2,DOWNTAB          FIELD LENGTHS                                
MOVED10  CLI   0(R2),X'FF'                                                      
         BE    MOVEDWNX                                                         
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVC   DLCBTYP,0(R2)       DATA TYPE IS TEXT                            
         MVC   DLCBLEN,1(R2)                                                    
         SR    RF,RF                                                            
         IC    RF,1(R2)                                                         
         LR    R4,RF                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),0(R3)                                                 
         GOTO1 =V(DLFLD),DLCBD,RR=RELO     SEND TO DLFLD                        
         LA    R2,2(R2)                                                         
         AR    R3,R4                                                            
         B     MOVED10                                                          
*                                                                               
MOVEDWNX MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD,RR=RELO                                          
         B     PREPDX                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              PRINT HEADER LINE FOR SPREADSHEET                                
         SPACE 1                                                                
PRTHEAD  NTR1                                                                   
*                                                                               
         LA    R2,HDLINE1          ESTIMATE INFO                                
PHEAD1   CLI   0(R2),X'FF'                                                      
         BE    PHEADX                                                           
         MVI   DLCBACT,DLCBPUT     PUT ITEM TO PRINT LINE                       
         MVI   DLCBTYP,DLCBTXT     DATA TYPE IS TEXT                            
         MVC   DLCBLEN,0(R2)                                                    
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),1(R2)                                                 
         GOTO1 =V(DLFLD),DLCBD,RR=RELO     SEND TO DLFLD                        
         SR    RF,RF                                                            
         IC    RF,0(R2)                                                         
         LA    R2,1(RF,R2)                                                      
         B     PHEAD1                                                           
*                                                                               
PHEADX   MVI   DLCBACT,DLCBEOL     END OF LINE                                  
         GOTO1 =V(DLFLD),DLCBD,RR=RELO                                          
PHEADZ   B     PREPDX                                                           
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
HDLINE1  DC    AL1(06),C'Agency'                                                
         DC    AL1(11),C'Agency Name'                                           
         DC    AL1(06),C'Client'                                                
         DC    AL1(11),C'Client Name'                                           
         DC    AL1(07),C'Product'                                               
         DC    AL1(12),C'Product Name'                                          
         DC    AL1(08),C'Estimate'                                              
         DC    AL1(13),C'Estimate Date'                                         
         DC    AL1(13),C'Estimate Name'                                         
*                                                                               
         DC    AL1(06),C'Number'                                                
         DC    AL1(04),C'Name'                                                  
         DC    AL1(08),C'Category'                                              
         DC    AL1(06),C'Camera'                                                
         DC    AL1(04),C'Demo'                                                  
         DC    AL1(05),C'Spots'                                                 
         DC    AL1(11),C'Hour.Minute'                                           
         DC    AL1(04),C'Days'                                                  
         DC    AL1(08),C'Overtime'                                              
         DC    AL1(09),C'Double OT'                                             
         DC    AL1(07),C'Doubles'                                               
         DC    AL1(03),C'Tag'                                                   
         DC    AL1(06),C'Travel'                                                
         DC    AL1(05),C'Pd-Wd'                                                 
         DC    AL1(03),C'Non'                                                   
         DC    AL1(07),C'Cartage'                                               
         DC    AL1(07),C'Comment'                                               
         DC    AL1(10),C'Net Amount'                                            
         DC    AL1(09),C'Overscale'                                             
         DC    AL1(03),C'P&&H'                                                  
         DC    AL1(03),C'H&&W'                                                  
         DC    AL1(11),C'Payroll Tax'                                           
         DC    AL1(08),C'Handling'                                              
         DC    AL1(07),C'EMS Fee'                                               
         DC    X'FFFF'                                                          
         EJECT                                                                  
*                                                                               
*        TYPE AND FIELD LENGTHS FOR MOVEDWN                                     
*                                                                               
DOWNTAB  DC    C'T',AL1(6)                                                      
         DC    C'T',AL1(36)                                                     
         DC    C'T',AL1(6)                                                      
         DC    C'T',AL1(36)                                                     
         DC    C'T',AL1(6)                                                      
         DC    C'T',AL1(36)                                                     
         DC    C'T',AL1(20)                                                     
         DC    C'T',AL1(8)                                                      
         DC    C'T',AL1(36)                                                     
*                                                                               
         DC    C'N',AL1(3)                                                      
         DC    C'T',AL1(30)                                                     
         DC    C'T',AL1(3)                                                      
         DC    C'T',AL1(3)                                                      
         DC    C'T',AL1(1)                                                      
         DC    C'N',AL1(2)                                                      
         DC    C'N',AL1(5)                                                      
         DC    C'N',AL1(2)                                                      
         DC    C'N',AL1(2)                                                      
         DC    C'N',AL1(2)                                                      
         DC    C'N',AL1(1)                                                      
         DC    C'N',AL1(3)                                                      
         DC    C'N',AL1(5)                                                      
         DC    C'N',AL1(5)                                                      
         DC    C'T',AL1(1)                                                      
         DC    C'N',AL1(10)                                                     
         DC    C'T',AL1(20)                                                     
         DC    C'N',AL1(10)                                                     
         DC    C'N',AL1(6)                                                      
         DC    C'N',AL1(9)                                                      
         DC    C'N',AL1(7)                                                      
         DC    C'N',AL1(9)                                                      
         DC    C'N',AL1(9)                                                      
         DC    C'N',AL1(9)                                                      
         DC    X'FFFF'                                                          
*                                                                               
*        CATEGORIES                                                             
*                                                                               
CATTABLS DC    C'S  ',CL30'Solo/Duo'                                            
         DC    C'SM ',CL30'Solo/Duo: multi-tracking'                            
         DC    C'G3 ',CL30'Group 3-5'                                           
         DC    C'G6 ',CL30'Group 6-8'                                           
         DC    C'G9 ',CL30'Group 9+'                                            
         DC    C'G3M',CL30'Group 3-5: multi-tracking'                           
         DC    C'G6M',CL30'Group 6-8: multi-tracking'                           
         DC    C'G9M',CL30'Group 9+: multi-tracking'                            
*                                                                               
         DC    C'EX ',CL30'Extra'                                               
         DC    C'EXB',CL30'Bought out extra'                                    
         DC    C'HM ',CL30'Hand Model'                                          
         DC    C'HMB',CL30'Bought out hand model'                               
         DC    C'EXD',CL30'Precision Driver'                                    
*                                                                               
         DC    C'L  ',CL30'Leader'                                              
         DC    C'LA ',CL30'Leader, arranger'                                    
         DC    C'LO ',CL30'Leader, orchestrator'                                
         DC    C'LAC',CL30'Leader, arranger, copyist'                           
         DC    C'LAO',CL30'Leader, arranger, orchestrator'                      
         DC    C'LOC',CL30'Leader, orchestrator, copyist'                       
         DC    C'LCP',CL30'Leader, copyist'                                     
         DC    C'C  ',CL30'Contractor'                                          
         DC    C'CA ',CL30'Contractor, Arranger'                                
         DC    C'CO ',CL30'Contractor, orchestrator'                            
         DC    C'CCP',CL30'Contractor, copyist'                                 
         DC    C'A  ',CL30'Arranger'                                            
         DC    C'AM ',CL30'Arranger, musician'                                  
         DC    C'AO ',CL30'Arranger, orchestrator'                              
         DC    C'AMC',CL30'Arranger, musician, copyist'                         
         DC    C'ACP',CL30'Arranger, copyist'                                   
         DC    C'O  ',CL30'Orchestrator'                                        
         DC    C'OM ',CL30'Orchestrator, musician'                              
         DC    C'OMC',CL30'Orchestrator, musician, copyist'                     
         DC    C'OCP',CL30'Orchestrator, copyist'                               
         DC    C'M  ',CL30'Musician'                                            
         DC    C'MCP',CL30'Musician, copyist'                                   
         DC    C'CP ',CL30'Copyist'                                             
         DC    C'LM ',CL30'Leader, musician'                                    
         DC    C'SYN',CL30'Synthesizer'                                         
         DC    C'ZZZ',CL30'Other'                                               
         DC    X'FF'                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
*              DOWNLOAD DSECT FOR CAST                                          
DWCASTD  DSECT                                                                  
DWCAGY   DS    CL6                 AGENCY                                       
DWCAGYN  DS    CL36                AGENCY NAME                                  
DWCCLI   DS    CL6                 CLIENT                                       
DWCCLIN  DS    CL36                CLIENT NAME                                  
DWCPRD   DS    CL6                 PRODUCT                                      
DWCPRDN  DS    CL36                PRODUCT NAME                                 
DWCEST   DS    CL20                ESTIMATE                                     
DWCESTD  DS    CL8                 ESTIMATE DATE                                
DWCESTN  DS    CL36                ESTIMATE NAME                                
*                                                                               
DWCNUM   DS    CL3                 NUMBER OF PERFORMERS                         
DWCNAME  DS    CL30                NAME OF PERFORMER TYPE                       
DWCCAT   DS    CL3                 CATEGORY                                     
DWCCAMRA DS    CL3                 ON-CAMERA   ON/OFF                           
DWCDEMO  DS    CL1                 DEMO COMMERCIAL                              
DWCSPOT  DS    CL2                 NUMBER OF SPOTS                              
DWCHRMN  DS    CL5                 HH.MM                                        
DWCDAY   DS    CL2                 NUMBER OF DAYS                               
DWCOVER  DS    CL2                 NUMBER OF OVERTIME HOURS                     
DWCDOT   DS    CL2                 NUMBER OF DOUBLE OVERTIME HOURS              
DWCDBLE  DS    CL1                 NUMBER OF MUSICIAN DOUBLES                   
DWCTAGS  DS    CL3                 TAGS                                         
DWCTRVL  DS    CL5                 TRAVEL                                       
DWCPDWD  DS    CL5                 PRIOR DAY WARDROBE                           
DWCNONUN DS    CL1                 NON-UNION                                    
DWCCARTG DS    CL10                CARTAGE                                      
DWCCOMMT DS    CL20                COMMENT                                      
DWCNAMT  DS    CL10                NETAMOUNT                                    
DWCOVSC  DS    CL6                 OVERSCALE                                    
DWCPNH   DS    CL9                 P&H                                          
DWCHNW   DS    CL7                 H&W                                          
DWCTAX   DS    CL9                 TAX                                          
DWCHANDL DS    CL9                 HANDLING                                     
DWCEMSF  DS    CL9                 EMS FEE                                      
DWCASTLN EQU   *-DWCASTD                                                        
DWCASTL2 EQU   *-DWCNUM                                                         
         EJECT                                                                  
*                                                                               
*              LOCAL SAVED STORAGE                                              
         SPACE 2                                                                
LOCALD   DSECT                                                                  
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
DLBLOCK  DS    CL(DLCBXLX)                                                      
RELO     DS    F                                                                
AMASTD   DS    A                                                                
ALOGOC   DS    A                                                                
ALOGO    DS    A                                                                
AREMOT   DS    A                                                                
TEMPNAMH DS    CL8                 FAKE HEADER FIELD                            
TEMPNAME DS    CL36                TEMPORARY NAME RETURNED FROM RECVAL          
TEMPDETL DS    (DWCASTLN)C                                                      
TAWORK   DS    2200C               TALENT SYSTEM WORK AREAS                     
         SPACE 2                                                                
       ++INCLUDE TASYSESTD                                                      
         SPACE 2                                                                
         DS    0CL(L'TWAHOLE-TEBLKLNQ-(TEBLOCK-LOCALD))                         
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRC1D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDDLCB                                                                        
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
TWADCOND DSECT                                                                  
*PREFIX=TC$                                                                     
       ++INCLUDE DDTWADCONS                                                     
*PREFIX=                                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026TAGENC1   07/20/12'                                      
         END                                                                    
