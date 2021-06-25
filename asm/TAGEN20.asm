*          DATA SET TAGEN20    AT LEVEL 058 AS OF 10/10/14                      
*PHASE T70220A                                                                  
         TITLE 'T70220 - COMMERCIAL ID MAINTENANCE'                             
T70220   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 TMPLNQ,T70220,R7                                                 
         LR    R6,RC               R6=A(TEMPORARY STORAGE)                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         USING TMPD,R6                                                          
         LA    RE,SVPTRBLK                                                      
         ST    RE,ASVPBLK                                                       
         AHI   RE,L'SVPTRBLK                                                    
         ST    RE,AADPBLK                                                       
         EJECT                                                                  
*                                                                               
*              MODE CONTROLLED ROUTINES                                         
*                                                                               
         GOTO1 INITIAL,DMCB,0                                                   
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SICAGYH),SICAGYNH                     
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'38',SICOIDH),SICOIDNH                    
*                                                                               
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTWEB))                                     
         BE    WEBERR                                                           
                                                                                
         CLI   MODE,VALREC         VALIDATE THE RECORD                          
         BNE   CIDX                                                             
         BAS   RE,BLDREC                                                        
*                                                                               
CIDX     B     XIT                                                              
         SPACE 2                                                                
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              BUILD THE RECORD                                                 
*                                                                               
BLDREC   NTR1                                                                   
         GOTO1 SAVPTRS,DMCB,ASVPBLK   SAVE PASSIVE POINTERS                     
         LA    R2,SICNIDH          NEW COMMERCIAL ID                            
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         OC    SICNID,SPACES       PAD WITH SPACES                              
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,(X'20',TLCOICDQ),(X'04',SICNIDH) SEE IF COMM         
         BE    EXISTERR            ID AND VERSIONS ID ALREADY EXISTS            
         MVC   AIO,AIO1                                                         
*                                                                               
         USING TLCAPD,R4                                                        
         MVI   CHGHLD,C'N'                                                      
         LA    R4,KEY              READ ALL HOLDING FEE KEYS FOR                
         XC    KEY,KEY             COMMERCIAL'S CAST TO SEE IF NEW              
         MVI   TLCAPCD,TLCAHCDQ    HOLDING FEE HAS TO BE GENERATED              
         MVC   TLCAHCOM,TGCOM                                                   
         GOTO1 HIGH                (SKIP COMMERCIAL POINTER)                    
BREC10   GOTO1 SEQ                                                              
         CLC   KEY(TLCAHSRT-TLCAPD),KEYSAVE                                     
         BNE   BREC20                                                           
         OC    TLCAHNXT,TLCAHNXT   IF NO CAST HAS RECEIVED HOLDING              
         BZ    BREC10              FEE YET                                      
         CLC   TLCAHDTE,TLCAHNXT   OR LATEST NOTICE WAS PAID                    
         BH    BREC10              DO NOT TURN ON STATUS                        
         MVI   CHGHLD,C'Y'         ELSE, TURN ON STATUS                         
         DROP  R4                                                               
*                                                                               
         USING TLCOD,R4                                                         
BREC20   L     R4,AIO                                                           
         XC    SVCOKEY,SVCOKEY     SAVE INITIAL KEY                             
         MVC   SVCOKEY(L'TLCOKEY),TLCOKEY                                       
*                                                                               
         CLC   TLCOCID,SICNID      IF COMM ID IS SAME AS FAR                    
         JE    BREC30              AS KEY IS CONCERNED, SKIP AHEAD              
*                                                                               
         OI    TLCOSTAT,X'80'      DELETE INITIAL RECORD                        
         GOTO1 PUTREC              AND KEYS                                     
         GOTO1 ADDPTRS,DMCB,(X'E8',ASVPBLK),AADPBLK                             
         MVC   TLCOKEY,SVCOKEY                                                  
         XC    TLCOSTAT,TLCOSTAT                                                
*                                                                               
         L     RE,ASVPBLK          CLEAR PASSIVE POINTER BLOCK                  
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         MVC   TLCOCID,SICNID      CHANGE TO NEW COMM ID                        
         DROP  R4                                                               
*                                                                               
         USING TACOD,R4                                                         
BREC30   MVI   ELCODE,TACOELQ      GET COMM DETAILS ELEM                        
         BAS   RE,GETEL                                                         
         BE    *+6                 MUST BE AT LEAST 1 ELEMENT                   
         DC    H'0'                                                             
*                                                                               
         MVC   TACOCID,SICNID      MOVE IN NEW ID NUM                           
*                                                                               
         CLI   CHGHLD,C'Y'         IF NEED TO GENERATE UPDATED                  
         BNE   *+8                 HOLDING FEE                                  
         OI    TACOSTA2,TACOCHHF   TURN ON STATUS                               
         GOTOR SNDMQHFR,DMCB,(PRGSTAT,TGCOM),(TGSYSTA2,HEXOUT),MQIO             
*                                                                               
         MVC   TGCTEQU,TACOTYPE    SAVE COMMERCIAL TYPE                         
*                                                                               
         LA    R4,ELEM             OLD CID                                      
         XC    ELEM,ELEM                                                        
         USING TAOCD,R4                                                         
         MVI   TAOCEL,TAOCELQ                                                   
         MVI   TAOCLEN,TAOCLNQ                                                  
         MVC   TAOCDTE,TGTODAY1                                                 
         XC    TAOCDTE,=X'FFFFFF'  COMPLEMENTED DATE                            
         MVC   TAOCAGY,SICAGY      AGENCY                                       
         OC    TAOCAGY,SPACES                                                   
         MVC   TAOCCID,SICOID                                                   
         OC    TAOCCID,SPACES                                                   
         GOTO1 ADDELEM                                                          
*                                                                               
         USING TAVRD,R4                                                         
         L     R4,AIO              CHANGE A VERSION CID                         
         MVI   ELCODE,TAVRELQ      TO MATCH                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
BREC40   BAS   RE,NEXTEL                                                        
         BNE   BREC50                                                           
         MVI   TGBYTE,C'A'                                                      
         TM    TGSYSTAT,TASYS3VR                                                
         BZ    *+8                                                              
         MVI   TGBYTE,1                                                         
         CLC   TAVRVERS,TGBYTE                                                  
         BNE   BREC40                                                           
         MVC   TAVRCID,SICNID                                                   
         DROP  R4                                                               
*                                                                               
         USING TAMCD,R4                                                         
BREC50   TM    TGSYSTAT,TASYSMUS   IF NEW MUSIC RULES ENABLED                   
         BZ    BREC50B                                                          
         CLI   TGCTEQU,CTYMUS      AND COMMERCIAL TYPE IS MUSIC                 
         BNE   BREC50B                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TAMCELQ      READ ALL MUSIC CONTRACT DETAILS              
         BAS   RE,GETEL            ELEMENTS                                     
         B     *+8                                                              
BREC50A  BAS   RE,NEXTEL                                                        
         BNE   BREC50B                                                          
         CLC   TAMCCON,SICNID      IF CONTRACT NUMBER MATCHES NEW               
         BNE   BREC50A             ID                                           
         XC    TAMCCON,TAMCCON     CLEAR CONTRACT NUMBER                        
         OI    TAMCSTAT,TAMCSNEW   AND SET CONFORMS TO NEW MUSIC RULES          
         B     BREC50A                                                          
         DROP  R4                                                               
                                                                                
BREC50B  GOTO1 ACTVIN,DMCB,0         LAST CHANGED                               
*                                                                               
         L     RE,ASVPBLK            IF NEED TO ADD THE RECORD                  
         OC    0(255,RE),0(RE)       DO SO NOW                                  
         BNZ   BREC70                                                           
*                                                                               
         L     R4,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLDRKEY),0(R4)                                             
         OI    DMINBTS,X'08'                                                    
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
*                                                                               
         CLC   KEY(L'TLDRKEY),KEYSAVE                                           
         BNE   BREC60                                                           
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     BREC70                                                           
*                                                                               
BREC60   GOTO1 ADDREC                                                           
         GOTO1 ADDPTRS,DMCB,(X'28',ASVPBLK),AADPBLK                             
         B     BREC80                                                           
*                                                                               
BREC70   GOTO1 PUTREC                ELSE, JUST CHANGE IT                       
         GOTO1 ADDPTRS,DMCB,(X'88',ASVPBLK),AADPBLK                             
*                                                                               
         USING TLCOD,R3                                                         
BREC80   LA    R3,KEY                                                           
         L     R4,AIO                                                           
*                                                                               
BREC90   CLI   SVCOKEY+TLCOVER-TLCOD,TLCOV250  DONE IF ALL COMM'L RECS          
         BE    BREC100                         HAVE BEEN UPDATED                
*                                                                               
         MVC   KEY,SVCOKEY          READ FOR NEXT COMMERCIAL RECORD             
         ZIC   RE,TLCOVER                                                       
         AHI   RE,1                                                             
         STC   RE,TLCOVER                                                       
         MVC   SVCOKEY(L'TLCOKEY),TLCOKEY                                       
         GOTO1 HIGH                                                             
         CLC   TLCOKEY,KEYSAVE                                                  
         BNE   BREC90                                                           
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                               IF FOUND ...                
         GOTO1 SAVPTRS,DMCB,ASVPBLK                 SAVE INITIAL PTRS           
         MVC   TLCOCID-TLCOD(L'TLCOCID,R4),SICNID   CHANGE ID                   
         GOTO1 PUTREC                               PUT RECORD                  
         GOTO1 ADDPTRS,DMCB,(X'88',ASVPBLK),AADPBLK AND UPDATE POINTERS         
         B     BREC90                                                           
         DROP  R3                                                               
*                                                                               
         USING TLCMD,R3                                                         
BREC100  XC    TLCMKEY,TLCMKEY     READ ALL COMMENTS FOR COMMERCIAL             
         MVI   TLCMCD,TLCMCDQ                                                   
         MVC   TLCMAGY,TGAGY                                                    
         MVI   TLCMTYP,TLCMTCOM                                                 
         MVC   TLCMCID,SICOID                                                   
         OC    TLCMCID,SPACES                                                   
         MVC   TLCMICOM,TGCOM                                                   
BREC110  GOTO1 HIGH                                                             
         CLC   KEY(TLCMVER-TLCMD),KEYSAVE                                       
         JNE   BREC140                                                          
         MVC   SVCMKEY,KEY                                                      
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
                                                                                
         USING TLCMD,R4                                                         
         L     R4,AIO              DELETE ORIGINAL COMMENT RECORD               
         OI    TLCMSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         DROP  R4                                                               
                                                                                
         USING TLDRD,R3                                                         
         OI    TLDRSTAT,X'80'      DELETE ORIGINAL COMMENT KEY                  
         GOTO1 WRITE                                                            
         DROP  R3                                                               
                                                                                
         USING TLCMD,R4                                                         
         MVC   TLCMKEY,SVCMKEY     BUILD NEW COMMENT KEY                        
         MVC   TLCMCID,SICNID                                                   
         NI    TLCMSTAT,X'FF'-X'80'                                             
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLDRKEY),0(R4)                                             
         OI    DMINBTS,X'08'                                                    
         MVI   RDUPDATE,C'Y'      CHECK IF COMMENT EXISTS DELETED               
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08'                                              
         CLC   KEY(L'TLDRKEY),KEYSAVE                                           
         BNE   BREC120                                                          
                                                                                
         USING TLDRD,R3                                                         
         NI    TLDRSTAT,X'FF'-X'80' IF SO, RESTORE ORIGINAL COMMENT             
         GOTO1 WRITE                KEY                                         
         DROP  R3                                                               
                                                                                
         MVI   RDUPDATE,C'Y'                                                    
         MVC   AIO,AIO2           AND WRITE BACK RECORD                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC                                                           
         B     BREC130                                                          
                                                                                
BREC120  GOTO1 ADDREC             IF NOT, ADD IT                                
                                                                                
BREC130  MVC   KEY,SVCMKEY        RESTORE COMMENT READ SEQUENCE                 
         J     BREC110                                                          
                                                                                
BREC140  NI    SICOIDH+4,X'DF'     FORCE READ OF NEXT CHANGE                    
         MVI   IOOPT,C'Y'                                                       
         B     CIDCHG                                                           
         EJECT                                                                  
*                                                                               
EXISTERR MVI   ERROR,RECEXIST      RECORD ALREADY EXIST                         
         LA    R2,SICNIDH                                                       
         B     ERRXIT                                                           
*                                                                               
WEBERR   MVC   MYMSGNO,=Y(ERUSEWEB)                                             
         J     ERREND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       RECORD MUST BE UPDATED FROM                  
         OI    GENSTAT2,USGETTXT   WEB APPLICATION                              
         B     ERRXIT                                                           
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         LA    R2,SICNIDH                                                       
         B     ERRXIT                                                           
*                                                                               
CIDCHG   MVI   MYMSGNO1,48                                                      
         B     INFXIT                                                           
*                                                                               
INFXIT   LA    R2,CONRECH                                                       
         OI    GENSTAT2,USGETTXT                                                
*                                                                               
ERRXIT   XC    DMCB,DMCB                                                        
         GOTO1 EXIT,DMCB                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 3                                                                
         EJECT                                                                  
       ++INCLUDE TAMQHFR                                                        
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR20D                                                       
         EJECT                                                                  
         SPACE 3                                                                
         ORG   SICWORK                                                          
*                                                                               
SVCOM    DS    XL4                 LAST COMMERCIAL NUMBER                       
SVCOKEY  DS    XL(L'KEY)           SAVED INITIAL COMMERCIAL KEY                 
SVCMKEY  DS    XL(L'KEY)           SAVED COMMENT KEY                            
CHGHLD   DS    XL1                 TURN ON CHANGED SINCE LAST HF STAT?          
*                                                                               
ASVPBLK  DS    A                   A(SAVED PTR BLOCK)                           
AADPBLK  DS    A                   A(ADD PTR BLOCK)                             
         EJECT                                                                  
*              DSECT TO COVER TEMPORARY STORAGE                                 
         SPACE                                                                  
TMPD     DSECT                                                                  
SVPTRBLK DS    CL((520*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS                      
ADPTRBLK DS    CL((520*L'TLDRREC)+1) PASSIVE + ACTIVE PTRS FOR ADDPTRS          
TMPLNQ   EQU   *-TMPD                                                           
         SPACE 3                                                                
* TAGENWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE TASYSDSECT                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058TAGEN20   10/10/14'                                      
         END                                                                    
