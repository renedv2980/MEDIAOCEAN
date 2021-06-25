*          DATA SET TAGEN7F    AT LEVEL 044 AS OF 07/20/12                      
*PHASE T7027FC,*                                                                
         TITLE 'T7027F - ADVICE COPY'                                           
T7027F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7027F,R7                                                      
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         SPACE 3                                                                
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         IF VALIDATE KEY                              
         BNE   MAIN20                                                           
         GOTO1 FLDVAL,DMCB,(X'40',AFRSTKEY),999  IF ALL FLDS VALID              
         BNE   *+12                                                             
         TM    STATUS,PFKPEND      AND PFKEY IS NOT PENDING                     
         BO    *+12                                                             
         BAS   RE,VKEY             VALIDATE KEY                                 
         B     XIT                                                              
*                                                                               
         CLI   PFAID,13            ELSE, IF NOT PF13                            
         BE    XIT                                                              
         B     INFMSG              CONTINUE GIVING MESSAGE                      
         SPACE 1                                                                
MAIN20   CLI   MODE,VALREC         COPY RECORD                                  
         BNE   XIT                                                              
         BAS   RE,COPY                                                          
         B     COPIED              GIVE COMPLETION MESSAGE                      
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         BAS   RE,VFRMKEY          VALIDATE FROM DETAILS                        
         SPACE 2                                                                
         BAS   RE,VTOKEY           VALIDATE TO DETAILS                          
         SPACE 2                                                                
         TM    AGYSTA3,TAAYSADV    IF ADVICE NUMBER INPUT                       
         BO    VK20                                                             
         MVC   TGCID,FROMCID       MAKE SURE NEW ADVICE NOT ON FILE             
         OC    TOCID,TOCID                                                      
         BZ    *+10                                                             
         MVC   TGCID,TOCID                                                      
         MVC   TGADV,TOADV                                                      
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'24',0)                                    
         BE    ONFILEDV            ERROR RECORD ALREADY EXISTS                  
         SPACE 2                                                                
VK20     OI    STATUS,PFKPEND      SET PF13 PENDING                             
         GOTO1 FLDVAL,DMCB,(X'20',AFRSTKEY),999 MAKE ALL FLDS VALID             
         B     INFMSG                                                           
         EJECT                                                                  
*              ROUTINE TO VALIDATE FROM KEY FIELDS                              
         SPACE 1                                                                
VFRMKEY  NTR1                                                                   
         LA    R2,SDVFAYH                                                       
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',SDVFAYH),SDVFAYNH                     
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         L     R4,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R4                                                         
         MVC   AGYSTA3,TAAYSTA3    SAVE 3RD AGENCY STATUS                       
*                                                                               
         LA    R2,SDVFCIDH         R2=A(FROM COMML ID FIELD)                    
         XC    SDVFCDN,SDVFCDN     PRECLEAR COMML NAME                          
         OI    SDVFCDNH+6,X'80'                                                 
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'2C',SDVFCIDH),SDVFCDNH                   
         BE    *+12                                                             
         CLI   ERROR,NOTFOUND                                                   
         BNE   THEEND                                                           
         MVC   FROMCID,TGCID       SAVE FROM CID                                
*                                                                               
         LA    R2,SDVFDVH          R2=A(FROM ADVICE NUMBER FIELD)               
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'20',(R2))                                 
         L     R4,AIO                                                           
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R4                                                         
         CLI   TADVTYPE,TADVTYPS   ONLY SESSIONS VALID FOR THIS ACTION          
         BNE   ERRADV                                                           
         MVC   FROMADV,TGADV       SAVE FROM ADVICE                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE TO KEY FIELDS                                
         SPACE 1                                                                
VTOKEY   NTR1                                                                   
         XC    TODTLS(TODLNQ),TODTLS                                            
*                                                                               
         LA    R2,SDVTDVH          R2=A(TO ADVICE FIELD)                        
         TM    AGYSTA3,TAAYSADV    IF ADVICE NUMBERS AUTO GENERATED             
         BZ    *+16                                                             
         CLI   5(R2),0             NO INPUT ALLOWED                             
         BNE   FLDINV                                                           
         B     VTOKEY5                                                          
         GOTO1 ANY                 ELSE, INPUT REQUIRED                         
         MVC   TOADV,WORK          SET TO ADVICE NUMBER                         
         SPACE 1                                                                
VTOKEY5  LA    R2,SDVTCIDH         R2=A(TO COMML ID FIELD)                      
         CLI   5(R2),0                                                          
         BE    VTOKEY10                                                         
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'24',(R2))                                
         BE    ONFILE                                                           
         CLI   ERROR,NOTFOUND      COMMERCIAL RECORD MUST NOT EXIST             
         BNE   THEEND                                                           
         MVC   TOCID,TGCID         SET TO CID                                   
         SPACE 1                                                                
VTOKEY10 LA    R2,SDVTTTLH         R2=A(TO TITLE FIELD)                         
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   *+12                                                             
         BAS   RE,CHKREQ           CHECK INPUT IN FIELD REQUIRED                
         B     VTOKEY20                                                         
         BAS   RE,CHKALLOW         ELSE,CHECK IF INPUT IN FIELD ALLOWED         
         GOTO1 ANY                                                              
         MVC   TOTTL,WORK                                                       
         SPACE 1                                                                
VTOKEY20 LA    R2,SDVTSECH         R2=A(TO LENGTH FIELD)                        
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   *+12                                                             
         BAS   RE,CHKREQ           CHECK INPUT IN FIELD REQUIRED                
         B     VTOKEYX                                                          
         BAS   RE,CHKALLOW         ELSE,CHECK IF INPUT IN FIELD ALLOWED         
         GOTO1 VALINUM                                                          
         MVC   TOSEC,ACTUAL                                                     
*                                                                               
VTOKEYX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINES CALLED FROM VTOKEY                                      
         SPACE 2                                                                
CHKALLOW DS    0H                  ROUTINE RETURNS IF INP ALLOWED               
         OC    TOCID,TOCID         IF TO CID NOT SPECIFIED                      
         BZ    NOINPERR            THEN NO INPUT REQUIRED                       
         BR    RE                  ELSE, RETURN (INPUT ALLOWED)                 
         SPACE 2                                                                
CHKREQ   DS    0H                  ROUTINE RETURNS IF INP NOT REQUIRED          
         OC    TOCID,TOCID         IF TO CID SPECIFIED                          
         BNZ   MISSERR             THEN FIELD REQUIRED                          
         BR    RE                  ELSE RETURN (INPUT NOT REQUIRED)             
         EJECT                                                                  
*              COPY TO NEW ADVICE RECORD                                        
         SPACE 1                                                                
COPY     NTR1                                                                   
         MVC   TGCID,FROMCID       READ ADVICE RECORD TO COPY                   
         MVC   TGADV,FROMADV                                                    
         GOTO1 RECVAL,DMCB,TLDVCDQ,(X'20',0)                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,GETADV           GENERATE ADVICE NUMBER IF NECESSARY          
*                                  DISPLAYS AND RETURNS TOADV                   
         L     R6,AIO                                                           
         USING TLDVD,R6                                                         
         MVC   TLDVCID,TGCID       SET 'FROM CID'                               
         OC    TOCID,TOCID         IF 'TO CID' SPECIFIED                        
         BZ    *+10                                                             
         MVC   TLDVCID,TOCID       USE IT                                       
         MVC   TLDVADV,TOADV       SET 'TO ADV'                                 
*                                                                               
         BAS   RE,SETDVEL          SET ADVICE DETAILS INFO                      
*                                  RETURNS TYPE                                 
         MVI   ELCODE,TAAIELQ                                                   
         GOTO1 REMELEM                                                          
*                                                                               
         OC    TOTTL,TOTTL         IF 'TO TITLE' SPECIFIED                      
         BZ    COPY40                                                           
         MVI   BLOCK,8+L'TOTTL     SET UP DUMMY SCREEN HEADER                   
         MVI   BLOCK+5,L'TOTTL                                                  
         MVC   BLOCK+8(L'TOTTL),TOTTL                                           
         GOTO1 NAMIN,DMCB,TAFNELQ,BLOCK,TAFNTTTL                                
         SPACE 1                                                                
COPY40   GOTO1 ACTVIN,DMCB,0       ADD NEW ACTIVITY RECORD                      
         BAS   RE,MYADDREC         ADD THE ADVICE RECORD TO THE FILE            
         SPACE 1                                                                
         CLI   TYPE,TADVTYPS       IF SESSION ADVICE                            
         BNE   *+8                                                              
         BAS   RE,COPYDC           ALSO COPY ADVICE CAST REC(S)                 
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO GET NEXT ADVICE NUMBER IF AGENCY                      
*              IS ON AUTOMATIC ADVICE NUMBERS                                   
         SPACE 1                                                                
GETADV   NTR1                                                                   
         TM    AGYSTA3,TAAYSADV    IF ADVICE NUMBERS AUTO GENERATED             
         BZ    GETADVX                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 CHNADV              GET NEXT ADVICE NUM FROM AGY NOW             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         MVC   TOADV,TGADV         SET TO ADVICE NUMBER                         
         MVC   SDVTDV,TGADV        DISPLAY NEW ADVICE NUMBER                    
         MVI   SDVTDVH+5,L'TGADV   SET LENGTH                                   
         NI    SDVTDVH+4,X'DF'     SET TO VALIDATE NEXT TIME IN                 
         OI    SDVTDVH+6,X'80'     AND TRANSMIT                                 
*                                                                               
GETADVX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET INFO IN ADVICE DETAILS ELEMENT                    
         SPACE 1                                                                
SETDVEL  NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R4            R4=A(ADVICE DETAILS ELEMENT)                 
         SPACE 1                                                                
         MVC   TYPE,TADVTYPE       SAVE ADVICE TYPE FOR LATER CHECK             
         SPACE 1                                                                
         OC    TOSEC,TOSEC         IF 'TO LENGTH' SPECIFIED                     
         BZ    *+10                                                             
         MVC   TADVSEC,TOSEC       USE IT                                       
         SPACE 1                                                                
         OC    TOCID,TOCID         IF 'TO CID' SPECIFIED                        
         BZ    *+8                                                              
         MVI   TADVOFF2,0          CLEAR SECOND OFFICE CODE                     
         SPACE 1                                                                
         MVI   TADVSTAT,0          CLEAR APPROPRIATE STATUS INFO                
         XC    TADVVINF,TADVVINF                                                
         XC    TADVSINF,TADVSINF                                                
         XC    TADVRINF,TADVRINF                                                
         XC    TADVPINF,TADVPINF                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS ADDING RECORDS (TLDV & TLDC) TO FILE            
         SPACE                                                                  
MYADDREC NTR1                                                                   
         L     R6,AIO              R6=A(RECORD TO ADD)                          
         MVC   KEY,0(R6)           SEE IF RECORD ON FILE MARKED DELETED         
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       AND FOR UPDATE                               
         GOTO1 HIGH                LOOK FOR RECORD ALREADY ON FILE              
         SPACE 1                                                                
         LA    R4,KEY              R4=A(DIRECTORY RECORD)                       
         USING TLDRD,R4                                                         
         CLC   TLDRKEY,KEYSAVE     TEST WE FOUND RECORD                         
         BNE   MYADR5                                                           
         TM    TLDRSTAT,X'80'      IT HAD BETTER BE DELETED                     
         BO    *+6                                                              
         DC    H'0'                                                             
         NI    TLDRSTAT,X'7F'      TURN OFF DELETE BIT                          
         GOTO1 WRITE               AND WRITE IT BACK                            
*                                                                               
         MVC   AIO,AIO3            NOW SET TO GET THE RECORD                    
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD SO THAT WE CAN                
         GOTO1 SAVPTRS,DMCB,PTRBLK                                              
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              WRITE NEW ONE BACK OVER DELETED ONE          
         GOTO1 ADDPTRS,DMCB,PTRBLK                                              
         B     MYADRX                                                           
*                                                                               
MYADR5   MVC   TLDRKEY,KEYSAVE     OKAY TO ADD THE RECORD                       
         GOTO1 ADDREC                                                           
         LA    R2,PTRBLK                                                        
         XC    0(L'TLDRREC+1,R2),0(R2)                                          
         GOTO1 ADDPTRS,DMCB,(R2)                                                
*                                                                               
MYADRX   NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO COPY ADVICE CAST RECORDS                              
*              ASSOCIATED WITH ADVICE RECORD                                    
         SPACE 1                                                                
COPYDC   NTR1                                                                   
         MVC   TGCID,FROMCID       READ HIGH FOR DC RECORD TO COPY              
         MVC   TGADV,FROMADV                                                    
         GOTO1 RECVAL,DMCB,TLDCCDQ,0                                            
         B     COPYDC8                                                          
*                                                                               
COPYDC5  GOTO1 SEQ                                                              
COPYDC8  CLC   KEY(TLDCSEQ-TLDCKEY),KEYSAVE                                     
         BNE   COPYDCX                                                          
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY FOR READ SEQUENCE                   
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,AIO                                                           
         USING TLDCD,R4                                                         
         MVI   TLDCCD,TLDCCDQ      SET NEW RECORD TYPE                          
         MVC   TLDCCID,TGCID       SET 'FROM CID'                               
         OC    TOCID,TOCID         IF 'TO CID' SPECIFIED                        
         BZ    *+10                                                             
         MVC   TLDCCID,TOCID       USE IT                                       
         MVC   TLDCADV,TOADV       SET 'TO ADV'                                 
         BAS   RE,MYADDREC         ADD AN ADVICE CAST RECORD                    
*                                                                               
         MVC   KEY,SVKEY           REREAD 'FROM' RECORD JUST COPIED             
         GOTO1 HIGH                                                             
         B     COPYDC5             SEE IF ANY MORE TO ADD                       
*                                                                               
COPYDCX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 1                                                                
ONFILEDV LA    R2,SDVTDVH          TO ADVICE FIELD                              
ONFILE   MVI   ERROR,RECEXIST      RECORD ALREADY EXISTS                        
         B     THEEND                                                           
         SPACE 1                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
MISSERR  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
NOINPERR MVI   ERROR,ERNOINP       NO INPUT ALLOWED                             
         B     THEEND                                                           
         SPACE 1                                                                
ERRADV   MVC   MYMSGNO,=Y(ERREUSE) ADVICE NOT A SESSION                         
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         B     INFEND                                                           
         SPACE 1                                                                
INFMSG   MVI   MYMSGNO1,35         PRESS PF13 TO MAKE CHANGES                   
         LA    R2,CONRECH                                                       
         B     INFEND                                                           
         SPACE 1                                                                
COPIED   MVI   MYMSGNO1,97         SET COMPLETION MESSAGE                       
         LA    R2,CONRECH                                                       
         B     INFEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPP2)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT   SET INFO MESSAGE                             
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC               SET CONDITION CODE                           
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR7FD                                                       
*                                                                               
         ORG   SDVWORK                                                          
*                                                                               
AGYSTA3  DS    XL1                 THIRD AGENCY STATUS BYTE                     
STATUS   DS    XL1                 STATUS BYTE                                  
PFKPEND  EQU   X'80'               PFKEY CONFIRMATION PENDING                   
TYPE     DS    XL1                 ADVICE TYPE                                  
*                                                                               
FROMDTLS DS    0C                  FROM DETAILS                                 
FROMCID  DS    CL12                CID                                          
FROMADV  DS    CL6                 ADVICE NUMBER                                
FROMDLNQ EQU   (*-FROMDTLS)                                                     
*                                                                               
TODTLS   DS    0C                  TO DETAILS                                   
TOADV    DS    CL6                 ADVICE NUMBER                                
TOCID    DS    CL12                CID                                          
TOTTL    DS    CL36                TITLE                                        
TOSEC    DS    XL1                 LENGTH IN SECONDS                            
TODLNQ   EQU   (*-TODTLS)                                                       
*                                                                               
SVKEY    DS    CL(L'KEY)           SAVED KEY                                    
PTRBLK   DS    CL((10*L'TLDRREC)+1) ACTIVE + 2 PASSIVE'S                        
         DS    0X                                                               
         SPACE 3                                                                
         EJECT                                                                  
* TASYSIOD   (MUST FOLLOW LAST SCREEN)                                          
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044TAGEN7F   07/20/12'                                      
         END                                                                    
