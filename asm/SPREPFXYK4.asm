*          DATA SET SPREPFXYK4 AT LEVEL 140 AS OF 05/01/02                      
*PHASE SPFX02Y4                                                                 
SPFX02YK TITLE 'SPFX02YK - DEL UNMATCHED ESTS IN STALOCK RECS FOR CLT'          
         SPACE 1                                                                
SPFX02   CSECT                                                                  
         DS    6000C                                                            
         ORG   SPFX02                                                           
         SPACE 1                                                                
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02,RB,RC                                                     
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX2                                                              
*                                                                               
YES      CR    RB,RB               SET CC EQUAL                                 
         B     XIT                                                              
*                                                                               
NO       LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
*=========================================================*                     
* CLTFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FX2      DS    0H                  CLEAR COUNTERS                               
         XC    SVEST,SVEST                                                      
         XC    SVSEQ,SVSEQ                                                      
         XC    FLAG,FLAG                                                        
         ZAP   NOCHANGE,=P'0'                                                   
         ZAP   CHANGED,=P'0'                                                    
         ZAP   DELETED,=P'0'                                                    
         ZAP   NOEST,=P'0'                                                      
         ZAP   ESTOUT,=P'0'                                                     
*                                                                               
         MVC   P(33),=C'HERE IS THE CLT SPONSOR RETURNED'                       
         GOTO1 CLUNPK,DMCB,BCLT,P+40                                            
         GOTO1 REPORT                                                           
*                                                                               
FX100    LA    R1,IO               GET REC IN MY IO                             
         ST    R1,AREC                                                          
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING SLHRECD,R6          GET 1ST LOKIN HDR KEY                        
         MVI   SLHKTYP,SLHKTYPQ                                                 
         MVI   SLHKSUB,SLHKSUBQ                                                 
         MVC   SLHKAGMD,BAGYMD     GET AGY & MEDIA FROM REQ CARD                
*        CLC   QCLT,=C'ALL'                                                     
*        BE    CLTERR                                                           
*        CLC   QCLT,=C'   '                                                     
*        BE    CLTERR              FOR THIS PROG CLT IS REQUIRED                
         MVC   SLHKCLT,BCLT                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   FXIT                                                             
         MVC   SVSLHKEY,KEY        SAVE LOCKIN HDR KEY                          
         BAS   RE,BLDESTB          BUILD EST TABLE FOR GIVEN CLIENT             
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVSLHKEY                                                 
         GOTO1 HIGH                DUMMY HIGH                                   
         CLC   KEY(13),KEYSAVE                                                  
         BE    FX110                                                            
         DC    H'0'                                                             
*                                                                               
FX105    GOTO1 SEQ                                                              
         CLC   KEY(5),KEYSAVE                                                   
         BNE   PRTTOT              NO MORE STALOCK  HDR RECS                    
*                                                                               
FX110    MVC   SVSLHKEY,KEY        SAVE LOCKIN HDR KEY                          
         LA    R6,KEY                                                           
         CLC   SVSEQ,SLHKSEQ                                                    
         BNE   *+6                                                              
         DC    H'0'                SEQ# HAD TO CHANGE                           
         MVC   SVSEQ,SLHKSEQ                                                    
*                                                                               
FX112    LA    R6,KEY                                                           
         XC    KEY,KEY             BIULD KEY FOR LOCKIN REC                     
         USING SLKRECD,R6                                                       
         MVI   SLKKTYP,SLKKTYPQ                                                 
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,BAGYMD                                                  
         MVC   SLKKSEQ,SVSEQ                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    FX120                                                            
         MVC   P(10),=C'*****SEQ #'                                             
         EDIT  SVSEQ,(6,P+11),FILL=0            SEQ #                           
         MVC   P+19(10),=C'FOR CLIENT'                                          
         GOTO1 CLUNPK,DMCB,BCLT,P+30            CLIENT                          
         MVC   P+35(42),=C'EXISTS IN HDR REC, BUT NOT IN LOKIN RECORD'          
         GOTO1 REPORT                                                           
         MVC   KEY(13),SVSLHKEY                                                 
         GOTO1 HIGH                                                             
         B     FX105                                                            
*                                                                               
FX115    GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    FX120               IF SEQ# CHANGED, GET NXT HDR REC             
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVSLHKEY                                                 
         GOTO1 HIGH                                                             
         B     FX105                                                            
*                                                                               
FX120    MVC   SVSLKKEY,KEY        REMEBER LOCKIN REC KEY                       
         LA    R6,KEY                                                           
         MVC   SVEST,SLKKEST       REMEBER EST FROM LOCKIN REC                  
         BAS   RE,MATCHEST                                                      
         BE    FX122                                                            
         AP    NOEST,=P'1'                                                      
         B     DELREC                                                           
*                                                                               
FX122    GOTO1 GET                                                              
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL            FIND WEEKLY LCKIN ELEM                       
         BE    FX125                                                            
         MVC   P(5),=C'!!!!!'                                                   
         MVC   P+5(43),=C'FOLLOWING REC DOES NOT HAVE WKLY LOKIN ELEM'          
         GOTO1 HEXOUT,DMCB,IO,P2+5,13,0                                         
         GOTO1 REPORT                                                           
         B     FX115                                                            
*                                                                               
FX125    L     R3,ESTBPTR          PICK UP WHER LEFT OFF                        
         USING ESTD,R3                                                          
         USING LOKEL,R6                                                         
*                                                                               
FX127    CLC   LOKWEEK,SDATE                                                    
         BL    MISMATCH                                                         
         CLC   LOKWEEK,EDATE                                                    
         BH    MISMATCH                                                         
         DROP  R3                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    FX127                                                            
         B     FX130                                                            
*                                                                               
MISMATCH DS    0H                                                               
         OI    FLAG,X'80'          SET RECORD UPDATE                            
         GOTO1 RECUP,DMCB,IO,(R6)  DELETE THIS 03 ELEM                          
         CLI   0(R6),X'03'                                                      
         BE    FX127                                                            
*                                                                               
FX130    TM    FLAG,X'80'                                                       
         BNZ   *+14                                                             
         AP    NOCHANGE,=P'1'                                                   
         B     FX115                                                            
         AP    ESTOUT,=P'1'                                                     
         LA    R6,IO               CHECK IF ANY 03 ELEMS LEFT                   
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL            FIND WEEKLY LCKIN ELEM                       
         BNE   DELREC              ALL DELETED? - DELETE REC                    
         OI    FLAG,X'40'          REC WAS CHANGED ?                            
         AP    CHANGED,=P'1'                                                    
         B     DELREC                                                           
*                                                                               
*                                                                               
PRTTOT   DS    0H                                                               
         MVC   P(8),=C'NOCHANGE'                                                
         OI    NOCHANGE+3,X'0F'                                                 
         UNPK  P+10(7),NOCHANGE                                                 
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(7),=C'CHANGED'                                                 
         OI    CHANGED+3,X'0F'                                                  
         UNPK  P+10(7),CHANGED                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(7),=C'DELETED'                                                 
         OI    DELETED+3,X'0F'                                                  
         UNPK  P+10(7),DELETED                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(5),=C'NOEST'                                                   
         OI    NOEST+3,X'0F'                                                    
         UNPK  P+10(7),NOEST                                                    
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(6),=C'ESTOUT'                                                  
         OI    ESTOUT+3,X'0F'                                                   
         UNPK  P+10(7),ESTOUT                                                   
         GOTO1 REPORT                                                           
*                                                                               
*                                                                               
FXIT     MVC   P(4),=C'DONE'                                                    
         GOTO1 REPORT                                                           
**** THIS IS NOT TO BE CALLED WITH OTHER THEN CLTFIRST MODES ****               
         MVI   MODE,CLTLAST                                                     
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
CLTERR   MVC   P(41),=C'CLIENT HAS TO BE DEFINED FOR THIS PROGRAM'              
         GOTO1 REPORT                                                           
         B     FXIT                                                             
*                                                                               
*                                                                               
*********************************************************************           
* BUILD TABLE : EST(1),SDATE(2),EDATE(2) FOR CLIENT FROM LOCKIN REC *           
*********************************************************************           
*                                                                               
BLDESTB  NTR1                                                                   
*                                                                               
         LA    R0,5                                                             
         LA    R3,ESTTBL                                                        
BLDEST1  XC    0(256,R3),0(R3)     CLEAR ESTTBL                                 
         LA    R3,256(R3)                                                       
         BCT   R0,BLDEST1                                                       
*                                                                               
         LA    R3,ESTTBL                                                        
         USING ESTD,R3                                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTHDRD,R6          BUILD EST REC KEY                            
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,BCLT                                                     
         MVC   EKEYPRD,=C'POL'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    BLDEST10            THERE HAS TO BE REC WITH THIS CLT            
         DC    H'0'                                                             
*                                                                               
BLDEST5  GOTO1 SEQ                                                              
*        GOTO1 HEXOUT,DMCB,KEY,P,13,0                                           
         CLC   KEY(7),KEYSAVE                                                   
         BE    BLDEST10                                                         
         MVI   ESTMT,X'FF'         MARK END OF ESTTBL                           
         B     BLDESTX             IF CLT CHANGED, EXIT                         
*                                                                               
BLDEST10 LA    R6,KEY                                                           
         CLI   EKEYEST,0                                                        
         BE    BLDEST5                                                          
         OC    KEY+8(5),KEY+8                                                   
         BNZ   BLDEST5                                                          
*        GOTO1 HEXOUT,DMCB,KEY,P,13,0                                           
*        GOTO1 REPORT                                                           
         MVC   ESTMT,EKEYEST                                                    
         GOTO1 GET                 GET REC FOR S/E DATES                        
*                                                                               
         LA    R6,IO                                                            
         GOTO1 DATCON,DMCB,(0,ESTART),(2,SDATE)   PUT ST DATE IN TAB            
         GOTO1 DATCON,DMCB,(0,EEND),(2,EDATE)     PUT END DATE IN TAB           
         LA    R3,ESTDLQ(R3)       MOVE TO NXT TBL ENTRY                        
         B     BLDEST5                                                          
         DROP  R3                                                               
         DROP  R6                                                               
*                                                                               
BLDESTX  XIT1                                                                   
*                                                                               
*********************************************************************           
*           CHECK EST FROM LOCKIN REC AGAINS ESTTBL                 *           
*********************************************************************           
*                                                                               
MATCHEST NTR1                                                                   
*                                                                               
         LA    R3,ESTTBL                                                        
         USING ESTD,R3                                                          
*                                                                               
COMPEST  CLC   SVEST,ESTMT         TRY TO FIND LOCKIN EST IN ESTTBL             
         BE    ESTFOUND                                                         
         LA    R3,ESTDLQ(R3)                                                    
         CLI   ESTMT,X'FF'                                                      
         BNE   COMPEST                                                          
*                                                                               
*        ZIC   R4,SVEST                                                         
*        LA    R5,TMPESTB2(R4)                                                  
*        CLI   0(R5),0                                                          
*        BNE   NOMATCH                                                          
*        MVC   0(1,R5),SVEST                                                    
NOMATCH  B     NO                  SET CC NEQ AND EXIT                          
*                                                                               
ESTFOUND ST    R3,ESTBPTR                                                       
         B     YES                 SET CC = AND EXIT                            
         DROP  R3                                                               
*                                                                               
*********************************************************************           
*                   DEL LOCKIN REC                                  *           
*********************************************************************           
*                                                                               
DELREC   DS    0H                                                               
*                                                                               
         AP    PRTCNTR,=P'1'       DON'T PRINT MORE THAN 1000 LINES             
*                                                                               
         TM    FLAG,X'40'                                                       
         BZ    *+14                                                             
         MVC   P(7),=C'CHANGED'                                                 
         B     DELREC5                                                          
         OI    KEY+13,X'80'        SET DIRECTORY DELETED                        
         GOTO1 WRITE                                                            
         AP    DELETED,=P'1'                                                    
         MVC   P(7),=C'DELETED'                                                 
DELREC5  CP    PRTCNTR,=P'100'                                                  
         BH    DELREC10                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+10,14,0                                        
         GOTO1 REPORT                                                           
*                                                                               
DELREC10 TM    FLAG,X'80'                                                       
         BZ    FX115                                                            
         NI    FLAG,X'FF'-X'80'                                                 
         TM    FLAG,X'40'                                                       
         BZ    *+12                                                             
         NI    FLAG,X'FF'-X'40'                                                 
         B     FX115                                                            
         LA    R6,IO                                                            
         OI    15(R6),X'80'                                                     
         GOTO1 PUT                                                              
         CP    PRTCNTR,=P'100'                                                  
         BH    FX115                                                            
         GOTO1 HEXOUT,DMCB,(R6),P+10,16,0                                       
         GOTO1 REPORT                                                           
         B     FX115                                                            
*                                                                               
*                                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
* COUNTERS                                                                      
NOCHANGE DC    PL4'0'                                                           
CHANGED  DC    PL4'0'                                                           
DELETED  DC    PL4'0'                                                           
NOEST    DC    PL4'0'                                                           
ESTOUT   DC    PL4'0'                                                           
PRTCNTR  DC    PL4'0'                                                           
*                                                                               
ESTBPTR  DS    F                   PTR WHERE LEFT OFF IN ESTTBL                 
SVSLHKEY DS    XL13                SAVE LOCKIN HDR KEY                          
SVSLKKEY DS    XL13                SAVE LOCKIN REC KEY                          
SVSEQ    DS    XL3                                                              
TEMPDATE DS    XL2                                                              
ELCODE   DS    XL1                                                              
FLAG     DS    XL1                                                              
ESTTBL   DS    XL1280              EST/SDATE/EDATE(COMPRESSED)                  
ESTTBLX  DS    X                                                                
         DS    D                                                                
IO       DS    CL2000                                                           
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE DDCOMFACSD                                                     
         PRINT ON                                                               
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
       ++INCLUDE SPGENSLH                                                       
       ++INCLUDE SPGENSLK                                                       
*                                                                               
ESTD     DSECT                  COVERS ESTTBL                                   
ESTMT    DS    X                                                                
SDATE    DS    XL2                                                              
EDATE    DS    XL2                                                              
ESTDLQ   EQU   *-ESTD                                                           
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'140SPREPFXYK405/01/02'                                      
         END                                                                    
