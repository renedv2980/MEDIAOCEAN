*          DATA SET SPREPFXYK1 AT LEVEL 114 AS OF 05/01/02                      
*PHASE SPFX02YK                                                                 
SPFX02YK TITLE 'SPFX02YK - FIND UNMATCHED ESTS IN STALOCK RECS'                 
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
         CLI   MODE,REQFRST                                                     
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
* REQFRST PROCESSING                                      *                     
*=========================================================*                     
         SPACE 1                                                                
FX2      DS    0H                                                               
         XC    SVCLT,SVCLT                                                      
         XC    SVEST,SVEST                                                      
         XC    SVSEQ,SVSEQ                                                      
         XC    FLAG,FLAG                                                        
         XC    TEMPESTB,TEMPESTB                                                
         XC    TMPESTB2,TMPESTB2                                                
*                                                                               
FX100    LA    R1,IO               GET REC IN MY IO                             
         ST    R1,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING SLHRECD,R6          GET 1ST LOKIN HDR KEY                        
         MVI   SLHKTYP,SLHKTYPQ                                                 
         MVI   SLHKSUB,SLHKSUBQ                                                 
         GOTO1 HIGH                                                             
         B     FX110                                                            
*                                                                               
FX105    GOTO1 SEQ                                                              
*                                                                               
FX110    CLC   KEY(2),KEYSAVE                                                   
         BNE   FXIT                NO MORE STALOCK  HDR RECS                    
*                                                                               
         MVC   SVSLHKEY,KEY        SAVE LOCKIN HDR KEY                          
         LA    R6,KEY                                                           
         CLC   SVSEQ,SLHKSEQ                                                    
         BNE   *+6                                                              
         DC    H'0'                SEQ# HAD TO CHANGE                           
         MVC   SVSEQ,SLHKSEQ                                                    
         XC    TEMPESTB,TEMPESTB   CLEAR TEMPESTS TAB FOR NEW SEQ#              
         CLC   SVCLT,SLHKCLT       DID CLT CHANGE ?                             
         BE    FX112                                                            
         BAS   RE,OUTESTS                                                       
         MVC   SVCLT,SLHKCLT       YES, SET CLT                                 
         BAS   RE,BLDESTB          YES, START ESTTBL AGAIN                      
         XC    TMPESTB2,TMPESTB2   TAB OF EST NOT EXISTING IN ESTHDR            
         OI    FLAG,X'80'                                                       
*                                                                               
FX112    LA    R6,KEY                                                           
         XC    KEY,KEY             BIULD KEY FOR LOCKIN REC                     
         USING SLKRECD,R6                                                       
         MVI   SLKKTYP,SLKKTYPQ                                                 
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,SVSLHKEY+2                                              
         MVC   SLKKSEQ,SVSEQ                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    FX120                                                            
         MVC   P(10),=C'*****SEQ #'                                             
         EDIT  SVSEQ,(6,P+11),FILL=0            SEQ #                           
         MVC   P+19(10),=C'FOR CLIENT'                                          
         GOTO1 CLUNPK,DMCB,SVCLT,P+30           CLIENT                          
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
         ZIC   R4,SLKKEST                                                       
         LA    R5,TEMPESTB(R4)                                                  
         CLI   0(R5),0                                                          
         BNE   FX115                                                            
         MVC   0(1,R5),SLKKEST                                                  
         MVC   SVEST,SLKKEST       REMEBER EST FROM LOCKIN REC                  
         BAS   RE,MATCHEST                                                      
         BNE   FX115                                                            
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
         USING LOKEL,R6                                                         
FX125    MVC   TEMPDATE,LOKWEEK                                                 
         BAS   RE,MATCHDAT                                                      
         BNE   PRTREC                                                           
         BAS   RE,NEXTEL                                                        
         BE    FX125                                                            
*                                                                               
*X130    XC    KEY,KEY             NOMORE 03 ELEMS FOUND, GET NXT LOKIN         
*        MVC   KEY(13),SVSLKKEY                                                 
         B     FX115                                                            
*                                                                               
*                                                                               
*                                                                               
FXIT     MVC   P(4),=C'DONE'                                                    
         GOTO1 REPORT                                                           
**** THIS IS NOT TO BE CALLED WITH OTHER MODES ****                             
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*********************************************************************           
* PRINT ESTS FROM LOKIN REC NOT FOUND IN ESTHDR REC FOR SAME CLIENT *           
*********************************************************************           
*                                                                               
OUTESTS  NTR1                                                                   
         SR    R2,R2                                                            
         LA    R5,TMPESTB2                                                      
*                                                                               
         OC    TMPESTB2,TMPESTB2   ANY ESTS NOT EXISTING IN ESTHDR              
         BZ    OUTESTSX                                                         
*                                                                               
OUTESTS2 CLI   0(R5),0                                                          
         BE    OUTESTS6                                                         
         MVC   P+5(8),=C'ESTIMATE'                                              
         EDIT  (R2),(3,P+14),FILL=0                                             
         MVC   P+18(10),=C'FOR CLIENT'                                          
         GOTO1 CLUNPK,DMCB,SVCLT,P+30                                           
         MVC   P2+5(40),=C'NOT FOUND IN ESTHDR RECS FOR THIS CLIENT'            
         GOTO1 REPORT                                                           
OUTESTS6 LA    R5,1(R5)            NXT EST                                      
         AHI   R2,1                                                             
         CHI   R2,256                                                           
         BL    OUTESTS2                                                         
*                                                                               
         GOTO1 REPORT                                                           
OUTESTSX XIT1                                                                   
*                                                                               
         EJECT                                                                  
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
         MVC   EKEYAM,SVSLHKEY+2                                                
         MVC   EKEYCLT,SVSLHKEY+3                                               
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
         ZIC   R4,SVEST                                                         
         LA    R5,TMPESTB2(R4)                                                  
         CLI   0(R5),0                                                          
         BNE   NOMATCH                                                          
         MVC   0(1,R5),SVEST                                                    
NOMATCH  B     NO                  SET CC NEQ AND EXIT                          
*                                                                               
ESTFOUND ST    R3,ESTBPTR                                                       
         B     YES                 SET CC = AND EXIT                            
*                                                                               
*********************************************************************           
*     CHECK EST DATE FROM WEEKLY LOCKIN ELEM AGAINS ESTTBL          *           
*********************************************************************           
*                                                                               
MATCHDAT NTR1                                                                   
*                                                                               
         L     R3,ESTBPTR          PICK UP WHER LEFT OFF                        
         CLC   TEMPDATE,SDATE                                                   
         BL    MISMATCH                                                         
         CLC   TEMPDATE,EDATE                                                   
         BH    MISMATCH                                                         
*                                                                               
         B     YES                                                              
*                                                                               
MISMATCH B     NO                  SET CC NEQ                                   
         DROP  R3                                                               
*                                                                               
*                                                                               
*********************************************************************           
* PRT LOCKIN REC WHERE LOKWEEK IS OUTSIDE ST/END DATES OF ESTHDRREC *           
*********************************************************************           
*                                                                               
PRTREC   DS    0H                                                               
*                                                                               
         TM    FLAG,X'80'                                                       
         BZ    PRTREC5                                                          
         NI    FLAG,X'FF'-X'80'                                                 
         BAS   RE,PRTTOP                                                        
*                                                                               
PRTREC5  GOTO1 HEXOUT,DMCB,SVSLHKEY+2,P,1,0     AGY/MED                         
         GOTO1 CLUNPK,DMCB,SVCLT,P+3            CLIENT                          
         GOTO1 MSUNPK,DMCB,SVSLHKEY+5,P+7,P+12  MKT STA                         
         EDIT  SVSEQ,(6,P+19),FILL=0            SEQ #                           
         EDIT  SVEST,(3,P+27),FILL=0            ESTIMATE                        
         GOTO1 REPORT                                                           
*                                                                               
         B     FX115                            READ NXT LOKIN REC              
*                                                                               
*********************************************************************           
*               PRT LOCKIN REC TBL HEADER                           *           
*********************************************************************           
*                                                                               
PRTTOP   NTR1                                                                   
*                                                                               
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(2),=C'AM'                                                      
         MVC   P+3(3),=C'CLT'                                                   
         MVC   P+8(3),=C'MKT'                                                   
         MVC   P+13(3),=C'STA'                                                  
         MVC   P+20(4),=C'SEQ#'                                                 
         MVC   P+27(3),=C'EST'                                                  
         MVI   P2,C'-'                                                          
         MVC   P2+1(29),P2                                                      
         GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
ESTBPTR  DS    F                   PTR WHERE LEFT OFF IN ESTTBL                 
SVSLHKEY DS    XL13                SAVE LOCKIN HDR KEY                          
SVSLKKEY DS    XL13                SAVE LOCKIN REC KEY                          
SVSEQ    DS    XL3                                                              
TEMPDATE DS    XL2                                                              
ELCODE   DS    XL1                                                              
FLAG     DS    XL1                                                              
TEMPESTB DS    XL256                                                            
TMPESTB2 DS    XL256                                                            
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
**PAN#1  DC    CL21'114SPREPFXYK105/01/02'                                      
         END                                                                    
