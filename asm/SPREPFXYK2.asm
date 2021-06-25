*          DATA SET SPREPFXYK2 AT LEVEL 132 AS OF 05/08/00                      
*PHASE SPFX02Y2                                                                 
SPFX02Y2 TITLE 'SPFX02Y2 FINDS UNMTCHD ESTS IN STLOCK RECS TO DEL THEM'         
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
         CLI   MODE,ESTFRST                                                     
         BE    EST5                                                             
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
* ESTIMATE PROCESSING                                     *                     
*=========================================================*                     
         SPACE 1                                                                
EST5     DS    0H                                                               
         MVC   BYTE,BAGYMD         MAKE SURE AGY IS COKE                        
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'B0'                                                       
         BNE   ESTXIT              THIS PROG ONLY FOR COKE                      
*                                                                               
         XC    SVSEQ,SVSEQ                                                      
         XC    FLAG,FLAG                                                        
*                                                                               
         LA    R1,IO               GET REC IN MY IO                             
         ST    R1,AREC                                                          
*                                                                               
         BAS   RE,ESTDAT           GET EST START AND END DATES                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING SLHRECD,R6          GET 1ST LOKIN HDR KEY                        
         MVI   SLHKTYP,SLHKTYPQ                                                 
         MVI   SLHKSUB,SLHKSUBQ                                                 
         MVC   SLHKAGMD,BAGYMD     GET AGY & MEDIA FROM REQ CARD                
         CLC   QCLT,=C'ALL'                                                     
         BE    CLTERR                                                           
         CLC   QCLT,=C'   '                                                     
         BE    CLTERR              FOR THIS PROG CLT IS REQUIRED                
         MVC   SLHKCLT,BCLT                                                     
         CLC   QMKT,=C'    '                                                    
         BNE   EST10                                                            
*        CLC   QSTA(3),=C'ALL'                                                  
*        BE    EST10                                                            
*        MVC   SLHKMKT,BMKT                                                     
*        MVC   SLHKSTA,BSTA                                                     
*        OI    FLAG,X'40'          WILL COMPARE FOR ADDIT FILTERS               
EST10    GOTO1 HIGH                                                             
         B     EST20                                                            
*                                                                               
EST15    GOTO1 SEQ                                                              
*                                                                               
EST20    CLC   KEY(5),KEYSAVE                                                   
         BNE   ESTXIT              NOMORE STALCK HDR RECS FOR MY PARAMS         
         TM    FLAG,X'40'          SHOULD I CHECK FURTHER ?                     
         BZ    EST25                                                            
         CLC   KEY+5(5),KEYSAVE+5                                               
         BNE   ESTXIT                                                           
*                                                                               
EST25    MVC   SVSLHKEY,KEY        SAVE LOCKIN HDR KEY                          
         LA    R6,KEY                                                           
         CLC   SVSEQ,SLHKSEQ                                                    
         BNE   *+6                                                              
         DC    H'0'                SEQ# HAD TO CHANGE                           
         MVC   SVSEQ,SLHKSEQ                                                    
*                                                                               
         LA    R6,KEY                                                           
         XC    KEY,KEY             BIULD KEY FOR LOCKIN REC                     
         USING SLKRECD,R6                                                       
         MVI   SLKKTYP,SLKKTYPQ                                                 
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,SVSLHKEY+2                                              
         MVC   SLKKSEQ,SVSEQ                                                    
*        MVC   SLKKPRD,BPRD                                                     
*        MVC   SLKKEST,BEST                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    EST35                                                            
         MVC   P(10),=C'*****SEQ #'                                             
         EDIT  SVSEQ,(6,P+11),FILL=0            SEQ #                           
         MVC   P+19(10),=C'FOR CLIENT'                                          
         MVC   P+30(3),QCLT                     CLIENT                          
         MVC   P+35(42),=C'EXISTS IN HDR REC, BUT NOT IN LOKIN RECORD'          
         GOTO1 REPORT                                                           
         MVC   KEY(13),SVSLHKEY                                                 
         GOTO1 HIGH                                                             
         B     EST15                                                            
*                                                                               
EST30    GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    EST35               IF SEQ# CHANGED, GET NXT HDR REC             
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVSLHKEY                                                 
         GOTO1 HIGH                                                             
         B     EST15                                                            
*                                                                               
EST35    LA    R6,KEY                                                           
         CLC   SLKKEST,BEST                                                     
         BNE   EST30                                                            
         MVC   SVSLKKEY,KEY        REMEBER LOCKIN REC KEY                       
*        CLC   SLKKPRD,BPRD                                                     
*        BNE   EST30                                                            
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL            FIND WEEKLY LCKIN ELEM                       
         BE    EST40                                                            
         MVC   P(5),=C'!!!!!'                                                   
         MVC   P+5(43),=C'FOLLOWING REC DOES NOT HAVE WKLY LOKIN ELEM'          
         GOTO1 HEXOUT,DMCB,IO,P2+5,13,0                                         
         GOTO1 REPORT                                                           
         B     EST30                                                            
*                                                                               
         USING LOKEL,R6                                                         
EST40    MVC   TEMPDATE,LOKWEEK                                                 
         BAS   RE,MATCHDAT                                                      
         BNE   DELREC                                                           
         BAS   RE,NEXTEL                                                        
         BE    EST40                                                            
         B     EST30                                                            
*                                                                               
*                                                                               
*                                                                               
ESTXIT   MVC   P(4),=C'DONE'                                                    
         GOTO1 REPORT                                                           
**** THIS IS NOT TO BE CALLED WITH OTHER MODES ****                             
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
CLTERR   MVC   P(41),=C'CLIENT HAS TO BE DEFINED FOR THIS PROGRAM'              
         GOTO1 REPORT                                                           
         B     ESTXIT                                                           
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
*********************************************************************           
* FIND EST REC FOR GIVEN CLT PRD AND EST AND GET START & END DATES  *           
*********************************************************************           
*                                                                               
ESTDAT   NTR1                                                                   
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6          BUILD EST REC KEY                            
         GOTO1 DATCON,DMCB,(0,ESTART),(2,SDATE)                                 
         GOTO1 DATCON,DMCB,(0,EEND),(2,EDATE)                                   
         DROP  R6                                                               
*                                                                               
         XIT1                                                                   
*                                                                               
*********************************************************************           
*     CHECK EST DATE FROM WEEKLY LOCKIN ELEM AGAINS ESTTBL          *           
*********************************************************************           
*                                                                               
MATCHDAT NTR1                                                                   
*                                                                               
         CLC   TEMPDATE,SDATE                                                   
         BL    MISMATCH                                                         
         CLC   TEMPDATE,EDATE                                                   
         BH    MISMATCH                                                         
*                                                                               
         B     YES                                                              
*                                                                               
MISMATCH B     NO                  SET CC NEQ                                   
*                                                                               
*                                                                               
*********************************************************************           
* PRT LOCKIN REC WHERE LOKWEEK IS OUTSIDE ST/END DATES OF ESTHDRREC *           
*********************************************************************           
*                                                                               
YKVAREC  DS    0H                                                               
*                                                                               
         LA    R6,IO                                                            
         USING SLKRECD,R6                                                       
         OI    SLKKCNTL,X'C0'      CLOSE RECORD                                 
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 HEXOUT,DMCB,IO,P,14,0                                            
         GOTO1 REPORT              PRT REC FOR REFERENCE                        
*                                                                               
         B     EST30                            READ NXT LOKIN REC              
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,24,ELCODE                                                     
         LTORG                                                                  
*                                                                               
SDATE    DS    XL2                                                              
EDATE    DS    XL2                                                              
SVSLHKEY DS    XL13                SAVE LOCKIN HDR KEY                          
SVSLKKEY DS    XL13                SAVE LOCKIN REC KEY                          
SVSEQ    DS    XL3                                                              
TEMPDATE DS    XL2                                                              
ELCODE   DS    XL1                                                              
FLAG     DS    XL1                                                              
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
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'132SPREPFXYK205/08/00'                                      
         END                                                                    
