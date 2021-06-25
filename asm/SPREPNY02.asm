*          DATA SET SPREPNY02  AT LEVEL 074 AS OF 09/18/18                      
*PHASE SPNY02B                                                                  
*INCLUDE BUFFERIN                                                               
         TITLE 'SPNY02 - READ UNIT BILLING RECORDS FOR SAP INTERFACE'           
*======================================================================         
* READ THE OUTPUT RECORDS FROM SPNX02 FOR ALL ACC BILLING POSTINGS              
* THEN GO AND READ THE UNIT BILLING RECORDS FOR THE MEDIA/CLIENT                
* GOING BACK 7 DAYS FROM TODAY'S DATE AND BUILD A BUFFER OF THE DATA            
*======================================================================         
                                                                                
SPNY02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPNY02                                                         
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPNY02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX00                                                             
*                                                                               
EQXIT    CR    RC,RC                                                            
         J     *+6                                                              
NEQXIT   LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
FX00     DS    0H                                                               
         OPEN  (POSTIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         JNE   *+2                                                              
*                                                                               
         OPEN  (SAPOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         JNE   *+2                                                              
*                                                                               
         XC    SRREC,SRREC                                                      
         GOTO1 DATCON,DMCB,TODAY,(2,TODAYP)                                     
         GOTO1 (RF),(R1),,(3,TODAYB)                                            
*                                                                               
         LHI   R0,-7               BACK UP 7 DAYS                               
*                                                                               
         CLI   QOPT1,C'0'          TEST OVERRIDE BACKUP PERIOD                  
         BL    FX02                                                             
         PACK  DUB,QOPT1(2)                                                     
         CVB   R0,DUB                                                           
         MHI   R0,30                                                            
         LNR   R0,R0                                                            
*                                                                               
FX02     GOTO1 ADDAY,DMCB,TODAY,DUB,(R0)                                        
         GOTO1 DATCON,DMCB,DUB,(2,TODAYM7)                                      
         EJECT                                                                  
*===================================================================            
* READ NEXT POSTING RECORD. IF CLIENT CHANGES, GET NEW CLIENT RECORD            
* CLEAR BUFFERIN AND REBUILD TABLES                                             
*===================================================================            
                                                                                
FX10     MVC   SRRECOLD,SRREC      SAVE PREVIOUS SRREC                          
         GET   POSTIN,SRREC                                                     
*                                                                               
         CLC   SRCLT,SRCLT-SRREC+SRRECOLD                                       
         JE    FX20                                                             
                                                                                
*==========================================================                     
* READ THE CLIENT RECORD TO INSERT OFFICE IN OUTPUT RECORDS                     
*==========================================================                     
                                                                                
         MVI   CLTSW,C'Y'          SET NEW CLIENT SWITCH                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
*                                                                               
         GOTO1 CLPACK,DMCB,SRCLT,KEY+2                                          
*                                                                               
         L     R8,ADCLT                                                         
         USING CLTHDRD,R8                                                       
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
*                                                                               
         GOTO1 GETCLT                                                           
         DROP  R8                                                               
                                                                                
*===============================================================                
* NOW READ BILLING HEADER RECORD                                                
* KEY HAS KEY OF CLIENT RECORD IN IT                                            
*===============================================================                
                                                                                
FX20     L     RE,ADCLT                                                         
         MVC   KEY(4),0(RE)        MOVE 00/A-M/CLT                              
         MVC   KEY+4(3),SRPRD                                                   
         PACK  DUB,SREST                                                        
         CVB   R0,DUB                                                           
         STC   R0,KEY+7                                                         
         MVC   KEY+8(2),SRBILMOS   YS/MS                                        
* NO DDS DATE!                                                                  
         GOTO1 DATCON,DMCB,(2,SRBILDAT),(X'20',WORK)   GET YYMMDD               
         LLC   R0,WORK+1           GET LAST DIGIT OF YEAR (E.G., F7)            
         SLL   R0,4                AND LEFT ALIGN                               
*                                                                               
         PACK  DUB,WORK+2(2)                                                    
         CVB   R1,DUB              GET MONTH (X'01'-X'0C')                      
         OR    R1,R0                                                            
         STC   R1,KEY+10                                                        
*                                                                               
         PACK  DUB,SRBILINV+2(4)                                                
         CVB   R0,DUB                                                           
         STCM  R0,3,KEY+11                                                      
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   *+2                                                              
         MVC   SVBILKEY,KEY                                                     
*                                                                               
         L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         USING BILLRECD,R8                                                      
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         MVI   BILLMAN,C'N'                                                     
         MVI   BILLADJ,C'N'                                                     
         MVI   BILLCOMM,C'N'                                                    
         MVI   BILLCOS2,C'N'                                                    
         MVI   BILLAOR,C'N'                                                     
         MVI   BILLNET,C'N'                                                     
         MVI   NOVENDOR,C'N'                                                    
*                                                                               
         TM    BILSTAT,X'80'       TEST ADJ BILL                                
         JZ    *+12                                                             
         MVI   BILLADJ,C'Y'                                                     
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    BILSTAT,X'40'       TEST MANUAL BILL                             
         JZ    *+12                                                             
         MVI   BILLMAN,C'Y'                                                     
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    BILSTAT,X'02'       TEST COMM ONLY                               
         JZ    *+12                                                             
         MVI   BILLCOMM,C'Y'                                                    
         MVI   NOVENDOR,C'Y'                                                    
*                                                                               
         TM    BILSTAT,X'34'       TEST AOR                                     
         JZ    *+8                                                              
         MVI   BILLAOR,C'Y'                                                     
*                                                                               
         TM    BILSTAT,X'08'       TEST NET BILL                                
         JZ    *+8                                                              
         MVI   BILLNET,C'Y'                                                     
*                                                                               
         TM    BILSTAT2,BSTC2Q     TEST COST2 BILL                              
         JZ    *+8                                                              
         MVI   BILLCOS2,C'Y'                                                    
*                                                                               
         CLI   NOVENDOR,C'Y'                                                    
         JNE   FX70                                                             
                                                                                
*==================================================================             
* CREATE MYSAMB WHEN THERE ARE NO VENDOR DETAILS                                
*==================================================================             
                                                                                
FX66     BAS   RE,BLDSAMB          BUILD COMMON SAP FIELDS                      
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         ZAP   SAMBDOLS,BNETP                                                   
         ZAP   SAMBGRS,BGRSP                                                    
* MH 05FEB18 - NO DOLLARS FOR ADJ OR COMM ONLY BILLS                            
**NOP    TM    BILSTAT,X'82'       TEST ADJ BILL OR COMM ONLY BILL              
**NOP    BZ    *+10                                                             
**NOP    ZAP   SAMBDOLS,BACTP                                                   
*                                                                               
         LLC   R0,SRBILMOS         BILLABLE YR/SVC                              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBYS,DUB                                                       
*                                                                               
         IC    R0,SRBILMOS+1       BILLABLE MOS                                 
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBMS,DUB                                                       
*                                                                               
         MVC   SAMBVNDR(7),=C'NETWORK'                                          
         MVC   SAMBVJN,SAMBVNDR                                                 
         DROP  R8                                                               
*                                                                               
         BAS   RE,PUTSAMB                                                       
*                                                                               
         CLI   QOPT5,C'Y'          TEST RUN?                                    
         JE    FX90                YES - GO CHECK FOR MANUAL BILLS              
         J     FX10                NO STABUCK RECS - READ NEXT RECOVERY         
         DROP  R7                                                               
                                                                                
*                                                                               
FX70     CLI   CLTSW,C'Y'          TEST NEED TO REBUILD CLT BUFFER              
         JNE   FX72                                                             
         BAS   RE,BLDBUFF          BUILD BUFFERIN RECORDS FOR THIS CLT          
         MVI   CLTSW,C'N'                                                       
*                                                                               
         GOTO1 GET                 RESTORE BILLING HEADER RECORD                
                                                                                
*=============================================================                  
* READ UNIT DETAIL FROM BUFFERIN                                                
*=============================================================                  
                                                                                
FX72     XC    BFKEY,BFKEY         LOOK FOR RECORDS IN BUFFER                   
         MVC   BFQPRD,SRPRD                                                     
         PACK  DUB,SREST                                                        
         CVB   R0,DUB                                                           
         STC   R0,BFEST                                                         
         MVC   BFBILDAT,SRBILDAT   BILL DATE                                    
         PACK  DUB,SRBILINV+2(4)                                                
         CVB   R0,DUB                                                           
         STCM  R0,3,BFBILINV                                                    
         MVC   BFBILMOS,SRBILMOS   YEAR/MONTH OF SERVICE                        
*                                                                               
         MVC   BFKEYOLD,BFREC      SAVE KEY TRHU MOS                            
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
*                                                                               
         TM    4(R1),BUFFEEOF                                                   
         JO    FX10                                                             
*                                                                               
         CLC   BFKEYOLD,BFREC                                                   
         JE    FX74                                                             
         MVI   NOVENDOR,C'Y'                                                    
         J     FX66                                                             
*                                                                               
FX74     BAS   RE,BLDSAMB                                                       
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
FX76     MVC   SAMBVJN(4),BFBILNTW    MOVE NETWORK                              
         OC    SAMBVJN,SPACES                                                   
         MVC   SAMBVNDR,SAMBVJN                                                 
*                                                                               
         ZAP   SAMBDOLS,BFBILNET                                                
         ZAP   SAMBGRS,BFBILGRS                                                 
*                                                                               
         BAS   RE,PUTSAMB                                                       
*                                                                               
         GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
         TM    4(R1),BUFFEEOF                                                   
         JO    FX90                                                             
*                                                                               
         CLC   BFKEYOLD,BFREC      TEST SAME THRU MOS                           
         JE    FX76                YES - PROCESS                                
         EJECT                                                                  
*=================================================================              
* CHECK FOR STABUCK RECORD FOR MANUAL BILL THAT WAS REVERSED                    
*=================================================================              
                                                                                
FX90     LA    R8,KEY                                                           
         USING STABUCKD,R8                                                      
*                                                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   STABKCOD,=X'0E01'                                                
         L     RE,ADCLT                                                         
         MVC   STABKAM(3),1(RE)     MOVE A-M/CLT FROM CLTHDR                    
         MVC   STABKPRD,SVPRDCD                                                 
         MVC   STABKEST,SVEST                                                   
         DROP  R8                                                               
*                                                                               
         GOTO1 HIGH                                                             
         J     FX94                                                             
*                                                                               
FX92     GOTO1 SEQ                                                              
*                                                                               
FX94     CLC   KEY(7),KEYSAVE      SAME 0E01/AM/CLT/PRD/EST                     
         JNE   FX98                                                             
*                                                                               
FX95     L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         USING STABUCKD,R8                                                      
*                                                                               
         GOTO1 GET                                                              
*                                                                               
         LA    R6,STABELEM                                                      
         USING STABELEM,R6                                                      
*                                                                               
         MVI   ELCDLO,X'0E'        GET NORMAL ELS                               
         MVI   ELCDHI,X'1E'        AND UNBILLING ELS                            
         BAS   RE,NEXTEL2                                                       
         B     *+8                                                              
*                                                                               
FX96     BAS   RE,NEXTEL                                                        
         JNE   FX92                                                             
*                                                                               
         CLC   STABPER,SRBILMOS    MATCH Y/M OF SERVICE                         
         JNE   FX96                                                             
*                                                                               
         CLC   STABBDT,SRBILDAT    RIGHT BILL DATE                              
         BNE   FX96                NO - SKIP                                    
*                                                                               
         MVC   HALF,STABINV                                                     
         NI    HALF,X'3F'          DROP X'80'+X'40'                             
         CLC   HALF,SRBILINV       RIGHT INVOICE                                
         BNE   FX96                                                             
*                                                                               
         MVI   BILLMAN,C'Y'        INDICATE MANUAL BILL                         
         BAS   RE,BLDSAMB                                                       
*                                                                               
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         L     R0,STABNET                                                       
         L     R1,STABGRS                                                       
         CLI   0(R6),X'1E'                                                      
         JNE   *+6                                                              
         LCR   R0,R0                                                            
         LCR   R1,R1                                                            
         CVD   R0,DUB                                                           
         ZAP   SAMBDOLS,DUB                                                     
         CVD   R1,DUB                                                           
         ZAP   SAMBGRS,DUB                                                      
*                                                                               
         MVC   SAMBVNDR(7),=C'NETWORK'                                          
         MVC   SAMBVJN,SAMBVNDR                                                 
         DROP  R8                                                               
*                                                                               
         BAS   RE,PUTSAMB                                                       
         DROP  R7                                                               
*                                                                               
FX98     CLI   QOPT5,C'Y'          IS IT A TEST RUN?                            
         JNE   FX10                NO - NEXT POSTING REC                        
         EJECT                                                                  
*============================================================                   
* END OF RECOVERY FILE -  CLOSE FILES AND                                       
*============================================================                   
                                                                                
ENDIN    CLOSE POSTIN                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         CLOSE SAPOUT                                                           
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         MVI   MODE,REQLAST                                                     
         J     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* ROUTINE TO BUILD COMMON SAMB DATA FIELDS                                      
*==================================================================             
                                                                                
BLDSAMB  NTR1                                                                   
         MVC   MYSAMBREC,SPACES                                                 
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
*                                                                               
         MVC   SAMBKAGY,AGENCY                                                  
         MVI   SAMBKSYS,SAPKSYS_NET                                             
         MVC   SAMBKMED,SRMED                                                   
         MVC   SAMBKCLT,SRCLT                                                   
*                                                                               
         L     RE,ADCLT                                                         
         MVC   SAMBKOFF,CACCOFC-CLTHDR(RE)                                      
         CLI   SAMBKOFF,C' '                                                    
         BH    BLDSAMB2                                                         
         MVC   SAMBKOFF,COFFICE-CLTHDR(RE)                                      
         MVI   SAMBKOFF,C' '                                                    
*                                                                               
BLDSAMB2 MVC   SAMBKPRD,SRPRD                                                   
         MVC   SAMBKEST,SREST                                                   
*                                                                               
         MVC   SAMBKINV,SRBILINV                                                
         MVI   SAMBKOVRD,C'N'      OVERRIDE MEDIA CODE FOR JOIN                 
*                                                                               
         LLC   R0,SRBILMOS         YR/SVC                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBYS,DUB                                                       
*                                                                               
         IC    R0,SRBILMOS+1       MOS                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SAMBMS,DUB                                                       
*                                                                               
         CLI   BILLMAN,C'Y'                                                     
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'MAN'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLADJ,C'Y'                                                     
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'ADJ'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLCOMM,C'Y'                                                    
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'COM'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLCOS2,C'Y'                                                    
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'CO2'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLAOR,C'Y'                                                     
         JNE   *+14                                                             
         MVC   SAMBTYPE,=C'AOR'                                                 
         J     BLDSAMBX                                                         
*                                                                               
         CLI   BILLNET,C'Y'                                                     
         JNE   *+10                                                             
         MVC   SAMBTYPE,=C'NET'                                                 
*                                                                               
BLDSAMBX J     EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* COMMON ROUTINE TO PUT/PRINT SAP RECORDE                                       
*===========================================================                    
*                                                                               
PUTSAMB  NTR1                                                                   
*                                                                               
         PUT   SAPOUT,MYSAMBREC                                                 
         AP    OUTCNT,=P'1'                                                     
* NOW PRINT RECORD                                                              
         LA    R7,MYSAMBREC                                                     
         USING SAMBRECD,R7                                                      
                                                                                
         MVC   PAGY,SAMBKAGY                                                    
         MVC   PMED,SAMBKMED                                                    
         MVC   POFFC,SAMBKOFF                                                   
         MVC   PCLT,SAMBKCLT                                                    
         MVC   PPRD,SAMBKPRD                                                    
         MVC   PEST,SAMBKEST                                                    
         MVC   PSTA,SAMBVNDR                                                    
*                                                                               
         MVC   PBILTYPE,SAMBTYPE                                                
*                                                                               
         MVC   PINV,SAMBKINV                                                    
*                                                                               
         MVC   PYRSVC,SAMBYS                                                    
         MVC   PMONSVC,SAMBMS                                                   
         EDIT  (P6,SAMBGRS),(12,PGRS),2,MINUS=YES                               
         EDIT  (P6,SAMBDOLS),(12,PNET),2,MINUS=YES                              
*                                                                               
         GOTO1 REPORT                                                           
         J     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
*=========================================================                      
* ON CHANGE OF CLIENT, REINITIALIZE BUFFERIN                                    
*=========================================================                      
                                                                                
BLDBUFF  NTR1                                                                   
*                                                                               
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
         XC    BUFFCNT,BUFFCNT                                                  
                                                                                
*===============================================================                
* NOW READ UNIT BILLING RECORDS FOR THIS CLIENT FROM XSPFILE!!!                 
* AND TABLE RECORDS WITHIN THE LAST 7 DAYS                                      
*===============================================================                
                                                                                
         LA    R8,BIGKEY                                                        
         USING NUBK0KEY,R8                                                      
*                                                                               
         XC    NUBK0KEY,NUBK0KEY                                                
         MVI   NUBK0SYS,NUBK0SYQ   X'0E'                                        
         MVI   NUBK0STY,NUBK0STQ   X'0A'                                        
         L     RE,ADCLT                                                         
         MVC   NUBK0AM(3),1(RE)    A-M/CLT                                      
*                                                                               
         GOTO1 XSPHIGH                                                          
         B     BLDB12                                                           
*                                                                               
BLDB10   GOTO1 XSPSEQ                                                           
*                                                                               
BLDB12   CLC   BIGKEY(5),BIGKEYSV  SAME A-M/CLT                                 
         JNE   EXIT                                                             
*                                                                               
         LA    R8,BIGKEY                                                        
         USING NUBK0KEY,R8                                                      
*                                                                               
         CLC   NUBK0BDT,TODAYM7    TEST RUN IN LAST WEEK                        
         JNH   BLDB10              NO - IGNORE                                  
         DROP  R8                                                               
*                                                                               
         L     R8,ADBUY                                                         
         ST    R8,AREC                                                          
         USING NUBRECD,R8                                                       
*                                                                               
         GOTO1 XSPGET                                                           
*                                                                               
         LA    R6,NUBDATA                                                       
         USING NBILD,R6                                                         
*                                                                               
         MVI   ELCDLO,NBILELQ      GET X'10' BILLING ELEMENTS                   
         MVI   ELCDHI,NBILELQ                                                   
         BRAS  RE,NEXTEL2                                                       
         J     *+8                                                              
*                                                                               
BLDB20   BAS   RE,NEXTEL                                                        
         BNE   BLDB10                                                           
*                                                                               
         XC    BFREC,BFREC                                                      
         MVC   BFQPRD,NBILPRDC     3-CHAR PRD CODE                              
         MVC   BFEST,NUBK0EST                                                   
         MVC   BFBILDAT,NUBK0BDT                                                
         MVC   BFBILINV,NBILNUM                                                 
         MVC   BFBILMOS,NBILMOS                                                 
         MVC   BFBILNTW,NUBK0NET                                                
*------------------->>>                                                         
*------------------->>>                                                         
*------------------->>>                                                         
         CLC   BFQPRD,QPRD         TEST TO FILTER ON ONE PRD                    
         JNE   *+8                                                              
         J     *+4                 FOR IDF PATCH                                
*------------------->>>                                                         
*------------------->>>                                                         
*------------------->>>                                                         
*                                                                               
         L     R0,NBILGRS                                                       
         CVD   R0,DUB                                                           
         ZAP   BFBILGRS,DUB                                                     
         L     R0,NBILNET                                                       
         CVD   R0,DUB                                                           
         ZAP   BFBILNET,DUB                                                     
*                                                                               
         CP    BFBILGRS,=P'0'                                                   
         JNE   BLDB30                                                           
         CP    BFBILNET,=P'0'                                                   
         JZ    BLDB20                                                           
*                                                                               
BLDB30   L     R0,BUFFCNT                                                       
         AHI   R0,1                                                             
         ST    R0,BUFFCNT                                                       
*                                                                               
         CLI   QOPT5,C'Y'         TEST TRACE OPTION                             
         JNE   BLDB32                                                           
         GOTOR PRNTBL,DMCB,=C'PUTBUF',BFREC,C'DUMP',BFRECL,=C'1D00'             
                                                                                
BLDB32   GOTOR BUFFERIN,DMCB,('BUFFAPUT',BUFFET),BFREC,ACOMFACS                 
         J     BLDB20                                                           
         EJECT                                                                  
*==============================================================                 
* I/O ROUTINES FOR XSPDIR/XSPFILE                                               
*==============================================================                 
                                                                                
XSPHIGH  NTR1                                                                   
         MVC   BIGKEYSV,BIGKEY                                                  
         GOTO1 DATAMGR,DMCB,DMRDHI,XSPDIR,BIGKEYSV,BIGKEY                       
         J     EXIT                                                             
*                                                                               
XSPSEQ   NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DMRSEQ,XSPDIR,BIGKEY,BIGKEY                         
         J     EXIT                                                             
*                                                                               
XSPGET   NTR1                                                                   
         LA    R0,BIGKEY+NUBDA-NUBKEY                                           
         GOTO1 DATAMGR,DMCB,GETREC,XSPFILE,(R0),AREC,DMWORK                     
         J     EXIT                                                             
*                                                                               
XSPDIR   DC    CL8'XSPDIR'                                                      
XSPFILE  DC    CL8'XSPFILE'                                                     
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         LLC   R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JZ    *+2                                                              
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
COUNTS   DS    0D                                                               
INCNT    DC    PL4'0',CL20'POSTING RECS IN'                                     
OUTCNT   DC    PL4'0',CL20'SAP RECS OUT'                                        
COUNTX   EQU   *                                                                
BUFFERIN DC    V(BUFFERIN)                                                      
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
SVBILLDT DS    XL2                                                              
SVSYS    DS    CL1                                                              
SVPWOS   DS    XL3                                                              
SVINVB   DS    XL2                                                              
TODAYM7  DS    XL2                 TODAY - 7 DAYS                               
SVBILKEY DS    XL18                                                             
BILLMAN  DS    CL1                                                              
BILLADJ  DS    CL1                                                              
BILLCOMM DS    CL1                                                              
BILLCOS2 DS    CL1                                                              
BILLAOR  DS    CL1                                                              
BILLNET  DS    CL1                                                              
NOVENDOR DS    CL1                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'**SRREC*'                                                    
SRREC    DS    0XL32                                                            
SRSYS    DS    CL1                                                              
SRAGY    DS    CL2                                                              
SRMED    DS    CL1                                                              
SRCLT    DS    CL3                                                              
SRPRD    DS    CL3                                                              
SREST    DS    CL3                                                              
*                                                                               
SRBILDAT DS    XL2                 2-BYTE PACKED                                
SRBILINV DS    CL6                                                              
SRBILMOS DS    XL2                 Y/M PWOS                                     
         DS    XL9                 SPARE                                        
SRRECL   EQU   *-SRREC                                                          
*                                                                               
SRRECOLD DS    XL32                                                             
*                                                                               
BUFFCNT  DS    F                                                                
*                                                                               
         DS    0D                                                               
BIGKEY   DS    CL40                                                             
BIGKEYSV DS    CL40                                                             
*                                                                               
         DS    0D                                                               
         DC    CL8'*SAMBREC'                                                    
MYSAMBREC DS   CL100                                                            
*                                                                               
POSTIN   DCB   DDNAME=POSTIN,DSORG=PS,RECFM=FB,                        X        
               LRECL=32,BLKSIZE=16000,MACRF=GM,EODAD=ENDIN                      
*                                                                               
SAPOUT   DCB   DDNAME=SAPOUT,DSORG=PS,RECFM=FB,LRECL=100,BLKSIZE=12800,X        
               MACRF=PM                                                         
         LTORG                                                                  
*                                                                               
BUFFET   BUFFD TYPE=P,KEYLEN=L'BFKEY,COLUMNS=2,FILE=BUFFWK,            *        
               BUFFERS=20                                                       
*                                                                               
         DS    0D                                                               
         DC    CL8'**BFREC*'                                                    
BFREC    DS    0XL32                                                            
BFKEY    DS    0XL16                                                            
BFQPRD   DS    CL3                 PRODUCT NUMBER                               
BFEST    DS    XL1                 ESTIMATE NUMBER                              
BFBILDAT DS    XL2                 BILLING Y/M                                  
BFBILINV DS    XL2                 INVOICE NUMBER                               
BFBILMOS DS    XL2                 BILLING Y/M OF SVC                           
BFBILNTW DS    CL4                 NETWORK                                      
         DS    XL2                                                              
*                                                                               
BFBILGRS DS    PL8                 GROSS DOLLARS                                
BFBILNET DS    PL8                 NET DOLLARS                                  
BFRECL   EQU   *-BFREC                                                          
*                                                                               
         DC    CL8'BFKEYOLD'                                                    
BFKEYOLD DS    XL10                PRD/EST/BILLDT/INV/MOS                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE ACSAPREC                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
*                                                                               
* DSECT FOR PRINT LINE                                                          
         ORG   P                                                                
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
POFFC    DS    CL2                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PPRD     DS    CL3                                                              
         DS    CL1                                                              
PSTA     DS    CL9                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PYRSVC   DS    CL2                                                              
         DS    CL1                                                              
PMONSVC  DS    CL2                                                              
         DS    CL1                                                              
PBILTYPE DS    CL3                                                              
         DS    CL1                                                              
PINV     DS    CL6                                                              
         DS    CL1                                                              
PGRS     DS    CL13                                                             
         DS    CL1                                                              
PNET     DS    CL13                                                             
         DS    0D                                                               
         EJECT                                                                  
       ++INCLUDE NEGENUBILL                                                     
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BILLRECD DSECT                                                                  
       ++INCLUDE SPGENBILL                                                      
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE DDBUFFD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074SPREPNY02 09/18/18'                                      
         END                                                                    
