*          DATA SET TAGENCF    AT LEVEL 078 AS OF 01/28/16                      
*PHASE T702CFC,*                                                                
         TITLE 'T702CF - ISPLIT AND ASPLIT DIS/CHANGE'                          
T702CF   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702CF                                                         
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL STORAGE AREA                        
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROL                                                     
         SPACE 2                                                                
         L     R5,AFRSTREC                                                      
         LA    R2,APFTAB                                                        
         CLI   TWASCR,SCRCF        IF ISPLIT SCREEN                             
         BE    MAIN5                                                            
         LA    R2,PFTAB            SET PFKEY TABLE                              
         L     R1,AFRSTLIN                                                      
         USING LINED,R1                                                         
         LA    RE,LINSTRTH                                                      
         ST    RE,AFRSTREC         SET AFRSTREC FOR PF14                        
         DROP  R1                                                               
MAIN5    GOTO1 INITIAL,DMCB,(R2)   INITIALIZE                                   
         ST    R5,AFRSTREC         RESTORE                                      
         SPACE 2                                                                
         LA    R1,SDVL1H           R1=A(FIRST LINE OF ASPLIT)                   
         LA    R2,SDVLSTH          R2=A(LAST LINE)                              
         LA    R3,SDVLCHGH         R3=A(LAST CHANGED)                           
         CLI   TWASCR,SCRCF                                                     
         BE    MAIN10                                                           
         LA    R1,SIDL1H           R2=A(FIRST LINE OF ISPLIT)                   
         LA    R2,SIDLSTH          R2=A(LAST LINE)                              
         LA    R3,SIDLCHGH         R3=A(LAST CHANGED)                           
MAIN10   ST    R1,AFRSTLIN                                                      
         ST    R2,ALSTLIN                                                       
         ST    R3,ALCHGHDR                                                      
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   *+12                                                             
         BAS   RE,VREC                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    MAIN20                                                           
         CLI   MODE,XRECPUT        AFTER CHANGING RECORD                        
         BNE   XIT                                                              
MAIN20   BRAS  RE,DISPLAY          (RE-)DISPLAY IT                              
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         CLI   TWASCR,SCRCF        IF ASPLIT                                    
         BNE   *+12                                                             
         BAS   RE,VKAS             VALIDATE ASPLIT KEY                          
         B     VKEYX                                                            
*                                                                               
         BAS   RE,VKIS             OTHERWISE, VALIDATE ISPLIT KEY               
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FOR ASPLIT SCREEN                                   
         SPACE 1                                                                
VKAS     NTR1                                                                   
         CLI   SCRSTAT,0                                                        
         BE    *+8                                                              
         NI    SDVAGYH+4,X'DF'     FORCE KEY RE-VALIDATION                      
*                                                                               
         LA    R2,SDVAGYH          R2=A(AGENCY FIELD)                           
         TM    4(R2),X'20'         IF FIELD HAS CHANGED                         
         BO    VKAS15                                                           
         NI    SDVCIDH+4,X'DF'     SET COMM ID NOT VALIDATED                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),0                               
         BAS   RE,SETAGY           SET AGENCY INFO                              
*                                                                               
VKAS15   LA    R2,SDVCIDH          R2=A(COMM ID FIELD)                          
         TM    4(R2),X'20'         IF FIELD HAS CHANGED                         
         BO    VKAS20                                                           
         NI    SDVADVH+4,X'DF'     SET ADVICE NOT VALIDATED                     
         GOTO1 RECVAL,DMCB,TLCOICDQ,(X'24',(R2))                                
         BE    VKAS20                                                           
         CLI   ERROR,NOTFOUND      IF ERROR IS NOT RECORD NOT FOUND             
         BNE   THEEND              GIVE IT                                      
*                                                                               
VKAS20   LA    R2,SDVADVH          R2=A(ADVICE FIELD)                           
         TM    4(R2),X'20'         IF FIELD HAS CHANGED                         
         BO    VKASX                                                            
         CLI   5(R2),0             IF NO INPUT                                  
         BNE   VKAS25                                                           
         OC    TGADV,TGADV         USE GLOBAL IF AVAILABLE                      
         BZ    ERRMISS                                                          
         MVC   8(L'TGADV,R2),TGADV                                              
         OI    6(R2),X'80'                                                      
VKAS25   GOTO1 RECVAL,DMCB,TLDVCDQ,(X'24',(R2))  GET ADVICE RECORD              
         BNE   THEEND                                                           
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   VKASX                                                            
         BAS   RE,CHKADV           CHECK ADVICE RECORD                          
VKASX    B     XIT                                                              
         EJECT                                                                  
*              CHECK ADVICE RECORD OKAY FOR CHANGE                              
         SPACE 1                                                                
CHKADV   NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,TADVELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   VKASX                                                            
         USING TADVD,R3                                                         
*                                                                               
         TM    TADVSTAT,TADVSSNT                                                
         BO    ERRSENT             CANNOT CHANGE IF SENT                        
*                                                                               
         MVI   ELCODE,TANUELQ      IF ESTIMATE NUMBER EXISTS                    
         GOTO1 GETL,DMCB,(1,=AL1(TANUTEST))                                     
         BNE   XIT                                                              
*                                                                               
         TM    AGYSTAT,TAAYSEST    AND EST WAS REQUIRED ON ADVICE               
         BZ    ERREST                                                           
         L     R3,TGELEM           ONLY ESTIMATE 'SPLIT' ALLOWED                
         USING TANUD,R3                                                         
         ZIC   R1,TANULEN                                                       
         SH    R1,=Y(TANULNQ)                                                   
         CH    R1,=H'5'                                                         
         BNE   ERREST                                                           
         CLC   =C'SPLIT',TANUMBER                                               
         BNE   ERREST                                                           
*                                                                               
         CLI   MODE,VALREC         THEN AT VALREC                               
         BNE   XIT                                                              
         GOTO1 REMELEM             REMOVE IT                                    
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              VALIDATE KEY FOR ISPLIT SCREEN                                   
         SPACE 1                                                                
VKIS     NTR1                                                                   
         CLI   SCRSTAT,0                                                        
         BE    *+8                                                              
         NI    SIDAGYH+4,X'DF'     FORCE KEY RE-VALIDATION                      
*                                                                               
         LA    R2,SIDAGYH          R2=A(AGENCY FIELD)                           
         TM    4(R2),X'20'         IF FIELD HAS CHANGED                         
         BO    VKIS10                                                           
         NI    SIDINVH+4,X'DF'     SET INVOICE NOT VALIDATED                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SIDAGYNH                        
         BAS   RE,SETAGY           SET AGENCY INFO                              
*                                                                               
VKIS10   LA    R2,SIDINVH          R2=A(INVOICE FIELD)                          
         TM    4(R2),X'20'         IF FIELD HAS CHANGED                         
         BO    VKISX                                                            
         CLI   5(R2),0             IF NO INVOICE INPUT                          
         BNE   VKIS18                                                           
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'04',SIDINVH) USE GLOBAL INVOICE           
         NI    SIDINVH+4,X'DF'     MAKE SURE NOT VAL. TILL ALL OKAY             
         CLI   ERROR,MISSING       IF THERE IS A GLOBAL INVOICE                 
         BE    THEEND                                                           
         XC    SIDINV,COMPLM       UN-COMPLEMENT INVOICE NUMBER                 
         GOTO1 TINVCON,DMCB,SIDINV,INV,DATCON                                   
         MVC   SIDINV,INV          CONVERT FOR DISPLAY                          
         CLI   0(R1),X'FF'         IF INVALID                                   
         BNE   VKIS14                                                           
         XC    SIDINV,SIDINV       CLEAR FROM SCREEN                            
         LA    R2,SIDINVH          R2=A(INVOICE FIELD)                          
         B     ERRMISS             REQUIRE INVOICE INPUT                        
*                                                                               
VKIS14   LA    R2,SIDAGYH                                                       
         CLI   ERROR,NOTFOUND                                                   
         BE    THEEND                                                           
*                                                                               
VKIS18   GOTO1 TINVCON,DMCB,SIDINV,INV,DATCON CONVERT INV INPUT FOR KEY         
         CLI   0(R1),X'FF'                                                      
         BE    ERRINV                                                           
*                                                                               
         XC    INV,COMPLM          COMPLEMENT INVOICE NUMBER                    
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A0',INV) VALIDATE/READ THE REC            
         BNE   THEEND                                                           
         CLI   ACTNUM,ACTCHA       IF ACTION IS CHANGE                          
         BNE   VKIS30                                                           
         L     R3,AIO                                                           
         USING TAIND,R3                                                         
         MVI   ELCODE,TAINELQ      FIND INVOICE STATUS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    TAINSTAT,TAINSAPR                                                
         BO    ERRAPR              CHANGE NOT ALLOWED IF APPROVED               
         TM    TAINSTAT,TAINSPAY                                                
         BNO   VKIS30                                                           
         TM    TAINSTA2,TAINSPRM                                                
         BNO   ERRSPLT             CANNOT SPLIT PAID REGULAR INVOICE            
*                                                                               
VKIS30   MVI   ELCODE,TAPDELQ      PAYMENT DETAILS ELEMENT                      
         L     R3,AIO                                                           
         USING TAPDD,R3                                                         
         BAS   RE,GETEL                                                         
         BNE   VKIS35                                                           
         TM    TAPDSTA2,TAPDSSUB   IF SUBSIDIARY INVOICE                        
         BNO   VKIS35                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,TANUELQ      FIND IT'S PRIMARY AND DISPLAY IT             
         USING TANUD,R3                                                         
         GOTO1 GETL,DMCB,(1,=AL1(TANUTSPL))                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,TGELEM                                                        
         GOTO1 TINVCON,DMCB,TANUMBER,SIDINV,DATCON                              
         OI    SIDINVH+6,X'80'     TRANSMIT                                     
         B     VKIS18                                                           
*                                                                               
VKIS35   OI    SIDINVH+4,X'20'     SET FIELD PREVIOUSLY VALIDATED               
*                                                                               
VKISX    B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              SET SOME AGENCY INFORMATION                                      
         SPACE 1                                                                
SETAGY   NTR1                                                                   
         MVI   INTER,C'N'                                                       
         XC    DEFJOB,DEFJOB                                                    
                                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TAAYELQ                                                   
         BAS   RE,GETEL            GET AGENCY ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R3                                                         
         MVC   AGYSTAT,TAAYSTAT   SAVE AGENCY STATUS                            
         MVC   AGYSTAT2,TAAYSTA2                                                
         MVC   AGYSTAT5,TAAYSTA5                                                
         MVC   AGYSTAT7,TAAYSTA7                                                
         MVC   TGJWBUNT,TAAYBUNT                                                
         TM    TAAYSTA6,TAAYST10  TEST TYPE 10 JOB VALIDATION                   
         BZ    *+8                                                              
         MVI   INTER,C'Y'                                                       
         DROP  R3                                                               
*                                                                               
         USING TANUD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TANUELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TANUTJOB))                                     
         BNE   SETAGY10                                                         
         L     R4,TGELEM                                                        
         MVC   LENGTHS,TANUMBER    SAVE ESTIMATE SETUP LENGTHS                  
         NI    LENGTHS,X'0F'                                                    
         NI    LENGTHS+1,X'0F'                                                  
         NI    LENGTHS+2,X'0F'                                                  
         DROP  R4                                                               
                                                                                
         USING TANUD,R4                                                         
SETAGY10 GOTO1 GETL,DMCB,(1,=AL1(TANUTDJB))                                     
         BNE   SETAGY20                                                         
         L     R4,TGELEM                                                        
         MVC   DEFJOB,TANUMBER     SAVE DEFAULT JOB                             
         DROP  R4                                                               
*                                                                               
SETAGY20 L     R3,AIO                                                           
         MVI   ELCODE,TABRELQ      GET BILLING RULES ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   SETAGYX                                                          
         USING TABRD,R3                                                         
         TM    TABRSTAT,TABRSINT+TABRSCIN  TEST ON PRODUCTION INTERFACE         
         BZ    *+8                                                              
         MVI   INTER,C'Y'                                                       
SETAGYX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              VALIDATE RECORD                                                  
VREC     NTR1                                                                   
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         BNE   ERPPLSI                                                          
*                                                                               
         MVC   SVKEY,KEY           SAVE KEY                                     
         MVI   ANYELEM,C'N'        NO TASIDS ON RECORD BEFORE CHANGE            
         L     R3,AIO                                                           
         MVI   ELCODE,TASIELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         MVI   ANYELEM,C'Y'        TASID EXISTS ON RECORD                       
*                                                                               
         CLI   TWASCR,SCRCF        IF ASPLIT                                    
         BNE   *+12                                                             
         BAS   RE,CHKADV           CHECK OKAY TO CHANGE                         
         B     VREC2                                                            
*                                                                               
         L     R3,AIO              ELSE, CHECK ISPLIT OKAY TO CHANGE            
         MVI   ELCODE,TAINELQ      FIND INVOICE STATUS ELEMENT                  
         USING TAIND,R3                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVSTAT,TAINSTAT     SAVE INVOICE STATUS                          
         MVC   SVSTA2,TAINSTA2     SAVE INVOICE STATUS2                         
         TM    TAINSTAT,TAINSAPR                                                
         BO    ERRAPR              INPUT NOT ALLOWED IF APPROVED                
         NI    TAINSTA2,X'FF'-TAINSPRM                                          
         DROP  R3                                                               
*                                                                               
VREC2    MVI   ELCODE,TASIELQ      DELETE EXISTING ELEMENTS                     
         GOTO1 REMELEM                                                          
*                                                                               
         ZAP   COUNTER,=P'0'       NUMBER OF SUBSIDIARY LINES                   
         XC    TOTPCT,TOTPCT       TOTAL PERCENTAGE OF RECORD                   
         LA    R5,MXLINES          NUMBER OF LINES                              
         USING LINED,R2                                                         
         L     R2,AFRSTLIN         R2=A(FIRST LINE)                             
*                                                                               
VREC5    GOTO1 FLDVAL,DMCB,(X'80',LINESTH),2 IF BLANK LINE                      
         BE    VREC10              SKIP IT                                      
         XC    ELEMENT,ELEMENT     ADD NEW ONES                                 
         LA    R3,ELEMENT                                                       
         USING TASID,R3                                                         
         MVI   TASIEL,TASIELQ      ELEMENT CODE                                 
         MVI   TASILEN,TASILNQ     ELEMENT LENGTH                               
         BAS   RE,VREST            VALIDATE ESTIMATE NUMBER                     
         BRAS  RE,VRPCT            VALIDATE PERCENTAGE                          
         GOTO1 ADDELEM                                                          
         AP    COUNTER,=P'1'       ADD TO COUNTER                               
VREC10   LA    R2,LINLNQ(R2)       BUMP TO NEXT LINE                            
         BCT   R5,VREC5            LOOP                                         
*                                                                               
         CP    COUNTER,=P'0'       IF NO ESTIMATES                              
         BNE   VREC15                                                           
         TM    SVSTAT,TAINSPAY     AND PAID INVOICE                             
         BO    VREC12                                                           
         CLI   ANYELEM,C'N'                                                     
         BE    XIT                                                              
         B     VREC20                                                           
*                                                                               
VREC12   TM    SVSTA2,TAINSPRM                                                  
         BNO   ERRSPLT             IF REG - CAN'T SPLIT                         
         B     ERRPAY              PRIMARY - NOT ALLOWED TO DEL LINES           
*                                                                               
VREC15   CP    COUNTER,=P'2'                                                    
         BL    ERRCNT              MUST HAVE AT LEAST TWO TO SPLIT              
         CLC   TOTPCT,=F'1000000'                                               
         BNE   ERRPCT              PERCENTAGES MUST ADD TO 100.0000             
         CLI   TWASCR,SCRCF        IF ISPLIT                                    
         BE    *+8                                                              
         BAS   RE,SETPRM           SET INVOICE AS PRIMARY                       
*                                                                               
VREC20   GOTO1 ACTVIN,DMCB,(X'80',0) ADD ACTIVITY INFO BY SCRN                  
         MVC   KEY,SVKEY           RESTORE KEY                                  
         MVC   AIO,AIO2            REREAD RECORD                                
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES ESTIMATE NUMBER                                
         SPACE 1                                                                
VREST    NTR1                                                                   
         LA    R2,LINESTH          R2=A(ESTIMATE NUMBER FIELD)                  
         CLI   5(R2),0             INPUT REQUIRED                               
         BE    ERRMISS                                                          
         TM    AGYSTAT5,TAAYSBBD   TEST BBDO JOBS                               
         BO    VREST2                                                           
         TM    AGYSTAT2,TAAYSNDF   TEST NO DEFAULT JOBS ALLOWED FOR AGY         
         BO    VREST3                                                           
*                                                                               
VREST2   TM    1(R2),X'08'         TEST ERROR PENDING                           
         BNO   VREST3                                                           
         NI    1(R2),X'F7'         RETURN FIELD TO NORMAL INTENSITY             
         OI    6(R2),X'80'         TRAMSIT                                      
         TM    4(R2),X'20'         RE-VALIDATE IF IT CHANGED                    
         BZ    VREST5                                                           
         CLI   TWASCR,SCRCF                                                     
         BE    *+12                                                             
         CLI   PFAID,20            ISPLIT SCREEN ,CHECK IF PF20                 
         B     *+8                                                              
         CLI   PFAID,18            ASPLIT SCREEN - CHECKS PF18                  
         BNE   JOBINV                                                           
         B     VREST20             TAKE OVERRIDE                                
*                                                                               
VREST3   TM    4(R2),X'20'         RE-VALIDATE IF IT CHANGED                    
         BO    VREST20                                                          
*                                                                               
VREST5   BAS   RE,REPCOMMA         REPLACE COMMAS WITH CLI/PRD                  
         BAS   RE,CHKDEF                                                        
         BAS   RE,CHKDUP           CANNOT HAVE DUPLICATE EST #'S                
                                                                                
         OC    DEFJOB,DEFJOB       IF AGENCY SETUP WITH DEFAULT                 
         BZ    VREST7              JOB, VALIDATE AGAINST FORMAT                 
         OC    8(16,R2),SPACES                                                  
         GOTOR VALPJOB,DMCB,8(R2)                                               
         BNE   ERRPJOB                                                          
         CLI   23(R2),C' '                                                      
         BE    VREST20                                                          
         B     ERRPJOB                                                          
                                                                                
VREST7   CLI   INTER,C'Y'          IF AGENCY NOT ON INTERFACE                   
         BE    VREST8                                                           
         TM    AGYSTAT5,TAAYISPL   AGENCY WANTS TO VALIDATE JOBS                
         BZ    VREST20             DON'T BOTHER VALIDATING                      
*                                                                               
         MVC   AIO,AIO2                                                         
         BRAS  RE,VALJOB                                                        
         BNE   JOBINV                                                           
         MVC   AIO,AIO1                                                         
         B     VREST20             DON'T BOTHER VALIDATING                      
*                                                                               
VREST8   CLI   5(R2),6             FIELD MUST INCLUDE CLI/PRD/JOB               
         BNH   ERRINV                                                           
         CLI   5(R2),12            MAXIMUM OF 12 CHARS                          
         BNH   VREST10                                                          
         TM    AGYSTAT2,TAAYSNDF   TEST NO DEFAULT JOBS ALLOWED FOR AGY         
         BO    ERRINV                                                           
         B     JOBINV                                                           
*                                                                               
VREST10  MVC   KEY,SPACES          OTHERWISE, BUILD JOB KEY                     
         MVC   KEY+1(2),=C'SJ'     UNIT/LEDGER                                  
         MVC   KEY+3(12),8(R2)     CLIENT/PRODUCT/JOB CODE                      
         OC    KEY+3(12),SPACES    INSURE KEY PADDED WITH SPACES                
         MVC   AIO,AIO2            SET AIO2                                     
         GOTO1 READACC,DMCB,(X'80',0) READ ACCOUNT FILE                         
         MVC   AIO,AIO1            RESET AIO                                    
         BE    VREST15                                                          
         TM    AGYSTAT2,TAAYSNDF   TEST NO DEFAULT JOBS ALLOWED FOR AGY         
         BO    THEEND                                                           
         CLI   ERROR,NOTFOUND      IF ERROR IS RECORD NOT FOUND                 
         BE    JOBINV              GIVE USER A CHANCE TO OVERRIDE               
         B     THEEND                                                           
*                                                                               
VREST15  BAS   RE,CKCLSLOC         CHECK ACC RECORD FOR CLOSED/LOCKED           
*                                                                               
VREST20  ZIC   RE,5(R2)            SET ESTIMATE NUMBER                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TASIEST(0),8(R2)    SET ESTIMATE NUMBER                          
         LA    RE,1(RE)            RESTORE LENGTH                               
         ZIC   R1,TASILEN          CURRENT ELEMENT LENGTH                       
         AR    R1,RE               ADD ESTIMATE NUMBER                          
         STC   R1,TASILEN          SAVE NEW LENGTH                              
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE ESTIMATE BY JOB RECORDS                      
         SPACE 1                                                                
VALJOB   NTR1                                                                   
         XC    WORK2,WORK2         CLEAR TEMPORARY AREA                         
         MVC   WORK2(L'TGCLI),TGCLI AND SAVE CLIENT AND PRODUCT                 
         MVC   WORK2+L'TGCLI(L'TGPRD),TGPRD                                     
VJOB050  MVC   TGDATE,TGTODAY1     READ JOB RECORDS FOR AGENCY/DATE             
         OC    TGJWBUNT,TGJWBUNT   AGENCY USING JWT PROJECT IDS                 
         BNZ   VJOB100                                                          
         TM    TGAYSTA7,TAAYSBBD                                                
         BO    VJOB100                                                          
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         B     VJOB200                                                          
*                                                                               
VJOB100  OC    8(16,R2),SPACES                                                  
*                                                                               
         USING TLJBD,R4                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         MVI   TLJBCD,TLJBCDQ                                                   
         MVI   TLJBSPCL,TLJBSPJW                                                
         TM    TGAYSTA7,TAAYSBBD   BBDO JOB?                                    
         BZ    VJOB110             NO                                           
         MVI   TLJBSPCL,TLJBSPBD                                                
         MVC   TLJBPROJ,8(R2)                                                   
         OC    TLJBPROJ,SPACES                                                  
         B     VJOB118                                                          
*                                                                               
VJOB110  ZIC   R1,5(R2)            R1=LENGTH OF INPUT                           
         CHI   R1,15                                                            
         BL    VJOBNO                                                           
         LA    R1,7                JOB LENGTH                                   
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TLJBPRJI(0),8(R2)                                                
         OC    TLJBPRJI,SPACES                                                  
VJOB118  GOTO1 HIGH                SEE IF JWT PROJECT ID EXISTS                 
         TM    TGAYSTA7,TAAYSBBD                                                
         BZ    VJOB119                                                          
         CLC   KEY(TLJBDAT-TLJBD),KEYSAVE                                       
         BNE   VJOBNO                                                           
         B     VJOBYES                                                          
                                                                                
VJOB119  CLC   KEY(TLJBDATE-TLJBD),KEYSAVE                                      
         BNE   VJOBNO              JOB INVALID                                  
*&&DO                                                                           
         CLI   5(R2),7             ONLY JOB ENTERED                             
         BH    VJOB120             NO, MORE THAN JUST JOB ENTERED               
         MVC   15(L'TLJBCSTI,R2),TLJBCSTI    FILL IN REST IN EST FIELD          
         MVC   21(L'TLJBPRDI,R2),TLJBPRDI                                       
         MVI   5(R2),16            NEW LENGTH OF EST FIELD                      
         OI    6(R2),X'80'         TRANSMIT                                     
         B     VJOBYES             THAT'S ALL FOR JWT PROJECTS                  
*&&                                                                             
VJOB120  CLC   TLJBCSTI,15(R2)     MUST MATCH WHOLE EST FIELD                   
         BNE   VJOBNO                                                           
         CLC   TLJBPRDI,21(R2)                                                  
         BNE   VJOBNO                                                           
         B     VJOBYES             THAT'S ALL FOR JWT PROJECTS                  
*                                                                               
VJOB200  GOTO1 RECVAL,DMCB,TLJBCDQ,0                                            
         CLC   KEY(TLJBCLI-TLJBD),KEYSAVE IF NONE FOR TODAY'S DATE              
         BE    VJOB250                                                          
         CLC   KEY(TLJBDTE-TLJBD),KEYSAVE BUT EARLIER DATE FOR AGENCY           
         BNE   VJOBNO                                                           
         MVC   TGDATE,KEY+TLJBDTE-TLJBD   USE THAT DATE                         
         XC    TGDATE,=X'FFFFFF'                                                
*                                                                               
VJOB250  LA    R3,8(R2)            R3=A(INPUT)                                  
         ZIC   R1,5(R2)            R1=LENGTH OF INPUT                           
*&&DO                                                                           
         CLC   5(1,R2),LENGTHS+2   IF INPUT IS JOB ONLY                         
         BH    VJOB300                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+12(0),0(R3)   SAVE IT                                      
         OC    WORK2,SPACES                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R3),0(R3)       CLEAR FIELD                                  
         MVC   TGCLI,WORK2         RESTORE CLIENT & PRODUCT                     
         MVC   TGPRD,WORK2+L'TGCLI                                              
*                                                                               
         ZIC   R1,LENGTHS          AND RE-DISPLAY AS CLIENTPRODJOB              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TGCLI                                                    
         LA    R3,1(R1,R3)                                                      
         ZIC   R1,LENGTHS+1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TGPRD                                                    
         LA    R3,1(R1,R3)                                                      
         ZIC   R1,LENGTHS+2                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),WORK2+12                                                 
         LA    R3,1(R1,R3)                                                      
         LA    R1,8(R2)                                                         
         SR    R3,R1                                                            
         STC   R3,5(R2)            SET INPUT LENGTH                             
         OI    6(R2),X'80'         AND TRANSMIT                                 
         B     VJOB400                                                          
*&&                                                                             
VJOB300  ZIC   RE,LENGTHS          CHECK LEN EQUAL TO SETUP LENGTH              
         ZIC   RF,LENGTHS+1                                                     
         AR    RE,RF                                                            
         ZIC   RF,LENGTHS+2                                                     
         AR    RE,RF                                                            
         CR    R1,RE                                                            
*        BNE   VJOBNO                                                           
         BH    VJOBNO              SAATCHI MAY NOT FILL UP JOB LENGTH           
*                                                                               
         ZIC   R1,LENGTHS                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGCLI(0),0(R3)                                                   
         OC    TGCLI,SPACES                                                     
         LA    R3,1(R1,R3)                                                      
         ZIC   R1,LENGTHS+1                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TGPRD(0),0(R3)                                                   
         OC    TGPRD,SPACES                                                     
         LA    R3,1(R1,R3)                                                      
         ZIC   R1,LENGTHS+2                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK2+12(0),0(R3)                                                
         OC    WORK2,SPACES                                                     
*                                                                               
VJOB400  MVC   WORK(L'TGCLI),TGCLI                                              
         TM    AGYSTAT5,TAAYBUSU   BUSINESS UNIT PART OF CLIENT                 
         BZ    VJOB500             NOT SAATCHI                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLJBD,R4            SEE IF VALID BUSINESS UNIT                   
         MVI   TLJBCD,TLJBCDQ                                                   
         MVC   TLJBAGY,TGAGY                                                    
         MVC   TLJBDTE,TGDATE                                                   
         XC    TLJBDTE,=X'FFFFFF'                                               
         MVC   TLJBCLI(3),TGCLI                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLJBCLI-TLJBD+3),KEYSAVE                                     
         BNE   JOBINV              BUSINESS UNIT NOT FOUND                      
*                                                                               
         MVC   TGCLI,SPACES        JUST CLIENT W/O BUS UNIT                     
         MVC   TGCLI(3),WORK+3                                                  
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'A0',0)                                    
         BNE   VJOB500             NOT FOUND, REGULAR VALIDATION                
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TABRELQ      BILLING RULES HAS NOINT STATUS               
         BRAS  RE,GETEL                                                         
         BNE   VJOB500                                                          
         USING TABRD,R4                                                         
         TM    TABRSTAT,TABRSNIN   TEST NO INTERFACE                            
         BZ    VJOB500             ON INTERFACE, REGULAR VALIDATION             
*                                                                               
VJOBYES  MVC   TGCLI,WORK2         RESTORE CLIENT & PRODUCT                     
         MVC   TGPRD,WORK2+L'TGCLI                                              
         CR    RB,RB               SET CC=EQUAL                                 
         B     XIT                                                              
*                                                                               
VJOB500  MVC   TGCLI,WORK                                                       
         GOTO1 RECVAL,DMCB,TLJBCDQ,(X'A0',0)                                    
         MVC   TGCLI,WORK2         RESTORE CLIENT & PRODUCT                     
         MVC   TGPRD,WORK2+L'TGCLI                                              
         BNE   JOBINV                                                           
*                                                                               
VJOB600  XC    WORK,WORK           VALIDATE JOB CODE                            
         MVC   WORK+1(6),WORK2+12                                               
         MVI   ELCODE,TAGLELQ                                                   
         GOTO1 GETL,DMCB,(7,WORK)                                               
         BE    XIT                                                              
         GOTO1 SEQ                 CHECK FOR A CONTINUATION REC                 
         CLC   KEY(TLJBSEQ-TLJBKEY),KEYSAVE                                     
         BNE   JOBINV                                                           
         GOTO1 GETREC                                                           
         B     VJOB600                                                          
*                                                                               
VJOBNO   MVC   TGCLI,WORK2         RESET - SHOULD BE THE SAME AS TALENT         
         MVC   TGPRD,WORK2+L'TGCLI                                              
         B     JOBINV                                                           
         EJECT                                                                  
REPCOMMA NTR1                                                                   
         LA    RF,SIDESTH                                                       
         CR    R2,RF               FIRST LINE?                                  
         BE    REPCX               YES - CAN'T REPLACE ANY COMMAS               
         LR    R4,R2                                                            
         SH    R4,=Y(LINLNQ)       R4=A(PREVIOUS ESTIMATE NUMBER)               
         LA    R4,8(R4)                                                         
*                                                                               
         XC    WORK,WORK                                                        
         ZIC   R5,5(R2)            LENGTH OF ESTIMATE NUMBER                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       SAVE ESTIMATE NUMBER                         
*                                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         LA    R5,1(R5)            RESTORE LENGTH                               
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R3,WORK                                                          
         CLC   =C',,',0(R3)        REPLACE CLIENT AND PRODUCT                   
         BNE   REPC5                                                            
         AH    R5,=H'4'            ADJUST LENGTH OF INPUT                       
         STC   R5,5(R2)                                                         
         LA    R2,8(R2)                                                         
         LA    R1,5                R1=LENGTH FOR EX TO MOVE                     
         LA    R3,2(R3)            R3=(EST NUMBER)                              
         B     REPC20                                                           
*                                                                               
REPC5    CLC   =C',',0(R3)         REPLACE JUST CLIENT                          
         BNE   REPC10                                                           
         AH    R5,=H'2'            ADJUST LENGTH OF INPUT                       
         STC   R5,5(R2)                                                         
         LA    R2,8(R2)                                                         
         LA    R1,2                R1=LENGTH FOR EX TO MOVE                     
         LA    R3,1(R3)            R3=(EST NUMBER)                              
         B     REPC20                                                           
*                                                                               
REPC10   CLC   =C',',3(R3)         REPLACE JUST PRODUCT                         
         BNE   REPC40                                                           
         AH    R5,=H'2'            ADJUST LENGTH OF INPUT                       
         STC   R5,5(R2)                                                         
         LA    R2,8(R2)            R2=A(FIELD)                                  
         MVC   0(3,R2),0(R3)       SET CLIENT                                   
         LA    R2,3(R2)                                                         
         LA    R4,3(R4)                                                         
         LA    R1,2                R1=LENGTH FOR EX TO MOVE                     
         LA    R3,4(R3)            R3=(EST NUMBER)                              
*                                                                               
REPC20   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R4)       SET INFO FROM LINE ABOVE                     
         LA    R1,1(R1)                                                         
         AR    R2,R1                                                            
         MVC   0(10,R2),0(R3)      SET ESTIMATE CODE                            
         B     REPCX                                                            
*                                                                               
REPC40   MVC   8(16,R2),WORK       RESET ESTIMATE NUMBER                        
REPCX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CHECKS FOR "DEFAULT" JOB ENTRY                           
CHKDEF   NTR1                                                                   
         OC    DEFJOB,DEFJOB                                                    
         BZ    XIT                                                              
         OC    8(16,R2),SPACES                                                  
         CLC   =CL16'DEFAULT',8(R2)                                             
         BNE   XIT                                                              
         MVI   5(R2),L'DEFJOB                                                   
         OI    6(R2),X'80'                                                      
         XC    8(16,R2),8(R2)                                                   
         MVC   8(L'DEFJOB,R2),DEFJOB                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE MAKES SURE NO DUPLICATE ESTIMATE NUMBER                  
CHKDUP   NTR1                                                                   
         MVC   WORK(32),SPACES                                                  
         MVI   ELCODE,TASIELQ                                                   
         L     R3,AIO                                                           
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHKDUP10 BAS   RE,NEXTEL                                                        
         BNE   CHKDUPX                                                          
*                                                                               
         USING TASID,R3                                                         
         ZIC   RE,TASILEN                                                       
         SH    RE,=Y(TASILNQ+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),TASIEST                                                  
         ZIC   RE,5(R2)            LENGTH OF INPUT                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+16(0),8(R2)                                                 
         CLC   WORK(16),WORK+16                                                 
         BE    ERRDUP              DUPLICATE ESTIMATE FIELD                     
         B     CHKDUP10                                                         
*                                                                               
CHKDUPX  B     XIT                                                              
         EJECT                                                                  
*              MARK INVOICE AS PRIMARY                                          
SETPRM   NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,TAINELQ      FIND INVOICE STATUS ELEMENT                  
         USING TAIND,R3                                                         
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TAINSTA2,TAINSPRM   SET PRIMARY INVOICE                          
         B     XIT                                                              
         DROP  R3                                                               
         SPACE                                                                  
*              ROUTINE CHECKS ACC RECORD IS CLOSED/LOCKED                       
         SPACE                                                                  
CKCLSLOC NTR1                                                                   
         L     R3,AIO2                                                          
         USING ACKEYD,R3                                                        
         TM    ACSTATUS,X'60'      TEST FOR CLOSED/LOCKED                       
         BNZ   CLOSLOCK                                                         
         TM    TGSYSTA2,TASYSPDJ   IF PROHIBITING DRAFT JOBS                    
         BZ    XIT                                                              
         TM    ACSTATUS,X'08'      TEST FOR DRAFT                               
         BO    DFTJOB                                                           
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 1                                                                
ERRPCT   MVI   ERROR,ERMAXPCT      PERCENTAGE NOT EQU 100.0000                  
         L     RE,AFRSTLIN                                                      
         USING LINED,RE                                                         
         LA    R2,LINESTH          R2=A(FIRST LINE)                             
         B     THEEND                                                           
         DROP  RE                                                               
*                                                                               
ERRDUP   MVI   ERROR,ERDUPFLD      DUPLICATE INPUT FIELD                        
         B     THEEND                                                           
*                                                                               
ERRAPR   MVI   ERROR,ERINVAPR      INVOICE ALREADY APPROVED                     
         LA    R2,CONACTH          R2=A(ACTION FIELD)                           
         B     THEEND                                                           
*                                                                               
ERRSENT  MVI   ERROR,ERDVSENT      CHANGE NOT ALLOWED -SENT                     
         LA    R2,CONACTH          R2=A(ACTION FIELD)                           
         B     THEEND                                                           
*                                                                               
ERREST   MVI   ERROR,ERADVEST      ESTIMATE ALREADY ON ADVICE                   
         LA    R2,CONRECH          R2=A(RECORD FIELD)                           
         B     THEEND                                                           
*                                                                               
ERRPAY   MVI   ERROR,ERINVPAY      CAN'T DELETE ALL ESTS ON A PAID INV          
         L     RE,AFRSTLIN         TO SPLIT                                     
         USING LINED,RE                                                         
         LA    R2,LINESTH          R2=A(ESTIMATE FIELD)                         
         B     THEEND                                                           
         DROP  RE                                                               
*                                                                               
ERRSPLT  MVI   ERROR,ERINVSPT      CANNOT SPLIT PAID INVOICE                    
         LA    R2,CONACTH          R2=A(ACTION FIELD)                           
         B     THEEND                                                           
*                                                                               
ERRMISS  MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
*                                                                               
ERRCNT   MVI   ERROR,ERSPLIT       MUST HAVE AT LEAST TWO ESTIMATES             
         L     RE,AFRSTLIN         TO SPLIT                                     
         USING LINED,RE                                                         
         LA    R2,LINESTH          R2=A(FIRST LINE)                             
         B     THEEND                                                           
         DROP  RE                                                               
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
*                                                                               
ERPPLSI  LA    R2,SIDAGYH                                                       
         MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
                                                                                
JOBINV   MVI   ERROR,ERINVJOB      INVALID JOB NUMBER (PF20)                    
         CLI   TWASCR,SCRCF                                                     
         BNE   *+8                                                              
         MVI   ERROR,ERINVJB2      INVALID JOB NUMBER (PF18)                    
         OI    1(R2),X'08'         SET HIGH INTENSITY                           
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
         OI    6(R2),X'80'         TRANSMIT                                     
         B     THEEND                                                           
*                                                                               
ERRPJOB  MVC   MYMSGNO,=Y(EPUBJOB)                                              
         B     EXTEND                                                           
*                                                                               
CLOSLOCK MVI   ERROR,ERCLSLCK      JOB CLOSED/LOCKED                            
         B     THEEND                                                           
*                                                                               
DFTJOB   MVC   MYMSGNO,=Y(ERDFTJOB)                                             
EXTEND   MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
*                                                                               
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL (R3),DATADISP,ELCODE                                             
         SPACE                                                                  
MXLINES  EQU   10                                                               
         SPACE                                                                  
COMPLM   DC    6X'FF'                                                           
         SPACE                                                                  
*              PFK TABLE FOR ISPLIT SCREEN                                      
PFTAB    DS    0C                                                               
         DC    AL1(PF13X-*,13,0,0,0)                                            
         DC    CL3'   ',CL8'HISTORY ',CL8'DISPLAY'                              
PF13X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,PFTNOSEL)                   
         DC    CL3'   ',CL8'HISTORY ',CL8'DISPLAY'                              
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LININV-1),AL2(LININV-LINSTRT)                     
PF14X    EQU   *                                                                
         SPACE                                                                  
         DC    AL1(PF20X-*,20,0,0,PFTRETRN)                                     
         DC    CL3'   ',CL8'        ',CL8'       '                              
PF20X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
*              PFK TABLE FOR ASPLIT SCREEN                                      
APFTAB   DS    0C                                                               
         DC    AL1(APF16X-*,16,0,0,0)                                           
         DC    CL3'   ',CL8'ADVICE  ',CL8'VERIFY '                              
APF16X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(APF17X-*,17,0,0,0)                                           
         DC    CL3'   ',CL8'ADVICE  ',CL8'SEND   '                              
APF17X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(APF18X-*,18,0,0,PFTRETRN)                                    
         DC    CL3'   ',CL8'        ',CL8'       '                              
APF18X   EQU   *                                                                
         SPACE                                                                  
         DC    AL1(APF20X-*,20,0,0,0)                                           
         DC    CL3'   ',CL8'ADVICE  ',CL8'       '                              
APF20X   EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*--------------------------------------------------------------------           
*              ROUTINE DISPLAYS RECORD                                          
*--------------------------------------------------------------------           
DISPLAY  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,CLRSCRN          CLEAR THE SCREEN                             
         LA    R5,MXLINES          MAX LINES ON SCREEN                          
         L     R2,AFRSTLIN         R2=A(FIRST LINE)                             
         USING LINED,R2                                                         
         L     R3,AIO                                                           
         MVI   ELCODE,TASIELQ      GET SUBSIDIARY INVOICE ELEMENTS              
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISP10   BRAS  RE,NEXTEL           GET NEXT ELEMENT                             
         BNE   DISP50                                                           
         USING TASID,R3                                                         
         BAS   RE,DREST            DISPLAY ESTIMATE NUMBER                      
         BAS   RE,DRPCT            DISPLAY PERCENTAGE                           
         BAS   RE,DRINV            DISPLAY INVOICE NUMBER                       
*                                                                               
         LA    R2,LINLNQ(R2)       BUMP TO NEXT DISPLAY LINE                    
         BCT   R5,DISP10           LOOP                                         
*                                                                               
DISP50   MVC   SVTWASCR,TWASCR     SAVE SCREEN NUMBER                           
         CLI   ACTNUM,ACTAPP       IF ACTION IS APPROVE                         
         BNE   *+8                                                              
         MVI   TWASCR,SCRD0        SET ISPLIT SCREEN                            
         L     R2,ALCHGHDR                                                      
         GOTO1 ACTVOUT,DMCB,(X'80',(R2))                                        
         MVC   TWASCR,SVTWASCR     RESET SCREEN NUMBER                          
DISPXIT  XIT1                                                                   
         EJECT                                                                  
*--------------------------------------------------------------------           
*              ROUTINE DISPLAYS ESTIMATE NUMBER                                 
*--------------------------------------------------------------------           
DREST    NTR1                                                                   
         LA    R2,LINEST            ESTIMATE NUMBER                             
         ZIC   RE,TASILEN           ELEMENT LENGTH                              
         SH    RE,=Y(TASILNQ+1)                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),TASIEST     DISPLAY ESTIMATE NUMBER                      
         B     DISPXIT                                                          
*--------------------------------------------------------------------           
*              ROUTINE DISPLAYS PERCENTAGE                                      
*--------------------------------------------------------------------           
DRPCT    NTR1                                                                   
         LA    R2,LINPCT                                                        
         EDIT  TASIPCT3,(8,0(R2)),4,ALIGN=LEFT                                  
         AR    R2,R0               + LENGTH OF EDIT                             
         SH    R2,=H'4'            PT R2 TO DECIMAL PLACES                      
         LA    R1,3                CHECK DECIMALS FOR ZERO                      
DRPCT5   EX    R1,CKZERO                                                        
         BE    DRPCT8                                                           
         LA    R2,1(R2)                                                         
         BCT   R1,DRPCT5                                                        
         B     DRPCTX              NO ZEROES IN DECIMAL                         
*                                                                               
DRPCT8   CH    R1,=H'3'            IF ALL DECIMAL PLACES ARE ZERO               
         BNE   DRPCT9                                                           
         LA    R1,1(R1)            CLEAR DECIMAL POINT TOO                      
         BCTR  R2,0                                                             
DRPCT9   EX    R1,MVCSPC           SET TO SPACES                                
DRPCTX   B     DISPXIT                                                          
*                                                                               
CKZERO   CLC   0(0,R2),=C'0000'    CHECK IF ZEROES                              
MVCSPC   MVC   0(0,R2),SPACES      SET ZEROES TO SPACES                         
*--------------------------------------------------------------------           
*              ROUTINE DISPLAYS INVOICE NUMBER                                  
*--------------------------------------------------------------------           
DRINV    NTR1                                                                   
         OC    TASIINV,TASIINV     INV NUMBER ONLY AFTER BILLED                 
         BZ    DRINVX                                                           
         GOTO1 TINVCON,DMCB,TASIINV,LININV,DATCON                               
DRINVX   B     DISPXIT                                                          
         EJECT                                                                  
*--------------------------------------------------------------------           
*              CLEAR THE SCREEN                                                 
*--------------------------------------------------------------------           
CLRSCRN  NTR1                                                                   
         USING LINED,R2                                                         
         L     R2,AFRSTLIN         R2=A(FIRST LINE)                             
         L     R5,ALSTLIN          R5=A(LAST LINE)                              
         GOTO1 FLDVAL,DMCB,(X'01',(R2)),(X'80',(R5))                            
*                                                                               
         LA    R5,MXLINES          MAX LINES FOR SCREEN                         
CLRSCRN5 XC    LININV,LININV       CLEAR INVOICE (PROTECTED FLD)                
         OI    LININVH+6,X'80'     TRANSMIT                                     
         LA    R2,LINLNQ(R2)       BUMP TO NEXT DISPLAY LINE                    
         BCT   R5,CLRSCRN5         LOOP                                         
         B     DISPXIT                                                          
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE VALIDATES PERCENTAGE                                     
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING TASID,R3                                                         
VRPCT    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,LINPCTH          R2=A(PERCENTAGE FIELD)                       
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)                                                       
         BZ    ERRMISS2            REQUIRED INPUT                               
         GOTO1 CASHVAL,DMCB,(4,8(R2)),(RF)                                      
         CLI   0(R1),X'FF'                                                      
         BE    ERRINV2                                                          
         MVC   TASIPCT3,5(R1)      SET PERCENTAGE                               
         OC    TASIPCT3,TASIPCT3   CANNOT BE 0%                                 
         BZ    ERRINV2                                                          
         CLC   TASIPCT3,=XL3'F4240'   AND CANNOT BE 100%                        
         BE    ERRINV2                                                          
*                                                                               
         L     R1,TOTPCT           ADD TO TOTAL PERCENTAGE                      
         ZICM  R0,TASIPCT3,3                                                    
         AR    R1,R0                                                            
         ST    R1,TOTPCT                                                        
         XIT1                                                                   
         DROP  R2,R3                                                            
ERRMISS2 MVI   ERROR,MISSING                                                    
         B     THEEND2                                                          
*                                                                               
ERRINV2  MVI   ERROR,INVALID                                                    
         B     THEEND2                                                          
*                                                                               
THEEND2  GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAVALPJB                                                       
*              CONSTANTS, ETC.                                                  
LINED    DSECT                                                                  
LINSTRTH DS    CL8                                                              
LINSTRT  DS    CL1                 >                                            
LINESTH  DS    CL8                                                              
LINEST   DS    CL16                EST #                                        
         DS    CL8                                                              
LINPCTH  DS    CL8                                                              
LINPCT   DS    CL8                 PERCENTAGE                                   
         DS    CL8                                                              
LININVH  DS    CL8                                                              
LININV   DS    CL6                 INVOICE NUMBER                               
         DS    CL8                                                              
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         SPACE                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRCFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD0D                                                       
         ORG   SIDWORK                                                          
         DS    0D                                                               
AFRSTLIN DS    A                                                                
ALSTLIN  DS    A                                                                
ALCHGHDR DS    A                                                                
TOTPCT   DS    F                   COUNTER FOR TOTAL PERCENTAGE                 
*                                                                               
COUNTER  DS    PL4                 LINE COUNTER                                 
INV      DS    CL6                 INVOICE NUMBER                               
ANYELEM  DS    CL1                 Y=TASID ELEMENTS ON RECORD                   
SVTWASCR DS    XL1                 SAVED SCREEN NUMBER                          
SVSTAT   DS    XL1                 SAVED TAINSTAT                               
SVSTA2   DS    XL1                 SAVED TAINSTA2                               
AGYSTAT  DS    XL1                 AGENCY STATUS                                
AGYSTAT2 DS    XL1                 AGENCY STATUS TWO                            
AGYSTAT5 DS    XL1                 AGENCY STATUS FIVE                           
AGYSTAT7 DS    XL1                 AGENCY STATUS SEVEN                          
INTER    DS    CL1                 Y= AGENCY ON INTERFACE                       
LENGTHS  DS    XL3                 LENGTHS FOR CLI/PRD/JOB                      
DEFJOB   DS    CL15                AGENCY DEFAULT JOB                           
WORK2    DS    CL24                SECONDARY WORK AREA                          
SVKEY    DS    CL(L'TLDRREC)       SAVED KEY                                    
         EJECT                                                                  
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENEQUS                                                                     
* DDSPLWORKD                                                                    
* DDSPOOLD                                                                      
* TAGENFILE                                                                     
* ACGENBOTH                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'078TAGENCF   01/28/16'                                      
         END                                                                    
